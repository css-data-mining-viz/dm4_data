# SOLUTIONS for in-class exercises on Day 4

# Mining SOTU Addresses, George Washington - Barack Obama

# Load some libraries
library(tidyverse)
library(tidytext)
library(tm)
library(sotu)
library(igraph) # for graph_from_data_frame function
library(ggraph) # for network plot
library(quanteda)
library(wordcloud)
library(topicmodels)

# Call and attach the data: bind the speech meta data with the raw text of the speeches
sotu_meta$text <- sotu_text

# 1. Tokenize all speeches using a word tokenizer. 
(unt_sotu <- sotu_meta %>%
    unnest_tokens(output = word, 
                  input = text))

# 2. Omit stop words for targeted exploration.
as_tibble(stop_words)

stops_sotu <- unt_sotu %>%
  anti_join(stop_words, 
            by = "word") # drop words in stop words data by "unjoining" them from the df

# 3. What happened to the size of the full data?

# 4. Inspect the total "quality" (- stop_words) words of speeches by year *numerically* (i.e., a table).
stops_sotu %>%
  group_by(year) %>%
  summarise(all_words = length(word)) %>%
  arrange(desc(all_words)) %>% 
  as_tibble()

stops_sotu %>%
  count(word, 
        sort = TRUE)

# 5. Visualize the words used more than 1500 times across all years of SOTU addresses.
stops_sotu %>%
  count(word, sort = TRUE) %>%
  filter(n > 1500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = amerika::amerika_palette("Dem_Ind_Rep7", 27, "continuous")) +
  coord_flip() +
  labs(x = "Term",
       y = "Count") +
  theme_minimal()

# 6. What do you see? Is this what you'd expect?

# 7. Let's drill down and make these aggregate trends more accessible. To do so, we will use Obama as a case study, starting with exploration of bigrams. First, filter the data to only include Obama (from 2008 through 2016). Then, manually clean the data to omit Obama-specific stop words (use your judgement here). Then, print out a snap shot (10 or so) bigrams. What do you notice? 

(obama_bigrams <- sotu_meta %>%
    filter(year > 2008 & year <= 2016) %>%
    unnest_tokens(output = bigram, 
                  input = text, 
                  token = "ngrams", 
                  n = 2, 
                  to_lower =   FALSE) %>% 
    separate(bigram, c("unigram_1", "unigram_2"), 
             sep = " ") %>% 
    filter(!unigram_1 %in% stop_words$word) %>%
    filter(!unigram_2 %in% stop_words$word))

for (i in seq(obama_bigrams)) {
  obama_bigrams[[i]] <- gsub("Mr", " ", obama_bigrams[[i]])
  obama_bigrams[[i]] <- gsub("Madam", " ", obama_bigrams[[i]])
}; obama_bigrams

obama_bigrams <- obama_bigrams[!(obama_bigrams$unigram_1 == " " | 
                                   obama_bigrams$unigram_2 == " "), ]; obama_bigrams

# 8. Now, explore combinations of words, but this time based on the *second unigram* in the bigram set. This exploration allows you to more targetedly inspect bigrams. Specifically, print out counts of the bigrams associated with the following key terms that we might think of with Obama:
# 8.1 unigram 2: campaign
obama_bigrams %>%
  filter(unigram_2 == "campaign") %>%
  count(president, unigram_1, sort = TRUE)

# 8.2 unigram 2: change
obama_bigrams %>%
  filter(unigram_2 == "change") %>%
  count(president, unigram_1, sort = TRUE)

# 8.3 unigram 2: economy
obama_bigrams %>%
  filter(unigram_2 == "economy") %>%
  count(president, unigram_1, sort = TRUE)

# 8.4 unigram 2: war
obama_bigrams %>%
  filter(unigram_2 == "war") %>%
  count(president, unigram_1, sort = TRUE)

# 8.5 unigram 2: peace
obama_bigrams %>%
  filter(unigram_2 == "peace") %>%
  count(president, unigram_1, sort = TRUE)

# 9. Discuss the output in a few sentences. For example, what can you deduce from Obama's policy priorities based on the simple EDA exercise? What is the context surrounding his use of these common words? Etc. 

### Sentiment analysis

# dictionaries from tidytext:
# 1. bing: binary negative/positive 
# 2. afinn: categorizes words negative to positive, from -5 to +5
# 3. loughran: six categories: constraining, litigious, negative, positive, superfluous, and uncertainty 
# 4. nrc: ten categories: anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise, and trust
get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("loughran")
get_sentiments("nrc")


# 10. Let's consider presidents from the depression (> 1929) and WWII (< 1945) eras. Calculate and visualize the sentiment scores for these presidents using the Bing dictionary. Condition the color for negative sentiments to be red, and positive sentiments to be blue.
pres_sent <- stops_sotu %>%
  inner_join(get_sentiments("bing")) %>%
  filter(year > 1929 & year < 1945) %>%
  count(president, year, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  arrange(year)
  
pres_sent$President <- pres_sent$president

ggplot(pres_sent, aes(year, sentiment, fill = President)) +
  geom_col(fill = ifelse(pres_sent$sentiment <= 0, 
                         amerika::amerika_palette("Republican", 3)[3],  
                         amerika::amerika_palette("Democrat", 3)[3])) +
  labs(x = "Year",
       y = "Sentiment\n(Negative - Positive)",
       title = "Sentiment during Depression through WWII") +
  theme_minimal()


### Topic models

# 11. Now, let's explore presidents in the "modern" era (post Korean war (1955) to present). First, create, clean, and preprocess a new "modern" corpus for these presidents.
modern <- sotu_meta %>%
  filter(year > 1955) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(year) %>%
  count(word)

# 12. Create a document term matrix for this modern corpus using `tidytext`, not `tm`.
dtm <- cast_dtm(modern, year, word, n)

# 13. Fit an LDA topic model with Gibbs sampling, initialized at `k = 6` for a first attempt.
modern_lda <- LDA(dtm, k = 6, 
                  method = "Gibbs", control = list(seed = 1234, 
                                                   verbose = 1))

# 14. Print out th dominant topic by year to show which topic characterized the given year's address.
topics(modern_lda)

#table(topics(modern_lda))

#terms(modern_lda, 10) # top 5 terms from each topic


# 15. Visualize the \beta values (i.e., the probability of a term generated from a topic) for the top 15 most frequently used terms across all topics. Be sure to vary color by topic. 
topics <- tidy(modern_lda, 
               matrix = "beta")
terms <- topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(fill = amerika::amerika_palette(name = "Democrat", 
                                           n = 92, 
                                           "continuous"), 
           show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme_minimal()

# 16. Check perplexity and update the LDA model fit. Specifically, fit 9 additional topic models varying $k$ each time for values \in {2,3,4,5,6,7,8,9,10}. Plot the perplexity for each model. Which is the optimal value for $k$?

n_topics <- c(2,3,4,5,6,7,8,9,10)

{  # takes about 3.5 minutes to run
  lda_compare <- n_topics %>%
    map(LDA, x = dtm, control = list(seed = 1234))
}

tibble(k = n_topics,
       perplex = map_dbl(lda_compare, perplexity)) %>% 
  arrange(perplex) 

tibble(k = n_topics,
       perplex = map_dbl(lda_compare, perplexity)) %>%
  ggplot(aes(k, perplex)) +
  geom_point(color = amerika::amerika_palette("Republican", 9, "continuous")) +
  geom_line(color = amerika::amerika_palette("Republican", 9, "continuous")) +
  labs(title = "Evaluating Perplexity of LDA topic models",
       subtitle = "Optimal number of topics, k",
       x = "K Topics",
       y = "Perplexity Score") +
  theme_minimal()


# 17. Store the \gamma values for each document, which is the probability a speech is associated with a topic. Then, print these values for Reagan's first SOTU (1981) compared to his first SOTU *post*-reelection (1985). Do you notice any difference in the general focus of the speech?
gamma <- tidy(modern_lda, 
              matrix = "gamma")

(reagan81 <- gamma %>% 
    filter(document == "1981") %>% 
    arrange(-gamma))

(reagan85 <- gamma %>% 
    filter(document == "1985") %>% 
    arrange(-gamma))

