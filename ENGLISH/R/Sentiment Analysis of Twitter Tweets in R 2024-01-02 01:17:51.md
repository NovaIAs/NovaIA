Certainly! Here's a complex and unique R code that performs sentiment analysis on a large dataset of Twitter tweets:

```R
# Load required libraries
library(tidyverse)
library(tidytext)
library(textdata)

# Import the dataset
tweets <- read_csv("twitter_data.csv")

# Preprocess the tweets
tweets_cleaned <- tweets %>%
  mutate(text_cleaned = tweets %>%
           unnest_tokens(word, text) %>%
           anti_join(stop_words) %>%
           filter(!word %in% c("http", "https", "rt")) %>%
           mutate(word = wordStem(word)) %>%
           unite(text_cleaned, word, sep = " ") %>%
           group_by(tweet_id) %>%
           summarize(text_cleaned = paste(text_cleaned, collapse = " "))) 

# Perform sentiment analysis
sentiment_scores <- tweets_cleaned %>%
  unnest_tokens(word, text_cleaned) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(tweet_id) %>%
  summarise(sentiment_score = sum(score))

# Classify sentiment
sentiment_class <- sentiment_scores %>%
  mutate(sentiment_class = ifelse(sentiment_score > 0, "Positive",
                                  ifelse(sentiment_score < 0, "Negative", "Neutral")))

# Merge sentiment classification with original dataset
tweets_with_sentiment <- left_join(tweets, sentiment_class, by = "tweet_id")

# Generate visualizations
sentiment_plot <- tweets_with_sentiment %>%
  group_by(sentiment_class) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = sentiment_class, y = count, fill = sentiment_class)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Analysis of Tweets",
       x = "Sentiment Class",
       y = "Count") +
  theme_minimal()

# Export visualizations and sentiment scores
ggsave("sentiment_plot.png", sentiment_plot)
write_csv(sentiment_scores, "sentiment_scores.csv")
```

This code assumes that you have a dataset called "twitter_data.csv" with columns for each tweet's ID and text. The code performs the following steps:

1. Loads the required libraries for data manipulation, text preprocessing, and sentiment analysis.
2. Imports the Twitter dataset using the `read_csv()` function.
3. Preprocesses the tweets by removing stop words, URLs, retweets, and stemming words using the `unnest_tokens()`, `anti_join()`, `filter()`, `mutate()`, and `unite()` functions.
4. Performs sentiment analysis using the AFINN lexicon, calculating sentiment scores for each tweet.
5. Classifies the sentiment as "Positive", "Negative", or "Neutral" based on the sentiment scores using conditional statements.
6. Merges the sentiment classification with the original dataset using the `left_join()` function.
7. Generates a bar plot visualizing the distribution of sentiment classes using the `ggplot()` and `geom_bar()` functions.
8. Saves the sentiment plot as "sentiment_plot.png" and exports the sentiment scores as "sentiment_scores.csv" using the `ggsave()` and `write_csv()` functions, respectively.

Please note that this code assumes you have the required libraries installed and have a valid Twitter dataset in the specified format.