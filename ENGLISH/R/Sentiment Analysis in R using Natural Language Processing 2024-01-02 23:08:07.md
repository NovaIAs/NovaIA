Certainly! Here's a complex and differentiated R code that performs sentiment analysis on a given text dataset using a combination of natural language processing techniques:

```R
# Required Libraries
library(tm)
library(SnowballC)
library(textclean)
library(quanteda)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)

# Function to Perform Sentiment Analysis
performSentimentAnalysis <- function(data) {
  
  # Preprocessing
  corpus <- Corpus(VectorSource(data$text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  
  # Document-Term Matrix
  dtm <- DocumentTermMatrix(corpus)
  
  # Sentiment Lexicon
  lexicon <- get_sentiments("afinn")
  
  # Calculate Sentiment Scores
  sentiment_scores <- tidy(dtm) %>%
    left_join(lexicon, by = c("term" = "word")) %>%
    group_by(document) %>%
    summarise(sentiment_score = sum(score))
  
  # Plot Sentiment Scores
  ggplot(sentiment_scores, aes(x = document, y = sentiment_score)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Document", y = "Sentiment Score", title = "Sentiment Analysis")
}

# Sample Usage
data <- data.frame(
  text = c("I love this product!", "This is terrible.", "The service was excellent."),
  stringsAsFactors = FALSE
)

performSentimentAnalysis(data)
```

This code begins by loading necessary libraries such as `tm` for text mining, `SnowballC` for stemming, `textclean` for text cleaning, `quanteda` for corpus manipulation, `dplyr` and `tidyr` for data wrangling, `tidytext` for tidy text analysis, and `ggplot2` for data visualization.

The `performSentimentAnalysis` function takes a dataset with a "text" column as input. It preprocesses the text by converting it to lowercase, removing punctuation, numbers, stop words, and white spaces, and performs stemming. 

Next, it creates a Document-Term Matrix (DTM) to represent the text data numerically. It also loads a sentiment lexicon called "afinn" using the `get_sentiments` function.

The sentiment scores for each document are calculated by joining the DTM with the sentiment lexicon using `left_join`, grouping by the document, and summing the sentiment scores. These scores are then plotted using `ggplot2` to visualize the sentiment analysis results.

Finally, a sample usage is provided where a data frame with three example texts is created. The `performSentimentAnalysis` function is called with this data frame as an argument.

This code utilizes various R packages and techniques to perform sentiment analysis on textual data, making it a complex and differentiated solution.