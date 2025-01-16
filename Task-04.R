# Install required libraries (run only once)
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tidytext")) install.packages("tidytext")
if (!require("wordcloud")) install.packages("wordcloud")
if (!require("sentimentr")) install.packages("sentimentr")

# Load libraries
library(tidyverse)
library(tidytext)
library(wordcloud)
library(sentimentr)

# Load the dataset
file_path <- "C:/Users/ASUS/Downloads/twitter_validation.csv"  # Update with your file path
tweets <- read.csv(file_path)

# Preview the dataset
head(tweets)
str(tweets)

# 1. Data Cleaning
# Select relevant columns (assuming "text" contains the tweet text)
tweets_clean <- tweets %>%
  select(text) %>%  # Adjust if the column name is different
  filter(!is.na(text))

# Preprocess text data
tweets_clean$text <- tweets_clean$text %>%
  tolower() %>%                                # Convert to lowercase
  gsub("[^a-z ]", "", .) %>%                   # Remove special characters
  gsub("\\s+", " ", .)                         # Remove extra spaces

# Remove stop words
data("stop_words")
tweets_clean <- tweets_clean %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# 2. Sentiment Analysis
# Calculate sentiment scores for each word
tweets_clean <- tweets_clean %>%
  mutate(sentiment_score = sentimentr::sentiment(word)$sentiment)

# Classify sentiments (positive, neutral, negative)
tweets_clean <- tweets_clean %>%
  mutate(sentiment_label = case_when(
    sentiment_score > 0 ~ "Positive",
    sentiment_score < 0 ~ "Negative",
    TRUE ~ "Neutral"
  ))

# 3. Visualization
# Bar chart for sentiment distribution
tweets_clean %>%
  count(sentiment_label) %>%
  ggplot(aes(x = sentiment_label, y = n, fill = sentiment_label)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Distribution", x = "Sentiment", y = "Count") +
  theme_minimal()

# Word cloud for frequent words
tweets_clean %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 100, colors = brewer.pal(8, "Dark2")))
