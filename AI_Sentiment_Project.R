library(tidyverse)
library(glue)
library(stringr)

library(tidyverse)
library(tidytext)
library(glue)


analyze_sentiment <- function(text) {
  
  tokens <- tibble(word = unlist(strsplit(tolower(text), "\\s+")))
  
  #Bing Lexicon
  bing_lexicon <- get_sentiments("bing")
  sentiment <- tokens %>%
    inner_join(bing_lexicon, by = c("word" = "word")) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(net_sentiment = positive - negative)
  
  
  return(sentiment)
}


file_names <- c("2021ArticlesAI.txt", "2022ArticlesAI.txt", "2023ArticlesAI.txt", "2024ArticlesAI.txt")

#analyzing the data for each year
for (file_name in file_names) {
  file_path <- glue("C:/Users/noelm/Documents/{file_name}")
  text <- readLines(file_path, warn = FALSE)
  sentiment_results <- analyze_sentiment(text)
  
  
  cat("Sentiment analysis for", file_name, ":\n")
  print(sentiment_results)
  cat("\n")
}

library(ggplot2)

#making a data frame out of the results
sentiment_data <- data.frame(
  Year = c(2021, 2022, 2023, 2024),
  Negative = c(233, 1329, 809, 19),
  Positive = c(534, 2038, 1277, 58),
  Net_Sentiment = c(301, 709, 468, 39)
)

# Bar plot
bar_plot <- ggplot(sentiment_data, aes(x = factor(Year), y = Net_Sentiment, fill = factor(Year))) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Net Sentiment", title = "Net Sentiment Over Years") +
  scale_fill_manual(values = c("lightblue", "yellow", "skyblue", "red")) +  # Custom color palette
  theme_minimal()

# Line plot
line_plot <- ggplot(sentiment_data, aes(x = Year, y = Net_Sentiment)) +
  geom_line(color = "lightblue") +  # Light blue color
  geom_point(color = "lightblue") +  # Light blue color
  geom_smooth(method = "loess", se = FALSE, color = "lightblue") +  # Curved line
  labs(x = "Year", y = "Net Sentiment", title = "Net Sentiment Trend Over Years") +
  theme_minimal()

print(bar_plot)
print(line_plot)

#47,986 words in 2021
#77,022 words in 2022
#44794 words in 2023
#72090 words in 2024

#word counts now considered. calculating the sentiment per word
word_counts <- c(47986, 77022, 44794, 72090)


sentiment_data <- data.frame(
  Year = c(2021, 2022, 2023, 2024),
  Negative = c(233, 1329, 809, 19),
  Positive = c(534, 2038, 1277, 58),
  Net_Sentiment = c(301, 709, 468, 39)
)

#Calculation for positive word percentage
sentiment_data$Charged_Positive_Percent <- sentiment_data$Positive / (sentiment_data$Negative + sentiment_data$Positive) * 100

# Line plot
line_plot <- ggplot(sentiment_data, aes(x = Year, y = Charged_Positive_Percent)) +
  geom_line(color = "lightblue") +
  geom_point(color = "lightblue") +
  geom_smooth(method = "loess", se = FALSE, color = "lightblue") +  # Curved line
  labs(x = "Year", y = "% of Words with Positive Net Sentiment", title = "Percentage of Words with Positive Net Sentiment Over the Years") +
  theme_minimal()

print(line_plot)

#bar graph
bar_plot <- ggplot(sentiment_data, aes(x = factor(Year), y = Charged_Positive_Percent, fill = factor(Year))) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "% of Charged Words with Positive Net Sentiment", title = "Percentage of Charged Words with Positive Net Sentiment each year") +
  scale_fill_manual(values = c("orange", "brown", "orange", "brown")) + 
  theme_minimal()

print(bar_plot)

#
#wordcloud (plswordplsworkwplsworkplswork)
#

library(wordcloud)
library(tm)

preprocess_text <- function(text) {

  text <- tolower(text)

  text <- removePunctuation(text)

  text <- removeNumbers(text)

  text <- stripWhitespace(text)

  text <- removeWords(text, c(stopwords("en"), "ieee", "standard", "also", "ai"))
  return(text)
}

# Analyze sentiment function with word cloud generation
analyze_sentiment <- function(text) {
  tokens <- tibble(word = unlist(strsplit(tolower(text), "\\s+")))
  
  word_freq <- table(tokens$word)
  
  brown_palette <- colorRampPalette(c("#8B4513", "#A0522D", "#CD853F", "#8B7355"))(length(word_freq))
  
  wordcloud(words = names(word_freq), freq = word_freq,
            max.words = 100, random.order = FALSE,
            colors = brown_palette,
            scale = c(5, 0.5))  # Custom scaling for font sizes
  
  bing_lexicon <- get_sentiments("bing")
  sentiment <- tokens %>%
    inner_join(bing_lexicon, by = c("word" = "word")) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(net_sentiment = positive - negative) # Calculate net sentiment
  
  return(sentiment)
}

for (file_name in file_names) {
  file_path <- glue("C:/Users/noelm/Documents/{file_name}")
  text <- readLines(file_path, warn = FALSE)
  
  processed_text <- preprocess_text(text)
  
  sentiment_results <- analyze_sentiment(processed_text)
  
  cat("Sentiment analysis for", file_name, ":\n")
  print(sentiment_results)
  cat("\n")
}