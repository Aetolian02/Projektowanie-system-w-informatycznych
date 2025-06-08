#install.packages(c("tm", "tidyverse", "wordcloud", "tidytext", "textdata", "scales", "ggthemes", "RColorBrewer"))

# Wymagane pakiety
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(textdata)
library(RColorBrewer)
library(ggthemes)
library(scales)

# Wczytanie dokumentów z folderu
folder_path <- "C:/Users/marci/OneDrive/Pulpit/ProjektPSI/speeches_Ukraine" 
corpus <- VCorpus(DirSource(folder_path, encoding = "UTF-8"), readerControl = list(language = "en"))

# Czyszczenie tekstu
clean_corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

#. Macierz dokumentów i częstości (TF)
dtm <- DocumentTermMatrix(clean_corpus)
term_matrix <- as.matrix(dtm)
term_freq <- colSums(term_matrix)
term_freq_df <- data.frame(term = names(term_freq), freq = term_freq) %>%
  arrange(desc(freq))

# Chmura słów – TF
set.seed(123)
wordcloud(words = term_freq_df$term, freq = term_freq_df$freq,
          min.freq = 500, max.words = 200, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"),
          scale = c(4, 0.5))

# Macierz TF-IDF i chmura
dtm_tfidf <- weightTfIdf(dtm)
tfidf_matrix <- as.matrix(dtm_tfidf)
tfidf <- colSums(tfidf_matrix)
tfidf_df <- data.frame(term = names(tfidf), tfidf = tfidf) %>%
  arrange(desc(tfidf))

set.seed(456)
wordcloud(words = tfidf_df$term, freq = tfidf_df$tfidf,
          min.freq = 0.7, max.words = 16, random.order = FALSE,
          colors = brewer.pal(8, "Set1"),
          scale = c(4, 0.5))

# Połączenie wszystkich dokumentów w jeden tekst
full_text <- paste(sapply(clean_corpus, content), collapse = " ")

# Przygotowanie do analizy sentymentu
text_df <- tibble(text = full_text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# Analiza sentymentu – Loughran
loughran_sentiment <- text_df %>%
  inner_join(get_sentiments("loughran"), relationship = "many-to-many") %>%
  count(sentiment, sort = TRUE)

ggplot(loughran_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.3) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Loughran: Sentyment całego korpusu", x = "Sentyment", y = "Liczba słów") +
  theme_minimal(base_size = 14)

# Analiza sentymentu – Bing
bing_sentiment <- text_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, sort = TRUE)

ggplot(bing_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.3) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Bing: Sentyment całego korpusu", x = "Sentyment", y = "Liczba słów") +
  theme_minimal(base_size = 14)

# Analiza sentymentu – NRC (emocje)
nrc_sentiment <- text_df %>%
  inner_join(get_sentiments("nrc"),relationship = "many-to-many") %>%
  count(sentiment, sort = TRUE)

ggplot(nrc_sentiment, aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.2) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "NRC: Emocje w całym korpusie", x = "Emocja", y = "Liczba słów") +
  theme_minimal(base_size = 14)

# Analiza sentymentu – AFINN (wartości numeryczne)
afinn_sentiment <- text_df %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(total_sentiment = sum(value),
            mean_sentiment = mean(value),
            word_count = n())

print(afinn_sentiment)

ggplot(afinn_sentiment, aes(x = "Total Sentiment", y = total_sentiment)) +
  geom_col(fill = "steelblue") +
  labs(title = "Łączny sentyment (AFINN)",
       x = NULL, y = "Suma sentymentu") +
  theme_minimal(base_size = 15)

