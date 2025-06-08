#install.packages(c("tidyverse", "tidytext", "tm", "textstem", "SnowballC", "readr", "stringr","cluster", "factoextra", "topicmodels","ggplot2", "wordcloud", "RColorBrewer", "scales", "reshape2"))


# Wczytaj potrzebne pakiety
library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(textstem)
library(cluster)
library(factoextra)
library(SnowballC)
library(ggplot2)
library(wordcloud)

# Folder z plikami
folder_path <- "C:/Users/marci/OneDrive/Pulpit/ProjektPSI/speeches_Ukraine"
files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
files <- sort(files)

# Wczytanie pliku
texts <- map_chr(files, read_file)
doc_df <- tibble(doc_id = basename(files), text = texts)

data(stop_words)

cleaned_tokens <- doc_df %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$")) %>%
  mutate(word = lemmatize_words(word))  # lematyzacja

dtm <- cleaned_tokens %>%
  count(doc_id, word) %>%
  cast_dtm(document = doc_id, term = word, value = n)

# Uproszczenie 
dtm_matrix <- as.matrix(dtm)
dtm_dist <- dist(scale(dtm_matrix))
dtm_pca <- prcomp(dtm_matrix, scale. = TRUE)

# liczba tematów, przykładowo 5
lda_model <- LDA(dtm, k = 5, control = list(seed = 123))
topics <- tidy(lda_model, matrix = "beta")

# Najważniejsze słowa dla każdego tematu
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Wykres słów-tematów
ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top słowa dla każdego tematu (LDA)",
       x = NULL, y = "Beta (prawdopodobieństwo)") +
  theme_minimal(base_size = 14)

