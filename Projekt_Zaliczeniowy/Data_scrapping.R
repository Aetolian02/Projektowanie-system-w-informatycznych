library(httr)
library(rvest)
library(stringr)
library(fs)

#Konfiguracja oraz podanie user URL aby móc wejść na stronę
base_url <- "https://www.president.gov.ua/en/news/speeches?date-from=24-02-2022&date-to=25-05-2025&_token=En0mUxxxgMhlgBI4UfqcCoiJnunyJ8D7giBbZoqj"
user_agent_str <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36"
output_dir <- "speeches_Ukraine"
dir_create(output_dir)

#Uzyskanie ze strony z linkami do przemówień samych linków
get_speech_links <- function(page_num) {
  url <- paste0(base_url, "&page=", page_num)
  cat("Fetching listing page:", url, "\n")
  
  res <- GET(url, user_agent(user_agent_str))
  if (status_code(res) != 200) {
    warning("Failed to retrieve listing page ", page_num)
    return(character(0))
  }
  
  html <- content(res, as = "text", encoding = "UTF-8") %>% read_html()
  
  links <- html %>%
    html_nodes("div.item_stat_headline a") %>%
    html_attr("href") %>%
    unique()
  
  return(links)
}

#Zapisywanie przemowienia jako pliku txt
scrape_speech <- function(url) {
  cat("Scraping speech:", url, "\n")
  
  res <- GET(url, user_agent(user_agent_str))
  if (status_code(res) != 200) {
    warning("Failed to retrieve speech page: ", url)
    return(NULL)
  }
  
  html <- content(res, as = "text", encoding = "UTF-8") %>% read_html()
  
  title <- html %>%
    html_node("h1") %>%
    html_text(trim = TRUE)
  
  paragraphs <- html %>%
    html_nodes("div.article_content p") %>%
    html_text(trim = TRUE)
  
  if (length(paragraphs) == 0) {
    warning("No speech content found for: ", url)
    return(NULL)
  }
  
  content_text <- paste(paragraphs, collapse = "\n\n")
  
  list(title = title, content = content_text)
}

#Poprawne nazywanie utworzonego pliku
sanitize_filename <- function(filename) {
  filename <- str_replace_all(filename, "[/:*?\"<>|]", "_")
  filename <- str_squish(filename)
  if (nchar(filename) > 100) {
    filename <- substr(filename, 1, 100)
  }
  filename
}

#Wypisanie granic wyszukiwania (można zautomatyzować, ręcznie wpisałem 149 stron)
all_speech_links <- character()
page <- 1
max_pages <- 149

repeat {
  links <- get_speech_links(page)
  if (length(links) == 0) break
  
  all_speech_links <- unique(c(all_speech_links, links))
  if (page >= max_pages) break
  page <- page + 1
}

all_speech_links <- rev(all_speech_links)
total_speeches <- length(all_speech_links)
cat("Total speeches found:", total_speeches, "\n")

#Wykonanie kody i zapisanie wszystkich przemówień
for (i in seq_along(all_speech_links)) {
  speech_url <- all_speech_links[i]
  speech_data <- scrape_speech(speech_url)
  
  if (!is.null(speech_data)) {
    padded_num <- sprintf("%04d", i)
    clean_title <- sanitize_filename(speech_data$title)
    filename <- sprintf("Speech_%s_%s.txt", padded_num, clean_title)
    filepath <- path(output_dir, filename)
    
    writeLines(speech_data$content, filepath, useBytes = TRUE)
    cat("Saved:", filepath, "\n")
  }
}
