library(httr)
library(rvest)
library(stringr)
library(fs)

# Configuration
base_url <- "https://www.president.gov.ua/en/news/speeches?date-from=24-02-2022&date-to=25-05-2025&_token=En0mUxxxgMhlgBI4UfqcCoiJnunyJ8D7giBbZoqj"
user_agent_str <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36"
output_dir <- "speeches_txt"
dir_create(output_dir)  # Create directory if doesn't exist

# Function to get all speech URLs on a single listing page
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

# Function to scrape title and speech content from a speech page URL
scrape_speech <- function(url) {
  cat("Scraping speech:", url, "\n")
  
  res <- GET(url, user_agent(user_agent_str))
  if (status_code(res) != 200) {
    warning("Failed to retrieve speech page: ", url)
    return(NULL)
  }
  
  html <- content(res, as = "text", encoding = "UTF-8") %>% read_html()
  
  # Extract title (usually <h1>)
  title <- html %>%
    html_node("h1") %>%
    html_text(trim = TRUE)
  
  # Extract speech paragraphs inside div.article_content
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

# Function to sanitize filenames
sanitize_filename <- function(filename) {
  filename <- str_replace_all(filename, "[/:*?\"<>|]", "_")
  filename <- str_trim(filename)
  if (nchar(filename) > 100) {
    filename <- substr(filename, 1, 100)
  }
  filename
}

# Main script
all_speech_links <- character()
page <- 1
max_pages <- 149

repeat {
  links <- get_speech_links(page)
  if (length(links) == 0) {
    cat("No more speeches found, stopping at page", page, "\n")
    break
  }
  
  all_speech_links <- unique(c(all_speech_links, links))
  
  if (page >= max_pages) {
    cat("Reached max page limit:", max_pages, "\n")
    break
  }
  page <- page + 1
}

cat("Total speeches found:", length(all_speech_links), "\n")

# Download and save each speech as a .txt file
for (speech_url in all_speech_links) {
  speech_data <- scrape_speech(speech_url)
  
  if (!is.null(speech_data)) {
    filename <- sanitize_filename(speech_data$title)
    filepath <- path(output_dir, paste0(filename, ".txt"))
    
    # Save the speech content to a text file
    writeLines(speech_data$content, filepath, useBytes = TRUE)
    
    cat("Saved:", filepath, "\n")
  }
}
