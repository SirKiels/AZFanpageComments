# Scrape (reviewed, cleaned, simplified) and write to DuckDB
# One-off runs across the board

# To-dos
{
# function to check for new comments in historical scrapes
# Work through highlights marked with #!
# Weekly run that checks past 2 months for new comments and scrapes them
}

# Packages
{
library(httr)
library(tidyverse)
library(disqusR)
library(duckdb)
}

# Functions
{

# Function to fetch thread information for a given URL
F1_URL <- function(url) {
return(threads("list", forum = "azfanpage", thread = paste0("link:", url)))
  }

# Function to fetch comments for a given thread
F2_Fetch_Comments_ThreadID <- function(threadId) {
return(posts("list", thread = threadId))
}

# Function to process and create a data frame from the fetched comments
F3_Comments_To_DF <- function(postslist, link_text) {
    if (length(postslist$raw_message) > 0) {
      return(data.frame(
        id = postslist$id,
        raw_message = postslist$raw_message,
        author_name = postslist$author.name,
        created = postslist$createdAt,
        LinkText = link_text
      ))
    } else {
      warning("No posts found for thread ", postslist$id)
      return(NULL)
    }
}

# Function to iterate threads in URL::F1-F3
F4_Final_Iterator <- function(urls) {
    result_df <- data.frame()  # Initialize an empty data frame to store results
    
    for (url in urls) {
      thread_info <- tryCatch(
          F1_URL(url),
        error = function(e) {
          warning("Error fetching thread information for URL: ", url)
          return(NULL)
        }
      )
      
      if (!is.null(thread_info) && length(thread_info) > 0 && !is.null(thread_info$id)) {
        thread_id <- thread_info$id
        comments <- tryCatch(
          F2_Fetch_Comments_ThreadID(thread_id),
          error = function(e) {
            warning("Error fetching comments for thread ", thread_id)
            return(NULL)
          }
        )
        
        if (!is.null(comments) && length(comments$raw_message) > 0) {
          processed_data <- F3_Comments_To_DF(comments, link_text = url)
          if (!is.null(processed_data)) {
            result_df <- rbind(result_df, processed_data)
          }
        } else {
          warning("No posts found for thread ", thread_id)
        }
      } else {
        warning("Unable to fetch thread information for URL: ", url)
      }
    }
    
    return(result_df)
}

# An improved version by GPT with delay
F4_Final_Iterator_delay <- function(urls) {
  results_list <- list()
  for (i in seq_along(urls)) {
    url <- urls[i]
    message(sprintf("Processing URL %d/%d: %s", i, length(urls), url))
    Sys.sleep(0.1) # Add a delay of 0.1 second between requests
    
    thread_info <- tryCatch(
      F1_URL(url),
      error = function(e) {
        warning(sprintf("Error fetching thread information for URL '%s': %s", url, e$message))
        return(NULL)
      }
    )
    
    if (is.null(thread_info) || !is.list(thread_info) || is.null(thread_info$id)) {
      warning(sprintf("No valid thread information found for URL: %s", url))
      next
    }
    
    thread_id <- thread_info$id
    comments <- tryCatch(
      F2_Fetch_Comments_ThreadID(thread_id),
      error = function(e) {
        warning(sprintf("Error fetching comments for thread ID '%s': %s", thread_id, e$message))
        return(NULL)
      }
    )
    
    if (is.null(comments) || length(comments$raw_message) == 0) {
      warning(sprintf("No comments found for thread ID: %s", thread_id))
      next
    }
    
    processed_data <- F3_Comments_To_DF(comments, link_text = url)
    if (!is.null(processed_data)) {
      results_list[[length(results_list) + 1]] <- processed_data
    }
  }
  
  result_df <- dplyr::bind_rows(results_list, .id = "source")
  return(result_df)
}

# Function that cleans up Title and Content columns
F5_clean_up_values <- function(data) {
  data$title <- gsub("&#\\d+;", "", data$title) # Remove HTML entities
  data$title <- gsub("<[^>]+>", "", data$title) # Remove HTML tags
  
  data$content <- gsub("&#\\d+;", "", data$content) # Remove HTML entities
  data$content <- gsub("<[^>]+>", "", data$content) # Remove HTML tags
  
  return(data)
}

# Function to extract meta data and map to tibble
F6_extract_data <- function(response) {
  # Check if the response is successful (category is "Success")
  if (http_status(response)$category == "Success") {
    # Extract content from the response
    content <- content(response, "parsed")
    
    # Extract relevant information from the content
    data <- content %>%
      purrr::map_df(~ tibble(
        id = .x$id,
        title = .x$title$rendered,
        date = .x$date,
        content = .x$content$rendered,
        author = .x$author,
        URL = .x$link
      ))
    
    return(data)
  } else {
    stop("Error: Unable to fetch data from the API.")
  }
}

# Function to call and iterate API call in monthly chunks within defined year::F6
F7_iterate_over_month <- function(api_url, start_year) {
  # Initialize an empty data frame to store the results
  all_data <- data.frame()
  
  # Iterate over each month in the year
  for (month in 1:12) {
    # Create start and end dates for each month
    start_date <- paste0(start_year, "-", sprintf("%02d", month), "-01T00:00:00")
    end_date <- ifelse(month == 12, paste0(start_year + 1, "-01-01T00:00:00"), paste0(start_year, "-", sprintf("%02d", month + 1), "-01T00:00:00"))
    
    # Set up parameters
    query_params <- list(
      after = start_date,
      before = end_date,
      per_page = 100 #! Is this right?
    )
    
    # Make the GET request
    response <- GET(url = api_url, query = query_params)
    
    # Extract data using the F6_extract_data function
    data <- F6_extract_data(response)
    
    # Append the data to the results
    all_data <- bind_rows(all_data, data)
  }
  
  return(all_data)
}

}

# Prep for all scrapes
{
	pubkey <- "xxxx"
	seckey <- "xxxx"
  api_url <- "xxxx"
	
# DuckDB prep
# Create a connection to the database
db_file <- "azfanpage_comments_db.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = FALSE)	

}

# Run 2020
{
# Set up API url + parameters
start_date <- "2020-01-01T00:00:00"
end_date <- "2020-12-31T00:00:00"
start_year <- 2020
query_params <- list(
	after = start_date,
	before = end_date,
	per_page = 100 #! Is this right?
)
# Make the GET request
response <- GET(url = api_url, query = query_params)
  
# Run the functions, double check for duplicates, clean up of HTML leftovers
links_table <- F7_iterate_over_month(api_url, start_year)
links_table <- distinct(links_table, URL, .keep_all = TRUE)
links_table <- F5_clean_up_values(links_table)
 
# Use the links_table dataframe for to iterate comment scraping using F4_Final_Iterator
# Note: don't overdo it on how often you call API. There's a rate limit at play
result <- F4_Final_Iterator(links_table$URL)

# Move links_table and results to DuckDB--URL is Key
dbWriteTable(con, "links", links_table, append = TRUE)
dbWriteTable(con, "comments", result, append = TRUE)

#Clean up
remove(start_date, end_date, start_year, query_params, response, links_table, result)
}

# Run 2021
{
# Set up API url + parameters
start_date <- "2021-01-01T00:00:00"
end_date <- "2021-12-31T00:00:00"
start_year <- 2021
query_params <- list(
	after = start_date,
	before = end_date,
	per_page = 100 #! Is this right?
)
# Make the GET request
response <- GET(url = api_url, query = query_params)
  
# Run the functions, double check for duplicates, clean up of HTML leftovers
links_table <- F7_iterate_over_month(api_url, start_year)
links_table <- distinct(links_table, URL, .keep_all = TRUE)
links_table <- F5_clean_up_values(links_table)
 
# Use the links_table dataframe for to iterate comment scraping using F4_Final_Iterator
# Note: don't overdo it on how often you call API. There's a rate limit at play
result <- F4_Final_Iterator(links_table$URL)

# Move links_table and results to DuckDB--URL is Key
dbWriteTable(con, "links", links_table, append = TRUE)
dbWriteTable(con, "comments", result, append = TRUE)

#Clean up
remove(start_date, end_date, start_year, query_params, response, links_table, result)
}

# Run 2022
{
# Set up API url + parameters
start_date <- "2022-01-01T00:00:00"
end_date <- "2022-12-31T00:00:00"
start_year <- 2022
query_params <- list(
	after = start_date,
	before = end_date,
	per_page = 100 #! Is this right?
)
# Make the GET request
response <- GET(url = api_url, query = query_params)
  
# Run the functions, double check for duplicates, clean up of HTML leftovers
links_table <- F7_iterate_over_month(api_url, start_year)
links_table <- distinct(links_table, URL, .keep_all = TRUE)
links_table <- F5_clean_up_values(links_table)
 
# Use the links_table dataframe for to iterate comment scraping using F4_Final_Iterator
# Note: don't overdo it on how often you call API. There's a rate limit at play
result <- F4_Final_Iterator(links_table$URL)

# Move links_table and results to DuckDB--URL is Key
dbWriteTable(con, "links", links_table, append = TRUE)
dbWriteTable(con, "comments", result, append = TRUE)

#Clean up
remove(start_date, end_date, start_year, query_params, response, links_table, result)
}

# Run 2023
{
# Set up API url + parameters
start_date <- "2023-01-01T00:00:00"
end_date <- "2023-12-31T00:00:00"
start_year <- 2023
query_params <- list(
	after = start_date,
	before = end_date,
	per_page = 100 #! Is this right?
)
# Make the GET request
response <- GET(url = api_url, query = query_params)
  
# Run the functions, double check for duplicates, clean up of HTML leftovers
links_table <- F7_iterate_over_month(api_url, start_year)
links_table <- distinct(links_table, URL, .keep_all = TRUE)
links_table <- F5_clean_up_values(links_table)
 
# Use the links_table dataframe for to iterate comment scraping using F4_Final_Iterator
# Note: don't overdo it on how often you call API. There's a rate limit at play
result <- F4_Final_Iterator(links_table$URL)

# Move links_table and results to DuckDB--URL is Key
dbWriteTable(con, "links", links_table, append = TRUE)
dbWriteTable(con, "comments", result, append = TRUE)

#Clean up
remove(start_date, end_date, start_year, query_params, response, links_table, result)  
}

# Run 2024 -- excluding December
{
# Set up API url + parameters
start_date <- "2024-01-01T00:00:00"
end_date <- "2024-11-31T00:00:00"
start_year <- 2024
query_params <- list(
	after = start_date,
	before = end_date,
	per_page = 100 #! Is this right?
)
# Make the GET request
response <- GET(url = api_url, query = query_params)
  
# Run the functions, double check for duplicates, clean up of HTML leftovers
links_table <- F7_iterate_over_month(api_url, start_year)
links_table <- distinct(links_table, URL, .keep_all = TRUE)
links_table <- F5_clean_up_values(links_table)
 
# Use the links_table dataframe for to iterate comment scraping using F4_Final_Iterator
# Note: don't overdo it on how often you call API. There's a rate limit at play
result <- F4_Final_Iterator(links_table$URL)

# Move links_table and results to DuckDB--URL is Key
dbWriteTable(con, "links", links_table, append = TRUE)
dbWriteTable(con, "comments", result, append = TRUE)

#Clean up
remove(start_date, end_date, start_year, query_params, response, links_table, result)  
}

# Disconnect from the database
dbDisconnect(con)

# Final clean-up
remove(api_url, db_file, pubkey, query_links, seckey)
