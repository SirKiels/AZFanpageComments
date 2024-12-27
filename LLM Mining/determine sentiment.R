# Use LLM (dorian2b/vera or ALIENTELLIGENCE/sentimentanalyzer) to determine sentiment

# Packages
{
library(jsonlite)
library(httr)
library(purrr)
library(duckdb)
library(tidyverse)
}

# Functions
{
# dorian2b/vera ollama call function
dorian2b <- function(prompt) {
  # API endpoint URL (replace with the actual endpoint if different)
  url <- "http://localhost:11434/api/chat"
  
  # Data to be sent in the POST request
  data <- list(
    model = "dorian2b/vera",
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    ),
    stream = FALSE
  )
  # Convert the data to JSON format
  body <- toJSON(data, auto_unbox = TRUE)
  
  # Define the headers for the request
  headers <- add_headers(
    `Content-Type` = "application/json"
  )
  
  # Make the POST request
  response <- POST(url, headers, body = body)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON response
    content <- content(response, "text", encoding = "UTF-8")
    json_data <- fromJSON(content)
    
    # Extract and return the content from the response
    return(json_data$message$content)
  } else {
    stop(paste("Request failed with status code", status_code(response)))
  }
}

# ALIENTELLIGENCE/sentimentanalyzer call function
sentimentanalyzer <- function(prompt) {
  # API endpoint URL (replace with the actual endpoint if different)
  url <- "http://localhost:11434/api/chat"
  
  # Data to be sent in the POST request
  data <- list(
    model = "ALIENTELLIGENCE/sentimentanalyzer",
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    ),
    stream = FALSE
  )
  # Convert the data to JSON format
  body <- toJSON(data, auto_unbox = TRUE)
  
  # Define the headers for the request
  headers <- add_headers(
    `Content-Type` = "application/json"
  )
  
  # Make the POST request
  response <- POST(url, headers, body = body)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON response
    content <- content(response, "text", encoding = "UTF-8")
    json_data <- fromJSON(content)
    
    # Extract and return the content from the response
    return(json_data$message$content)
  } else {
    stop(paste("Request failed with status code", status_code(response)))
  }
}

# Call on LLM to identify players, staff, teams, etc
F1_Determine_Sentiment <- function(df, LLM) {
  P1_Sentiment <- 
    "Determine the sentiment of the comment from a Dutch football forum.
    Return sentiment as a 2 decimal value between -1 (negative) and +1 (positive).
    Only ever return a number.
    The comment:"

  # Create a new column to store results
  df$sentiment <- pmap_chr(df, function(raw_message, ...) {
    tryCatch({
      LLM(
        paste(P1_Sentiment, raw_message)
      )
    }, error = function(e) {
      return(NA)  # Return NA in case of error
    })
  })
  
  return(df)
}

# Calculate time estimate based on 50 rows
F2_Run_Estimate <- function(time_in_seconds, nr_of_batch_rows){
  
    time_in_seconds <- as.numeric(time_in_seconds)
    nr_of_batch_rows <- as.numeric(nr_of_batch_rows)
    seconds_per_comment <- time_in_seconds / 50
    estimate_in_seconds <- nr_of_batch_rows * seconds_per_comment
    estimate_in_minutes <- estimate_in_seconds / 60
    
    return(
      list(
      "Seconds per comment" = seconds_per_comment,
      "Batch estimate in seconds" = estimate_in_seconds,
      "Batch estimate in minutes" = estimate_in_minutes)
      )
}
}

# Call on DuckDB to create dataframe to enrich
{
# Create a connection to the database
db_file <- "azfanpage_comments_db.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = FALSE)	

query_col_id_comments <- 
"
  SELECT 
  *
  FROM 
  comments
"
comments_id_rawmessage <- as_tibble(dbGetQuery(con, query_col_id_comments))
comments_id_rawmessage_50 <- comments_id_rawmessage[1:50, ] |> 
  select(1, 2)
comments_id_rawmessage_2020 <- comments_id_rawmessage  |> 
  filter(year(created) == 2020) |>
	select(1, 2)

# Disconnect from the database
dbDisconnect(con)
}

# Run the function with progress as background job
#print(F2_Run_Estimate(92, 1760))
#$`Seconds per comment` 1.84
#`Batch estimate in seconds` 3238.4
#`Batch estimate in minutes` 53.97333
#sentiment <- F1_Determine_Sentiment(comments_id_rawmessage_50, dorian2b)

# Run the second LLM function with progress as background job
#print(F2_Run_Estimate(71, 1760))
#$`Seconds per comment` 1.42
#`Batch estimate in seconds` 2499.2
#`Batch estimate in minutes` 41.65333
# Actual time: 30:30
sentiment <- F1_Determine_Sentiment(comments_id_rawmessage_2020, sentimentanalyzer)

# Save the results to a file
saveRDS(sentiment, "sentiment_2020.rds")

# Use terminal command to not let machine go to sleep
# caffeinate

# Load result after running the script as a background job
sentiment <- readRDS("~/Documents/GitHub/AZFanpage_Comments/sentiment_2020.rds")

# After thoughts
{
#hist(as.numeric(sentiment$sentiment))
#comparison <- sentiment %>%
#  left_join(sentiment2, by = "id") |> 
#  select(-4)
# Manual check: second LLM sentimentanalyzer performs better.
}