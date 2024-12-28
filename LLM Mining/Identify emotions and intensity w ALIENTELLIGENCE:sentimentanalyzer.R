# Use LLM (ALIENTELLIGENCE/sentimentanalyzer) to determine Sentiment Intensity Analysis

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
# Seemed promising but won't obey overall.
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

# New run. Back to basics to work with sentimentanalyzer
F2_Sentimentanalyzer_basic <- function(df, LLM) {
  Prompt1 <- 
"Return primary emotion and intensity (1-10) in the Dutch football comment below
Only return emotion and intensity so no introductions or explanations
Return format: emotion,#;emotion,#; etc
Return emotions as English nouns so no other language
Check for format and no duplicates before returning
The comment:"

  # Create a new column to store results
  df$emotion_intensity <- pmap_chr(df, function(raw_message, ...) {
    tryCatch({
      LLM(
        paste(Prompt1, raw_message)
      )
    }, error = function(e) {
      return(NA)  # Return NA in case of error
    })
  })
  
  return(df)
}

}

# Call on DuckDB to create dataframe to enrich
{
# Create a connection to the database
db_file <- "/Users/samuel/Documents/GitHub/AZFanpageComments/azfanpage_comments_db.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = FALSE)	

query_col_id_comments <- 
"
  SELECT 
  *
  FROM 
  comments
"
comments_id_rawmessage <- as_tibble(dbGetQuery(con, query_col_id_comments))
comments_id_rawmessage_50 <- comments_id_rawmessage[1:50, ] |> select(1, 2)
comments_id_rawmessage_2020 <- comments_id_rawmessage  |> 
  filter(year(created) == 2020) |>
	select(1, 2)

# Disconnect from the database
dbDisconnect(con)
}

# Load result after running the script as a background job
#emotions <- readRDS("~/Documents/GitHub/AZFanpage_Comments/emotions.rds")

# Run the function with progress as background job
#print(F3_Run_Estimate(124, 1760))
# `Seconds per comment` 2.48
# `Batch estimate in seconds` 4364.8
# `Batch estimate in minutes` 72.74667
# Actual time: 77
emotions_intensity <- F2_Sentimentanalyzer_basic(comments_id_rawmessage_2020, sentimentanalyzer)

# Save the results to a file
saveRDS(emotions_intensity, "emotions_intensity.rds")

# Use terminal command to not let machine go to sleep
# caffeinate

# Save the results to a file
#readRDS("~/Documents/GitHub/AZFanpageComments/LLM Mining/emotions_intensity.rds")
