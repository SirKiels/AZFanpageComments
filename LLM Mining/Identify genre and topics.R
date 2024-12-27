# Use LLM (dorian2b/vera) to identify genres and topics within genres

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

# Call on LLM to identify genres


F1_Determine_Genre <- function(df) {
  genres <- "Praise, Criticism, Analysis, Predictions, Speculations, Discussion, Comparison, Disputes, Proposal, Humor, Emotional outburst, Suggestion, Questions/Inquiries, Update, Celebration, Complaints, Apology. The comment:"
  
  P1_Determine_Genre <- 
"Analyze the comment from a Dutch football forum.
Identify which of the genres fits best.
Return as few genres as possible. If none fit return NA.
Only return the genres or NA, nothing else ever.
The genres:"

  # Create a new column to store results
  df$genre <- map_chr(df$raw_message, ~ {
    tryCatch({
      dorian2b(paste(P1_Determine_Genre, genres,  .))
    }, error = function(e) {
      return(NA)  # Return NA in case of error
    })
  })
  
  return(df)
}

F2_Determine_Topics <- function(df) {
  P1_Determine_Topics <- 
    "Analyze the comment from a Dutch football forum.
    Identify topics in the comment within the given genre.
    Do not repeat the given genre.
    Specific people, teams, competitions cannot be topics.
    The topics you identify should be extremely specific and detailed.
    Return only the topics you identify, separated by commas.
    ONLY return topics in English. NEVER any Chinese.
    The genre:"

  # Create a new column to store results
  df$topics <- pmap_chr(df, function(raw_message, genre, ...) {
    tryCatch({
      dorian2b(
        paste(P1_Determine_Topics, genre, ".The comment:", raw_message)
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
db_file <- "azfanpage_comments_db.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = FALSE)	

query_col_id_comments <- 
"
  SELECT 
  id, raw_message, created
  FROM 
  comments
"
comments_id_rawmessage <- as_tibble(dbGetQuery(con, query_col_id_comments))
comments_id_rawmessage_50 <- comments_id_rawmessage[1:50, ]
comments_id_rawmessage_2020 <- comments_id_rawmessage  |> 
  filter(year(created) == 2020)
# Disconnect from the database
dbDisconnect(con)
}

# Run the function with progress as background job
# Test time for 50 comments = 2:22 | 142 seconds / 50 = 2.84 sec per comment
# Estimated time for 2020 = 1760 * 1.64 = 4998.4/60 = 83.30667 minutes | 1 hour 23 minutes
# Actual time: 79:51 minutes| 3 minutes shorter than estimate
genre <- F1_Determine_Genre(comments_id_rawmessage_2020)
topics <- F2_Determine_Topics(genre)

#
# Save the results to a file
saveRDS(topics, "comments_gt_2020.rds")

# Use terminal command to not let machine go to sleep
# caffeinate

