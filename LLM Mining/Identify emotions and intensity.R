# Use LLM (dorian2b/vera) to determine Sentiment Intensity Analysis

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

# Call on LLM to identify sentiment intensity
F1_Primary_Emotions <- function(df, LLM) {
  P1_emotion_primary <- 
"Identify which of the primary emotion in the comment below.
Return only 1 word: positive, negative, neutral/complex.
Provide no introductions or explanations.
The comment:"

  # Create a new column to store results
  df$emotion_primary <- pmap_chr(df, function(raw_message, ...) {
    tryCatch({
      LLM(
        paste(P1_emotion_primary, raw_message)
      )
    }, error = function(e) {
      return(NA)  # Return NA in case of error
    })
  })
  
  return(df)
}

F1_1_Identify_Emotions_spec <- function(df, LLM) {
  P1_emotions_spec <- 
"Within the given primary emotion, identify which of these emotions you see in the comment:
Excitement, Pride, Relief, Admiration, Joy, Optimism, Nostalgia, Surprise, Amusement, Gratitude, Frustration, Disappointment, Anger, Jealousy, Sadness, Anxiety, Fear, Confusion, Hope, Determination, Respect, Euphoria, Contentment, Awe, Bitterness, Regret, Anticipation, Contempt, Sympathy, Resentment, Triumph, Agitation, Envy, Loneliness, Despair, Suspense, Grief, Trust, Impatience, Vulnerability, Elation, Dissatisfaction, Curiosity, Indifference, Shame, Guilt, Empowerment, Embarrassment, Satisfaction, Inspiration, Antagonism, Loyalty, Apprehension, Nervousness, Reluctance, Passion, Self-doubt.
Return only these emotions; if no match return 'NA'.
The primary emotion:"

  # Create a new column to store results
  df$emotions_specific <- pmap_chr(df, function(raw_message, emotion_primary, ...) {
    tryCatch({
      LLM(
        paste(P1_emotions_spec, emotion_primary, ".The comment:", raw_message)
      )
    }, error = function(e) {
      return(NA)  # Return NA in case of error
    })
  })
  
  return(df)
}

F1_2_Emotion_Intensity <- function(df, LLM) {
  P1_emotions_intensity <- 
"For each of the emotions, return how intense they are in the comment.
Return 1 number per emotion between 0 (low) and 10 (high).
Return in this format: emotion, #; emotion, #; etc
Do not return explanations or intros.
The emotions:"

  # Create a new column to store results
df$emotions_intensity <- pmap_chr(df, function(raw_message, emotions_specific, ...) {
  if (emotions_specific == "NA") {
    return(NA)  # Skip rows where emotions_specific is NA
  }
  
  tryCatch({
    LLM(
      paste(P1_emotions_intensity, emotions_specific, ".The comment:", raw_message)
    )
  }, error = function(e) {
    return(NA)  # Return NA in case of error
  })
})
  
  return(df)
}

# Archived functions
{
# Improved promt for call on LLM to identify sentiment intensity
F2_Identify_Emotions <- function(df, LLM) {
  P1_emotion_intensity <- 
    "Return emotions and intensity between 0 (low) and 1 (high).
    Return in English, in this format: [emotion, .##; emotion, 0.##; ...]
    Never return intro or explanations.
  	The comment:"

  # Create a new column to store results
  df$emotion_intensity <- pmap_chr(df, function(raw_message, ...) {
    tryCatch({
      LLM(
        paste(P1_emotion_intensity, raw_message)
      )
    }, error = function(e) {
      return(NA)  # Return NA in case of error
    })
  })
  
  return(df)
}

# Calculate time estimate based on 50 rows
F3_Run_Estimate <- function(time_in_seconds, nr_of_batch_rows){
  
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

# Emotions only test
F4_Identify_Emotions <- function(df, LLM) {
  P1_emotion_intensity <- 
    "Return emotions from a Dutch football website comment.
    Return emotions in English separated by a comma.
    Return nothing else but emotion. No intro, no explanations, nothing else.
  	The comment:"

  # Create a new column to store results
  df$Sentiment_intensity <- pmap_chr(df, function(raw_message, ...) {
    tryCatch({
      LLM(
        paste(P1_emotion_intensity, raw_message)
      )
    }, error = function(e) {
      return(NA)  # Return NA in case of error
    })
  })
  
  return(df)
}
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

# Run the function with progress as background job
# print(F3_Run_Estimate(150, 1760))
# `Seconds per comment` 3
# `Batch estimate in seconds` 5280
# `Batch estimate in minutes` 88
# Actual time: 77
emotions_p <- F1_Primary_Emotions(comments_id_rawmessage_2020, dorian2b)
emotions_p_s <- F1_1_Identify_Emotions_spec(emotions_p, dorian2b)
emotions_p_s_i <- F1_2_Emotion_Intensity(emotions_p_s, dorian2b)

# Save the results to a file
saveRDS(emotions_only, "emotions_only.rds")

# Use terminal command to not let machine go to sleep
# caffeinate

# Load result after running the script as a background job
#emotions <- readRDS("~/Documents/GitHub/AZFanpage_Comments/emotions.rds")
