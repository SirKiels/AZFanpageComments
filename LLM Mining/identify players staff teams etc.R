# Use LLM (dorian2b/vera) to identify players, staff, teams, etc

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

# Call on LLM to identify players, staff, teams, etc
F1_Identify_Players_Staff_Teams_etc <- function(df) {
  P1_Identify <- 
    "Analyze the comment from a Dutch football forum.
    Identify the names of players, staff, teams, competetions, leagues.
    Return only the players, staff, teams, competitions, leagues you identify, separated by commas.
    If you cannot identify anything, return NA.
    ONLY return in English. NEVER any Chinese.
    The comment:"

  # Create a new column to store results
  df$pstl <- pmap_chr(df, function(raw_message, ...) {
    tryCatch({
      dorian2b(
        paste(P1_Identify, raw_message)
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
# Test time for 50 comments = 1:29 | 89 seconds / 50 = 1.78 sec per comment
# Estimated time for 2020 = 1760 * 1.64 = 3132.8/60 = 52.21333 minutes
# Actual time: 43:53 minutes | 8ish minutes faster than estimate
playerstaffetc <- F1_Identify_Players_Staff_Teams_etc(comments_id_rawmessage_2020)

# Save the results to a file
saveRDS(playerstaffetc, "playerstaffetc.rds")

# Use terminal command to not let machine go to sleep
# caffeinate

# Load result after running the script as a background job
#playerstaffetc <- readRDS("~/Documents/GitHub/AZFanpage_Comments/playerstaffetc.rds")

