# Clean up files and send to duckdb

# Directory
{
#getwd()
#setwd("~/Users/samuel/Documents/GitHub/AZFanpageComments")
}

# Packages
{
	library(dplyr)
	library(stringr)
	library(tidyr)
}

# DuckDB overview
{
# Create a connection to the database
db_file <- "azfanpage_comments_db.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = FALSE)

# Overview
tables <- dbListTables(con)
as_tibble(tables)
remove(tables)

# Disconnect from the database
dbDisconnect(con)
}

# Emotions
emotions <- readRDS("~/Documents/GitHub/AZFanpageComments/LLM Mining/Data from runs/2020/emotions_2020.rds")
{

# Cleanup
{
	emotions_db <- emotions |> 
		filter(emotion_intensity != "") |>
		mutate(
	    emotion_intensity = str_remove_all(emotion_intensity, "[\\[\\]]"), # Remove square brackets
	    emotion_intensity = str_split(emotion_intensity, "; ") # Split into list of strings
	  ) |> 
	  unnest(emotion_intensity) |> # Expand the list into rows
	  separate(emotion_intensity, into = c("emotion", "emotion_intensity"), sep = ", ", convert = TRUE) |> 
	  mutate(
	    emotion = str_to_lower(emotion), # Convert to lowercase
	    emotion_intensity = as.numeric(emotion_intensity) # Ensure numeric values
	  ) |> 
		select(-2) # Drop comment itself. Id = key
	
	remove(emotions)
}

# Send to DuckDB
{
# Create a connection to the database
db_file <- "azfanpage_comments_db.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = FALSE)	

# Write the data frame to a table in DuckDB
dbWriteTable(con, "emotions", emotions_db, overwrite = TRUE)

# Check
result <- as_tibble(dbGetQuery(con, "SELECT * FROM emotions"))
result
remove(result, emotions_db)

# Disconnect from the database
dbDisconnect(con)
}
}

# genre_topics into genre and topics
genre_topics <- readRDS("~/Documents/GitHub/AZFanpageComments/LLM Mining/Data from runs/2020/comments_gt_2020.rds")
{

# Cleanup and break up into genre and topic
{
	genre_db <- genre_topics |> 
		select(-2, -3, -5) |>  # drop columns. Id = key
	  mutate(
	    genre = str_replace_all(genre, "\\s+", ", "),          # Replace spaces with ", "
	    genre = str_replace_all(genre, ",+", ","),            # Replace multiple commas with a single comma
	  			) |> 
		separate_rows(genre, sep = ",") |>  # Split on a single comma
		mutate(
    	genre = str_trim(genre),       # Trim spaces from `genre`
  				) |> 
		mutate(
    	genre = tolower(genre),       # Convert `genre` to lowercase
  				)
	
		topics_db <- genre_topics |> 
		select(-2, -3, -4) |>  # drop columns. Id = key
	  mutate(
	    topics = str_replace_all(topics, "\\s+", ", "),        # Replace spaces with ", "
	    topics = str_replace_all(topics, ",+", ",")           # Replace multiple commas with a single comma
	  			) |> 
		separate_rows(topics, sep = ",") |>  # Split on a single comma
		mutate(
    	topics = str_trim(topics)     # Trim spaces from `topics`
  				) |> 
		mutate(
    	topics = tolower(topics)      # Convert `topics` to lowercase
  				)
	
	remove(genre_topics)
}

# Send to DuckDB
{
# Create a connection to the database
db_file <- "azfanpage_comments_db.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = FALSE)	

# Write the data frame to a table in DuckDB
dbWriteTable(con, "genre", genre_db, overwrite = TRUE)
dbWriteTable(con, "topics", topics_db, overwrite = TRUE)

# Check
result <- as_tibble(dbGetQuery(con, "SELECT * FROM genre"))
result2 <- as_tibble(dbGetQuery(con, "SELECT * FROM topics"))
result
result2

remove(result, result2, genre_db, topics_db)

# Disconnect from the database
dbDisconnect(con)
}
}

# Sentiment
sentiment <- readRDS("~/Documents/GitHub/AZFanpageComments/LLM Mining/Data from runs/2020/sentiment_2020.rds")
{

# Cleanup
{
	sentiment_db <- sentiment |> 
		select(-2) |> 
		mutate(sentiment = trimws(sentiment),         # Remove any leading/trailing whitespace
         sentiment = as.numeric(sentiment)) |>  # Convert to numeric
  	filter(!is.na(sentiment)) 									# Filter out rows with NA values	
		
	remove(sentiment)
}

# Send to DuckDB
{
# Create a connection to the database
db_file <- "azfanpage_comments_db.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = FALSE)	

# Write the data frame to a table in DuckDB
dbWriteTable(con, "sentiment", sentiment_db, overwrite = TRUE)

# Check
result <- as_tibble(dbGetQuery(con, "SELECT * FROM sentiment"))
result
remove(result, sentiment_db)

# Disconnect from the database
dbDisconnect(con)
}
}

# playerstaffetc
# IN PROGRESS
playerstaffetc <- readRDS("~/Documents/GitHub/AZFanpageComments/LLM Mining/Data from runs/2020/playerstaffetc_2020.rds")
{

# Cleanup
{
	playerstaffetc_db <- playerstaffetc |> 
		select(-2) |> 
		mutate(sentiment = trimws(sentiment),         # Remove any leading/trailing whitespace
         sentiment = as.numeric(sentiment)) |>  # Convert to numeric
  	filter(!is.na(sentiment)) 									# Filter out rows with NA values	
		
	remove(sentiment)
}

# Send to DuckDB
{
# Create a connection to the database
db_file <- "azfanpage_comments_db.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = FALSE)	

# Write the data frame to a table in DuckDB
dbWriteTable(con, "sentiment", sentiment_db, overwrite = TRUE)

# Check
result <- as_tibble(dbGetQuery(con, "SELECT * FROM sentiment"))
result
remove(result, sentiment_db)

# Disconnect from the database
dbDisconnect(con)
}
}