# DuckDB workbench

# Load the DuckDB package
library(duckdb)

# Create a connection to the database
db_file <- "azfanpage_comments_db.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = FALSE)	

# Disconnect from the database
dbDisconnect(con)

# Merge example
{
# Example Fact Table: Scores
fact_table <- data.frame(
  id = 1:5,  # Primary key
  name_id = c(1, 2, 3, 4, 5),  # Foreign key referencing the dimension table
  score = c(95.5, 89.4, 72.8, 85.1, 90.0)
)

# Example Dimension Table: Names
dimension_table <- data.frame(
  name_id = 1:5,  # Primary key
  name = c("Alice", "Bob", "Charlie", "David", "Eve")
)

# Write the fact and dimension tables to the DuckDB database
dbWriteTable(con, "fact_scores", fact_table, overwrite = TRUE)
dbWriteTable(con, "dim_names", dimension_table, overwrite = TRUE)

# Query to join the fact and dimension tables
query <- "
  SELECT 
    fs.id AS fact_id,
    dn.name AS person_name,
    fs.score
  FROM 
    fact_scores fs
  JOIN 
    dim_names dn
  ON 
    fs.name_id = dn.name_id
"

# Execute the query and display the results
result <- dbGetQuery(con, query)
print("Joined Result:")
print(result)

# Clean up: Disconnect from the database
dbDisconnect(con)
}

# Query * as tibble
{
query_links <- 
"
  SELECT 
  *
  FROM 
  links
"

query_comments <- 
"
  SELECT 
  *
  FROM 
  comments
"
# Execute the query and display the results
as_tibble(dbGetQuery(con, query_links))
as_tibble(dbGetQuery(con, query_comments))
}

# Get column titles
{
  query_col_titles <- 
"
  SELECT column_name
  FROM information_schema.columns
  WHERE table_name = 'comments'
"
dbGetQuery(con, query_col_titles)
}

# Extract column + id from [comments] for enrichment
{
query_col_id_comments <- 
"
  SELECT 
  id, raw_message
  FROM 
  comments
"
comments_id_rawmessage <- as_tibble(dbGetQuery(con, query_col_id_comments))
}
