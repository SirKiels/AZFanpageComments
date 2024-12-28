Little scraping and text-mining project using the AZFanpage comments.

## Goals

- technical skills: scraping
- SQL skills: working with a SQL db in the form of a DuckDB
- Text mining:
	- cleaning, prepping, enriching comments
	- quanteda for tokenizing etc
	- ollama for genre, topics, sentiment etc
- Statistics:
	- Extract insights from comments using
		- FA, Regressions, SEM
		- Possibly do ML to dabble in prediction

## Project tasks and status

-- Scrapes
- Scrape historical comments and store in duckdb ✓
- Automate future scrapes 🛑

--- Text mining

-- Develop and test LLM analysis for:
- Genre & Topic ✓
- PSTETC ✓
- Emotions ✓
- Sentiment & Emotional intensity ✓
- Sentiment & Emotional intensity v2 ✓

-- Experimental run on 2020 data:
- Run batches as background jobs to create 2020 set ✓
- Tokenize and clean-up in preparation for storage 🛑
- Store in duckdb 🛑
- Develop and test statistical scripts 🛑
