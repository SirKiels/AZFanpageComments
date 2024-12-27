# Ollama local test
#install.packages("httr")
#install.packages("reticulate")
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(reticulate)
library(quanteda)

# Idea for workflow:
## Add the following columns per comment:
## Translate comment into English (llm)
## Turn translation into cleaned up, tokenized comment (quanteda)
## Analyze and summarize into topics (llm)
## TODO: sentiment & tone, emotion & volition
## Output is a list with original comment, translation, topics + relevant tokens ...

# llama3 function
llama3 <- function(prompt) {
  # API endpoint URL (replace with the actual endpoint if different)
  url <- "http://localhost:11434/api/chat"
  
  # Data to be sent in the POST request
  data <- list(
    model = "llama3",
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
# phi3 function
phi3 <- function(prompt) {
  # API endpoint URL (replace with the actual endpoint if different)
  url <- "http://localhost:11434/api/chat"
  
  # Data to be sent in the POST request
  data <- list(
    model = "phi3.5",
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
# thinkverse/towerinstruct:latest function
thinkversetowerinstruct <- function(prompt) {
  # API endpoint URL (replace with the actual endpoint if different)
  url <- "http://localhost:11434/api/chat"
  
  # Data to be sent in the POST request
  data <- list(
    model = "thinkverse/towerinstruct:latest",
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
thinkversetowerinstruct("sup?")

# Translation function
Function_Translate_To_English <- function(Insert_Comment){
Prompt_Restart <- "Begin anew |"
Prompt_translation_1 <- "Translate the football website comment below into English |"
Prompt_translation_2 <- "IMPORTANT: your response cannot contain an intro or outro. Provide the translation only |"
Translation_Prompt <- paste(Prompt_Restart, Prompt_translation_1, Prompt_translation_2, "comment -> ", Insert_Comment)
Translation <- phi3(Translation_Prompt)
Translation
}

# Tokenizer function
Function_Tokenize_Quanteda <- function(Insert_Translation){
# Convert the comment to a corpus object
corpus_obj <- corpus(Insert_Translation)

# Create a tokenized object and apply text preprocessing
tokens_clean <- 
  tokens(corpus_obj,
         what = "word",               # Tokenize at the word level
         remove_punct = TRUE,         # Remove punctuation
         remove_numbers = TRUE) |>    # Remove numbers
  tokens_tolower() |>                 # Convert to lowercase
  tokens_remove(stopwords("en")) |>   # Remove English stopwords
  tokens_remove("<") |>
  tokens_remove(">")
  
# View the cleaned tokens
tokens_clean
}
# Topic extractor function
Function_Topic_Extraction <- function(Insert_Tokenized_List){
Prompt_Restart <- "Begin anew |"
Prompt_1 <- "Provide 1 CSV table with 5 columns 'Cluster_Related_tokens','Cluster_topic','Sentiment_between_minus1_plus1','Purpose_1word', 'Tone_1word', 'Style_1word' |"
#Prompt_2 <- "Summarize the football-forum comment into topics you identify in it. Create 1 row per topic. |"
Prompt_2 <- "Analyze the football-forum comment to fill out the columns. Create 1 row per clustered tokens you identify |"
Prompt_3 <- "IMPORTANT: your response cannot contain an intro. Provide in CSV format only. Check if response is in correct column |" 
Comment_as_string <- paste(Insert_Tokenized_List, collapse = " ")

Topics_Prompt <- paste(Prompt_Restart, Prompt_1, Prompt_2, Prompt_3, "comment -> ", Comment_as_string)
Topics_CSV <- llama3(Topics_Prompt)
read_csv(Topics_CSV, col_names = TRUE) |> print()

}
# Function collecting all previous functions with list output
Function_Comment_To_Topics <- function(Comment){
  Translation_Test <- Function_Translate_To_English(Comment)
  Tokenized_Test <- Function_Tokenize_Quanteda(Translation_Test)
  Topics_Extraction <- Function_Topic_Extraction(Tokenized_Test)
  Comment_List <- list(
    Comment_Raw = Comment,
    Translation = Translation_Test,
    Topics = Topics_Extraction
    )
}

# Let's go
# Initialize an empty list to store the results
all_results <- list()
# Iterate through the first 10 elements and append the results
for (i in 1:10) {
  result_iterated <- Function_Comment_To_Topics(result$raw_message[i])
  all_results <- append(all_results, list(result_iterated))
}
all_results
# Initialize an empty data frame
topics_df <- data.frame()
# Loop through all_results and extract the "Topics", appending to the data frame
for (i in 1:length(all_results)) {
  topics_df <- rbind(topics_df, as.data.frame(all_results[[i]][["Topics"]], stringsAsFactors = FALSE))
}
topics_df

C1 <- Function_Comment_To_Topics(result$raw_message[1])
C2 <- Function_Comment_To_Topics(result$raw_message[2])
C3 <- Function_Comment_To_Topics(result$raw_message[3])
C4 <- Function_Comment_To_Topics(result$raw_message[4])
C5 <- Function_Comment_To_Topics(result$raw_message[5])
C6 <- Function_Comment_To_Topics(result$raw_message[6])
C7 <- Function_Comment_To_Topics(result$raw_message[7]) # Contains only 1 word -> to do: filter out
C8 <- Function_Comment_To_Topics(result$raw_message[8])


# Sample comments
Comment_1 <- "<<1e helft redelijk tot goed. 2e helft matig. Maar wat me zorgen baart: Dit AZ heeft geen scorende middenvelder à la De Wit en dat gaat AZ lelijk opbreken. DE oplossing: Davy Klaassen binnenhalen. Heeft heel veel routine en een neusje voor de goal. Hij wil heel veel geld inleveren en neemt genoegen met een jaarsalaris van 4 1/2 - 5 ton. Iets wat de gemiddelde AZ -speler ook verdient. Kom op Huiberts sla je slag.>>"
Comment_2 <- "<<Voor rust een leuke wedstrijd. Na rust leverde AZ door de wissels teveel kwaliteit in. De Basis staat wel vast en enkele wissels hebben vandaag hun kans gekregen. Maikuma, Goes, Bazoer, Wolfe, Clasie, Belic, Mijnans, Sadiq, Bommel en Parrot zijn de basisspelers. Zoet is veel beter als Verhulst, Koopmeiners zit kort tegen de basis. Poku, Addai, Brederode, Buurmeester en de anderen zullen uit noodzaak af en toe hun minuten maken.>>"
Comment_3 <- "<<Ik ben wel een beetje klaar met Bazoer. Maakt elke wedstrijd wel een (cruciale) fout. De kwaliteiten die hij vroeger had zie ik niet meer terug. Heb daarom veel liever Dekker in de basis.
Daarnaast mag Clasie al in de rust worden vervangen door Koopmeiners. Of nog beter, Clasie na 6o minuten voor Peer erin.>>"
Comment_4 <-"<<De wissels voorin baarden me echt zorgen. Addai en Poku brachten niets. Samen met Zeefuik liepen er op een gegeven moment drie voorin met hun schouders omlaag, ze straalden samen niets uit. Zeefuik compenseerde daar nog voor door een slimme beweging en een goede goal, maar die andere twee lieten niets zien. Van Brederode mocht weer veel te lang blijven staan, terwijl hij net aan goed genoeg is voor het recterrijtje. Waar is Van Bommel? Die leek toch onder Martens de concurrentiestrijd gewonnen te hebben. Ik hoop dat Max wakker is geworden, want de talenten voorin hebben het gewoon niet en het is tijd om daar door te selecteren.
Parrot en Sadiq lijken het wel te hebben, ookal zie je bij beiden dat ze nog niet heel makkelijk aanhaken op een hoger niveau. Hopelijk duurt dat niet al te lang.>>"


list_of_lists <- list(C1, C2, C3, C4, C5, C6, C8)
# Extract and combine "Topics" data frames from each list
combined_topics <- do.call(rbind, lapply(list_of_lists, function(x) x$Topics))
# Print the combined dataframe
print(combined_topics)



llama3("I've got you installed locally and am using R to work with you. Give me instructions on how to train you")
llama3("Let's start over. What do I do or prompt for you to awlays ONLY return a csv table? How do I stop you from adding things like 'Here is the CSV table with the topics found in the comment'")
llama3("Give me feedback on this prompt: Begin anew | Provide 1 CSV table with 2 columns 'Topic' & 'Relevant tokens' | Summarize the football comment attached below. Summarize into topics you identify in it. Create 1 row per topic. | IMPORTANT: your response cannot contain an intro. Provide in CSV format only | comment -> [comment]")

os <- import("os")
os$listdir(".")

# Turn llama3 returns into tables
convert_to_table <- function(input_string) {
  # Split the input string into lines
  lines <- strsplit(input_string, "\n")[[1]]
  
  # Split each line into fields based on tabs
  fields <- lapply(lines, function(line) strsplit(line, "\t")[[1]])
  
  # Convert the list of fields to a data frame
  df <- as.data.frame(do.call(rbind, fields[-1]), stringsAsFactors = FALSE)
  
  # Set the column names
  colnames(df) <- fields[[1]]
  
  return(df)
}

# Topics
{
Cats_Subcats1 <- "Category,Subcategory
Transfer News,Player Transfers
Transfer News,Transfer Rumors
Transfer News,Transfer Fees
Transfer News,Contract Length
Transfer News,Club Reactions
Match Previews,Team Line-ups
Match Previews,Tactical Approaches
Match Previews,Key Players
Match Previews,Historical Context
Match Previews,Expert Opinions
Match Reports,Match Highlights
Match Reports,Player Performances
Match Reports,Referee Decisions
Match Reports,Key Moments
Match Reports,Match Statistics
Injury Updates,Injury Severity
Injury Updates,Player Recovery Times
Injury Updates,Impact on Team
Injury Updates,Medical Opinions
Injury Updates,Player Fitness
Managerial Changes,New Appointments
Managerial Changes,Managerial Style
Managerial Changes,Previous Performance
Managerial Changes,Club Reactions
Managerial Changes,Contract Terms
Player Contracts,Contract Length
Player Contracts,Salary Details
Player Contracts,Release Clauses
Player Contracts,Contract Extensions
Player Contracts,Player Reactions
Tactical Analysis,Formation Changes
Tactical Analysis,Game Strategies
Tactical Analysis,Individual Player Tactics
Tactical Analysis,Team Strengths and Weaknesses
Tactical Analysis,Coaching Decisions
Club Financial News,Revenue Reports
Club Financial News,Debt and Loans
Club Financial News,Player Salaries
Club Financial News,Transfer Budgets
Club Financial News,Sponsorship Deals
International Competitions,Tournament Draws
International Competitions,Team Performances
International Competitions,Match Schedules
International Competitions,Player Standouts
International Competitions,Historical Results
League Standings and Statistics,League Table Positions
League Standings and Statistics,Top Scorers
League Standings and Statistics,Assist Leaders
League Standings and Statistics,Defensive Records
League Standings and Statistics,Recent Form
Youth Academies,Player Development
Youth Academies,Transfer to Senior Team
Youth Academies,Academy Facilities
Youth Academies,Coaching Staff
Youth Academies,Youth Team Performances
Off-field Controversies,Legal Issues
Off-field Controversies,Disciplinary Actions
Off-field Controversies,Player Conduct
Off-field Controversies,Club Scandals
Off-field Controversies,Fan Reactions
"
}
{
Cats_Subcats1_simpler <- "
Player Transfers
Transfer Rumors
Team Line-ups
Tactical Approaches
Key Players
Match Highlights
Player Performances
Referee Decisions
Key Moments
Match Statistics
Injury
Impact on Team
Player Fitness
Managerial Changes
Managerial Style
Previous Performance
Club Reactions
Player Contracts
Salary Details
Player Reactions
Tactical Analysis
Formation Changes
Game Strategies
Team Strengths and Weaknesses
Coaching Decisions
Club Financial News
Player Salaries
Transfer Budgets
Sponsorship Deals
International Competitions
Team Performances
Match Schedules
League Standings and Statistics
League Table Positions
Top Scorers
Assist Leaders
Goalkeeper
Defence
Midfield
Attack
Recent Form
Player Development
Youth Academy
Coaching Staff
Youth Team
Off-field Controversies
Disciplinary Actions
Player Conduct
Club Scandals
Fan Reactions
"
}
{
Cats_Subcats1_even_simpler <- "
Match Analysis
Player Performance
Team Strategy
Transfer News
Referee Decisions
Coaching Critiques
Predictions
League Standings
Club Management
Media Coverage
Matchday Experience
Youth Academy
Training
Merchandise
Match Highlights
Pre-match
Post-match
Player Interviews
Club Finances
League Regulations
International Competitions
Fan Theories
Player Transfers
Loan Deals
Fixture Scheduling
VAR Decisions
Player Development
Rumors
Contracts
Goalkeeper
Defence
Midfield
Attack
Tactics
Strategy
"
}