# Clean up files and send to duckdb

# packages
{
	library(dplyr)
	library(stringr)
	library(tidyr)
}

# load data
{
	emotions <- readRDS("~/Documents/GitHub/AZFanpage_Comments/emotions_2020.rds")
}

# emotions cleanup
{
	emotions_db <- emotions |> 
	mutate(
    emotion_intensity = str_remove_all(emotion_intensity, "[\\[\\]]"), # Remove square brackets
    emotion_intensity = str_split(emotion_intensity, "; ") # Split into list of strings
  ) |> 
  unnest(emotion_intensity) |> # Expand the list into rows
  separate(emotion_intensity, into = c("emotion", "emotion_intensity"), sep = ", ", convert = TRUE) |> 
  mutate(
    emotion = str_to_lower(emotion), # Convert to lowercase
    emotion_intensity = as.numeric(emotion_intensity) # Ensure numeric values
  )

}

# Emotions visuals
{
# Frequency count of unique values
emotion_counts <- as.data.frame(table(emotions_db$emotion))
colnames(emotion_counts) <- c("Emotion", "Count")

# Arrange and filter the top 20 most frequent emotions
top_n <- 50
top_emotions <- emotion_counts %>%
  arrange(desc(Count)) %>%
  mutate(Emotion = as.character(Emotion)) %>%
  slice(1:top_n)

# Combine all others into "Other" category
top_emotions <- rbind(
  top_emotions,
  data.frame(Emotion = "Other", Count = sum(emotion_counts$Count) - sum(top_emotions$Count))
)

# Create the bar plot
ggplot(top_emotions, aes(x = reorder(Emotion, -Count), y = Count, fill = Emotion)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = paste("Top", top_n, "Emotions with Frequencies"), x = "Emotion", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")  # Remove legend for simplicity

# Filter for the top 10 emotions by frequency
top_n <- 10
top_emotions <- emotions_db %>%
  count(emotion) %>%
  arrange(desc(n)) %>%
  slice(1:top_n) %>%
  pull(emotion)

# Filter the dataset for these emotions
filtered_data <- emotions_db %>%
  filter(emotion %in% top_emotions)

# Create the box plot
ggplot(filtered_data, aes(x = emotion, y = emotion_intensity, fill = emotion)) +
  geom_boxplot(outlier.shape = NA) + # Exclude outliers for clarity
  theme_minimal() +
  labs(
    title = "Distribution of Emotion Intensity for Top Emotions",
    x = "Emotion",
    y = "Emotion Intensity"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")  # Remove legend for simplicity
}
