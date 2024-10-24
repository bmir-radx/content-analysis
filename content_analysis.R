library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(ggplot2)
library(treemapify)
library(RColorBrewer)
library(purrr)  # Add purrr for pmap_chr function

base_path <- "path/to/your/projectfolder/"

publications_file_path <- paste0(base_path, "files/RADx_Data_Hub-List_of_Publications.xlsx")
study_metadata_file_path <- paste0(base_path, "files/RADx_Study_Metadata_10232024xlsx.xlsx")

### FUNCTIONS

# Function to extract the year from the publication date
extract_year <- function(pub_date) {
  # Try to extract the first part of the string (the year)
  year <- as.numeric(strsplit(pub_date, " ")[[1]][1])
  return(year)
}

# Function to standardize a column
standardize_column <- function(df, column_name) {
  df %>%
    mutate(!!sym(column_name) := str_trim(!!sym(column_name))) %>%    # Remove leading/trailing spaces
    mutate(!!sym(column_name) := str_to_lower(!!sym(column_name))) %>% # Convert to lowercase
    mutate(!!sym(column_name) := str_replace_all(!!sym(column_name), "[^a-z0-9]", ""))  # Remove special characters (optional)
}

# Function to generate treemaps
generate_treemap <- function(data, columns, title = "Treemap of Publications", palette = NULL, min_count = 1) {
  
  # Step 1: Combine the columns if multiple are provided
  if (length(columns) > 1) {
    data <- data %>%
      mutate(combined_column = pmap_chr(across(all_of(columns)), ~ paste(na.omit(c(...)), collapse = "; ")))
  } else {
    # If only one column, just use that one
    data <- data %>%
      mutate(combined_column = !!sym(columns))
  }
  
  # Step 2: Clean and split the combined column
  data_cleaned <- data %>%
    filter(!is.na(combined_column), combined_column != "", str_trim(combined_column) != "") %>%  # Filter NA and empty strings
    separate_rows(combined_column, sep = "; ") %>%
    mutate(combined_column = str_replace_all(combined_column, "[\n\r]", " "),  # Replace newlines with space
           combined_column = str_trim(combined_column),  # Trim leading/trailing spaces
           combined_column = str_remove(combined_column, ";\\s*$")) %>%  # Remove trailing semicolons
    filter(combined_column != "N/A", combined_column != "")  # Filter explicit "N/A" and empty values
  
  # Step 3: Group by the combined column and count publications
  treemap_data <- data_cleaned %>%
    group_by(combined_column) %>%
    summarize(publication_count = n_distinct(PMID),
              total_participants = sum(estimated_participants, na.rm = TRUE)) %>%  
    filter(publication_count >= min_count)  # Filter rows with publication_count below the minimum threshold
  
  # Step 4: Generate the treemap
  treemap_plot <- ggplot(treemap_data, aes(area = publication_count, fill = combined_column, 
                           label = paste0(combined_column, "\n(", publication_count, ")"))) +
    geom_treemap() +
    geom_treemap_text(colour = "white", place = "centre", grow = TRUE, reflow = TRUE) +
    labs(title = title) +
    theme(legend.position = "none", plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    (if (!is.null(palette)) scale_fill_manual(values = palette) else NULL)  # Optional palette support
  
  print(treemap_plot)  # Ensure the treemap is plotted before returning
  return(data_cleaned)
}

#### PREPROCESSING ###

# Load the data 
publications_df <- read_excel(publications_file_path)
study_df <- read_excel(study_metadata_file_path, sheet = "Cleaned Metadata")

# Rename the columns so that they have the same name for merging
publications_df <- publications_df %>% rename(dbgap_accession = "dbGaP Accession")
study_df <- study_df %>% rename(dbgap_accession = PHS)

# Remove rows where dbgap_accession is NA
publications_df <- publications_df %>% filter(!is.na(dbgap_accession))
study_df <- study_df %>% filter(!is.na(dbgap_accession))

# Apply normalization to both dataframes
publications_df <- standardize_column(publications_df, "dbgap_accession")
study_df <- standardize_column(study_df, "dbgap_accession")

# Find records in publications_df that don't have a match in study_df
publications_without_match_df <- publications_df %>%
  anti_join(study_df, by = "dbgap_accession") %>% distinct(dbgap_accession)

# Find records in study_df that don't have a match in publications_df
studies_without_match_df <- study_df %>%
  anti_join(publications_df, by = "dbgap_accession") %>% distinct(dbgap_accession)

# Select only the relevant columns from study_df (dbgap_accession it's used for joining)
study_df_selected <- study_df %>%
  select(dbgap_accession, source, source_other_specify, estimated_participants, 
         Study_Population_Focus, data_general_types, data_general_types_other_specify, topics, institutes_supporting_study, subject, types)

# Perform the merge (keeping only relevant columns after the merge)
merged_df <- publications_df %>%
  left_join(study_df, by = "dbgap_accession") 

# Apply the function to add the 'Publication Year' column
merged_df <- merged_df %>%
  mutate(Publication_Year = sapply(`Publication Date`, extract_year))

# Define the file path where you want to save the CSV file
output_csv_file <- paste0(base_path, "files/merged_df.csv")

# Export merged_df to a CSV file
write.csv(merged_df, file = output_csv_file, row.names = FALSE)

### VISUALIZATION

# TreeMaps
generate_treemap(merged_df, c("source", "source_other_specify"), title = "Treemap of Publications by Study Source", min_count = 3)
generate_treemap(merged_df, c("Study_Population_Focus"), title = "Treemap of Publications by Study Population Focus")
generate_treemap(merged_df, c("data_general_types", "data_general_types_other_specify"), title = "Treemap of Publications by Data Type", min_count = 5)
treemap_topics_data = generate_treemap(merged_df, c("topics"), title = "Treemap of Publications by Study Topics")
generate_treemap(merged_df, c("types"), title = "Treemap of Publications by Study Type")
generate_treemap(merged_df, c("institutes_supporting_study"), title = "Treemap of Publications by Institute")
generate_treemap(merged_df, c("subject"), title = "Treemap of Publications by Subject", min_count = 7)
generate_treemap(merged_df, c("Journal"), title = "Treemap of Publications by Journal", min_count = 2)

# Bar Chart: Number of Publications per year
ggplot(merged_df, aes(x = as.factor(Publication_Year))) +
  geom_bar(fill = "lightblue") +
  scale_x_discrete(limits = as.character(2020:format(Sys.Date(), "%Y"))) +  # Set x-axis to start from 2020 onwards
  labs(title = "Publications Per Year", x = "Year", y = "Number of Publications") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

# Number of publications per dataset
# Group by 'dbgap_accession' (dataset) and count distinct publications (PMID)
dataset_publications <- merged_df %>%
  filter(!is.na(dbgap_accession), !is.na(PMID)) %>%  # Ensure no missing values
  group_by(dbgap_accession) %>%
  summarize(publication_count = n_distinct(PMID)) %>%
  arrange(desc(publication_count))  # Sort by publication count in descending order

# Select the top 20 datasets based on publication count
top_datasets <- dataset_publications %>%
  top_n(20, wt = publication_count)

# Create a bar chart showing the top datasets
ggplot(top_datasets, aes(x = reorder(dbgap_accession, -publication_count), y = publication_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Datasets by Number of Publications", x = "Dataset (dbGaP Accession)", y = "Number of Publications") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

### Heatmap: topics by participant size by year

treemap_topics_data <- treemap_topics_data %>%
  filter(!is.na(estimated_participants), !is.na(Publication_Year), Publication_Year >= 2020)

# Group by 'topics' and 'Publication_Year' and calculate the average participant size for each topic per year
heatmap_data <- treemap_topics_data %>%
  group_by(combined_column, Publication_Year) %>%
  summarize(avg_participant_size = mean(estimated_participants, na.rm = TRUE)) %>%
  arrange(desc(avg_participant_size))

# Create the heatmap with both topics and years
ggplot(heatmap_data, aes(x = Publication_Year, y = reorder(combined_column, avg_participant_size), fill = avg_participant_size)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Topics vs. Avg. Participant Size by Year", x = "Publication Year", y = "Topics", fill = "Avg Participant Size") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),  # Adjust text size for readability
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


### Heatmap: number of publications by topic by year
heatmap_data <- treemap_topics_data %>%
  count(combined_column, Publication_Year) %>%
  spread(Publication_Year, n, fill = 0)

# Pivot longer to get back the 'Publication_Year' column for heatmap plotting
heatmap_data_long <- heatmap_data %>%
  pivot_longer(cols = starts_with("20"),  # Assumes years are 20XX, adjust if needed
               names_to = "Publication_Year",
               values_to = "n")

# Limit to the top 10 topics by publication count and recent years (e.g., 2020 onwards)
top_topics <- treemap_topics_data %>%
  count(combined_column, sort = TRUE) %>%
  top_n(10, wt = n)  # Top 10 topics by publication count

heatmap_data_filtered <- heatmap_data_long %>%
  filter(combined_column %in% top_topics$combined_column, Publication_Year >= 2020)

# Now plot the heatmap with filtered data
ggplot(heatmap_data_filtered, aes(x = Publication_Year, y = combined_column, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Publications by Topic and Year", x = "Publication Year", y = "Topics") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


