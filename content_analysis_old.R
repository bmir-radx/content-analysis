library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(ggplot2)
library(treemapify)
library(RColorBrewer)
library(purrr)  # Add purrr for pmap_chr function
library(viridis)

base_path <- "/Users/marcosromero/Projects/2024-10-RADxDataHub-ContentReport/"

publications_file_path <- paste0(base_path, "files/RADx_Data_Hub-List_of_Publications.xlsx")
study_metadata_file_path <- paste0(base_path, "files/RADx_Study_Metadata_10232024xlsx.xlsx")
study_domainpopfocus_file_path <- paste0(base_path, "files/Study Domain and Population Focus Consolitation.xlsx")

palette1 <- c(
  "#FF5733", "#1E88E5", "#689F38", "#D81B60", "#3949AB", "#6A1B9A", "#00897B", "#546E7A", 
  "#D98C00", "#8E24AA", "#039BE5", "#00ACC1", "#7CB342", "#FB8C00", "#C62828", "#5E35B1", 
  "#AD1457", "#00838F", "#303F9F", "#7B1FA2", "#0097A7", "#C2185B", "#1976D2", "#388E3C", 
  "#F57C00", "#512DA8", "#00796B", "#9C27B0", "#0288D1", "#455A64", "#8BC34A", "#F06292", 
  "#673AB7", "#689F38", "#FDD835", "#FF7043", "#3949AB", "#00838F", "#00796B", "#C2185B", 
  "#F57C00", "#D32F2F", "#5D4037", "#9E9D24", "#616161", "#1E88E5", "#43A047", "#D81B60", 
  "#FFB300", "#6A1B9A", "#00897B", "#F4511E", "#3949AB", "#8E24AA", "#039BE5", "#00ACC1", 
  "#7CB342", "#FB8C00", "#C62828", "#5E35B1", "#AD1457", "#00838F", "#303F9F", "#7B1FA2", 
  "#0097A7", "#C2185B", "#1976D2", "#388E3C", "#F57C00", "#512DA8", "#00796B", "#9C27B0", 
  "#0288D1", "#455A64", "#8BC34A", "#F06292", "#673AB7", "#43A047", "#FDD835", "#FF7043", 
  "#3949AB", "#00838F", "#00796B", "#C2185B", "#F57C00", "#D32F2F", "#5D4037", "#9E9D24", 
  "#3949AB", "#4CAF50", "#E53935", "#9E9E24", "#CDDC39", "#8D6E63", "#E91E63", "#00BCD4", 
  "#F4511E", "#FFEB3B", "#FF5722", "#009688", "#FFC107", "#3F51B5", "#9C27B0", "#673AB7"
)

palette2 <- c(
  "#303F9F", "#FF5733", "#00ACC1", "#F57C00", "#039BE5", "#C2185B", "#FFB300", "#512DA8",
  "#8E24AA", "#43A047", "#D32F2F", "#1E88E5", "#FF7043", "#0097A7", "#AD1457", "#3949AB",
  "#8BC34A", "#D81B60", "#00BCD4", "#689F38", "#FF5722", "#6A1B9A", "#F4511E", "#0288D1",
  "#673AB7", "#00838F", "#C62828", "#388E3C", "#D98C00", "#E53935", "#7B1FA2", "#00ACC1",
  "#AD1457", "#5D4037", "#AD1457", "#F06292", "#9C27B0", "#00796B", "#455A64", "#4CAF50",
  "#3949AB", "#E91E63", "#00897B", "#9E9D24", "#616161", "#9C27B0", "#CDDC39", "#FB8C00",
  "#00BCD4", "#8BC34A", "#512DA8", "#D81B60", "#FF5722", "#F57C00", "#689F38", "#8D6E63",
  "#9E9D24", "#F06292", "#673AB7", "#C2185B", "#8BC34A", "#00ACC1", "#E53935", "#AD1457",
  "#1E88E5", "#009688", "#F4511E", "#0097A7", "#C62828", "#43A047", "#3949AB", "#00BCD4",
  "#6A1B9A", "#F57C00", "#00BCD4", "#9E9E24", "#D32F2F", "#F06292", "#1976D2", "#D98C00",
  "#8E24AA", "#303F9F", "#FFB300", "#FF7043", "#388E3C", "#5E35B1", "#0288D1", "#00796B",
  "#455A64", "#E53935", "#4CAF50", "#FB8C00", "#FF5733", "#E91E63", "#616161", "#9C27B0"
)

palettes <- list(palette1, palette2)

### FUNCTIONS

# Function to extract the year from the publication date
extract_year <- function(pub_date) {
  year <- as.numeric(strsplit(pub_date, " ")[[1]][1])
  return(year)
}

# Function to standardize a column
standardize_column <- function(df, column_name) {
  df %>%
    mutate(!!sym(column_name) := str_trim(!!sym(column_name))) %>%    
    mutate(!!sym(column_name) := str_to_lower(!!sym(column_name))) %>% 
    mutate(!!sym(column_name) := str_replace_all(!!sym(column_name), "[^a-z0-9]", ""))
}

# Function to generate all chart types
generate_all_charts <- function(data, columns, id_column = "PMID", title = "Charts of Publications", min_count = 1, custom_palettes = NULL) {
  
  # Combine columns if multiple are provided
  if (length(columns) > 1) {
    data <- data %>%
      mutate(combined_column = pmap_chr(across(all_of(columns)), ~ paste(na.omit(c(...)), collapse = "; ")))
  } else {
    data <- data %>%
      mutate(combined_column = !!sym(columns[[1]])) 
  }
  
  # Clean and split the combined column
  data_cleaned <- data %>%
    filter(!is.na(combined_column), combined_column != "", str_trim(combined_column) != "") %>%
    separate_rows(combined_column, sep = "; ") %>%
    mutate(combined_column = str_replace_all(combined_column, "[\n\r]", " "),
           combined_column = str_trim(combined_column),
           combined_column = str_remove(combined_column, ";\\s*$")) %>%
    filter(combined_column != "N/A", combined_column != "")
  
  # Group by the combined column and count distinct IDs
  chart_data <- data_cleaned %>%
    group_by(combined_column) %>%
    summarize(publication_count = n_distinct(!!sym(id_column))) %>%
    filter(publication_count >= min_count)
  
  # List of chart types to generate
  chart_types <- c("treemap", "bar", "bubble")
  
  # Helper function to generate and print charts
  generate_plot <- function(custom_palette, title_suffix, chart_type) {
    plot <- NULL
    
    if (chart_type == "treemap") {
      plot <- ggplot(chart_data, aes(area = publication_count, fill = combined_column, label = combined_column)) +
        geom_treemap() +
        geom_treemap_text(colour = "white", place = "centre", grow = TRUE, reflow = TRUE)
      
    } else if (chart_type == "bar") {
      plot <- ggplot(chart_data, aes(x = reorder(combined_column, -publication_count), y = publication_count, fill = combined_column)) +
        geom_bar(stat = "identity") +
        coord_flip()
      
    } else if (chart_type == "bubble") {
      plot <- ggplot(chart_data, aes(x = combined_column, y = publication_count, size = publication_count, fill = combined_column)) +
        geom_point(alpha = 0.7, shape = 21, colour = "black") +
        theme_minimal()
      
    }
    
    if (!is.null(custom_palette)) {
      plot <- plot + scale_fill_manual(values = custom_palette)
    }
    
    plot <- plot + labs(title = paste(title, title_suffix)) +
      theme(legend.position = "none", plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    print(plot)
  }
  
  # Generate all chart types (no custom palette)
  for (chart_type in chart_types) {
    generate_plot(custom_palette = NULL, title_suffix = paste("- Default Colors -", chart_type), chart_type = chart_type)
  }
  
  # Generate charts for each custom palette and each chart type
  if (!is.null(custom_palettes)) {
    for (i in seq_along(custom_palettes)) {
      for (chart_type in chart_types) {
        generate_plot(custom_palette = custom_palettes[[i]], title_suffix = paste("- Custom Palette", i, "-", chart_type), chart_type = chart_type)
      }
    }
  }
  
  return(data_cleaned)
}

#### PREPROCESSING ###

# Load the data 
publications_df <- read_excel(publications_file_path)
study_df <- read_excel(study_metadata_file_path, sheet = "Cleaned Metadata")
study_dpf_df <- read_excel(study_domainpopfocus_file_path)

# Rename the columns so that they have the same name for merging
publications_df <- publications_df %>% rename(dbgap_accession = "dbGaP Accession")
study_df <- study_df %>% rename(dbgap_accession = PHS)
study_dpf_df <- study_dpf_df %>% rename(dbgap_accession = PHS)

# Remove rows where dbgap_accession is NA
publications_df <- publications_df %>% filter(!is.na(dbgap_accession))
study_df <- study_df %>% filter(!is.na(dbgap_accession))
study_dpf_df <- study_dpf_df %>% filter(!is.na(dbgap_accession))

# Apply normalization to both dataframes
publications_df <- standardize_column(publications_df, "dbgap_accession")
study_df <- standardize_column(study_df, "dbgap_accession")
study_dpf_df <- standardize_column(study_dpf_df, "dbgap_accession")

# Perform the merge
merged_df <- publications_df %>%
  left_join(study_df, by = "dbgap_accession")

# Apply the function to add the 'Publication Year' column
merged_df <- merged_df %>%
  mutate(Publication_Year = sapply(`Publication Date`, extract_year))

# Export merged_df to a CSV file
output_csv_file <- paste0(base_path, "files/merged_df.csv")
write.csv(merged_df, file = output_csv_file, row.names = FALSE)

### STUDY CHARTS

# Filtered data frames by specific DCC values
radx_up_df <- study_dpf_df %>% filter(DCC == "RADx-UP")
radx_rad_df <- study_dpf_df %>% filter(DCC == "RADx-rad")
radx_tech_df <- study_dpf_df %>% filter(DCC == "RADx-Tech")

# Generate charts
generate_all_charts(study_dpf_df, c("refined topic"), id_column = "dbgap_accession", title = "Studies by Topic", min_count = 2, custom_palettes = palettes)
generate_all_charts(radx_up_df, c("refined topic"), id_column = "dbgap_accession", title = "RADx-UP Studies by Topic", min_count = 3, custom_palettes = palettes)
generate_all_charts(radx_up_df, c("Refined population"), id_column = "dbgap_accession", title = "RADx-UP Studies by Population Focus", min_count = 2, custom_palettes = palettes)
generate_all_charts(radx_rad_df, c("refined topic"), id_column = "dbgap_accession", title = "RADx-rad Studies by Topic", min_count = 3, custom_palettes = palettes)
generate_all_charts(radx_tech_df, c("refined topic"), id_column = "dbgap_accession", title = "RADx-Tech Studies by Topic", min_count = 1, custom_palettes = palettes)



# # TreeMaps
# generate_all_charts(merged_df, c("source", "source_other_specify"), title = "Treemap of Publications by Study Source", min_count = 3, custom_palettes=palettes)
# generate_all_charts(merged_df, c("Study_Population_Focus"), title = "Treemap of Publications by Study Population Focus")
# generate_all_charts(merged_df, c("data_general_types", "data_general_types_other_specify"), title = "Treemap of Publications by Data Type", min_count = 5)
# generate_all_charts(merged_df, c("topics"), title = "Treemap of Publications by Study Topics")
# generate_all_charts(merged_df, c("types"), title = "Treemap of Publications by Study Type")
# generate_all_charts(merged_df, c("institutes_supporting_study"), title = "Treemap of Publications by Institute")
# generate_all_charts(merged_df, c("subject"), title = "Treemap of Publications by Subject", min_count = 7)
# generate_all_charts(merged_df, c("Journal"), title = "Treemap of Publications by Journal", min_count = 2)
# 
# # Bar Chart: Number of Publications per year
# ggplot(merged_df, aes(x = as.factor(Publication_Year))) +
#   geom_bar(fill = "lightblue") +
#   scale_x_discrete(limits = as.character(2020:format(Sys.Date(), "%Y"))) +  # Set x-axis to start from 2020 onwards
#   labs(title = "Publications Per Year", x = "Year", y = "Number of Publications") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed
# 
# # Number of publications per dataset
# # Group by 'dbgap_accession' (dataset) and count distinct publications (PMID)
# dataset_publications <- merged_df %>%
#   filter(!is.na(dbgap_accession), !is.na(PMID)) %>%  # Ensure no missing values
#   group_by(dbgap_accession) %>%
#   summarize(publication_count = n_distinct(PMID)) %>%
#   arrange(desc(publication_count))  # Sort by publication count in descending order
# 
# # Select the top 20 datasets based on publication count
# top_datasets <- dataset_publications %>%
#   top_n(20, wt = publication_count)
# 
# # Create a bar chart showing the top datasets
# ggplot(top_datasets, aes(x = reorder(dbgap_accession, -publication_count), y = publication_count)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   labs(title = "Top 10 Datasets by Number of Publications", x = "Dataset (dbGaP Accession)", y = "Number of Publications") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
# 
# 
# 
# ### Heatmap: number of publications by topic by year
# heatmap_data <- treemap_topics_data %>%
#   count(combined_column, Publication_Year) %>%
#   spread(Publication_Year, n, fill = 0)
# 
# # Pivot longer to get back the 'Publication_Year' column for heatmap plotting
# heatmap_data_long <- heatmap_data %>%
#   pivot_longer(cols = starts_with("20"),  # Assumes years are 20XX, adjust if needed
#                names_to = "Publication_Year",
#                values_to = "n")
# 
# # Limit to the top 10 topics by publication count and recent years (e.g., 2020 onwards)
# top_topics <- treemap_topics_data %>%
#   count(combined_column, sort = TRUE) %>%
#   top_n(10, wt = n)  # Top 10 topics by publication count
# 
# heatmap_data_filtered <- heatmap_data_long %>%
#   filter(combined_column %in% top_topics$combined_column, Publication_Year >= 2020)
# 
# # Now plot the heatmap with filtered data
# ggplot(heatmap_data_filtered, aes(x = Publication_Year, y = combined_column, fill = n)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient(low = "lightblue", high = "darkblue") +
#   labs(title = "Heatmap of Publications by Topic and Year", x = "Publication Year", y = "Topics") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
# 
# 



### TRASH

# # Step 1: Split the 'subject' column by semicolon into multiple rows
# merged_df_split <- merged_df %>%
#   filter(!is.na(subject)) %>%  # Remove rows with missing subjects
#   separate_rows(subject, sep = ";") %>%  # Split the 'subject' column by semicolon
#   mutate(subject = str_trim(subject))  # Trim any leading/trailing spaces
# 
# # Step 2: Group data by 'subject' and count the number of distinct publications (PMID) and datasets (dbgap_accession)
# subject_data <- merged_df_split %>%
#   filter(!is.na(PMID), !is.na(dbgap_accession)) %>%  # Filter out missing values
#   group_by(subject) %>%
#   summarize(publication_count = n_distinct(PMID),
#             dataset_count = n_distinct(dbgap_accession)) %>%
#   filter(dataset_count > 0)  # Remove any subjects without datasets
# 
# # Step 3: Calculate the ratio of publications to datasets
# subject_data <- subject_data %>%
#   mutate(publication_to_dataset_ratio = publication_count / dataset_count)
# 
# # Step 4: Identify subjects with a high ratio of publications to datasets
# # (This highlights subjects with many publications but few datasets)
# subject_data_high_ratio <- subject_data %>%
#   filter(publication_to_dataset_ratio > 1)  # Adjust the threshold if needed
# 
# # Step 5: Limit to the top 50 subjects based on publication-to-dataset ratio
# subject_data_top_50 <- subject_data_high_ratio %>%
#   top_n(50, wt = publication_to_dataset_ratio)
# 
# # Step 6: Generate a bar chart showing the ratio of publications to datasets for the top 50 subjects
# ggplot(subject_data_top_50, aes(x = reorder(subject, -publication_to_dataset_ratio), y = publication_to_dataset_ratio)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   labs(title = "Top 50 Subjects by Ratio of Publications to Datasets",
#        x = "Subject",
#        y = "Publications to Datasets Ratio") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 80, hjust = 1))  # Rotate x-axis labels for better readability
