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

study_metadata_file_path <- paste0(base_path, "files/RADx_Study_Metadata_10232024xlsx.xlsx")
study_domainpopfocus_file_path <- paste0(base_path, "files/Study Domain and Population Focus Consolitation.xlsx")

output_dir <- paste0(base_path, "plots/") 

# Ensure the output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

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

palette3 <- c(
  "#A58C6F",  # Light Brown
  "#7A6174",  # Muted Purple
  "#8A6F56",  # Brown
  "#6B737B",  # Muted Blue Gray
  "#AFB68F",  # Muted Green
  "#8D6B94",  # Muted Purple
  "#A49379",  # Light Beige
  "#7D8E7A",  # Muted Forest Green
  "#799F99",  # Muted Green Blue
  "#AC8471",  # Muted Reddish Brown
  "#918471",  # Grayish Brown
  "#998C7B",  # Taupe Brown
  "#9D9381",  # Earthy Beige
  "#8F8D79",  # Olive Gray
  "#A66E7A",  # Muted Reddish Purple
  "#647A68",  # Muted Greenish Gray
  "#75617E",  # Muted Purple
  "#6A5D57",  # Earthy Gray Brown
  "#9B786F",  # Earthy Pinkish Brown
  "#6C7074",  # Muted Blue-Gray
  
  # Additional colors that complement the earthy palette
  "#6B4226", "#5D4037", "#7F462C", "#845422", "#8E460E", "#986D4F", "#A08058", "#9C8466",
  "#B07D62", "#7C614A", "#7E481B", "#6D3D1B", "#A98464", "#BC8F8F", "#CD853F", "#8B4513",
  "#8FBC8F", "#556B2F", "#6B8E23", "#808000", "#2F4F4F", "#008080", "#2E8B57", "#3CB371",
  "#5F6A6A", "#4D5656", "#6E7F80", "#4E4E4E", "#515A5A", "#424949", "#1C2833", "#1E4D2B",
  "#324D4F", "#4D644F", "#6B4226", "#7F7F7F", "#9C9C9C", "#7C756A", "#7B7D7D", "#AD743C",
  "#9F8C63", "#9B5513", "#B87333", "#C9AE82", "#846544", "#897665", "#8B7E6B", "#A52A2A",
  "#BA4A00", "#873600", "#DAA520", "#B8860B", "#D2691E", "#DEB887", "#D2B48C", "#8B0000",
  "#B22222", "#800000", "#CD5C5C", "#FF4500", "#D35400", "#E59866", "#D98880", "#FF6347",
  "#FF8C00", "#FF7F50", "#FFD700", "#F39C12", "#E67E22", "#F4A460", "#FA8072", "#E9967A",
  "#F08080", "#B03A2E", "#C0392B", "#922B21", "#7B241C", "#943126", "#78281F", "#641E16")


palettes <- list(palette1, palette2, palette3)

### FUNCTIONS

# Function to standardize a column
standardize_column <- function(df, column_name) {
  df %>%
    mutate(!!sym(column_name) := str_trim(!!sym(column_name))) %>%    
    mutate(!!sym(column_name) := str_to_lower(!!sym(column_name))) %>% 
    mutate(!!sym(column_name) := str_replace_all(!!sym(column_name), "[^a-z0-9]", ""))
}

# Function to generate all chart types
generate_all_charts <- function(data, columns, id_column = "PMID", title = "Charts", min_count = 1, custom_palettes = NULL) {
  
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
    summarize(record_count = n_distinct(!!sym(id_column))) %>%
    filter(record_count >= min_count)
  
  # List of chart types to generate
  chart_types <- c("treemap", "bar")
  
  # Helper function to generate and print charts
  generate_plot <- function(custom_palette, title_suffix, chart_type) {
    plot <- NULL
    plot_title <- paste(title, title_suffix)
    
    # Generate different types of plots based on chart_type
    if (chart_type == "treemap") {
      plot <- ggplot(chart_data, aes(area = record_count, fill = combined_column, label = combined_column)) +
        geom_treemap() +
        geom_treemap_text(colour = "white", place = "centre", grow = TRUE, reflow = TRUE)
      
    } else if (chart_type == "bar") {
      plot <- ggplot(chart_data %>% 
                       arrange(desc(record_count)) %>% 
                       slice_head(n = 20), 
                     aes(x = reorder(combined_column, record_count), y = record_count, fill = combined_column)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(y = "Study Count", x = "")
      
    } else if (chart_type == "bubble") {
      plot <- ggplot(chart_data, aes(x = combined_column, y = record_count, size = record_count, fill = combined_column)) +
        geom_point(alpha = 0.7, shape = 21, colour = "black") +
        theme_minimal()
    }
    
    # Apply custom palette if provided
    if (!is.null(custom_palette)) {
      plot <- plot + scale_fill_manual(values = custom_palette)
    }
    
    # Add title and theme settings
    plot <- plot + labs(title = plot_title) +
      theme(legend.position = "none", plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    # Display the plot
    print(plot)
    
    # Define a file name based on the plot type, title, and palette
    file_name <- paste0(output_dir, gsub(" ", "_", plot_title), "_", chart_type, "10x6.png")
    # Save the plot to a file
    ggsave(file_name, plot = plot, width = 10, height = 6, dpi = 300)
    
    # Define a file name based on the plot type, title, and palette
    file_name <- paste0(output_dir, gsub(" ", "_", plot_title), "_", chart_type, "12x10.png")
    # Save the plot to a file
    ggsave(file_name, plot = plot, width = 12, height = 10, dpi = 300)
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
study_df <- read_excel(study_metadata_file_path, sheet = "Cleaned Metadata")
study_dpf_df <- read_excel(study_domainpopfocus_file_path)

# Rename the columns so that they have the same name for merging
study_df <- study_df %>% rename(dbgap_accession = PHS)
study_dpf_df <- study_dpf_df %>% rename(dbgap_accession = PHS)

# Remove rows where dbgap_accession is NA
study_df <- study_df %>% filter(!is.na(dbgap_accession))
study_dpf_df <- study_dpf_df %>% filter(!is.na(dbgap_accession))

# Apply normalization to both dataframes
study_df <- standardize_column(study_df, "dbgap_accession")
study_dpf_df <- standardize_column(study_dpf_df, "dbgap_accession")

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

