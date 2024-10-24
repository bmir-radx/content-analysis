# RADx Data Hub Content Analysis

This repository contains R scripts and tools to generate visualizations and conduct analysis on the content from the **RADx Data Hub** repository.

## Features
- Generate visualizations such as treemaps, heatmaps, and bar charts for various aspects of the RADx Data Hub.
- Analyze study participant sizes, publication counts, and study topics.

## Repository Structure
- `content_analysis.R`: Main R script containing the analysis and visualization functions.
- `files/`: Directory containing any data files used in the analysis.

## Getting Started
1. Clone the repository:
   ```bash
   git clone https://github.com/bmir-radx/content-analysis.git
   cd content-analysis
   ```

2. Install the necessary R packages:
   - `dplyr`
   - `ggplot2`
   - `treemapify`
   - `tidyr`
   - `purrr`

   You can install them with:
   ```r
   install.packages(c("dplyr", "ggplot2", "treemapify", "tidyr", "purrr"))
   ```

3. Run the R scripts for your analysis needs:
   ```r
   source("content_analysis.R")
   ```
