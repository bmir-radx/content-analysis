# RADx Data Hub Content Analysis

This repository contains R scripts and tools to generate visualizations and conduct analysis on the content from the **RADx Data Hub** repository.

## Features
- Generate various visualizations such as:
  - Treemaps
  - Heatmaps
  - Bar charts
  - Pie charts
  - Bubble charts
- Analyze multiple dimensions, including:
  - Study participant sizes
  - Publication counts
  - Study topics
- Apply multiple custom color palettes to your visualizations.
  
## Repository Structure
- `content_analysis.R`: The main R script containing analysis and visualization functions.
- `content_analysis_old.R`: An older version of the script retained for reference.
- `files/`: Directory containing necessary data files used in the analysis.
- `plots/`: Directory where generated visualizations (plots) are saved.

## Getting Started

### 1. Clone the Repository:
```bash
git clone https://github.com/bmir-radx/content-analysis.git
cd content-analysis
```

### 2. Install Necessary R Packages:
You will need the following R packages:
- `dplyr`
- `ggplot2`
- `treemapify`
- `tidyr`
- `purrr`
- `viridis`
  
You can install them using:
```r
install.packages(c("dplyr", "ggplot2", "treemapify", "tidyr", "purrr", "viridis"))
```

### 3. Running the Analysis and Generating Plots:
Run the main script to conduct the analysis and generate the plots:
```r
source("content_analysis.R")
```

### 4. Viewing Generated Plots:
All generated plots are saved in the `plots/` directory. Plots are created using different color palettes and formats. You can modify the output options within the script to customize the analysis further.
