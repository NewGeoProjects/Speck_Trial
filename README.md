# Speck_Trial
# README: Usage Guide for a user friendly way to do Time Series Analysis in R

## Overview
This script provides a comprehensive toolset for interpolating ages, analyzing periodograms, and comparing spectral data in time series analysis, accommodating both unevenly and evenly sampled signals. 
The script is divided into three main sections: interpolating ages, analyzing periodograms, and comparing periodograms.

## Installation of Necessary Libraries
Before running the script, install the necessary libraries by executing the following commands in your R environment:
```R
install.packages("Hmisc")
install.packages("astsa")
install.packages("rstudioapi")
install.packages("lomb")
install.packages("zoo")
```

## Loading Libraries
Load the required libraries using:
```R
library(Hmisc)
library(astsa)
library(rstudioapi)
library(lomb)
library.zoo)
```

## Script Sections

### Section 1: Interpolating Ages
This section interpolates ages for given depth data in a dataset. This part is particularly useful for unevenly sampled signals, where data points are not equally spaced.

**Steps to Run:**
1. Execute the `main` function.
2. Follow the prompts to select:
   - A folder directory.
   - An input dataset file.
   - The variable representing ages.
   - The variable representing depths.

The script will interpolate the ages and save the updated dataset with interpolated ages in a file named `interpolated.ages.data.txt` in the selected folder directory.

### Section 2: Analyzing Periodograms
This section computes and analyzes Lomb-Scargle periodograms for selected variables in a dataset. This is suitable for unevenly sampled signals, where the time intervals between data points are irregular.

**Steps to Run:**
1. Execute the `main` function.
2. Follow the prompts to select:
   - A folder directory.
   - An input dataset file.
   - Variables for analysis.
   - A column representing the period parameter (time).

The script will generate periodograms for the selected variables, save plots as PDFs, and summaries as CSV files in a subfolder named `Plot` within the selected folder directory.

### Section 3: Comparing Spectra
This section compares periodograms from two different datasets. This analysis is designed for evenly sampled signals, where the time intervals between data points are consistent. This consistency allows for a more straightforward comparison of spectral features.

**Steps to Run:**
1. Execute the `main_compare_periodograms` function.
2. Follow the prompts to select:
   - A folder directory.
   - The first and second input dataset files.
   - Variables for periodogram analysis in both datasets.
   - A column representing the period parameter (time) in both datasets.

The script will compare the periodograms and display comparison results, focusing on peak frequencies and their amplitudes.

**Script Code:**
```R
#-----------------------------------------------------------------------------#

#                     Compare Spectra                                         #
# Define function to compare periodograms
compare_periodograms <- function(periodogram1, periodogram2, variable_names) {
  # Compare periodograms using statistical measures or visual inspection
  # For example, you can compare peaks, significance levels, or overall shapes
  
  # Perform statistical comparison
  # For demonstration, let's compare the peak frequencies and their corresponding amplitudes
  peak_freq1 <- periodogram1$freq[periodogram1$spec == max(periodogram1$spec)]
  peak_freq2 <- periodogram2$freq[periodogram2$spec == max(periodogram2$spec)]
  peak_ampl1 <- max(periodogram1$spec)
  peak_ampl2 <- max(periodogram2$spec)
  
  # Display comparison results
  cat("Comparison between", variable_names[1], "and", variable_names[2], "periodograms:\n")
  cat("Peak frequency of", variable_names[1], ":", peak_freq1, "\n")
  cat("Peak frequency of", variable_names[2], ":", peak_freq2, "\n")
  cat("Amplitude of peak in", variable_names[1], ":", peak_ampl1, "\n")
  cat("Amplitude of peak in", variable_names[2], ":", peak_ampl2, "\n")
  
  # You can add more detailed comparisons or statistical tests here
}

# Call the main function
main_compare_periodograms <- function() {
  # Prompt user to select folder directory
  folder_directory <- rstudioapi::selectDirectory("Select Folder Directory")
  setwd(folder_directory)
  
  # Prompt user to select input datasets for comparison
  cat("Select the first input dataset for comparison:\n")
  input_filename1 <- file.choose()
  cat("Select the second input dataset for comparison:\n")
  input_filename2 <- file.choose()
  
  # Read datasets and perform periodogram analysis
  periodograms <- lapply(list(input_filename1, input_filename2), function(input_filename) {
    df_raw <- read.delim(input_filename)
    selected_variable <- select.list(names(df_raw), title = "Select Variable for Periodogram Analysis", multiple = FALSE)
    selected_period_column <- select.list(names(df_raw), title = "Select Period Parameter Column", graphics = TRUE)
    
    # Compute Lomb-Scargle periodogram
    periodogram <- lsp(x = df_raw[[selected_variable]], 
                       times = df_raw[[selected_period_column]], 
                       type = "period", 
                       ofac = 5,
                       alpha = 0.05)
    
    # Return periodogram and variable name
    return(list(periodogram = periodogram, variable_name = selected_variable))
  })
  
  # Compare periodograms
  compare_periodograms(periodograms[[1]]$periodogram, periodograms[[2]]$periodogram, 
                       c(periodograms[[1]]$variable_name, periodograms[[2]]$variable_name))
}

# Call the function to compare periodograms
main_compare_periodograms()
```

## Usage Example

### Running Section 1
```R
main() # Run this function for interpolating ages
```

### Running Section 2
```R
main() # Run this function for analyzing periodograms
```

### Running Section 3
```R
main_compare_periodograms() # Run this function for comparing periodograms
```

## Output
- **Section 1 Output:** `interpolated.ages.data.txt` file containing the dataset with interpolated ages.
- **Section 2 Output:** PDF files for periodograms and CSV files for summaries stored in the `Plot` folder.
- **Section 3 Output:** Comparison results printed in the console.

## Notes
- Ensure your dataset files are in the correct format and accessible.
- Follow the prompts carefully to select the correct files and variables.
- Check the output files in the specified folder directory for results.

