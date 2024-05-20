# Install and load necessary libraries
install.packages("Hmisc")
install.packages("astsa")
install.packages("rstudioapi")
install.packages("lomb")
install.packages("zoo")

library(astsa)
library(rstudioapi)
library(Hmisc)
library(lomb)
library(zoo)

#-----------------------------------------------------------------------------#
#  SECTION 1: if you d not need to interpolate ages jump to section 2(row73)  #
#-----------------------------------------------------------------------------#
# Define main function
main <- function() {
  # Prompt user to select folder directory
  folder_directory <- rstudioapi::selectDirectory("Select Folder Directory")
  setwd(folder_directory)
  
  # Prompt user to select input dataset
  input_filename <- rstudioapi::selectFile("Select Input Dataset", path = folder_directory)
  df_raw <- read.delim(input_filename)
  
  # Prompt user to select the variable
  selected_variable <- select.list(
    names(df_raw), 
    title = "Select tie age variable", 
    multiple = FALSE
  )
  
  # Prompt user to select another variable
  selected_variable_2 <- select.list(
    names(df_raw), 
    title = "Select tie depth Variable", 
    multiple = FALSE
  )
  
  # Create a data frame with only the provided depth and age data
  depth_age_data <- df_raw[!is.na(df_raw[[selected_variable]]), c(selected_variable_2, selected_variable)]
  
  # Interpolate ages for all depths
  interpolated_ages <- approxfun(depth_age_data[[selected_variable_2]], depth_age_data[[selected_variable]])(df_raw[[selected_variable_2]])
  
  # Update the dataframe with the interpolated ages
  df_raw$Interpolated_Age <- interpolated_ages
  
  # Find the maximum length
  max_length <- max(length(interpolated_ages), nrow(df_raw))
  
  # Scale interpolated ages to match the maximum length
  scaled_interpolated_ages <- approx(
    x = seq_along(interpolated_ages),
    y = interpolated_ages,
    n = max_length
  )$y
  
  # Update the dataframe with scaled interpolated ages
  df_raw$scaled_interpolated_ages <- scaled_interpolated_ages
  
  # Save the updated dataframe as .txt file
  write.table(df_raw, file = "interpolated.ages.data.txt", sep = "\t", row.names = FALSE)
  
  # Optional: Continue with the rest of your analysis or data processing
  cat("now check into the WD foldet the file called interpolated.ages.data, interpolated ages are in in scaled_interpolated_ages column")
  
}

# Call the main function
main()

# ----------------------------------------------------------------------------#
#                                   SECTION 2                                 #
#-----------------------------------------------------------------------------#

# Define main function
main <- function() {
  # Prompt user to select folder directory
  folder_directory <- rstudioapi::selectDirectory("Select Folder Directory")
  setwd(folder_directory)
  
  # Prompt user to select input dataset
  input_filename <- rstudioapi::selectFile("Select Input Dataset", path = folder_directory)
  df_raw <- read.delim(input_filename)
  
  # Prompt user to select variables to analyze
  selected_variables <- select.list(
    names(df_raw),
    title = "Select Variables to Analyze",
    multiple = TRUE,
    graphics = TRUE
  )
  variables <- df_raw[, selected_variables]
  
  # Prompt user to set period parameter
  selected_period_column <- select.list(
    names(df_raw),
    title = "Select Period Parameter Column",
    graphics = TRUE
  )
  period <- df_raw[[selected_period_column]]
  
  # Function to save summary of periodogram as CSV
  save_periodogram_summary <- function(periodogram_summary, variable_name, folder_path) {
    filename <- paste(folder_path, "/", variable_name, "_summary.csv", sep = "")
    write.csv(periodogram_summary, file = filename, row.names = TRUE)
  }
  
  # Function to analyze periodogram results and save periodogram as PDF
  analyze_periodogram_and_save_summary <- function(selected_column, variable_name, time_variable, plot_folder) {
    # Compute Lomb-Scargle periodogram for selected variable considering time as Ages Ma
    periodogram <- lsp(x = selected_column, 
                       times = time_variable, 
                       type = "period", 
                       ofac = 5,
                       alpha = 0.05,
                       plot = TRUE)
    
    # Save periodogram as PDF with A4 size
    plot_filename <- paste(plot_folder, "/", variable_name, "_periodogram.pdf", sep="")
    pdf(plot_filename, width = 11.69, height = 8.27)
    plot(periodogram, main = paste(variable_name, "Lomb-Scargle Periodogram", sep = " - "))
    dev.off()
    
    # Save periodogram summary as CSV
    periodogram_summary <- summary(periodogram)
    save_periodogram_summary(periodogram_summary, variable_name, plot_folder)
  }
  
  # Create folder for plots
  plot_folder <- paste(folder_directory, "/Plot", sep="")
  dir.create(plot_folder, showWarnings = FALSE)
  
  # Analyze each variable for plots
  analised_variables_list <- lapply(selected_variables, function(variable_name) {
    analyze_periodogram_and_save_summary(variables[[variable_name]], variable_name, period, plot_folder)
  })
}

# Call the main function
main()

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
