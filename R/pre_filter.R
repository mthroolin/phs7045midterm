library(data.table)
library(Rcpp)



# Pre-filter function using data.table
pre_filter_dt <- function(df, df_population, threshold, max.T, var_type_override) {
  # Convert to data.table for speed
  setDT(df)
  setDT(df_population)
  
  # 1. Filter rows based on population
  df <- df[ID %in% df_population$ID]
  
  # 2. Filter rows with time outside [0, T)
  df <- df[is.na(t) | (t >= 0 & t < max.T)]
  
  # 3. Identify categorical/hierarchical variables
  categorical_vars <- names(var_type_override)[
    grepl("hierarchical|categorical", var_type_override, ignore.case = TRUE)
  ]
  df_num <- df[!var %in% categorical_vars]
  
  # Check for duplicate numerical inconsistencies
  if (anyDuplicated(df_num, by = c("ID", "t", "var")) > 0) {
    stop("Inconsistent numerical values found")
  }
  
  # 4. Calculate variable frequencies
  var_counts <- df[, .(count = uniqueN(ID)), by = var]
  var_counts[, freq := count / uniqueN(df_population$ID)]
  
  # Keep variables above the threshold
  vars_keep <- var_counts[freq > threshold, var]
  df <- df[var %in% vars_keep]
  
  # print(paste("Remaining variables:", length(vars_keep)))
  return(df)
}


# # Sample data to simulate the input
# data <- "
# ID,t,var,value
# 1,1,age,55
# 1,2,heart_rate,72
# 2,3,heart_rate,80
# 2,,sex,male
# 3,1,age,60
# 3,5,heart_rate,90
# 4,0,bp_systolic,120
# 4,,sex,female
# 5,8,bp_systolic,130
# "
# 
# population_data <- "
# ID
# 1
# 2
# 3
# 4
# 5
# "
# 
# # Load data into data.tables
# df <- fread(data)
# df_population <- fread(population_data)
# 
# # Convert 't' to numeric and 'value' to character
# # Ensure 'ID' is character, 't' is numeric, 'var' and 'value' are character
# df[, ID := as.character(ID)]
# df_population[, ID := as.character(ID)]
# 
# df[, t := as.numeric(t)]
# df[, var := as.character(var)]
# df[, value := as.character(value)]
# 
# # Convert 'df' and 'df_population' to data.frames in place
# setDF(df)
# setDF(df_population)
# 
# # Test parameters
# max.T <- 6
# threshold <- 0.2
# 
# 
# # Apply the function and view results
# filtered_df <- pre_filter_dt(df, df_population, threshold, max.T, var_type_override)
# print(filtered_df)
# 
# filtered_df_cpp <- pre_filter_cpp(df, df_population, threshold, max.T, unlist(var_type_override))
# print(filtered_df_cpp)
