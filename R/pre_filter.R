#' Pre-Filter Data with data.table
#'
#' This function filters a dataset based on population inclusion, time constraints, 
#' variable types, and frequency thresholds. It is designed to handle large datasets 
#' efficiently using the `data.table` package.
#'
#' @param df A `data.table` containing the dataset to filter. Must include the columns 
#'   `ID`, `t`, `var`, and optionally `value`.
#' @param df_population A `data.table` containing the population data with a column `ID`.
#'   Only rows in `df` with IDs in `df_population$ID` will be retained.
#' @param threshold Numeric. Minimum frequency threshold for variables to be retained, 
#'   as a proportion of unique IDs in `df_population`.
#' @param max.T Numeric. Maximum time value for filtering rows in `df`. Rows with `t >= max.T` 
#'   will be excluded.
#' @param var_type_override Named character vector. Indicates the types of variables 
#'   (e.g., "categorical", "numerical", "hierarchical"). Variable types are used to identify 
#'   categorical or hierarchical variables for special handling.
#' @return A filtered `data.table` containing only the rows that satisfy the filtering criteria.
#' @details The function performs the following filtering steps:
#' \itemize{
#'   \item Filters rows in `df` to include only IDs present in `df_population`.
#'   \item Retains rows with time (`t`) values within the range \[0, `max.T`\).
#'   \item Identifies categorical or hierarchical variables based on `var_type_override` and ensures 
#'         that numerical variables have no duplicate values for the same `ID` and `t`.
#'   \item Calculates the frequency of each variable as the proportion of unique IDs and retains 
#'         variables with frequencies above the specified `threshold`.
#' }
#' @import data.table
#' @examples
#' library(data.table)
#' 
#' # Sample dataset
#' df <- data.table(
#'   ID = c("1", "1", "2", "2", "3", "3", "4", "4", "5"),
#'   t = c(1, 2, 3, NA, 1, 5, 0, NA, 8),
#'   var = c("age", "heart_rate", "heart_rate", "sex", "age",
#'             "heart_rate", "bp_systolic", "sex", "bp_systolic"),
#'   value = c("55", "72", "80", "male", "60", "90", "120", "female", "130")
#' )
#' 
#' # Population data
#' df_population <- data.table(ID = c("1", "2", "3", "4", "5"))
#' 
#' # Variable type override
#' var_type_override <- c(age = "numerical", heart_rate = "numerical", 
#'                        bp_systolic = "numerical", sex = "categorical")
#' 
#' # Parameters
#' max.T <- 6
#' threshold <- 0.2
#' 
#' # Apply the pre_filter_dt function
#' filtered_df <- pre_filter_dt(df, df_population, threshold, max.T, var_type_override)
#' print(filtered_df)
#' @import data.table
#' @export
pre_filter_dt <- function(df, df_population, threshold, max.T, var_type_override) {
  # Convert to data.table for speed
  data.table::setDT(df)
  data.table::setDT(df_population)
  
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
  var_counts <- df[, .(count = data.table::uniqueN(ID)), by = var]
  var_counts[, freq := count / data.table::uniqueN(df_population$ID)]
  
  # Keep variables above the threshold
  vars_keep <- var_counts[freq > threshold, var]
  df <- df[var %in% vars_keep]
  
  return(df)
}