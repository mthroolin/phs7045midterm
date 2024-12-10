#' Generate Synthetic Data Without Duplicates for Numerical Variables
#'
#' This function generates a synthetic dataset with specified numbers of records, IDs, 
#' and variables. Numerical variables are ensured to have no duplicates across `ID`, 
#' `t`, and `var`. It also generates metadata for variable types and population data.
#'
#' @param num_records Integer. The total number of records to generate.
#' @param num_ids Integer. The total number of unique IDs to generate.
#' @param num_vars Integer. The total number of variables to generate.
#' @return A list containing the following elements:
#' \item{df}{A `data.table` with the generated dataset, including `ID`, `t`, `var`, and `value`.}
#' \item{df_population}{A `data.table` with the population data (unique IDs).}
#' \item{var_type_override}{A named vector indicating whether each variable is "numerical" or "categorical".}
#' @importFrom data.table data.table :=
#' @importFrom stats rnorm
#' @examples
#' # Generate a dataset with 1000 records, 100 unique IDs, and 50 variables
#' data_list <- generate_data(num_records = 1000, num_ids = 100, num_vars = 50)
#' df <- data_list$df
#' df_population <- data_list$df_population
#' var_type_override <- data_list$var_type_override
#'
#' # Inspect the generated data
#' head(df)
#' head(df_population)
#' var_type_override
#' @import data.table
#' @export
generate_data <- function(num_records, num_ids, num_vars) {
  # Generate IDs
  IDs <- sample(1:num_ids, num_records, replace = TRUE)
  
  # Generate time points
  t_values <- sample(c(NA_real_, seq(0, 10, by = 0.1)), num_records, replace = TRUE)
  
  # Generate variables
  vars <- paste0("var", sample(1:num_vars, num_records, replace = TRUE))
  
  # Generate variable types (categorical or numerical)
  var_types <- rep("numerical", num_vars)
  categorical_vars <- paste0("var", sample(1:num_vars, num_vars * 0.1))  # 10% categorical
  if (!is.null(categorical_vars) && length(categorical_vars) > 0) {
    var_types[match(categorical_vars, paste0("var", 1:num_vars), nomatch = 0)] <- "categorical"
  }
  var_type_override <- setNames(var_types, paste0("var", 1:num_vars))
  
  # Combine into a data.table
  df <- data.table(
    ID = as.character(IDs),
    t = t_values,
    variable_name = vars
  ) |> unique()
  
  # Identify numerical variables
  numerical_vars <- names(var_type_override)[var_type_override == "numerical"]
  
  # Remove duplicates for numerical variables
  df_num <- df[variable_name %in% numerical_vars]
  df_cat <- df[!variable_name %in% numerical_vars]
  
  df_num <- unique(df_num, by = c("ID", "t", "variable_name"))
  
  # Combine back
  df <- rbind(df_num, df_cat)
  
  # Generate values
  df[, variable_value := as.character(sample(c(NA_real_, rnorm(.N)), .N, replace = TRUE))]
  
  # Generate population data
  df_population <- data.table(ID = as.character(1:num_ids))
  
  return(list(df = df, df_population = df_population, var_type_override = var_type_override))
}