# Function to generate synthetic data without duplicates for numerical variables
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
  var_types[match(categorical_vars, paste0("var", 1:num_vars))] <- "categorical"
  var_type_override <- setNames(var_types, paste0("var", 1:num_vars))
  
  # Combine into a data.table
  df <- data.table(
    ID = as.character(IDs),
    t = t_values,
    var = vars) |>
    unique()
  
  # Identify numerical variables
  numerical_vars <- names(var_type_override)[var_type_override == "numerical"]
  
  # Remove duplicates for numerical variables
  df_num <- df[var %in% numerical_vars]
  df_cat <- df[!var %in% numerical_vars]
  
  df_num <- unique(df_num, by = c("ID", "t", "var"))
  
  # Combine back
  df <- rbind(df_num, df_cat)
  
  # Generate values
  df[, value := as.character(sample(c(NA_real_, rnorm(.N)), .N, replace = TRUE))]
  
  # Generate population data
  df_population <- data.table(ID = as.character(1:num_ids))
  
  return(list(df = df, df_population = df_population, var_type_override = var_type_override))
}