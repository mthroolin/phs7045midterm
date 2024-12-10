#' Pre-filter Data Table in Parallel
#'
#' This function performs pre-filtering on a data.table in parallel. It filters rows based on population,
#' time constraints, and variable types. It also calculates variable frequencies in parallel and removes
#' variables that do not meet a specified frequency threshold.
#'
#' @param df A data.table containing the main dataset to filter.
#' @param df_population A data.table containing the population data for filtering.
#' @param threshold A numeric value specifying the minimum frequency threshold for variables to retain.
#' @param max.T A numeric value specifying the maximum allowed time for filtering rows.
#' @param var_type_override A named vector indicating the types of variables (e.g., "categorical", "hierarchical").
#'
#' @return A filtered data.table containing only the rows and variables that meet the specified criteria.
#'
#' @import data.table
#' @importFrom parallel detectCores makeCluster clusterEvalQ clusterExport parLapply stopCluster
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' # Example data
#' df <- data.table(ID = c(1, 1, 2, 2, 3),
#'                  var = c("age", "sex", "age", "sex", "age"),
#'                  t = c(0.5, 1, 2, 3, 0.5))
#' df_population <- data.table(ID = c(1, 2, 3))
#' var_type_override <- c(age = "numerical", sex = "categorical")
#'
#' # Run pre-filtering
#' result <- pre_filter_dt_parallel(
#'   df = df,
#'   df_population = df_population,
#'   threshold = 0.5,
#'   max.T = 2,
#'   var_type_override = var_type_override
#' )
#' print(result)
#' }
#' 
#' @export
pre_filter_dt_parallel <- function(df, df_population, threshold, max.T, var_type_override) {
  # Define count_unique_ids inside the function
  count_unique_ids <- function(sub_df) {
    var_name <- unique(sub_df$var)
    count <- uniqueN(sub_df$ID)
    list(var = var_name, count = count)
  }
  
  # Ensure df and df_population are data.tables
  setDT(df)
  setDT(df_population)
  
  # Set the number of cores
  numCores <- detectCores() - 1
  
  # 1. Filter rows based on population
  setkey(df, ID)
  setkey(df_population, ID)
  df <- df[df_population, nomatch = 0]
  
  # 2. Filter rows with time outside [0, max.T)
  df <- df[is.na(t) | (t >= 0 & t < max.T)]
  
  # 3. Identify categorical/hierarchical variables
  categorical_vars <- names(var_type_override)[
    grepl("hierarchical|categorical", var_type_override, ignore.case = TRUE)
  ]
  df_num <- df[!var %in% categorical_vars]
  
  # Check for duplicate numerical inconsistencies
  if (df_num[, .N, by = .(ID, t, var)][N > 1L, .N] > 0L) {
    stop("Inconsistent numerical values found")
  }
  
  # 4. Calculate variable frequencies in parallel
  df_split <- split(df, by = "var", keep.by = TRUE)
  
  cl <- makeCluster(numCores)
  
  clusterEvalQ(cl, {
    library(data.table)
  })
  
  clusterExport(cl, varlist = c("count_unique_ids"), envir = environment())
  
  var_counts_list <- parLapply(cl, df_split, count_unique_ids)
  
  stopCluster(cl)
  
  var_counts <- rbindlist(lapply(var_counts_list, as.data.table))
  
  total_IDs <- uniqueN(df_population$ID)
  var_counts[, freq := count / total_IDs]
  
  vars_keep <- var_counts[freq > threshold, var]
  df <- df[var %in% vars_keep]
  
  message("Remaining variables: ", length(vars_keep))
  return(df)
}
