#' Run FIDDLE Python Script with R Data
#'
#' This function runs the FIDDLE Python module with data passed as an R object.
#'
#' @param data_file A data frame or matrix to be used as the data input.
#' @param population_file Path to a vector or data frame representing the population.
#' @param config_file Path to config file.
#' @param output_dir Path to output directory.
#' @param T_ Time period.
#' @param dt Time delta.
#' @param theta_1 First theta value. Default is 0.001.
#' @param theta_2 Second theta value. Default is 0.001.
#' @param theta_freq Theta frequency. Default is 1.0.
#' @param stats_functions Statistical functions to apply. Default is c("min", "max", "mean").
#' @param impute_method Method to use for imputing missing values ("ffill","mean","median", "linear", or "none"). Default is "ffill".
#' @param python_env Python virtual environment or path to Python executable.
#' @return Outputs from the FIDDLE Python module.
#' @export
run_fiddle <- function(data_file, population_file, config_file, output_dir,
                       T_ = 4, dt = 1.0,
                       theta_1 = 0.001, theta_2 = 0.001, theta_freq = 1,
                       stats_functions = c("min", "max", "mean"),
                       impute_method = "ffill",
                       python_env = "test_env") {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  } 
  
  # Activate the virtual environment
  reticulate::use_virtualenv(python_env, required = TRUE)
  
  # Resolve Python executable path
  python_path <- reticulate::virtualenv_python(python_env)
  
  # Construct the command
  command <- c(
    python_path,
    "-m", "FIDDLE.run",
    sprintf("--data_fname=%s", data_file),
    sprintf("--population_fname=%s", population_file),
    sprintf("--config_fname=%s", config_file),
    sprintf("--output_dir=%s", output_dir),
    sprintf("--T=%f", T_),
    sprintf("--dt=%f", dt),
    sprintf("--theta_1=%f", theta_1),
    sprintf("--theta_2=%f", theta_2),
    sprintf("--theta_freq=%f", theta_freq),
    paste0("--stats_functions=", stats_functions),
    sprintf("--impute_method=%s", impute_method)
  )
  
  old_dir <- getwd()
  on.exit(setwd(old_dir))
  setwd(system.file("python", package = "fiddleR"))
  # Run the command and capture the output
  result <- system2(command[1], args = command[-1], stdout = TRUE, stderr = TRUE)
  
  # Print the result
  print(result)
  
  # Return the result
  return(result)
}

# # Example usage
# result <- run_fiddle(
#   data_file = "input/data.csv",
#   population_file = "input/pop.csv",
#   config_file = "input/config-1-parallel.yaml",
#   output_dir = "output-FFILL"
# )
