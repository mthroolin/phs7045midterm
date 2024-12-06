#' Run FIDDLE Python Script with R Data
#'
#' This function runs the FIDDLE Python module with data passed as an R object.
#'
#' @param data_file A data frame or matrix to be used as the data input.
#' @param population A vector or data frame representing the population.
#' @param config Path to config file. Defaults to FIDDLE/conf-default.yaml
#' @param T_ Time period.
#' @param dt Time delta.
#' @param theta_1 First theta value. Default is 0.001.
#' @param theta_2 Second theta value. Default is 0.001.
#' @param theta_freq Theta frequency. Default is 1.0.
#' @param stats_functions Statistical functions to apply. Default is c("min", "max", "mean").
#' @param python_env Python virtual environment or path to Python executable.
#' @return Outputs from the FIDDLE Python module.
#' @export
# run_fiddle <- function(data, population, T_, dt,
#                        theta_1 = 0.001, theta_2 = 0.001, theta_freq = 1.0,
#                        stats_functions = c("min", "max", "mean"),
#                        # binarize = TRUE,
#                        config_path = system.file("python", "FIDDLE", "config-default.yaml", package ="fiddleR"),
#                        python_env = "test_env",
#                        fiddle_dir = system.file("python/FIDDLE", package = "fiddleR")) {
# 
#   # Resolve the Python executable path
#   if (!is.null(python_env)) {
#     virtualenv_path <- reticulate::virtualenv_root()
#     python_executable <- file.path(virtualenv_path, python_env, "Scripts", "python.exe")
#     python_executable <- normalizePath(python_executable, winslash = "/", mustWork = TRUE)
#     if (!file.exists(python_executable)) {
#       stop("Python executable not found in the specified virtual environment: ", python_executable)
#     }
#     message("Using Python environment: ", python_executable)
#   } else {
#     stop("Python environment not specified.")
#   }
# 
#   # Add FIDDLE directory to PYTHONPATH without duplicates
#   current_pythonpath <- Sys.getenv("PYTHONPATH", unset = "")
#   if (!grepl(fiddle_dir, current_pythonpath, fixed = TRUE)) {
#     updated_pythonpath <- if (current_pythonpath == "") {
#       fiddle_dir
#     } else {
#       paste(current_pythonpath, fiddle_dir, sep = .Platform$path.sep)
#     }
#     Sys.setenv(PYTHONPATH = updated_pythonpath)
#     message("FIDDLE directory added to PYTHONPATH: ", fiddle_dir)
#   } else {
#     message("FIDDLE directory already present in PYTHONPATH: ", fiddle_dir)
#   }
# 
#   # Write data to temporary CSV files
#   data_file <- tempfile(fileext = ".csv")
#   population_file <- tempfile(fileext = ".csv")
#   write.csv(data, data_file, row.names = FALSE)
#   write.csv(population, population_file, row.names = FALSE)
#   message("Temporary data files created.")
#   output_dir <- tempfile("fiddle_output")
#   dir.create(output_dir)
# 
#   # Build the system command to run FIDDLE
#   command <- sprintf(
#     '"%s" -m FIDDLE.run --data_fname=%s --population_fname=%s --config_fname= %s --output_dir=%s --T=%f --dt=%f --theta_1=%f --theta_2=%f --theta_freq=%f %s',
#     python_executable,
#     shQuote(data_file),
#     shQuote(population_file),
#     shQuote(config_path),
#     shQuote(output_dir),
#     T_,
#     dt,
#     theta_1,
#     theta_2,
#     theta_freq,
#     paste0("--stats_functions=", stats_functions, collapse = " ")
#     # as.integer(binarize),
#   )
# 
#   # Informing about the command
#   message("Running FIDDLE with the following command:")
#   message(command)
# 
#   # Run the command and capture output
#   old_dir <- getwd()
#   on.exit(setwd(old_dir))
#   setwd(system.file("python", package = "fiddleR"))
#   result <- tryCatch({
#     system(command, intern = TRUE)
#   }, error = function(e) {
#     stop("Error running FIDDLE: ", conditionMessage(e))
#   })
# 
#   # Check for errors in the command execution
#   if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
#     stop("Error running FIDDLE: ", paste(result, collapse = "\n"))
#   }
# 
#   # Return result
#   return(result)
# }



run_fiddle <- function(data_file, population_file, config_file, output_dir,
                       T_ = 4, dt = 1.0,
                       theta_1 = 0.001, theta_2 = 0.001, theta_freq = 1,
                       stats_functions = c("min", "max", "mean"),
                       impute_method = "ffill",
                       python_env = "test_env") {
  
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
