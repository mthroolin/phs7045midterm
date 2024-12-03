#' Run FIDDLE Python Script with R Data
#'
#' This function runs the FIDDLE Python module with data passed as an R object.
#'
#' @param data A data frame or matrix to be used as the data input.
#' @param population A vector or data frame representing the population.
#' @param T_ Time period.
#' @param dt Time delta.
#' @param theta_1 First theta value. Default is 0.001.
#' @param theta_2 Second theta value. Default is 0.001.
#' @param theta_freq Theta frequency. Default is 1.0.
#' @param stats_functions Statistical functions to apply. Default is c("min", "max", "mean").
#' @param python_env Python virtual environment or path to Python executable.
#' @return Outputs from the FIDDLE Python module.
#' @export
run_fiddle <- function(data, population, T_, dt, 
                       theta_1 = 0.001, theta_2 = 0.001, theta_freq = 1.0,
                       stats_functions = c("min", "max", "mean"), 
                       binarize = TRUE, python_env = "test_env",
                       fiddle_dir = system.file("python/FIDDLE", package = utils::packageName())) {
  
  # Resolve the Python executable path
  if (!is.null(python_env)) {
    virtualenv_path <- reticulate::virtualenv_root()
    python_executable <- file.path(virtualenv_path, python_env, "Scripts", "python.exe")
    python_executable <- normalizePath(python_executable, winslash = "/", mustWork = TRUE)
    if (!file.exists(python_executable)) {
      stop("Python executable not found in the specified virtual environment: ", python_executable)
    }
    message("Using Python environment: ", python_executable)
  } else {
    stop("Python environment not specified.")
  }
  
  # Add FIDDLE directory to PYTHONPATH without duplicates
  current_pythonpath <- Sys.getenv("PYTHONPATH", unset = "")
  if (!grepl(fiddle_dir, current_pythonpath, fixed = TRUE)) {
    updated_pythonpath <- if (current_pythonpath == "") {
      fiddle_dir
    } else {
      paste(current_pythonpath, fiddle_dir, sep = .Platform$path.sep)
    }
    Sys.setenv(PYTHONPATH = updated_pythonpath)
    message("FIDDLE directory added to PYTHONPATH: ", fiddle_dir)
  } else {
    message("FIDDLE directory already present in PYTHONPATH: ", fiddle_dir)
  }
  
  # Write data to temporary CSV files
  data_file <- tempfile(fileext = ".csv")
  population_file <- tempfile(fileext = ".csv")
  write.csv(data, data_file, row.names = FALSE)
  write.csv(population, population_file, row.names = FALSE)
  message("Temporary data files created.")
  output_dir <- tempfile("fiddle_output")
  dir.create(output_dir)
  
  # Build the system command to run FIDDLE
  command <- sprintf(
    '"%s" -m FIDDLE.run --data_fname=%s --population_fname=%s --T=%f --dt=%f --theta_1=%f --theta_2=%f --theta_freq=%f %s --output_dir=%s',
    python_executable,
    shQuote(data_file),
    shQuote(population_file),
    T_,
    dt,
    theta_1,
    theta_2,
    theta_freq,
    paste0("--stats_functions=", stats_functions, collapse = " "),
    # as.integer(binarize),
    shQuote(output_dir)
  )
  
  # Informing about the command
  message("Running FIDDLE with the following command:")
  message(command)
  
  # Run the command and capture output
  old_dir <- getwd()
  on.exit(setwd(old_dir))
  setwd(system.file("python", package = "phs7045midterm"))
  result <- tryCatch({
    system(command, intern = TRUE)
  }, error = function(e) {
    stop("Error running FIDDLE: ", conditionMessage(e))
  })
  
  # Check for errors in the command execution
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    stop("Error running FIDDLE: ", paste(result, collapse = "\n"))
  }
  
  # Return result
  return(result)
}


# run_fiddle <- function(data, population, T_, dt,
#                        theta_1 = 0.001, theta_2 = 0.001, theta_freq = 1.0,
#                        stats_functions = c("min", "max", "mean"),
#                        binarize = TRUE, python_env = NULL) {
#   
#   # Ensure Python environment is activated
#   if (!is.null(python_env)) {
#     reticulate::use_virtualenv(python_env, required = TRUE)
#   }
#   
#   # Locate the FIDDLE module path
#   fiddle_dir <- system.file("python/FIDDLE", package = utils::packageName())
#   if (fiddle_dir == "") {
#     stop("Could not find the 'FIDDLE' Python module in the package.")
#   }
#   
#   # Set PYTHONPATH
#   #Sys.setenv(PYTHONPATH = fiddle_dir)
#   
#   # Write data and population to temporary files
#   data_file <- tempfile(fileext = ".csv")
#   population_file <- tempfile(fileext = ".csv")
#   write.csv(data, data_file, row.names = FALSE)
#   write.csv(population, population_file, row.names = FALSE)
#   
#   # Construct the command
#   command <- paste(
#     "python -m FIDDLE.run",
#     paste0("--data_fname=", data_file),
#     paste0("--population_fname=", population_file),
#     paste0("--T=", T_),
#     paste0("--dt=", dt),
#     paste0("--theta_1=", theta_1),
#     paste0("--theta_2=", theta_2),
#     paste0("--theta_freq=", theta_freq),
#     paste0("--stats_functions=", paste(stats_functions, collapse = " ")),
#     if (binarize) "--binarize"
#   )
#   
#   # Run the command
#   result <- system(command, intern = TRUE)
#   
#   # Return results
#   return(result)
# }

# run_fiddle <- function(data, population, T_, dt, 
#                        theta_1 = 0.001, theta_2 = 0.001, theta_freq = 1.0,
#                        stats_functions = c("min", "max", "mean"), 
#                        binarize = TRUE, python_env = NULL) {
#   
#   # Ensure Python environment is set up
#   if (!is.null(python_env)) {
#     if (!reticulate::virtualenv_exists(python_env)) {
#       stop("The specified Python environment does not exist.")
#     }
#     reticulate::use_virtualenv(python_env, required = TRUE)
#   } else {
#     stop("Python environment is not specified.")
#   }
#   
#   # Locate the FIDDLE module
#   fiddle_dir <- system.file("python/FIDDLE", package = utils::packageName())
#   if (fiddle_dir == "") {
#     stop("Could not find the 'FIDDLE' Python module in the package.")
#   }
#   
#   cat("Using Python environment:", reticulate::py_config()$python, "\n")
#   
#   # Write data and population to temporary files
#   data_file <- tempfile(fileext = ".csv")
#   population_file <- tempfile(fileext = ".csv")
#   write.csv(data, data_file, row.names = FALSE)
#   write.csv(population, population_file, row.names = FALSE)
#   
#   # Import the FIDDLE module
#   fiddle <- reticulate::import("FIDDLE.run", convert = FALSE)
#   
#   # Call the FIDDLE module with the arguments
#   result <- fiddle$run(
#     data_fname = data_file,
#     population_fname = population_file,
#     T_ = T_,
#     dt = dt,
#     theta_1 = theta_1,
#     theta_2 = theta_2,
#     theta_freq = theta_freq,
#     stats_functions = stats_functions,
#     binarize = binarize
#   )
#   
#   # Convert the Python result back to R
#   return(reticulate::py_to_r(result))
# }
# 
# 



# run_fiddle <- function(data, population, T_, dt, 
#                        theta_1 = 0.001, theta_2 = 0.001, theta_freq = 1.0,
#                        stats_functions = c("min", "max", "mean"), 
#                        binarize = TRUE, python_env = NULL) {
#   
#   cat("Starting function\n")
#   
#   # Check and activate Python environment
#   if (!is.null(python_env)) {
#     if (reticulate::virtualenv_exists(python_env)) {
#       cat("Using specified Python environment:", python_env, "\n")
#       reticulate::use_virtualenv(python_env, required = TRUE)
#     } else {
#       stop("Specified Python environment does not exist: ", python_env)
#     }
#   } else {
#     cat("No Python environment specified. Setting up a new one...\n")
#     python_env <- "fiddle_env"
#     if (!reticulate::virtualenv_exists(python_env)) {
#       setup_python_env(envname = python_env) # Your setup function should create the environment
#     }
#     reticulate::use_virtualenv(python_env, required = TRUE)
#     cat("New Python environment created and activated:", python_env, "\n")
#   }
#   
#   # Locate the FIDDLE module in the package
#   fiddle_dir <- system.file("python/FIDDLE", package = utils::packageName())
#   if (fiddle_dir == "") {
#     stop("Could not find the 'FIDDLE' Python module in the package.")
#   }
#   
#   # Add the FIDDLE directory to Python's sys.path
#   reticulate::py_run_string(sprintf("import sys; sys.path.insert(0, '%s')", fiddle_dir))
#   fiddle <- reticulate::import("run", convert = FALSE) # Import the module
#   
#   # Convert R objects to Python objects
#   py_data <- reticulate::r_to_py(data)
#   py_population <- reticulate::r_to_py(population)
#   
#   # Call the FIDDLE.run function
#   cat("Running FIDDLE module...\n")
#   result <- fiddle$run(
#     data = py_data,
#     population = py_population,
#     T_ = T_,
#     dt = dt,
#     theta_1 = theta_1,
#     theta_2 = theta_2,
#     theta_freq = theta_freq,
#     stats_functions = stats_functions,
#     binarize = binarize
#   )
#   
#   # Convert Python result back to R object
#   cat("FIDDLE run completed.\n")
#   return(reticulate::py_to_r(result))
# }
# 
