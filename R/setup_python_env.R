#' Set Up Python Environment for Tests
#'
#' This function sets up a Python virtual environment, installs required
#' dependencies listed in the `requirements.txt` file, and ensures the
#' environment is ready for testing.
#'
#' @param envname Name of the virtual environment to create/use.
#' @param requirements_path Path to the `requirements.txt` file in python package.
#' @param fiddle_dir Path to the FIDDLE module directory.
#' @importFrom reticulate virtualenv_exists virtualenv_create use_virtualenv py_install
#' @examples
#' # Set up Python environment for testing
#' setup_python_env(envname = "test_env")
#' @export
setup_python_env <- function(envname = "test_env",
                             requirements_path = system.file("python/requirements.txt",
                                                             package = utils::packageName()),
                             fiddle_dir = system.file("python/FIDDLE", package = utils::packageName())) {
  # Check if requirements file exists
  if (!file.exists(requirements_path)) {
    stop("requirements.txt not found at: ", requirements_path)
  }
  
  # Create or activate the virtual environment
  if (!virtualenv_exists(envname)) {
    message("Creating virtual environment: ", envname)
    virtualenv_create(envname)
  }
  use_virtualenv(envname, required = TRUE)
  
  # Install dependencies
  message("Installing dependencies from: ", requirements_path)
  py_install(packages = NULL, envname = envname, pip = TRUE, requirements = requirements_path)
  
  
  # Add FIDDLE directory to PYTHONPATH
  if (!dir.exists(fiddle_dir)) {
    stop("Could not find the 'FIDDLE' Python module directory at: ", fiddle_dir)
  }
  
  # Update PYTHONPATH
  current_pythonpath <- Sys.getenv("PYTHONPATH", unset = "")
  fiddle_dir <- normalizePath(fiddle_dir)
  if (current_pythonpath == "") {
    Sys.setenv(PYTHONPATH = fiddle_dir)
  } else {
    Sys.setenv(PYTHONPATH = paste(current_pythonpath, fiddle_dir, sep = .Platform$path.sep))
  }
  message("FIDDLE module added to PYTHONPATH: ", fiddle_dir)
}
