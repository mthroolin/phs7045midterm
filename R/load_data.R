#' Load a Sparse Matrix from an NPZ File
#'
#' This function loads a sparse matrix from a `.npz` file using Python's `numpy` and `scipy.sparse` modules.
#'
#' @param npz_path A string specifying the path to the `.npz` file.
#' @param python_env A string specifying the name of the Python virtual environment to use. Defaults to `"test_env"`.
#'
#' @return A matrix or tensor loaded from the `.npz` file.
#' @examples
#' \dontrun{
#'   s_matrix <- load_npz("path/to/file.npz", python_env = "test_env")
#' }
#' @import reticulate
#' @export
load_npz = function(npz_path,
                    python_env = "test_env") {
  
  # Activate the virtual environment
  reticulate::use_virtualenv(python_env, required = TRUE)
  
  # Resolve Python executable path
  python_path <- reticulate::virtualenv_python(python_env)
  
  sparse = reticulate::import("sparse")
  
  # Load the .npz file
  npz_file <- sparse$load_npz(npz_path)$todense()
  
  # Return the output
  return(npz_file)
}

#' Load Data from an Output Directory
#'
#' This function loads data from a specified output directory, including a sparse matrix and a tensor,
#' along with their respective feature names.
#'
#' @param output_dir A string specifying the path to the output directory.
#' @param python_env A string specifying the name of the Python virtual environment to use. Defaults to `"test_env"`.
#'
#' @return A list containing:
#' \item{s_matrix}{A sparse matrix loaded from `"S.npz"`.}
#' \item{x_tensor}{A tensor loaded from `"X.npz"` with named dimensions.}
#' @examples
#' \dontrun{
#'   data <- load_data("path/to/output_dir", python_env = "test_env")
#'   s_matrix <- data$s_matrix
#'   x_tensor <- data$x_tensor
#' }
#' @import rjson
#' @export
load_data = function(output_dir,
                     python_env = "test_env") {
  # Activate the virtual environment
  reticulate::use_virtualenv(python_env, required = TRUE)
  
  # Resolve Python executable path
  python_path <- reticulate::virtualenv_python(python_env)
  
  s_matrix = load_npz(file.path(output_dir, "S.npz"))
  x_tensor = load_npz(file.path(output_dir, "X.npz"))
  
  colnames(s_matrix) = rjson::fromJSON(file = file.path(output_dir, "S.feature_names.json"))
  dimnames(x_tensor) <- list(NULL, NULL, rjson::fromJSON(file = file.path(output_dir, "X.feature_names.json")))
  
  
  
  # Return the output
  return(list(s_matrix = s_matrix, x_tensor = x_tensor))
}
