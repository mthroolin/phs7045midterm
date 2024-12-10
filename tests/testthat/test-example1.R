setup_python_env()

dat <- system.file("extdata", "test1", "data.csv", package = "fiddleR")
pop <- system.file("extdata", "test1",  "pop.csv", package = "fiddleR")
config_path <- system.file("extdata", "test1", "config-1-parallel.yaml", package = "fiddleR")
output_dir <- tempdir()

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
} 
run_fiddle(
  data = dat, 
  population = pop, 
  T_ = 4, 
  dt = 1, 
  output_dir = output_dir,
  config_file = config_path,
  theta_1 = 0.001, 
  theta_2 = 0.001, 
  theta_freq = 1
)
 
