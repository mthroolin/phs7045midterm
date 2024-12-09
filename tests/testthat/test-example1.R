setup_python_env()
#dat = generate_data(num_records = 500, num_ids = 20, num_vars = 20)

# dat = "C:/Users/mthro/Desktop/FIDDLE/tests/imputation_test/input/data.csv"
# pop = "C:/Users/mthro/Desktop/FIDDLE/tests/imputation_test/input/pop.csv"
# config_path = "C:/Users/mthro/Desktop/FIDDLE/tests/imputation_test/input/config-1-parallel.yaml"
# 
# 
# output_dir <- "C:/Users/mthro/Desktop/output/"
# 
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }
# 
# # run_fiddle(
# #   data = read.csv(dat), population = read.csv(pop), T_ = 4, dt = 1, output_dir = "C:/Users/mthro/Desktop/output/",
# #   config_path = "C:/Users/mthro/Desktop/FIDDLE/tests/imputation_test/input/config-1-parallel.yaml" ,
# #   theta_1 =.001, theta_2 = .001, theta_freq = 1)
# 
# run_fiddle(
#   data = dat, population = pop, T_ = 4, dt = 1, output_dir = "C:/Users/mthro/Desktop/output/",
#   config_file = "C:/Users/mthro/Desktop/FIDDLE/tests/imputation_test/input/config-1-parallel.yaml" ,
#   theta_1 =.001, theta_2 = .001, theta_freq = 1)


dat <- system.file("extdata", "test1", "data.csv", package = "FIDDLEPackage")
pop <- system.file("extdata", "test1",  "pop.csv", package = "FIDDLEPackage")
config_path <- system.file("extdata", "test1", "config-1-parallel.yaml", package = "FIDDLEPackage")
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
 
