# coefficients: m_intercept 0, 1, 2, 3
# m_slope: -2
# g_intercept: -2
# g_slope: 1, 2, 3, 4

# obtain all args
args <- commandArgs(trailingOnly = TRUE)
simulation_dir <- if (is.na(args[1])) "~/Box/glm-eiv/simulation_dir" else args[1]
small_example <- if (is.na(args[2])) TRUE else as.logical(args[2])

# create simulation dir, if it does not exist
if (!dir.exists(simulation_dir)) dir.create(simulation_dir, recursive = TRUE)
drl <- c("data", "results", "logs")
data_results_logs <- setNames(paste0(simulation_dir, "/", drl), drl)
for (dir in data_results_logs) {
  # create (if necessary)
  if (!dir.exists(dir)) dir.create(dir)
  # delete contents
  file.remove(list.files(dir, full.names = TRUE))
}

# load package
library(stats)
library(dplyr)
library(glmeiv)
library(future)
library(furrr)

# define parameter data frame
n_partitions <- 10
data_sizes <- c(1000, 10000, 100000)
n_reps_per_row <- if (small_example) 5 else 100
g <- expand.grid(partition_id = seq(1, 10),
                 beta_m0 = seq(0, 3),
                 n = data_sizes,
                 beta_g1 = seq(1, 4)) %>%
  mutate(param_id = rep(1:(length(partition_id)/n_partitions), each = n_partitions),
                run_id = paste0(param_id, "-", partition_id),
                dataset_id = factor(n, data_sizes, 1:length(data_sizes))) %>%
  select(param_id, partition_id, run_id, dataset_id, everything())
saveRDS(object = g, file = paste0(data_results_logs[["data"]], "/parameter_df.rds"))
if (small_example) g <- slice(g, seq(1, 20))
cat(nrow(g))

# set seed for generating covariate matrices
set.seed(4)

# for each value of n, create a covariate_matrix, in this case NULL
covariate_matrix_table <- g %>% group_by(n) %>% summarize(n = unique(n), dataset_id = unique(dataset_id))
for (i in seq(1, nrow(covariate_matrix_table))) {
  saveRDS(object = NULL, file = paste0(data_results_logs[["data"]], "/covariate_matrix_", i, ".rds"))
}

# next, for each for row of g, generate synthetic data
m_fam <- poisson()
g_fam <- poisson()
pi <- 0.25
m_slope <- -2
g_intercept <- -2

# generate the data in parallel
plan(multicore)
suppressWarnings(future_map(.x = unique(g$param_id), .f = function(i) {
  # set seed within each param setting
  set.seed(16)
  curr_param_subset <- filter(g, param_id == i)
  curr_row <- curr_param_subset[1,]
  m_coef <- c(curr_row$beta_m0, m_slope)
  g_coef <- c(g_intercept, curr_row$beta_g1)
  n <- curr_row$n
  covariate_matrix <- NULL
  data <- replicate(n_reps_per_row * n_partitions, generate_data_from_model(m_fam = m_fam, g_fam = g_fam,
                                                     m_coef = m_coef, g_coef = g_coef,
                                                     pi = pi, covariate_matrix = covariate_matrix, n = n), simplify = FALSE)
  
  v <- seq(0, n_reps_per_row * n_partitions, n_reps_per_row)
  for (j in seq(1, n_partitions)) {
   run_id <- curr_param_subset$run_id[j]
   saveRDS(data[seq(v[j] + 1, v[j + 1])], file = paste0(data_results_logs[["data"]], "/data_", run_id, ".rds"))
  }
  ground_truth <- list(m_coef = m_coef, g_coef = g_coef, pi = pi, m_fam = m_fam, g_fam = g_fam, param_id = i, n = curr_row$n)
  saveRDS(ground_truth, file = paste0(data_results_logs[["data"]], "/ground_truth_", curr_row$param_id, ".rds"))
})) %>% invisible()
