# obtain all args
args <- commandArgs(trailingOnly = TRUE)
simulation_dir <- if (is.na(args[1])) "~/Box/glm-eiv/simulation_dir" else args[1]
scripts_dir <- if (is.na(args[2])) "~/Box/glm-eiv/glmeiv_scripts" else args[2]
small_example <- if (is.na(args[3])) TRUE else as.logical(args[3])

# source the param file from the scripts_dir
source(paste0(scripts_dir, "/param_file.R"))

# create simulation dir, if it does not exist
if (!dir.exists(simulation_dir)) dir.create(simulation_dir, recursive = TRUE)
for (dir in data_results_logs) {
  # create (if necessary)
  if (!dir.exists(dir)) dir.create(dir)
  # delete contents
  file.remove(list.files(dir, full.names = TRUE))
}

# load packages
library(stats)
library(dplyr)
library(glmeiv)
library(future)
library(furrr)

# define the grid "g" data frame
param_id <- rep(seq(1, nrow(varying_params)), each = n_outer_reps)
partition_id <- rep(seq(1, n_outer_reps), times = nrow(varying_params))

g <- varying_params[param_id, ] %>%
  mutate(param_id = param_id,
         partition_id = partition_id,
         run_id = paste0(param_id, "-", partition_id))
row.names(g) <- NULL
saveRDS(object = g, file = paste0(data_results_logs[["data"]], "/parameter_df.rds"))
if (small_example) g <- slice(g, seq(1, 20))
cat(nrow(g))

# set seed for generating covariate matrices
set.seed(4)

# create covariate matrices
if (!no_covariates) { # if covariates are present
  if (n_fixed) {
    # generate a single data frame
    covariate_matrix <- lapply(X = covariate_sampler,
                               FUN = function(f) f(fixed_params[["n"]])) %>% as.data.frame
    saveRDS(covariate_matrix, file = paste0(data_results_logs[["data"]], "/covariate_matrix_", fixed_params[["n"]], ".rds"))
  } else {
    # generate a data frame for each level of n
    n_levels <- unique(g$n)
    covariate_matrices <- lapply(n_levels, function(curr_n) {
      curr_m <- lapply(X = covariate_sampler,
             FUN = function(f) f(curr_n)) %>% as.data.frame
      saveRDS(object = curr_m, file = paste0(data_results_logs[["data"]], "/covariate_matrix_", curr_n, ".rds"))
      return(curr_m)
    })
  }
}


# if small example, use only 5 inner reps; generate data in parallel
if (small_example) n_inner_reps <- 5
plan(multicore(workers = 40))
suppressWarnings(future_map(.x = unique(g$param_id), .f = function(i) {
  # set seed within each param setting
  set.seed(16)
  curr_param_df <- filter(g, param_id == i)
  varying_param_row <- curr_param_df[1,]
  
  # first, the regression coefficients
  curr_m_coef <- get_param_vector(fixed_params, varying_param_row, m_coef_names)
  curr_g_coef <- get_param_vector(fixed_params, varying_param_row, g_coef_names)
  
  # next, n and pi
  curr_n <- if (n_fixed) fixed_params[["n"]] else varying_param_row[["n"]]
  curr_pi <- if (pi_fixed) fixed_params[["pi"]] else varying_param_row[["pi"]]

  # Finally, the covariate matrix
  if (no_covariates) {
    curr_covariate_matrix <- NULL
  } else {
    if (n_fixed) {
      curr_covariate_matrix <- covariate_matrix
    } else {
      curr_covariate_matrix <- covariate_matrices[[which(curr_n == n_levels)]]
    }
  }
  
  # Generate the data
  data <- replicate(n_inner_reps * n_outer_reps, generate_data_from_model(m_fam = m_fam, g_fam = g_fam,
                                                     m_coef = curr_m_coef, g_coef = curr_g_coef,
                                                     pi = curr_pi, covariate_matrix = curr_covariate_matrix, n = curr_n), simplify = FALSE)
   v <- seq(0, n_inner_reps * n_outer_reps, n_inner_reps)
   for (j in seq(1, n_outer_reps)) {
    run_id <- curr_param_df$run_id[j]
    saveRDS(data[seq(v[j] + 1, v[j + 1])], file = paste0(data_results_logs[["data"]], "/data_", run_id, ".rds"))
   }
  ground_truth <- list(m_coef = curr_m_coef, g_coef = curr_g_coef, pi = curr_pi, m_fam = m_fam, g_fam = g_fam, param_id = i, n = curr_n)
  saveRDS(ground_truth, file = paste0(data_results_logs[["data"]], "/ground_truth_", i, ".rds"))
})) %>% invisible()
