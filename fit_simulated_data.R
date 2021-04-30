# obtain all args
args <- commandArgs(trailingOnly = TRUE)
simulation_dir <- if (is.na(args[1])) "~/Box/glm-eiv/simulation_dir" else args[1]
scripts_dir <- if (is.na(args[2])) "~/Box/glm-eiv/glmeiv_scripts" else args[2]
run_id <- if (is.na(args[3])) 1 else as.integer(args[3])

# Load required packages
library(magrittr)
library(glmeiv)

# source the param file
source(paste0(scripts_dir, "/param_file.R"))
g <- readRDS(paste0(data_results_logs[["data"]], "/parameter_df.rds"))

# Load data, ground truth, and covariate matrix for current run_id
curr_row <- g[run_id,]
sink(file = paste0(data_results_logs[["logs"]], "/log_", curr_row$run_id, ".Rout"))
data <- readRDS(file = paste0(data_results_logs[["data"]], "/data_", curr_row$run_id, ".rds"))

# load the covariate matrix
curr_n <- if (n_fixed) fixed_params[["n"]] else curr_row[["n"]]
curr_covariate_matrix <- if (no_covariates) NULL else readRDS(paste0(data_results_logs[["data"]], "/covariate_matrix_", curr_n, ".rds"))

g_coefs <- get_param_vector(fixed_params, curr_row, g_coef_names)
m_coefs <- get_param_vector(fixed_params, curr_row, g_coef_names)

# run the em length(data) times
result_list <- lapply(X = seq(1, length(data)), FUN = function(i) {
  print(paste0("Running rep ", i, " of ", length(data)))
  curr_data <- data[[i]]
  
  # first, obtain the optimal threshold (just ignore covariates -- thresholding method cannot really handle those)
  p_hat <- threshold_counts_no_covariates(g_intercept = g_coefs[["g_intercept"]],
                                          g_pert = g_coefs[["g_perturbation"]],
                                          g = curr_data$g,
                                          g_fam = g_fam,
                                          pi = if (pi_fixed) fixed_params[["pi"]] else curr_row[["pi"]])
  em_fit <- run_glmeiv_known_p(m = curr_data$m, g = curr_data$g,
                     m_fam = m_fam, g_fam = g_fam,
                     covariate_matrix = curr_covariate_matrix, p = p_hat, n_runs = 5,
                     max_it = 100, alpha = ci_coverage) %>% dplyr::mutate(method = "em")
  thresh_fit <- run_thresholding_method(m = curr_data$m,
                                        m_fam = m_fam,
                                        covariate_matrix = curr_covariate_matrix,
                                        p_hat = p_hat,
                                        alpha = ci_coverage) %>% dplyr::mutate(method = "threshold")
  out <- rbind(em_fit, thresh_fit) %>% dplyr::mutate(rep_id = paste0(curr_row$run_id, "-", i),
                                                     param_id = curr_row$param_id)
  
  return(out)
})

# combine results into a data frame and save in results dir
result_df <- do.call(what = rbind, args = result_list)
saveRDS(result_df, paste0(data_results_logs[["results"]], "/result_", curr_row$run_id, ".rds"))

# close the sink
closeAllConnections()
