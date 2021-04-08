# obtain all args
args <- commandArgs(trailingOnly = TRUE)
simulation_dir <- if (is.na(args[1])) "~/Box/glm-eiv/simulation_dir" else args[1]
run_id <- if (is.na(args[2])) 1 else as.integer(args[2])

# Load required packages
library(magrittr)
library(glmeiv)

# set file paths and load parameter grid; sik results
drl <- c("data", "results", "logs")
data_results_logs <- setNames(paste0(simulation_dir, "/", drl), drl)
g <- readRDS(paste0(data_results_logs[["data"]], "/parameter_df.rds"))

# Load data, ground truth, and covariate matrix for current run_id
curr_row <- g[run_id,]
sink(file = paste0(data_results_logs[["logs"]], "/log_", curr_row$run_id, ".Rout"))
data <- readRDS(file = paste0(data_results_logs[["data"]], "/data_", curr_row$run_id, ".rds"))
ground_truth <- readRDS(file = paste0(data_results_logs[["data"]], "/ground_truth_", curr_row$param_id, ".rds"))
covariate_matrix <- readRDS(file = paste0(data_results_logs[["data"]], "/covariate_matrix_", curr_row$dataset_id, ".rds"))

# run the em length(data) times
result_list <- lapply(X = seq(1, length(data)), FUN = function(i) {
  print(paste0("Running rep ", i, " of ", length(data)))
  curr_data <- data[[i]]
  em_fit <- run_glmeiv_known_p(m = curr_data$m, g = curr_data$g,
                     m_fam = ground_truth$m_fam, g_fam = ground_truth$g_fam,
                     covariate_matrix = covariate_matrix, p = curr_data$p, n_runs = 5,
                     alpha = 0.9) %>% dplyr::mutate(rep_id = paste0(curr_row$run_id, "-", i),
                                                    param_id = curr_row$param_id)
  return(em_fit)
})

# combine results into a data frame and save in results dir
result_df <- do.call(what = rbind, args = result_list)
saveRDS(result_df, paste0(data_results_logs[["results"]], "/result_", curr_row$run_id, ".rds"))

# close the sink
closeAllConnections()
