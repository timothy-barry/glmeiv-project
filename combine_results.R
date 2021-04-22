# obtain all args
args <- commandArgs(trailingOnly = TRUE)
simulation_dir <- if (is.na(args[1])) "~/Box/glm-eiv/simulation_dir" else args[1]
small_example <- if (is.na(args[2])) TRUE else as.logical(args[2])

library(dplyr)
library(tidyr)

# load the results
res_dir <- paste0(simulation_dir, "/results")
fs <- list.files(res_dir, full.names = TRUE)[ grep(pattern = "^result_*", x = list.files(res_dir))]
all_res <- lapply(fs, readRDS) %>% do.call(what = rbind, args = .)
var_names <- filter(all_res, rep_id == "1-1-1") %>% pull(variable) %>% unique()

# load the ground truth
data_dir <- paste0(simulation_dir, "/data")
fs_load <- list.files(data_dir, full.names = TRUE)[grep(pattern = "ground_truth*", x = list.files(data_dir))]
all_gt <- lapply(fs_load, readRDS) %>% lapply(., function(gt) {
  m <- t(matrix(c(gt$param_id, gt$pi, gt$m_coef, gt$g_coef, gt$n)))
  row.names(m) <- NULL
  colnames(m) <- c("param_id", var_names, "n") 
  return(m)
}) %>% do.call(what = rbind, args = .) %>% as.data.frame()

# a function to compute the coverage across all param_ids of a given variable
compute_statistics_for_variable <- function(variable, all_res, all_gt) {
  gt_var <- select(all_gt, param_id, !!variable)
  res_var <- filter(all_res, variable == !!variable) %>% select(estimate, confint_lower, confint_higher, rep_id, param_id, method)
  res_var$gt_value <- pull(gt_var, !!variable)[match(x = res_var$param_id, table = gt_var$param_id)]
  out <- res_var %>% mutate(covered = (gt_value >= confint_lower & gt_value <= confint_higher)) %>%
    group_by(param_id, method) %>% summarize(coverage = mean(covered),
                                     mean_estimate = mean(estimate),
                                     mean_lower_ci = mean(confint_lower),
                                     mean_upper_ci = mean(confint_higher),
                                     variable = variable,
                                     gt_value = gt_value[1],
                                     coverage_lower_ci = coverage - 1.96 * sqrt(coverage * (1-coverage)/n()),
                                     coverage_upper_ci = coverage + 1.96 * sqrt(coverage * (1-coverage)/n())) %>% 
    ungroup() %>% select(variable, param_id, method, gt_value, everything())
  return(out)
}

# finally, compute the statistics for each variable
varwise_stats <- lapply(var_names, function(variable)
  compute_statistics_for_variable(variable, all_res, all_gt)) %>%
  do.call(what = rbind, args = .)

varwise_stats_final <- left_join(x = varwise_stats, y = all_gt, by = "param_id")

# save result
saveRDS(object = varwise_stats_final, file = paste0(res_dir, "/combined_results.rds"))
