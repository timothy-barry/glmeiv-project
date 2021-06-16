# obtain all args
args <- commandArgs(trailingOnly = TRUE)
simulation_dir <- if (is.na(args[1])) "~/Box/glm-eiv/simulation_dir" else args[1]
scripts_dir <- if (is.na(args[2])) "~/Box/glm-eiv/glmeiv_scripts/" else args[2]

library(dplyr)
library(tidyr)

# load the set of parameters
source(paste0(scripts_dir, "/param_file.R"))
g <- readRDS(paste0(data_results_logs[["data"]], "/parameter_df.rds")) %>% select(-partition_id, -run_id) %>% distinct()
g_full <- g
for (fixed_param in names(fixed_params)) g_full[[fixed_param]] <- fixed_params[[fixed_param]]
gt_long <- g_full %>% pivot_longer(cols = -param_id, names_to = "variable", values_to = "gt_value")

# load the results
res_dir <- paste0(simulation_dir, "/results")
fs <- list.files(res_dir, full.names = TRUE)[grep(pattern = "^result_*", x = list.files(res_dir))]
all_res <- lapply(fs, readRDS) %>% do.call(what = rbind, args = .)
all_res$variable[all_res$variable == "m_(Intercept)"] <- "m_intercept"
all_res$variable[all_res$variable == "g_(Intercept)"] <- "g_intercept"

# for the varying parameters, compute the summary statistics
x <- all_res %>% left_join(x = ., y = gt_long, by = c("param_id", "variable")) %>%
  mutate(covered = (gt_value >= confint_lower & gt_value <= confint_higher)) %>%
  group_by(variable, param_id, method) %>%
  summarize(
    n_success_ci = sum(!is.na(confint_lower)),
    coverage = mean(covered, na.rm = TRUE),
    mean_estimate = mean(estimate, na.rm = TRUE),
    mean_lower_ci = mean(confint_lower, na.rm = TRUE),
    mean_upper_ci = mean(confint_higher, na.rm = TRUE),
    coverage_lower_ci = coverage - 1.96 * sqrt(coverage * (1-coverage)/n_success_ci),
    coverage_upper_ci = coverage + 1.96 * sqrt(coverage * (1-coverage)/n_success_ci),
    gt_value = gt_value[1]) %>% ungroup()

# Finally, append all varying params
x_final <- x %>% left_join(x = ., y = g, by = "param_id")
saveRDS(object = x_final, file = paste0(data_results_logs[["results"]], "/combined_results.rds"))
