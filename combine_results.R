# obtain all args
args <- commandArgs(trailingOnly = TRUE)
simulation_dir <- if (is.na(args[1])) "~/Box/glm-eiv/simulation_dir" else args[1]

library(dplyr)

# load the results
res_dir <- paste0(simulation_dir, "/results")
fs <- list.files(res_dir, full.names = TRUE)
all_res <- lapply(fs, readRDS) %>% do.call(what = rbind, args = .)
var_names <- filter(all_res, rep_id == "1-1-1") %>% pull(variable)

# load the ground truth
data_dir <- paste0(simulation_dir, "/data")
fs_load <- list.files(data_dir, full.names = TRUE)[grep(pattern = "ground_truth*", x = list.files(data_dir))]
all_gt <- lapply(fs_load, readRDS) %>% lapply(., function(gt) {
  data.frame(variable = var_names, value = c(gt$pi, gt$m_coef, gt$g_coef), param_id  = gt$param_id)
}) %>% do.call(what = rbind, args = .)

all_res %>% filter(variable == 'm_perturbation') %>% mutate(ci_coverage =  ) %>% group_by(parm_id) %>% summarize()

# For each coefficient setting, we want to compute 95% CI coverage for every parameter.
