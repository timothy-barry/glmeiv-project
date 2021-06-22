sim_dir <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "private/simulations")
library(tidyverse)
library(simulatr)

# Study 1.
sim_spec <- readRDS(paste0(sim_dir, "/sim_spec_1.rds"))
sim_res <- readRDS(paste0(sim_dir, "/raw_result_1.rds")) %>% filter(parameter %in% c("pi", "m_perturbation", "g_perturbation"))
summarized_results <- summarize_results(simulatr_specifier = sim_spec, raw_result_df = sim_res, metrics = c("coverage", "bias"))

a1 <- filter(summarized_results, arm_m_perturbation, parameter == "g_perturbation", metric == "bias")
ggplot(a1, aes(x = g_perturbation, y = value, col = method)) + geom_point() + geom_line() + ylab("Bias") + geom_errorbar(aes(ymin = lower_mc_ci, ymax = upper_mc_ci))
