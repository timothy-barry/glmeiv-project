sim_dir <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "private/simulations")
library(tidyverse)
library(simulatr)

# Study 1.
sim_spec <- readRDS(paste0(sim_dir, "/sim_spec_1.rds"))
sim_res <- readRDS(paste0(sim_dir, "/raw_result_1.rds"))
sim_res %>% filter(parameter %in% c("pi", "m_perturbation"))
summarized_results <- summarize_results(simulatr_specifier = sim_spec, raw_result_df = sim_res, metrics = c("coverage", "bias"))
