library(simulatr)
require(dplyr)

sim_dir <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "private/simulations")
sim_spec <- readRDS(paste0(sim_dir, "/sim_spec_1.rds")) # simulatr specifier object
sim_res <- readRDS(paste0(sim_dir, "/raw_result_1.rds")) # raw results

# obtain the "valid" IDs.
valid_em_ids <- sim_res %>% filter(method == "em") %>%
  group_by(id) %>%
  summarize(valid_idx = (value[target == "converged"] == 1 & value[target == "membership_probability_spread"] > 0.15 & value[target == "n_approx_0"] >= 40 & value[target == "n_approx_1"] >= 40)) %>%
  filter(valid_idx) %>% pull(id) %>% as.character()
valid_thresh_ids <- sim_res %>%
  filter(method == "thresholding") %>%
  group_by(id) %>%
  summarize(valid_idx = (value[target == "fit_attempted"] == 1)) %>%
  filter(valid_idx) %>% pull(id) %>% as.character()
valid_ids <- c(valid_em_ids, valid_thresh_ids)

# filter the results according to the valid IDs.
sim_res_sub <- filter(sim_res, id %in% valid_ids)

# compute summary statistics
summarized_results <- summarize_results(sim_spec = sim_spec, sim_res = sim_res_sub,
                                        metrics = c("coverage", "bias", "count", "rejection_probability"),
                                        parameters = c("m_perturbation", "pi")) %>% filter(pi < 0.5)

# make plots
plot_all_arms(summarized_results, "m_perturbation", "bias")
plot_all_arms(summarized_results, "m_perturbation", "coverage")
plot_all_arms(summarized_results, "m_perturbation", "count", c(0, 1000))
plot_all_arms(summarized_results, "m_perturbation", "rejection_probability")
