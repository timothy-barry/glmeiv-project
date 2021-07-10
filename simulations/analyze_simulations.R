load_all("~/research_code/glmeiv/")
load_all("~/research_code/simulatr/")
library(dplyr)
library(ggplot2)

####################
# simulation study 1
####################

sim_dir <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "private/simulations")
sim_spec <- readRDS(paste0(sim_dir, "/sim_spec_1.rds")) # simulatr specifier object
sim_res <- readRDS(paste0(sim_dir, "/raw_result_1.rds")) # raw results

# Obtain the (theoretical) thresholds and mixture distribution plotting dfs
density_dfs_and_thresholds <- get_theoretical_densities_and_thresholds(sim_spec = sim_spec, xgrid = seq(0, 10))

# update the parameter grid with g_thresh
row.names(sim_spec@parameter_grid) <- NULL
sim_spec@parameter_grid$grid_row_idx <- seq(1, nrow(sim_spec@parameter_grid))
sim_spec@parameter_grid$g_thresh <- density_dfs_and_thresholds$g_threshold

# obtain the "valid" IDs function.
obtain_valid_ids <- function(sim_res, spread_thresh = 0.1, approx_1_thresh = 75, approx_0_thresh = 75, g_pert_thresh = -0.5, m_pert_thresh = 0.5, pi_thresh = 0.3) {
  valid_em_ids <- sim_res %>% filter(method == "em") %>%
    group_by(id) %>%
    summarize(valid_idx = (# Filter out solutions where problem is inherently hard (i.e., effective sample size too small for good solution) 
                            value[target == "converged"] == 1
                           & value[target == "membership_probability_spread"] > spread_thresh 
                           & value[target == "n_approx_0"] >= approx_0_thresh 
                           & value[target == "n_approx_1"] >= approx_1_thresh
                           # below: filter out local optima using knowledge of distribution
                           & value[parameter == "g_perturbation" & target == "estimate"] >= g_pert_thresh
                           & value[parameter == "m_perturbation" & target == "estimate"] <= m_pert_thresh
                           & value[parameter == "m_perturbation" & target == "estimate"] <= pi_thresh),
              ) %>%
    filter(valid_idx) %>% pull(id) %>% as.character()
  valid_thresh_ids <- sim_res %>%
    filter(method == "thresholding") %>%
    group_by(id) %>%
    summarize(valid_idx = (value[target == "fit_attempted"] == 1)) %>%
    filter(valid_idx) %>% pull(id) %>% as.character()
  valid_ids <- c(valid_em_ids, valid_thresh_ids)
}

# filter the results according to the valid IDs.
valid_ids <- obtain_valid_ids(sim_res, pi_thresh = 0.5)
sim_res_sub <- filter(sim_res, id %in% valid_ids)

# compute summary statistics
summarized_results <- summarize_results(sim_spec = sim_spec, sim_res = sim_res_sub,
                                        metrics = c("coverage", "bias", "mse", "count", "se"),
                                        parameters = c("m_perturbation", "pi")) %>% filter(pi < 0.5)
# make plots (relocate to glm-eiv package)
plot_all_arms(summarized_results, "m_perturbation", "bias", plot_discont_points = TRUE)
plot_all_arms(summarized_results, "m_perturbation", "mse", plot_discont_points = TRUE)
plot_all_arms(summarized_results, "m_perturbation", "se", plot_discont_points = TRUE)
plot_all_arms(summarized_results, "m_perturbation", "coverage")


# examine gRNA and mRNA distributions for arm c (g_perturbation varying)
hard_idx <- filter(sim_spec@parameter_grid, g_perturbation == 0.5, arm_g_perturbation) %>% pull(grid_row_idx)
easy_idx <- filter(sim_spec@parameter_grid, g_perturbation == 3, arm_g_perturbation) %>% pull(grid_row_idx)
# gRNA count distribution challenging extreme:
plot_mixture(density_dfs_and_thresholds$g_dfs[[hard_idx]],
             ceiling(density_dfs_and_thresholds$g_threshold[hard_idx]), x_max = 6)
# gRNA count distribution easier extreme:
plot_mixture(density_dfs_and_thresholds$g_dfs[[easy_idx]],
             ceiling(density_dfs_and_thresholds$g_threshold[easy_idx]), x_max = 6)
# mRNA count distribution (fixed)
plot_mixture(density_dfs_and_thresholds$m_dfs[[hard_idx]], x_max = 10)

####################
# simulation study 2
####################
sim_spec <- readRDS(paste0(sim_dir, "/sim_spec_2.rds")) # simulatr specifier object
sim_res <- readRDS(paste0(sim_dir, "/raw_result_2.rds")) # raw results

# obtain the (theoretical) thresholds and mixture distribution plotting dfs
density_dfs_and_thresholds <- get_theoretical_densities_and_thresholds(sim_spec = sim_spec, xgrid = seq(0, 50))

# update parameter grid
row.names(sim_spec@parameter_grid) <- NULL
sim_spec@parameter_grid$grid_row_id <- seq(1, nrow(sim_spec@parameter_grid))
sim_spec@parameter_grid$g_thresh <- density_dfs_and_thresholds$g_threshold

# get the valid IDs and filter
valid_ids <- obtain_valid_ids(sim_res, spread_thresh = 0.1, approx_1_thresh = 75, approx_0_thresh = 75)
sim_res_sub <- filter(sim_res, id %in% valid_ids)

# compute summary stats
summarized_results <- summarize_results(sim_spec = sim_spec, sim_res = sim_res_sub,
                                        metrics = c("bias", "coverage", "count", "mse", "se"),
                                        parameters = c("m_perturbation")) %>% as_tibble()
# plot arms
plot_all_arms(summarized_results, "m_perturbation", "count", plot_discont_points = FALSE, ylim = c(0, 1000))
plot_all_arms(summarized_results, "m_perturbation", "bias", plot_discont_points = TRUE, ylim = c(-0.01, 0.075))
plot_all_arms(summarized_results, "m_perturbation", "mse", plot_discont_points = TRUE)
plot_all_arms(summarized_results, "m_perturbation", "se", plot_discont_points = FALSE)
plot_all_arms(summarized_results, "m_perturbation", "coverage", plot_discont_points = TRUE, ylim = c(0,1))

# examinine mixture distributions
hard_idx_g <- filter(sim_spec@parameter_grid, arm_g_perturbation, g_perturbation == 0.2) %>% pull(grid_row_id)
easy_idx_g <- filter(sim_spec@parameter_grid, g_perturbation == 3, arm_g_perturbation) %>% pull(grid_row_id)
hard_idx_m <- filter(sim_spec@parameter_grid, arm_m_perturbation, m_perturbation == -0.2) %>% pull(grid_row_id)
easy_idx_m <- filter(sim_spec@parameter_grid, arm_m_perturbation, m_perturbation == -2) %>% pull(grid_row_id)

# gRNA count distribution challenging extreme:
plot_mixture(density_dfs_and_thresholds$g_dfs[[hard_idx_g]],
             ceiling(density_dfs_and_thresholds$g_threshold[hard_idx_g]),
             x_max = 6)
# gRNA count distribution easy extreme
plot_mixture(density_dfs_and_thresholds$g_dfs[[easy_idx_g]],
             ceiling(density_dfs_and_thresholds$g_threshold[easy_idx_g]),
             x_max = 13)
# mRNA count distribution challenging extreme
plot_mixture(density_dfs_and_thresholds$m_dfs[[hard_idx_m]],
             x_max = 30,points = FALSE)
# mRNA count distribution easy extreme
plot_mixture(density_dfs_and_thresholds$m_dfs[[easy_idx_m]],
             x_max = 30, points = FALSE)

####################
# simulation study 3
####################
