load_all("~/research_code/glmeiv/")
load_all("~/research_code/simulatr/")
library(dplyr)
library(ggplot2)
sim_dir <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "private/simulations")

####################
# simulation study 1
####################
sim_spec <- readRDS(paste0(sim_dir, "/sim_spec_1.rds")) # simulatr specifier object
sim_res <- readRDS(paste0(sim_dir, "/raw_result_1.rds")) # raw results

# Obtain the (theoretical) thresholds and mixture distribution plotting dfs
density_dfs_and_thresholds <- get_theoretical_densities_and_thresholds(sim_spec = sim_spec, xgrid = seq(0, 15))

# update the parameter grid with g_thresh
row.names(sim_spec@parameter_grid) <- NULL
sim_spec@parameter_grid$grid_row_idx <- seq(1, nrow(sim_spec@parameter_grid))
sim_spec@parameter_grid$g_thresh <- density_dfs_and_thresholds$g_threshold

# filter the results according to the valid IDs.
id_classifications <- obtain_valid_ids(sim_res, pi_upper = Inf)
valid_ids <- id_classifications$valid_ids
sim_res_sub <- filter(sim_res, id %in% valid_ids)

# compute summary statistics
summarized_results <- summarize_results(sim_spec = sim_spec, sim_res = sim_res_sub,
                                        metrics = c("coverage", "bias", "mse", "count", "se"),
                                        parameters = c("m_perturbation", "pi")) %>% filter(pi < 0.5)
# make plots
plot_all_arms(summarized_results, "m_perturbation", "bias", plot_discont_points = TRUE)
plot_all_arms(summarized_results, "m_perturbation", "mse", plot_discont_points = TRUE)
plot_all_arms(summarized_results, "m_perturbation", "se", plot_discont_points = TRUE)
plot_all_arms(summarized_results, "m_perturbation", "coverage")
plot_all_arms(summarized_results, "m_perturbation", "count", ylim = c(0, 1000))

# examine gRNA and mRNA distributions for arm c (g_perturbation varying)
hard_idx <- filter(sim_spec@parameter_grid, g_perturbation == 0.5, arm_g_perturbation) %>% pull(grid_row_idx)
easy_idx <- filter(sim_spec@parameter_grid, g_perturbation == 3, arm_g_perturbation) %>% pull(grid_row_idx)
# gRNA count distribution challenging extreme:
plot_mixture(density_df = density_dfs_and_thresholds$g_dfs[[hard_idx]],
             thresh = ceiling(density_dfs_and_thresholds$g_threshold[hard_idx]),
             x_max = 4)
# gRNA count distribution easier extreme:
plot_mixture(density_dfs_and_thresholds$g_dfs[[easy_idx]],
             ceiling(density_dfs_and_thresholds$g_threshold[easy_idx]), x_max = 8)
# mRNA count distribution (fixed)
plot_mixture(density_dfs_and_thresholds$m_dfs[[hard_idx]], x_max = 15)

# plot em classifications
plot_em_classifications(em_classifications = id_classifications$em_classifications, sim_spec = sim_spec, grid_row_id = 30, parameter = "m_perturbation")

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
id_classifications <- obtain_valid_ids(sim_res = sim_res, m_pert_upper = 0.25)
valid_ids <- id_classifications$valid_ids
sim_res_sub <- filter(sim_res, id %in% valid_ids)

# compute summary stats
summarized_results <- summarize_results(sim_spec = sim_spec, sim_res = sim_res_sub,
                                        metrics = c("bias", "coverage", "count", "mse", "se"),
                                        parameters = c("m_perturbation")) %>% as_tibble()
# plot arms
plot_all_arms(summarized_results, "m_perturbation", "count", plot_discont_points = FALSE, ylim = c(0, 1000))
plot_all_arms(summarized_results, "m_perturbation", "bias", plot_discont_points = TRUE, ylim = c(-0.02, 0.07))
plot_all_arms(summarized_results, "m_perturbation", "mse", plot_discont_points = TRUE, ylim = c(0, 0.007))
plot_all_arms(summarized_results, "m_perturbation", "se", plot_discont_points = FALSE, ylim = c(0, 0.05))
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

# examine failure modes
plot_em_classifications(em_classifications = id_classifications$em_classifications, sim_spec = sim_spec, grid_row_id = 16, parameter = "m_perturbation")

####################
# simulation study 3
####################
sim_spec <- readRDS(paste0(sim_dir, "/sim_spec_3.rds")) # simulatr specifier object
sim_res <- readRDS(paste0(sim_dir, "/raw_result_3.rds")) # raw results

# obtain the (theoretical) thresholds and mixture distribution plotting dfs
density_dfs_and_thresholds <- get_theoretical_densities_and_thresholds(sim_spec = sim_spec, xgrid = seq(0, 50))

# get the valid IDs and filter
id_classifications <- obtain_valid_ids(sim_res = sim_res, spread_thresh = 0.1, approx_0_thresh = 60, approx_1_thresh = 60, g_pert_lower = -0.5, m_pert_upper = 0.5, pi_upper = 0.3)
valid_ids <- id_classifications$valid_ids
sim_res_sub <- filter(sim_res, id %in% valid_ids)

# compute summary stats
summarized_results <- summarize_results(sim_spec = sim_spec, sim_res = sim_res_sub,
                                        metrics = c("bias", "coverage", "count", "mse", "se", "rejection_probability"),
                                        parameters = c("m_perturbation"),
                                        threshold = 0.1) %>% as_tibble()

arm_info <- list(arm_names = c("arm_intermediate", "arm_easy", "arm_null"),
                 varying_param = c("m_perturbation", "m_perturbation", "g_perturbation"),
                 all_params = c("m_perturbation", "g_perturbation"))
# make plots
plot_all_arms(summarized_results, "m_perturbation", "rejection_probability", ylim = c(0, 1), plot_discont_points = FALSE, arm_info = arm_info)
plot_all_arms(summarized_results, "m_perturbation", "bias", plot_discont_points = FALSE, arm_info = arm_info)
plot_all_arms(summarized_results, "m_perturbation", "coverage", plot_discont_points = FALSE, arm_info = arm_info)
plot_all_arms(summarized_results, "m_perturbation", "count", plot_discont_points = FALSE, arm_info = arm_info, ylim = c(0, 1000))

# examine failure modes
plot_em_classifications(em_classifications = id_classifications$em_classifications, sim_spec = sim_spec, grid_row_id = 16, categories = "confident-implausible")

####################
# simulation study 4
####################
sim_spec <- readRDS(paste0(sim_dir, "/sim_spec_4.rds")) # simulatr specifier object
sim_res <- readRDS(paste0(sim_dir, "/raw_result_4.rds")) # raw results; note: PSC failed for certain settings in which pi was small and g_perturbation was large. Unclear why. Investigate.

id_classifications <- obtain_valid_ids(sim_res = sim_res, spread_thresh = 0.1, approx_0_thresh = 60, approx_1_thresh = 60, g_pert_lower = -0.5, m_pert_upper = 0.5, pi_upper = 0.3)
valid_ids <- id_classifications$valid_ids
sim_res_sub <- filter(sim_res, id %in% valid_ids)

summarized_results <- summarize_results(sim_spec = sim_spec, sim_res = sim_res_sub,
                                        metrics = c("bias", "coverage", "count", "mse", "se", "rejection_probability"),
                                        parameters = c("m_perturbation"),
                                        threshold = 0.1) %>% as_tibble()

arm_info <- list(arm_names = c("arm_arm_pi_small", "arm_arm_pi_intermediate", "arm_arm_pi_big"),
                 varying_param = c("g_perturbation", "g_perturbation", "g_perturbation"),
                 all_params = c("g_perturbation", "pi"))

# for each arm, compute the theoretical bias over a grid of g_perturbation
arm_names <- purrr::set_names(arm_info$arm_names, arm_info$arm_names)
theoretical_values <- purrr::map_dfr(.x = arm_names, function(arm_name) {
  curr_arm_df <- sim_spec@parameter_grid %>% filter(!!as.symbol(arm_name))
  pi <- curr_arm_df$pi[1]
  g_pert_range <- curr_arm_df$g_perturbation %>% range()
  g_pert_grid <- seq(g_pert_range[1], g_pert_range[2], length.out = 100)
  bias <- get_tresholding_estimator_bias(m_perturbation = sim_spec@fixed_parameters$m_perturbation,
                                         g_perturbation = g_pert_grid, pi = pi)
  data.frame(value = c(bias, rep(0, length(g_pert_grid))),
             x = c(g_pert_grid, g_pert_grid),
             method = c(rep("thresholding", length(g_pert_grid)), rep("em", length(g_pert_grid))))
}, .id = "arm")

# make plots
plot_all_arms(summarized_results, "m_perturbation", "count", plot_discont_points = FALSE, arm_info = arm_info, ylim = c(0, 1000))
plot_all_arms(summarized_results, "m_perturbation", "bias", plot_discont_points = FALSE, arm_info = arm_info, theoretical_values = theoretical_values, ylim = c(-0.1, 3))
plot_all_arms(summarized_results, "m_perturbation", "coverage", plot_discont_points = FALSE, arm_info = arm_info)

# make some plots of mixture distributions
density_dfs_and_thresholds <- get_theoretical_densities_and_thresholds(sim_spec = sim_spec, xgrid = seq(-10, 10, 0.1))
# pi small, g_perturbation big
idx <- sim_spec@parameter_grid %>% filter(arm_arm_pi_small, g_perturbation == 6) %>% pull(grid_id)
plot_mixture(density_df = density_dfs_and_thresholds$g_dfs[[idx]], points = FALSE, x_min = -4, x_max = 9, thresh = density_dfs_and_thresholds$g_threshold[idx])
# pi small, g_perturbation small
idx <- sim_spec@parameter_grid %>% filter(arm_arm_pi_small, g_perturbation == 0.5) %>% pull(grid_id)
plot_mixture(density_df = density_dfs_and_thresholds$g_dfs[[idx]], points = FALSE, x_min = -4, x_max = 4)
# pi small, m perturbation (fixed)
plot_mixture(density_df = density_dfs_and_thresholds$m_dfs[[idx]], points = FALSE, x_min = -7, x_max = 4)

# pi big, g_perturbation big
idx <- sim_spec@parameter_grid %>% filter(arm_arm_pi_big, g_perturbation == 6) %>% pull(grid_id)
plot_mixture(density_df = density_dfs_and_thresholds$g_dfs[[idx]], points = FALSE, x_min = -4, x_max = 9, thresh = density_dfs_and_thresholds$g_threshold[idx])
# pi big, p_perturbation small
idx <- sim_spec@parameter_grid %>% filter(arm_arm_pi_big, g_perturbation == 0.5) %>% pull(grid_id)
plot_mixture(density_df = density_dfs_and_thresholds$g_dfs[[idx]], points = FALSE, x_min = -4, x_max = 4)
# pi big, m_perturbation (fixed)
plot_mixture(density_df = density_dfs_and_thresholds$m_dfs[[idx]], points = FALSE, x_min = -7, x_max = 4)

# plot EM classifications
plot_em_classifications(em_classifications = id_classifications$em_classifications, sim_spec = sim_spec, grid_row_id = 3)

####################
# simulation study 5
####################
sim_spec <- readRDS(paste0(sim_dir, "/sim_spec_5.rds")) # simulatr specifier object
sim_res <- readRDS(paste0(sim_dir, "/raw_result_5.rds"))
density_dfs_and_thresholds <- get_theoretical_densities_and_thresholds(sim_spec = sim_spec, xgrid = seq(0, 10))
sim_spec@parameter_grid$g_thresh <- density_dfs_and_thresholds$g_threshold

id_classifications <- obtain_valid_ids(sim_res = sim_res)
valid_ids <- id_classifications$valid_ids
sim_res_sub <- filter(sim_res, id %in% valid_ids)

summarized_results <- summarize_results(sim_spec = sim_spec, sim_res = sim_res_sub,
                                        metrics = c("bias", "coverage", "count", "mse", "se", "rejection_probability"),
                                        parameters = c("m_perturbation"),
                                        threshold = 0.1) %>% as_tibble()

plot_all_arms(summarized_results, "m_perturbation", "bias", plot_discont_points = TRUE)
plot_all_arms(summarized_results, "m_perturbation", "coverage", plot_discont_points = TRUE)
plot_all_arms(summarized_results, "m_perturbation", "mse", plot_discont_points = TRUE)
plot_all_arms(summarized_results, "m_perturbation", "count", ylim = c(0, 1000))

# examine the membership probabilities in four different grid IDs
mem_probs <- plot_posterior_membership_probabilities(sim_res = sim_res, grid_row_id = 1, valid_ids = valid_ids)

# plot the m_perturbation estimates themselves
plot_em_classifications(em_classifications = id_classifications$em_classifications, sim_spec = sim_spec, grid_row_id = 2)
