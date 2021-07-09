# studying non-monotonicity
sim_spec@parameter_grid$grid_row_id <- seq(1, nrow(sim_spec@parameter_grid))
mean_phat <- filter(sim_res, method == "thresholding", target == "sum_phat") %>% group_by(grid_row_id) %>% summarize(m_sum_phat = mean(value)) %>% mutate(grid_row_id = as.integer(as.character(grid_row_id)), m_mean_phat = m_sum_phat/2000)
to_plot <- left_join(x = sim_spec@parameter_grid, y = mean_phat, by = "grid_row_id") %>% filter(pi < 0.5)
require(ggplot2)
ggplot(data = filter(to_plot , arm_pi), mapping = aes(x = pi, y = m_mean_phat)) + geom_point() + geom_line() + geom_abline(slope = 1, intercept = 0, col = "red")

bayes_opt <- sapply(X = seq(0.05,0.45,0.05), FUN = function(i) {
  glmeiv::get_optimal_threshold(g_intercept = sim_spec@fixed_parameters$g_intercept,
                                g_perturbation = 3,
                                g_fam = sim_spec@fixed_parameters$g_fam,
                                pi = i,
                                covariate_matrix = NULL,
                                g_covariate_coefs = NULL,
                                g_offset = NULL)})
