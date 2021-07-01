sim_dir <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "private/simulations")

library(cowplot)
library(tidyverse)
library(simulatr)

plot_all_arms <- function(summarized_results, parameter, metric) {
  arms <- grep(pattern = "^arm_", x = colnames(summarized_results), value = TRUE) %>% gsub(pattern = "^arm_", replacement = "", x = .)
  summarized_results_sub <- dplyr::filter(summarized_results, parameter == !!parameter)
  ps <- lapply(arms, function(arm) {
    arm_name <- paste0("arm_", arm)
    other_arms <- arms[ !(arms == arm) ]
    title <- sapply(other_arms, function(other_arm) paste0(other_arm, " = ", summarized_results_sub[[other_arm]][1]))
    title <- paste0(paste0(title, collapse = ", "))
    to_plot <- filter(summarized_results_sub, !!as.symbol(arm_name) & metric == !!metric)
    p <- ggplot(to_plot, aes(x = !!as.symbol(arm), y = value, col = method)) + geom_point() + geom_line() + ylab(metric) + geom_errorbar(aes(ymin = lower_mc_ci, ymax = upper_mc_ci, width = I(0.03))) + theme_bw() + theme(plot.title = element_text(size = 11, hjust = 0.5)) + ggtitle(title)
    l <- get_legend(p + theme(legend.position = "bottom"))
    p_out <- p + theme(legend.position = "none")
    return(list(plot = p_out, legend = l))
  })
  n_ps <- length(ps)
  vert_plot <- plot_grid(plotlist = lapply(ps, function(i) i$plot),
                         align = "v", axis = "l", nrow = n_ps, labels = letters[1:n_ps])
  out <- plot_grid(vert_plot, ps[[1]]$legend, ncol = 1, rel_heights = c(1, .1))
  return(out)
}


# Study 1.
sim_spec <- readRDS(paste0(sim_dir, "/sim_spec_1.rds"))
sim_res <- readRDS(paste0(sim_dir, "/raw_result_1.rds"))
summarized_results <- summarize_results(sim_spec = sim_spec, sim_res = sim_res, metrics = c("coverage", "bias"),
                                        parameters = c("m_perturbation", "m_intercept", "pi"))

get_arm_plots(summarized_results = summarized_results, parameter = "m_perturbation", metric = "bias")
