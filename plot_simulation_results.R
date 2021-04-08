# obtain all args
args <- commandArgs(trailingOnly = TRUE)
simulation_dir <- if (is.na(args[1])) "~/Box/glm-eiv/simulation_dir" else args[1]
results <- readRDS(paste0(simulation_dir, "/results/combined_results.rds"))

figure_dir <- paste0(simulation_dir, "/figures")
if (!dir.exists(figure_dir)) dir.create(figure_dir)

library(ggplot2)
library(dplyr)
library(gridExtra)

colnames(results)[colnames(results) == "m_perturbation"] <- "m_pert"
colnames(results)[colnames(results) == "g_perturbation"] <- "g_pert"

summary_plot_for_var <- function(variable, results) {
res_var <- filter(results, variable == !!variable)
to_plot <- res_var %>% mutate(n = factor(n))
p1 <- ggplot(data = to_plot, aes(x = n, y = mean_estimate)) + 
  facet_grid(m_pert ~ g_pert, labeller = label_context, scales = "free_y") +
  geom_hline(mapping = aes(yintercept = gt_value), col = "blue") + 
  geom_point() + geom_errorbar(aes(ymin = mean_lower_ci, ymax = mean_upper_ci, width = 0.2), col = "red") +
  theme_bw() +
  ggtitle(paste0("Mean est. and CI for ", variable)) +
  ylab("Mean estimate and CI") +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data = to_plot, aes(x = n, y = coverage)) + 
  facet_wrap(. ~ m_pert + g_pert, labeller = label_context) +
  geom_point() + theme_bw() + 
  ggtitle(paste0("CI coverage rate for ", variable)) + 
  ylab("Coverage probability") +
  geom_hline(mapping = aes(yintercept = 0.95), col = "blue") +
  geom_errorbar(aes(ymin = coverage_lower_ci, ymax = coverage_upper_ci, width = 0.2), col = "red") +
  theme(plot.title = element_text(hjust = 0.5))
  
p_out <- grid.arrange(p1, p2, nrow = 1)
return(p_out)
}

# create plot for each variable
for (variable in unique(results$variable)) {
  p <- summary_plot_for_var(variable, results) 
  f_name <- paste0(figure_dir, "/", variable, "_plot.pdf")
  ggsave(filename = f_name, plot = p, device = "pdf", scale = 1, width = 8, height = 4.5)
}
