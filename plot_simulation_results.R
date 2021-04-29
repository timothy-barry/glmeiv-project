# obtain all args
args <- commandArgs(trailingOnly = TRUE)
simulation_dir <- if (is.na(args[1])) "~/Box/glm-eiv/simulation_dir" else args[1]
results <- readRDS(paste0(simulation_dir, "/results/combined_results.rds"))

figure_dir <- paste0(simulation_dir, "/figures")
if (!dir.exists(figure_dir)) dir.create(figure_dir)

library(ggplot2)
library(dplyr)
library(gridExtra)

colnames(results)[colnames(results) == "m_(Intercept)"] <- "m_intercept"
colnames(results)[colnames(results) == "g_(Intercept)"] <- "g_intercept"


make_summary_plot <- function(variable, method, results) {
  res_var <- filter(results, variable == !!variable & method == !!method)
  to_plot <- res_var %>% mutate(n = factor(n),
                              m_intercept = paste("m-int=", m_intercept),
                              g_perturbation = paste("g-pert=", g_perturbation))
  free <- if (variable %in% c("g_perturbation", "m_(Intercept)")) "free_y" else "fixed"
  form <- if (variable == "g_perturbation") "g_perturbation ~ m_intercept" else "m_intercept ~ g_perturbation"

  p1 <- ggplot(data = to_plot, aes(x = n, y = mean_estimate)) + 
  facet_grid(form, labeller = label_context, scales = free) +
  geom_hline(mapping = aes(yintercept = gt_value), col = "blue") + 
  geom_point() + geom_errorbar(aes(ymin = mean_lower_ci, ymax = mean_upper_ci, width = 0.2), col = "red") +
  theme_bw() +
  ggtitle("Mean estimate and CI") +
  ylab("Mean estimate and CI") +
  theme(plot.title = element_text(hjust = 0.5))

  p2 <- ggplot(data = to_plot, aes(x = n, y = coverage)) + 
  facet_grid(form, labeller = label_context) +
  geom_point() + theme_bw() + 
  ggtitle("CI coverage rate") + 
  ylab("Coverage probability") +
  geom_hline(mapping = aes(yintercept = 0.90), col = "blue") +
  geom_errorbar(aes(ymin = coverage_lower_ci, ymax = coverage_upper_ci, width = 0.2), col = "red") +
  theme(plot.title = element_text(hjust = 0.5))
  
  p_out <- grid.arrange(p1, p2, nrow = 1, top = paste0("Variable: ", variable, ", method: ", method))
  return(p_out)
}

pairs <- results %>% group_by(variable, method) %>% summarize(variable = variable[1], method = method[1], .groups = "drop")

# create plot for each variable
for (i in seq(1, nrow(pairs))) {
  method <- pairs[[i, "method"]]
  variable <- pairs[[i, "variable"]]
  p <- make_summary_plot(variable, method, results) 
  f_name <- paste0(figure_dir, "/", method, "-", variable, "_plot.pdf")
  ggsave(filename = f_name, plot = p, device = "pdf", scale = 1, width = 8, height = 4.5)
}
