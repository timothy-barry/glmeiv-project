# parameter file
# the parameter file should define the following variables:
# - m_fam: family object for m
# - g_fam: family object for g
# - m_coef_names: the ordered names of the m coefficients
# - g_coef_names: the ordered names of the g coefficients
# - fixed_params: a named numeric vector giving the fixed params
# - varying_params: a named data frame giving all combinations of the varying params.
# - covariate_sampler: a list of functions; each function takes 'n' as an argument and outputs random values.
# - n_outer_reps: number of processors to distribute given param id across
# - n_inner_reps: number of reps to run within a given param id and processor

# note: the order of m_coef_names, g_coef_names, and covariate_sampler matter (the order of the others do not).
# It is assumed that, for coef_names, the first entry is the intercept, the second is the perturbation, and
# the subsequent names are the technical factors. Additionally, the order of the technical factors in 
# coef_names should match those of varying_params.

# The following params should be declared across fixed_params and varying_params:
# m_intercept, m_perturbation, g_intercept, g_perturbation, n, pi.
library(magrittr)

m_fam <- poisson()
g_fam <- poisson()
m_coef_names <- c("m_intercept", "m_perturbation")
g_coef_names <- c("g_intercept", "g_perturbation")
fixed_params <- c(m_intercept = 2,
                  g_intercept = 1,
                  n = 1000)
varying_params <- rbind(data.frame(pi = 0.2, m_perturbation = seq(0, -1, -0.1), g_perturbation = 1),
                        data.frame(pi = 0.2, m_perturbation = -1, g_perturbation = seq(0, 1, 0.1)),
                        data.frame(pi = seq(0.05, 0.5, 0.05), m_perturbation = -1, g_perturbation = 1)) %>% dplyr::distinct()
covariate_sampler <- NULL
n_outer_reps <- 10
n_inner_reps <- 200
ci_coverage <- 0.9

# finally, define the offsite data, results, and logs dirs
drl <- c("data", "results", "logs")
data_results_logs <- setNames(paste0(simulation_dir, "/", drl), drl)
n_fixed <- "n" %in% names(fixed_params)
pi_fixed <- "pi" %in% names(fixed_params)
no_covariates <- length(covariate_sampler) == 0

# a function to obtain the parameter vector given the fixed_params vector, a row from varying_params, and the ordered names of the m or g coefficients
get_param_vector <- function(fixed_params, varying_param_row, coef_names) {
  sapply(X = coef_names, FUN = function(curr_name) {
    if (curr_name %in% names(fixed_params)) fixed_params[[curr_name]] else varying_param_row[[curr_name]]
  })
}
