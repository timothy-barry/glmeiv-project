# create simulatr objects
# This script creates simulatr_specifier objects corresponding to the simulation studies reported in the manuscript.
# It stores the objects as .rds files in the simulations subdirectory of the offsite directory.
sim_dir <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "private/simulations")

# 1. Study 1: no covariates; varying m_pert, g_pert, and pi; poisson family objects.
to_save <- paste0(sim_dir, "/sim_spec_1.rds")
param_grid <- simulatr::create_param_grid(varying_values = list(pi = seq(0.05, 0.5, 0.05),
                                                  m_perturbation = seq(0.0, -2, -0.2),
                                                  g_perturbation = seq(0.0, 3, 0.25)),
                            baseline_values = list(pi = 0.25, m_perturbation = -2, g_perturbation = 3))
fixed_params <- list(
  seed = 4,
  n = 2000,
  B = 2000,
  n_processors = 5,
  m_intercept = 1,
  g_intercept = -2,
  m_fam = poisson(),
  g_fam = poisson(),
  alpha = 0.95,
  n_em_rep = 5,
  p_flip = 0.01,
  m_covariate_coefs = NULL,
  g_covariate_coefs = NULL,
  covariate_matrix = NULL,
  m_offset = NULL,
  g_offset = NULL
)
one_rep_times <- list(generate_data_function = 0.1,
                      thresholding = 0.3,
                      em = 42.5)
sim_spec_1 <- glmeiv::create_simulatr_specifier_object(param_grid = param_grid,
                                                       fixed_params = fixed_params,
                                                       one_rep_times = one_rep_times,
                                                       covariate_sampler = NULL)

saveRDS(object = sim_spec_1, to_save)

# 2. A test example
sim_spec_ex <- glmeiv::create_simulatr_specifier_object(param_grid = param_grid[1:5,],
                                                        fixed_params = fixed_params,
                                                        one_rep_times = list(one_rep_times),
                                                        covariate_sampler = NULL)
saveRDS(object = sim_spec_ex, paste0(sim_dir, "/sim_spec_ex.rds"))

# 2. Study 2: covariates present, varying m_pert, g_pert, and pi; poisson family objects.
# to_save <- paste0(sim_dir, "/sim_spec_2.rds")
# fixed_params[["m_covariate_coefs"]] <- c(0.2, -0.1)
# fixed_params[["g_covariate_coefs"]] <- c(0.1, -0.2)
# covariate_sampler <- list(lib_size = function(n) rpois(n, 10),
#                          p_mito = function(n) runif(n, 0, 10))
# sim_spec_2 <- glmeiv::create_simulatr_specifier_object(param_grid, fixed_params, covariate_sampler)
# saveRDS(object = sim_spec_2, to_save)
