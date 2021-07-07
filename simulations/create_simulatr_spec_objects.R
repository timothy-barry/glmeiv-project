# create simulatr objects
# This script creates simulatr_specifier objects corresponding to the simulation studies reported in the manuscript.
# It stores the objects as .rds files in the simulations subdirectory of the offsite directory.
sim_dir <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "private/simulations")

# 1. Study 1: no covariates; varying m_pert, g_pert, and pi; poisson family objects.
# m_pert and g_pert baselines are easy (large values)
to_save <- paste0(sim_dir, "/sim_spec_1.rds")
param_grid <- simulatr::create_param_grid(varying_values = list(pi = seq(0.05, 0.5, 0.05),
                                                                  m_perturbation = seq(0.0, -2, -0.2),
                                                                  g_perturbation = seq(0.0, 3, 0.25)),
                                            baseline_values = list(pi = 0.25,
                                                                   m_perturbation = -2,
                                                                   g_perturbation = 3))
fixed_params <- list(
    seed = 4,
    n = 2000,
    B = 1000,
    n_processors = 5,
    m_intercept = 2,
    g_intercept = -2,
    m_fam = poisson(),
    g_fam = poisson(),
    alpha = 0.95,
    n_em_rep = 5,
    lambda = NULL,
    sd = 0.15,
    m_covariate_coefs = NULL,
    g_covariate_coefs = NULL,
    covariate_matrix = NULL,
    m_offset = NULL,
    g_offset = NULL
  )
one_rep_times <- list(generate_data_function = 1,
                        thresholding = 1,
                        em = 11)
sim_spec_1 <- glmeiv::create_simulatr_specifier_object(param_grid = param_grid,
                                                         fixed_params = fixed_params,
                                                         one_rep_times = one_rep_times,
                                                         covariate_sampler = NULL)
saveRDS(object = sim_spec_1, to_save)

# Study 2: no covariates; varying m_pert and g_pert; poisson family objects; no covariates; m_pert and g_pert 0 (challenging setting).
to_save_2 <- paste0(sim_dir, "/sim_spec_2.rds")
param_grid <- simulatr::create_param_grid(varying_values = list(m_perturbation = seq(0.0, -2, -0.2),
                                                                g_perturbation = seq(0.0, 3, 0.2)),
                                          baseline_values = list(m_perturbation = -0.1,
                                                                 g_perturbation = 0.1))
fixed_params <- list(
  seed = 4,
  n = 2000,
  B = 1000,
  n_processors = 5,
  m_intercept = 3,
  g_intercept = -1,
  pi = 0.25,
  m_fam = poisson(),
  g_fam = poisson(),
  alpha = 0.95,
  n_em_rep = 10,
  lambda = NULL,
  sd = 0.15,
  m_covariate_coefs = NULL,
  g_covariate_coefs = NULL,
  covariate_matrix = NULL,
  m_offset = NULL,
  g_offset = NULL
)
one_rep_times <- list(generate_data_function = 1,
                      thresholding = 1,
                      em = 50)
sim_spec_2 <- glmeiv::create_simulatr_specifier_object(param_grid = param_grid,
                                                       fixed_params = fixed_params,
                                                       one_rep_times = one_rep_times,
                                                       covariate_sampler = NULL)

saveRDS(object = sim_spec_2, file = to_save_2)
