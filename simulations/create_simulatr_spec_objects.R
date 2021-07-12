library(magrittr)
library(glmeiv)

# optional command line argument (default FALSE); overwrite
args <- commandArgs(trailingOnly = TRUE)
overwrite <- if (is.na(args[1])) FALSE else as.logical(args[1])

# function to save; if file is nonexistent on disk, then save; if the file does exist on disk, save if "overwrite" set to TRUE
save_obj <- function(obj, file_path, overwrite) {
  if (!file.exists(file_path)) { # if file does not exist, save
    saveRDS(obj, file_path)
  } else { # if file does exist, save only if overwrite true
    if (overwrite) {
      saveRDS(obj, file_path)
    }
  }
}

# create simulatr objects
# This script creates simulatr_specifier objects corresponding to the simulation studies reported in the manuscript.
# It stores the objects as .rds files in the simulations subdirectory of the offsite directory.
sim_dir <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "private/simulations")

# 1. Study 1: no covariates; varying m_pert, g_pert, and pi; poisson family objects.
# m_pert and g_pert baselines are easy (large values)
to_save_1 <- paste0(sim_dir, "/sim_spec_1.rds")
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
    save_membership_probs_mult = 250,
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
save_obj(sim_spec_1, to_save_1, overwrite)

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
  save_membership_probs_mult = 250,
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
save_obj(sim_spec_2, to_save_2, overwrite)

# Study 3: Power to detect alternative. No covariates; varying m_pert over a fine, near-zero grid over three levels of g_pert (hard, easy, medium); Also test null hypothesis: m_pert = 0, vary g_pert over grid spanning hard to easy.
to_save_3 <- paste0(sim_dir, "/sim_spec_3.rds")
m_pert_seq <- seq(-0.01, -0.1, -0.01)
l_m_mert_seq <- length(m_pert_seq)
g_pert_seq <- c(1, 1.5, 2)
g1 <- expand.grid(m_perturbation = m_pert_seq, g_perturbation = g_pert_seq) %>%
  dplyr::mutate(arm_hard = c(rep(TRUE, l_m_mert_seq), rep(FALSE, 2 * l_m_mert_seq)),
                arm_intermediate = c(rep(FALSE, l_m_mert_seq), rep(TRUE, l_m_mert_seq), rep(FALSE, l_m_mert_seq)),
                arm_easy = c(rep(FALSE, 2 * l_m_mert_seq), rep(TRUE, l_m_mert_seq)))
g2 <- data.frame(m_perturbation = 0,
                 g_perturbation = seq(g_pert_seq[1],  g_pert_seq[3], length.out = l_m_mert_seq),
                 arm_hard = FALSE, arm_intermediate = FALSE, arm_easy = FALSE)
param_grid <- rbind(g1, g2) %>% dplyr::mutate(arm_null = c(rep(FALSE, 3 * l_m_mert_seq), rep(TRUE, l_m_mert_seq))) %>%
  dplyr::mutate(grid_id = seq(1, 4 * l_m_mert_seq))

fixed_params <- list(
  seed = 4,
  n = 2000,
  B = 1000,
  n_processors = 10,
  m_intercept = 3,
  g_intercept = -1,
  pi = 0.15,
  m_fam = poisson(),
  g_fam = poisson(),
  alpha = 0.95,
  n_em_rep = 10,
  lambda = NULL,
  sd = 0.2,
  save_membership_probs_mult = 250,
  m_covariate_coefs = NULL,
  g_covariate_coefs = NULL,
  covariate_matrix = NULL,
  m_offset = NULL,
  g_offset = NULL
)
one_rep_times <- list(generate_data_function = 1,
                      thresholding = 1,
                      em = 80)
sim_spec_3 <- glmeiv::create_simulatr_specifier_object(param_grid = param_grid,
                                                       fixed_params = fixed_params,
                                                       one_rep_times = one_rep_times,
                                                       covariate_sampler = NULL)
save_obj(sim_spec_3, to_save_3, overwrite)
