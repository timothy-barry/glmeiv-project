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
sim_dir <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "private/simulations")
set.seed(4)

# at-scale simulation with Poisson response
n <- 200000
g_perts <- log(seq(2, 4, 0.2))
lib_size <- rpois(n = n, lambda = 5000)
param_grid <- data.frame(g_perturbation = g_perts, grid_id = seq(1, length(g_perts)))
fixed_params <- list(
  B = 100,
  n = 200000,
  pi = 0.005,
  n_processors = 5,
  seed = 4,
  m_fam = poisson() %>% augment_family_object(),
  g_fam = poisson() %>% augment_family_object(),
  m_intercept = log(0.01),
  m_perturbation = log(0.5),
  g_intercept = log(0.002),
  covariate_matrix = data.frame(batch = rbinom(n = n, size = 1, prob = 0.5)),
  m_covariate_coefs = log(0.9),
  g_covariate_coefs = log(1.1),
  m_offset = log(lib_size),
  g_offset = log(lib_size),
  run_unknown_theta_precomputation = FALSE,
  alpha = 0.95,
  n_em_rep = 15,
  save_membership_probs_mult = 250,
  pi_guess_range = c(1e-5, 0.03),
  m_perturbation_guess_range = log(c(0.1, 1.5)),
  g_perturbation_guess_range = log(c(0.5, 10)),
  m_intercept_guess_range = log(c(1e-5, 1e-1)),
  g_intercept_guess_range = log(c(1e-5, 1e-1)),
  m_covariate_coefs_guess_range = log(c(0.5, 1.5)),
  g_covariate_coefs_guess_range = log(c(0.5, 1.5))
)

sim_spec_7 <- create_simulatr_specifier_object_v2(param_grid = param_grid,
                                                          fixed_params = fixed_params,
                                                          one_rep_times = list(generate_data_function = 5,
                                                                               thresholding = 10,
                                                                               glmeiv_fast = 15,
                                                                               glmeiv_slow = 160))
to_save_7 <- paste0(sim_dir, "/sim_spec_7.rds")
save_obj(sim_spec_7, to_save_7, overwrite)
