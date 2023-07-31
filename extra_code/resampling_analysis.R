library(dplyr)
library(ondisc)
library(magrittr)
load_all("~/research_code/glmeiv/")

glmeiv_dir <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "public/gasperini/data/")
gasp_dir <- paste0(.get_config_path("LOCAL_GASPERINI_2019_DATA_DIR"), "at-scale/processed/")

# load positive control pairs
pairs_df <- paste0(glmeiv_dir, "gRNA_gene_pairs.rds") %>% readRDS()
pos_pairs <- pairs_df %>% dplyr::filter(site_type == "selfTSS")

# load data
covariate_matrix <- paste0(glmeiv_dir, "covariate_matrix.rds") %>% readRDS()
gene_odm <- read_odm(odm_fp = paste0(gasp_dir, "gene/gasp_scale_gene_expressions.odm"),
                     metadata_fp = paste0(glmeiv_dir, "gene_qc_metadata.rds"))
m_offset <- paste0(glmeiv_dir, "m_offsets.rds") %>% readRDS()
gRNA_odm <- read_odm(odm_fp = paste0(gasp_dir, "gRNA_grouped/gasp_scale_gRNA_counts_grouped.odm"),
                     metadata_fp = paste0(glmeiv_dir, "gRNA_qc_metadata.rds"))
g_offset <- paste0(glmeiv_dir, "g_offsets.rds") %>% readRDS()

# get random pc pair
rand_pair <- pos_pairs %>% dplyr::sample_n(1) # ENSG00000136732, GYPC_TSS
m <- gene_odm[[as.character(rand_pair$gene_id),]] %>% as.numeric()
g <- gRNA_odm[[as.character(rand_pair$gRNA_id),]] %>% as.numeric()
phat <- as.integer(g >= 5)
dat <- data.frame(m = m, g = g)
dat <- set_attr(dat, "i", 1)

# family objects
m_fam <- poisson() %>% augment_family_object()
g_fam <- poisson() %>% augment_family_object()

# run methods
fit_base_glmeiv <- run_glmeiv_at_scale_simulatr(dat = dat, m_fam = m_fam, g_fam = g_fam, covariate_matrix = covariate_matrix,
                                                m_offset = m_offsets, g_offset = g_offset, alpha = 0.95, n_em_rep = 15,
                                                save_membership_probs_mult = 1, exponentiate_coefs = FALSE)
pi_est <- fit_base_glmeiv %>% dplyr::filter(parameter == "pi", target == "estimate") %>% dplyr::pull(value)

fit_base_thresh <- run_thresholding_method(phat = phat, m = m, m_fam = m_fam, m_offset = m_offset,
                                           covariate_matrix = covariate_matrix, exponentiate_coefs = FALSE,
                                           n_examples_per_param = 5, alpha = 0.95)

# extract fitted coefficients and membership probabilities
param_ests <- fit_base_glmeiv %>% dplyr::filter(parameter %in% c("g_intercept" , "g_perturbation", "g_batch", "g_p_mito"), target == "estimate") %>%
  dplyr::select(-target)
param_ests_v <- set_names(param_ests$value, param_ests$parameter)
posterior_pert_probs <- fit_base_glmeiv %>% dplyr::filter(target == "membership_probability") %>% pull(value)
n <- length(posterior_pert_probs)
B <- 10
mat <- sapply(X = seq(1, n), FUN = function(i) {
  rbinom(n = B, size = 1, prob = posterior_pert_probs[i])
}) %>% t()

# generate synthetic gRNA data from the fitted model
g_resample <- generate_glm_data_sim(intercept = param_ests_v[["g_intercept"]],
                                    perturbation_coef = param_ests_v[["g_perturbation"]],
                                    perturbation_indicators = mat,
                                    fam = g_fam, covariate_matrix = covariate_matrix,
                                    covariate_coefs = param_ests_v[c("g_batch", "g_p_mito")],
                                    offset = g_offset, n = n, B = B)
# mean expressions for p = 0, p = 1
mean_tbl <- sapply(X = seq(1, B), FUN = function(i) {
  p <- mat[,i]
  curr_g <- g_resample[,i]
  c(mean_0 = mean(curr_g[p == 0]),
    mean_1 = mean(curr_g[p == 1]))
})

# fit models again, but with resampled g's
fit_resample_glmeiv <- run_glmeiv_at_scale_simulatr(dat = data.frame(m, g_resample[,1]), m_fam = m_fam, g_fam = g_fam,
                                                    covariate_matrix = covariate_matrix, m_offset = m_offsets,
                                                    g_offset = g_offset, alpha = 0.95, n_em_rep = 15,
                                                    save_membership_probs_mult = 250, exponentiate_coefs = FALSE)


# resample again, this time increasing background contamination (while keeping the expression for pert = 1 constant)
g_pert <- param_ests_v[["g_perturbation"]]
g_int <- param_ests_v[["g_intercept"]]
get_new_coefs <- function(background_contamination, g_int, g_pert) {
  if (background_contamination == 0) {
    out <- c(new_g_int = g_int, new_g_pert = g_pert)
  } else {
    mean_expression_1 <- exp(g_int + g_pert)
    new_beta_0 <- log(background_contamination * mean_expression_1)
    new_beta_1 <- log(mean_expression_1) - new_beta_0
    out <- c(new_g_int = new_beta_0, new_g_pert = new_beta_1)
  }
  return(out)
}

# mean expressions for p = 0, p = 1
# mean_tbl <- sapply(X = seq(1, B), FUN = function(i) {
#  p <- mat[,i]
#  curr_g <- g_resample[,i]
#  c(mean_0 = mean(curr_g[p == 0]),
#    mean_1 = mean(curr_g[p == 1]))
# })

new_coefs <- get_new_coefs(0.3, g_int, g_pert)
g_resample <- generate_glm_data_sim(intercept = new_coefs[["new_g_int"]],
                                    perturbation_coef = new_coefs[["new_g_pert"]],
                                    perturbation_indicators = mat,
                                    fam = g_fam, covariate_matrix = covariate_matrix,
                                    covariate_coefs = param_ests_v[c("g_batch", "g_p_mito")],
                                    offset = g_offset, n = n, B = B)

# fit the glmeiv/thresholding again
fit_resample_glmeiv_2 <- run_glmeiv_at_scale_simulatr(dat = data.frame(m, g_resample[,1]), m_fam = m_fam, g_fam = g_fam,
                                                      covariate_matrix = covariate_matrix, m_offset = m_offsets,
                                                      g_offset = g_offset, alpha = 0.95, n_em_rep = 15,
                                                      save_membership_probs_mult = 250, exponentiate_coefs = TRUE)
fit_thresholding_2 <- run_thresholding_method_simulatr(dat = data.frame(m, g_resample[,1]), g_intercept = new_coefs[["new_g_int"]],
                                                       g_perturbation = new_coefs[["new_g_pert"]], g_fam = g_fam, m_fam = m_fam,
                                                       pi = pi_est, covariate_matrix = covariate_matrix, g_covariate_coefs = param_ests_v[c("g_batch", "g_p_mito")],
                                                       m_offset = m_offset, g_offset = g_offset, 0.95)
fit_thresholding_2 %>% dplyr::filter(parameter == "m_perturbation") %>% dplyr::mutate(value = exp(value))
fit_base_thresh %>% dplyr::filter(parameter == "m_perturbation") %>% dplyr::mutate(value = exp(value))

