library(ondisc)
library(dplyr)
load_all("~/research_code/glmeiv/")
library(magrittr)

###########
# GASPERINI
###########
glmeiv_res <- paste0("~/Desktop/result_glmeiv.rds") %>% readRDS()
problem_pairs <- glmeiv_res %>% filter(parameter == "m_perturbation", target == "estimate", site_type == "NTC", value < 0.5) %>% pull(pair_id) %>% as.character()
problem_pairs_df <- glmeiv_res %>% filter(parameter == "m_perturbation", target == "estimate", site_type == "NTC", value < 0.5) %>% select(gene_id, gRNA_id, pair_id, site_type, value)
saveRDS(object = problem_pairs_df, file = "/Users/timbarry/research_offsite/glmeiv/public/gasperini/data/gene_gRNA_pairs_problem.rds")

# i <- 40
# split <- strsplit(problem_pairs[i], ":")[[1]]
# gene_id <- split[1]
# gRNA_id <- split[2]
# glmeiv_res %>% filter(pair_id == problem_pairs[i], target == "estimate", parameter == "m_perturbation")

gene_odm <- read_odm(odm_fp = "/Users/timbarry/research_offsite/gasperini-2019/at-scale/processed/gene/gasp_scale_gene_expressions.odm",
                     metadata_fp = "/Users/timbarry/research_offsite/glmeiv/public/gasperini/data/gene_qc_metadata.rds")
m <- as.numeric(gene_odm[[gene_id,]])

gRNA_odm <- read_odm(odm_fp = "/Users/timbarry/research_offsite/gasperini-2019/at-scale/processed/gRNA_grouped/gasp_scale_gRNA_counts_grouped.odm",
                     metadata_fp = "/Users/timbarry/research_offsite/glmeiv/public/gasperini/data/gRNA_qc_metadata.rds")
g <- as.numeric(gRNA_odm[[gRNA_id,]])
dat <- data.frame(m = m, g = g)
dat <- set_attr(dat, which = "i", 1)

covariate_matrix <- readRDS("/Users/timbarry/research_offsite/glmeiv/public/gasperini/data/covariate_matrix.rds")
m_offset <- readRDS("/Users/timbarry/research_offsite/glmeiv/public/gasperini/data/m_offsets.rds")
g_offset <- readRDS("/Users/timbarry/research_offsite/glmeiv/public/gasperini/data/g_offsets.rds")

m_fam <- MASS::negative.binomial(NA) %>% augment_family_object()
g_fam <- poisson() %>% augment_family_object()

m_precomp <- run_glmeiv_precomputation(m, covariate_matrix, m_offset, m_fam)
g_precomp <- run_glmeiv_precomputation(g, covariate_matrix, g_offset, g_fam)

fit1 <- run_glmeiv_given_precomputations(m = m, g = g, m_precomp = m_precomp, g_precomp = g_precomp,
                                 covariate_matrix = covariate_matrix, m_offset = m_offset, g_offset = g_offset, pi_guess_range = c(0.001, 0.03),
                                 m_perturbation_guess_range = log(c(0.1, 1.5)), g_perturbation_guess_range = log(c(5, 15)), n_em_rep = 15)
run_inference_on_em_fit(fit1) %>% wrangle_glmeiv_result(s = ., time = 0, em_fit = fit1, exponentiate_coefs = TRUE, save_membership_probs_mult = 100, i = 1)

fit <- run_glmeiv_at_scale_simulatr(dat = dat, m_fam = m_fam, g_fam = g_fam, covariate_matrix = covariate_matrix,
                                    m_offset = m_offset, g_offset = g_offset, exponentiate_coefs = TRUE, alpha = 0.95,
                                    n_em_rep = 15, save_membership_probs_mult = 500, pi_guess_range = c(0.001, 0.03),
                                    m_perturbation_guess_range = log(c(0.1, 1.5)), g_perturbation_guess_range = log(c(5, 15)))

############
# XIE
############
gene_id <- "ENSG00000229124.6"
gRNA_id <- "chr18:9825498-9825898"

gene_odm <- read_odm(odm_fp = "/Users/timbarry/research_offsite/xie-2019/processed/gene/expression_matrix.odm",
                     metadata_fp = "/Users/timbarry/research_offsite/glmeiv/public/xie/data/gene_metadata.rds")
m <- as.numeric(gene_odm[[gene_id,]])

gRNA_odm <- read_odm(odm_fp = "/Users/timbarry/research_offsite/xie-2019/processed/gRNA/raw_grouped.odm",
                     metadata_fp = "/Users/timbarry/research_offsite/glmeiv/public/xie/data/gRNA_metadata.rds")
g <- as.numeric(gRNA_odm[[gRNA_id,]])

covariate_matrix <- readRDS("/Users/timbarry/research_offsite/glmeiv/public/xie/data/covariate_matrix.rds")
m_offset <- readRDS("/Users/timbarry/research_offsite/glmeiv/public/xie/data/m_offset.rds")
g_offset <- readRDS("/Users/timbarry/research_offsite/glmeiv/public/xie/data/g_offset.rds")

m_fam <- MASS::negative.binomial(NA) %>% augment_family_object()
g_fam <- poisson() %>% augment_family_object()

m_precomp <- run_glmeiv_precomputation(m, covariate_matrix, m_offset, m_fam)
g_precomp <- run_glmeiv_precomputation(g, covariate_matrix, g_offset, g_fam)

fit1 <- run_glmeiv_given_precomputations(m = m, g = g, m_precomp = m_precomp, g_precomp = g_precomp,
                                         covariate_matrix = covariate_matrix, m_offset = m_offset, g_offset = g_offset, pi_guess_range = c(0.001, 0.03),
                                         m_perturbation_guess_range = log(c(0.1, 1.5)), g_perturbation_guess_range = log(c(5, 15)), n_em_rep = 15)
run_inference_on_em_fit(fit1) %>% wrangle_glmeiv_result(s = ., time = 0, em_fit = fit1, exponentiate_coefs = TRUE, save_membership_probs_mult = 100, i = 1)

# Code to debug PSC

require(dplyr);
problem_pairs <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "public/xie/data/gRNA_gene_pairs_problem.rds");
prob_pairs <- readRDS(problem_pairs) %>% dplyr::pull(pair_id) %>% as.character();
res_pairs <- readRDS("raw_result.rds") %>% summarize(paste0(gene_id, ":", gRNA_id)) %>% pull() %>% unique();
any(res_pairs %in% prob_pairs)
all_pairs <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "public/xie/data/gRNA_gene_pairs.rds") %>% readRDS() %>% mutate(pair_id = paste0(gene_id, ":", gRNA_id)) %>% pull(pair_id) %>% as.character()

which(prob_pairs[1] == all_pairs)/200
