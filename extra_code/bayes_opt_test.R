library(ondisc)
load_all("~/research_code/glmeiv/")

res <- readRDS("/Users/timbarry/research_offsite/glmeiv/public/xie/results/glmeiv_result.rds")
set.seed(4)
ests <- res %>% filter(target == "estimate") %>% select(-target, -gene_name, -protein_coding) %>% mutate(parameter = as.character(parameter)) %>%
  tidyr::pivot_wider(values_from = "value", names_from = "parameter") %>% select(-starts_with("m_")) %>% filter(type == "cis") %>% 
  sample_n(100)


gRNA_odm <- read_odm(odm_fp = "/Users/timbarry/research_offsite/xie-2019/processed/gRNA/raw_grouped.odm",
                     metadata_fp = "/Users/timbarry/research_offsite/glmeiv/public/xie/data/gRNA_metadata.rds")
covariate_matrix <- readRDS("/Users/timbarry/research_offsite/glmeiv/public/xie/data/covariate_matrix.rds")
g_offset <- readRDS("/Users/timbarry/research_offsite/glmeiv/public/xie/data/g_offset.rds")
g_fam <- poisson() %>% augment_family_object()

get_bayes_opt_thres <- function(r) {
  g_int <- r$g_intercept %>% log()
  g_pert <- r$g_perturbation %>% log()
  pi <- r$pi
  glmeiv::get_optimal_threshold(g_intercept = g_int, g_perturbation = g_pert, g_fam = g_fam, pi = pi, g_offset = g_offset) %>% median(na.rm = TRUE)
}

sapply(X = seq(1, nrow(ests)), FUN = function(i) {
  r <- ests[i,]
  get_bayes_opt_thres(r)
}) %>% median(na.rm = TRUE) %>% round()


###########
# Gasperini
###########
glmeiv_res <- readRDS("~/Desktop/result_glmeiv.rds")
ests <- glmeiv_res %>% filter(target == "estimate") %>% select(-target) %>% mutate(parameter = as.character(parameter)) %>%
  tidyr::pivot_wider(values_from = "value", names_from = "parameter") %>% select(-starts_with("m_")) %>% filter(site_type == "selfTSS") %>% 
  sample_n(100)

covariate_matrix <- readRDS("/Users/timbarry/research_offsite/glmeiv/public/gasperini/data/covariate_matrix.rds")
g_offset <- readRDS("/Users/timbarry/research_offsite/glmeiv/public/gasperini/data/g_offsets.rds")
g_fam <- poisson()

sapply(X = seq(1, nrow(ests)), FUN = function(i) {
  r <- ests[i,]
  get_bayes_opt_thres(r)
}) %>% median(na.rm = TRUE)
