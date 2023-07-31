require(dplyr)

bad_pairs_local <- readRDS("/Users/timbarry/research_offsite/glmeiv/public/xie/results/glmeiv_result_trial.rds")
res <- readRDS("/Users/timbarry/research_offsite/glmeiv/public/xie/results/glmeiv_result.rds")

local_pairs <- bad_pairs_local$pair_id %>% unique() %>% as.character()
psc_pairs <- res$pair_id %>% unique() %>% as.character()
all(local_pairs %in% psc_pairs)
res <- res %>% filter(pair_id %in% local_pairs)

local <- bad_pairs_local %>% filter(parameter == "m_perturbation", target == "estimate") %>% select(pair_id, value)
psc <- res %>% filter(parameter == "m_perturbation", target == "estimate") %>% select(pair_id, value)

dplyr::left_join(local, psc, by = "pair_id") %>% arrange(pair_id) %>% print(n = 31)

#####
to_save <- rbind(bad_pairs_local %>% filter(parameter == "m_perturbation", target == "estimate") %>% select(gene_id, gRNA_id, pair_id, type))
#####


