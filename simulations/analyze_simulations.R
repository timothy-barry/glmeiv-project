require(dplyr)

sim_dir <- paste0(.get_config_path("LOCAL_GLMEIV_DATA_DIR"), "private/simulations")
sim_spec <- readRDS(paste0(sim_dir, "/sim_spec_1.rds")) # simulatr specifier object
sim_res <- readRDS(paste0(sim_dir, "/raw_result_1.rds")) # raw results

a <- filter(sim_res,
            grid_row_id == 1,
            method == "em" &
            target %in% c("confint_lower", "confint_higher") & 
            parameter == "m_perturbation")

filter(a, target == "confint_lower") %>% summarize(mean_lower_ci = mean(value),
                                                  frac_greater_than_gt = mean(value > -2))
filter(a, target == "confint_higher") %>% summarize(mean_upper_ci = mean(value),
                                                   frac_lessthan_gt = mean(value < -2))

coverage <- dplyr::group_by(a, id) %>% dplyr::summarize(covered = (value[target == "confint_lower"] < -2 & value[target == "confint_higher"] > -2))
coverage_rate <- coverage %>% summarize(mean(covered)) %>% pull()
