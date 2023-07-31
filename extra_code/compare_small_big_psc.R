x <-
y <-

y_sub <- y %>% filter(parameter == "m_perturbation", target == "estimate")
p_pairs <- y_sub$pair_id %>% as.character()
x_sub <- x %>% filter(parameter == "m_perturbation", target == "estimate", pair_id %in% p_pairs)

comp <- left_join(x_sub, y_sub, by = "pair_id") %>% select(value.x, value.y)