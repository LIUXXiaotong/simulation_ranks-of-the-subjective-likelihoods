


sanitise_ranks <- function(rank) {
  str_remove_all(rank, "_") %>% 
    str_remove_all(" ")
}

coh_marg_conj_corr <- read_csv("rank1_strict1_strict_table_marg_conj.csv")
coh_marg_conj_corr <- coh_marg_conj_corr %>% 
  mutate(marginal = sanitise_ranks(`P(A)_P(¬A)_P(B)_P(¬B)`), 
         conjunction = sanitise_ranks(`P(A^B)_P(A^¬B)_P(¬A^B)_P(¬B^¬A)`))

calc_coh_marg_conj <- function(data) {
  #browser()

  ## calculate relative orders
  positions <- data %>% 
    slice(1) %>% 
    pivot_longer(starts_with("order")) %>% 
    summarise(marginal_pos = which(value == "marginal"),
              conj_pos = which(value == "conjunction"))
  
  ## check if order matches
  if (positions$marginal_pos > positions$conj_pos) {
    return(NA)
  }
  
  ## get possible rankings for existing order
  possible_ranks <- coh_marg_conj_corr %>% 
    filter(marginal == sanitise_ranks(data[ data$type == "marginal", ]$response ))
  
  if (length(possible_ranks) == 0) return(0)
  
  ## check if response matches
  if (sanitise_ranks(data[ data$type == "conjunction", ]$response ) %in% 
      possible_ranks$conjunction) {
    return(1)
  } else {
    return(0)
  }
}
