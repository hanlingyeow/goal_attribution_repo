
rnd <- function(val){
  format(round(val, 2), nsmall = 2)
}

p <- function(val){
  if (val > .1) {
    str_c("p = ", sub("^0+", "", format(round(val, 2), nsmall = 2)))
  } else if (val > .001) {
    str_c("p = ", sub("^0+", "", format(round(val, 3), nsmall = 3)))
  } else {
    "p < .001"
  }
}

format_table <- function(.data, p = F) {
  .data <- .data |> mutate(term = str_replace_all(term, ":", ' x '),
                           term = str_replace_all(term, "speciesbonobo", 'SpeciesBonobo'),
                           term = str_replace_all(term, "speciesorang", 'SpeciesOrang'),
                           term = str_replace_all(term, "conditionhand", 'ConditionHand'), 
                           term = str_replace_all(term, "conditionagent", 'ConditionAgent'),
                           term = str_replace_all(term, "handseen", 'HandSeen'), 
                           term = str_replace_all(term, "agentseen", 'AgentSeen'),
                           term = str_replace_all(term, "z_trial_consecutive", 'Session number'), 
                           term = str_replace_all(term, "exp2024", 'Year'),
                           term = str_replace_all(term, "z_trial_within", 'Trial number'))
  if (p){
    .data %>% mutate(p = case_when(p > .1 ~ sub("^0+", "", format(round(p, 2), nsmall = 2)),
                                   p > .001 ~ sub("^0+", "", format(round(p, 3), nsmall = 3)),
                                   is.na(p) ~ NA,
                                   TRUE ~ '< .001'),
                     across(!c(p, term), ~ round(.x, 2)))
  } else {
    .data %>% mutate(across(!term, ~ round(.x, 2)))
  }

}
