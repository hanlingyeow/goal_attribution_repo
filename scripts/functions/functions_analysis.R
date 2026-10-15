
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
# 
# ### warning: no zi, disp formula
# stabtmb <- function(m.res, name){
#   
#   if (str_detect(name, 'lmer')){
#     print(c(name, 'lmer'))
#     return(glmm.model.stab(m.res, contr = lmerControl(optCtrl=list(maxfun=10000000), optimizer = "bobyqa"), data = get(summary(m.res)$call$data), para = T))
#   }
#   else {
#     print(c('glmmtmb', name))
#     data = get(m.res$call$data)
#     x <- as.character(m.res$call$family)
#     
#     lss <- list(coef = matrix(NA, ncol = length(fixef(m.res)$cond),
#                               nrow = sum(summary(m.res)$ngrps$cond),
#                               dimnames = list(unlist(sapply(names(summary(m.res)$ngrps$cond), function(x){unique(data[[x]])})),
#                                               names(summary(m.res)$coefficients$cond[,1]))),
#                 varcor = matrix(NA, ncol = length(unlist(sapply(summary(m.res)$varcor$cond, function(x){attr(x, 'stddev')}))),
#                                 nrow = sum(summary(m.res)$ngrps$cond),
#                                 dimnames = list(unlist(sapply(names(summary(m.res)$ngrps$cond), function(x){unique(data[[x]])})),
#                                                 names(unlist(sapply(summary(m.res)$varcor$cond, function(x){attr(x, 'stddev')}))))),
#                 warnings = matrix(NA, nrow = sum(summary(m.res)$ngrps$cond),
#                                   dimnames = list(unlist(sapply(names(summary(m.res)$ngrps$cond), function(x){unique(data[[x]])})), 'warnings')))
#     
#     if (!is.null(unlist(lapply(sapply(summary(m.res)$varcor$cond, function(x){attr(x, 'correlation')}), function(x){rep(rownames(x), times = ncol(x))})))) {
#       print('corr present')
#       d <- extract_corr(m.res)
#       lss$cor = matrix(NA, ncol = nrow(d), nrow = sum(summary(m.res)$ngrps$cond), 
#                        dimnames = list(unlist(sapply(names(summary(m.res)$ngrps$cond), function(x){unique(data[[x]])})), str_c(d$Groups, d$var1, d$var2, sep = '•'))
#       )
#     }
#     
#     for (re in names(summary(m.res)$ngrps$cond)){
#       for (i in 1:length(unique(data[[re]]))){   #   if (i > 2){next} ###
#         
#         mod <- glmmTMB(m.res$call$formula,
#                        data = data[data[[re]] != unique(data[[re]])[i],],
#                        family = get(x[1])(x[2]))
#         
#         lss$coef[str_c(unique(data[[re]])[i]),] <- summary(mod)$coefficients$cond[,1]
#         lss$varcor[str_c(unique(data[[re]])[i]),] <- unlist(sapply(summary(mod)$varcor$cond, function(x){attr(x, 'stddev')}))
#         if ('cor' %in% names(lss)){
#           d <- extract_corr(mod)
#           lss$cor[str_c(unique(data[[re]])[i]),] <- d$sdcor
#         }
#         
#         
#         lss$warnings[str_c(unique(data[[re]])[i]),] <- mod$sdr$pdHess
#       }
#     }
#     return(as_tibble(do.call(cbind, lss), rownames = NA))
#   }
# }
# 
# summarise_stab <- function(m.res, stab.res){
#   ls2 <- list(coef = tibble(column = names(summary(m.res)$coefficients$cond[,1]), orig = summary(m.res)$coefficients$cond[,1]),
#               varcor = tibble(column = names(unlist(sapply(summary(m.res)$varcor$cond, function(x){attr(x, 'stddev')}))),
#                               orig = unlist(sapply(summary(m.res)$varcor$cond, function(x){attr(x, 'stddev')}))))
#   
#   if (sum(str_detect(names(stab.res), '•')) > 0){
#     ls2$corr <- tibble(extract_corr(m.res)) |> 
#       mutate(column = str_c(Groups, var1, var2, sep= '•')) |> 
#       select(column, orig = 'sdcor')
#   }
#   
#   res <- stab.res |> 
#     select(!warnings) |> 
#     summarise(across(everything(), list(mmin = ~min(.x, na.rm = T), mmax = ~max(.x, na.rm = T)))) |>
#     pivot_longer(everything(), names_to = c('column', '.value'), names_sep = '_m') |> 
#     left_join(do.call(rbind, ls2))
#   
#   
#   return(res)
# }
