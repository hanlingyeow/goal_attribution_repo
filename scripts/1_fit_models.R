library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(arrow)
library(car)
library(knitr)
library(here)
library(lme4)
library(emmeans)
source('scripts/functions/diagnostic_fcns.r')
source('scripts/functions/functions_analysis.r')
source("scripts/functions/boot_glmmTMB_edit.r")
source('scripts/functions/glmmTMB_stability_edit.r')

# get data ----------------------------------------------------------------
df_testreach <- read_parquet('data/prereg.parquet') |> 
  filter(action == 'test_reach_all') |> 
  filter(!is.na(pls)) |> 
  mutate(condition_hand = as.numeric(condition=='hand') - mean(as.numeric(condition=='hand')), #dummy code and center 
         condition_agent = as.numeric(condition == 'agent') - mean(as.numeric(condition == 'agent')),
         species_bonobo = as.numeric(species == 'bonobo') - mean(as.numeric(species == 'bonobo')),
         species_orang = as.numeric(species == 'orang') - mean(as.numeric(species == 'orang')),
         z_trial_consecutive = (trial_consecutive - mean(trial_consecutive, na.rm = T))/sd(trial_consecutive, na.rm = T))


df_hab <- read_parquet('data/prereg_latency.parquet') |> 
  filter(action == 'hab1') |> 
  filter(!is.na(latency_filtered)) |> 
  mutate(condition_hand = as.numeric(condition=='hand') - mean(as.numeric(condition=='hand')), #dummy code and center 
         condition_agent = as.numeric(condition == 'agent') - mean(as.numeric(condition == 'agent')),
         species_bonobo = as.numeric(species == 'bonobo') - mean(as.numeric(species == 'bonobo')),
         species_orang = as.numeric(species == 'orang') - mean(as.numeric(species == 'orang')),
         z_trial_consecutive = (trial_consecutive - mean(trial_consecutive, na.rm = T))/sd(trial_consecutive, na.rm = T))

df_expl_subset <- read_parquet('data/expl.parquet') |> 
  filter(action == 'test_reach_all') |> 
  filter(!is.na(pls_transform)) |> 
  mutate(cond_center = as.numeric(condition == levels(condition)[2]) - mean(as.numeric(condition == levels(condition)[2])),
         species_bonobo = as.numeric(species == 'bonobo') - mean(as.numeric(species == 'bonobo')),
         species_orang = as.numeric(species == 'orang') - mean(as.numeric(species == 'orang')),
         z_trial_within = (trial_within - mean(trial_within, na.rm = T))/sd(trial_within, na.rm = T))

models <- list()
drops <- list()
boots <- list()


# proportional looking score ----------------------------------------------

models[["pls"]] <- glmmTMB(pls_transform ~ condition * species + z_trial_consecutive + handseen + agentseen + 
                             ((1 | participant_name) + (0 + condition_hand | participant_name) + (0 + condition_agent | participant_name) + (0 + z_trial_consecutive | participant_name)), 
                           dispformula = ~ 1 + either_time, 
                           data = df_testreach, 
                           family = beta_family(link = 'logit'))
drops[["pls"]] <- drop1(models[["pls"]], test = 'Chisq')

models[["pls_noint"]] <- glmmTMB(pls_transform ~ condition + species + z_trial_consecutive + handseen + agentseen + 
                                   ((1 | participant_name) + (0 + condition_hand|participant_name) + (0 + condition_agent|participant_name) + (0 + z_trial_consecutive | participant_name)),
                                 dispformula = ~ 1 + either_time, 
                                 data = df_testreach, 
                                 family = beta_family(link = 'logit'))
drops[["pls_noint"]] <- drop1(models[["pls_noint"]], test = 'Chisq')



# total viewing time ------------------------------------------------------
models[['vt']] <- glmmTMB(either_time ~ condition * species + z_trial_consecutive + handseen + agentseen +
                            ((1 | participant_name) + (0 + condition_hand | participant_name) + (0 + condition_agent | participant_name) + (0 + z_trial_consecutive | participant_name)),
                          data = df_testreach,
                          family = Gamma(link = 'log'))
drops[['vt']] <- drop1(models[['vt']], test = 'Chisq')

models[['vt_noint']] <- glmmTMB(either_time ~ condition + species + z_trial_consecutive + handseen + agentseen + 
                                  ((1 | participant_name) + (0 + condition_hand | participant_name) + (0 + condition_agent | participant_name) +  (0 + z_trial_consecutive | participant_name)), #remove condition agent because of non-convergence
                                data = df_testreach, 
                                family = Gamma(link = 'log'))
drops[['vt_noint']] <- drop1(models[['vt_noint']], test = 'Chisq')




# first looks -------------------------------------------------------------
models[['fl']] <- glmmTMB(target_firsthit ~ condition * species + z_trial_consecutive + handseen + agentseen + 
                            ((1 | participant_name) + (0 + condition_hand | participant_name) + (0 + condition_agent | participant_name) + (0 + z_trial_consecutive | participant_name)), 
                          data = df_testreach, 
                          family = binomial(link = 'logit'))
drops[['fl']] <- drop1(models[['fl']], test = 'Chisq')

models[['fl_noint']] <- glmmTMB(target_firsthit ~ condition + species + z_trial_consecutive + handseen + agentseen + 
                                  ((1 | participant_name) + (0 + condition_hand | participant_name) + (0 + condition_agent | participant_name) + (0 + z_trial_consecutive | participant_name)),
                                data = df_testreach,
                                family = binomial(link = 'logit'))
drops[['fl_noint']] <- drop1(models[['fl_noint']], test = 'Chisq')




# saccades ----------------------------------------------------------------
models[['sc']] <- glmmTMB(latency_filtered ~ condition * species + z_trial_consecutive + handseen + agentseen + 
                            ((1 | participant_name) + (0 + condition_hand | participant_name) + (0 + condition_agent | participant_name) + (0 + z_trial_consecutive | participant_name)), 
                          data = df_hab, 
                          family = gaussian())
drops[['sc']] <- drop1(models[['sc']], test = 'Chisq')

models[['sc_noint']] <- glmmTMB(latency_filtered ~ condition + species + z_trial_consecutive + handseen + agentseen + 
                                  ((1 | participant_name) + (0 + condition_hand | participant_name) + (0 + condition_agent | participant_name) + (0 + z_trial_consecutive | participant_name)), 
                                data = df_hab, 
                                family = gaussian(link = 'identity'))
drops[['sc_noint']] <- drop1(models[['sc_noint']], test = 'Chisq')




# exploratory proportional looking score ----------------------------------------------------------------
models[['expl_pls']] <- glmmTMB(pls_transform ~ condition * exp * species + z_trial_within + 
                                  ((1 | participant_name) + (0 + cond_center | participant_name) + (0 + z_trial_within | participant_name)), 
                                dispformula = ~ 1 + either_time,
                                data = df_expl_subset,
                                family = beta_family(link = 'logit'))
drops[['expl_pls']] <- drop1(models[['expl_pls']], test = 'Chisq')


models[['expl_pls_1int']] <- glmmTMB(pls_transform ~ condition * exp + species + z_trial_within + 
                                       ((1 | participant_name) + (0 + cond_center | participant_name) + (0 + z_trial_within | participant_name)), 
                                     dispformula = ~ 1 + either_time,
                                     data = df_expl_subset, 
                                     family = beta_family(link = 'logit'))
drops[['expl_pls_1int']] <- drop1(models[['expl_pls_1int']], test = 'Chisq')


models[['expl_pls_noint']] <- glmmTMB(pls_transform ~ condition + exp + species + z_trial_within + 
                                        ((1 | participant_name) + (0 + cond_center | participant_name) + (0 + z_trial_within | participant_name)),
                                      dispformula = ~ 1 + either_time, 
                                      data = df_expl_subset, 
                                      family = beta_family(link = 'logit'))
drops[['expl_pls_noint']] <- drop1(models[['expl_pls_noint']], test = 'Chisq') 





# exploratory total viewing time ------------------------------------------------
models[['expl_vt']] <- glmmTMB(either_time ~ condition * exp * species + z_trial_within + 
                                 ((1 | participant_name) + (0 + cond_center | participant_name) + (0 + z_trial_within | participant_name)), 
                               data = df_expl_subset, 
                               family = Gamma(link = 'log'))
drops[['expl_vt']] <- drop1(models[['expl_vt']], test = 'Chisq')

models[['expl_vt_1int']] <- glmmTMB(either_time ~ condition * exp + species + z_trial_within + 
                                      ((1 | participant_name) + (0 + cond_center | participant_name) + (0 + z_trial_within | participant_name)),
                                    data = df_expl_subset, 
                                    family = Gamma(link = 'log'))
drops[['expl_vt_1int']] <- drop1(models[['expl_vt_1int']], test = 'Chisq')



# exploratory first looks -------------------------------------------------
models[['expl_fl']] <- glmmTMB(target_firsthit ~ condition * exp * species + z_trial_within +
                                 ((1 | participant_name) + (0 + cond_center | participant_name) + (0 + z_trial_within | participant_name)),
                               data = df_expl_subset,
                               family = binomial(link = 'logit'))
drops[['expl_fl']] <- drop1(models[['expl_fl']], test = 'Chisq')

models[['expl_fl_1int']] <- glmmTMB(target_firsthit ~ condition * exp + species + z_trial_within + 
                                      ((1 | participant_name) + (0 + cond_center | participant_name) + (0 + z_trial_within | participant_name)), 
                                    data = df_expl_subset, 
                                    family = binomial(link = 'logit'))
drops[['expl_fl_1int']] <- drop1(models[['expl_fl_1int']], test = 'Chisq') 

models[['expl_fl_noint']] <- glmmTMB(target_firsthit ~ condition + exp + species + z_trial_within + 
                                       ((1 | participant_name) + (0 + cond_center | participant_name) + (0 + z_trial_within | participant_name)), 
                                     data = df_expl_subset, 
                                     family = binomial(link = 'logit'))
drops[['expl_fl_noint']] <- drop1(models[['expl_fl_noint']], test = 'Chisq') 




# bootstrap ---------------------------------------------------------------
for (n in names(models)){
  boots[[n]] <- boot.glmmTMB(models[[n]], 
                             nboots=1000, 
                             para=T, 
                             n.cores="all-1", 
                             resol=1000, 
                             level=0.95,
                             data = get(models[[n]]$call$data))
}


# stability checks --------------------------------------------------------
stab <- list()

for (n in c('pls', 'vt', 'fl', 'sc', 'expl_pls', 'expl_vt', 'expl_fl')) {
  stab[[n]] <- glmmTMB.stab(models[[n]], data = get(models[[n]]$call$data), para = T)
}


# fitted ci ----------------------------------------------------------------------
get_ci <- function(model_name){
  m <- glmmTMB(reformulate(str_replace(as.character(models[[model_name]]$call$formula), 'species', 'species_orang + species_bonobo')[3], terms(models[[model_name]]$call$formula)[[2]]),
               dispformula = as.formula(models[[model_name]]$call$dispformula),
               family = eval(models[[model_name]]$call$family),
               data = get(models[[model_name]]$call$data))
  if (str_detect(model_name, 'expl')){
    uu = c('condition', 'exp')
  } else{uu = 'condition'}
  ci <- boot.glmmTMB(m, 
                     nboots=1000, 
                     para=T, 
                     n.cores="all-1",
                     resol=1000,
                     level=0.95, 
                     use = uu,
                     data = m$frame)
}



ci <- list()
for (n in c('pls_noint', 'vt_noint', 'fl_noint', 'sc_noint', 'expl_pls_noint', 'expl_vt_1int', 'expl_fl_noint')){
  print(n)
  now = Sys.time()
  ci[[n]] <- get_ci(n)
  print(Sys.time() - now)
}


# save model objects ------------------------------------------------------
m_obj <- list()
for (n in c('models', 'drops', 'boots', 'stab', 'ci')){
  m_obj[[n]] <- get(n)
}

write_rds(m_obj, file = 'output/model_object.rds')
