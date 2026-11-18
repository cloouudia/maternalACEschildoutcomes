#Ontario birth study (OBS) data
#regressions

library(lavaan.mi)

#=======================
#adjusted models
#ADD covariates
#24mo: + Male + weight + GA + age + age_24mo + race + mother_born_not_Canada + employ + hh_inc_lower150 + education
#54mo: + Male + weight + GA + age + age_54 + race + mother_born_not_Canada + employ + hh_inc_lower150 + education
#=======================

#to store all results for tables later
all_results <- list(
  unadjusted_24 = list(),
  adjusted_24 = list(),
  unadjusted_54_cog = list(),
  adjusted_54_cog = list(),
  unadjusted_54_soc = list(),
  adjusted_54_soc = list()
)


#==========================================================
##all other models below

#define predictor models

# ACEs factor
ace_model <- '
  aces =~ childhood_swear + childhood_push + childhood_touch + childhood_mother +
          childhood_divorce + childhood_alcohol + childhood_suicide + childhood_prison
'

# Abuse factor
abuse_model <- '
  abuse =~ childhood_swear + childhood_push + childhood_touch + childhood_mother
'

# Household dysfunction factor
hd_model <- '
  hd =~ childhood_alcohol + childhood_suicide + childhood_prison
'

#HD with divorce**
hd_div_model <- '
  hd_divorce =~ childhood_alcohol + childhood_suicide + childhood_prison + childhood_divorce
'
#aces WITHOUT divorce
ace_nodiv_model <- '
  aces_nodiv =~ childhood_swear + childhood_push + childhood_touch + childhood_mother +
          childhood_alcohol + childhood_suicide + childhood_prison
'


#24months

#unadjusted model template
run_unadjusted_mi_24 <- function(latent_model, outcome) {
  
  latent_name <- trimws(strsplit(latent_model, "=~")[[1]][1])
  
  model <- paste(
    latent_model,
    paste0(outcome, " ~ ", latent_name),
    sep = "\n"
  )
  
  fit <- sem.mi(
    model,
    data = covar_list_24,
    missing = "fiml",
    estimator = "MLR",
    std.lv = TRUE
  )
  
  out <- parameterEstimates.mi(
    fit,
    standardized = TRUE,
    ci = TRUE,
    fmi = TRUE
  )
  
  out[out$op == "~", c("lhs","op","rhs","est","ci.lower","ci.upper","std.all","se","pvalue")]
}


#adjusted template
run_adjusted_mi_24 <- function(latent_model, outcome) {
  
  # Extract latent variable name cleanly (removes leading/trailing whitespace)
  latent_name <- trimws(strsplit(latent_model, "=~")[[1]][1])
  
  # 24-month covariates
  covars_24 <- "Male + weight + GA + age + age_24mo + race +
                mother_born_not_Canada + hh_inc_lower150 + education"
  
  # Build model string safely (avoids stray whitespace/newlines)
  model <- paste(
    latent_model,
    paste0(outcome, " ~ ", latent_name, " + ", covars_24),
    sep = "\n"
  )
  
  # Fit pooled MI model
  fit <- sem.mi(
    model,
    data = covar_list_24, #analysis_list_24
    missing = "fiml",
    estimator = "MLR",
    std.lv = TRUE
  )
  
  # Extract pooled regression results
  out <- parameterEstimates.mi(
    fit,
    standardized = TRUE,
    ci = TRUE,
    fmi = TRUE
  )
  
  # Return regression paths only
  out[out$op == "~",
      c("lhs","op","rhs","est","ci.lower","ci.upper","std.all","se","pvalue")]
}

#24 month outcomes
outcomes_24 <- c(
  "Score_communication", "Score_gross", "Score_fine",
  "Score_probsolve", "Score_social"
)


#run unadjusted models
all_results$unadjusted_24$aces <- lapply(outcomes_24, function(out) run_unadjusted_mi_24(ace_model, out))
all_results$unadjusted_24$abuse <- lapply(outcomes_24, function(out) run_unadjusted_mi_24(abuse_model, out))
all_results$unadjusted_24$hd <- lapply(outcomes_24, function(out) run_unadjusted_mi_24(hd_model, out))
all_results$unadjusted_24$hd_div <- lapply(outcomes_24, function(out) run_unadjusted_mi_24(hd_div_model, out))

#adjusted
all_results$adjusted_24$aces <- lapply(outcomes_24, function(out) run_adjusted_mi_24(ace_model, out))
all_results$adjusted_24$abuse <- lapply(outcomes_24, function(out) run_adjusted_mi_24(abuse_model, out))
all_results$adjusted_24$hd <- lapply(outcomes_24, function(out) run_adjusted_mi_24(hd_model, out))
all_results$adjusted_24$hd_div <- lapply(outcomes_24, function(out) run_adjusted_mi_24(hd_div_model, out))


#aces no divorce
all_results$unadjusted_24$aces_nodiv <- lapply(outcomes_24, function(out) run_unadjusted_mi_24(ace_nodiv_model, out))
all_results$adjusted_24$aces_nodiv <- lapply(outcomes_24, function(out) run_adjusted_mi_24(ace_nodiv_model, out))


#===============================================================================
#54 months
#unadjusted model template (same as above, adj is dif covars)

run_unadjusted_mi_54 <- function(latent_model, outcome) {
  
  latent_name <- trimws(strsplit(latent_model, "=~")[[1]][1])
  
  model <- paste(
    latent_model,
    paste0(outcome, " ~ ", latent_name),
    sep = "\n"
  )
  
  fit <- sem.mi(
    model,
    data = covar_list_54,
    missing = "fiml",
    estimator = "MLR",
    std.lv = TRUE
  )
  
  out <- parameterEstimates.mi(
    fit,
    standardized = TRUE,
    ci = TRUE,
    fmi = TRUE
  )
  
  out[out$op == "~", c("lhs","op","rhs","est","ci.lower","ci.upper","std.all","se","pvalue")]
}


#adjusted template
run_adjusted_mi_54 <- function(latent_model, outcome) {
  
  # Extract latent variable name cleanly (removes leading/trailing whitespace)
  latent_name <- trimws(strsplit(latent_model, "=~")[[1]][1])
  
  # 54-month covariates
  covars_54 <- "Male + weight + GA + age + age_54 + race +
                mother_born_not_Canada + hh_inc_lower150 + education"
  
  # Build model string safely (avoids stray whitespace/newlines)
  model <- paste(
    latent_model,
    paste0(outcome, " ~ ", latent_name, " + ", covars_54),
    sep = "\n"
  )
  
  # Fit pooled MI model
  fit <- sem.mi(
    model,
    data = covar_list_54,
    missing = "fiml",
    estimator = "MLR",
    std.lv = TRUE
  )
  
  # Extract pooled regression results
  out <- parameterEstimates.mi(
    fit,
    standardized = TRUE,
    ci = TRUE,
    fmi = TRUE
  )
  
  # Return regression paths only
  out[out$op == "~",
      c("lhs","op","rhs","est","ci.lower","ci.upper","std.all","se","pvalue")]
}


#54 month outcomes
outcomes_54_cog <- c(
  "vocab_agecorrected", "flanker_agecorrected",
  "DCCS_agecorrected", "picturesequence_agecorrected",
  "cognitioncomp_agecorrected"
)

outcomes_54_soc <- c(
  "anxietysummary_fullycorrected",
  "negpsychsummary_fullycorrected",
  "wellbeingsummary_fullycorrected"
)


#run unadjusted + adjusted models
all_results$unadjusted_54_cog$aces <- lapply(outcomes_54_cog, function(out) run_unadjusted_mi_54(ace_model, out))
all_results$unadjusted_54_cog$abuse <- lapply(outcomes_54_cog, function(out) run_unadjusted_mi_54(abuse_model, out))
all_results$unadjusted_54_cog$hd <- lapply(outcomes_54_cog, function(out) run_unadjusted_mi_54(hd_model, out))
all_results$unadjusted_54_cog$hd_div <- lapply(outcomes_54_cog, function(out) run_unadjusted_mi_54(hd_div_model, out))

all_results$adjusted_54_cog$aces <- lapply(outcomes_54_cog, function(out) run_adjusted_mi_54(ace_model, out))
all_results$adjusted_54_cog$abuse <- lapply(outcomes_54_cog, function(out) run_adjusted_mi_54(abuse_model, out))
all_results$adjusted_54_cog$hd <- lapply(outcomes_54_cog, function(out) run_adjusted_mi_54(hd_model, out))
all_results$adjusted_54_cog$hd_div <- lapply(outcomes_54_cog, function(out) run_adjusted_mi_54(hd_div_model, out))

all_results$unadjusted_54_soc$aces <- lapply(outcomes_54_soc, function(out) run_unadjusted_mi_54(ace_model, out))
all_results$unadjusted_54_soc$abuse <- lapply(outcomes_54_soc, function(out) run_unadjusted_mi_54(abuse_model, out))
all_results$unadjusted_54_soc$hd <- lapply(outcomes_54_soc, function(out) run_unadjusted_mi_54(hd_model, out))
all_results$unadjusted_54_soc$hd_div <- lapply(outcomes_54_soc, function(out) run_unadjusted_mi_54(hd_div_model, out))

all_results$adjusted_54_soc$aces <- lapply(outcomes_54_soc, function(out) run_adjusted_mi_54(ace_model, out))
all_results$adjusted_54_soc$abuse <- lapply(outcomes_54_soc, function(out) run_adjusted_mi_54(abuse_model, out))
all_results$adjusted_54_soc$hd <- lapply(outcomes_54_soc, function(out) run_adjusted_mi_54(hd_model, out))
all_results$adjusted_54_soc$hd_div <- lapply(outcomes_54_soc, function(out) run_adjusted_mi_54(hd_div_model, out))

#aces no divorce
all_results$unadjusted_54_cog$aces_nodiv <- lapply(outcomes_54_cog, function(out) run_unadjusted_mi_54(ace_nodiv_model, out))
all_results$unadjusted_54_soc$aces_nodiv <- lapply(outcomes_54_soc, function(out) run_unadjusted_mi_54(ace_nodiv_model, out))

all_results$adjusted_54_cog$aces_nodiv <- lapply(outcomes_54_cog, function(out) run_adjusted_mi_54(ace_nodiv_model, out))
all_results$adjusted_54_soc$aces_nodiv <- lapply(outcomes_54_soc, function(out) run_adjusted_mi_54(ace_nodiv_model, out))


