#Ontario birth study (OBS) data
#ORs for ASQ and MCHAT (24mo)

#0 = typically developing / 1 = at risk

#add to store all results 

all_results <- list(
  unadjusted_24 = list(),
  adjusted_24 = list(),
  unadjusted_54_cog = list(),
  adjusted_54_cog = list(),
  unadjusted_54_soc = list(),
  adjusted_54_soc = list(),
  unadjusted_24_or = list(),
  adjusted__24_or = list(),
)


#==========================================================

#lavaan estimates log ratios with
#OR = exp(est)
#CI_lower = exp(ci.lower)
#CI_upper = exp(ci.upper)


#unadjusted model
run_unadjusted_mi_24_or <- function(latent_model, outcome) {
  
  latent_name <- trimws(strsplit(latent_model, "=~")[[1]][1])
  
  model <- paste(
    latent_model,
    paste0(outcome, " ~ ", latent_name),
    sep = "\n"
  )
  
  fit <- sem.mi(
    model,
    data = analysis_list_or,
    std.lv = TRUE,
    link = "logit",
    ordered = outcome
  )
  
  out <- parameterEstimates.mi(
    fit,
    standardized = FALSE,
    ci = TRUE,
    fmi = TRUE
  )
  
  reg <- out[out$op == "~",
             c("lhs","op","rhs","est","ci.lower","ci.upper","se","pvalue")]
  
  reg$OR        <- exp(reg$est)
  reg$OR_lower  <- exp(reg$ci.lower)
  reg$OR_upper  <- exp(reg$ci.upper)
  
  reg
}




#adjusted model
run_adjusted_mi_24_or <- function(latent_model, outcome) {
  
  # Extract latent variable name cleanly
  latent_name <- trimws(strsplit(latent_model, "=~")[[1]][1])
  
  # 24-month covariates
  covars_24 <- "Male + weight + GA + age + age_24mo + race +
                mother_born_not_Canada + hh_inc_lower150 + education"
  
  # Build model string safely
  model <- paste(
    latent_model,
    paste0(outcome, " ~ ", latent_name, " + ", covars_24),
    sep = "\n"
  )
  
  # Fit pooled MI logistic regression
  fit <- sem.mi(
    model,
    data = analysis_list_or,
    std.lv = TRUE,
    link = "logit",
    ordered = c(ace_items, outcome),
    missing = "default"   # <-- crucial: override "ml"
  )
  
  # Extract pooled results
  out <- parameterEstimates.mi(
    fit,
    standardized = FALSE,
    ci = TRUE,
    fmi = TRUE
  )
  
  reg <- out[out$op == "~",
             c("lhs","op","rhs","est","ci.lower","ci.upper","se","pvalue")]
  
  reg$OR       <- exp(reg$est)
  reg$OR_lower <- exp(reg$ci.lower)
  reg$OR_upper <- exp(reg$ci.upper)
  
  reg
}



#24mo outcomes
outcomes_24_or <- c(
  "comm_risk", "fine_risk","gross_risk",
  "probsolve_risk", "social_risk", "autism_risk"
)


#run
all_results$unadjusted_24_or$aces <- lapply(outcomes_24_or,function(out) run_unadjusted_mi_24_or(ace_model, out))
all_results$unadjusted_24_or$abuse <- lapply(outcomes_24_or, function(out) run_unadjusted_mi_24_or(abuse_model, out))
all_results$unadjusted_24_or$hd <- lapply(outcomes_24_or,function(out) run_unadjusted_mi_24_or(hd_model, out))
all_results$unadjusted_24_or$hd_div <- lapply(outcomes_24_or,function(out) run_unadjusted_mi_24_or(hd_div_model, out))

all_results$adjusted_24_or$aces <- lapply(outcomes_24_or, function (out) run_adjusted_mi_24_or(ace_model, out))
all_results$adjusted_24_or$abuse <- lapply(outcomes_24_or,function(out) run_adjusted_mi_24_or(abuse_model, out))
all_results$adjusted_24_or$hd <- lapply(outcomes_24_or,function(out) run_adjusted_mi_24_or(hd_model, out))
all_results$adjusted_24_or$hd_div <- lapply(outcomes_24_or,function(out) run_adjusted_mi_24_or(hd_div_model, out))

#aces no div models
all_results$unadjusted_24_or$aces_nodiv <- lapply(outcomes_24_or,function(out) run_unadjusted_mi_24_or(ace_nodiv_model, out))
all_results$adjusted_24_or$aces_nodiv <- lapply(outcomes_24_or, function (out) run_adjusted_mi_24_or(ace_nodiv_model, out))


adjusted_24_or_ace_nodiv <- lapply(outcomes_24_or,function(outcome) {run_adjusted_mi_24_or(ace_nodiv_model,outcome)})
adjusted_24_or_abuse <- lapply(outcomes_24_or,function(outcome) {run_adjusted_mi_24_or(abuse_model,outcome)})
adjusted_24_or_hd <- lapply(outcomes_24_or,function(outcome) {run_adjusted_mi_24_or(hd_model,outcome)})
adjusted_24_or_aces <- lapply(outcomes_24_or,function(outcome) {run_adjusted_mi_24_or(ace_model,outcome)})
adjusted_24_or_hd_div <- lapply(outcomes_24_or,function(outcome) {run_adjusted_mi_24_or(hd_div_model,outcome)})

