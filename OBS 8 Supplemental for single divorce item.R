#Ontario birth study (OBS) data
#Supplemental for *single divorce item* analyses

library(mice)
library(dplyr)


#divorce item with 24 month outcomes
outcomes_24 <- c(
  "Score_communication",
  "Score_gross",
  "Score_fine",
  "Score_probsolve",
  "Score_social"
)

covars_24 <- c(
  "Male",
  "weight",
  "GA",
  "age",
  "age_24mo",
  "race",
  "mother_born_not_Canada",
  "employ",
  "hh_inc_lower150",
  "education"
)

run_pooled_lm <- function(data_list,
                          outcome,
                          predictor = "childhood_divorce",
                          covariates = NULL) {
  
  if (is.null(covariates)) {
    
    formula <- as.formula(
      paste(outcome, "~", predictor)
    )
    
  } else {
    
    formula <- as.formula(
      paste(
        outcome,
        "~",
        paste(c(predictor, covariates), collapse = " + ")
      )
    )
  }
  
fits <- lapply(data_list, function(dat) {
    
    lm(
      formula = formula,
      data = dat
    )
    
  })

mira_fit <- as.mira(fits)

pooled_fit <- pool(mira_fit)
  
pooled_results <- summary(
    pooled_fit,
    conf.int = TRUE,
    conf.level = 0.95
  )
  
  
#keep childhood divorce item only
  pooled_results <- pooled_results %>%
    filter(term == predictor) %>%
    mutate(
      outcome = outcome,
      model = ifelse(
        is.null(covariates),
        "Unadjusted",
        "Adjusted"
      )
    ) %>%
    select(
      outcome,
      model,
      term,
      estimate,
      std.error,
      statistic,
      df,
      p.value,
      `2.5 %`,
      `97.5 %`
    )
  
  return(pooled_results)
}


#unadjusted models
results_24_unadjusted <- map_dfr(
  outcomes_24,
  ~ run_pooled_lm(
    data_list = data_list,
    outcome = .x,
    predictor = "childhood_divorce",
    covariates = NULL
  )
)


#adjusted models
results_24_adjusted <- map_dfr(
  outcomes_24,
  ~ run_pooled_lm(
    data_list = data_list,
    outcome = .x,
    predictor = "childhood_divorce",
    covariates = covars_24
  )
)

#54mo outcomes
outcomes_54_soc <- c(
  "anxietysummary_fullycorrected",
  "negpsychsummary_fullycorrected",
  "wellbeingsummary_fullycorrected"
)

covars_54 <- c(
  "Male",
  "weight",
  "GA",
  "age",
  "age_54",
  "race",
  "mother_born_not_Canada",
  "employ",
  "hh_inc_lower150",
  "education"
)


run_pooled_lm <- function(data_list,
                          outcome,
                          predictor = "childhood_divorce",
                          covariates = NULL) {
  
  if (is.null(covariates)) {
    
    formula <- as.formula(
      paste(outcome, "~", predictor)
    )
    
  } else {
    
    formula <- as.formula(
      paste(
        outcome,
        "~",
        paste(c(predictor, covariates), collapse = " + ")
      )
    )
  }
  
  fits <- lapply(data_list, function(dat) {
    
    lm(
      formula = formula,
      data = dat
    )
    
  })
  
  mira_fit <- as.mira(fits)
  
  pooled_fit <- pool(mira_fit)
  
  pooled_results <- summary(
    pooled_fit,
    conf.int = TRUE,
    conf.level = 0.95
  )
  
  
  #keep childhood divorce item only
  pooled_results <- pooled_results %>%
    filter(term == predictor) %>%
    mutate(
      outcome = outcome,
      model = ifelse(
        is.null(covariates),
        "Unadjusted",
        "Adjusted"
      )
    ) %>%
    select(
      outcome,
      model,
      term,
      estimate,
      std.error,
      statistic,
      df,
      p.value,
      `2.5 %`,
      `97.5 %`
    )
  
  return(pooled_results)
}


results_54_soc_unadjusted <- map_dfr(
  outcomes_54_soc,
  ~ run_pooled_lm(
    data_list = data_list,
    outcome = .x,
    predictor = "childhood_divorce",
    covariates = NULL
  )
)

results_54_soc_adjusted <- map_dfr(
  outcomes_54_soc,
  ~ run_pooled_lm(
    data_list = data_list,
    outcome = .x,
    predictor = "childhood_divorce",
    covariates = covars_54
  )
)


results_54_soc_display <- results_54_soc %>%
  mutate(
    B = estimate,
    
    CI_low = `2.5 %`,
    CI_high = `97.5 %`,
    
    `B [95% CI]` = paste0(
      sprintf("%.2f", B),
      " [",
      sprintf("%.2f", CI_low),
      ", ",
      sprintf("%.2f", CI_high),
      "]"
    ),
    
    `β` = round(estimate, 3),
    
    SE = round(std.error, 3),
    
    p = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p.value)
    )
  )

#autism (MCHAT) OR
data_list <- analysis_list_24_or

outcome <- "autism_risk"

#run_pooled_logistic from earlier

pooled_results <- pooled_results %>%
  filter(term == predictor) %>%
  mutate(
    outcome = outcome,
    model = ifelse(
      is.null(covariates),
      "Unadjusted",
      "Adjusted"
    ),
    
    # Logistic regression coefficient
    beta = estimate,
    
    # Convert log-odds coefficient to OR
    OR = exp(estimate),
    
    # Convert CI from log-odds to OR scale
    OR_low = exp(`2.5 %`),
    OR_high = exp(`97.5 %`)
  ) %>%
  select(
    outcome,
    model,
    term,
    beta,
    OR,
    OR_low,
    OR_high,
    std.error,
    statistic,
    df,
    p.value
  )

return(pooled_results)
}

results_autism_unadjusted <- run_pooled_logistic(
  data_list = data_list,
  outcome = outcome,
  predictor = "childhood_divorce",
  covariates = NULL
)

results_autism_display <- results_autism %>%
  mutate(
    
    `OR [95% CI]` = paste0(
      sprintf("%.2f", OR),
      " [",
      sprintf("%.2f", OR_low),
      ", ",
      sprintf("%.2f", OR_high),
      "]"
    ),
    
    `β` = round(beta, 3),
    
    SE = round(std.error, 3),
    
    p = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p.value)
    )
  )

results_autism_report

