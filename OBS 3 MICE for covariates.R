#Ontario birth study (OBS) data
#MICE for covariates + ACEs

library(mice)
library(dplyr)

#24mo
data_imp_24 <- subset(
  obs_24,
  select = c(
    id, kid_id,
    
    # covariates/predictors to impute
    Male, weight, GA, age, age_24mo,
    education, mother_born_not_Canada,
    hh_inc_lower150, race,
    
    childhood_swear, childhood_push,
    childhood_touch, childhood_divorce,
    childhood_mother, childhood_alcohol,
    childhood_suicide, childhood_prison,
    
    # outcomes (do not impute)
    Score_communication,
    Score_gross,
    Score_fine,
    Score_probsolve,
    Score_social,
    mchat
  )
)

meth_24 <- make.method(data_imp_24)
pred_24 <- make.predictorMatrix(data_imp_24)

# Do not impute identifiers
meth_24[c("id", "kid_id")] <- ""

# Do not impute outcomes
outcomes_24 <- c(
  "Score_communication",
  "Score_gross",
  "Score_fine",
  "Score_probsolve",
  "Score_social",
  "mchat"
)

meth_24[outcomes_24] <- ""

# Identifiers not used as predictors
pred_24[, c("id", "kid_id")] <- 0

# Outcomes can predict missing values in other variables
# but outcomes themselves are not imputed
pred_24[outcomes_24, ] <- 0

covar_imp_24 <- mice(
  data_imp_24,
  m = 20,
  maxit = 50,
  method = meth_24,
  predictorMatrix = pred_24,
  seed = 500
)

covar_list_24 <- complete(
  covar_imp_24,
  action = "all"
)

head(covar_list_24[[1]][, c("kid_id", "id", "age", "childhood_swear")])



#=====================================================
#54 months

library(mice)

data_imp_54 <- subset(
  obs_54,
  select = c(
    id, kid_id,
    
    # covariates/predictors to impute
    Male, weight, GA, age, age_54,
    education, mother_born_not_Canada,
    employ, hh_inc_lower150, race,
    
    childhood_swear, childhood_push,
    childhood_touch, childhood_divorce,
    childhood_mother, childhood_alcohol,
    childhood_suicide, childhood_prison,
    
    # outcomes (do not impute)
    vocab_agecorrected,
    flanker_agecorrected,
    DCCS_agecorrected,
    picturesequence_agecorrected,
    cognitioncomp_agecorrected,
    anxietysummary_fullycorrected,
    negpsychsummary_fullycorrected,
    wellbeingsummary_fullycorrected
  )
)

meth_54 <- make.method(data_imp_54)
pred_54 <- make.predictorMatrix(data_imp_54)

# Do not impute identifiers
meth_54[c("id", "kid_id")] <- ""

# Outcomes not imputed
outcomes_54 <- c(
  "vocab_agecorrected",
  "flanker_agecorrected",
  "DCCS_agecorrected",
  "picturesequence_agecorrected",
  "cognitioncomp_agecorrected",
  "anxietysummary_fullycorrected",
  "negpsychsummary_fullycorrected",
  "wellbeingsummary_fullycorrected"
)

meth_54[outcomes_54] <- ""

# Identifiers not used
pred_54[, c("id", "kid_id")] <- 0

# Outcomes predict others but are not imputed
pred_54[outcomes_54, ] <- 0

covar_imp_54 <- mice(
  data_imp_54,
  m = 20,
  maxit = 50,
  method = meth_54,
  predictorMatrix = pred_54,
  seed = 500
)

covar_list_54 <- complete(
  covar_imp_54,
  action = "all"
)

head(covar_list_54[[1]][, c("kid_id", "id", "age", "childhood_swear")])
