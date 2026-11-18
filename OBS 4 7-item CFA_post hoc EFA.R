#Ontario birth study (OBS) data
#EFA and CFA for 24 and 54mo ACEs
#Raykov's Reliability for CFA

library(dplyr)
library(semTools)
library(lavaan)
library(readr)
library(psych)
library(tidyr)
library(lavaan)
library(lavaan.mi)
library(purrr)
library(numDeriv)


#CFAs
# Extract all 20 completed datasets
ace_imp <- complete(covar_imp_2, action = "all")

# Check the number of imputations
length(ace_imp)
# Expected: 20

ace_vars <- c(
  "childhood_swear",
  "childhood_push",
  "childhood_touch",
  "childhood_divorce",
  "childhood_mother",
  "childhood_alcohol",
  "childhood_suicide",
  "childhood_prison"
)

# Extract ACE items from each imputed dataset
ace_items <- lapply(ace_imp, function(dat) {
  dat %>%
    select(all_of(ace_vars))
})

# Confirm there are 20 datasets
length(ace_items)

# Inspect the first dataset
head(ace_items[[1]])

# Check the dimensions of all 20 datasets
lapply(ace_items, dim)

setdiff(ace_vars, names(ace_imp[[1]]))
#should be character(0)

#cfa 1 factor model
cfa_1factor_24 <- '
  aces =~ childhood_swear +
          childhood_push +
          childhood_touch +
          childhood_mother +
          childhood_alcohol +
          childhood_suicide +
          childhood_prison
'

fit_1factor_24 <- cfa.mi(
  model = cfa_1factor_24,
  data = ace_items,
  ordered = TRUE, #for binary indicators
  std.lv = TRUE
)

summary(
  fit_1factor_24,
  standardized = TRUE,
  fit.measures = TRUE
)

pe_1factor_24 <- parameterEstimates.mi(
  fit_1factor_24,
  standardized = TRUE,
  ci = TRUE,
  fmi = TRUE
)

#for new package
summary(fit_1factor_24)
fitMeasures(fit_1factor_24)

# Extract factor loadings
loadings_1factor_24 <- pe_1factor_24 %>%
  filter(op == "=~") %>%
  select(
    lhs,
    rhs,
    est,
    se,
    ci.lower,
    ci.upper,
    std.all,
    pvalue,
    fmi
  )

loadings_1factor_24
#=========
#2 factor model CFA



cfa_2factor_24 <- '
  abuse =~ childhood_swear +
           childhood_push +
           childhood_touch +
           childhood_mother

  household_dys =~ childhood_alcohol +
                   childhood_suicide +
                   childhood_prison'

fit_2factor_24 <- cfa.mi(
  model = cfa_2factor_24,
  data = ace_items,
  ordered = TRUE, #for binary indicators
  std.lv = TRUE
)

#for new package
summary(fit_2factor_24)
fitMeasures(fit_2factor_24)

summary(
  fit_2factor_24,
  standardized = TRUE,
  fit.measures = TRUE
)

pe_2factor_24 <- parameterEstimates.mi(
  fit_2factor_24,
  standardized = TRUE,
  ci = TRUE,
  fmi = TRUE
)

# Extract factor loadings
loadings_2factor_24 <- pe_2factor_24 %>%
  filter(op == "=~") %>%
  select(
    lhs,
    rhs,
    est,
    se,
    ci.lower,
    ci.upper,
    std.all,
    pvalue,
    fmi
  )

loadings_2factor_24



#try with DWLS estimator
fit_2factor_24_dwls <- cfa.mi(
  model = cfa_2factor_24,
  data = ace_items,
  ordered = TRUE,
  estimator = "DWLS",
  std.lv = TRUE
)


summary(fit_2factor_24_dwls)
#looks similar to above, no heywood cases




####

#54 month
ace_imp_54 <- complete(covar_imp_54, action = "all")

length(ace_imp_54)
# Expected: 20

ace_vars_54 <- c(
  "childhood_swear",
  "childhood_push",
  "childhood_touch",
  "childhood_divorce",
  "childhood_mother",
  "childhood_alcohol",
  "childhood_suicide",
  "childhood_prison"
)

# Extract ACE items from each imputed dataset
ace_items_54 <- lapply(ace_imp_54, function(dat) {
  dat %>%
    select(all_of(ace_vars_54))
})

# Confirm there are 20 datasets
length(ace_items_54)

# Inspect the first dataset
head(ace_items_54[[1]])

# Check the dimensions of all 20 datasets
lapply(ace_items_54, dim)

setdiff(ace_vars_54, names(ace_imp_54[[1]]))
#should be character(0)

#cfa 1 factor model
cfa_1factor_54 <- '
  aces =~ childhood_swear +
          childhood_push +
          childhood_touch +
          childhood_mother +
          childhood_alcohol +
          childhood_suicide +
          childhood_prison
'

fit_1factor_54 <- cfa.mi(
  model = cfa_1factor_54,
  data = ace_items_54,
  ordered = TRUE, #for binary indicators
  std.lv = TRUE
)

#for new package
summary(fit_1factor_54)
fitMeasures(fit_1factor_54)

summary(
  fit_1factor_54,
  standardized = TRUE,
  fit.measures = TRUE
)

pe_1factor_54 <- parameterEstimates.mi(
  fit_1factor_54,
  standardized = TRUE,
  ci = TRUE,
  fmi = TRUE
)

# Extract factor loadings
loadings_1factor_54 <- pe_1factor_54 %>%
  filter(op == "=~") %>%
  select(
    lhs,
    rhs,
    est,
    se,
    ci.lower,
    ci.upper,
    std.all,
    pvalue,
    fmi
  )

loadings_1factor_54



#2 factor model
cfa_2factor_54 <- '
  abuse =~ childhood_swear +
           childhood_push +
           childhood_touch +
           childhood_mother

  household_dys =~ childhood_alcohol +
                   childhood_suicide +
                   childhood_prison'

fit_2factor_54 <- cfa.mi(
  model = cfa_2factor_54,
  data = ace_items_54,
  ordered = TRUE, #for binary indicators
  std.lv = TRUE
)

#for new package
summary(fit_2factor_54)
fitMeasures(fit_2factor_54)

summary(
  fit_2factor_54,
  standardized = TRUE,
  fit.measures = TRUE
)

pe_2factor_54 <- parameterEstimates.mi(
  fit_2factor_54,
  standardized = TRUE,
  ci = TRUE,
  fmi = TRUE
)

# Extract factor loadings
loadings_2factor_54 <- pe_2factor_54 %>%
  filter(op == "=~") %>%
  select(
    lhs,
    rhs,
    est,
    se,
    ci.lower,
    ci.upper,
    std.all,
    pvalue,
    fmi
  )

loadings_2factor_54

#=======================================================
#EFAs

ace_vars <- c(
  "childhood_swear",
  "childhood_push",
  "childhood_touch",
  "childhood_mother",
  "childhood_alcohol",
  "childhood_suicide",
  "childhood_divorce",
  "childhood_prison"
)

run_efa_mi <- function(data_list, nfactors){
  
  lapply(data_list, function(dat){
    
    R <- tetrachoric(
      dat[, ace_vars]
    )$rho
    
    fa(
      r = R,
      nfactors = nfactors,
      fm = "wls",
      rotate = "geominQ",
      n.obs = nrow(dat)
    )
    
  })
  
}

efa24_1 <- run_efa_mi(ace_items, 1)
efa24_2 <- run_efa_mi(ace_items, 2)
efa24_3 <- run_efa_mi(ace_items, 3)

efa54_1 <- run_efa_mi(ace_items_54, 1)
efa54_2 <- run_efa_mi(ace_items_54, 2)
efa54_3 <- run_efa_mi(ace_items_54, 3)


#extract loadings
summarize_loadings <- function(efa_list){
  
  loadings_long <- map_dfr(
    seq_along(efa_list),
    function(i){
      
      L <- as.data.frame(
        unclass(efa_list[[i]]$loadings)
      )
      
      L$item <- rownames(L)
      L$imp <- i
      
      L
      
    }
  )
  
  loadings_long |>
    group_by(item) |>
    summarise(
      across(
        where(is.numeric) & !matches("imp"),
        list(
          mean = mean,
          sd = sd,
          min = min,
          max = max
        ),
        .names = "{.col}_{.fn}"
      )
    )
  
}

load24_1 <- summarize_loadings(efa24_1)
load24_2 <- summarize_loadings(efa24_2)
load24_3 <- summarize_loadings(efa24_3)

load24_1
load24_2
load24_3

load54_1 <- summarize_loadings(efa54_1)
load54_2 <- summarize_loadings(efa54_2)
load54_3 <- summarize_loadings(efa54_3)

load54_1
load54_2
load54_3


#loading tables
make_loading_table <- function(efa_list){
  
  loadings_long <- map_dfr(
    seq_along(efa_list),
    function(i){
      
      L <- as.data.frame(
        unclass(efa_list[[i]]$loadings)
      )
      
      L$item <- rownames(L)
      
      L
      
    }
  )
  
  loadings_long |>
    group_by(item) |>
    summarise(
      across(
        where(is.numeric),
        mean
      ),
      .groups = "drop"
    )
  
}

table24_1 <- make_loading_table(efa24_1)
table24_2 <- make_loading_table(efa24_2)
table24_3 <- make_loading_table(efa24_3)


table54_1 <- make_loading_table(efa54_1)
table54_2 <- make_loading_table(efa54_2)
table54_3 <- make_loading_table(efa54_3)

table24_1
table24_2
table24_3

table54_1
table54_2
table54_3


#fit statisitics
extract_fit <- function(efa_list){
  
  tibble(
    
    RMSR = mean(
      sapply(efa_list, \(x) x$rms),
      na.rm = TRUE
    ),
    
    TLI = mean(
      sapply(efa_list, \(x) x$TLI),
      na.rm = TRUE
    ),
    
    RMSEA = mean(
      sapply(efa_list, \(x){
        
        if(!is.null(x$RMSEA))
          x$RMSEA[1]
        else
          NA
        
      }),
      na.rm = TRUE
    ),
    
    BIC = mean(
      sapply(efa_list, \(x) x$BIC),
      na.rm = TRUE
    )
    
  )
  
}

#comparison tables
#24
fit24 <- bind_rows(
  
  extract_fit(efa24_1),
  extract_fit(efa24_2),
  extract_fit(efa24_3)
  
) |>
  mutate(
    Factors = c(1,2,3),
    .before = 1
  )

#54
fit54 <- bind_rows(
  
  extract_fit(efa54_1),
  extract_fit(efa54_2),
  extract_fit(efa54_3)
  
) |>
  mutate(
    Factors = c(1,2,3),
    .before = 1
  )


fit24
fit54
#add variance explained below


#variance explained
variance_explained <- function(efa_list){
  
  mean(
    sapply(
      efa_list,
      function(x)
        sum(x$Vaccounted["Proportion Var", ])
    )
  )
  
}

fit24$Variance_Explained <- c(
  variance_explained(efa24_1),
  variance_explained(efa24_2),
  variance_explained(efa24_3)
)

fit54$Variance_Explained <- c(
  variance_explained(efa54_1),
  variance_explained(efa54_2),
  variance_explained(efa54_3)
)

fit24
fit54








#=========================================================
#parallel analyses & scree plots
fa.parallel(
  R_pool,
  n.obs = nrow(ace_items[[1]]),
  fa = "fa",
  fm = "wls",
  main = "24-Month ACEs: Parallel Analysis"
)


R_pool_54 <- Reduce("+", tet_list_54) / length(tet_list_54)

fa.parallel(
  R_pool_54,
  n.obs = nrow(ace_items_54[[1]]),
  fa = "fa",
  fm = "wls",
  main = "54-Month ACEs: Parallel Analysis"
)

#parallel analysis across all imputations
pa24 <- lapply(ace_items, function(dat){
  
  R <- tetrachoric(
    dat[, ace_vars]
  )$rho
  
  fa.parallel(
    R,
    n.obs = nrow(dat),
    fa = "fa",
    fm = "wls",
    plot = FALSE
  )
  
})

sapply(pa24, function(x) x$nfact)


#average eigenvalues across imputations for scree plots
eig24 <- sapply(ace_items, function(dat){
  
  R <- tetrachoric(
    dat[, ace_vars]
  )$rho
  
  eigen(R)$values
  
})

eig24_mean <- rowMeans(eig24)
eig24_mean

eig54 <- sapply(ace_items_54, function(dat){
  
  R <- tetrachoric(
    dat[, ace_vars]
  )$rho
  
  eigen(R)$values
  
})

eig54_mean <- rowMeans(eig54)
eig54_mean

#scree plot
plot(
  eig24_mean,
  type = "b",
  pch = 19,
  xlab = "Factor Number",
  ylab = "Eigenvalue",
  main = "24-Month ACE Scree Plot"
)

abline(h = 1, lty = 2)

plot(
  eig54_mean,
  type = "b",
  pch = 19,
  xlab = "Factor Number",
  ylab = "Eigenvalue",
  main = "54-Month ACE Scree Plot"
)

abline(h = 1, lty = 2)

#===============================================================================
#Raykov's Reliability

#CFA models
# 24-month: 1-factor
cfa_1factor_24 <- '
  aces =~ childhood_swear +
          childhood_push +
          childhood_touch +
          childhood_mother +
          childhood_alcohol +
          childhood_suicide +
          childhood_divorce +
          childhood_prison
'

# 24-month: 2-factor
cfa_2factor_24 <- '
  abuse =~ childhood_swear +
           childhood_push +
           childhood_touch +
           childhood_mother

  household_dys =~ childhood_alcohol +
                   childhood_suicide +
                   childhood_divorce +
                   childhood_prison
'

# 54-month: 1-factor
cfa_1factor_54 <- '
  aces =~ childhood_swear +
          childhood_push +
          childhood_touch +
          childhood_mother +
          childhood_alcohol +
          childhood_suicide +
          childhood_divorce +
          childhood_prison
'

# 54-month: 2-factor
cfa_2factor_54 <- '
  abuse =~ childhood_swear +
           childhood_push +
           childhood_touch +
           childhood_mother

  household_dys =~ childhood_alcohol +
                   childhood_suicide +
                   childhood_divorce +
                   childhood_prison
'


#Raykov's R function
raykov_function <- function(params, n_items) {
  
  loadings <- params[1:n_items]
  residuals <- params[(n_items + 1):(2 * n_items)]
  
  numerator <- sum(loadings)^2
  denominator <- numerator + sum(residuals)
  
  numerator / denominator
}


#single imputation test
raykov_one_imputation <- function(data, model, factor_name) {
  
  fit <- cfa(
    model = model,
    data = data,
    estimator = "MLR",
    std.lv = TRUE
  )
  
# Parameter table
  pe <- parameterEstimates(
    fit,
    standardized = FALSE
  )
  
# Identify indicators for factor
  loading_rows <- pe %>%
    filter(
      op == "=~",
      lhs == factor_name
    )
  
  indicators <- loading_rows$rhs
  
# Loadings
  loadings <- loading_rows$est
  
# Residual variances
  residual_rows <- pe %>%
    filter(
      op == "~~",
      lhs == rhs,
      lhs %in% indicators
    )
  
  residuals <- residual_rows$est
  
# Combined parameter vector
  params <- c(loadings, residuals)
  
  n_items <- length(loadings)
  
# Raykov rho
  rho <- raykov_function(
    params,
    n_items
  )
  
#approximate variance using parameter covariance matrix

# Get full parameter covariance matrix
  vcov_full <- lavInspect(fit, "vcov")
  
# Identify rows corresponding to loadings and residuals
  parameter_table <- parameterTable(fit)
  
  loading_indices <- which(
    parameter_table$op == "=~" &
      parameter_table$lhs == factor_name
  )
  
  residual_indices <- which(
    parameter_table$op == "~~" &
      parameter_table$lhs == parameter_table$rhs &
      parameter_table$lhs %in% indicators
  )
  
  indices <- c(
    loading_indices,
    residual_indices
  )
  
# Extract covariance matrix for relevant parameters
  vcov_rho <- vcov_full[indices, indices, drop = FALSE]
  
# Delta-method gradient
  gradient <- grad(
    func = function(x) {
      raykov_function(
        x,
        n_items
      )
    },
    x = params
  )
  
# Delta-method variance
  var_rho <- as.numeric(
    t(gradient) %*%
      vcov_rho %*%
      gradient
  )
  
  se_rho <- sqrt(var_rho)
  
  list(
    rho = rho,
    variance = var_rho,
    se = se_rho
  )
}


#pool Raykov's Reliability across imputations
pool_raykov <- function(data_list, model, factor_name) {
  
  results <- lapply(
    data_list,
    function(dat) {
      raykov_one_imputation(
        data = dat,
        model = model,
        factor_name = factor_name
      )
    }
  )
  
# Extract estimates
  rho <- sapply(results, `[[`, "rho")
  
# Within-imputation variances
  U <- sapply(results, `[[`, "variance")
  
  m <- length(rho)
  
# Pooled estimate
  rho_bar <- mean(rho)
  
# Within-imputation variance
  U_bar <- mean(U)
  
# Between-imputation variance
  B <- var(rho)
  
# Total variance
  T <- U_bar + (1 + 1/m) * B
  
# Pooled SE
  SE <- sqrt(T)
  
# Rubin's degrees of freedom
  df <- (m - 1) *
    (1 + U_bar / ((1 + 1/m) * B))^2
  
# 95% CI
  t_crit <- qt(
    0.975,
    df = df
  )
  
  CI_lower <- rho_bar - t_crit * SE
  CI_upper <- rho_bar + t_crit * SE
  
# Keep within theoretical range
  CI_lower <- max(0, CI_lower)
  CI_upper <- min(1, CI_upper)
  
  data.frame(
    Factor = factor_name,
    Raykov_rho = rho_bar,
    SE = SE,
    df = df,
    CI_lower = CI_lower,
    CI_upper = CI_upper,
    Within_variance = U_bar,
    Between_variance = B
  )
}

#run all 4 models
#24mo
raykov_1factor_24 <- pool_raykov(
  data_list = ace_items,
  model = cfa_1factor_24,
  factor_name = "aces"
)

raykov_abuse_24 <- pool_raykov(
  data_list = ace_items,
  model = cfa_2factor_24,
  factor_name = "abuse"
)

raykov_household_24 <- pool_raykov(
  data_list = ace_items,
  model = cfa_2factor_24,
  factor_name = "household_dys"
)

#54mo
raykov_1factor_54 <- pool_raykov(
  data_list = ace_items_54,
  model = cfa_1factor_54,
  factor_name = "aces"
)

raykov_abuse_54 <- pool_raykov(
  data_list = ace_items_54,
  model = cfa_2factor_54,
  factor_name = "abuse"
)

raykov_household_54 <- pool_raykov(
  data_list = ace_items_54,
  model = cfa_2factor_54,
  factor_name = "household_dys"
)


#combine results
raykov_results <- bind_rows(
  mutate(raykov_1factor_24, Age = "24 months", Model = "One-factor"),
  mutate(raykov_abuse_24, Age = "24 months", Model = "Two-factor"),
  mutate(raykov_household_24, Age = "24 months", Model = "Two-factor"),
  mutate(raykov_1factor_54, Age = "54 months", Model = "One-factor"),
  mutate(raykov_abuse_54, Age = "54 months", Model = "Two-factor"),
  mutate(raykov_household_54, Age = "54 months", Model = "Two-factor")
) %>%
  select(
    Age,
    Model,
    Factor,
    Raykov_rho,
    SE,
    df,
    CI_lower,
    CI_upper
  )

raykov_results
