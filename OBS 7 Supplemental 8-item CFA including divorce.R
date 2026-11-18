#Ontario birth study (OBS) data
#FAs with divorce item included (8 items total)


#cfa 1 factor model
cfa_1factor_24 <- '
  aces =~ childhood_swear +
          childhood_push +
          childhood_touch +
          childhood_mother +
          childhood_alcohol +
          childhood_suicide +
          childhood_prison +
          childhood_divorce
'

fit_1factor_24 <- cfa.mi(
  model = cfa_1factor_24,
  data = ace_items,
  ordered = TRUE, #for binary indicators
  std.lv = TRUE
)


#for new package
summary(fit_1factor_24)
fitMeasures(fit_1factor_24)

#=========
#2 factor model CFA



cfa_2factor_24 <- '
  abuse =~ childhood_swear +
           childhood_push +
           childhood_touch +
           childhood_mother

  household_dys =~ childhood_alcohol +
                   childhood_suicide +
                   childhood_prison +
                   childhood_divorce
'

fit_2factor_24 <- cfa.mi(
  model = cfa_2factor_24,
  data = ace_items,
  ordered = TRUE, #for binary indicators
  std.lv = TRUE
)

#for new package
summary(fit_2factor_24)
fitMeasures(fit_2factor_24)


#################
#54 mo
#cfa 1 factor model
cfa_1factor_54 <- '
  aces =~ childhood_swear +
          childhood_push +
          childhood_touch +
          childhood_mother +
          childhood_alcohol +
          childhood_suicide +
          childhood_prison +
          childhood_divorce
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



#2 factor model
cfa_2factor_54 <- '
  abuse =~ childhood_swear +
           childhood_push +
           childhood_touch +
           childhood_mother

  household_dys =~ childhood_alcohol +
                   childhood_suicide +
                   childhood_prison +
                   childhood_divorce
'

fit_2factor_54 <- cfa.mi(
  model = cfa_2factor_54,
  data = ace_items_54,
  ordered = TRUE, #for binary indicators
  std.lv = TRUE
)