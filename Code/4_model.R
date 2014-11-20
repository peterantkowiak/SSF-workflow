##lmer


library(lme4)

# model 1

model1 = glmer(response ~ ruggedness + canopycover + (1|ID/strata), 
               data = cougars.final.DF,
               family = binomial)