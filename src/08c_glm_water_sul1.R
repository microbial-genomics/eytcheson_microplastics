####################################
####################################
############## sul1 in water
####################################
####################################

#View(mp_water)
dim(mp_water)
summary(mp_water)
colnames(mp_water)
mp_water$Collect <- factor(mp_water$Collect,
                           levels = c("week0", "week2", "week6", "week10"),
                           ordered=TRUE)
mp_water$Treatment <- as.factor(mp_water$Treatment)

sul1_water_glm <- glm(sul1 ~ Treatment + Collect, 
                      data = mp_water, 
                      family = gaussian(link='identity'))
summary(sul1_water_glm)
# Call:
#   glm(formula = sul1 ~ Treatment + Collect, family = gaussian(link = "identity"), 
#       data = mp_water)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    852281     174786   4.876 4.25e-05 ***
#   TreatmentTWW   961297     247185   3.889 0.000593 ***
#   Collect.L     1681921     247185   6.804 2.62e-07 ***
#   Collect.Q      822078     247185   3.326 0.002549 ** 
#   Collect.C      814551     247185   3.295 0.002753 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 488803482856)
# 
# Null deviance: 5.3936e+13  on 31  degrees of freedom
# Residual deviance: 1.3198e+13  on 27  degrees of freedom
# AIC: 958.66
# 
# Number of Fisher Scoring iterations: 2

### step wise implementation
step(sul1_water_glm, 
     scope = list(lower = ~ 1, upper = ~ Treatment + Collect),
     direction = "both")

sul1_water_step <- step(sul1_water_glm, 
                        scope = list(lower = ~ 1, upper = ~ Treatment + Collect),
                        direction = "both")
summary(sul1_water_step)
# Call:
#   glm(formula = sul1 ~ Treatment + Collect, family = gaussian(link = "identity"), 
#       data = mp_water)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    852281     174786   4.876 4.25e-05 ***
#   TreatmentTWW   961297     247185   3.889 0.000593 ***
#   Collect.L     1681921     247185   6.804 2.62e-07 ***
#   Collect.Q      822078     247185   3.326 0.002549 ** 
#   Collect.C      814551     247185   3.295 0.002753 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 488803482856)
# 
# Null deviance: 5.3936e+13  on 31  degrees of freedom
# Residual deviance: 1.3198e+13  on 27  degrees of freedom
# AIC: 958.66
# 
# Number of Fisher Scoring iterations: 2
