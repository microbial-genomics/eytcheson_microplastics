####################################
####################################
############## int1 in water
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

int1_water_glm <- glm(int1 ~ Treatment + Collect, 
                               data = mp_water, 
                               family = gaussian(link='log'))
summary(int1_water_glm)
# Call:
#   glm(formula = int1 ~ Treatment + Collect, family = gaussian(link = "identity"), 
#       data = mp_water)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   8696875    1089278   7.984 1.40e-08 ***
#   TreatmentTWW  4189375    1540472   2.720   0.0113 *  
#   Collect.L     7840772    1540472   5.090 2.39e-05 ***
#   Collect.Q      810938    1540472   0.526   0.6029    
# Collect.C     7474477    1540472   4.852 4.53e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 1.898443e+13)
# 
# Null deviance: 1.5970e+15  on 31  degrees of freedom
# Residual deviance: 5.1258e+14  on 27  degrees of freedom
# AIC: 1075.8
# 
# Number of Fisher Scoring iterations: 2

### step wise implementation
int1_water_step <- step(int1_water_glm, 
                                 scope = list(lower = ~ 1, upper = ~ Treatment + Collect),
                                 direction = "both")
summary(int1_water_step)
# Call:
#   glm(formula = int1 ~ Treatment + Collect, family = gaussian(link = "identity"), 
#       data = mp_water)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   8696875    1089278   7.984 1.40e-08 ***
#   TreatmentTWW  4189375    1540472   2.720   0.0113 *  
#   Collect.L     7840772    1540472   5.090 2.39e-05 ***
#   Collect.Q      810938    1540472   0.526   0.6029    
# Collect.C     7474477    1540472   4.852 4.53e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 1.898443e+13)
# 
# Null deviance: 1.5970e+15  on 31  degrees of freedom
# Residual deviance: 5.1258e+14  on 27  degrees of freedom
# AIC: 1075.8
# 
# Number of Fisher Scoring iterations: 2
