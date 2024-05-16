####################################
####################################
############## S. maltophilia in water
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

s_maltophilia_water_glm <- glm(S_maltophilia ~ Treatment + Collect, 
                         data = mp_water, 
                         family = gaussian(link='identity'))
summary(s_maltophilia_water_glm)
# Call:
#   glm(formula = S_maltophilia ~ Treatment + Collect, family = gaussian(link = "identity"), 
#       data = mp_water)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3045869     723206   4.212 0.000473 ***
#   TreatmentTWW   274057    1071294   0.256 0.800840    
# Collect.L     6306483    1412965   4.463 0.000266 ***
#   Collect.Q     5414435    1232183   4.394 0.000312 ***
#   Collect.C     1854242    1019842   1.818 0.084841 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 6.312195e+12)
# 
# Null deviance: 2.9493e+14  on 23  degrees of freedom
# Residual deviance: 1.1993e+14  on 19  degrees of freedom
# (8 observations deleted due to missingness)
# AIC: 781.87
# 
# Number of Fisher Scoring iterations: 2

### step wise implementation
s_maltophilia_water_step <- step(s_maltophilia_water_glm, 
                           scope = list(lower = ~ 1, upper = ~ Treatment + Collect),
                           direction = "both")
summary(s_maltophilia_water_step)
# Call:
#   glm(formula = S_maltophilia ~ Collect, family = gaussian(link = "identity"), 
#       data = mp_water)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3148640     587144   5.363    3e-05 ***
#   Collect.L    6214561    1334204   4.658 0.000152 ***
#   Collect.Q    5345921    1174287   4.552 0.000194 ***
#   Collect.C    1823601     988838   1.844 0.080022 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 6.017239e+12)
# 
# Null deviance: 2.9493e+14  on 23  degrees of freedom
# Residual deviance: 1.2034e+14  on 20  degrees of freedom
# (8 observations deleted due to missingness)
# AIC: 779.95
# 
# Number of Fisher Scoring iterations: 2
