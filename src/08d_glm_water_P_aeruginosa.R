####################################
####################################
############## P aeruginosa in water
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

P_aeruginosa_water_glm <- glm(P_aeruginosa ~ Treatment + Collect, 
                      data = mp_water, 
                      family = gaussian(link='log'))
summary(P_aeruginosa_water_glm)
# Call:
#   glm(formula = P_aeruginosa ~ Treatment + Collect, family = gaussian(link = "identity"), 
#       data = mp_water)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     14362       3483   4.124 0.000319 ***
#   TreatmentTWW    -5068       4926  -1.029 0.312665    
# Collect.L        6651       4926   1.350 0.188144    
# Collect.Q        5055       4926   1.026 0.313866    
# Collect.C       -6698       4926  -1.360 0.185133    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 194096183)
# 
# Null deviance: 6363283547  on 31  degrees of freedom
# Residual deviance: 5240596932  on 27  degrees of freedom
# AIC: 708.06
# 
# Number of Fisher Scoring iterations: 2

### step wise implementation
step(P_aeruginosa_water_glm, 
     scope = list(lower = ~ 1, upper = ~ Treatment + Collect),
     direction = "both")

P_aeruginosa_water_step <- step(P_aeruginosa_water_glm, 
                        scope = list(lower = ~ 1, upper = ~ Treatment + Collect),
                        direction = "both")
summary(P_aeruginosa_water_step)
# Call:
#   glm(formula = P_aeruginosa ~ 1, family = gaussian(link = "identity"), 
#       data = mp_water)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    11828       2533    4.67 5.52e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 205267211)
# 
# Null deviance: 6363283547  on 31  degrees of freedom
# Residual deviance: 6363283547  on 31  degrees of freedom
# AIC: 706.27
# 
# Number of Fisher Scoring iterations: 2
