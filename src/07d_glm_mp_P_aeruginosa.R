####################################
####################################
#### P_aeruginosa on microplastics
####################################
####################################

### full model fit
mp_composites_glm_Pa <- glm(P_aeruginosa ~ Plastic_Glass + Treatment + Collect, 
                            data = mp_composites, 
                            family = gaussian(link='log'))
summary(mp_composites_glm_Pa)
# Call:
#   glm(formula = P_aeruginosa ~ Plastic_Glass + Treatment + Collect, 
#       family = gaussian(link = "identity"), data = mp_composites)
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)          596.7      642.5   0.929    0.356
# Plastic_GlassHDPE   -142.7      616.1  -0.232    0.818
# Plastic_GlassLDPE    894.6      616.1   1.452    0.151
# Plastic_GlassPP      883.2      616.1   1.433    0.156
# Plastic_GlassPS      345.7      616.1   0.561    0.577
# TreatmentFB        -1441.9     1715.2  -0.841    0.404
# TreatmentTWW        -247.6      435.7  -0.568    0.572
# Collect.L            459.6     1193.1   0.385    0.701
# Collect.Q           1516.7      924.2   1.641    0.106
# Collect.C               NA         NA      NA       NA
# 
# (Dispersion parameter for gaussian family taken to be 2847029)
# 
# Null deviance: 253877509  on 74  degrees of freedom
# Residual deviance: 187903936  on 66  degrees of freedom
# AIC: 1337.9
# 
# Number of Fisher Scoring iterations: 2

# .L linear, .C cubic
# .Q represents the quadratic trend. The significant .Q term suggests a curvilinear, U-shaped relationship between 
# time and the S maltophilia levels.


### step wise implementation
mp_Pa_step <- step(mp_composites_glm_Pa, 
                     scope = list(lower = ~ 1, upper = ~ Plastic_Glass + Treatment + Collect),
                     direction = "both")
summary(mp_Pa_step)
# Call:
#   glm(formula = P_aeruginosa ~ Collect, family = gaussian(link = "identity"), 
#       data = mp_composites)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)    539.6      196.7   2.743  0.00770 **
#   Collect.L     1343.9      405.3   3.316  0.00144 **
#   Collect.Q      857.7      393.4   2.180  0.03256 * 
#   Collect.C      294.7      381.1   0.773  0.44184   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 2857121)
# 
# Null deviance: 253877509  on 74  degrees of freedom
# Residual deviance: 202855600  on 71  degrees of freedom
# AIC: 1333.6
# 
# Number of Fisher Scoring iterations: 2