####################################
####################################
############## sul1 on microplastics
####################################
####################################

### full model fit
# sul1
mp_composites_glm_sul1 <- glm(sul1 ~ Plastic_Glass + Treatment + Collect, 
                              data = mp_composites, 
                              family = gaussian(link='identity'))
summary(mp_composites_glm_sul1)
# Call:
#   glm(formula = sul1 ~ Plastic_Glass + Treatment + Collect, family = gaussian(link = "identity"), 
#       data = mp_composites)
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)           7119      15874   0.448  0.65528   
# Plastic_GlassHDPE    -2540      15222  -0.167  0.86801   
# Plastic_GlassLDPE    44859      15222   2.947  0.00443 **
#   Plastic_GlassPP       7012      15222   0.461  0.64655   
# Plastic_GlassPS      13853      15222   0.910  0.36611   
# TreatmentFB         -60409      42377  -1.425  0.15873   
# TreatmentTWW         25916      10764   2.408  0.01886 * 
#   Collect.L           -21312      29478  -0.723  0.47225   
# Collect.Q            52714      22834   2.309  0.02410 * 
#   Collect.C               NA         NA      NA       NA   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 1737903461)
# 
# Null deviance: 1.7169e+11  on 74  degrees of freedom
# Residual deviance: 1.1470e+11  on 66  degrees of freedom
# AIC: 1818.9
# 
# Number of Fisher Scoring iterations: 2

# .L linear, .C cubic
# .Q represents the quadratic trend. The significant .Q term suggests a curvilinear, U-shaped relationship between 
# time and the S maltophilia levels.


### step wise implementation
mp_sul1_step <- step(mp_composites_glm_sul1, 
                     scope = list(lower = ~ 1, upper = ~ Plastic_Glass + Treatment + Collect),
                     direction = "both")
summary(mp_sul1_step)
# Call:
#   glm(formula = sul1 ~ Plastic_Glass + Treatment + Collect, family = gaussian(link = "identity"), 
#       data = mp_composites)
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)           7119      15874   0.448  0.65528   
# Plastic_GlassHDPE    -2540      15222  -0.167  0.86801   
# Plastic_GlassLDPE    44859      15222   2.947  0.00443 **
#   Plastic_GlassPP       7012      15222   0.461  0.64655   
# Plastic_GlassPS      13853      15222   0.910  0.36611   
# TreatmentFB         -60409      42377  -1.425  0.15873   
# TreatmentTWW         25916      10764   2.408  0.01886 * 
#   Collect.L           -21312      29478  -0.723  0.47225   
# Collect.Q            52714      22834   2.309  0.02410 * 
#   Collect.C               NA         NA      NA       NA   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 1737903461)
# 
# Null deviance: 1.7169e+11  on 74  degrees of freedom
# Residual deviance: 1.1470e+11  on 66  degrees of freedom
# AIC: 1818.9
# 
# Number of Fisher Scoring iterations: 2
