####################################
####################################
############## int1 on microplastics
####################################
####################################

### full model fit
#View(mp_composites)
dim(mp_composites)
summary(mp_composites)
colnames(mp_composites)
mp_composites$Plastic_Glass <- as.factor(mp_composites$Plastic_Glass)
mp_composites$Collect <- factor(mp_composites$Collect,
                                levels = c("week0", "week2", "week6", "week10"),
                                ordered=TRUE)
mp_composites$Treatment <- as.factor(mp_composites$Treatment)

colnames(mp_composites)

# int1
mp_composites_glm_int1 <- glm(int1 ~ Plastic_Glass + Treatment + Collect, 
                              data = mp_composites, 
                              family = gaussian(link='log'))
summary(mp_composites_glm_int1)
# Call:
#   glm(formula = int1 ~ Plastic_Glass + Treatment + Collect, family = gaussian(link = "identity"), 
#       data = mp_composites)
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)  
# (Intercept)           5284     119030   0.044   0.9647  
# Plastic_GlassHDPE    -3960     115371  -0.034   0.9727  
# Plastic_GlassLDPE   278804     115371   2.417   0.0185 *
#   Plastic_GlassPP      39986     115371   0.347   0.7300  
# Plastic_GlassPS      26150     115371   0.227   0.8214  
# TreatmentFB        -223344     318330  -0.702   0.4854  
# TreatmentTWW        133899      80832   1.657   0.1024  
# Collect.L           -13835     222393  -0.062   0.9506  
# Collect.Q           281168     173056   1.625   0.1091  
# Collect.C               NA         NA      NA       NA  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 96184379685)
# 
# Null deviance: 8.4118e+12  on 73  degrees of freedom
# Residual deviance: 6.2520e+12  on 65  degrees of freedom
# (1 observation deleted due to missingness)
# AIC: 2091.8
# 
# Number of Fisher Scoring iterations: 2

# .L linear, .C cubic
# .Q represents the quadratic trend. The significant .Q term suggests a curvilinear, U-shaped relationship between 
# time and the S maltophilia levels.


### step wise implementation
mp_int1_step <- step(mp_composites_glm_int1, 
                     scope = list(lower = ~ 1, upper = ~ Plastic_Glass + Treatment + Collect),
                     direction = "both")
summary(mp_int1_step)
# Call:
#   glm(formula = int1 ~ Plastic_Glass + Treatment + Collect, family = gaussian(link = "identity"), 
#       data = mp_composites)
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)  
# (Intercept)           5284     119030   0.044   0.9647  
# Plastic_GlassHDPE    -3960     115371  -0.034   0.9727  
# Plastic_GlassLDPE   278804     115371   2.417   0.0185 *
#   Plastic_GlassPP      39986     115371   0.347   0.7300  
# Plastic_GlassPS      26150     115371   0.227   0.8214  
# TreatmentFB        -223344     318330  -0.702   0.4854  
# TreatmentTWW        133899      80832   1.657   0.1024  
# Collect.L           -13835     222393  -0.062   0.9506  
# Collect.Q           281168     173056   1.625   0.1091  
# Collect.C               NA         NA      NA       NA  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 96184379685)
# 
# Null deviance: 8.4118e+12  on 73  degrees of freedom
# Residual deviance: 6.2520e+12  on 65  degrees of freedom
# (1 observation deleted due to missingness)
# AIC: 2091.8
# 
# Number of Fisher Scoring iterations: 2

