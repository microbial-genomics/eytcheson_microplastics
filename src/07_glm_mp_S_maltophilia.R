####################################
####################################
############## S. maltophilia on microplastics
####################################
####################################

#View(s_maltophilia)
dim(s_maltophilia)
summary(s_maltophilia)
colnames(s_maltophilia)
s_maltophilia$Plastic_Glass <- as.factor(s_maltophilia$Plastic_Glass)
s_maltophilia$Collect <- factor(s_maltophilia$Collect,
                                   levels = c("week0", "week2", "week6", "week10"),
                                   ordered=TRUE)
s_maltophilia$Treatment <- as.factor(s_maltophilia$Treatment)

s_maltophilia_glm <- glm(S_maltophilia ~ Plastic_Glass + Treatment + Collect, 
                         data = s_maltophilia, 
                         family = gaussian(link='identity'))
summary(s_maltophilia_glm)

#Call:
#  glm(formula = S_maltophilia ~ Plastic_Glass + Treatment + Collect, 
#      family = gaussian(link = "identity"), data = s_maltophilia)#
#
#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)   
#(Intercept)       -14058.9    11970.3  -1.174  0.24288   
#Plastic_GlassHDPE   -691.5     5731.4  -0.121  0.90420   
#Plastic_GlassLDPE  11307.6     5604.9   2.017  0.04622 * 
#  Plastic_GlassPP     4515.9     5728.0   0.788  0.43225   
#Plastic_GlassPS     1144.8     5731.4   0.200  0.84207   
#TreatmentRiver     19952.0    14947.9   1.335  0.18487   
#TreatmentTWW       24692.4    15190.6   1.626  0.10708   
#Collect.L            428.5    10106.1   0.042  0.96626   
#Collect.Q          22189.7     8253.5   2.689  0.00836 **
#  Collect.C               NA         NA      NA       NA   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 368148405)#
#
#Null deviance: 5.1049e+10  on 112  degrees of freedom
#Residual deviance: 3.8287e+10  on 104  degrees of freedom
#AIC: 2560.1#
#
#Number of Fisher Scoring iterations: 2

# .L linear, .C cubic
# .Q represents the quadratic trend. The significant .Q term suggests a curvilinear, U-shaped relationship between 
# time and the S maltophilia levels.

s_maltophilia_step <- step(s_maltophilia_glm, 
                           scope = list(lower = ~ 1, upper = ~ Plastic_Glass + Treatment + Collect),
                           direction = "both")
summary(s_maltophilia_step)
#Call:
#  glm(formula = S_maltophilia ~ Collect, family = gaussian(link = "identity"), 
#      data = s_maltophilia)
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     6078       1952   3.113 0.002365 ** 
#  Collect.L      15597       4375   3.565 0.000542 ***
#  Collect.Q      10925       3905   2.798 0.006087 ** 
#  Collect.C       4804       3370   1.426 0.156855    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 377172077)
#
#Null deviance: 5.1049e+10  on 112  degrees of freedom
#Residual deviance: 4.1112e+10  on 109  degrees of freedom
#AIC: 2558.2
#
#Number of Fisher Scoring iterations: 2

####################################
####################################
############## int1 on microplastics
####################################
####################################
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
                         family = gaussian(link='identity'))
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

mp_int1_step <- step(mp_composites_glm_int1, 
                           scope = list(lower = ~ 1, upper = ~ Plastic_Glass + Treatment + Collect),
                           direction = "both")
summary(mp_int1_step)

####################################
####################################
############## sul1 on microplastics
####################################
####################################
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

####################################
####################################
#P_aeruginosa
####################################
####################################
mp_composites_glm_Pa <- glm(P_aeruginosa ~ Plastic_Glass + Treatment + Collect, 
                              data = mp_composites, 
                              family = gaussian(link='identity'))
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


####################################
####################################

####################################
####################################

#View(mp_water)
dim(mp_water)
summary(mp_water)
colnames(mp_water)
