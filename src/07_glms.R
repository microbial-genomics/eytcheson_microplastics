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


# .Q represents the quadratic trend. The significant .Q term suggests a curvilinear, U-shaped relationship between 
# time and the S maltophilia levels.



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

# sul1
mp_composites_glm_sul1 <- glm(sul1 ~ Plastic_Glass + Treatment + Collect, 
                              data = mp_composites, 
                              family = gaussian(link='identity'))
summary(mp_composites_glm_sul1)

#P_aeruginosa
mp_composites_glm_Pa <- glm(P_aeruginosa ~ Plastic_Glass + Treatment + Collect, 
                              data = mp_composites, 
                              family = gaussian(link='identity'))
summary(mp_composites_glm_Pa)




#View(mp_water)
dim(mp_water)
summary(mp_water)
colnames(mp_water)
