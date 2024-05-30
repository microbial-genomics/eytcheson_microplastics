####################################
####################################
############## intI1 on microplastics
####################################
####################################
# "intI1" refers to the integrase gene found in class 1 integrons, which are mobile genetic elements 
# that can capture and express antibiotic resistance genes.
# intI1 is the integrase gene found in class 1 integrons. It is highly conserved in sequence.
# Class 1 integrons are frequently found in Gram-negative bacteria, especially Enterobacteriaceae, 
# Pseudomonadaceae, and Moraxellaceae families.
# Class 1 integrons are major contributors to the acquisition and spread of antibiotic resistance genes 
# in bacteria. Over 50% of intI1 genes are associated with multiple resistance genes.
# intI1 is found in bacteria from clinical settings as well as natural environments like soil and water, 
# likely due to pollution with human and animal waste containing resistant bacteria.
# The abundance of intI1 has been proposed as a proxy for measuring anthropogenic pollution and the spread 
# of antibiotic resistance from human sources into the environment.

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

######################################
# transformations 
colnames(mp_composites)

# Perform Box-Cox transformation and find optimal lambda
int1_bc <- powerTransform(mp_composites$int1)
int1_bc$lambda
# Transform the response variable using the optimal lambda
transformed_int1_response <- (mp_composites$int1^int1_bc$lambda - 1) / int1_bc$lambda
mp_composites$transformed_int1_response <- transformed_int1_response
#log10
mp_composites$log10_int1_response <- log10(mp_composites$int1)
#ln response
mp_composites$log_int1_response <- log(mp_composites$int1)

# normality tests
# untransformed
shapiro.test(mp_composites$int1)
hist(mp_composites$int1)
qqnorm(mp_composites$int1, main="Untransformed (W=0.21)"); qqline(mp_composites$int1)
#Shapiro-Wilk normality test
#data:  mp_composites$int1
#W = 0.25298, p-value < 2.2e-16
# box cox
shapiro.test(mp_composites$transformed_int1_response)
hist(mp_composites$transformed_int1_response)
qqnorm(mp_composites$transformed_int1_response, main="Box-Cox (W=0.66)"); qqline(mp_composites$transformed_int1_response)
#Shapiro-Wilk normality test
#data:  mp_composites$transformed_response
#W = 0.92868, p-value = 4.231e-06
# log10
shapiro.test(mp_composites$log10_int1_response)
hist(mp_composites$log10_int1_response)
qqnorm(mp_composites$log10_int1_response, main="log10  (W=0.69)"); qqline(mp_composites$log10_int1_response)
#Shapiro-Wilk normality test
#data:  mp_composites$log10_response
#W = 0.90999, p-value = 3.239e-07
# log/ln
shapiro.test(mp_composites$log_int1_response)
hist(mp_composites$log_int1_response)
qqnorm(mp_composites$log_int1_response, main="ln (W=0.91)"); qqline(mp_composites$log_int1_response)
#Shapiro-Wilk normality test
#data:  mp_composites$log10_response
#W = 0.90999, p-value = 3.239e-07

par(mfrow = c(2, 2))
qqnorm(mp_composites$int1, main="Untransformed  (W=0.26)"); qqline(mp_composites$int1)
qqnorm(mp_composites$transformed_int1_response, main="Box-Cox (W=0.93)"); qqline(mp_composites$transformed_int1_response)
qqnorm(mp_composites$log10_int1_response, main="log10 (W=0.91)"); qqline(mp_composites$log10_int1_response)
qqnorm(mp_composites$log_int1_response, main="ln (W=0.91)"); qqline(mp_composites$log_int1_response)
par(mfrow=c(1,1))

# end transformations
############################################################













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

