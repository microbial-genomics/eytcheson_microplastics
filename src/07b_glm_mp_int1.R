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

#########################################
### data prep
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
### end transformations
##############################################################



###########################################################
### normality tests
# untransformed
mp_int1_ut_W_test <- shapiro.test(mp_composites$int1)
mp_int1_ut_W_test
mp_int1_ut_W_test$p.value
mp_int1_ut_W_value <- round(mp_int1_ut_W_test$statistic, 3)
mp_int1_ut_W_value
# box-cox
mp_int1_bc_W_test <- shapiro.test(mp_composites$transformed_int1_response)
mp_int1_bc_W_test
mp_int1_bc_W_test$p.value
mp_int1_bc_W_value <- round(mp_int1_bc_W_test$statistic, 3)
mp_int1_bc_W_value
#log10
mp_int1_log10_W_test <- shapiro.test(mp_composites$log10_int1_response)
mp_int1_log10_W_test
mp_int1_log10_W_test$p.value
mp_int1_log10_W_value <- round(mp_int1_log10_W_test$statistic, 3)
mp_int1_log10_W_value
#log/ln
mp_int1_log_W_test <- shapiro.test(mp_composites$log_int1_response)
mp_int1_log_W_test
mp_int1_log_W_test$p.value
mp_int1_log_W_value <- round(mp_int1_log_W_test$statistic, 3)
mp_int1_log_W_value
### end normality tests
##################################################

##################################################
### visual inspections with figure output
### data = mp_composites
### counts = int1
### histogram, qqplot, boxplot
# untransformed
# histogram
mp_int1_hist_ut <- ggplot(mp_composites, aes(x=int1)) + geom_histogram() + 
  labs(x = "Collect", y = "int1") + theme_bw()
mp_int1_hist_ut
# qqplot
mp_int1_qqplot_ut <- ggqqplot(mp_composites$int1, 
                            title=paste0("qqplot: Untransformed (W = ", mp_int1_ut_W_value, ")"))
mp_int1_qqplot_ut
# boxplot
mp_int1_boxplot_ut <- ggplot(mp_composites, aes(x = Collect, y = int1)) +
  geom_boxplot() +
  labs(x = "Collect", y = "int1") +
  theme_bw()
mp_int1_boxplot_ut

# box-cox
# histogram
mp_int1_hist_bc <- ggplot(mp_composites, aes(x=transformed_int1_response)) + geom_histogram() + 
  labs(x = "Collect", y = "int1") + theme_bw()
mp_int1_hist_bc
# qqplot
mp_int1_qqplot_bc <- ggqqplot(mp_composites$transformed_int1_response, 
                            title=paste0("qqplot: Box-Cox (W = ", mp_int1_bc_W_value, ")"))
mp_int1_qqplot_bc
# boxplot
mp_int1_boxplot_bc <- ggplot(mp_composites, aes(x = Collect, y = transformed_int1_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "transformed_int1_response") +
  theme_bw()
mp_int1_boxplot_bc

# log10
# histogram
mp_int1_hist_log10 <- ggplot(mp_composites, aes(x=log10_int1_response)) + geom_histogram() + 
  labs(x = "Collect", y = "int1") + theme_bw()
mp_int1_hist_log10
# qqplot
mp_int1_qqplot_log10 <- ggqqplot(mp_composites$log10_int1_response, 
                               title=paste0("qqplot: log10 (W = ", mp_int1_log10_W_value, ")"))
mp_int1_qqplot_log10
#boxplot
mp_int1_boxplot_log10 <- ggplot(mp_composites, aes(x = Collect, y = log10_int1_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "log10_response") +
  theme_bw()
mp_int1_boxplot_log10

# log/ln
mp_int1_hist_log <- ggplot(mp_composites, aes(x=log_int1_response)) + geom_histogram() + 
  labs(x = "Collect", y = "int1") + theme_bw()
mp_int1_hist_log
# qqplot
mp_int1_qqplot_log <- ggqqplot(mp_composites$log_int1_response, 
                             title=paste0("qqplot: log (W = ", mp_int1_log_W_value, ")"))
mp_int1_qqplot_log
#boxplot
mp_int1_boxplot_log <- ggplot(mp_composites, aes(x = Collect, y = log_int1_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "log_response") +
  theme_bw()
mp_int1_boxplot_log
# end visualizations
########################################################################


#######################################################################
# Use ggarrange to combine the plots into a 4x3 grid
# hist, qqplot, boxplot
mp_int1_mp_all <- ggarrange(mp_int1_hist_ut, mp_int1_qqplot_ut, mp_int1_boxplot_ut, 
                          mp_int1_hist_bc, mp_int1_qqplot_bc, mp_int1_boxplot_bc,
                          mp_int1_hist_log10, mp_int1_qqplot_log10, mp_int1_boxplot_log10,
                          mp_int1_hist_log, mp_int1_qqplot_log, mp_int1_boxplot_log,
                          ncol = 3, nrow = 4,
                          labels = c("A", "B", "C", 
                                     "D", "E", "F",
                                     "G", "H", "I",
                                     "J", "K", "L"))
mp_int1_mp_all
# end ggarrange
############################################################



################################################################
# full glm
colnames(mp_composites)
# common log 10
mp_composites_glm_int1_log10 <- glm(log10_int1_response ~ Plastic_Glass + Treatment + Collect, 
                              data = mp_composites, 
                              family = gaussian(link='identity'))
summary(mp_composites_glm_int1_log10)
# end full glm
################################################################




################################################################
# stepwise glm
# common log 10
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

