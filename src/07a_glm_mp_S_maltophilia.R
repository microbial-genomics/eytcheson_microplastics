####################################
####################################
############## S. maltophilia on microplastics
####################################
####################################
# Stenotrophomonas maltophilia 
# It is an aerobic, non-fermenting, gram-negative bacillus that is found in various aqueous environments 
# like plant rhizospheres, soil, and water sources.
# S. maltophilia is an opportunistic pathogen that can cause nosocomial infections, especially in 
# immunocompromised patients. Risk factors include underlying malignancy, cystic fibrosis, mechanical 
# ventilation, recent surgery, and exposure to broad-spectrum antibiotics.
# It frequently colonizes humid surfaces like ventilator tubes and catheters and can form biofilms on 
# plastic surfaces. S. maltophilia also often co-occurs with Pseudomonas aeruginosa.
# Infections can occur in various organs, but it is commonly found in respiratory tract infections. 
# Treatment can be challenging as S. maltophilia is intrinsically resistant to many antibiotic classes.


#########################################
### data prep
### full model fit
#View(s_maltophilia)
dim(s_maltophilia)
summary(s_maltophilia)
colnames(s_maltophilia)
s_maltophilia$Plastic_Glass <- as.factor(s_maltophilia$Plastic_Glass)
s_maltophilia$Collect <- factor(s_maltophilia$Collect,
                                   levels = c("week0", "week2", "week6", "week10"),
                                   ordered=TRUE)
s_maltophilia$Treatment <- as.factor(s_maltophilia$Treatment)
### end data prep
###############################################


######################################
### transformations 
colnames(s_maltophilia)

# Perform Box-Cox transformation and find optimal lambda
sm_bc <- powerTransform(s_maltophilia$S_maltophilia)
sm_bc$lambda
# Transform the response variable using the optimal lambda
transformed_sm_response <- (s_maltophilia$S_maltophilia^sm_bc$lambda - 1) / sm_bc$lambda
s_maltophilia$transformed_sm_response <- transformed_sm_response
#log10
s_maltophilia$log10_sm_response <- log10(s_maltophilia$S_maltophilia)
#ln response
s_maltophilia$log_sm_response <- log(s_maltophilia$S_maltophilia)
### end transformations
##############################################################


###########################################################
### normality tests
# untransformed
mp_sm_ut_W_test <- shapiro.test(s_maltophilia$S_maltophilia)
mp_sm_ut_W_test
mp_sm_ut_W_test$p.value
mp_sm_ut_W_value <- round(mp_sm_ut_W_test$statistic, 3)
mp_sm_ut_W_value
# box-cox
mp_sm_bc_W_test <- shapiro.test(s_maltophilia$transformed_sm_response)
mp_sm_bc_W_test
mp_sm_bc_W_test$p.value
mp_sm_bc_W_value <- round(mp_sm_bc_W_test$statistic, 3)
mp_sm_bc_W_value
#log10
mp_sm_log10_W_test <- shapiro.test(s_maltophilia$log10_sm_response)
mp_sm_log10_W_test
mp_sm_log10_W_test$p.value
mp_sm_log10_W_value <- round(mp_sm_log10_W_test$statistic, 3)
mp_sm_log10_W_value
#log/ln
mp_sm_log_W_test <- shapiro.test(s_maltophilia$log_sm_response)
mp_sm_log_W_test
mp_sm_log_W_test$p.value
mp_sm_log_W_value <- round(mp_sm_log_W_test$statistic, 3)
mp_sm_log_W_value
### end normality tests
##################################################



##################################################
### visual inspections with figure output
### data = s_maltophilia
### counts = S_maltophilia
### histogram, qqplot, boxplot
# untransformed
# histogram
mp_sm_hist_ut <- ggplot(s_maltophilia, aes(x=S_maltophilia)) + geom_histogram() + 
                    labs(x = "Collect", y = "S_maltophilia") + theme_bw()
mp_sm_hist_ut
# qqplot
mp_sm_qqplot_ut <- ggqqplot(s_maltophilia$S_maltophilia, 
         title=paste0("qqplot: Untransformed (W = ", mp_sm_ut_W_value, ")"))
mp_sm_qqplot_ut
# boxplot
mp_sm_boxplot_ut <- ggplot(s_maltophilia, aes(x = Collect, y = S_maltophilia)) +
  geom_boxplot() +
  labs(x = "Collect", y = "S_maltophilia") +
  theme_bw()
mp_sm_boxplot_ut

# box-cox
# histogram
mp_sm_hist_bc <- ggplot(s_maltophilia, aes(x=transformed_sm_response)) + geom_histogram() + 
  labs(x = "Collect", y = "S_maltophilia") + theme_bw()
mp_sm_hist_bc
# qqplot
mp_sm_qqplot_bc <- ggqqplot(s_maltophilia$transformed_sm_response, 
                         title=paste0("qqplot: Box-Cox (W = ", mp_sm_bc_W_value, ")"))
mp_sm_qqplot_bc
# boxplot
mp_sm_boxplot_bc <- ggplot(s_maltophilia, aes(x = Collect, y = transformed_sm_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "transformed_sm_response") +
  theme_bw()
mp_sm_boxplot_bc

# log10
# histogram
mp_sm_hist_log10 <- ggplot(s_maltophilia, aes(x=log10_sm_response)) + geom_histogram() + 
  labs(x = "Collect", y = "S_maltophilia") + theme_bw()
mp_sm_hist_log10
# qqplot
mp_sm_qqplot_log10 <- ggqqplot(s_maltophilia$log10_sm_response, 
                         title=paste0("qqplot: log10 (W = ", mp_sm_log10_W_value, ")"))
mp_sm_qqplot_log10
#boxplot
mp_sm_boxplot_log10 <- ggplot(s_maltophilia, aes(x = Collect, y = log10_sm_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "log10_response") +
  theme_bw()
mp_sm_boxplot_log10

# log/ln
mp_sm_hist_log <- ggplot(s_maltophilia, aes(x=log_sm_response)) + geom_histogram() + 
  labs(x = "Collect", y = "S_maltophilia") + theme_bw()
mp_sm_hist_log
# qqplot
mp_sm_qqplot_log <- ggqqplot(s_maltophilia$log_sm_response, 
                            title=paste0("qqplot: log (W = ", mp_sm_log_W_value, ")"))
mp_sm_qqplot_log
#boxplot
mp_sm_boxplot_log <- ggplot(s_maltophilia, aes(x = Collect, y = log_sm_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "log_response") +
  theme_bw()
mp_sm_boxplot_log
# end visualizations
########################################################################


#######################################################################
# Use ggarrange to combine the plots into a 4x3 grid
# hist, qqplot, boxplot
mp_sm_mp_all <- ggarrange(mp_sm_hist_ut, mp_sm_qqplot_ut, mp_sm_boxplot_ut, 
                          mp_sm_hist_bc, mp_sm_qqplot_bc, mp_sm_boxplot_bc,
                          mp_sm_hist_log10, mp_sm_qqplot_log10, mp_sm_boxplot_log10,
                          mp_sm_hist_log, mp_sm_qqplot_log, mp_sm_boxplot_log,
          ncol = 3, nrow = 4,
          labels = c("A", "B", "C", 
                     "D", "E", "F",
                     "G", "H", "I",
                     "J", "K", "L"))
mp_sm_mp_all
# end ggarrange
############################################################


################################################################
# glms
# natural log
s_maltophilia_glm <- glm(S_maltophilia ~ Plastic_Glass + Treatment + Collect, 
                         data = s_maltophilia, 
                         family = gaussian(link='log'))
summary(s_maltophilia_glm)

# boxcox
s_maltophilia_glm_bc <- glm(transformed_response ~ Plastic_Glass + Treatment + Collect, 
                         data = s_maltophilia, 
                         family = gaussian(link='identity'))
summary(s_maltophilia_glm_bc)

# common log 10
s_maltophilia_glm_log10 <- glm(log10_response ~ Plastic_Glass + Treatment + Collect, 
                         data = s_maltophilia, 
                         family = gaussian(link='identity'))
summary(s_maltophilia_glm_log10)

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


### step wise implementation (box cox)
s_maltophilia_step <- step(s_maltophilia_glm_bc, 
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

