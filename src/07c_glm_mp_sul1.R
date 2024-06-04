####################################
####################################
############## sul1 on microplastics
####################################
####################################

#########################################
### data prep
# done for int1
### end data prep
###############################################


######################################
# transformations 
colnames(mp_composites)

# Perform Box-Cox transformation and find optimal lambda
sul1_bc <- powerTransform(mp_composites$sul1)
sul1_bc$lambda
# Transform the response variable using the optimal lambda
transformed_sul1_response <- (mp_composites$sul1^sul1_bc$lambda - 1) / sul1_bc$lambda
mp_composites$transformed_sul1_response <- transformed_sul1_response
#log10
mp_composites$log10_sul1_response <- log10(mp_composites$sul1)
#ln response
mp_composites$log_sul1_response <- log(mp_composites$sul1)
### end transformations
##############################################################


###########################################################
### normality tests
# untransformed
mp_sul1_ut_W_test <- shapiro.test(mp_composites$sul1)
mp_sul1_ut_W_test
mp_sul1_ut_W_test$p.value
mp_sul1_ut_W_value <- round(mp_sul1_ut_W_test$statistic, 3)
mp_sul1_ut_W_value
# box-cox
mp_sul1_bc_W_test <- shapiro.test(mp_composites$transformed_sul1_response)
mp_sul1_bc_W_test
mp_sul1_bc_W_test$p.value
mp_sul1_bc_W_value <- round(mp_sul1_bc_W_test$statistic, 3)
mp_sul1_bc_W_value
#log10
mp_sul1_log10_W_test <- shapiro.test(mp_composites$log10_sul1_response)
mp_sul1_log10_W_test
mp_sul1_log10_W_test$p.value
mp_sul1_log10_W_value <- round(mp_sul1_log10_W_test$statistic, 3)
mp_sul1_log10_W_value
#log/ln
mp_sul1_log_W_test <- shapiro.test(mp_composites$log_sul1_response)
mp_sul1_log_W_test
mp_sul1_log_W_test$p.value
mp_sul1_log_W_value <- round(mp_sul1_log_W_test$statistic, 3)
mp_sul1_log_W_value
### end normality tests
##################################################


##################################################
### visual inspections with figure output
### data = mp_composites
### counts = sul1
### histogram, qqplot, boxplot
# untransformed
# histogram
mp_sul1_hist_ut <- ggplot(mp_composites, aes(x=sul1)) + geom_histogram() + 
  labs(x = "Collect", y = "sul1") + theme_bw()
mp_sul1_hist_ut
# qqplot
mp_sul1_qqplot_ut <- ggqqplot(mp_composites$sul1, 
                              title=paste0("qqplot: Untransformed (W = ", mp_sul1_ut_W_value, ")"))
mp_sul1_qqplot_ut
# boxplot
mp_sul1_boxplot_ut <- ggplot(mp_composites, aes(x = Collect, y = sul1)) +
  geom_boxplot() +
  labs(x = "Collect", y = "sul1") +
  theme_bw()
mp_sul1_boxplot_ut

# box-cox
# histogram
mp_sul1_hist_bc <- ggplot(mp_composites, aes(x=transformed_sul1_response)) + geom_histogram() + 
  labs(x = "Collect", y = "sul1") + theme_bw()
mp_sul1_hist_bc
# qqplot
mp_sul1_qqplot_bc <- ggqqplot(mp_composites$transformed_sul1_response, 
                              title=paste0("qqplot: Box-Cox (W = ", mp_sul1_bc_W_value, ")"))
mp_sul1_qqplot_bc
# boxplot
mp_sul1_boxplot_bc <- ggplot(mp_composites, aes(x = Collect, y = transformed_sul1_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "transformed_sul1_response") +
  theme_bw()
mp_sul1_boxplot_bc

# log10
# histogram
mp_sul1_hist_log10 <- ggplot(mp_composites, aes(x=log10_sul1_response)) + geom_histogram() + 
  labs(x = "Collect", y = "sul1") + theme_bw()
mp_sul1_hist_log10
# qqplot
mp_sul1_qqplot_log10 <- ggqqplot(mp_composites$log10_sul1_response, 
                                 title=paste0("qqplot: log10 (W = ", mp_sul1_log10_W_value, ")"))
mp_sul1_qqplot_log10
#boxplot
mp_sul1_boxplot_log10 <- ggplot(mp_composites, aes(x = Collect, y = log10_sul1_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "log10_response") +
  theme_bw()
mp_sul1_boxplot_log10

# log/ln
mp_sul1_hist_log <- ggplot(mp_composites, aes(x=log_sul1_response)) + geom_histogram() + 
  labs(x = "Collect", y = "sul1") + theme_bw()
mp_sul1_hist_log
# qqplot
mp_sul1_qqplot_log <- ggqqplot(mp_composites$log_sul1_response, 
                               title=paste0("qqplot: log (W = ", mp_sul1_log_W_value, ")"))
mp_sul1_qqplot_log
#boxplot
mp_sul1_boxplot_log <- ggplot(mp_composites, aes(x = Collect, y = log_sul1_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "log_response") +
  theme_bw()
mp_sul1_boxplot_log
# end visualizations
########################################################################


#######################################################################
# Use ggarrange to combine the plots sulo a 4x3 grid
# hist, qqplot, boxplot
mp_sul1_mp_all <- ggarrange(mp_sul1_hist_ut, mp_sul1_qqplot_ut, mp_sul1_boxplot_ut, 
                            mp_sul1_hist_bc, mp_sul1_qqplot_bc, mp_sul1_boxplot_bc,
                            mp_sul1_hist_log10, mp_sul1_qqplot_log10, mp_sul1_boxplot_log10,
                            mp_sul1_hist_log, mp_sul1_qqplot_log, mp_sul1_boxplot_log,
                            ncol = 3, nrow = 4,
                            labels = c("A", "B", "C", 
                                       "D", "E", "F",
                                       "G", "H", "I",
                                       "J", "K", "L"))
mp_sul1_mp_all
# end ggarrange
############################################################



################################################################
# full glm
colnames(mp_composites)
# common log 10
mp_composites_glm_sul1_log10 <- glm(log10_sul1_response ~ Plastic_Glass + Treatment + Collect, 
                              data = mp_composites, 
                              family = gaussian(link='identity'))
summary(mp_composites_glm_sul1_log10)
# end full glm
################################################################




################################################################
# stepwise glm
# common log 10
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
