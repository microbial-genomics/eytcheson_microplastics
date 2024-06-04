####################################
####################################
############## P aeruginosa in water
####################################
####################################

#########################################
### data prep
#View(mp_water)
# done for S multophilia
### end data prep
###############################################

######################################
### transformations 
colnames(mp_water)

# Perform Box-Cox transformation and find optimal lambda
h20_Pa_bc <- powerTransform(mp_water$P_aeruginosa)
h20_Pa_bc$lambda
# Transform the response variable using the optimal lambda
h20_transformed_Pa_response <- (mp_water$P_aeruginosa^h20_Pa_bc$lambda - 1) / h20_Pa_bc$lambda
mp_water$transformed_Pa_response <- h20_transformed_Pa_response
#log10
mp_water$log10_Pa_response <- log10(mp_water$P_aeruginosa)
#ln response
mp_water$log_Pa_response <- log(mp_water$P_aeruginosa)
### end transformations
##############################################################

###########################################################
### normality tests
# untransformed
h2o_Pa_ut_W_test <- shapiro.test(mp_water$P_aeruginosa)
h2o_Pa_ut_W_test
h2o_Pa_ut_W_test$p.value
h2o_Pa_ut_W_value <- round(h2o_Pa_ut_W_test$statistic, 3)
h2o_Pa_ut_W_value
# box-cox
h2o_Pa_bc_W_test <- shapiro.test(mp_water$transformed_Pa_response)
h2o_Pa_bc_W_test
h2o_Pa_bc_W_test$p.value
h2o_Pa_bc_W_value <- round(h2o_Pa_bc_W_test$statistic, 3)
h2o_Pa_bc_W_value
#log10
h2o_Pa_log10_W_test <- shapiro.test(mp_water$log10_Pa_response)
h2o_Pa_log10_W_test
h2o_Pa_log10_W_test$p.value
h2o_Pa_log10_W_value <- round(h2o_Pa_log10_W_test$statistic, 3)
h2o_Pa_log10_W_value
#log/ln
h2o_Pa_log_W_test <- shapiro.test(mp_water$log_Pa_response)
h2o_Pa_log_W_test
h2o_Pa_log_W_test$p.value
h2o_Pa_log_W_value <- round(h2o_Pa_log_W_test$statistic, 3)
h2o_Pa_log_W_value
### end normality tests
##################################################




##################################################
### visual inspections with figure output
### data = mp_water
### counts = P_aeruginosa
### histogram, qqplot, boxplot
# untransformed
# histogram
h2o_Pa_hist_ut <- ggplot(mp_water, aes(x=P_aeruginosa)) + geom_histogram() + 
  labs(x = "Collect", y = "P_aeruginosa") + theme_bw()
h2o_Pa_hist_ut
# qqplot
h2o_Pa_qqplot_ut <- ggqqplot(mp_water$P_aeruginosa, 
                               title=paste0("qqplot: Untransformed (W = ", h2o_Pa_ut_W_value, ")"))
h2o_Pa_qqplot_ut
# boxplot
h2o_Pa_boxplot_ut <- ggplot(mp_water, aes(x = Collect, y = P_aeruginosa)) +
  geom_boxplot() +
  labs(x = "Collect", y = "P_aeruginosa") +
  theme_bw()
h2o_Pa_boxplot_ut

# box-cox
# histogram
h2o_Pa_hist_bc <- ggplot(mp_water, aes(x=transformed_Pa_response)) + geom_histogram() + 
  labs(x = "Collect", y = "P_aeruginosa") + theme_bw()
h2o_Pa_hist_bc
# qqplot
h2o_Pa_qqplot_bc <- ggqqplot(mp_water$transformed_Pa_response, 
                               title=paste0("qqplot: Box-Cox (W = ", h2o_Pa_bc_W_value, ")"))
h2o_Pa_qqplot_bc
# boxplot
h2o_Pa_boxplot_bc <- ggplot(mp_water, aes(x = Collect, y = transformed_Pa_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "transformed_Pa_response") +
  theme_bw()
h2o_Pa_boxplot_bc

# log10
# histogram
h2o_Pa_hist_log10 <- ggplot(mp_water, aes(x=log10_Pa_response)) + geom_histogram() + 
  labs(x = "Collect", y = "P_aeruginosa") + theme_bw()
h2o_Pa_hist_log10
# qqplot
h2o_Pa_qqplot_log10 <- ggqqplot(mp_water$log10_Pa_response, 
                                  title=paste0("qqplot: log10 (W = ", h2o_Pa_log10_W_value, ")"))
h2o_Pa_qqplot_log10
#boxplot
h2o_Pa_boxplot_log10 <- ggplot(mp_water, aes(x = Collect, y = log10_Pa_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "log10_response") +
  theme_bw()
h2o_Pa_boxplot_log10

# log/ln
h2o_Pa_hist_log <- ggplot(mp_water, aes(x=log_Pa_response)) + geom_histogram() + 
  labs(x = "Collect", y = "P_aeruginosa") + theme_bw()
h2o_Pa_hist_log
# qqplot
h2o_Pa_qqplot_log <- ggqqplot(mp_water$log_Pa_response, 
                                title=paste0("qqplot: log (W = ", h2o_Pa_log_W_value, ")"))
h2o_Pa_qqplot_log
#boxplot
h2o_Pa_boxplot_log <- ggplot(mp_water, aes(x = Collect, y = log_Pa_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "log_response") +
  theme_bw()
h2o_Pa_boxplot_log
# end visualizations
########################################################################


#######################################################################
# Use ggarrange to combine the plots sulo a 4x3 grid
# hist, qqplot, boxplot
h2o_Pa_mp_all <- ggarrange(h2o_Pa_hist_ut, h2o_Pa_qqplot_ut, h2o_Pa_boxplot_ut, 
                             h2o_Pa_hist_bc, h2o_Pa_qqplot_bc, h2o_Pa_boxplot_bc,
                             h2o_Pa_hist_log10, h2o_Pa_qqplot_log10, h2o_Pa_boxplot_log10,
                             h2o_Pa_hist_log, h2o_Pa_qqplot_log, h2o_Pa_boxplot_log,
                             ncol = 3, nrow = 4,
                             labels = c("A", "B", "C", 
                                        "D", "E", "F",
                                        "G", "H", "I",
                                        "J", "K", "L"))
h2o_Pa_mp_all
# end ggarrange
############################################################


################################################################
# full glm
colnames(mp_water)
# common log 10
P_aeruginosa_water_glm_log10 <- glm(log10_Pa_response ~ Treatment + Collect, 
                      data = mp_water, 
                      family = gaussian(link='identityg'))
summary(P_aeruginosa_water_glm_log10)
# end full glm
################################################################




################################################################
# stepwise glm
# common log 10
# .L linear, .C cubic
# .Q represents the quadratic trend. The significant .Q term suggests a curvilinear, U-shaped relationship between 
# time and the S maltophilia levels.
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
