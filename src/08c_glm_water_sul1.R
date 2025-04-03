####################################
####################################
############## sul1 in water
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
h2o_sul1_bc <- powerTransform(mp_water$sul1)
h2o_sul1_bc$lambda
# Transform the response variable using the optimal lambda
h2o_transformed_sul1_response <- (mp_water$sul1^h2o_sul1_bc$lambda - 1) / h2o_sul1_bc$lambda
mp_water$transformed_sul1_response <- h2o_transformed_sul1_response
#log10
mp_water$log10_sul1_response <- log10(mp_water$sul1)
#ln response
mp_water$log_sul1_response <- log(mp_water$sul1)
### end transformations
##############################################################

###########################################################
### normality tests
# untransformed
h2o_sul1_ut_W_test <- shapiro.test(mp_water$sul1)
h2o_sul1_ut_W_test
h2o_sul1_ut_W_test$p.value
h2o_sul1_ut_W_value <- round(h2o_sul1_ut_W_test$statistic, 3)
h2o_sul1_ut_W_value
# box-cox
h2o_sul1_bc_W_test <- shapiro.test(mp_water$transformed_sul1_response)
h2o_sul1_bc_W_test
h2o_sul1_bc_W_test$p.value
h2o_sul1_bc_W_value <- round(h2o_sul1_bc_W_test$statistic, 3)
h2o_sul1_bc_W_value
#log10
h2o_sul1_log10_W_test <- shapiro.test(mp_water$log10_sul1_response)
h2o_sul1_log10_W_test
h2o_sul1_log10_W_test$p.value
h2o_sul1_log10_W_value <- round(h2o_sul1_log10_W_test$statistic, 3)
h2o_sul1_log10_W_value
#log/ln
h2o_sul1_log_W_test <- shapiro.test(mp_water$log_sul1_response)
h2o_sul1_log_W_test
h2o_sul1_log_W_test$p.value
h2o_sul1_log_W_value <- round(h2o_sul1_log_W_test$statistic, 3)
h2o_sul1_log_W_value
### end normality tests
##################################################

##################################################
### visual inspections with figure output
### data = mp_water
### counts = sul1
### histogram, qqplot, boxplot
# untransformed
# histogram
h2o_sul1_hist_ut <- ggplot(mp_water, aes(x=sul1)) + geom_histogram() + 
  labs(x = "Collect", y = "sul1") + theme_bw()
h2o_sul1_hist_ut
# qqplot
h2o_sul1_qqplot_ut <- ggqqplot(mp_water$sul1, 
                               title=paste0("qqplot: Untransformed (W = ", h2o_sul1_ut_W_value, ")"))
h2o_sul1_qqplot_ut
# boxplot
h2o_sul1_boxplot_ut <- ggplot(mp_water, aes(x = Collect, y = sul1)) +
  geom_boxplot() +
  labs(x = "Collect", y = "sul1") +
  theme_bw()
h2o_sul1_boxplot_ut

# box-cox
# histogram
h2o_sul1_hist_bc <- ggplot(mp_water, aes(x=transformed_sul1_response)) + geom_histogram() + 
  labs(x = "Collect", y = "sul1") + theme_bw()
h2o_sul1_hist_bc
# qqplot
h2o_sul1_qqplot_bc <- ggqqplot(mp_water$transformed_sul1_response, 
                               title=paste0("qqplot: Box-Cox (W = ", h2o_sul1_bc_W_value, ")"))
h2o_sul1_qqplot_bc
# boxplot
h2o_sul1_boxplot_bc <- ggplot(mp_water, aes(x = Collect, y = transformed_sul1_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "transformed_sul1_response") +
  theme_bw()
h2o_sul1_boxplot_bc

# log10
# histogram
h2o_sul1_hist_log10 <- ggplot(mp_water, aes(x=log10_sul1_response)) + geom_histogram() + 
  labs(x = "Collect", y = "sul1") + theme_bw()
h2o_sul1_hist_log10
# qqplot
h2o_sul1_qqplot_log10 <- ggqqplot(mp_water$log10_sul1_response, 
                                  title=paste0("qqplot: log10 (W = ", h2o_sul1_log10_W_value, ")"))
h2o_sul1_qqplot_log10
#boxplot
h2o_sul1_boxplot_log10 <- ggplot(mp_water, aes(x = Collect, y = log10_sul1_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "log10_response") +
  theme_bw()
h2o_sul1_boxplot_log10

# log/ln
h2o_sul1_hist_log <- ggplot(mp_water, aes(x=log_sul1_response)) + geom_histogram() + 
  labs(x = "Collect", y = "sul1") + theme_bw()
h2o_sul1_hist_log
# qqplot
h2o_sul1_qqplot_log <- ggqqplot(mp_water$log_sul1_response, 
                                title=paste0("qqplot: log (W = ", h2o_sul1_log_W_value, ")"))
h2o_sul1_qqplot_log
#boxplot
h2o_sul1_boxplot_log <- ggplot(mp_water, aes(x = Collect, y = log_sul1_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "log_response") +
  theme_bw()
h2o_sul1_boxplot_log
# end visualizations
########################################################################


#######################################################################
# Use ggarrange to combine the plots sulo a 4x3 grid
# hist, qqplot, boxplot
h2o_sul1_mp_all <- ggarrange(h2o_sul1_hist_ut, h2o_sul1_qqplot_ut, h2o_sul1_boxplot_ut, 
                             h2o_sul1_hist_bc, h2o_sul1_qqplot_bc, h2o_sul1_boxplot_bc,
                             h2o_sul1_hist_log10, h2o_sul1_qqplot_log10, h2o_sul1_boxplot_log10,
                             h2o_sul1_hist_log, h2o_sul1_qqplot_log, h2o_sul1_boxplot_log,
                             ncol = 3, nrow = 4,
                             labels = c("A", "B", "C", 
                                        "D", "E", "F",
                                        "G", "H", "I",
                                        "J", "K", "L"))
h2o_sul1_mp_all
# end ggarrange
############################################################





################################################################
# full glm
colnames(mp_water)
# common log 10
sul1_water_glm_log10 <- glm(log10_sul1_response ~ Treatment + Collect, 
                      data = mp_water, 
                      family = gaussian(link='identity'))
summary(sul1_water_glm_log10)
# end full glm
################################################################




################################################################
# stepwise glm
# common log 10
# .L linear, .C cubic
# .Q represents the quadratic trend. The significant .Q term suggests a curvilinear, U-shaped relationship between 
# time and the S maltophilia levels.
### step wise implementation
sul1_water_step_log10 <- step(sul1_water_glm_log10, 
                        scope = list(lower = ~ 1, upper = ~ Treatment + Collect),
                        direction = "both")
summary(sul1_water_step_log10)
# end stepwise glm
################################################################
