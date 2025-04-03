####################################
####################################
#### P_aeruginosa on microplastics
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
Pa_bc <- powerTransform(mp_composites$P_aeruginosa)
Pa_bc$lambda
# Transform the response variable using the optimal lambda
transformed_Pa_response <- (mp_composites$P_aeruginosa^Pa_bc$lambda - 1) / Pa_bc$lambda
mp_composites$transformed_Pa_response <- transformed_Pa_response
#log10
mp_composites$log10_Pa_response <- log10(mp_composites$P_aeruginosa)
#ln response
mp_composites$log_Pa_response <- log(mp_composites$P_aeruginosa)
### end transformations
##############################################################

###########################################################
### normality tests
# untransformed
mp_Pa_ut_W_test <- shapiro.test(mp_composites$P_aeruginosa)
mp_Pa_ut_W_test
mp_Pa_ut_W_test$p.value
mp_Pa_ut_W_value <- round(mp_Pa_ut_W_test$statistic, 3)
mp_Pa_ut_W_value
# box-cox
mp_Pa_bc_W_test <- shapiro.test(mp_composites$transformed_Pa_response)
mp_Pa_bc_W_test
mp_Pa_bc_W_test$p.value
mp_Pa_bc_W_value <- round(mp_Pa_bc_W_test$statistic, 3)
mp_Pa_bc_W_value
#log10
mp_Pa_log10_W_test <- shapiro.test(mp_composites$log10_Pa_response)
mp_Pa_log10_W_test
mp_Pa_log10_W_test$p.value
mp_Pa_log10_W_value <- round(mp_Pa_log10_W_test$statistic, 3)
mp_Pa_log10_W_value
#log/ln
mp_Pa_log_W_test <- shapiro.test(mp_composites$log_Pa_response)
mp_Pa_log_W_test
mp_Pa_log_W_test$p.value
mp_Pa_log_W_value <- round(mp_Pa_log_W_test$statistic, 3)
mp_Pa_log_W_value
### end normality tests
##################################################



##################################################
### visual inspections with figure output
### data = mp_composites
### counts = P_aeruginosa
### histogram, qqplot, boxplot
# untransformed
# histogram
mp_Pa_hist_ut <- ggplot(mp_composites, aes(x=P_aeruginosa)) + geom_histogram() + 
  labs(x = "Collect", y = "Pa") + theme_bw()
mp_Pa_hist_ut
# qqplot
mp_Pa_qqplot_ut <- ggqqplot(mp_composites$P_aeruginosa, 
                              title=paste0("qqplot: Untransformed (W = ", mp_Pa_ut_W_value, ")"))
mp_Pa_qqplot_ut
# boxplot
mp_Pa_boxplot_ut <- ggplot(mp_composites, aes(x = Collect, y = P_aeruginosa)) +
  geom_boxplot() +
  labs(x = "Collect", y = "P_aeruginosa") +
  theme_bw()
mp_Pa_boxplot_ut

# box-cox
# histogram
mp_Pa_hist_bc <- ggplot(mp_composites, aes(x=transformed_Pa_response)) + geom_histogram() + 
  labs(x = "Collect", y = "P_aeruginosa") + theme_bw()
mp_Pa_hist_bc
# qqplot
mp_Pa_qqplot_bc <- ggqqplot(mp_composites$transformed_Pa_response, 
                              title=paste0("qqplot: Box-Cox (W = ", mp_Pa_bc_W_value, ")"))
mp_Pa_qqplot_bc
# boxplot
mp_Pa_boxplot_bc <- ggplot(mp_composites, aes(x = Collect, y = transformed_Pa_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "transformed_Pa_response") +
  theme_bw()
mp_Pa_boxplot_bc

# log10
# histogram
mp_Pa_hist_log10 <- ggplot(mp_composites, aes(x=log10_Pa_response)) + geom_histogram() + 
  labs(x = "Collect", y = "Pa") + theme_bw()
mp_Pa_hist_log10
# qqplot
mp_Pa_qqplot_log10 <- ggqqplot(mp_composites$log10_Pa_response, 
                                 title=paste0("qqplot: log10 (W = ", mp_Pa_log10_W_value, ")"))
mp_Pa_qqplot_log10
#boxplot
mp_Pa_boxplot_log10 <- ggplot(mp_composites, aes(x = Collect, y = log10_Pa_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "log10_response") +
  theme_bw()
mp_Pa_boxplot_log10

# log/ln
mp_Pa_hist_log <- ggplot(mp_composites, aes(x=log_Pa_response)) + geom_histogram() + 
  labs(x = "Collect", y = "Pa") + theme_bw()
mp_Pa_hist_log
# qqplot
mp_Pa_qqplot_log <- ggqqplot(mp_composites$log_Pa_response, 
                               title=paste0("qqplot: log (W = ", mp_Pa_log_W_value, ")"))
mp_Pa_qqplot_log
#boxplot
mp_Pa_boxplot_log <- ggplot(mp_composites, aes(x = Collect, y = log_Pa_response)) +
  geom_boxplot() +
  labs(x = "Collect", y = "log_response") +
  theme_bw()
mp_Pa_boxplot_log
# end visualizations
########################################################################


#######################################################################
# Use ggarrange to combine the plots sulo a 4x3 grid
# hist, qqplot, boxplot
mp_Pa_mp_all <- ggarrange(mp_Pa_hist_ut, mp_Pa_qqplot_ut, mp_Pa_boxplot_ut, 
                            mp_Pa_hist_bc, mp_Pa_qqplot_bc, mp_Pa_boxplot_bc,
                            mp_Pa_hist_log10, mp_Pa_qqplot_log10, mp_Pa_boxplot_log10,
                            mp_Pa_hist_log, mp_Pa_qqplot_log, mp_Pa_boxplot_log,
                            ncol = 3, nrow = 4,
                            labels = c("A", "B", "C", 
                                       "D", "E", "F",
                                       "G", "H", "I",
                                       "J", "K", "L"))
mp_Pa_mp_all
# end ggarrange
############################################################

################################################################
# full glm
colnames(mp_composites)
# common log 10
mp_composites_glm_Pa_log10 <- glm(log10_Pa_response ~ Plastic_Glass + Treatment + Collect, 
                            data = mp_composites, 
                            family = gaussian(link='identity'))
summary(mp_composites_glm_Pa_log10)
# end full glm
################################################################




################################################################
# stepwise glm
# common log 10
# .L linear, .C cubic
# .Q represents the quadratic trend. The significant .Q term suggests a curvilinear, U-shaped relationship between 
# time and the S maltophilia levels.
### step wise implementation
mp_Pa_step_log10 <- step(mp_composites_glm_Pa_log10, 
                     scope = list(lower = ~ 1, upper = ~ Plastic_Glass + Treatment + Collect),
                     direction = "both")
summary(mp_Pa_step_log10)
# end stepwise glm
################################################################