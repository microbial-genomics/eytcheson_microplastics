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
# full glm
colnames(s_maltophilia)
# common log 10
s_maltophilia_glm_log10 <- glm(log10_sm_response ~ Plastic_Glass + Treatment + Collect, 
                         data = s_maltophilia, 
                         family = gaussian(link='identity'))
summary(s_maltophilia_glm_log10)
# end full glm
################################################################


################################################################
# stepwise glm
# common log 10
# .L linear, .C cubic
# .Q represents the quadratic trend. The significant .Q term suggests a curvilinear, U-shaped relationship between 
# time and the S maltophilia levels.


### step wise implementation (log10)
s_maltophilia_step <- step(s_maltophilia_glm_log10, 
                           scope = list(lower = ~ 1, upper = ~ Plastic_Glass + Treatment + Collect),
                           direction = "both")
summary(s_maltophilia_step)

# end stepwise glm
################################################################
