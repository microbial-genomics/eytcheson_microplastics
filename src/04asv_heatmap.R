#View(asv_class)
#reduce esf_otu by dropping otus
summary(asv_class)
dim(asv_class)
#rowSums(asv_class[,2:152])

#View(asv_class)
colnames(asv_class)
# "X"         "ASV"       "Sample"    "Abundance" "sampleid"  "effluent"  "type"      "wk"        "pt"        "Kingdom"  
# "Phylum"    "Class"  
unique(asv_class$wk)
unique(asv_class$effluent)
unique(asv_class$type)
unique(asv_class$pt)

#heatmap_these_rows <- subset_these_rows
heatmap_these_rows_wk2 <- which(asv_class$wk=="wk02")
heatmap_these_rows_wk6 <- which(asv_class$wk=="wk06")
heatmap_these_rows_wk10 <- which(asv_class$wk=="wk10")

#week 2, class
asv_class_grouped_wk2 <- 
  asv_class[heatmap_these_rows_wk2,] %>%
  group_by(effluent, pt, Class) %>%
  summarise(sum = mean(Abundance)) #, n = n()
#View(asv_class_grouped_wk2)
dim(asv_class_grouped_wk2)
colnames(asv_class_grouped_wk2)
asv_class_grouped_wk2.df <- as.data.frame(asv_class_grouped_wk2)
asv_class_grouped_wk2.df$effluent <- as.factor(asv_class_grouped_wk2.df$effluent)
asv_class_grouped_wk2.df$pt <- as.factor(asv_class_grouped_wk2.df$pt)
asv_class_grouped_wk2.df$Class <- as.factor(asv_class_grouped_wk2.df$Class)
#View(asv_class_grouped_wk2.df)
summary(asv_class_grouped_wk2.df)
hist(asv_class_grouped_wk2.df$sum)
#cast replaced by acast for array and dcast for dataframe
asv_class_grouped_wk2_wide <- dcast(asv_class_grouped_wk2.df, Class ~ pt+effluent, value = "sum")
row.names(asv_class_grouped_wk2_wide) <- asv_class_grouped_wk2_wide$Class
asv_class_grouped_wk2_wide <- asv_class_grouped_wk2_wide[,2:13]
asv_class_grouped_wk2_wide[is.na(asv_class_grouped_wk2_wide)] <- min(asv_class_grouped_wk2_wide, na.rm=TRUE)/2 #replace nas with min/2
#coerce to matrix
temp_colnames <- colnames(asv_class_grouped_wk2_wide)
temp_rownames <- rownames(asv_class_grouped_wk2_wide)
asv_class_grouped_wk2_wide <- as.matrix(asv_class_grouped_wk2_wide)
colnames(asv_class_grouped_wk2_wide) <- temp_colnames
rownames(asv_class_grouped_wk2_wide) <- temp_rownames
#create annotation dataframe
colnames(asv_class_grouped_wk2_wide)
annotate_class <- data.frame(
  plastic_type = c("Glass, Water (Absent)", "Glass, Water (Absent)", "LDPE, HDPE (Low dens)", "LDPE, HDPE (Low dens)", 
                   "LDPE, HDPE (Low dens)", "LDPE, HDPE (Low dens)", 
                   "PP, PS (High dens)", "PP, PS (High dens)", "PP, PS (High dens)", "PP, PS (High dens)", 
                   "Glass, Water (Absent)", "Glass, Water (Absent)"),
  water_type = c("River", "Treated","River", "Treated","River", "Treated","River", "Treated","River", "Treated","River", "Treated"),
  row.names = colnames(asv_class_grouped_wk2_wide))
annotate_class$plastic_type <- as.factor(annotate_class$plastic_type)
#annotate_class$water_type <- as.factor(annotate_class$water_type)
#View(annotate_class)
typeof(annotate_class)
dim(annotate_class)
#View(asv_class_grouped_wk2_wide)
colnames(asv_class_grouped_wk2_wide)
phm_class_2 <- pheatmap(log(asv_class_grouped_wk2_wide),
                         annotation=annotate_class, legend=F, annotation_legend=F,
                         main="Week 2")

#week 6, class
asv_class_grouped_wk6 <- 
  asv_class[heatmap_these_rows_wk6,] %>%
  group_by(effluent, pt, Class) %>%
  summarise(sum = mean(Abundance)) #, n = n()
asv_class_grouped_wk6
dim(asv_class_grouped_wk6)
colnames(asv_class_grouped_wk6)
asv_class_grouped_wk6.df <- as.data.frame(asv_class_grouped_wk6)
asv_class_grouped_wk6.df$effluent <- as.factor(asv_class_grouped_wk6.df$effluent)
asv_class_grouped_wk6.df$pt <- as.factor(asv_class_grouped_wk6.df$pt)
asv_class_grouped_wk6.df$Class <- as.factor(asv_class_grouped_wk6.df$Class)
#View(asv_class_grouped_wk6.df)
summary(asv_class_grouped_wk6.df)
hist(asv_class_grouped_wk6.df$sum)
asv_class_grouped_wk6_wide <- dcast(asv_class_grouped_wk6.df, Class ~ pt+effluent, value = "sum")
row.names(asv_class_grouped_wk6_wide) <- asv_class_grouped_wk6_wide$Class
asv_class_grouped_wk6_wide <- asv_class_grouped_wk6_wide[,2:13]
asv_class_grouped_wk6_wide[is.na(asv_class_grouped_wk6_wide)] <- min(asv_class_grouped_wk6_wide, na.rm=TRUE)/2 #replace nas with min/2
#coerce to matrix
temp_colnames <- colnames(asv_class_grouped_wk6_wide)
temp_rownames <- rownames(asv_class_grouped_wk6_wide)
asv_class_grouped_wk6_wide <- as.matrix(asv_class_grouped_wk6_wide)
colnames(asv_class_grouped_wk6_wide) <- temp_colnames
rownames(asv_class_grouped_wk6_wide) <- temp_rownames
#create annotation dataframe
colnames(asv_class_grouped_wk6_wide)
annotate_class <- data.frame(
  plastic_type = c("Glass, Water (Absent)", "Glass, Water (Absent)", "LDPE, HDPE (Low dens)", "LDPE, HDPE (Low dens)", 
                   "LDPE, HDPE (Low dens)", "LDPE, HDPE (Low dens)", 
                   "PP, PS (High dens)", "PP, PS (High dens)", "PP, PS (High dens)", "PP, PS (High dens)", 
                   "Glass, Water (Absent)", "Glass, Water (Absent)"),
  water_type = c("River", "Treated","River", "Treated","River", "Treated","River", "Treated","River", "Treated","River", "Treated"),
  row.names = colnames(asv_class_grouped_wk6_wide))
annotate_class$plastic_type <- as.factor(annotate_class$plastic_type)
#annotate_class$water_type <- as.factor(annotate_class$water_type)
#View(annotate_class)
typeof(annotate_class)
dim(annotate_class)
#View(asv_class_grouped_wk6_wide)
colnames(asv_class_grouped_wk6_wide)
phm_class_6 <- pheatmap(log(asv_class_grouped_wk6_wide),
                        annotation=annotate_class, legend=F, annotation_legend=F,
                        main="Week 6")

#week 10, class
asv_class_grouped_wk10 <- 
  asv_class[heatmap_these_rows_wk10,] %>%
  group_by(effluent, pt, Class) %>%
  summarise(sum = mean(Abundance)) #, n = n()
asv_class_grouped_wk10
dim(asv_class_grouped_wk10)
colnames(asv_class_grouped_wk10)
asv_class_grouped_wk10.df <- as.data.frame(asv_class_grouped_wk10)
asv_class_grouped_wk10.df$effluent <- as.factor(asv_class_grouped_wk10.df$effluent)
asv_class_grouped_wk10.df$pt <- as.factor(asv_class_grouped_wk10.df$pt)
asv_class_grouped_wk10.df$Class <- as.factor(asv_class_grouped_wk10.df$Class)
#View(asv_class_grouped_wk10.df)
summary(asv_class_grouped_wk10.df)
hist(asv_class_grouped_wk10.df$sum)
asv_class_grouped_wk10_wide <- dcast(asv_class_grouped_wk10.df, Class ~ pt+effluent, value = "sum")
row.names(asv_class_grouped_wk10_wide) <- asv_class_grouped_wk10_wide$Class
asv_class_grouped_wk10_wide <- asv_class_grouped_wk10_wide[,2:13]
asv_class_grouped_wk10_wide[is.na(asv_class_grouped_wk10_wide)] <- min(asv_class_grouped_wk10_wide, na.rm=TRUE)/2 #replace nas with min/2
#coerce to matrix
temp_colnames <- colnames(asv_class_grouped_wk10_wide)
temp_rownames <- rownames(asv_class_grouped_wk10_wide)
asv_class_grouped_wk10_wide <- as.matrix(asv_class_grouped_wk10_wide)
colnames(asv_class_grouped_wk10_wide) <- temp_colnames
rownames(asv_class_grouped_wk10_wide) <- temp_rownames
#create annotation dataframe
colnames(asv_class_grouped_wk10_wide)
annotate_class <- data.frame(
  plastic_type = c("Glass, Water (Absent)", "Glass, Water (Absent)", "LDPE, HDPE (Low dens)", "LDPE, HDPE (Low dens)", 
                   "LDPE, HDPE (Low dens)", "LDPE, HDPE (Low dens)", 
                   "PP, PS (High dens)", "PP, PS (High dens)", "PP, PS (High dens)", "PP, PS (High dens)", 
                   "Glass, Water (Absent)", "Glass, Water (Absent)"),
  water_type = c("River", "Treated","River", "Treated","River", "Treated","River", "Treated","River", "Treated","River", "Treated"),
  row.names = colnames(asv_class_grouped_wk10_wide))
annotate_class$plastic_type <- as.factor(annotate_class$plastic_type)
#annotate_class$water_type <- as.factor(annotate_class$water_type)
#View(annotate_class)
typeof(annotate_class)
dim(annotate_class)
#View(asv_class_grouped_wk2_wide)
colnames(asv_class_grouped_wk10_wide)
phm_class_10 <- pheatmap(log(asv_class_grouped_wk10_wide),
                          annotation=annotate_class, 
                          main="Week 10")


gridExtra::grid.arrange(grobs=list(phm_class_2$gtable, phm_class_6$gtable, phm_class_10$gtable), 
                        ncol= 3, widths=c(1,1,1.43),labels=LETTERS[1:3])

asv_class_heatmaps_filename <- paste(eytch_graphics,"/eytch_asv_class_heatmaps.jpg",sep="")
jpeg(asv_class_heatmaps_filename, width = 18, height = 9, units = "in",res=300)
  gridExtra::grid.arrange(grobs=list(phm_class_2$gtable, phm_class_6$gtable, phm_class_10$gtable), 
                        ncol= 3, widths=c(1,1,1.43),labels=LETTERS[1:3])
dev.off()


###########################
###########################
###########################
###########################
#week 2, phylum
asv_phylum_grouped_wk2 <- 
  asv_class[heatmap_these_rows_wk2,] %>%
  group_by(effluent, pt, Phylum) %>%
  summarise(sum = sum(Abundance)) #, n = n()
asv_phylum_grouped_wk2
dim(asv_phylum_grouped_wk2)
colnames(asv_phylum_grouped_wk2)
asv_phylum_grouped_wk2.df <- as.data.frame(asv_phylum_grouped_wk2)
asv_phylum_grouped_wk2.df$effluent <- as.factor(asv_phylum_grouped_wk2.df$effluent)
asv_phylum_grouped_wk2.df$pt <- as.factor(asv_phylum_grouped_wk2.df$pt)
asv_phylum_grouped_wk2.df$Phylum <- as.factor(asv_phylum_grouped_wk2.df$Phylum)
#View(asv_phylum_grouped_wk2.df)
summary(asv_phylum_grouped_wk2.df)
#hist(asv_phylum_grouped_wk2.df$sum)
asv_phylum_grouped_wk2_wide <- dcast(asv_phylum_grouped_wk2.df, Phylum ~ pt+effluent, value = "sum")
row.names(asv_phylum_grouped_wk2_wide) <- asv_phylum_grouped_wk2_wide$Phylum
asv_phylum_grouped_wk2_wide <- asv_phylum_grouped_wk2_wide[,2:13]
asv_phylum_grouped_wk2_wide[is.na(asv_phylum_grouped_wk2_wide)] <- min(asv_phylum_grouped_wk2_wide, na.rm=TRUE)/2 #replace nas with min/2
#coerce to matrix
temp_colnames <- colnames(asv_phylum_grouped_wk2_wide)
temp_rownames <- rownames(asv_phylum_grouped_wk2_wide)
asv_phylum_grouped_wk2_wide <- as.matrix(asv_phylum_grouped_wk2_wide)
colnames(asv_phylum_grouped_wk2_wide) <- temp_colnames
rownames(asv_phylum_grouped_wk2_wide) <- temp_rownames
#create annotation dataframe
colnames(asv_phylum_grouped_wk2_wide)
annotate_phylum <- data.frame(
  plastic_type = c("Glass, Water (Absent)", "Glass, Water (Absent)", "LDPE, HDPE (Low dens)", "LDPE, HDPE (Low dens)", 
                   "LDPE, HDPE (Low dens)", "LDPE, HDPE (Low dens)", 
                   "PP, PS (High dens)", "PP, PS (High dens)", "PP, PS (High dens)", "PP, PS (High dens)", 
                   "Glass, Water (Absent)", "Glass, Water (Absent)"),
  water_type = c("River", "Treated","River", "Treated","River", "Treated","River", "Treated","River", "Treated","River", "Treated"),
  row.names = colnames(asv_phylum_grouped_wk2_wide))
annotate_phylum$plastic_type <- as.factor(annotate_phylum$plastic_type)
#annotate_phylum$water_type <- as.factor(annotate_phylum$water_type)
#View(annotate_phylum)
typeof(annotate_phylum)
dim(annotate_phylum)
#View(asv_class_grouped_wk2_wide)
colnames(asv_phylum_grouped_wk2_wide)
phm_phylum_2 <- pheatmap(log(asv_phylum_grouped_wk2_wide),
                         annotation=annotate_phylum, legend=F, annotation_legend=F,
                         main="Week 2")



#week 6, phylum
asv_phylum_grouped_wk6 <- 
  asv_class[heatmap_these_rows_wk6,] %>%
  group_by(effluent, pt, Phylum) %>%
  summarise(sum = sum(Abundance)) #, n = n()
asv_phylum_grouped_wk6
dim(asv_phylum_grouped_wk6)
colnames(asv_phylum_grouped_wk6)
asv_phylum_grouped_wk6.df <- as.data.frame(asv_phylum_grouped_wk6)
asv_phylum_grouped_wk6.df$effluent <- as.factor(asv_phylum_grouped_wk6.df$effluent)
asv_phylum_grouped_wk6.df$pt <- as.factor(asv_phylum_grouped_wk6.df$pt)
asv_phylum_grouped_wk6.df$Phylum <- as.factor(asv_phylum_grouped_wk6.df$Phylum)
#View(asv_phylum_grouped_wk6.df)
summary(asv_phylum_grouped_wk6.df)
hist(asv_phylum_grouped_wk6.df$sum)
asv_phylum_grouped_wk6_wide <- dcast(asv_phylum_grouped_wk6.df, Phylum ~ pt+effluent, value = "sum")
row.names(asv_phylum_grouped_wk6_wide) <- asv_phylum_grouped_wk6_wide$Phylum
asv_phylum_grouped_wk6_wide <- asv_phylum_grouped_wk6_wide[,2:13]
asv_phylum_grouped_wk6_wide[is.na(asv_phylum_grouped_wk6_wide)] <- min(asv_phylum_grouped_wk6_wide, na.rm=TRUE)/2 #replace nas with min/2
#coerce to matrix
temp_colnames <- colnames(asv_phylum_grouped_wk6_wide)
temp_rownames <- rownames(asv_phylum_grouped_wk6_wide)
asv_phylum_grouped_wk6_wide <- as.matrix(asv_phylum_grouped_wk6_wide)
colnames(asv_phylum_grouped_wk6_wide) <- temp_colnames
rownames(asv_phylum_grouped_wk6_wide) <- temp_rownames
#create annotation dataframe
colnames(asv_phylum_grouped_wk6_wide)
annotate_phylum <- data.frame(
  plastic_type = c("Glass, Water (Absent)", "Glass, Water (Absent)", "LDPE, HDPE (Low dens)", "LDPE, HDPE (Low dens)", 
                   "LDPE, HDPE (Low dens)", "LDPE, HDPE (Low dens)", 
                   "PP, PS (High dens)", "PP, PS (High dens)", "PP, PS (High dens)", "PP, PS (High dens)", 
                   "Glass, Water (Absent)", "Glass, Water (Absent)"),
  water_type = c("River", "Treated","River", "Treated","River", "Treated","River", "Treated","River", "Treated","River", "Treated"),
  row.names = colnames(asv_phylum_grouped_wk6_wide))
annotate_phylum$plastic_type <- as.factor(annotate_phylum$plastic_type)
#annotate_phylum$water_type <- as.factor(annotate_phylum$water_type)
#View(annotate_phylum)
typeof(annotate_phylum)
dim(annotate_phylum)
#View(asv_class_grouped_wk2_wide)
colnames(asv_phylum_grouped_wk6_wide)
phm_phylum_6 <- pheatmap(log(asv_phylum_grouped_wk6_wide),
                         annotation=annotate_phylum, legend=F,annotation_legend=F,
                         main="Week 6")

#week 10, phylum
asv_phylum_grouped_wk10 <- 
  asv_class[heatmap_these_rows_wk10,] %>%
  group_by(effluent, pt, Phylum) %>%
  summarise(sum = sum(Abundance)) #, n = n()
asv_phylum_grouped_wk10
dim(asv_phylum_grouped_wk10)
colnames(asv_phylum_grouped_wk10)
asv_phylum_grouped_wk10.df <- as.data.frame(asv_phylum_grouped_wk10)
asv_phylum_grouped_wk10.df$effluent <- as.factor(asv_phylum_grouped_wk10.df$effluent)
asv_phylum_grouped_wk10.df$pt <- as.factor(asv_phylum_grouped_wk10.df$pt)
asv_phylum_grouped_wk10.df$Phylum <- as.factor(asv_phylum_grouped_wk10.df$Phylum)
#View(asv_phylum_grouped_wk10.df)
summary(asv_phylum_grouped_wk10.df)
hist(asv_phylum_grouped_wk10.df$sum)
asv_phylum_grouped_wk10_wide <- dcast(asv_phylum_grouped_wk10.df, Phylum ~ pt+effluent, value = "sum")
row.names(asv_phylum_grouped_wk10_wide) <- asv_phylum_grouped_wk10_wide$Phylum
asv_phylum_grouped_wk10_wide <- asv_phylum_grouped_wk10_wide[,2:13]
asv_phylum_grouped_wk10_wide[is.na(asv_phylum_grouped_wk10_wide)] <- min(asv_phylum_grouped_wk10_wide, na.rm=TRUE)/2 #replace nas with min/2
#coerce to matrix
temp_colnames <- colnames(asv_phylum_grouped_wk10_wide)
temp_rownames <- rownames(asv_phylum_grouped_wk10_wide)
asv_phylum_grouped_wk10_wide <- as.matrix(asv_phylum_grouped_wk10_wide)
colnames(asv_phylum_grouped_wk10_wide) <- temp_colnames
rownames(asv_phylum_grouped_wk10_wide) <- temp_rownames
#create annotation dataframe
colnames(asv_phylum_grouped_wk10_wide)
annotate_phylum <- data.frame(
  plastic_type = c("Glass, Water (Absent)", "Glass, Water (Absent)", "LDPE, HDPE (Low dens)", "LDPE, HDPE (Low dens)", 
                   "LDPE, HDPE (Low dens)", "LDPE, HDPE (Low dens)", 
                   "PP, PS (High dens)", "PP, PS (High dens)", "PP, PS (High dens)", "PP, PS (High dens)", 
                   "Glass, Water (Absent)", "Glass, Water (Absent)"),
  water_type = c("River", "Treated","River", "Treated","River", "Treated","River", "Treated","River", "Treated","River", "Treated"),
  row.names = colnames(asv_phylum_grouped_wk10_wide))
annotate_phylum$plastic_type <- as.factor(annotate_phylum$plastic_type)
#annotate_phylum$water_type <- as.factor(annotate_phylum$water_type)
#View(annotate_phylum)
typeof(annotate_phylum)
dim(annotate_phylum)
#View(asv_class_grouped_wk2_wide)
colnames(asv_phylum_grouped_wk10_wide)
phm_phylum_10 <- pheatmap(log(asv_phylum_grouped_wk10_wide),
                         annotation=annotate_phylum, 
                         main="Week 10")


gridExtra::grid.arrange(grobs=list(phm_phylum_2$gtable, phm_phylum_6$gtable, phm_phylum_10$gtable), 
                        ncol= 3, widths=c(1,1,1.43),labels=LETTERS[1:3])

asv_phylum_heatmaps_filename <- paste(eytch_graphics,"/eytch_asv_phylum_heatmaps.jpg",sep="")
jpeg(asv_phylum_heatmaps_filename, width = 19, height = 9, units = "in",res=300)
  gridExtra::grid.arrange(grobs=list(phm_phylum_2$gtable, phm_phylum_6$gtable, phm_phylum_10$gtable), 
                        ncol= 3, widths=c(1,1,1.43),labels=LETTERS[1:3])
dev.off()
