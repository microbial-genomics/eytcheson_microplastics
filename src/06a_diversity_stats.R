#View(asv_class)
#reduce esf_otu by dropping otus
summary(asv_class)
dim(asv_class)
#rowSums(asv_class[,2:152])

#View(asv_class)
colnames(asv_class)
# "X"         "ASV"       "Sample"    "Abundance" "sampleid"  "effluent"  "type"      "wk"        "pt"        "Kingdom"  
# "Phylum"    "Class"  
unique(asv_class$wk) # "wk02" "wk06" "wk10" "wk0"
unique(asv_class$effluent) # "CON" "TWW"
unique(asv_class$type) # "CON"   "TWW"   "WATER"
unique(asv_class$pt) # "PS"    "PP"    "HDPE"  "Glass" "LDPE"  "water"
unique(asv_class$sample)
unique(asv_class$sampleid)
unique(asv_class$Class)
       
#heatmap_these_rows <- subset_these_rows
heatmap_these_rows_wk2 <- which(asv_class$wk=="wk02")
heatmap_these_rows_wk6 <- which(asv_class$wk=="wk06")
heatmap_these_rows_wk10 <- which(asv_class$wk=="wk10")

#week 2, class
asv_class_abundance_wk2 <- 
  asv_class[heatmap_these_rows_wk2,] %>%
  group_by(effluent, pt, Class, sampleid) 
#%>%
#summarise(sum = mean(Abundance)) #, n = n()
#View(asv_class_abundance_wk2)
dim(asv_class_abundance_wk2)
colnames(asv_class_abundance_wk2)

asv_class_abundance_wk2.df <- as.data.frame(asv_class_abundance_wk2)
asv_class_abundance_wk2.df$effluent <- as.factor(asv_class_abundance_wk2.df$effluent)
asv_class_abundance_wk2.df$pt <- as.factor(asv_class_abundance_wk2.df$pt)
asv_class_abundance_wk2.df$Class <- as.factor(asv_class_abundance_wk2.df$Class)
asv_class_abundance_wk2.df$Sample <- as.factor(asv_class_abundance_wk2.df$Sample)

#View(asv_class_grouped_wk2.df)
summary(asv_class_abundance_wk2.df)
colnames(asv_class_abundance_wk2.df)
hist(asv_class_abundance_wk2.df$Abundance)
#cast replaced by acast for array and dcast for dataframe
length(unique(asv_class_abundance_wk2.df$Class))
length(unique(asv_class_abundance_wk2.df$Sample))
asv_class_abundance_wk2_wide <- dcast(asv_class_abundance_wk2, Sample+pt+effluent~Class, value.var = "Abundance")
dim(asv_class_abundance_wk2_wide)
colnames(asv_class_abundance_wk2_wide)
rownames(asv_class_abundance_wk2_wide)
View(asv_class_abundance_wk2_wide)

#create phyloseq data object
# https://joey711.github.io/phyloseq/import-data.html
# phyloseq asv matrix object
asv_classes <- colnames(asv_class_abundance_wk2_wide)[4:69]
asv_samples <- asv_class_abundance_wk2_wide$Sample
asv_wk2 <- t(asv_class_abundance_wk2_wide[,4:69])
colnames(asv_wk2) <- asv_samples
asv_wk2[is.na(asv_wk2)] <- 0
dim(asv_wk2)
class(asv_wk2)
asv_wk2
asv_wk2_table = otu_table(asv_wk2, taxa_are_rows = TRUE)
dim(asv_wk2_table)
taxa_names(asv_wk2_table)
# phyloseq taxa table
#View(asv_taxa)
dim(asv_taxa)
colnames(asv_taxa)
class(asv_taxa)
asv_taxa2 <- as.matrix(asv_taxa)
View(asv_taxa2)

asv_physeq_taxa_table <- tax_table(asv_taxa2[,2:8])
taxa_names(asv_physeq_taxa_table)
dim(asv_physeq_taxa_table)
#tax_table - Works on any character matrix. The rownames must match the OTU names 
#(taxa_names) of the otu_table if you plan to combine it with a phyloseq-object.
rownames(asv_physeq_taxa_table) 
asv_physeq_wk2 = phyloseq(asv_wk2_table,asv_physeq_taxa_table)
asv_physeq_wk2
plot_bar(asv_physeq_wk2, fill = "Phylum")
