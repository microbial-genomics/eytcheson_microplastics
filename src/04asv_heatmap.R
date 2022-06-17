#View(asv_class)
#reduce esf_otu by dropping otus
summary(asv_class)
dim(asv_class)
rowSums(asv_class[,2:152])

colnames(asv_class)
unique(asv_class$wk)
unique(asv_class$effluent)
unique(asv_class$type)
unique(asv_class$pt)

# pick a week and effluent CON????
#week_effluent <- intersect(
#  which(asv_class$wk=="wk10"), # wk0, wk2, wk6, wk10
#  which(asv_class$effluent=="TWW")# TWW
#)
# pick types to compare (or pick one type)
#pick_types <- union(
#  which(asv_class$type=="TWW"), #TWW=Wastewater MP??# "River MP"      "Wastewater MP" "Wastewater"    "River water"
#  which(asv_class$type=="WATER")
#)
## pick types to compare (or pick one type) keep all
#pick_plastics <- union(
#  which(asv_class$pt=="water"), # "HDPE"  "LDPE"  "PP"    "Glass" "PS"    "water"
#  which(asv_class$pt=="Glass")
#)

#subset_these_rows <- intersect(week_effluent, pick_types)
#subset_these_rows

#heatmap_these_rows <- subset_these_rows
heatmap_these_rows_wk10 <- which(asv_class$wk=="wk2")
heatmap_these_rows_wk10 <- which(asv_class$wk=="wk10")
heatmap_these_rows_wk10 <- which(asv_class$wk=="wk10")

# asv_class$sampleid_type <- paste0(esf_otu_melt_meta$sampleid,"_",esf_otu_melt_meta$pt)

View(asv_class[heatmap_these_rows,])
# Create heatmap with ggplot2
summary(asv_class)
colnames(asv_class)
asv_class$log_count <- log(asv_class$Abundance)
gg_asv <- ggplot(asv_class[heatmap_these_rows,], aes(pt, ASV)) +         
  geom_tile(aes(fill = log_count)) +
  scale_fill_gradient(low = "blue", high = "red")  
gg_asv   

#week 10, class
asv_class_grouped_wk10 <- 
  asv_class[heatmap_these_rows,] %>%
  group_by(effluent, pt) %>%
  summarise(sum = sum(Abundance), n = n())
asv_class_grouped_wk10
dim(asv_class_grouped_wk10)

#week 10, class
asv_class_grouped_wk10 <- 
  asv_class[heatmap_these_rows,] %>%
    group_by(effluent, pt, Class) %>%
    summarise(sum = sum(Abundance), n = n())
asv_class_grouped_wk10
dim(asv_class_grouped_wk10)

# the sums of above are relative abundance sums and therefore need to be normalized
# to compare on a heatmap

gg_asv_grouped_class_wk10 <- ggplot(asv_class_grouped_wk10, aes(pt, Class)) +         
  geom_tile(aes(fill = log(sum))) +
  scale_fill_gradient(low = "blue", high = "red")  
gg_asv_grouped_wk10

#week 10, phyla
asv_phyla_grouped_wk10 <- 
  asv_class[heatmap_these_rows,] %>%
  group_by(effluent, pt, Phylum) %>%
  summarise(sum = sum(Abundance), n = n())
asv_phyla_grouped_wk10
dim(asv_phyla_grouped_wk10)

# the sums of above are relative abundance sums and therefore need to be normalized
# to compare on a heatmap

gg_asv_grouped_class_wk10 <- ggplot(asv_phyla_grouped_wk10, aes(pt, Phylum)) +         
  geom_tile(aes(fill = log(sum))) +
  scale_fill_gradient(low = "blue", high = "red")  
ggplotly(gg_asv_grouped_class_wk10)
