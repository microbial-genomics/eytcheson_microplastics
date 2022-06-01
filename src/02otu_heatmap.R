
#reduce esf_otu by dropping otus
summary(esf_otu)
dim(esf_otu)
rowSums(esf_otu[,2:152])

# reshape with melt
esf_otu_melt <- melt(esf_otu[1:104,])
head(esf_otu_melt)
summary(esf_otu_melt)
esf_otu_melt$ln_count <- log(esf_otu_melt$value+0.1)
esf_otu_melt$log_count <- log10(esf_otu_melt$value+0.1)
head(esf_otu_melt)
esf_otu_melt$sampleid <- esf_otu_melt$variable

#merge esf_design with esf_otu
head(esf_otu_melt)
is.data.frame(esf_otu_melt[,c(1,3:6)])
head(esf_design)
is.data.frame(esf_design)
esf_otu_melt_meta <- merge(esf_otu_melt[,c(1,3:6)],esf_design,by=c("sampleid"),all.x=T)
View(esf_otu_melt_meta)
summary(esf_otu_melt_meta)
unique(esf_otu_melt_meta$type)
unique(esf_otu_melt_meta$effluent)
unique(esf_otu_melt_meta$wk)
unique(esf_otu_melt_meta$pt) # "HDPE"  "LDPE"  "PP"    "Glass" "PS"    "water"

# pick a week and effluent CON????
week_effluent <- intersect(
  which(esf_otu_melt_meta$wk=="wk10"), # wk0, wk2, wk6, wk10
  which(esf_otu_melt_meta$effluent=="TWW")# TWW
)
# pick types to compare (or pick one type)
pick_types <- union(
  which(esf_otu_melt_meta$type=="Wastewater MP"), # "River MP"      "Wastewater MP" "Wastewater"    "River water"
  which(esf_otu_melt_meta$type=="River MP")
)
# pick types to compare (or pick one type)
pick_plastics <- union(
  which(esf_otu_melt_meta$pt=="HDPE"), # "HDPE"  "LDPE"  "PP"    "Glass" "PS"    "water"
  which(esf_otu_melt_meta$pt=="Glass")
)


subset_these_rows <- intersect(week_effluent, pick_types)
subset_these_rows

heatmap_these_rows <- intersect(subset_these_rows, pick_plastics)
heatmap_these_rows

esf_otu_melt_meta$sampleid_type <- paste0(esf_otu_melt_meta$sampleid,"_",esf_otu_melt_meta$pt)

View(esf_otu_melt_meta[heatmap_these_rows,])
# Create heatmap with ggplot2
summary(esf_otu_melt_meta)
gg_esf <- ggplot(esf_otu_melt_meta[subset_these_rows,], aes(pt, otu_id)) +         
  geom_tile(aes(fill = log_count)) +
  scale_fill_gradient(low = "blue", high = "red")  
gg_esf   
