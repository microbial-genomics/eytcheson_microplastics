# xlsx files from Erik Pilgrim

# 10/20/21
# updated version of the OTU table with Taxonomy added, the OTUs so that the ones that had less 
# than 10 reads are at the bottom in red (now marked as having 0 reads due to my editing process)
# ESFmicroplastic_515F-806R_cluster__otu_table_v2.xlsx
# ESFmicroplastics_515F-806R_OTU_Taxonomy.xlsx
# taxonomy SILVA based and RDP 

# 10/19/21
# bioinformatics results for the first primer set (515F/806R) MiSeq run, but strange results 
# from the other MiSeq run, so Erik is still working on that.
# Erik has attached the OTU table and OTU consensus sequence files from that first run, but 
# they’re also in that folder on the O Drive. Erik has not done any taxonomy by comparing to SILVA, 
# but can. For the OTU table, in the “Edited” worksheet Erik removed all instances on <10 reads for 
# any OTU from the table. The other worksheet is the raw data.
# ESFmicroplastic_515F-806R_cluster__otu_table_v2.xlsx
# ESFmicroplastic_515F-806R_otus.fa

# ESFmicroplastic_515F-806R_cluster__otu_table_v2.xlsx -- sheet esf_cluster_otu_table 
# saved as esf_cluster_otu_table.txt tab-delimited file

# 6/8/2022 From Stephanie to Maite
# I found this ASV table which has class information in Maitê’s folder on the O: drive 
# (CEMM_Microplastics/Pilgrim2021/MaiteAnalysis/DADA2 results/asv_table_w_taxa_species_v1.csv). 
# The only thing I am unsure about is that this file has a sequence instead of an identifier 
# (e.g. OTU001) in the first column, and there are many more ASVs than OTUs (34208 vs 5402, respectively). 
#
# Maitê, I think you and Huiyun made ASV tables; do you have one that links to the MP data (i.e. material, 
# collection time, etc) and provides information down to the class level?
  
esf_otu_filename <- file.path(eytch_data_in,"esf_cluster_otu_table.txt")

#check to see if directories are accessible
file_check = file.exists(esf_otu_filename)
print(paste("check to see if R can access files OK: ", file_check))
esf_otu <- read.table(esf_otu_filename, sep="\t", header=TRUE)
#View(esf_otu)
dim(esf_otu)
head(esf_otu)
rownames(esf_otu)
summary(esf_otu)

# need sample design matrix
sample_names <- as.data.frame(colnames(esf_otu)[-1])
colnames(sample_names) <- "sample_id"
rownames(sample_names) <- NULL
dim(sample_names)

# stephanie shared new labels on 3/22/22
esf_design_filename <- file.path(eytch_data_in,"220317_Metadata_NewLabels_SE.csv")
esf_design <- read.csv(esf_design_filename, header=TRUE)
dim(esf_design)
#View(esf_design)

#export sample_names for design matrix
sample_names_filename <- file.path(eytch_data_out,"esf_sample_names.txt")
write.table(sample_names, sample_names_filename, row.names=F)

#asv files from Maite
#asv classes
asv_class_filename <- file.path(eytch_data_in,"asv_class_level.csv")
#check to see if directories are accessible
file_check = file.exists(asv_class_filename)
print(paste("check to see if R can access files OK: ", file_check))
asv_class <- read.csv(asv_class_filename, header=TRUE)
#View(asv_class)
dim(asv_class)
head(asv_class)
rownames(asv_class)
summary(asv_class)
#View(asv_class)

#asv taxa
asv_taxa_filename <- file.path(eytch_data_in,"asv_table_w_taxa_species_v2.csv")
#check to see if directories are accessible
file_check = file.exists(asv_taxa_filename)
print(paste("check to see if R can access files OK: ", file_check))
asv_taxa <- read.csv(asv_taxa_filename, header=TRUE)
#View(asv_taxa)
dim(asv_taxa)
head(asv_taxa)
rownames(asv_class)
summary(asv_class)
