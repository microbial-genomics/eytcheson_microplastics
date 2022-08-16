### PHYLUM
dim(asv_phylum_grouped_wk10_wide)
#View(asv_phylum_grouped_wk10_wide)

###ind pca of treatments
esf_asv_pca_treatment_wk2 <- prcomp(t(asv_phylum_grouped_wk2_wide), scale = TRUE)
#graph of samples ## too many overlaps
fviz_eig(esf_asv_pca_treatment_wk2)
fviz_treatment_wk2 <-  fviz_pca_ind(esf_asv_pca_treatment_wk2,
                                    col.ind = "cos2", # Color by the quality of representation
                                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                    repel = TRUE) +     # Avoid text overlapping
  labs(title ="Treatment PCA Week 2 (Phylum)", x = "", y = "PC2")
fviz_treatment_wk2

esf_asv_pca_treatment_wk6 <- prcomp(t(asv_phylum_grouped_wk6_wide), scale = TRUE)
#graph of samples ## too many overlaps
fviz_eig(esf_asv_pca_treatment_wk6)
fviz_treatment_wk6 <- fviz_pca_ind(esf_asv_pca_treatment_wk6,
                                   col.ind = "cos2", # Color by the quality of representation
                                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                   repel = TRUE) +     # Avoid text overlapping
  labs(title ="Treatment PCA Week 6", x = "", y = "PC2")
fviz_treatment_wk6

esf_asv_pca_treatment_wk10 <- prcomp(t(asv_phylum_grouped_wk10_wide), scale = TRUE)
#graph of samples ## too many overlaps
fviz_eig(esf_asv_pca_treatment_wk10)
fviz_treatment_wk10 <- fviz_pca_ind(esf_asv_pca_treatment_wk10,
                                    col.ind = "cos2", # Color by the quality of representation
                                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                    repel = TRUE) +     # Avoid text overlapping
  labs(title ="Treatment PCA Week 10", x = "PC1", y = "PC2")
fviz_treatment_wk10

ggarrange(fviz_treatment_wk2, fviz_treatment_wk6, fviz_treatment_wk10, 
          ncol = 1, nrow = 3,
          labels = c("A", "B", "C"),
          heights = c(1, 1, 1))

###ind pca of asvs
esf_asv_pca_wk2 <- prcomp(asv_phylum_grouped_wk2_wide, scale = TRUE)
#graph of samples ## too many overlaps
fviz_eig(esf_asv_pca_wk2)
fviz_asv_wk2 <-  fviz_pca_ind(esf_asv_pca_wk2,
                              col.ind = "cos2", # Color by the quality of representation
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                              repel = TRUE) +     # Avoid text overlapping
  labs(title ="ASV PCA Week 2 (Phylum)", x = "", y = "")
fviz_asv_wk2

esf_asv_pca_wk6 <- prcomp(asv_phylum_grouped_wk6_wide, scale = TRUE)
#graph of samples ## too many overlaps
fviz_eig(esf_asv_pca_wk6)
fviz_asv_wk6 <- fviz_pca_ind(esf_asv_pca_wk6,
                             col.ind = "cos2", # Color by the quality of representation
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                             repel = TRUE) +     # Avoid text overlapping
  labs(title ="ASV PCA Week 6", x = "", y = "")
fviz_asv_wk6

esf_asv_pca_wk10 <- prcomp(asv_phylum_grouped_wk10_wide, scale = TRUE)
#graph of samples ## too many overlaps
fviz_eig(esf_asv_pca_wk10)
fviz_asv_wk10 <- fviz_pca_ind(esf_asv_pca_wk10,
                              col.ind = "cos2", # Color by the quality of representation
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                              repel = TRUE) +     # Avoid text overlapping
  labs(title ="ASV PCA Week 10", x = "PC1", y = "")
fviz_asv_wk10

ggarrange(fviz_asv_wk2, fviz_asv_wk6, fviz_asv_wk10, 
          ncol = 1, nrow = 3,
          labels = c("A", "B", "C"),
          heights = c(1, 1, 1))

ggarrange(fviz_treatment_wk2, fviz_asv_wk2, 
          fviz_treatment_wk6,fviz_asv_wk6, 
          fviz_treatment_wk10, fviz_asv_wk10,
          ncol = 2, nrow = 3,
          labels = c("A", "B", "C", "D", "E", "F"),
          heights = c(1, 1, 1, 1, 1, 1),
          widths = c(1, 1, 1, 1, 1, 1))
