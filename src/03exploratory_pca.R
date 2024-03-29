

dim(esf_otu)
View(esf_otu[,2:152])
esf_otu_pca <- prcomp(esf_otu[,2:152], scale = TRUE)

#graph of samples ## too many overlaps
fviz_eig(esf_otu_pca)
fviz_pca_ind(esf_otu_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) +     # Avoid text overlapping
    labs(title ="PCA", x = "PC1", y = "PC2")

#graph of pca variables 
fviz_pca_var(esf_otu_pca, #yp_pca
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#pca biplot
fviz_pca_biplot(esf_otu_pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

### this needs to be fixed
groups <- as.factor(esf_otu_pca$tneco3)
fviz_pca_ind(esf_otu_pca,
             col.ind = groups, # color by groups
             palette = display.brewer.pal(n=7, name = "Set1"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

length(groups)