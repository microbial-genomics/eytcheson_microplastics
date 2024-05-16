sessionInfo()

library(dplyr)
library(factoextra)
library(reshape2)
library(ggplot2)
library(plotly)
library(pheatmap)
library(gridExtra)
library(RColorBrewer)
library(ggpubr)
source("https://raw.githubusercontent.com/joey711/phyloseq/master/inst/scripts/installer.R",
       local = TRUE)
library(phyloseq)
library(phyloseqGraphTest)
library(vegan)

## setup root directory path
if(Sys.info()[4]=="DZ2626UTPURUCKE"){
  # tom epa windows
  eytch_root <- file.path("c:", "git", "eytcheson_microplastics")
} else if (Sys.info()[4]=="LZ26TPURUCKE-2"){ 
  # tom windows 
  eytch_root <- file.path("C:", "Users", "tpurucke", "git", "eytcheson_microplastics")
} else if(Sys.info()[1]=="Linux" & dir.exists(file.path("","work","BIOMARK","ww2dw","ww2dw"))){
  # hpc linux BIOMARK
  eytch_root <- file.path("","work","MICROPLASTICS","eytcheson_microplastics")
}

print(paste("Root directory location: ", eytch_root, sep=""))

eytch_data_in <- file.path(eytch_root, "data_in")
eytch_data_raw <- file.path(eytch_root, "data_raw")
eytch_data_out <- file.path(eytch_root, "data_out")
eytch_graphics <- file.path(eytch_root, "graphics")

print("list of loaded packages: ")
print((.packages()))
