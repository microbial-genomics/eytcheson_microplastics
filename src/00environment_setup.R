sessionInfo()

## setup root directory path
if(Sys.info()[4]=="DZ2626UTPURUCKE"){
  # tom epa windows
  eytch_root <- file.path("c:", "git", "eytcheson_microplastics")
} else if (Sys.info()[4]=="LZ2626UTPURUCKE"){ 
  # tom windows 
  eytch_root <- file.path("c:","git","eytcheson_microplastics")
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
