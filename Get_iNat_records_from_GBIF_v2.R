### get iNat South Africa Research Grade plants records from GBIF ###

setwd("/home/ross/data/ML_paper_coding/")
library(data.table)
library("stringr")
library(rgbif)

# The different components of the downloaded DOI (https://doi.org/10.15468/ab3s5x) are read into R 
S.A.RG.iNat.Plantae <- fread("/home/ross/data/ML_paper_coding/GBIF.Plantae.SAfr.ResGrad.iNat/occurrence.txt")
S.A.RG.iNat.Plantae<-as.data.frame(S.A.RG.iNat.Plantae)
multimedia<-fread("/home/ross/data/ML_paper_coding/GBIF.Plantae.SAfr.ResGrad.iNat/multimedia.txt")
multimedia<-as.data.frame(multimedia)

# Original list that was filtered to select the species.
plist<-read.csv("/home/ross/data/ML_paper_coding/All plants no dups.csv")

# Clean verbatim_Scientific to remove "subsp." and "Var. "
plist<-str_remove_all(plist$Species, "subsp. ")
plist<-str_remove_all(plist$Species, "var. ")
# Subset GBIF entries to species list.
SARGIP.pl<-S.A.RG.iNat.Plantae[S.A.RG.iNat.Plantae$verbatimScientificName %in% plist,]
mm.pl<-multimedia[multimedia$gbifID %in% SARGIP.pl$gbifID,]
# Add species names to data frame
mm.pl<-cbind(mm.pl, "verbatimScientificName"=SARGIP.pl$verbatimScientificName[match(mm.pl$gbifID, SARGIP.pl$gbifID)])
# Substitute spaces for underscores
mm.pl$verbatimScientificName<-as.character(gsub(" ","_",mm.pl$verbatimScientificName))
  

# There are a few photos that are used for two different species. Drop these.
mm.pl.u<-mm.pl[!duplicated(mm.pl$identifier),]
# Remove photos with missing links
mm.pl.u<-mm.pl.u[!mm.pl.u$identifier=="",]

# This will download everything in format "G.[GBIF.ID].id.[iNaturalist.img.URL.number].jpg" format. 
#   All other metadata can be referenced in the "occurrence.txt" file. 
for (i in mm.pl.u[,4]){
  GBIF.id <- mm.pl.u[which(i==mm.pl.u[,4]),1]
  verbatimScientificName<-mm.pl.u[which(i==mm.pl.u[,4]),16]
  download.file(i, paste0("for_phenology/", verbatimScientificName, ".G.", GBIF.id, ".id.", str_remove(str_remove(i, "https://inaturalist-open-data.s3.amazonaws.com/photos/"), "/original")))
}


###################################################################################
# Reads in the data frame, creates subset with high Flowering_2 values, makes random selection from subset. 
big_data <- read.csv("Table_S1-1_8_mil_records_for_input_cleaned.csv")
dim(big_data[big_data$Flowering_2>=50,])
a_subset_big_data <- (subset(big_data, Flowering_2>=60)) # creates a subset of a flowering 2 threshold greater than 60
a_subset_big_data2 <- a_subset_big_data[sample(1:nrow(a_subset_big_data),200),] # randomly selects 200 of the subset

a_subset_big_data2 <- big_data[sample(1:nrow(big_data),500),] # randomly selects 200 of the subset
dim(a_subset_big_data2)
a_subset_big_data2$File_Name[1]

download_images_subset(a_subset_big_data2[214:500,]) # runs the function below.

# Download images from a selected subset of the output table (Table s1)  
download_images_subset <- function(supp_table_subset) {
  
  for (j in 1:nrow(supp_table_subset)) {
    the_row <- supp_table_subset[j,]
    print(paste0(the_row$Species," - ",j,"/",nrow(supp_table_subset)))
    print(paste0("F1:",the_row$Flowering," | NF:",the_row$Not_Flowering," | F2:",the_row$Flowering_2))
    
    sp_occ_data <- occ_search(search =the_row$iNAT_ID, mediaType="StillImage", scientificName =the_row$Species)
    
    if (sp_occ_data$meta$count>0) {
      sp_image_url <- as.character(unlist(sp_occ_data$media[[1]])[grep(".identifier",names(unlist(sp_occ_data$media[[1]])))])
      sp_image_url <- sp_image_url[grep(the_row$iNAT_ID, sp_image_url)]
      
      sp_file_name <- the_row$File_Name
      download.file(sp_image_url,paste0(sp_file_name),mode="wb",quiet = TRUE)
    } else if (sp_occ_data$meta$count==0) {
      print("Failed to download")
    }
  }
}
