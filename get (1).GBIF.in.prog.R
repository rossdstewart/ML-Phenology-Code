### get South Africa Research Grade plants from GBIF ###

setwd("/home/ross/data/ML_paper_coding/")
library(data.table)
library(stringr)

S.A.RG.iNat.Plantae <- fread("/home/ross/data/ML_paper_coding/GBIF.Plantae.SAfr.ResGrad.iNat/occurrence.txt")
S.A.RG.iNat.Plantae<-as.data.frame(S.A.RG.iNat.Plantae)
multimedia<-fread("/home/ross/data/ML_paper_coding/GBIF.Plantae.SAfr.ResGrad.iNat/multimedia.txt")
multimedia<-as.data.frame(multimedia)
plist<-read.csv("/home/ross/data/ML_paper_coding/All plants no dups.csv")

#verbatimScientific name doesn't have "subsp.", remove this.
plist<-str_remove_all(plist$Species, "subsp. ")
#subset GBIF entries to species list.
SARGIP.pl<-S.A.RG.iNat.Plantae[S.A.RG.iNat.Plantae$verbatimScientificName %in% plist,]
mm.pl<-multimedia[multimedia$gbifID %in% SARGIP.pl$gbifID,]
#Add species names to dataframe.
mm.pl<-cbind(mm.pl, "verbatimScientificName"=SARGIP.pl$verbatimScientificName[match(mm.pl$gbifID, SARGIP.pl$gbifID)])
#sub spaces for underscores
mm.pl$verbatimScientificName<-as.character(gsub(" ","_",mm.pl$verbatimScientificName))
  
#there are a few photos that are used for two different species. Drop these.
mm.pl.u<-mm.pl[!duplicated(mm.pl$identifier),]
#remove photos with missing links
mm.pl.u<-mm.pl.u[!mm.pl.u$identifier=="",]

#NOTE: This will download everything in format "G.[GBIF.ID].id.[iNaturalist.img.URL.number].jpg" format. all other metadata can be referenced in the "occurrence.txt" file, subset for this dataset
# as SARGIP.pl. I will work on generating a clean metadata file to match the downloads next.

for (i in mm.pl.u[,4]){
  GBIF.id <- mm.pl.u[which(i==mm.pl.u[,4]),1]
  verbatimScientificName<-mm.pl.u[which(i==mm.pl.u[,4]),16]
  download.file(i, paste0("for_phenology/", verbatimScientificName, ".G.", GBIF.id, ".id.", str_remove(str_remove(i, "https://inaturalist-open-data.s3.amazonaws.com/photos/"), "/original")))
}
