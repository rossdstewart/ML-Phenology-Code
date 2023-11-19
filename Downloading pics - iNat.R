

######
# Downloading pics 
library(rinat)
library(stringr)

setwd("C:/Users/rossd/Desktop/R/Flowering ML/iNat/Iridaceae")

sp_list <- read.csv("unq_sp_list_no_grasses.csv")[,1]



set.seed(2023)

length(sp_list)
random_sp2 <-sample(sp_list,2500)



write.csv(random_sp2,"random_sp3.csv")


random_sp <- read.csv("random_sp3.csv")[,2]

#random_sp <- sp_list[-which(sp_list %in% random_sp)]


which(random_sp==i)
i <- random_sp[22]
for (i in random_sp[83:300]) {
  
  iNat_searched <- NULL
  bo <- 0
  while(bo!=3){
    x = try(iNat_searched <- get_inat_obs(taxon_name = "Iridaceae", quality = "research", place_id = "south-africa", maxresults = 10000)) ## has a problem with too many species 
    if (class(x)=="try-error") {
      cat("ERROR1: ", x, "\n")
      Sys.sleep(1)
      print("reconnecting...")
      bo <- bo+1
      print(bo)
    } else {
      break
    } 
  }
  iNat_searched$id
  length(iNat_searched$id)
  
  if (!is.null(iNat_searched)) {
    plant_ID <- NULL
    
    if (length(iNat_searched$id)>=4) {
      plant_ID <- sample(iNat_searched$id,4) # set the number to be downloaded
    } else {
      plant_ID <- iNat_searched$id
    }
    
    
    for (the_ID in plant_ID) {
      #179509399
      sp_image_url <- iNat_searched$image_ur[which(the_ID==iNat_searched$id)] # will download medium images
      sp_date <- str_extract(iNat_searched$datetime[which(iNat_searched$id==the_ID)],"([^\\s]+)")
      
      #
      i <- iNat_searched$scientific_name[which(the_ID==iNat_searched$id)]
      #random_sp <- iNat_searched$scientific_name
      
      sp_file_name <- paste0(sp_date,"_",gsub(" ","_",i), "_",the_ID,".jpg")
      
      #print(paste0(which(i==random_sp),"/",length(random_sp)," || ", which(the_ID==plant_ID),"/",length(plant_ID)," - " , sp_file_name))
      
      # downloads the picture and names it
      bo <- 0
      while(bo!=3){
        x = try(download.file(sp_image_url,paste0("./Training/",sp_file_name),mode="wb",quiet = TRUE),silent=TRUE)
        if (class(x)=="try-error") {
          cat("can't downlaod - ERROR1: ", x, "\n")
          Sys.sleep(1)
          print("reconnecting...")
          bo <- bo+1
          print(bo)
        } else {
          break
        } 
      }
    }
  } else {
    print(paste0("Can't find: ", i, " - ", paste0(which(i==random_sp),"/",length(random_sp)) ) )
  }
    
}


install.packages("filesstrings")
library(filesstrings)



not_flowering_files <- list.files(path="./Flower + Not/Training/Not Flowering/",pattern = ".jpg")


file.move(paste0("./Flower + Not/Training/Not Flowering/",sample(not_flowering_files,437)),destinations = "Other")


flowering_files <- paste0("./Flower + Not/Training/Flowering/",sample(list.files(path="./Flower + Not/Training/Flowering/",pattern = ".jpg"),6))
file.move(flowering_files,destinations = "./Flower + Not/Other")



other_files <-  paste0("./Flower + Not/Testing/Flowering/",sample(list.files(path="./Flower + Not/Testing/Flowering/",pattern = ".jpg"),6071))
file.move(other_files,destinations = "./Flower + Not/Other")


