####

# loads packages
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
library(lubridate)
require(Hmisc)
library(data.table)
library(Rcpp)
library(NPCirc)

setwd("C:/Users/rossd/Desktop/R/Flowering ML/Shared ML Phenology")


#Load the models
print("loading Fruiting model (weak_fruit2.model)")
fruit_mod <- load_model_hdf5("weak_fruit2.model")
print("Fruiting model loaded")
print("loading Flowering model (robust_1.model)")
flower_mod <- load_model_hdf5("robust_1.model")
print("Flowering model loaded")




location_of_files<-"for_phenology3"
  
the_list_files <- list.files(location_of_files)[which(list.files(location_of_files)%like%".jpg" | 
                                                        list.files(location_of_files)%like%".JPG" | 
                                                        list.files(location_of_files)%like%".jpeg")]
print(paste("Number of files:",length(the_list_files)))
  
 
ML_results <- data.frame()

# creates an empty variable to save the data
results_out <- data.frame()

# For loop for each image in the folder 
# k <- sp_file_names
for (k in the_list_files) {
  
  # Reads and formats the the picture into a relevant size and colour scale
  
  tt <- try(test_image <- image_load(paste0(location_of_files,"/",k), target_size = c(300,300)))
  
  
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x/255 # converts the RGB colours to a percentage
  
  # Runs the primary predictive model on the picture, change the model that is being used
  pred <- round((flower_mod %>% predict(x))*100,3)
  
  # Saves the Primary model results. Note Fruiting is set to zero at this stage 
  prediction_data <- data.frame("File_Name"=k,"Flowering"=pred[,1],"Not_Flowering"=pred[,2],"Fruiting"=0,"Flowering_2"=0)
  
  
  if (prediction_data$Flowering>=50) {
    
    ## Run the secondary model on images that have been ID'd as flowering. This will check if it is a fruit
    pred2 <- round((fruit_mod %>% predict(x))*100,3)
    # Saves the Primary model results. Note Fruiting is set to zero at this stage 
    prediction_data_fruit <- data.frame("Flowering"=pred2[,2],"Fruiting"=pred2[,3]) 
    
    # updates to the new stats to the prediction_data frame 
    prediction_data$Flowering_2 <- prediction_data_fruit$Flowering
    prediction_data$Fruiting <- prediction_data_fruit$Fruiting
    
  }
  
  print(paste(which(k==the_list_files),"/", length(the_list_files)))
  
  
  # Combines the prediction for each picture into a larger data.fame to be saved later
  results_out <- rbind(results_out, prediction_data)
  
}
head(results_out)


nrow(results_out[which(results_out$Flowering_2>=50 & results_out$File_Name %like% "Flower_"),]) # is a flower predicted as a flower
nrow(results_out[which(results_out$Not_Flowering>=50 & results_out$File_Name %like% "Not_"),]) # is a not predicted as a not
nrow(results_out[which(results_out$Fruiting>=50 & results_out$File_Name %like% "Fruit_"),]) # is a fruit predicted a fruit

nrow(results_out[which(results_out$Not_Flowering>=50 & results_out$File_Name %like% "Fruit_"),]) # is a fruit and pridicted as not 
nrow(results_out[which(results_out$Flowering_2>=50 & results_out$File_Name %like% "Fruit_"),]) # is a fruit and pridicted as flower 

nrow(results_out[which(results_out$Not_Flowering>=50 & results_out$File_Name %like% "Flower_"),]) # is a flower and pridicted as not 
nrow(results_out[which(results_out$Fruiting>=50 & results_out$File_Name %like% "Flower_"),]) # is a flower and pridicted as fruit 

nrow(results_out[which(results_out$Flowering>=50 & results_out$File_Name %like% "Not_"),]) # is a not and pridicted as flower 
nrow(results_out[which(results_out$Fruiting>=50 & results_out$File_Name %like% "Not_"),]) # is a not and pridicted as flower 

nrow(results_out[which(results_out$Flowering>=50 & results_out$File_Name %like% "Flower_"),]) # is a not and pridicted as flower 


nrow(results_out[which(results_out$Fruiting>=50 & results_out$File_Name %like% "Not_"),]) # is a not and pridicted as flower 

nrow(results_out[which(results_out$Flowering>=50),])/5000






R.Version()

the_list_files <- list.files()[which(list.files()%like%".jpg" | 
                                                        list.files()%like%".JPG" | 
                                                        list.files()%like%".jpeg")]
length(the_list_files)
old_files <- the_list_files
old_files
new_files <- paste0("more/Flower_",old_files)
file.copy(from = old_files, to = new_files)
file.remove(old_files)



