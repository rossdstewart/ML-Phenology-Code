####
# Code to investigate the performance of the overall two step model.



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
library(viridis)

setwd("C:/Users/rossd/Desktop/R/Flowering ML/Shared ML Phenology")


# Load the models
print("loading Fruiting model (weak_fruit2.model)")
fruit_mod <- load_model_hdf5("weak_fruit2.model")
print("Fruiting model loaded")
print("loading Flowering model (robust_1.model)")
flower_mod <- load_model_hdf5("robust_1.model")
print("Flowering model loaded")



# Set the location to a validation folder with pre-classified images
location_of_files<-"for_phenology3"

# Creates a list of the files in the validation folder, currently only set to .jpeg 
the_list_files <- list.files(location_of_files)[which(list.files(location_of_files)%like%".jpg" | 
                                                        list.files(location_of_files)%like%".JPG" | 
                                                        list.files(location_of_files)%like%".jpeg")]
print(paste("Number of files:",length(the_list_files)))



# Creates an empty variable to save the data
ML_results <- data.frame()
results_out <- data.frame()

# For loop - run through each image in the folder and execute the model on them 
# k <- sp_file_names
for (k in the_list_files) {
  
  # Reads and formats the picture into a constant size and colour scale
  tt <- try(test_image <- image_load(paste0(location_of_files,"/",k), target_size = c(300,300)))
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x/255 # converts the RGB colours to a percentage
  
  # Runs the primary predictive model on the picture, changes the model that is being used, and rounds the values
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
  
  
  # Combines the prediction for each picture into a larger data.frame 
  results_out <- rbind(results_out, prediction_data)
  
}
head(results_out)


# This provides the values for Fig 2A. The settings for the plot are below.
nrow(results_out[which(results_out$Flowering_2>=50 & results_out$File_Name %like% "Flower_"),]) # is a flower predicted as a flower
nrow(results_out[which(results_out$Not_Flowering>=50 & results_out$File_Name %like% "Not_"),]) # is a not predicted as a not
nrow(results_out[which(results_out$Fruiting>=50 & results_out$File_Name %like% "Fruit_"),]) # is a fruit predicted a fruit

nrow(results_out[which(results_out$Not_Flowering>=50 & results_out$File_Name %like% "Fruit_"),]) # is a fruit and predicted as not 
nrow(results_out[which(results_out$Flowering_2>=50 & results_out$File_Name %like% "Fruit_"),]) # is a fruit and predicted as flower 

nrow(results_out[which(results_out$Not_Flowering>=50 & results_out$File_Name %like% "Flower_"),]) # is a flower and predicted as not 
nrow(results_out[which(results_out$Fruiting>=50 & results_out$File_Name %like% "Flower_"),]) # is a flower and predicted as fruit 

nrow(results_out[which(results_out$Flowering>=50 & results_out$File_Name %like% "Not_"),]) # is a not and predicted as flower 
nrow(results_out[which(results_out$Fruiting>=50 & results_out$File_Name %like% "Not_"),]) # is a not and predicted as flower 

nrow(results_out[which(results_out$Flowering>=50 & results_out$File_Name %like% "Flower_"),]) # is a not and predicted as flower 


nrow(results_out[which(results_out$Fruiting>=50 & results_out$File_Name %like% "Not_"),]) # is a not and predicted as flower 


nrow(results_out[which(results_out$Flowering>=50),])/5000


#### confusion matrix

# Values From paper
# = 572958 - true positive - ML flower; PG flower
# = 214269 - false positive - ML flower PG not 
# = 268678 - true negative - ML not; PG not
# = 320705 - false negative - ML not PG flower


##############   ##############
# 2x2 confusion matrix - Fig 2b

TClass <- factor(c("PG Flowering", "PG Flowering", "PG Not", "PG Not"))
PClass <- factor(c("ML Flowering", "ML Not", "ML Flowering", "ML Not"))
Y      <- c(572958, 320705, 214269, 268678)
df <- data.frame(TClass, PClass, Y)


ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_viridis(option="magma",begin = 0.4, end = 1,direction = -1) +
  theme_bw() + theme(legend.position = "none")


round(Y[1]/sum(Y)*100,2) # = 572958 - true positive - ML flower; PG flower
round(Y[2]/sum(Y)*100,2) # = 320705 - false negative - ML not PG flower
round(Y[3]/sum(Y)*100,2) # = 214269 - false positive - ML flower PG not 
round(Y[4]/sum(Y)*100,2) # = 268678 - true negative - ML not; PG not

##############   ##############

# 3x3 confusion matrix
TClass <- factor(c("P Flowering", "P Flowering", "P Flowering", 
                   "P Not Flowering", "P Not Flowering", "P Not Flowering",
                   "P Fruiting", "P Fruiting", "P Fruiting"))
PClass <- factor(c("is Flowering", "is Not Flowering", "is Fruiting",
                   "is Flowering", "is Not Flowering", "is Fruiting",
                   "is Flowering", "is Not Flowering", "is Fruiting"))

# values are from the lines 83-100 
Y      <- c(847, 21, 10, 
            99, 479, 422,
            56, 4, 66)
df <- data.frame(TClass, PClass, Y)


ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_viridis(option="magma",begin = 0.4, end = 1,direction = -1) +
  theme_bw() + theme(legend.position = "none")

##############   ##############


## This assists with renaming files and moving them into a single folder.
the_list_files <- list.files()[which(list.files()%like%".jpg" | 
                                       list.files()%like%".JPG" | 
                                       list.files()%like%".jpeg")]

old_files <- the_list_files
old_files
new_files <- paste0("more/Flower_",old_files)
file.copy(from = old_files, to = new_files)
file.remove(old_files)
