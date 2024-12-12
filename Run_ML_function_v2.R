# Loads packages
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
library(lubridate)
require(Hmisc)
library(data.table)
library(Rcpp)
library(NPCirc)




# Load the models, These can be found on the KNB.
# https://knb.ecoinformatics.org/view/doi:10.5063/F1H130GV

print("loading Fruiting model (weak_fruit2.model)")
fruit_mod <- load_model_hdf5("/media/data/ross/ML_paper_coding/weak_fruit2.model")
print("Fruiting model loaded")
print("loading Flowering model (robust_1.model)")
flower_mod <- load_model_hdf5("/media/data/ross/ML_paper_coding/robust_1.model")
print("Flowering model loaded")



# A function that will create a list of all the files in a folder and then run 
#  through the species systematically and generate an output matrix with the probabilities classifications

model.pics.Ross <- function(location_of_files="for_phenology", file_output="ML_results", i=1,j=500){
  # i=1,j=500 allow to run in batches of species
  
  
 # This line will read in a list of the files that are within the folder, 
  #  or write a list if there isn't one to save time. 
  if (file.exists("list_img_pheno.csv")) {
    the_list_files <- read.csv("list_img_pheno.csv",stringsAsFactors=FALSE)[,2]
  } else {
    # If there isn't a .csv file then it will create one.
    # pulls only the .jpg and ignores others files types.
    the_list_files <- list.files(location_of_files)[which(list.files(location_of_files)%like%".jpg" | 
                                                          list.files(location_of_files)%like%".JPG" | 
                                                          list.files(location_of_files)%like%".jpeg")]
    write.csv(the_list_files,"list_img_pheno.csv")
  }
  
  # Note: The code is set up to work through everything by species and not by file
  #       This will allow it to be re-run in species batches if there are any problems.
  
  
  ## Separates the files name into the wanted info
  uniq_sp_names <- unique(word(the_list_files,1,1,"\\.")) # extracts name
  GBIF_ID <- word(the_list_files,2,3,"\\.") # extracts GBIF ID
  iNAT_ID <- word(the_list_files,4,5,"\\.") # extracts iNat photo ID
  
  print(paste0("number of files = ",length(the_list_files)))
  print(paste0("number of species = ",length(uniq_sp_names)))
  
  # Clears the ML_results that will be saved to the file
  ML_results <- data.frame()
  
  for (sp_name in uniq_sp_names[i:j]) {
    
    # Print the species number and how many have completed
    print(paste0(sp_name," - ",which(sp_name==uniq_sp_names[]),"/",length(uniq_sp_names)))
    
    ### ### ML Section ### ###
    
    # Saves a list of the file names for a given species. 
    sp_file_names <- the_list_files[the_list_files %like% sp_name]
    
    # Creates an empty variable to save the data to
    results_out <- data.frame()
    
    # For loop for each image in the species folder
    # k <- sp_file_names
    for (k in sp_file_names) {
      # This indicates how much it has completed of all the species images.
      print(paste0(k," - | ", which(sp_name==uniq_sp_names[]),"/",length(uniq_sp_names) ," | - ",which(k==sp_file_names),"/",length(sp_file_names)))
      
      # Reads and formats the picture into a relevant size and colour scale
      # Depending on what the built model was set to you might need to change the target size.
      tt <- try(test_image <- image_load(paste0(location_of_files,"/",k), target_size = c(300,300)))
      
      if (class(tt)!="try-error") {
        x <- image_to_array(test_image)
        x <- array_reshape(x, c(1, dim(x)))
        x <- x/255 # converts the RGB colours to a values between 0 and 1.
        
        # Runs the primary predictive model on the picture, change the model that is being used
        pred <- round((flower_mod %>% predict(x))*100,3)
        
        # Saves the primary model results. Note: Fruiting is set to zero at this stage 
        prediction_data <- data.frame("Species"=sp_name,"File_Name"=k,"Flowering"=pred[,1],"Not_Flowering"=pred[,2],"Fruiting"=0,"Flowering_2"=0)
        
        # For flowering values from the primary model > 50, run the secondary model
        if (prediction_data$Flowering>=50) {
          
          ## Run the secondary model on images determined to be flowering. This will check if it is a fruit.
          pred2 <- round((fruit_mod %>% predict(x))*100,3)
          # Saves the primary model results. Note: Fruiting is set to zero at this stage.
          prediction_data_fruit <- data.frame("Flowering"=pred2[,2],"Fruiting"=pred2[,3]) 
          
          # Updates the new stats to the prediction_data frame 
          prediction_data$Flowering_2 <- prediction_data_fruit$Flowering
          prediction_data$Fruiting <- prediction_data_fruit$Fruiting
          
        }
      }
      
      
      
      # Combines the prediction for each picture into a larger data.frame to be saved later
      results_out <- rbind(results_out, prediction_data)
      
    }
    
    
    # Saves the data into a spreadsheet that will read in a list of the files that are within the folder
    if (file.exists(paste0(file_output,".csv"))) {
      ML_results <- read.csv(paste0(file_output,".csv"))[,-1]
      ML_results <- rbind(ML_results, results_out)
      write.csv(ML_results,paste0(file_output,".csv"))
    } else {
      # if there isn't a csv then it will create one.
      ML_results <- results_out
      write.csv(ML_results,paste0(file_output,".csv"))
    }
    
  }
}
