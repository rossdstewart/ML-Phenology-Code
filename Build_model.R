# BUILDING the models


library(rinat)
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
library(plotly)
library(lubridate)
require(Hmisc)
library(data.table)
library(ggplot2)
library(circular)
library(viridis)
library(Rcpp)
library(NPCirc)

#reticulate::install_miniconda()
#install_tensorflow(extra_packages="pillow")
#install_keras()
#reticulate::py_install("pillow")

width <- 300 # was 224
height<- 300 # was 224
target_size <- c(width, height)
rgb <- 3 #color channels

# to use function below navigate to the data folder and run the code


Ross_make_model <- function(model_name="name_1",the_epochs=10,tune=1,batch_size=32) {
  print(paste0("Directories: ",paste(list.dirs(recursive=FALSE),collapse = "  ")))
  print(paste0("Epochs==",the_epochs))
  #print(paste0("Silent==",silent))
  print(paste0("Tune==",tune))
  
  time_start <- Sys.time()
  
  ## Setting the locations of data
  label_list <- dir("Training/")[-which(dir("Training/")%in%"desktop.ini")]
  output_n <- length(label_list)
  save(label_list, file="label_list_F+NF.R")
  
  path <- getwd()
  
  # Assigning the training Data
  train_data_gen <- image_data_generator(rescale = 1/255, validation_split = .2)
  train_image_files_path <- file.path(path, "Training")
  train_images <- flow_images_from_directory(train_image_files_path,
                                             train_data_gen,
                                             subset = 'training',
                                             target_size = target_size,
                                             class_mode = "categorical",
                                             shuffle=F,
                                             classes = label_list,
                                             seed = 2023)
  print("Training data:")
  print(table(train_images$classes))
  
  
  # Assigns the validation data - 20% of the training data
  validation_images <- flow_images_from_directory(train_image_files_path,
                                                  train_data_gen, 
                                                  subset = 'validation', 
                                                  target_size = target_size,
                                                  class_mode = "categorical",
                                                  classes = label_list,
                                                  seed = 2023)
  print("Valaidation data:")
  print(table(validation_images$classes))
  
  
  # Build the frame of the model
  print("Building the base model")
  mod_base <- application_xception(weights = 'imagenet', 
                                   include_top = FALSE, input_shape = c(width, height, 3))
  freeze_weights(mod_base) 
  
  model_function <- function(learning_rate = 0.001, 
                             dropoutrate=0.2, n_dense=1024){
    
    k_clear_session()
    
    model <- keras_model_sequential() %>%
      mod_base %>% 
      layer_global_average_pooling_2d() %>% 
      layer_dense(units = n_dense) %>%
      layer_activation("relu") %>%
      layer_dropout(dropoutrate) %>%
      layer_dense(units=output_n, activation="softmax")
    
    model %>% compile(
      loss = "categorical_crossentropy",
      optimizer = optimizer_adam(learning_rate = learning_rate),
      metrics = "accuracy"
    )
    
    return(model)
    
  }
  
  
  model <- model_function()
  
  print("Built the base model")
  
  # Starts setting the basic parameters of the model, this will be tuned later
  print("Setting basic parameters")
  batch_size <- batch_size
  epochs <- the_epochs ## if you suspect that validation accuracy would have increased beyond the 6 steps, set "epochs" to a higher value
  hist <- model %>% fit(
    train_images,
    steps_per_epoch = train_images$n %/% batch_size, 
    epochs = epochs, 
    validation_data = validation_images,
    validation_steps = validation_images$n %/% batch_size,
    verbose = 2 # originally 2
  )
  
  
  
  ## Assigns the Testing data
  #print("Testing the model")
  #test_image_files_path <- file.path(path, "Testing")
  #test_data_gen <- image_data_generator(rescale = 1/255)
  #test_images <- flow_images_from_directory(test_image_files_path,
  #                                          test_data_gen,
  #                                          target_size = target_size,
  #                                          class_mode = "categorical",
  #                                          classes = label_list,
  #                                          shuffle = F,
  #                                          seed = 2023)
  #print(table(test_images$classes))
  #
  ## evaluates the model
  #model %>% evaluate(test_images, 
  #                             steps = test_images$n)
  
  if (tune<=1) {
    print(paste0("Tuning the model, tune set to ",tune))
    
    
    ## Time to Tune the model
    # creates a grid of different variables that will be tests
    tune_grid <- data.frame("learning_rate" = c(0.001,0.0001),
                            "dropoutrate" = c(0.3,0.2),
                            "n_dense" = c(1024,256))
    if (tune==2) {
      tune_grid <- data.frame("learning_rate" = c(0.001,0.00055,0.0001),
                              "dropoutrate" = c(0.3,0.25,0.2),
                              "n_dense" = c(1024,512,256))
    }
    if (tune>=3) {
      tune_grid <- data.frame("learning_rate" = c(0.01,0.001,0.0001,0.00001),
                              "dropoutrate" = c(0.4,0.3,0.2,0.1),
                              "n_dense" = c(1024,512,256,128))
    }
    
    # Runs through the various variables
    tuning_results <- NULL
    set.seed(2023)
    for (i in 1:length(tune_grid$learning_rate)){
      
      for (j in 1:length(tune_grid$dropoutrate)){
        
        for (k in 1:length(tune_grid$n_dense)){
          cat("learnig rate: ",i,"\n")
          cat("dropout rate: ",j,"\n")
          cat("tune grid: ",k,"\n")
          model <- model_function(
            learning_rate = tune_grid$learning_rate[i],
            dropoutrate = tune_grid$dropoutrate[j],
            n_dense = tune_grid$n_dense[k])
          
          hist <- model %>% fit(
            train_images,
            steps_per_epoch = train_images$n %/% batch_size, 
            epochs = epochs, 
            validation_data = validation_images,
            validation_steps = validation_images$n %/% 
              batch_size,
            verbose = 2
          )
          
          #Save model configurations
          tuning_results <- rbind(
            tuning_results,
            c("learning_rate" = tune_grid$learning_rate[i],
              "dropoutrate" = tune_grid$dropoutrate[j],
              "n_dense" = tune_grid$n_dense[k],
              "val_accuracy" = hist$metrics$val_accuracy))
          
        }
      }
    }
    tuning_results
    print("Finished tuning the model")
    
    print("setting the best results")
    # uses the best results
    best_results <- tuning_results[which( 
      tuning_results[,ncol(tuning_results)] == 
        max(tuning_results[,ncol(tuning_results)])),]
    print(best_results)
    
    if (nrow(best_results)>1) {
      best_results <-  best_results[1,]
    }
    
    model <- model_function(learning_rate = best_results["learning_rate"],
                            dropoutrate = best_results["dropoutrate"],
                            n_dense = best_results["n_dense"])
    
    print("relearning the model using the best results")
    ## relearns the model but with best results in place
    hist <- model %>% fit(
      train_images,
      steps_per_epoch = train_images$n %/% batch_size, 
      epochs = epochs+5, 
      validation_data = validation_images,
      validation_steps = validation_images$n %/% batch_size,
      verbose = 2
    )
   
  }
  
 
  
  ### Assigns the Testing data
  ##test_image_files_path <- file.path(path, "Testing")
  ##test_data_gen <- image_data_generator(rescale = 1/255)
  ##test_images <- flow_images_from_directory(test_image_files_path,
  ##                                          test_data_gen,
  ##                                          target_size = target_size,
  ##                                          class_mode = "categorical",
  ##                                          classes = label_list,
  ##                                          shuffle = F,
  ##                                          seed = 2023)
  ##print(table(test_images$classes))
  ##
  ### evaluates the model
  ##model %>% evaluate(test_images, 
  ##                   steps = test_images$n)
  ##
  
  print("Saving the model")
  # Saves the model 
  model %>% save_model_tf(paste0(model_name,".model"))
  
  
  
  
  ## For time things
  time_end <- Sys.time()-time_start
  cat("Duration: ",time_end)
}
