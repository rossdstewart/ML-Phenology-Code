README for ML Phenology Analysis


Overview

This repository contains a series of R scripts designed for analyzing phenology 
data using machine learning techniques. The code facilitates data acquisition, 
model building, statistical analysis, and visualization of results related to 
plant phenology in South Africa.

File that can be found:

ML Phenology - Build_model_v2.R
description: This script is focused on building machine learning models to analyze phenological data. It uses a combonation of R and python. It will save the model and the optimal results.


ML Phenology - Get iNat records from GBIF_v2.R
description: This script retrieves research-grade plant records from iNaturalist via GBIF from the occurrence.txt and multimedia.txt files.


ML phenology - Run_ML_function_v2.R
description: This script loads necessary libraries and prepares the environment for running the machine learning model. It will run through a large directory of images and create an output file for the results.


ML phenology - Plots v2.R
description: These scripts contain functions for generating various plots related to phenological data.


ML phenology - Testing the two-step method.R
description: This script tests a two-step methodology in the analysis of phenological data.


ML Phenology - Additional stat functions.R
description: This script contains custom optional statistical functions to assist in the analysis, such as calculating circular means and standard devation.


**Step-by-step Guide**:
0 – The training dataset will need to be constructed, will consist of pre-selected images that are placed in pre-categorized subfolders. Code to download images can be found in “ML Phenology - Get iNat records from GBIF_v2.R”.
1 – Build your model with pre-selected images that are in pre-categorized subfolders and execute the Ross_make_model function to train and build the classification model.
2 – The model will then need to be run on the large test dataset (i.e. a dataset of unclassified images). Execute the model.pics.Ross from “ML phenology - Run_ML_funciton_v2.R”. This will run through the files to and classify the images. Note it is currently set up to execute two models one after the other. The output will be a large data frame with the classification details from each image.
3 – Running the code in “ML phenology - Plots v2.R” is optional but it does provide a number of different ways to visualize the results. 
4 – To confirm the accuracy of the models, one can run the code within the “ML phenology - Testing the two-step method.R” will use a similar method to “ML phenology - Run_ML_funciton_v2.R” but the images need to be pre-classified.
5 – We have provided code for additional statistical support, such as, determining the mean and standard deviation of the points on a circular axis. These can be used to detect outliers if needed.

