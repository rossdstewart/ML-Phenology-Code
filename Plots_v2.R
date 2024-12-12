#############################
# Produces manuscript plots #
#############################



library(overlap)
library(reshape)
library(aspace)
library(data.table)
library(tidyverse)
library(dplyr)
library(CircStats)
library(circular)



#############
setwd("C:/Users/Admin/Desktop/Mine/ML paper")


#################################################################################################################################

# Reads in the output file found here: 
# https://knb.ecoinformatics.org/view/doi:10.5063/F1H130GV
ML_data <- as.data.frame(fread("Table_S1-1_8_mil_records_for_input_cleaned.csv"))


# Converts the date to Day of Year (DoY) and formats it as a number.
ML_data$DOY <- as.numeric(strftime(as.Date(ML_data$Date),format = "%j"))

# Creates a subset for only records that have flowering_2 (secondary model) images classified above 50%.
melted_for_plot <- ML_data[which(ML_data$Flowering_2>=50),]

# Adjust if you only want to look at the primary model 
# melted_for_plot <- ML_data[which(ML_data$Flowering>=50),]




## ## ## ## ## ## ## ##
## ## plots from here - Fig. 4
## ## ## ## ## ## ## ##

# This is an elaborate plot that plots all the species as a separate line.
# It starts with creating a empty base plot and then adds lines for each species

# Making the empty base plot
# Generates "empty" data for the base plot
valuess_ang1 <- circular(rnorm(100,240), units = "radians",rotation = "clock", zero = pi / 2)
blank_line<- density.circular(valuess_ang1, bw=50, n=365, 
                        kernel = "vonmises", type = "K", adjust = 10,)

## The directory you want to save the file in
pdf(file = "1.8 mil plot 5 records species.pdf",   
    width = 15, # The width of the plot in inches
    height = 15)

# plots the base plot but with empty results
plot(blank_line,points.plot=FALSE, 
     plot.type="circle",
     rotation = "clock",
     zero = pi / 2,
     axes = FALSE,
     ticks = FALSE,
     tcl = 0.05,
     shrink=2,
     col=alpha("red",0),
     main = "Combined plots")


# adds ticks at the start of each month
monthsss <- as_radians(c(0,31,59,90,120,151,181,212,243,273,304,334,365)*360/365)
ticks.circular(
  x = circular(monthsss,units = "radians"),
  tcl = 0.25,
  zero = pi / 2)

# adds month labels for the plot 
axis.circular(
  at = circular(seq(pi / 12, 2 * pi, pi / 6)),
  labels = c(month.abb[c(3:1,12:4)]),
  cex = 0.8,
  rotation = "clock",
  tcl.text = 0.20,
  zero =  pi / 2)

###

# Based on your preferences you can plot the colours by: species, family, order


# Assigns a colour for each species, family, and order for plot.
# Creates a list of unique species
list_of_sp <- distinct(melted_for_plot,Species,.keep_all = T)

colss <- data.frame(Species = sort(list_of_sp$Species),sp_cols =viridis::magma(length(list_of_sp$Species)))
list_of_sp <- merge(list_of_sp, colss, by="Species",all.x = TRUE,sort = F)
head(list_of_sp)

colss2 <- data.frame(family = sort(unique(list_of_sp$family)),fam_cols =viridis::magma(length(unique(list_of_sp$family))))
list_of_sp <- merge(list_of_sp, colss2, by="family",all.x = TRUE,sort = F)
head(list_of_sp)

colss3 <- data.frame(order = sort(unique(list_of_sp$order)),order_cols =viridis::magma(length(unique(list_of_sp$order))))
list_of_sp <- merge(list_of_sp, colss3, by="order",all.x = TRUE,sort = TRUE)
head(list_of_sp)



# Sorts by species
list_of_sp <- list_of_sp[order(list_of_sp$Species),]

# Sorts by family
# list_of_sp <- list_of_sp[order(list_of_sp$family),]

# Sorts by Order
# list_of_sp <- list_of_sp[order(list_of_sp$order),]

# To colour all black
# list_of_sp$sp_cols <- "#000000"

# Creates an output variable where the y-axis values for each species will be stored.
# Saving this data allows you to calculate the mean, and 95% quantile for the plots as well as,
# move the plots off of a circular axis but still maintain circular properties. 
line_output <- NULL
image_count <- 0


which(sp_name==list_of_sp$Species)
sp_name <- list_of_sp$Species[2]
# This loop will add a line for each species.
for (sp_name in list_of_sp$Species) {
  
  # Gathers the data for the respective species
  melted_one <- melted_for_plot[which(melted_for_plot$Species == sp_name),]
  # Filters to only one occurrence for each observation, eliminating multiple images for the same observation. 
  melted_one <- melted_one %>% distinct(GBIF_ID, .keep_all = TRUE)
  
  # The default is set to only plot species with more than 5 images.
  # You can adjust this to replicate Fig S1-Fig S3
  # Will only plot points with more than a given number of records (in this case, 5) 
    if (nrow(melted_one)>5) {
    
    # Converts the values into radians
    melted_one$date_rads <- as_radians(melted_one$DOY*360/365)
    
    # Converts them into radians and a circular vector 
    melted_one$valuess_ang1 <- circular(melted_one$date_rads, units = "radians",rotation = "clock", zero = pi / 2)
    
    
    # If desired, you can implement outlier detection methods here by uncommenting next code chunk.
    #  # You will need to load the code in "ML Phenology - Additional stat functions.R"
    # # Use to filter out values of a given circular SD 
    # # Checks to see if the values are the same and then scales them over a week
    
    #if (length(unique(melted_one$valuess_ang1))==1) {
    #  
    #  # If all the points are the same adjust them by about 3.5 days on each side. Used when plotting with minimal points.
    #  melted_one$valuess_ang1 <- melted_one$valuess_ang1+seq(from=-0.01,to=0.01,length.out=length(melted_one$valuess_ang1))
    #  
    #  valuess_ang2 <- melted_one$valuess_ang1
    #  
    #} else if (length(unique(melted_one$valuess_ang1))!=1)  {
    #  # applies a custom function to remove values outside 1.25 SD
    #  z <- try(valuess_ang2 <- SD_con_val(melted_one$valuess_ang1,1.25))
    #}
    #
    #melted_one$within_sd<-melted_one$valuess_ang1%in%valuess_ang2
    
    # Creates a circular density of the converted DoY values,
    # # Currently the bandwidth is set to 50
    x <- try(the_line <- density.circular(melted_one$valuess_ang1, bw=50 ,
                                       n=365, kernel = "vonmises", type = "K", 
                                       k=bw.SJ(valuess_ang1),na.rm = T))
    
    if (class(x)!="try-error") {
      
      # Selects the colour for the line. Species, Family, Order.
      the_col <- list_of_sp$sp_cols[which(sp_name==list_of_sp$Species)] # Sets the colour by species
      #the_col <- list_of_sp$fam_cols[which(sp_name==list_of_sp$Species)] # Sets the colour by family
      #the_col <- list_of_sp$order_cols[which(sp_name==list_of_sp$Species)] # Sets the colour by order
      
      # plots the line on the current plot
      lines(the_line,col=alpha(the_col,0.15),shrink = 2,lwd=3)

      
      # saves the Y data into the output variable
      line_output[[sp_name]] <- the_line$y
      image_count <-  image_count + nrow(melted_one)
      
    } 
  }
  # prints a message every 50 species/lines
  if ((which(sp_name==list_of_sp$Species)%%200)==0) {
    print(paste0(which(sp_name==list_of_sp$Species)," / ", length(list_of_sp$Species))) 
  }
  if (class(x)=="try-error") {
    print(paste0(which(sp_name==list_of_sp$Species)," / ", length(list_of_sp$Species))) 
  }
  
}


# These should give summary numbers
length(list_of_sp) # total number of species  
nrow(the_line_output) # counts the number of rows/species that were used in the final plot
image_count # the number of  number of flowering 2 images that were used
nrow(melted_for_plot) # Total number of flowering 2 images

length(unique(ML_data$iNAT_ID)) # Total unique images


# Next the mean and 95 quantile lines need to be added,

# Flattens the output viable into a data frame, for calculations.
the_line_output <- do.call("rbind", line_output)

 
blank_line1 <- blank_line

# A mean line of all the column/degrees and multiply it by 5 for better visualization
blank_line1$y <- colMeans(the_line_output)*5
blank_line1$x <- the_line$x
blank_line1$bw <- 50
lines(blank_line1,col="green3",shrink = 2,lwd=5)


# A mean line of all the column/degrees with no manipulation.
blank_line1$y <- colMeans(the_line_output)
blank_line1$x <- the_line$x
blank_line1$bw <- 50
lines(blank_line1,col="blue4",shrink = 2,lwd=5)

# A 95% quantile line of all the column/degrees
blank_line1$y <- apply(the_line_output,2,quantile,0.95)
blank_line1$x <- the_line$x
blank_line1$bw <- 50
lines(blank_line1,col="red4",shrink = 2,lwd=5)

# A 5% quantile line of all the column/degrees
blank_line1$y <- apply(the_line_output,2,quantile,0.05)
blank_line1$x <- the_line$x
blank_line1$bw <- 50
lines(blank_line1,col="pink4",shrink = 2,lwd=5)

# closes the pdf plot.
dev.off()







################################################################################
#### Shows the families and the ratios that were successfully identified #######
# table 1 and table S3

n_flowering_fam <- data.frame(sort(table(ML_data[which(ML_data$Flowering_2>=50),]$family),decreasing = T))
n_not_flowering_fam <- data.frame(sort(table(ML_data[which(ML_data$Not_Flowering>=50|ML_data$Fruiting>=50),]$family),decreasing = T))
ratio_data<-data.frame(merge(n_flowering_fam,n_not_flowering_fam,by="Var1",all.x=T))
colnames(ratio_data) <- c("Family","Flowering","Not_flowering")
ratio_data$Percentage_flowering <- round((ratio_data$Flowering)/(ratio_data$Flowering+ratio_data$Not_flowering),digits = 4)*100


the_put_out <- NULL
for (a_fam in ratio_data$Family) {
  the_no_sp <-length(unique(ML_data$Species[which(ML_data$family==a_fam)]))
  the_put_out[[a_fam]] <- the_no_sp
  
}
length(the_put_out)
nrow(ratio_data)

the_put_out2 <- NULL
for (a_fam in ratio_data$Family) {
  the_no_sp <-length(unique(melted_for_plot$Species[which(melted_for_plot$family==a_fam)]))
  the_put_out2[[a_fam]] <- the_no_sp
  
}

ratio_data$Total_no_species <- the_put_out
ratio_data$no_flowering_species <- the_put_out2
ratio_data$Percentage_species_flowering <- round(the_put_out2/the_put_out*100,digits = 2)


head(ratio_data)
write.csv(ratio_data,"ratio_data.txt")



#################################### for the map figures ####################################
# Fig. 1, Fig. 5

library(sp)
library(ggplot2)
library(scales)
library(ggplot2)
library(sp)
library(automap)
library(rgdal)
library(scales)
library(raster)
library(pals)

# loads the data in
the_points <- ML_data

# Transform them into the right mapping format
gadm_t <- raster::getData("GADM", country = "South Africa", level = 1) # downlaods the country shape file
value_df <- SpatialPointsDataFrame(coords=data.frame(the_points$Long,the_points$lat),data = the_points[,c(10,16)])
crs(value_df) <- CRS('+proj=longlat +datum=WGS84')

# Divides the country up into 1000 different grids that will be given estimated from the fixed points
bb = bbox(value_df)
grd = spsample(gadm_t, type = "regular", n = 1000)
plot(grd)
points(value_df)




# For Fig 1
mn_value = sapply(1:length(grd), function(pt) {
  d = spDistsN1(pts=value_df, pt=grd[pt,])
  #return(d)
  the_data_frame <- data.frame(the_max=as.numeric(max(value_df[d < 0.15,]$month)),
                               the_sum=as.numeric(sum(value_df[d < 0.15,]$value)))
  return(the_data_frame)
})


mn_value_t <- t(mn_value)
dat = data.frame(coordinates(grd), mn_value_t)
dat$the_sum_log <- log10(as.numeric(dat$the_sum))

colnames(mn_value_t)<-c("the_max",'the_sum')

# plots the sum of values
ggplot() + 
  geom_tile(aes(x = x1, y = x2, fill = as.numeric(the_sum_log)), data = dat) + 
  scale_fill_viridis_c(option = "A",direction = -1,begin = 0.2,end = 0.9)  + 
  coord_equal() + 
  labs(fill='Log Species density') +
  geom_path(data = gadm_t, aes(long, lat,group = group))



########################################################################
# For Figure 5

# reloads the data in case it is not currently loaded
gadm_t <- raster::getData("GADM", country = "South Africa", level = 0)
ML_data$month <- format(as.Date(ML_data$Date,format="%Y-%m/%d"),"%m")
ML_data$degree <- circular(as_radians(ML_data$DOY*360/365), units = "radians",rotation = "clock", zero = pi / 2)



# gathers the points of values of interest.
value_df <- SpatialPointsDataFrame(coords=data.frame(ML_data$Long,ML_data$lat),data = ML_data[,c(9,15,16,17)])
value_df$value <- rep(1, nrow(value_df))
head(value_df)
crs(value_df) <- CRS('+proj=longlat +datum=WGS84')

# the below function calculates a number of different values for each cell.
# mode, normal mean, normal SD, circular mean, circular mean adjusted to 0 and 2pi, circular SD
mn_value = sapply(1:length(grd), function(pt) {
  d = spDistsN1(pts=value_df, pt=grd[pt,])
  #return(d)
  the_data_frame <- data.frame(the_mode=as.numeric(rds_mode(value_df[d < 0.15,]$month)), # find the mode month
                               the_mean=mean(value_df[d < 0.15,]$DOY), # finds the normal mean
                               the_sd=sd(value_df[d < 0.15,]$DOY), # find the normal SD
                               the_mean.circular=mean.circular(value_df[d < 0.15,]$degree), # finds the circular mean
                               the_mean.circular_rds=cic_mean_rds(value_df[d < 0.15,]$degree), # finds the circular mean, adjusted from 0 to 2pi
                               the_sd_circular=sd.circular(value_df[d < 0.15,]$degree)) # finds the circular SD
  return(the_data_frame)
})



# Creates a data frame and saves it to load it back in for the plot
mn_value_t <- t(mn_value)
colnames(mn_value_t)<-c("the_mode",'the_mean',"the_sd","the_mean.circular","the_mean.circular_ross","the_sd_circular")
dat = data.frame(coordinates(grd), mn_value_t)
fwrite(dat, file ="map plot values3.txt")
datass <- read.csv("map plot values3.txt")


# mode plot
ggplot(aes(x = x1, y = x2, fill = factor(the_mode)), data = datass) + 
  geom_tile() + 
  scale_fill_viridis_d(option = "A",direction = -1,begin = 0.2,end = 0.9,na.value = "grey50")  + 
  coord_equal() +
  labs(fill='Months') 


# Mean DoY
ggplot(aes(x = x1, y = x2, fill = the_mean), data = datass) + 
  geom_tile() + 
  scale_fill_viridis_c(option = "A",direction = 1,begin = 0.2,end = 0.9)  + 
  coord_equal() + 
  labs(fill='Mean DoY')

# testing to see if it looks better with the start and end being plotted over winter
datass$the_mean2 <- datass$the_mean
datass$the_mean2[which(datass$the_mean>151)] <- datass$the_mean[which(datass$the_mean>151)] -151
datass$the_mean2[which(datass$the_mean<151)] <- datass$the_mean[which(datass$the_mean<151)] +151

# mean adjusted to plot over winter
ggplot(aes(x = x1, y = x2, fill = the_mean2), data = datass) + 
  geom_tile() + 
  scale_fill_viridis_c(option = "A",direction = 1,begin = 0.2,end = 0.9)  + 
  coord_equal() + 
  labs(fill='Mean DoY 0=151')

# Mean Day of year circular
ggplot(aes(x = x1, y = x2, fill = the_mean.circular), data = datass) + 
  geom_tile() + 
  scale_fill_gradientn(colours=kovesi.cyclic_mrybm_35_75_c68_s25(10000), guide = "colourbar")  + 
  coord_equal()+ 
  labs(fill='Circular Mean') +  
  geom_path(data = gadm_t, aes(long, lat,group = group))

##########
# Fig. 5a - circular mean adjusted so that plots from 0 to 2pi
ggplot() + 
  geom_tile(aes(x = x1, y = x2, fill = the_mean.circular_ross), data = datass) + 
  scale_fill_gradientn(colours=kovesi.cyclic_mrybm_35_75_c68_s25(10000),  breaks = as_radians(c(0,59,120,181,243,304,365)*360/365))  + 
  coord_equal() + 
  labs(fill='Circular Mean') +
  geom_path(data = gadm_t, aes(long, lat,group = group))


# Fig. 5b - Circular SD in Rads
ggplot(aes(x = x1, y = x2, fill = the_sd_circular), data = datass) + 
  geom_tile() + 
  scale_fill_viridis_c(option = "A",direction = -1,begin = 0.2,end = 0.9)  + 
  coord_equal()+ 
  labs(fill='SD in rads')
##########


# normal SD
ggplot(aes(x = x1, y = x2, fill = the_sd), data = datass) + 
  geom_tile() + 
  scale_fill_viridis_c(option = "A",direction = -1,begin = 0.2,end = 0.9)  + 
  coord_equal() + 
  labs(fill='SD in days')


##################################################################################
## ##
## ## Local phenology of the Gardens - Fig 6

# loading table S2
garden_bounds <- read.csv("Garden bounds.txt",sep = "\t")

# colours for the respective garden
colss <- data.frame(garden_name = garden_bounds$Garden,cols = brewer.pal(11,"Paired"))


super_melted<- NULL
line_output <- NULL


# Creates a list of unique species
garden_name <- garden_bounds$Garden[6]
# Loop that will add a line for each garden
for (garden_name in garden_bounds$Garden) {
  
  # selects the garden and the boundries
  the_garden <- garden_bounds[which(garden_bounds$Garden==garden_name),]

  # creates a subset dataset of the plants with in the given garden.
  one_garden <- melted_for_plot[which(melted_for_plot$lat<= the_garden$Upper.decimalLatitude & # Upper Lat
                                        melted_for_plot$lat>= the_garden$Lower.decimalLatitude & # Lower Lat
                                        melted_for_plot$Long<= the_garden$Upper.decimalLongitude & # Upper long
                                        melted_for_plot$Long>= the_garden$Lower.decimalLongitude),] # Lower long 
  print(paste0(garden_name,": ",nrow(one_garden)," records"))
  print(paste0(garden_name,": ",length(unique(one_garden$Species))," unique species"))
  
  
  # Gathers the data for the respective garden
  melted_one <- one_garden
  melted_one <- melted_one %>% distinct(GBIF_ID, .keep_all = TRUE) # keeps only one observer
  head(melted_one[which(melted_one$DOY>120 & melted_one$DOY<150),c(1:3,9)],40)
  
  
  # converts the values into radians 
  melted_one$date_rads <- as_radians(melted_one$DOY*360/365)
  
  # converts them into radians and a circular vector 
  melted_one$valuess_ang1 <- circular(melted_one$date_rads, units = "radians",rotation = "clock", zero = pi / 2)
  
  the_line <- density.circular(melted_one$valuess_ang1, bw=50 ,#bw=getBandWidth(valuess_ang1), the_vlaues$valuess_ang1[the_vlaues$within_sd==TRUE]
                            n=365, kernel = "vonmises", type = "K", k=bw.SJ(valuess_ang1),na.rm = T,
                            from = circular(0), to = circular(2 * pi))
  
  
  melted_one$garden <- garden_name
  super_melted <- rbind(super_melted, melted_one)
  

  
  line_output[[garden_name]] <- the_line$y
  
}


# test plot for the combined data
super_melted
ggplot(super_melted, aes(x = DOY, y = garden, fill = garden)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

head(super_melted)


# flattens it into a data frame for the garden 
the_line_output <- do.call("rbind", line_output)

colnames(the_line_output) <- c(92:365,1:91) # accommodated for the shift created when making it liner
line_to_plot <- melt(the_line_output)

# Creates 11 different colour options
the_cols <- viridis(11,begin = 0.2,end = 0.8,option="A")

# Creates the line for a single garden and then saves it to a variable (free = Free State Garden)
garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[1]),]
max(garden_line_plot$value)
free <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[5]) +
  geom_area(fill=the_cols[5], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()
# Note: the 2.015 in the "ylim(0, 2.015)" is set to the each separate line following.
## Adjust the vertical limit if need, use "max(garden_line_plot$value)" to find the ylim max 


# Creates the line for a single garden and then saves it to a variable (Hantam)
garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[2]),] 
max(garden_line_plot$value)
Hantam <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[7]) +
  geom_area(fill=the_cols[7], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()


# Creates the line for a single garden and then saves it to a variable (Harold Porter)
# Additional geom_vline() lines create vertical lines at the start of months
garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[3]),] 
max(garden_line_plot$value)
Harold  <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[11]) +
  geom_area(fill=the_cols[11], alpha=0.6)+
  geom_vline(xintercept = 0, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 31, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 59, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 90, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 120, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 151, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 181, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 212, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 243, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 273, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 304, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 334, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 365, color = alpha("#6B686B",0.75)) +
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()


# Creates the line for a single garden and then saves it to a variable (Karro Desert Garden)
garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[4]),]
max(garden_line_plot$value)
Karoo <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[9]) +
  geom_area(fill=the_cols[9], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()


# Creates the line for a single garden and then saves it to a variable (Kirstenbosch Garden)
garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[5]),]
max(garden_line_plot$value)
Kirst <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[10]) +
  geom_area(fill=the_cols[10], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()

# Creates the line for a single garden and then saves it to a variable (KZN Garden)
garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[6]),]
max(garden_line_plot$value)
KZN <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[6]) +
  geom_area(fill=the_cols[6], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()

# Creates the line for a single garden and then saves it to a variable (Lowveld Garden)
garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[7]),]
max(garden_line_plot$value)
Low <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[2]) +
  geom_area(fill=the_cols[2], alpha=0.4)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()

# Creates the line for a single garden and then saves it to a variable (Kwelera Garden)
garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[8]),]
max(garden_line_plot$value)
Kwelera <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[8]) +
  geom_area(fill=the_cols[8], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()

# Creates the line for a single garden and then saves it to a variable (Pretoria Garden)
garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[9]),]
max(garden_line_plot$value)
Pret <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[3]) +
  geom_area(fill=the_cols[3], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.t0itle.y = element_blank())+ theme_void()

# Creates the line for a single garden and then saves it to a variable (Thohoyandou Garden)
garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[10]),]
max(garden_line_plot$value)
Thoho <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[1]) +
  geom_area(fill=the_cols[1], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()

# Creates the line for a single garden and then saves it to a variable (walter sisulu Garden)
garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[11]),]
max(garden_line_plot$value)
Walter  <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[4]) +
  geom_area(fill=the_cols[4], alpha=0.6) +
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank()) + theme_void()


# combines the lines that were generated below and then adds the values that were provided from the loop 
plot_grid(Thoho, Low, Pret, Walter, free, KZN, Kwelera, Hantam, Karoo, Harold, Kirst,
          labels=c("Thohoyandou (i=38;sp=19)", "Lowveld (i=81;sp=45)", 
                   "Pretoria (i=706;sp=181)", "Walter Sisulu (i=720;sp=181)", 
                   "Free State (i=220;sp=62)", "KZN (i=56;sp=32)", "Kwelera (i=52;sp=34)", 
                   "Hantam (i=1556;sp=174)",  "Karoo Desert (i=457;sp=105)", 
                   "Harold Porter (i=2026;sp=312)", "Kirstenbosch (i=5256;sp=545)"), 
          ncol = 1, nrow = 11,
          hjust = 0,label_size = 10)


############################################################



################################################
## Additional Figure examples 

# Example 1: filtering to a given genus and plotting the data of that over 3 different categorical time span.

# creates a subset of a specific group, in this case: Pelargonium 
Pelargonium <- melted_for_plot[which(melted_for_plot$Species %like% "Pelargonium"),]

write.csv(Pelargonium,"Pelargonium.csv")

# creates a year vector 
Pelargonium$year <- as.numeric(word(Pelargonium$Date,1,1,"-"))

# further filters for the more recent years
Pelargonium <- Pelargonium[which(Pelargonium$year>=2019 & Pelargonium$year<2023),]

# Creates a list of unique species
list_of_year <- sort(unique(Pelargonium$year))

# Assigns a colour for each species and it will use it for the future plot
colss <- data.frame(list_sp = list_of_year,cols =viridis::magma(length(list_of_year)))

# creates an output variable where the y-axis values for each species will be stored.
line_output <- NULL


year_name <- list_of_year[2]
# Loop that will add a line for each species.
for (year_name in list_of_year) {
  
  # Gathers the data for the respective species
  melted_one <- Pelargonium[which(Pelargonium$year == year_name),]
  
  melted_one <- melted_one %>% distinct(GBIF_ID, .keep_all = TRUE)
  
  
  if (nrow(melted_one)>2) {
    # converts the values into rads 
    melted_one$date_rads <- as_radians(melted_one$DOY*360/365)
    
    # converts them into rads and a circular vector 
    melted_one$valuess_ang1 <- circular(melted_one$date_rads, units = "radians",rotation = "clock", zero = pi / 2)
    
    
    
    # checks to see if the values are the same and then scales them over a week
    if (length(unique(melted_one$valuess_ang1))==1) {
      # If all the points are the same adjust them by about 3.5 days on each side
      melted_one$valuess_ang1 <- melted_one$valuess_ang1+seq(from=-0.01,to=0.01,length.out=length(melted_one$valuess_ang1))
      
      valuess_ang2 <- melted_one$valuess_ang1
    } else if (length(unique(melted_one$valuess_ang1))!=1)  {
      # applies a custom function to remove values outside 1.25 SD
      z <- try(valuess_ang2 <- SD_con_val(melted_one$valuess_ang1,1.25))
    }
    
    
    melted_one$within_sd<-melted_one$valuess_ang1%in%valuess_ang2
    
    x <- try(here2 <- density.circular(melted_one$valuess_ang1, bw=50 ,#bw=getBandWidth(valuess_ang1), the_vlaues$valuess_ang1[the_vlaues$within_sd==TRUE]
                                       n=365, kernel = "vonmises", type = "K", k=bw.SJ(valuess_ang1),na.rm = T))
    
    if (class(x)!="try-error") {
      
      line_output[[year_name]] <- here2$y
      
      
    } 
  }
  
}


the_line_output <- do.call("rbind", line_output)

colnames(the_line_output) <- c(92:365,1:91)
line_to_plot <- melt(the_line_output)


the_cols <- viridis(4,begin = 0.2,end = 0.8,option="A")


# Plots the sections one at a time, as above.
# The below will plot by year.


# 2020
liine_for_plot <- line_to_plot[which(line_to_plot$Var1==2),]
max(liine_for_plot$value)
liine_for_plot$Var2[which(liine_for_plot$value==max(liine_for_plot$value))]
the_2020 <- ggplot(liine_for_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[2]) +
  geom_area(fill=the_cols[2], alpha=0.6)+
  ylim(0, 0.73) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()
length(which(Pelargonium$year==2020))
length(unique(Pelargonium$Species[which(Pelargonium$year==2020)]))


# 2021
liine_for_plot <- line_to_plot[which(line_to_plot$Var1==3),]
max(liine_for_plot$value)
liine_for_plot$Var2[which(liine_for_plot$value==max(liine_for_plot$value))]
the_2021 <- ggplot(liine_for_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[3]) +
  geom_area(fill=the_cols[3], alpha=0.6)+
  ylim(0, 0.73) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()
length(which(Pelargonium$year==2021))
length(unique(Pelargonium$Species[which(Pelargonium$year==2021)]))

# 2022
liine_for_plot <- line_to_plot[which(line_to_plot$Var1==4),]
max(liine_for_plot$value)
liine_for_plot$Var2[which(liine_for_plot$value==max(liine_for_plot$value))]
the_2022 <- ggplot(liine_for_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[4]) +
  geom_area(fill=the_cols[4], alpha=0.6)+
  ylim(0, 0.73) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()+
  geom_vline(xintercept = 0, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 31, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 59, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 90, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 120, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 151, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 181, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 212, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 243, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 273, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 304, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 334, color = alpha("#6B686B",0.75)) +
  geom_vline(xintercept = 365, color = alpha("#6B686B",0.75)) 
length(which(Pelargonium$year==2022))
length(unique(Pelargonium$Species[which(Pelargonium$year==2022)]))

# plots the final graph
plot_grid(the_2020, the_2021, the_2022, 
          labels=c("2020 (i=4324;sp=135)", 
                   "2021 (i=7302;sp=142)", "2022 (i=7720;sp=140)"), 
          ncol = 1, nrow = ,
          hjust = 0,label_size = 10)



