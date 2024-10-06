
library(overlap)
library(reshape)
library(aspace)
library(data.table)
library(tidyverse)
library(dplyr)
library(CircStats)
library(circular)



#############
setwd("/media/data/ross/ML_paper_coding/Results_here")
setwd("C:/Users/Admin/Desktop/Mine/ML paper")


#################################################################################################################################


ML_data <- as.data.frame(fread("Table_S1-1_8_mil_records_for_input_cleaned.csv"))


# converts the date to DoY as a number
ML_data$DOY <- as.numeric(strftime(as.Date(ML_data$Date),format = "%j"))

# Creates a data set only for the flowering images.
melted_for_plot <- ML_data[which(ML_data$Flowering_2>=50),]






## ## ## ## ## ## ## ##
## ## plots from here - Fig. 4
## ## ## ## ## ## ## ##

# making the base plot
# gets some data for the base plot
valuess_ang1 <- circular(rnorm(100,240), units = "radians",rotation = "clock", zero = pi / 2)
blank_line<- density.circular(valuess_ang1, bw=50, n=365, 
                        kernel = "vonmises", type = "K", adjust = 10,)

pdf(file = "1.8 mil plot 5 records fam.pdf",   # The directory you want to save the file in
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

# Assigns a colour for each species, family and order then it will use it for the future plot
# Creates a lit of unique specie
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

# Sorts by fam
list_of_sp <- list_of_sp[order(list_of_sp$family),]

# Sorts by Order
list_of_sp <- list_of_sp[order(list_of_sp$order),]

# To colour all black
#list_of_sp$sp_cols <- "#000000"

# creates an output variable where the y-axis values for each species will be stored.
line_output <- NULL
image_count <- 0


which(sp_name==list_of_sp$Species)
sp_name <- list_of_sp$Species[2]
# Loop that will add a line for each species.
for (sp_name in list_of_sp$Species) {
  
  # Gathers the data for the respective species
  melted_one <- melted_for_plot[which(melted_for_plot$Species == sp_name),]
  # looks at only one occorence even if there are multiple images 
  melted_one <- melted_one %>% distinct(GBIF_ID, .keep_all = TRUE)
  
  # Will only plot points with more than a given number of records. in the this case it is 5
    if (nrow(melted_one)>5) {
    
    # converts the values into rads
    melted_one$date_rads <- as_radians(melted_one$DOY*360/365)
    
    # converts them into rads and a circular vector 
    melted_one$valuess_ang1 <- circular(melted_one$date_rads, units = "radians",rotation = "clock", zero = pi / 2)
    
    # uses some custom functions that needed to be loaded bellow
    # Use to filter out values of a given SD 
    # checks to see if the values are the same and then scales them over a week
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
    
    
    x <- try(the_line <- density.circular(melted_one$valuess_ang1, bw=50 ,
                                       n=365, kernel = "vonmises", type = "K", 
                                       k=bw.SJ(valuess_ang1),na.rm = T))
    
    if (class(x)!="try-error") {
      
      # selects the colour for the line 
      #the_col <- list_of_sp$sp_cols[which(sp_name==list_of_sp$Species)] # Sets the colour by species
      the_col <- list_of_sp$fam_cols[which(sp_name==list_of_sp$Species)] # Sets the colour by family
      #the_col <- list_of_sp$order_cols[which(sp_name==list_of_sp$Species)] # Sets the colour by family
      
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



# flattens the output viable into a data frame
the_line_output <- do.call("rbind", line_output)


length(list_of_sp) # total number of species  
nrow(the_line_output) # counts the number of rows/species that were used in the final plot
image_count # the number of  number of flowering 2 images that were used
nrow(melted_for_plot) # Totla number of flowering 2 images


length(unique(ML_data$iNAT_ID)) # total unique images

# 
blank_line1 <- blank_line

# A mean line of all the coloums/degrees and * it by 5 to visulize
blank_line1$y <- colMeans(the_line_output)*5
blank_line1$x <- the_line$x
blank_line1$bw <- 50
lines(blank_line1,col="green3",shrink = 2,lwd=5)


# A mean line of all the coloums/degrees 
blank_line1$y <- colMeans(the_line_output)
blank_line1$x <- the_line$x
blank_line1$bw <- 50
lines(blank_line1,col="blue4",shrink = 2,lwd=5)

# A 95% quntile line of all the coloums/degrees
blank_line1$y <- apply(the_line_output,2,quantile,0.95)
blank_line1$x <- the_line$x
blank_line1$bw <- 50
lines(blank_line1,col="red4",shrink = 2,lwd=5)

# A 5% quntile line of all the coloums/degrees
blank_line1$y <- apply(the_line_output,2,quantile,0.05)
blank_line1$x <- the_line$x
blank_line1$bw <- 50

lines(blank_line1,col="pink4",shrink = 2,lwd=5)


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



the_points <- tibble(read.csv("rdm_sample.txt",stringsAsFactors = FALSE))



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

#transform them into the right format
gadm_t <- raster::getData("GADM", country = "South Africa", level = 1)
value_df <- SpatialPointsDataFrame(coords=data.frame(the_points$Long,the_points$lat),data = the_points[,c(10,16)])
crs(value_df) <- CRS('+proj=longlat +datum=WGS84')


#generate a grid that can be estimated from the fixed points
bb = bbox(value_df)
grd = spsample(gadm_t, type = "regular", n = 1000)
plot(grd)
points(value_df)


# Averages the points within a certain distance
mn_value = sapply(1:length(grd), function(pt) {
  d = spDistsN1(pts=value_df, pt=grd[pt,])
  #return(d)
  return(mean(value_df[d < 0.15,]$DOY))
})

# finds the SD the points within a certain distance
mn_value = sapply(1:length(grd), function(pt) {
  d = spDistsN1(pts=value_df, pt=grd[pt,])
  #return(d)
  return(sd(value_df[d < 0.15,]$DOY))
})


# combines the values into a data frame with the grid
dat = data.frame(coordinates(grd), mn_value)
ggplot(aes(x = x1, y = x2, fill = mn_value), data = dat) + 
  geom_tile() + 
  scale_fill_viridis_c(option = "A",direction = -1,begin = 0.2,end = 0.9)  + 
  coord_equal() 




# for Fig 1
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

# Sum
ggplot() + 
  geom_tile(aes(x = x1, y = x2, fill = as.numeric(the_sum_log)), data = dat) + 
  scale_fill_viridis_c(option = "A",direction = -1,begin = 0.2,end = 0.9)  + 
  coord_equal() + 
  labs(fill='Log Species density') +
  geom_path(data = gadm_t, aes(long, lat,group = group))



########################################################################
# For Figure 5

gadm_t <- raster::getData("GADM", country = "South Africa", level = 0)
ML_data$month <- format(as.Date(ML_data$Date,format="%Y-%m/%d"),"%m")
ML_data$degree <- circular(as_radians(ML_data$DOY*360/365), units = "radians",rotation = "clock", zero = pi / 2)




value_df <- SpatialPointsDataFrame(coords=data.frame(ML_data$Long,ML_data$lat),data = ML_data[,c(9,15,16,17)])
value_df$value <- rep(1, nrow(value_df))
head(value_df)
crs(value_df) <- CRS('+proj=longlat +datum=WGS84')

mn_value = sapply(1:length(grd), function(pt) {
  d = spDistsN1(pts=value_df, pt=grd[pt,])
  #return(d)
  the_data_frame <- data.frame(the_mode=as.numeric(rds_mode(value_df[d < 0.15,]$month)), # find the mode month
                               the_mean=mean(value_df[d < 0.15,]$DOY), # finds the normal mean
                               the_sd=sd(value_df[d < 0.15,]$DOY), # find the normal SD
                               the_mean.circular=mean.circular(value_df[d < 0.15,]$degree), # finds the circular mean
                               the_mean.circular_rds=cic_mean_rds(value_df[d < 0.15,]$degree), # finds the circular mean, adjusted from 0 to2pi
                               the_sd_circular=sd.circular(value_df[d < 0.15,]$degree)) # finds the cicular SD
  return(the_data_frame)
})




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


# Fig. 5a - circular mean adjusted so that plots from 0 to 2pi
ggplot() + 
  geom_tile(aes(x = x1, y = x2, fill = the_mean.circular_ross), data = datass) + 
  scale_fill_gradientn(colours=kovesi.cyclic_mrybm_35_75_c68_s25(10000),  breaks = as_radians(c(0,59,120,181,243,304,365)*360/365))  + 
  coord_equal() + 
  labs(fill='Circular Mean') +
  geom_path(data = gadm_t, aes(long, lat,group = group))


# Fig. 5b - Circular Sd in Rads
ggplot(aes(x = x1, y = x2, fill = the_sd_circular), data = datass) + 
  geom_tile() + 
  scale_fill_viridis_c(option = "A",direction = -1,begin = 0.2,end = 0.9)  + 
  coord_equal()+ 
  labs(fill='SD in rads')

# normal SD
ggplot(aes(x = x1, y = x2, fill = the_sd), data = datass) + 
  geom_tile() + 
  scale_fill_viridis_c(option = "A",direction = -1,begin = 0.2,end = 0.9)  + 
  coord_equal() + 
  labs(fill='SD in days')


##################################################################################
## ##
## ## GARDENS - Fig 6

# loading table S2
garden_bounds <- read.csv("Garden bounds.txt",sep = "\t")

colss <- data.frame(garden_name = garden_bounds$Garden,cols = brewer.pal(11,"Paired"))


super_melted<- NULL
line_output <- NULL


# Creates a lit of uniq specie
garden_name <- garden_bounds$Garden[6]
# Loop that will add a line for each species.
for (garden_name in garden_bounds$Garden) {
  
  
  the_garden <- garden_bounds[which(garden_bounds$Garden==garden_name),]
  
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



super_melted
ggplot(super_melted, aes(x = DOY, y = garden, fill = garden)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

head(super_melted)



the_line_output <- do.call("rbind", line_output)

colnames(the_line_output) <- c(92:365,1:91) # accommodated for the shift creasted when making it liner
line_to_plot <- melt(the_line_output)


the_cols <- viridis(11,begin = 0.2,end = 0.8,option="A")

garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[1]),]
free <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[5]) +
  geom_area(fill=the_cols[5], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()

garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[2]),] ## Hantam looks wrong
Hantam <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[7]) +
  geom_area(fill=the_cols[7], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()

garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[3]),] # looks wrong
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



garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[4]),]
max(garden_line_plot$value)
Karoo <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[9]) +
  geom_area(fill=the_cols[9], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()



garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[5]),] # also looks wrong
max(garden_line_plot$value)
Kirst <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[10]) +
  geom_area(fill=the_cols[10], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()


garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[6]),]
max(garden_line_plot$value)
KZN <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[6]) +
  geom_area(fill=the_cols[6], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()


garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[7]),]
max(garden_line_plot$value)
Low <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[2]) +
  geom_area(fill=the_cols[2], alpha=0.4)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()

garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[8]),]
max(garden_line_plot$value)
Kwelera <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[8]) +
  geom_area(fill=the_cols[8], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()

garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[9]),]
max(garden_line_plot$value)
Pret <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[3]) +
  geom_area(fill=the_cols[3], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.t0itle.y = element_blank())+ theme_void()

garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[10]),]
max(garden_line_plot$value)
Thoho <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[1]) +
  geom_area(fill=the_cols[1], alpha=0.6)+
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())+ theme_void()

garden_line_plot <- line_to_plot[which(line_to_plot$Var1==garden_bounds$Garden[11]),]
max(garden_line_plot$value)
Walter  <- ggplot(garden_line_plot, aes(x=rev(Var2), y=value)) +
  geom_line(col=the_cols[4]) +
  geom_area(fill=the_cols[4], alpha=0.6) +
  ylim(0, 2.015) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank()) + theme_void()



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

# Example 1: filtering to a given genus and plotting the data of that over 3 different catagorical time span.

Pelargonium <- melted_for_plot[which(melted_for_plot$Species %like% "Pelargonium"),]

write.csv(Pelargonium,"Pelargonium.csv")


Pelargonium$year <- as.numeric(word(Pelargonium$Date,1,1,"-"))

Pelargonium <- Pelargonium[which(Pelargonium$year>=2019 & Pelargonium$year<2023),]

# Creates a lit of uniq specie
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

plot_grid(the_2020, the_2021, the_2022, 
          labels=c("2020 (i=4324;sp=135)", 
                   "2021 (i=7302;sp=142)", "2022 (i=7720;sp=140)"), 
          ncol = 1, nrow = ,
          hjust = 0,label_size = 10)



