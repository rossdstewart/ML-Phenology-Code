
library(overlap)
library(circular)
library(reshape)
library(aspace)
library(data.table)
library(tidyverse)
library(dplyr)
library(CircStats)


data(simulatedData)
# Do basic plot with defaults:
overlapPlot(pigObs, tigerObs)


overlapPlot(tigerObs, pigObs, linet = c(1,1), linec = c("red", "blue"),
            rug=TRUE, extend="lightgreen", main="Simulated data")
overlapPlot(pigObs, tigerObs, xcenter = "m", rug=TRUE)
# Mark sunrise/sunset; values to the left of "00:00" are negative
# so subtract 24:
abline(v=c(5.5, (18+47/60) - 24), lty=3)




date1 <- rnorm(n=50,mean = 255,sd = 18)
date1_rads <- as_radians(date1*360/365)


date2 <- rnorm(n=50,mean = 223,sd = 21)
date2_rads <- as_radians(date2*360/365)


overlapPlot(as.numeric(date2_rads), as.numeric(date1_rads),xscale=365, linec = c("green", "red"), xlab="Months",
            olapcol="lightblue",linewidth=c(2,2),main="Overlapping phenology", axes=FALSE,labels = FALSE, xaxt='n')
axis(2,las=2,cex.axis=0.8)
axis(1,at=c(1,60,152,244,335),labels=c("Jan","Mar","June","Sept","Dec"),cex.axis=0.8)



#############
#################################################################
#############
setwd("/media/data/ross/ML_paper_coding/Results_here")
setwd("C:/Users/rossd/Desktop/R/Flowering ML/Shared ML Phenology/320k results")

ML_data <- read.csv("ML_results_on_small_320.1.csv")

temp = list.files(pattern="ML_results.*.csv")
myfiles = lapply(temp, read.csv, stringsAsFactors=FALSE)

combined_files <- do.call("rbind", myfiles)



combined_files$File_Name <- basename(combined_files$File_Name)

dim(combined_files)
combined_files <- combined_files %>% distinct(File_Name, .keep_all = TRUE)
dim(combined_files)



combined_files$File_Name[which(combined_files$File_Name %like% "for_phenology")]



#the_meta_data <- read.csv("C:/Users/rossd/Desktop/R/Flowering ML/Shared ML Phenology/new_mult_data.csv")
#the_meta_data <- as.data.frame(fread("/media/data/ross/ML_paper_coding/new_mult_data.csv"))
the_meta_data <- as.data.frame(fread("/media/data/ross/ML_paper_coding/GBIF.Plantae.SAfr.ResGrad.iNat/multimedia.txt"))

the_meta_data <- read.csv("/media/data/ross/ML_paper_coding/GBIF.Plantae.SAfr.ResGrad.iNat/multimedia.txt",sep="\t")

the_meta_data2 <- as.data.frame(fread("/media/data/ross/ML_paper_coding/angio_meta_less_poales.txt"))
the_meta_data3 <- as.data.frame(fread("/media/data/ross/ML_paper_coding/GBIF.Plantae.SAfr.ResGrad.iNat/occurrence.txt"))



the_meta_data$identifier<- gsub("/original.*","",the_meta_data$identifier)
the_meta_data$ID <- basename(the_meta_data$identifier)
colnames(the_meta_data)

combined_files$GBIF_ID <- word(word(combined_files$File_Name,2,3,"\\."),2,2,"\\.")
combined_files$iNAT_ID <- word(word(combined_files$File_Name,4,5,"\\."),2,2,"\\.")

media_for_merge <- data.frame(ID=the_meta_data$ID,Date=the_meta_data$created)

the_big_data <- merge(combined_files,media_for_merge, by.x="iNAT_ID",by.y="ID",all.x= TRUE)

the_big_data$Date <- as.Date(word(the_big_data$Date,1,1,"T"))

occ_for_merge <- data.frame(gbifID=as.character(the_meta_data3$gbifID),family=the_meta_data3$family,order=the_meta_data3$order,class=the_meta_data3$class,
                            Long=the_meta_data3$decimalLong,
                            lat=the_meta_data3$decimalLat)

the_big_data2 <- merge(the_big_data,occ_for_merge, by.x="GBIF_ID",by.y="gbifID",all.x= TRUE)

dim(the_big_data2)
write.csv <- write.csv(the_big_data2, "1.9 mil records final output.txt")

length(which(is.na(the_big_data2$Species)))
##

which(combined_files$File_Name %like% "4126694446")

which(the_files_list %like% "png")

nrow(the_big_data2)

the_files_list4 <- read.csv("/media/data/ross/ML_paper_coding/list_img_pheno.csv")[,-1]
length(the_files_list4) # in 320 csv
the_files_list3 <- list.files(path = "/media/data/ross/ML_paper_coding/for_phenology")
length(the_files_list3) # in 320 file

the_files_list2 <- list.files(path = "/media/data2/ross/for_phenology")
length(the_files_list2) # in 1.5 file
the_files_list <- read.csv("/media/data2/ross/list_img_pheno.csv")[,-1]
length(the_files_list) # in 1.5 csv

"/media/data/ross/ML_paper_coding"

##
setwd("/media/data/ross/ML_paper_coding")
write.csv(the_files_list3[which(the_files_list3 %nin% the_files_list4)],"list_img_pheno2.csv")


which(the_files_list4 %like% "2573874016")
which(the_files_list4 %like% "1838414608")
#


test1 <- read.csv("1.7 mil records output file.txt")
test2 <- as.data.frame(fread("1.7 mil records output file.txt"))

######## Out dated method
combined_files[1:10,]

'%nin%' <- Negate('%in%')

length(combined_files$GBIF_ID)
length(which(the_meta_data3$gbifID %nin% as.numeric(combined_files$GBIF_ID)))

length(which(as.numeric(combined_files$GBIF_ID) %in% the_meta_data3$gbifID))

length(unique(combined_files$GBIF_ID[which(as.numeric(combined_files$GBIF_ID) %nin% the_meta_data3$gbifID)]))
length(which(as.numeric(combined_files$GBIF_ID) %nin% the_meta_data3$gbifID))
length(which(as.numeric(combined_files$GBIF_ID) %in% the_meta_data3$gbifID))

combined_files[which(as.numeric(combined_files$GBIF_ID) %in% the_meta_data2$gbifID),]


which(the_meta_data$gbifID %in% as.numeric(combined_files$GBIF_ID[14]))

colnames(combined_files)

combined_files$date <- rep(NA,nrow(combined_files))

i <- combined_files$File_Name[49]
for (i in unique(combined_files$File_Name)) {
  pos_file <- which(combined_files$File_Name==i)
  
  if ((pos_file%%100)==0) {
    print(paste0(pos_file,"/",length(combined_files$File_Name) )) 
  }
  
  the_date <- word(the_meta_data$date[which(the_meta_data$ID == combined_files$iNAT_ID[pos_file])],1,1,"T")
  
  combined_files$date[which(combined_files$File_Name==i)] <- the_date
  
}

## Creating merge function to swap it out the loop.

# Server has been run upto this point

combined_files$date[which(is.na(combined_files$date))]


data.frame(sort(table(combined_files$Species),decreasing = T))

# adds a coloumn about the day of year
combined_files$DOY <- as.numeric(strftime(as.Date(combined_files$date),format = "%j"))
#combined_files$iNAT_ID <- word(word(combined_files$File_Name,4,5,"\\."),2,2,"\\.")

# Saves the updated data
write.csv(combined_files,"ML_data_for_analysis-portion_1747902_No_dates.csv")
#################################################################################################################################
#################################################################################################################################
#################################################################################################################################

#################################################################################################################################
"ML_data_for_analysis-portion_1243621.csv"
ML_data <- read.csv("/media/data/ross/ML_paper_coding/Results_here/ML_data_for_analysis-portion_1243621.csv")[-1]

ML_data <- as.data.frame(fread("1.9 mil records final output.txt"))[,-c(1,4)]


dim(ML_data)

head(ML_data[which(is.na(ML_data$Date)),])
ML_data$DOY <- as.numeric(strftime(as.Date(ML_data$Date),format = "%j"))


melted_for_plot <- ML_data[which(ML_data$Flowering_2>=50),]



####
n_flowering_fam <- data.frame(sort(table(ML_data[which(ML_data$Flowering_2>=50),]$family),decreasing = T))
n_not_flowering_fam <- data.frame(sort(table(ML_data[which(ML_data$Not_Flowering>=50|ML_data$Fruiting>=50),]$family),decreasing = T))
ratio_data<-data.frame(merge(n_flowering_fam,n_not_flowering_fam,by="Var1",all.x=T))
colnames(ratio_data) <- c("Family","Flowering","Not_flowering")
ratio_data$Percentage_flowering <- round((ratio_data$Flowering)/(ratio_data$Flowering+ratio_data$Not_flowering),digits = 2)*100
####

### check that it is taking the values I need (flowering) only pull values with Flowering_2 above 50% 
bopp <- selected_cols[which(selected_cols$Flowering_2>=50),] %>% 
  pivot_longer(c(Flowering,Not_Flowering,Fruiting,Flowering_2), names_to = "colname") %>% 
  group_by(iNAT_ID) %>% 
  mutate(max = max(value), type = colname[which.max(value)]) %>% 
  pivot_wider(id_cols = everything(), names_from = "colname", values_from = "value") %>% 
  relocate(max, type, .after = Flowering_2)



melted_for_plot <- bopp[,-c(4:7)]



melted_one <- melted_for_plot[which(melted_for_plot$Species == "Abrus_precatorius"),]
melted_one <- melted_for_plot[which(melted_for_plot$Species == "Boophone_disticha"),]
melted_one <- melted_for_plot[which(melted_for_plot$Species == "Acacia_cyclops"),]
melted_one <- melted_for_plot[which(melted_for_plot$Species == "Aloe_arborescens"),]


the_vlaues <- melted_one$Date[which(melted_one$type=="Flowering"|melted_one$type=="Flowering_2")]
date2_rads <- as_radians(the_vlaues*360/365)



print(melted_one[which(melted_one$type=="Flowering"|melted_one$type=="Flowering_2"),],n=nrow(melted_one))
densityPlot(date2_rads,xscale=365, xlab="DoY\nMonth",
            extend="lightgrey",linewidth=2, k=getBandWidth(date2_rads, kmax = 1.5),#k=bw.SJ(date_all_rads)
            main=unique(melted_one$Species), axes=FALSE, axes=FALSE, xaxt='n')



densityPlot(as.numeric(z_scores_val(date2_rads)),xscale=365, xlab="DoY\nMonth",
            linewidth=8, k=bw.SJ(date2_rads), #k=getBandWidth(date2_rads, kmax = 1.5),
            main=unique(melted_one$Species), axes=FALSE, axes=FALSE, xaxt='n',add = TRUE,col="yellow3",n.grid = 365)

densityPlot(as.numeric(SD_con_val(date2_rads,n=1.25)),xscale=365, xlab="DoY\nMonth",
            linewidth=8, k=bw.SJ(date2_rads), #k=getBandWidth(date2_rads, kmax = 1.5),
            main=unique(melted_one$Species), axes=FALSE, axes=FALSE, xaxt='n',add = TRUE,col="green3",n.grid = 365)

densityPlot(as.numeric(z_scores_val2(date2_rads)),xscale=365, xlab="DoY\nMonth",
            linewidth=8, k=bw.SJ(date2_rads), #k=getBandWidth(date2_rads, kmax = 1.5),
            main=unique(melted_one$Species), axes=FALSE, axes=FALSE, xaxt='n',add = TRUE,col="red3",n.grid = 365)

axis(2,las=2,cex.axis=0.8)
axis(1,at=c(1,60,152,244,335),labels=c("01\nJan","60\nMar","152\nJune","244\nSept","335\nDec"),cex.axis=0.8,tck=-0.01)



# all values
all_vlaues <- melted_for_plot$Date[which(melted_for_plot$type=="Flowering" | melted_for_plot$type=="Flowering_2")]
date_all_rads <- as_radians(all_vlaues*360/365)
date_all_rads <- date_all_rads[-which(date_all_rads>2*pi)]

length(all_vlaues)

#print(melted_for_plot[which(melted_for_plot$type=="Flowering"|melted_for_plot$type=="Flowering_2"),],n=1000)

densityPlot(date_all_rads,xscale=365, xlab="DoY\nMonth",
            extend="lightgrey",linewidth=2, k=bw.nrd0(date_all_rads),
            main="Overall phenology", axes=FALSE, xaxt='n')



densityPlot(as.numeric(SD_con_val(date_all_rads)),xscale=365, xlab="DoY\nMonth",
            linewidth=8, k=bw.SJ(date_all_rads), #k=getBandWidth(date2_rads, kmax = 1.5)
            axes=FALSE, axes=FALSE, xaxt='n',add = TRUE,col="green3",n.grid = 365)


axis(2,las=2,cex.axis=0.8)
axis(1,at=c(1,60,152,244,335),labels=c("01\nJan","60\nMar","152\nJune","244\nSept","335\nDec"),cex.axis=0.8,tck=-0.01)










plot(here1,points.plot=FALSE, 
            plot.type="circle",
            rotation = "clock",
            zero = pi / 2,
            axes = FALSE,
            ticks = FALSE,
            tcl = 0.05,
            shrink=2,
            col="red",
            main = unique(melted_one$Species))
lines(here2,col="green3",shrink = 2,plot.info=res,lwd=3)


axis.circular(
  at = circular(seq(pi / 12, 2 * pi, pi / 6)),
  labels = c(month.abb[c(3:1,12:4)]),
  cex = 0.8,
  rotation = "clock",
  tcl.text = 0.20,
  zero =  pi / 2)
### 

## ## ## ## ## ## ## ##
## ## plot from here
## ## ## ## ## ## ## ##

# gets some data for the base plot
valuess_ang1 <- circular(rnorm(100,240), units = "radians",rotation = "clock", zero = pi / 2)
here<- density.circular(valuess_ang1, bw=50, n=365, 
                        kernel = "vonmises", type = "K", adjust = 10)

pdf(file = "1.9 mil plot < 10 records.pdf",   # The directory you want to save the file in
    width = 15, # The width of the plot in inches
    height = 15)

# plots the base plot but with empty results
plot(here,points.plot=FALSE, 
     plot.type="circle",
     rotation = "clock",
     zero = pi / 2,
     axes = FALSE,
     ticks = FALSE,
     tcl = 0.05,
     shrink=2,
     col=alpha("red",0),
     main = "Combined plots")
#lines(here2,col="green3",shrink = 2,plot.info=res,lwd=3)

# adds ticks at the start of each month
monthsss <- as_radians(c(0,31,59,90,120,151,181,212,243,273,304,334,365)*360/365)
ticks.circular(
  x = circular(monthsss,units = "radians"),
  tcl = 0.25,
  zero = pi / 2)

# adds month lables for the plot 
axis.circular(
  at = circular(seq(pi / 12, 2 * pi, pi / 6)),
  labels = c(month.abb[c(3:1,12:4)]),
  cex = 0.8,
  rotation = "clock",
  tcl.text = 0.20,
  zero =  pi / 2)

###
# clean Data


melted_for_plot2 <- melted_for_plot[-which(melted_for_plot$order=="Poales"),]
melted_for_plot2 <- melted_for_plot2[which(melted_for_plot2$class=="Liliopsida" | melted_for_plot2$class == "Magnoliopsida"),]


dim(melted_for_plot)
melted_for_plot <- melted_for_plot2
dim(melted_for_plot)


melted_for_plot[]





# Creates a lit of uniq specie
list_of_sp <- sort(unique(melted_for_plot$Species))

# Assigns a colour for each species and it will use it for the future plot
colss <- data.frame(list_sp = list_of_sp,cols =viridis::magma(length(list_of_sp)))

# creates an output variable where the y-axis values for each species will be stored.
line_output <- NULL



which(sp_name==list_of_sp)
sp_name <- list_of_sp[2]
# Loop that will add a line for each species.
for (sp_name in list_of_sp) {
  
  # Gathers the data for the respective species
  melted_one <- melted_for_plot[which(melted_for_plot$Species == sp_name),]
  # melted_one <- melted_for_plot
  melted_one <- melted_one %>% distinct(GBIF_ID, .keep_all = TRUE)

# look to force points with less than 2 points here
  
  # if there are more than zero flowering results it will executes the following
  if (nrow(melted_one)>10) {
    # converts the values into radians 
    melted_one$date_rads <- as_radians(melted_one$DOY*360/365)
    
    # converts them into radians and a circular vector 
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
    
    
   
    
    # # determines the density for the values within the SD
    #x <- try(here2 <- density.circular(the_vlaues$valuess_ang1[the_vlaues$within_sd==TRUE], bw=50 ,#bw=getBandWidth(valuess_ang1), the_vlaues$valuess_ang1[the_vlaues$within_sd==TRUE]
    #                                     n=365, kernel = "vonmises", type = "K", k=bw.SJ(valuess_ang1),na.rm = T))
    
    
    #print(the_vlaues,n=500)
    x <- try(here2 <- density.circular(melted_one$valuess_ang1, bw=50 ,#bw=getBandWidth(valuess_ang1), the_vlaues$valuess_ang1[the_vlaues$within_sd==TRUE]
                                       n=365, kernel = "vonmises", type = "K", k=bw.SJ(valuess_ang1),na.rm = T))
    
    if (class(x)!="try-error") {
      # selects the colour for the line 
      sp_col <- colss$cols[which(sp_name==colss$list_sp)]
      
      # plots the line on the current plot
      lines(here2,col=alpha(sp_col,0.15),shrink = 2,lwd=3)
      #lines(here2,col=alpha("green3",0.85),shrink = 2,lwd=3)

      
      # saves the Y data into the output variable
      line_output[[sp_name]] <- here2$y
        
        
    } 
  }
  # prints a message every 50 things.
  if ((which(sp_name==list_of_sp)%%100)==0) {
    print(paste0(which(sp_name==list_of_sp)," / ", length(list_of_sp))) 
  }
  if (class(x)=="try-error") {
    print(paste0(which(sp_name==list_of_sp)," / ", length(list_of_sp))) 
  }
  
}
    

  

# flattens the output viable into a data,fram
the_line_output <- do.call("rbind", line_output)


length(list_of_sp) # total number of species  
nrow(the_line_output) # counts the number of rows/ species that were used
nrow(melted_for_plot) # number of flowering 2 images
length(unique(ML_data$iNAT_ID)) # total unique images

here1 <- here
# determine the mean of all the coloums/degrees
here1$y <- colMeans(the_line_output)*5
here1$x <- here2$x
here1$bw <- 50

lines(here1,col="green3",shrink = 2,lwd=5)


here1$y <- colMeans(the_line_output)
here1$x <- here2$x
here1$bw <- 50

lines(here1,col="blue4",shrink = 2,lwd=5)


here1$y <- apply(the_line_output,2,quantile,0.95)
here1$x <- here2$x
here1$bw <- 50

lines(here1,col="red4",shrink = 2,lwd=5)

here1$y <- apply(the_line_output,2,quantile,0.05)
here1$x <- here2$x
here1$bw <- 50

lines(here1,col="pink4",shrink = 2,lwd=5)


# plots the mean line in green onto the plot


dev.off()

#lines(here3,col="blue3",shrink = 2,plot.info=res,lwd=3)
#valuess_ang3 <- z_scores_val(valuess_ang1)
#here3<- density.circular(valuess_ang3, bw=getBandWidth(valuess_ang3), n=365,
#                        kernel = "vonmises", type = "K", k=bw.SJ(valuess_ang2))

#################################################
tail(data.table(sort(table(melted_for_plot$Species[which(melted_for_plot$Date>=90 & melted_for_plot$Date <=120)]),
                     decreasing = F)),30)

which(melted_for_plot$Species%in%"Protea_repens")



############################################################

the_dates <- the_vlaues$valuess_ang1



SD_con_val <- function(the_dates, n=1) {
  
  ## checking Mean
  ## Check where the Mean is placing it back into the 2*pi region
  if (mean.circular(the_dates,na.rm = T)<=0) {
    mean_cic <- mean.circular(the_dates,na.rm = T)+2*pi
  } else if (mean.circular(the_dates,na.rm = T)>=2*pi) {
    mean_cic <- mean.circular(the_dates,na.rm = T)-2*pi
  } else if (mean.circular(the_dates,na.rm = T)>=0 & mean.circular(the_dates,na.rm = T)<=2*pi) {
    mean_cic <- mean.circular(the_dates,na.rm = T)
  }
  
  # checks to see if the values are the same and then scales them over a week
  if (length(unique(the_dates))==1) {
    the_dates <- the_dates+seq(from=-0.06,to=0.06,length.out=length(the_dates))
  }
  
  
  # check upper SD is fitting
  # if the upper SD is above 2*pi then calculate in two parts 
  # less than the mean then calculate it in two parts
  if (2*pi<=(mean_cic+(sd.circular(the_dates,na.rm = T)*n))) {
    pos_temp1 <- which(the_dates>=mean_cic & the_dates<=2*pi)
    
    upper_sd <- mean_cic+(sd.circular(the_dates,na.rm = T)*n)-(2*pi)
    pos_temp2 <- which(the_dates<=upper_sd & the_dates>=0)
    
    upper_sd_values <- the_dates[c(pos_temp1,pos_temp2)]
  } else {
    upper_sd_values <- the_dates[which(the_dates>=mean_cic  & the_dates<=(mean_cic+(sd.circular(the_dates,na.rm = T)*n)))]
  }
  
  
  # check lower SD is fitting
  # if the lower SD is above 2*pi then calculate in two parts 
  # less than the mean then calculate it in two parts
  if (0>=(mean_cic-(sd.circular(the_dates,na.rm = T)*n))) {
    pos_temp1 <- which(the_dates<=mean_cic & the_dates>=0)
    
    lower_sd <- mean_cic-(sd.circular(the_dates,na.rm = T)*n)+(2*pi)
    pos_temp2 <- which(the_dates>=lower_sd & the_dates<=(2*pi))
    
    lower_sd_values <- the_dates[c(pos_temp1,pos_temp2)]
  } else {
    lower_sd_values <- the_dates[which(the_dates<=mean_cic  & the_dates>=(mean_cic-(sd.circular(the_dates,na.rm = T)*n)))]
  }
  updated_values <- c(upper_sd_values,lower_sd_values)
  
  return(updated_values)
  
}



from_max <- function(the_dates,max_den, n=1) {
  # checks to see if the values are the same and then scales them over a week
  if (length(unique(the_dates))==1) {
    the_dates <- the_dates+seq(from=-0.06,to=0.06,length.out=length(the_dates))
  }

  # check upper SD is fitting 
  # if the upper SD is above 2*pi then calculate in two parts 
  # less than the mean then calculate it in two parts
  if (2*pi<=(max_den[1]+(sd.circular(the_dates,na.rm = T)*n))) {
    pos_temp1 <- which(the_dates>=max_den[1] & the_dates<=2*pi)
    
    upper_sd <- max_den[1]+(sd.circular(the_dates,na.rm = T)*n)-(2*pi)
    pos_temp2 <- which(the_dates<=upper_sd & the_dates>=0)
    
    upper_sd_values <- the_dates[c(pos_temp1,pos_temp2)]
  } else {
    upper_sd_values <- the_dates[which(the_dates>=max_den[1] & the_dates<=(max_den[1]+(sd.circular(the_dates,na.rm = T)*n)))]
  }
  
  
  # check lower SD is fitting
  # if the lower SD is above 2*pi then calculate in two parts 
  # less than the mean then calculate it in two parts
  if (0>=(max_den[1]-(sd.circular(the_dates,na.rm = T)*n))) {
    pos_temp1 <- which(the_dates<=max_den[1] & the_dates>=0)
    
    lower_sd <- max_den[1]-(sd.circular(the_dates,na.rm = T)*n)+(2*pi)
    pos_temp2 <- which(the_dates>=lower_sd & the_dates<=(2*pi))
    
    lower_sd_values <- the_dates[c(pos_temp1,pos_temp2)]
  } else {
    lower_sd_values <- the_dates[which(the_dates<=max_den[1] & the_dates>=(max_den[1]-(sd.circular(the_dates,na.rm = T)*n)))]
  }
  updated_values <- c(upper_sd_values,lower_sd_values)
  
  return(updated_values)
  
}


z_scores_val <- function(the_dates,n=3) {
  
  ## checking Mean
  ## Check where the Mean is placing it back into the 2*pi region
  if (mean.circular(the_dates)<=0) {
    mean_cic <- mean.circular(the_dates)+2*pi
  } else if (mean.circular(the_dates)>=2*pi) {
    mean_cic <- mean.circular(the_dates)-2*pi
  } else if (mean.circular(the_dates)>=0 & mean.circular(date2_rads)<=2*pi) {
    mean_cic <- mean.circular(the_dates)
  }
  z_scores <- (the_dates-mean_cic)/sd.circular(the_dates)
  #z_scores <- (the_dates-mean(the_dates))/sd(the_dates)
  
  
  new.the_dates<-the_dates[abs(z_scores)<n]
  
  return(new.the_dates)
}

z_scores_val2 <- function(the_dates) {
  
  
  
  ## checking Mean
  ## Check where the Mean is placing it back into the 2*pi region
  if (mean.circular(the_dates)<=0) {
    mean_cic <- mean.circular(the_dates)+2*pi
  } else if (mean.circular(the_dates)>=2*pi) {
    mean_cic <- mean.circular(the_dates)-2*pi
  } else if (mean.circular(the_dates)>=0 & mean.circular(date2_rads)<=2*pi) {
    mean_cic <- mean.circular(the_dates)
  }
  #z_scores <- (the_dates-mean_cic)/sd.circular(the_dates)
  z_scores <- (the_dates-mean(the_dates))/sd(the_dates)
  
  
  new.the_dates<-the_dates[abs(z_scores)<3]
  
  return(new.the_dates)
}
