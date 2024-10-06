####################################################################################################################################################################################
############################################################
## Custom functions to assist with some of the stats
############################################################

# calculates the mean from 0 to 2pi 
cic_mean_rds <- function(the_dates) {
  
  if (length(the_dates)==0) {
    mean_cic <- NA
    return(mean_cic)
  } else if (is.na(the_dates)) {
    mean_cic <- NA
    return(mean_cic)
  } else if (!is.na(the_dates)) {
    ## checking Mean
    ## Check where the Mean is placing it back into the 2*pi region
    if (mean.circular(the_dates,na.rm = T)<=0) {
      mean_cic <- mean.circular(the_dates,na.rm = T)+2*pi
    } else if (mean.circular(the_dates,na.rm = T)>=2*pi) {
      mean_cic <- mean.circular(the_dates,na.rm = T)-2*pi
    } else if (mean.circular(the_dates,na.rm = T)>=0 & mean.circular(the_dates,na.rm = T)<=2*pi) {
      mean_cic <- mean.circular(the_dates,na.rm = T)
    }
    
    return(mean_cic)
  }
  
}




# Omits values outside a of the set SD from the mean
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


# Omits values outside a given SD from the max and not from the mean 
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

# Omits values outside a z score
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

# custome mode

rds_mode <- function(the_number) {
  
  if (length(the_number)>0) {
    the_mode <- names(which.max(table(factor(the_number))))
    return(the_mode)
  } else {
    the_mode <- NA
    return(the_mode)
  }
  
}
