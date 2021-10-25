#Function for Problem No. 1
pollutantmean <- function(directory, pollutant, id){
    polfiles_list <- list.files(directory, full.names = TRUE)
    #polfiles_list is a vector that holds all file names in the specified directory

    poldata <- data.frame()
    #poldata is an empty data frame which will be used to hold the data from a specified ID
    
    #The for loop will copy the data from and to the specified ID
    for (i in id){
      poldata <- rbind(poldata, read.csv(polfiles_list[i]))
    }
    #The mean will be calculated from the data in the poldata with the specified pollutant name 
    mean(poldata[, pollutant], na.rm = TRUE)
}

#Function for Problem No. 2
complete <- function(directory, id){
  polfiles_list2 <- list.files(directory, full.names = TRUE)
  #polfiles_list2 is a vector that holds all file names in the specified directory
  
  poldata2 <- data.frame()
  #poldata2 is temporary data frame that will hold the data
  nobsfile <- data.frame(id = numeric(0), nobs = numeric(0))
  #nobsfile is a the data frame where the id number and nobs are stored
  prevnobs <- 0
  #prevnobs is a temporary numeric vector that will hold the previous nobs
  
  #The for loop will enable the code to iterate from and to the specified ID
  for (i in id){
    poldata2 <- rbind(poldata2, read.csv(polfiles_list2[i]))
    #data is copied into the poldata2
    withN <- poldata2[!is.na(poldata2$nitrate), ]
    #data with no NA in its Nitrate column will be held in withN
    withS <- withN[!is.na(withN$sulfate), ]
    #from withN, data with no NA in its Sulfate column will be held in withS
    numObs <- nrow(withS)
    #withS rows will be counted
    nobsfile <- rbind(nobsfile, data.frame(id = i, nobs = numObs - prevnobs))
    #code will place the monitor id, and the nobs into the nobsfile
    #wherein the previous nobs is subtracted from the current nobs 
    prevnobs <- numObs
  }
  nobsfile
}

#Function for Problem No. 3
corr <- function(directory, threshold){
  polfiles_list3 <- list.files(directory, full.names = TRUE)
  #polfiles_list3 is a vector that holds all file names in the specified directory
  cor_results <- numeric (0)
  
  poldata3 <- data.frame()
  #poldata3 is temporary data frame that will hold the data
  completeCases <- complete(directory, 1:322)
  #call the complete function to verify if the id/threshold has no NA values
  completeCases <- completeCases[completeCases$nobs>=threshold, ]
  #place the completeCases that is equal to or greater than the threshold value
  
  if (nrow(completeCases)>0){ #check if completeCases has saved data 
    for(i in completeCases$id){ #if there is:
      poldata3 <- rbind(poldata3, read.csv(polfiles_list3[i]))
      #data will be placed in poldata3
      withN <- poldata3[!is.na(poldata3$nitrate), ]
      withS <- withN[!is.na(withN$sulfate), ]
      #data will be cleaned here
      
      suldata <- withS["sulfate"]
      #data from withS from Sulfate column will be saved in suldata
      nitdata <- withS["nitrate"]
      #data from withS from Nitrate column will be saved in nitdata
      cor_results <- c(cor_results, cor(suldata, nitdata))
      #correlation will be calculated
    }
  }
  cor_results
}

#Code for Problem No. 4
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
#outcome is a vector that will be used to manipulate/use the data 
outcome[, 11] <- as.numeric(outcome[, 11])
#Coerced the columns to be numeric
hist(outcome[, 11], 
     main="Hospital 30-day Death (Mortality) Rates from Heart Attack", #Modified to change the plot title
     xlab="Deaths", #Modified to change the x-axis title
     border="black", #Modified to change the border color of the columns.
     col="light blue") # Modified to change the color inside the columns.