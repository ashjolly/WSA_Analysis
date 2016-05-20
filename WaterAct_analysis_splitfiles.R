# Script for choosing random individual files for Water Act paper
# Note that this function will take PDF files from a specific folder, take a random percentage, and save them in another folder

#17June2015 Ashlee J
# Altered into function for published package 22 April2016
###########

# load required libraries
library(reshape)
library(plyr)
library(gsubfn)

#######

filechoice <- function(directory_1, directory_2, directory_3, 
                       savedirectory, per, filetype) {
  # get PDF files from each of the directories - for each of the following
  getpdf <- function(directory, savedirectory, filetype){
    setwd(directory)
    filelist_pdf <- list.files(pattern = filetype)
    
    # for getting 10% of individual submissions
    numind = length(filelist_pdf)
    
    per.10 =round((numind*(per/100)),digits = 0)
    
    #create a vector of randomly generated numbers that comprise 10% of the total number of submisisons, but spans to the number of individual submissions
    random.10 <- sample(1:numind, per.10, replace=F)
    
    #select file numbers that correspond to randomly generated vector
    sample.ID <- 0 #create sample ID variable
    
    for (i in 1:numind){
      sample.ID.temp <- strapplyc(filelist_pdf[i], "_(.*).pdf", simplify = TRUE)
      sample.ID[i] <- sample.ID.temp
    }
    
    filelist_pdf.1 <- as.data.frame(cbind(filelist_pdf, sample.ID))
    
    # take a random sample of 10% from a dataset mydata 
    # sample without replacement
    files.10 <- as.data.frame(filelist_pdf.1[sample(1:nrow(filelist_pdf.1), per.10,
                                                    replace=FALSE),])
    
    return(files.10)
  }
  
  filelist.1 <- getPDF(directory_1, savedirectory, filetype) # filelist from the 1st stage
  filelist.2 <- getPDF(directory_2, savedirectory, filetype) # filelist from the 2nd stage
  filelist.3 <- getPDF(directory_3, savedirectory, filetype) # filelist from the 3nd stage
  
  filelist <- rbind(filelist.1, filelist.2, filelist.3)
  
  # extract the files, and save within the save folder
  y = dim(filelist)[1]
  for (i in 1:y){
    filename <- toString(files.10[i,1])
    filepath.temp <- file.path(Selected_individual, paste(filename, sep = ""))
    file.copy(filename, filepath.temp)
    }
}

#### will automatically save selected folder in new file
#### end