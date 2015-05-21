## Plot 4

## Question: Across the United States, how have emissions from coal combustion-related sources 
##           changed from 1999-2008?

## The data is taken from the link provided on the Coursera EDA Project2 site.

## Plot file is plot4.png

## Author : Michael Hunt

## Initial admin (clear work space, set working directory, load packages required)

        rm(list=ls())
        library(dplyr)
        library(ggplot2)

# (amend pathway as appropriate)

        #setwd("C:/Users/Mike/Rspace/JHU_EDA/CP2") # home
        setwd("H:/Rspace/JHU_Data_Science/JHU_EDA/CP2") # work


## Create rds file of NEI data and code files in ./data directory - or skip if already there.

# Create sub-directory of the working dir called "data" if one does not exist already

        if(!file.exists("data")){
                dir.create("data")
        }

# if data subset not already downloaded and in rds file,

# if rds of NEI data already present in ./data, read it into data.frame NEI
# and read code file into data frame SCC

        if(file.exists("./data/summarySCC_PM25.rds")){
                print("data already in ./data directory")
        
                NEI <- readRDS("./data/summarySCC_PM25.rds")
                SCC <- readRDS("./data/Source_Classification_Code.rds")
        } 

# If the files aren't there, we have to download then unzip them.

        if(!file.exists("./data/summarySCC_PM25.rds")){
        
                print("downloading NEI data from site" )
                temp<-tempfile()
                fileURL<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
                download.file(fileURL,temp, mode="wb")
                unzip(temp, "summarySCC_PM25.rds")
                file.rename(from="summarySCC_PM25.rds",to="./data/summarySCC_PM25.rds")
                
                ##read data from rds file into data frame NEI
                NEI <- readRDS("./data/summarySCC_PM25.rds")
                
                ## unzip code file too
                unzip(temp,"Source_Classification_Code.rds")
                file.rename(from="Source_Classification_Code.rds",to="./data/Source_Classification_Code.rds")
                
                ##read data from rds file into data frame NEI
                SCC <- readRDS("./data/Source_Classification_Code.rds")       
        }

##

# data are now in date.frame NEI
# source codes are in data.frame SCC

# Look at the data
# str(NEI)

## Process the data as necessary
        
# find the rows in SCC that have the word Coal in them, in any column - take these to be related
# to coal combustion
        
        f<-function(x){
                s<-grepl("Coal",SCC[,x],ignore.case=TRUE)     
        }
        t<-sapply(c(1:15),f)
        coal.related<-which(rowSums(t)>0) # vector containing coal related row numbers in SCC
        coal.id<-SCC[coal.related,1] # vectore containing all coal related ids. 
        
        
# Select coal related activities from NEI data set.

        Coal.data<-NEI[NEI$SCC %in% coal.id,]

# For ths question we require the total emissions from all coal sources for each year.
        
        annual.Coaltotals<-aggregate(Coal.data$Emissions,by=list(Coal.data$year),FUN="sum")
        names(annual.Coaltotals)[names(annual.Coaltotals)=="Group.1"] <- "Year"
        names(annual.Coaltotals)[names(annual.Coaltotals)=="x"] <- "Emissions"

# Convert annual totals to kt
        
        annual.Coaltotals$Emissions<-annual.Coaltotals$Emissions/1e3
        
## Create plot4 in png file

#open png device;create "plot4.png" in working directory
        
        png("plot4.png")
        
#create plot and send to the file

## Setup ggplot with data frame

        g <- ggplot(annual.Coaltotals, aes(Year, Emissions))+
                geom_point(size=4)+
                geom_smooth(method="lm", se=FALSE)+ # add linear regression line
                scale_y_continuous(limits = c(0, 800))+
                theme(axis.text.x = element_text(size=14),
                      axis.text.y=element_text(size=14))+
                labs(x = "Year",y = "Annual Emissions (kt)")+
                theme(axis.title.x = element_text(size=14,vjust=-.5),
                      axis.title.y=element_text(size=14,vjust=1.2))+
                labs(title = "Annual US emissions from coal related combustion")+
                theme(plot.title = element_text(size=14, face="bold", vjust=2, lineheight=.6))
        g
# close the png file device
        
        dev.off() 
