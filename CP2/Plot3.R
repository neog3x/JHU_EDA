## Plot 3

## Question: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
##           which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
##           Which have seen increases in emissions from 1999-2008?

## The data is taken from the link provided on the Coursera EDA Project2 site.

## Plot file is plot3.png

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
 
# For ths question we require data for Baltimore City only.

        BC.data<-NEI[NEI$fips=="24510",]

# for this Baltimore data, we need annual totals by grouped by type

        annual.BCtotals<-aggregate(BC.data$Emissions,by=list(BC.data$year,BC.data$type),FUN="sum")
        names(annual.BCtotals)[names(annual.BCtotals)=="Group.1"] <- "Year"
        names(annual.BCtotals)[names(annual.BCtotals)=="Group.2"] <- "Type"
        names(annual.BCtotals)[names(annual.BCtotals)=="x"] <- "Emissions"


## Create plot3 in png file

#open png device;create "plot3.png" in working directory
        
        png("plot3.png")
        
#create plot and send to the file

## Setup ggplot with data frame

        g <- ggplot(annual.BCtotals, aes(Year, Emissions,color=Type))+
                geom_point(size=4)+
                geom_smooth(aes(colour=Type),method="lm", se=FALSE)+
                labs(x = "Year",y = "Annual Emissions (t)")+
                labs(title = "Annual emission by type in Baltimore City")
        g
# close the png file device
        
        dev.off() 
