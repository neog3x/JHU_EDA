## Title: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?

## This code creates a plot in a png file showing the total PM2.5 emission from all sources for each 
## of the years 1999, 2002, 2005, and 2008.

## The data is taken from 

## Plot file is plot1.png

## Author : Michael Hunt


## Initial admin (clear work space, set working directory, load packages required)

rm(list=ls())
library(dplyr)

# (amend pathway as appropriate)

setwd("C:/Users/Mike/Rspace/JHU_EDA/CP2")

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
 
# For ths question we require the total emissions from all sources for each year.

annual.UStotals<-aggregate(NEI$Emissions,by=list(NEI$year),FUN="sum")

# rename(xmeans,"Group.1"="Activity","Group.2"="Subject")
names(annual.UStotals)[names(annual.UStotals)=="Group.1"] <- "Year"
names(annual.UStotals)[names(annual.UStotals)=="x"] <- "Emissions"

# Convert annual totals to Mt
annual.UStotals$Emissions<-annual.UStotals$Emissions/1e6

## Create plot1 in png file

#open png device;create "plot1.png" in working directory
png("plot1.png")
#create plot and send to the file
par(bg="white")
with(annual.UStotals,plot(Year,Emissions,
             xlim=c(1998,2008),
             ylim=c(0,max(annual.UStotals$Emissions)),
             xlab="Year",
             ylab="Total Emissions (Mt)",
             main="Total annual emissions from all sources in the United States",
             pch=19,
             col="red",)
)

# add grid lines
abline(h=(seq(0,6,2)),col="darkgray", lty="dotted")
abline(v=(seq(1998,2008,2)), col="darkgray", lty="dotted")

# add regression line
model <- lm(Emissions ~ Year, annual.UStotals)
abline(model, lwd = 1)

dev.off() # close the png file device
