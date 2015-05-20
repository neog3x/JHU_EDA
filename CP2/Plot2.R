## Title: Have total emissions from PM2.5 decreased in Baltimore City, Maryland (fips == "24510")
##        from 1999 to 2008?

## This code creates a plot in png file showing the total PM2.5 emission from all sources for each 
## of the years 1999, 2002, 2005, and 2008.

## The data is taken from 

## Plot file is plot2.png

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

BC.data<-NEI[NEI$fips=="24510",]

annual.BCtotals<-aggregate(BC.data$Emissions,by=list(BC.data$year),FUN="sum")

# rename(xmeans,"Group.1"="Activity","Group.2"="Subject")
names(annual.BCtotals)[names(annual.BCtotals)=="Group.1"] <- "Year"
names(annual.BCtotals)[names(annual.BCtotals)=="x"] <- "Emissions"


## Create plot1 in png file

#open png device;create "plot2.png" in working directory
png("plot2.png")
#create plot and send to the file
par(bg="white")
with(annual.BCtotals,plot(Year,Emissions,
             xlim=c(1998,2008),
             ylim=c(0,max(annual.BCtotals$Emissions)),
             xlab="Year",
             ylab="Total Emissions (t)",
             main="Total annual emissions from all sources in Baltimore City",
             pch=19,
             col="red",)
)

# add regression line
model <- lm(Emissions ~ Year, annual.BCtotals)
abline(model, lwd = 1)

# add grid lines
abline(h=(seq(0,3000,500)),col="darkgray", lty="dotted")
abline(v=(seq(1998,2008,2)), col="darkgray", lty="dotted")

dev.off() # close the png file device
