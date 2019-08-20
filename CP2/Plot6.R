## Plot 6

## Question: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle
##           sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes
##           over time in motor vehicle emissions?

## Given the very different emissions levels in the two counties, we answer this by 
## normalising the emissions of each place to their 1990 level (ie 1990 emissions=1)
## then plotting the emissions relative to that on the same axes. Thus, if Los Angeles
## emissions in 2000 had risen relative to their 1990 level, they would be plotted
## with value 1.1 etc.

## The data is taken from the link provided on the Coursera EDA Project2 site.

## Plot file is plot6.png

## Author : Michael Hunt

## Initial admin (clear work space, set working directory, load packages required)

        rm(list=ls())
        library(dplyr)
        library(ggplot2)

# (amend pathway as appropriate)

        setwd("C:/Users/Mike/Rspace/JHU_EDA/CP2") # home
        #setwd("H:/Rspace/JHU_Data_Science/JHU_EDA/CP2") # work


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
        
# We will define "motor vehicle" as being whatever is denoted by type "ON-ROAD" in the NEI data.
# This seems reasonable given this explanation of the NEI data by the EPA
# http://www.epa.gov/ttnchie1/net/2011inventory.html
        
# Filter from NEI the rows where $type is "ON-ROAD" AND where $fips = "24510" ie Baltimore City
# OR $fips = "06037" ie Los Angeles County.
             
        mv.data<-NEI[NEI$type=="ON-ROAD" & (NEI$fips=="24510" | NEI$fips=="06037"),]
        
# Find annual emissions totals for motor vehicles from Baltimore.
        
        mv.totals<-aggregate(mv.data$Emissions,by=list(mv.data$year,mv.data$fips),FUN="sum")
        names(mv.totals)[names(mv.totals)=="Group.1"] <- "Year"
        names(mv.totals)[names(mv.totals)=="Group.2"] <- "fips"
        names(mv.totals)[names(mv.totals)=="x"] <- "Emissions"
        # add descriptive county names
        mv.totals$County<-c(rep("Los Angeles",4),rep("Baltimore City",4))

# Have a look at mv.totals
        
        mv.totals 
         
# The LA emissions levels are much greater than those of Baltimore City, so we normalise the emissions of
# each city to better be able to make a comparison of trends from one year to the next.

# Divide all emissions by their 1999 levels.
        
        LA1999<-mv.totals$Emissions[mv.totals$Year==1999 & mv.totals$fips=="06037"]
        BC1999<-mv.totals$Emissions[mv.totals$Year==1999 & mv.totals$fips=="24510"]
        
        mv.totals$Emissions[mv.totals$fips=="06037"]<-mv.totals$Emissions[mv.totals$fips=="06037"]/LA1999
        mv.totals$Emissions[mv.totals$fips=="24510"]<-mv.totals$Emissions[mv.totals$fips=="24510"]/BC1999
        
## Create plot6 in png file

#open png device;create "plot6.png" in working directory
        
        png("plot6.png",width = 580, height = 480)
        
#create plot and send to the file

## Setup ggplot with data frame

        g <- ggplot(mv.totals, aes(Year, Emissions,color=County,shape=County))+
                geom_point(size=4)+ 
                scale_shape_manual(values=c(2,19)) +
                geom_smooth(method="lm", se=FALSE)+ # add linear regression lines
                scale_y_continuous(limits=c(0,1.2),breaks=seq(0,1.2,0.2))+
                theme(axis.text.x = element_text(size=14),
                      axis.text.y=element_text(size=14))+
                labs(x = "Year",y = "Annual Emissions relative to 1999")+
                theme(axis.title.x = element_text(size=14,vjust=-.5),
                      axis.title.y=element_text(size=14,vjust=1.2))+
                theme(legend.text=element_text(size=12),
                      legend.title=element_text(size=12))+
                ggtitle("Normalised annual emissions in Baltimore City and 
                        \n Los Angeles County from motor vehicle usage")+
                theme(plot.title = element_text(size=16, face="bold", vjust=2, lineheight=.6))
        g
        
# close the png file device
        
        dev.off() 
