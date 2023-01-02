#This code will count the frequency of themes
# Carrie Diaz Eaton 12/21/22

# packages
library(tidyverse) 
library(tidytext) 
library(rtweet)
library(ggplot2)

#Fake test data set to set up analysis
DisruptCoded<-read.csv("themescodes/fakedisruptdata.csv")
View(DisruptCoded)
size_dc<-dim(DisruptCoded)
nrow_dc<-size_dc[1]

#set up a summary dataframe and store theme counts and frequencies
Themes<-c("Self.organization", "Building.community","Broadening.the.counterpublic", "Creating.change.in.math", "SJEDI")
Nper<-c(sum(DisruptCoded$Self.organization), sum(DisruptCoded$Building.community), sum(DisruptCoded$Broadening.the.counterpublic), sum(DisruptCoded$Creating.change.in.math), sum(DisruptCoded$SJEDI))
Freqper <- Nper/nrow_dc
ThemeSummary<-data.frame(Nper, Freqper)
row.names(ThemeSummary)<-Themes

#Examine only the non-RT
#original content tweets (OCT) are original tweets (OT), replies, and quote tweets (QT) 
whichRT<-str_detect(DisruptCoded$text, "^RT ")
OCT<-filter(DisruptCoded,!whichRT)
NperOCT<-c(sum(OCT$Self.organization), sum(OCT$Building.community), sum(OCT$Broadening.the.counterpublic), sum(OCT$Creating.change.in.math), sum(OCT$SJEDI))
FreqperOCT <- NperOCT/nrow_dc
ThemeSummary <- cbind(ThemeSummary, NperOCT, FreqperOCT)

#Tweet subsets of each code
SOtweets<-filter(DisruptCoded,Self.organization == 1)
BCtweets<-filter(DisruptCoded,Building.community == 1)
BtCtweets<-filter(DisruptCoded,Broadening.the.counterpublic == 1)
CCtweets<-filter(DisruptCoded,Creating.change.in.math == 1)
SJtweets<-filter(DisruptCoded,SJEDI == 1)

#TimeSeries of Tweets by hours
#DisruptCoded<-cbind(DisruptCoded, time <- as.POSIXct(strptime(DisruptCoded$created_at, "%a, %d %b %Y %H:%M:%S %z", tz = "GMT")))
