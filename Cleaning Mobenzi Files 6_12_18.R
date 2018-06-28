library(readr)
library(readxl)
library(plyr)
library(ggplot2)
library(eeptools)

######READ ME##########
# This script will combine the raw csv files downloades from mobenzi from Baseline (T2 and V2), Sixmo, 12 etc...
# Each file comes with a Main survey and Drugs & Alcohol, then at 6 months, HIV is also included

#The raw mobenzi files are saved separately from the "Raw" data to be analyzed for clarity
#bc these files are uncombined and should be analyzed as is
setwd("C:/Users/cgoldbeck/Box Sync/soccer/Data/4-Scripts")


#First Baseline - T2
MainT2 <- read_csv("T2 Baseline.csv")
DAT2 <- read_csv("T2 DrugsAlcohol.csv")
#ID errors in the raw files, should notify stephan
DAT2$`Neighbourhood code`[which(DAT2$`Neighbourhood code` == 1)] <- "1B"
DAT2$`Neighbourhood code`[which(DAT2$`Neighbourhood code` == "13B")] <- "3B"
MainT2$`Participants date of birth`[which(MainT2$`Participant ID ICF`=='KLMPJE')] <- "24-6-1988" #From Stephans Updated DOBs
#Construct Age variables that matches what is shown in V2
MainT2$Age <- floor(age_calc(as.Date(MainT2$`Participants date of birth`, format = "%d-%m-%Y"), as.Date(MainT2$Start, format = "%d-%m-%Y"), units = "years"))


#Second Baseline - V2
MainV2 <- read_csv("V2 Baseline.csv")
MainV2$Age <- MainV2$AgeYears
DAV2 <- read_csv("V2 DrugsAlcohol.csv")


#6 mo Follow Up Data
DA6mo <- read_csv("SixMo DrugsAlcohol.csv")
Main6mo <- read_csv("SixMo Baseline.csv")
HIV6mo <- read_csv("SixMo HIV.csv")
#ID errors in the raw files, should notify stephan
HIV6mo$`Neighbourhood code`[which(HIV6mo$`Neighbourhood code`=='3a')] <- '3A'

#12 mo Follow Up Data
DA12mo <- read_csv("12Mo DrugsAlcohol.csv")
Main12mo <- read_csv("12Mo Main.csv")
HIV12mo <- read_csv("12Mo HIV.csv")

#Excel file for T2 and V2 Selection
#This files was made by Stephan to clarify which observations from T2 and V2 should be kept
T2V2 <- read_excel("Six month follow up recon.xlsx")

#Create baseline file combinng T2 and V2, w/o drugs and alc
#Tutorial: Grab ID's from T2V2 (Stefan's excel file sorting T2 and V2, false start and live data) 
#then match variable names bc they are not exactly the samne
Baselinemain <- rbind.fill(MainT2[which(MainT2$`Participant ID ICF` %in% T2V2[T2V2$`T2 / Version 2`== 'T2', ]$PID), ],
                      MainV2[which(MainV2$`Participant ID ICF` %in% T2V2[T2V2$`T2 / Version 2`== 'Version 2' | T2V2$`T2 / Version 2`=='V2' 
                                                                         | T2V2$`T2 / Version 2`=='v2', ]$PID |
                                     MainV2$`Neighbourhood code` %in% c('16B', '15A', '16A', '17A', '19A', '19B', '20B', '21B', '12A')), ]) #This last line is to includes triands 6,7,8 as they do not show up in Stephans excel sheet

BaselineDrug <- rbind.fill(DAT2[which(DAT2$`Participant ID ICF` %in% T2V2[T2V2$`T2 / Version 2`== 'T2', ]$PID), ], 
                     DAV2[which(DAV2$`Participant ID ICF` %in% T2V2[T2V2$`T2 / Version 2`== 'Version 2' | T2V2$`T2 / Version 2`=='V2' 
                                                                    | T2V2$`T2 / Version 2`=='v2', ]$PID |
                                  DAV2$`Neighbourhood code` %in% c('16B', '15A', '16A', '17A', '19A', '19B', '20B', '21B', '12A')), ])

#Extract variables of interest from the drug and alc baseline file
keep <- which(names(BaselineDrug) %in% c("Participant ID ICF",  "Results for test THC", "Results for test MET", "Results for test ETG"))
Baseline <- merge(Baselinemain, BaselineDrug[, keep], by = 'Participant ID ICF', all = TRUE)
Baseline$Time <- rep(1)
Baseline <- Baseline[-c(which(is.na(Baseline$`Neighbourhood code`))), ] #There are lots of blank (NA) enteries, a blank line of data

#Merging Six Month data
keep <- which(names(DA6mo) %in% c("Participant ID ICF",  "Results for test THC", "Results for test MET", "Results for test ETG", 
                                  "Results for test Mandraxx"))
SixMo <- merge(Main6mo, DA6mo[, keep], by = 'Participant ID ICF', all = TRUE)

keep <- which(names(HIV6mo) %in% c("Participant ID ICF",  "Results for HIV"))
SixMo <- merge(SixMo, HIV6mo[, keep], by = 'Participant ID ICF', all = TRUE)
SixMo <- SixMo[-c(which(is.na(SixMo$`Neighbourhood code`))), ] #There are lots of blank (NA) enteries, a blank line of data
#SixMo <- SixMo[c(which(SixMo$`Neighbourhood code` %in% 
#                         c('1B', '4A', '3A', '10B', '15B', '2A', '5B', '12B', '8B', '7A', '2B', '5A', '14B', '3B', '7B', '15A', '16B', '16A', '17A', '19A', '19B'))), ] 
SixMo <- merge(SixMo, Baseline[which(Baseline$'Participant ID ICF' %in% SixMo$'Participant ID ICF'), c('Participant ID ICF', 'Age')], by = 'Participant ID ICF', all = TRUE)
#Age is only asked at baseline, I am subsetting Baseline data for those who also have 6 mo data then applying age to SixMo data set

SixMo$Time <- rep(2)



#Cleaning some variable names that were not consistent across time points
names(SixMo)[which((names(SixMo) %in% c("Used Dagga")))] <- "Ever Used Dagga" 
names(SixMo)[which((names(SixMo) %in% c("Used Mandrax")))] <- "Ever Used Mandrax"
names(SixMo)[which((names(SixMo) %in% c("Used Tik")))] <- "Ever Used Tik"

#Merging 12 Month data
keep <- which(names(DA12mo) %in% c("Participant ID ICF",  "Results for test THC", "Results for test MET", "Results for test ETG", 
                                  "Results for test Mandrax"))
TwelveMo <- merge(Main12mo, DA12mo[, keep], by = 'Participant ID ICF', all = TRUE)

keep <- which(names(HIV6mo) %in% c("Participant ID ICF",  "Results for HIV"))
TwelveMo <- merge(TwelveMo, HIV12mo[, keep], by = 'Participant ID ICF', all = TRUE)
TwelveMo <- TwelveMo[-c(which(is.na(TwelveMo$`Neighbourhood code`))), ]
TwelveMo <- merge(TwelveMo, Baseline[which(Baseline$'Participant ID ICF' %in% TwelveMo$'Participant ID ICF'), c('Participant ID ICF', 'Age')], by = 'Participant ID ICF', all = TRUE)

TwelveMo$Time <- rep(3)


#Cleaning some variable names
names(TwelveMo)[which((names(TwelveMo) %in% c("Used Dagga")))] <- "Ever Used Dagga" 
names(TwelveMo)[which((names(TwelveMo) %in% c("Used Mandrax")))] <- "Ever Used Mandrax"
names(TwelveMo)[which((names(TwelveMo) %in% c("Used Tik")))] <- "Ever Used Tik"

#Join for a master set with Baseline, Six mo, adn 12
BaselineAndSixMo <- rbind.fill(Baseline, SixMo)
BaselineTo12Mo <- rbind.fill(BaselineAndSixMo, TwelveMo)

#Finally, add group indicator to each file 
Training <- c('1B', '4A', '15B', '3A', '10B', '16B', '17A', '21B')
Soccer <- c('2A', '5B', '12B', '8B', '7A', '15A', '19A', '20B')
Control <- c('2B', '5A', '14B', '3B', '7B', '16A', '19B', '12A')

Baseline$Control <- ifelse(Baseline$`Neighbourhood code` %in% Control, 1, 0)
Baseline$Soccer <- ifelse(Baseline$`Neighbourhood code` %in% Soccer, 1, 0)
Baseline$Training <- ifelse(Baseline$`Neighbourhood code` %in% Training, 1, 0)

SixMo$Control <- ifelse(SixMo$`Neighbourhood code` %in% Control, 1, 0)
SixMo$Soccer <- ifelse(SixMo$`Neighbourhood code` %in% Soccer, 1, 0)
SixMo$Training <- ifelse(SixMo$`Neighbourhood code` %in% Training, 1, 0)

TwelveMo$Control <- ifelse(TwelveMo$`Neighbourhood code` %in% Control, 1, 0)
TwelveMo$Soccer <- ifelse(TwelveMo$`Neighbourhood code` %in% Soccer, 1, 0)
TwelveMo$Training <- ifelse(TwelveMo$`Neighbourhood code` %in% Training, 1, 0)

BaselineAndSixMo$Control <- ifelse(BaselineAndSixMo$`Neighbourhood code` %in% Control, 1, 0)
BaselineAndSixMo$Soccer <- ifelse(BaselineAndSixMo$`Neighbourhood code` %in% Soccer, 1, 0)
BaselineAndSixMo$Training <- ifelse(BaselineAndSixMo$`Neighbourhood code` %in% Training, 1, 0)

BaselineTo12Mo$Control <- ifelse(BaselineTo12Mo$`Neighbourhood code` %in% Control, 1, 0)
BaselineTo12Mo$Soccer <- ifelse(BaselineTo12Mo$`Neighbourhood code` %in% Soccer, 1, 0)
BaselineTo12Mo$Training <- ifelse(BaselineTo12Mo$`Neighbourhood code` %in% Training, 1, 0)

#Set new directory to save data sets to be analyzed in the future
setwd("C:/Users/cgoldbeck/Box Sync/soccer/Data/1-Raw")

save(Baseline,file="Baseline5_15_18.Rda")
write.csv(Baseline, file = "Baseline5_15_18.csv")
save(SixMo,file="SixMo5_15_18.Rda")
write.csv(SixMo, file = "SixMo5_15_18.csv")
save(TwelveMo,file="TwelveMo5_15_18.Rda")
write.csv(TwelveMo, file = "TwelveMo5_15_18.csv")
save(BaselineAndSixMo,file="BaselineAndSixMo5_15_18.Rda")
write.csv(BaselineAndSixMo, file = "BaselineAndSixMo5_15_18.csv")
save(BaselineTo12Mo,file="BaselineTo12Mo5_15_18.Rda")
write.csv(BaselineTo12Mo, file = "BaselineTo12Mo5_15_18.csv")


