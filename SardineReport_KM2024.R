################################################################################
###################### Sardine Capture Mark Recapture ##########################
################################################################################
##################################### BY KELSEY MARSHALL########################
##################################### VERSION LAST UPDATED 01/16/2025 ##########
################################################################################

#I've tried to set this up so that you only need to run sections 1 and 3 every time (these load in packages and data and set the working directory folder)
#then each section after that (4-15) can be run on its own, so you can choose what stats you want, except section 15 uses daily capture history tables created in section 14
#section 2 is only needed when you're adding the new years data to the rest
#section 4 only needs to be run once each year, if that, to check on data errors
#section 17 pulls in and works with in egg mass data
#When using this script, please make edits to a copy of this file only and save this version, Box does not save old versions of R script for retrieval

#####1-READ IN PACKAGES and SET UP Working Directory folder(WD)#####

##only need to install.packages once, then each session after, you just need to bring them into the R session with the 'library' command
#install.packages("zoo")
#library(zoo)
#library(openxlsx)
#library(TMB)
#library(marked)
#library(magrittr)
#library(tibble)
#library(ggplot2)
#library(stringr)
#library(lubridate)
#library(Rcapture)
#library(dplyr)
#library(tidyr)
library("readxl")
library(tidyverse)
library(RMark)
 
###set your working directory 
#I chose to use pc location rather than box to keep the path simpler, 
#I put a copy of the data in the WD that Iload into R session, and the working directory is where R will default save any outputs
#I save the R script file in Box so it doesn't get lost
#When using this script, please make edits to a copy of this file only and save this version, Box does not save old versions of R script for retrieval

setwd("C:/Users/KelseyMarshall/Documents/YOTO") 

#####2-if needed, compile years of data, if not skip to next section #####
###read in your mark recap data and add data from latest year to compiled sheet, will have to update files for subsequent years
###"trim_ws = TRUE" while reading in tables gets rid of empty space in cells  
###and "gsub" gets rid of return/enter after Pit#s (this was an issue with 2024 data)

Sardine16_23 <- read_xlsx("Sardine_PIT_Squeeky.xlsx", sheet = "Sardine_KMM", trim_ws = TRUE)
Sardine16_23$Date <- as.Date(Sardine16_23$Date)
Sardine24 <- read_xlsx("Sardine_2024tabletdataJWelty.xlsx", sheet = "Sardine2024Cleanup", trim_ws = TRUE)
Sardine24$Date <- as.Date(Sardine24$Date)
Sardinecompile <- bind_rows(Sardine16_23, Sardine24)
Sardinecompile$Year <- format(as.Date(Sardinecompile$Date, format="%Y-%m-%d"),"%Y")
#code to get rid of the return/enter after the pits, may not need it but also doesnt hurt
Sardinecompile$Pit <- gsub("\r", "",Sardinecompile$Pit)
Sardinecompile$Pit <- gsub("\n", "",Sardinecompile$Pit)
###output compiled data in new excel file, this is the file I am going to QC (in excel)
### with code below and read in again as needed as I correct errors
#write.xlsx(Sardinecompile, "Sardine16_24.xlsx")


#####3 read in your mark recap data#####

#I had to specify each column's data type because it was throwing read errors
#at me for some of the columns, "coltypes" will need to be adjusted if columns change
#using trim_ws = TRUE while reading in both tables to get rid of empty space in cells  
#and gsub after combining to get rid of return/enter after Pit#s (issue with 2024 data)

Sardine <- read_xlsx("Sardine16_24QC.xlsx", sheet = "Compiled", trim_ws = TRUE, 
                     col_types = c("numeric","text","date","text","text","numeric","numeric","text", "text",
                     "text","text","text","text","text","text","text","text","numeric","numeric",
                     "text","text","text","text"))
Sardine$Year <- format(as.Date(Sardine$Date, format="%Y-%m-%d"),"%Y")
Sardine$Date <- as.Date(Sardine$Date)
#code to get rid of the return/enter after the pits, may not need it
Sardine$Pit <- gsub("\r", "",Sardine$Pit)
Sardine$Pit <- gsub("\n", "",Sardine$Pit)


###create data frame for only tagged captures, add detect column with 1s
Sardine_tag <- Sardine%>%
  drop_na(Pit) %>%
  add_column(detect = 1) %>%
  arrange(Date)

#####4 QA/QC - This code checks for some data errors that then will need to be checked and corrected in excel, then the corrected data read back in#####

##format data and examine to ensure groups are consistently named
Sardine$Mark_Recap <- replace_na(Sardine$Mark_Recap, replace = "Amplexus (not tagged)")
Sardine$Mark_Recap <- as.factor(Sardine$Mark_Recap)
summary(Sardine$Mark_Recap)

Sardine$Sex <- as.factor(Sardine$Sex)
summary(Sardine$Sex)

Sardine$Amplexus <- as.factor(Sardine$Amplexus)
summary(Sardine$Amplexus)

##find pits with fewer or greater than 15 digits
Sardine_pitoddlength <- Sardine_tag %>%
  filter(startsWith(Pit, "9")) %>%
  filter(nchar(Pit)!= 15)
#check thru and correct in excel where possible, where not possible make notes and maybe move incorrect pit to notes

##find pits with sex discrepancies
Sardine_checkPITSex <- Sardine_tag[,c("Pit", "Sex")]
Sardine_checkPITSex <- Sardine_checkPITSex %>%
  distinct()
Sardine_checkPITSex$Duplicate <- duplicated(Sardine_checkPITSex$Pit)
Sardine_checkPITSex <- Sardine_checkPITSex[Sardine_checkPITSex$Duplicate == "TRUE",]
#check thru and correct in excel where possible, where not possible write unknown, but indicate original sex in notes

#####5 Effort######
NDaysSurvey  <- Sardine %>%
  distinct(Site, Year, Date)%>%
  group_by(Site, Year) %>% 
  summarise(Count = n())

DatesSurvey  <- Sardine %>%
  group_by(Site, Year, Date) %>% 
  summarise(Count = n())

#####6 Toad movement btw sites#####
##originally had this way later in the code, but its actually been really useful in QAQC because it highlights toads the occur in multiple sites
##and if toads occur in two different sites quickly (i.e. same day or within year), it is likely worth checking pit data
Sardine_checkmultisite <- Sardine_tag[,c("Pit", "Sex", "Date", "Site")]
Sardine_checkmultisite <- Sardine_checkmultisite %>%
  distinct(Pit, Sex, Site)
Sardine_checkmultisite$Duplicate <- duplicated(Sardine_checkmultisite$Pit)
Sardine_checkmultisite <- Sardine_checkmultisite[Sardine_checkmultisite$Duplicate == "TRUE", "Pit"]
Sardine_multisite <- merge(Sardine_checkmultisite, Sardine)
Sardine_multisitewide <- Sardine_multisite %>%
  select(Pit, Sex, Site, Date) %>%
  arrange(Date) %>%
  pivot_wider(names_from=Date, values_from=Site)
Sardine_multisitedate <- Sardine_multisite %>%
  select(Pit, Sex, Site, Date) %>%
  arrange(Date) %>%
  pivot_wider(names_from=Site, values_from=Date)
#####7 Summary stats#####
Sardine_tag <- Sardine%>%
  drop_na(Pit) %>%
  add_column(detect = 1) %>%
  arrange(Date)

# unique pit tags deployed at all sites
UniqPITs <- Sardine_tag %>%
  distinct(Pit) %>%
  summarise(Count = n())
UniqPITs

#unique pit tags deployed at all sites by sex
UniqPIT_bysex <- Sardine_tag %>%
  distinct(Pit, Sex) %>%
  group_by(Sex) %>%
  summarise(Count = n())
UniqPIT_bysex

#unique pit tags deployed by site and sex
UniqPIT_bysitesex <- Sardine_tag %>%
  distinct(Site, Pit, Sex) %>%
  group_by(Site, Sex) %>%
  summarise(Count = n())
UniqPIT_bysitesex

# unique pit tags deployed by site
UniqPITs_bysite <- Sardine_tag %>%
  distinct(Site, Pit) %>%
  group_by(Site) %>% 
  summarise(Count = n())
UniqPITs_bysite

# unique pit tags deployed by site, sex, and year
UniqPITs_bysiteyr <- Sardine_tag %>%
  distinct(Site, Pit, Year) %>%
  group_by(Site, Year) %>% 
  summarise(Count = n()) %>%
  pivot_wider(, names_from = Year, values_from = Count, values_fill = 0) %>%
  add_column(Sex = "Total Unique")
UniqMFUYr <- Sardine_tag %>%
  distinct(Site, Year, Pit, Sex) %>%
  group_by(Site, Year, Sex) %>% 
  summarise(Count = n()) %>%
  pivot_wider(, names_from = Year, values_from = Count, values_fill = 0)
Sexby_Site_Year <- bind_rows(UniqMFUYr, UniqPITs_bysiteyr)
Sexby_Site_Year <- Sexby_Site_Year %>%
  arrange (Site, Sex)
Sexby_Site_Year
write.csv(Sexby_Site_Year, "TaggedSexby_Site_Year.csv")

# adult and juvie captures per site and year
CaptAJ <- Sardine %>%
  group_by(Site, Year, LifeStage) %>%
  summarise(Count = n()) %>%
  pivot_wider(, names_from = Year, values_from = Count, values_fill = 0)
CaptAJ
write.csv(CaptAJ, "CapturesbyYear.csv")

# Marks and Recaps per site per year
Sardine_CHY <- Sardine_tag %>%
  select(Year, Site, Pit, Sex, detect) %>%
  distinct() %>%
  pivot_wider(names_from=Year, values_from=detect, values_fill = 0)
Sardine_CHY$FirstCapture <- apply(Sardine_CHY[-c(1:3)], 1, function(x) names(x)[min(which(x > 0))])
Sardine_First_Year <- Sardine_CHY %>%
  pivot_longer("2016":"2024", names_to = "Survey_Year", values_to = "Detect_NonDetect")
Sardine_MRInd <- Sardine_First_Year %>%
  filter(! Detect_NonDetect == 0)
Sardine_MRInd$MR <- ifelse (Sardine_MRInd$Survey_Year == Sardine_MRInd$FirstCapture, "Mark", "Recap")

Sardine_MR <- Sardine_MRInd %>%
  group_by(Site, Survey_Year, MR) %>% 
  summarise(Count = n())
Sardine_MRwidetable <- Sardine_MR %>%
  pivot_wider(names_from=Survey_Year, values_from=Count, values_fill = 0)

#lower
Lower_MR <- Sardine_MR %>%
  filter(Site == "Lower Sardine")
ggplot(data=Lower_MR, aes(x=Survey_Year, y= Count, fill=MR)) +
  geom_bar(stat="identity") +
  labs(title = "Newly marked and recaptured Yosemite toads at Lower Sardine annually") +
  labs(x = "Year", y = "Number of unique, tagged adults") +
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL)) +
  theme(axis.text=element_text(size=10))

#upper
Upper_MR <- Sardine_MR %>%
  filter(Site == "Upper Sardine")
ggplot(data=Upper_MR, aes(x=Survey_Year, y= Count, fill=MR)) +
  geom_bar(stat="identity") +
  labs(title = "Newly marked and recaptured Yosemite toads at Upper Sardine annually") +
  labs(x = "Year", y = "Number of unique, tagged adults") +
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL)) +
  theme(axis.text=element_text(size=10))

#middle
Middle_MR <- Sardine_MR %>%
  filter(Site == "Middle Sardine")
ggplot(data=Middle_MR, aes(x=Survey_Year, y= Count, fill=MR)) +
  geom_bar(stat="identity") +
  labs(title = "Newly marked and recaptured Yosemite toads at Middle Sardine annually") +
  labs(x = "Year", y = "Number of unique, tagged adults") +
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL)) +
  theme(axis.text=element_text(size=10))

#####8 Year-marked tablesandgraphs#####
Sardine_CHY <- Sardine_tag %>%
  select(Year, Site, Pit, Sex, detect) %>%
  distinct() %>%
  pivot_wider(names_from=Year, values_from=detect, values_fill = 0)
Sardine_CHY$FirstCapture <- apply(Sardine_CHY[-c(1:3)], 1, function(x) names(x)[min(which(x > 0))])
FirstCapt <- Sardine_CHY %>%
  pivot_longer("2016":"2024", names_to = "Survey_Year", values_to = "Detect_NonDetect") 
FirstCapt_Count <- FirstCapt %>%
  filter(! Detect_NonDetect == 0) %>%
  #filter(! Sex == "Unknown") %>%
  group_by(Survey_Year, FirstCapture) %>%
  summarise(Count = n())
ggplot(data=FirstCapt_Count, aes(x=Survey_Year, y= Count, fill=FirstCapture)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  labs(x = "Survey Year", y = "Number of unique, tagged adults captured at all Sardine sites") +
  scale_fill_brewer(palette = "Set3", direction = -1) +
  theme_classic() +
  guides(fill = guide_legend(title = "Year Tagged")) +
  theme(axis.text.y=element_text(size=10)) +
  theme(axis.text.x=element_text(angle = 300))

##males
Firstcapt_Male <- FirstCapt %>%
  filter(! Detect_NonDetect == 0) %>%
  filter(Sex == "Male") %>%
  group_by(Survey_Year, FirstCapture, ) %>%
  summarise(Count = n())
Firstcapt_Male_Table <- Firstcapt_Male %>%
  pivot_wider(names_from=Survey_Year, values_from=Count)
ggplot(data=Firstcapt_Male, aes(x=Survey_Year, y= Count, fill=FirstCapture)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  labs(x = "Survey Year", y = "Number of unique, tagged adults at all Sardine sites") +
  scale_fill_brewer(palette = "PuBuGn", direction = -1) +
  theme_classic() +
  guides(fill = guide_legend(title = "Year Tagged")) +
  theme(axis.text.y=element_text(size=10)) +
  theme(axis.text.x=element_text(angle = 300))
write.csv(Firstcapt_Male_Table, "Firstcapt_Male_Table.csv")

##females
Firstcapt_Fem <- FirstCapt %>%
  filter(! Detect_NonDetect == 0) %>%
  filter(Sex == "Female") %>%
  group_by(Survey_Year, FirstCapture, ) %>%
  summarise(Count = n())
Firstcapt_Fem_Table <- Firstcapt_Fem %>%
  pivot_wider(names_from=Survey_Year, values_from=Count)
ggplot(data=Firstcapt_Fem, aes(x=Survey_Year, y= Count, fill=FirstCapture)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  labs(x = "Survey Year", y = "Number of unique, tagged adults at all Sardine sites") +
  scale_fill_brewer(palette = "YlOrBr", direction = -1) +
  theme_classic() +
  guides(fill = guide_legend(title = NULL)) +
  theme(axis.text.y=element_text(size=10)) +
  theme(axis.text.x=element_text(angle = 300))
write.csv(Firstcapt_Fem_Table, "Firstcapt_Fem_Table.csv")

#####9 SVLHistograms#####

FirstYrDetect <- Sardine_First_Year %>%
  filter(! Detect_NonDetect == 0) %>%
  rename(Year = Survey_Year)

SVLYrAvg <- Sardine %>%
  drop_na(Pit) %>%
  drop_na(SVL_mm) %>%
  group_by(Site, Year, Pit) %>%
  summarise(count= n(), SVL_mm = mean(SVL_mm))

SVL_FirstYr <- merge(SVLYrAvg, FirstYrDetect)
SVL_FirstYr$MR <- ifelse (SVL_FirstYr$Year == SVL_FirstYr$FirstCapture, "Mark", "Recap")

Male_SVL <- SVL_FirstYr %>%
  filter(Sex == "Male")

min(Male_SVL$SVL_mm) 
max(Male_SVL$SVL_mm)
mean(Male_SVL$SVL_mm)
sd(Male_SVL$SVL_mm)

ggplot(data= Male_SVL, aes(x=SVL_mm, fill= MR)) +
  geom_histogram(position = position_stack(reverse = TRUE)) +
  facet_wrap(~Year, scales = "free") +
  scale_fill_brewer(palette = "BuPu", direction = -1) +
  xlim(40,75)+
  labs(x = "SVL in mm") +
  theme_classic() +
  theme(axis.text=element_text(size=10), legend.title=element_blank()) 

Male_SVL_MRxYr <- Male_SVL %>%
  group_by(Year, MR) %>%
  summarise(CountM = n(), MeanM = mean(SVL_mm), StdDevM = sd(SVL_mm)) %>%
  arrange(MR, Year)
Male_SVL_MRAllYr <- Male_SVL %>%
  group_by(MR) %>%
  summarise(Count = n(), Mean = mean(SVL_mm), StdDev = sd(SVL_mm))
write.csv(Male_SVL_MRxYr, "MaleSVL.csv")

Male_SVL_MarkAllYr <- Male_SVL %>%
  filter(MR == "Mark") %>%
  summarise(Count = n(), Mean = mean(SVL_mm), StdDev = sd(SVL_mm))

###

Female_SVL <- SVL_FirstYr %>%
  filter(Sex == "Female")

min(Female_SVL$SVL_mm) 
max(Female_SVL$SVL_mm)
mean(Female_SVL$SVL_mm)
sd(Female_SVL$SVL_mm)

Female_SVL_MR <- Female_SVL %>%
  group_by(MR) %>%
  summarise(CountM = n(), MeanM = mean(SVL_mm), StdDevM = sd(SVL_mm))
Female_SVL_MRxYr <- Female_SVL %>%
  group_by(Year, MR) %>%
  summarise(CountM = n(), MeanM = mean(SVL_mm), StdDevM = sd(SVL_mm)) %>%
  arrange(MR, Year)




#####10 MassHistograms#####
Sardine$Year <- as.factor(Sardine$Year)
Male_Mass <- Sardine %>%
  drop_na(Mass_g) %>%
  filter(Sex == "Male") %>%
  filter(! Mark_Recap == "Amplexus (not tagged)")
M_Mass_AvgInd <- Male_Mass %>%
  group_by(Pit, Year) %>%
  summarise(count= n(), Mass_g = mean(Mass_g))
mean(M_Mass_AvgInd$Mass_g) 
sd(M_Mass_AvgInd$Mass_g)
min(M_Mass_AvgInd$Mass_g) 
max(M_Mass_AvgInd$Mass_g) 
Male_Mass_stats <- M_Mass_AvgInd %>%
  group_by(Year) %>%
  summarise(Count = n(), Mean = mean(Mass_g), StdDev = sd(Mass_g))

##
##at mark
##

Female_Mass <- Sardine %>%
  drop_na(Mass_g) %>%
  filter(Sex == "Female") %>%
  filter(! Mark_Recap == "Amplexus (not tagged)")
F_Mass_AvgInd <- Female_Mass %>%
  group_by(Pit, Year) %>%
  summarise(count = n(), Mass_g = mean(Mass_g))
FMeanMass <- mean(F_Mass_AvgInd$Mass_g) 
FStdDev_Mass <- sd(F_Mass_AvgInd$Mass_g)
FMinMass <- min(F_Mass_AvgInd$Mass_g) 
FMaxMass <- max(F_Mass_AvgInd$Mass_g) 
Female_Mass_stats <- F_Mass_AvgInd %>%
  group_by(Year) %>%
  summarise(Count = n(), Mean = mean(Mass_g), StdDev = sd(Mass_g))

#####11 Daily stats by site #####
Sardine_tag$Survey_Date <- Sardine_tag$Date
Sardine_D <- Sardine_tag[,c("Site", "Survey_Date", "Pit", "Sex", "detect")]
Sardine_CHD <- Sardine_D %>%
  distinct() %>%
  pivot_wider(names_from=Survey_Date, values_from=detect, values_fill = 0)
Sardine_CHD$FirstCapture <- apply(Sardine_CHD[-c(1:3)], 1, function(x) names(x)[min(which(x > 0))])
Sardine_tagfirsts <- Sardine_CHD[,c("Site", "Pit", "Sex", "FirstCapture")]
Sardine_MRA <- merge(Sardine, Sardine_tagfirsts, all.x = TRUE)
Sardine_MRA$MRA <- ifelse((is.na(Sardine_MRA$FirstCapture)& Sardine_MRA$Amplexus == "Y"), "Amplexus(not tagged)", ifelse (Sardine_MRA$Date == Sardine_MRA$FirstCapture, "Mark", "Recap"))
Sardine_MRA <- Sardine_MRA[! is.na(Sardine_MRA$MRA),]

LowerSardineMRA <- Sardine_MRA %>%
  filter(Site == "Lower Sardine")
UpperSardineMRA <- Sardine_MRA %>%
  filter(Site == "Upper Sardine")
MiddleSardineMRA <- Sardine_MRA %>%
  filter(Site == "Middle Sardine")


###LOWER

LowerDailyMRA <- LowerSardineMRA %>% 
  group_by(Year, Survey_Date, MRA, Sex) %>% 
  summarise(Count = n())
for (i in 1:nrow(LowerDailyMRA)) {
  if (!(LowerDailyMRA$Survey_Date[i] %in% LowerDailyMRA[LowerDailyMRA$Sex == "Female",]$Survey_Date)) {
    row <- data.frame("Year"=as.character(year(LowerDailyMRA$Survey_Date[i])), "Survey_Date"=LowerDailyMRA$Survey_Date[i], "Sex"="Female", "Count"=0)
    LowerDailyMRA <- rbind(LowerDailyMRA, row)
  }
  if (LowerDailyMRA$Survey_Date[i] %in% LowerDailyMRA[LowerDailyMRA$Sex == "Male",]$Survey_Date == FALSE){
    row <- data.frame("Year"= as.character(year(LowerDailyMRA$Survey_Date[i])), "Survey_Date"= LowerDailyMRA$Survey_Date[i], "Sex"= "Male", "Count"=0)
    LowerDailyMRA <- rbind(LowerDailyMRA, row)
  }
}
Lower_MaleDailyMRA <- LowerDailyMRA %>%
  filter(Sex == "Male")
Lower_FemaleDailyMRA <- LowerDailyMRA %>%
  filter(Sex == "Female")
ggplot(data=Lower_FemaleDailyMRA, aes(x=Survey_Date, y= Count, fill= MRA)) +
  geom_bar(stat="identity") +
  scale_x_date(breaks = Lower_FemaleDailyMRA$Survey_Date, date_labels = "%b %d") +
  facet_wrap(~Year, nrow = 1, scales = "free_x") +
  labs(x = "Survey Day", y = "Number of unique females") +
  scale_fill_brewer(palette = "YlOrBr") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "bottom") +
  ggtitle("Lower Sardine Meadow Daily Capture Totals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=6), axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))

ggplot(data=Lower_MaleDailyMRA, aes(x=Survey_Date, y= Count, fill= MRA)) +
  geom_bar(stat="identity") +
  facet_wrap(~Year, nrow = 1, scales = "free_x") +
  scale_x_date(breaks = Lower_MaleDailyMRA$Survey_Date, date_labels = "%b %d") +
  labs(x = "Survey Day", y = "Number of unique males") +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "bottom") +
  ggtitle("Lower Sardine Meadow Daily Capture Totals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=6), axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))


LowerDaily <- LowerSardineMRA %>% 
  group_by(Year, Date, Sex) %>% 
  summarise(Count = n())
for (i in 1:nrow(LowerDaily)) {
  if (!(LowerDaily$Date[i] %in% LowerDaily[LowerDaily$Sex == "Female",]$Date)) {
    row <- data.frame("Year"=as.character(year(LowerDaily$Date[i])), "Date"=LowerDaily$Date[i], "Sex"="Female", "Count"=0)
    LowerDaily <- rbind(LowerDaily, row)
  }
  if (LowerDaily$Date[i] %in% LowerDaily[LowerDaily$Sex == "Male",]$Date == FALSE){
    row <- data.frame("Year"= as.character(year(LowerDaily$Date[i])), "Date"= LowerDaily$Date[i], "Sex"= "Male", "Count"=0)
    LowerDaily <- rbind(LowerDaily, row)
  }
}

SexByDayPY_Lower <- LowerDaily %>%
  group_by(Sex, Year) %>%
  summarise_at(vars(Count), list(mean = mean, sd = sd, min = min, max = max)) 
SexByDay_Lower <- LowerDaily %>%
  group_by(Sex) %>%
  summarise_at(vars(Count), list(mean = mean, sd = sd, min = min, max = max))
Lo_DaysinMdw <- LowerSardine %>% 
  drop_na(Pit) %>%
  group_by(Sex, Pit, Year) %>% 
  summarise(Count = n())
Lo_DaysinMdwavg <- Lo_DaysinMdw %>% 
  group_by(Sex) %>% 
  summarise_at(vars(Count), list(mean = mean, sd = sd, min = min, max = max))


###UPPER

UpperDailyMRA <- UpperSardineMRA %>% 
  group_by(Year, Survey_Date, MRA, Sex) %>% 
  summarise(Count = n())
for (i in 1:nrow(UpperDailyMRA)) {
  if (!(UpperDailyMRA$Survey_Date[i] %in% UpperDailyMRA[UpperDailyMRA$Sex == "Female",]$Survey_Date)) {
    row <- data.frame("Year"=as.character(year(UpperDailyMRA$Survey_Date[i])), "Survey_Date"=UpperDailyMRA$Survey_Date[i], "Sex"="Female", "Count"=0)
    UpperDailyMRA <- rbind(UpperDailyMRA, row)
  }
  if (UpperDailyMRA$Survey_Date[i] %in% UpperDailyMRA[UpperDailyMRA$Sex == "Male",]$Survey_Date == FALSE){
    row <- data.frame("Year"= as.character(year(UpperDailyMRA$Survey_Date[i])), "Survey_Date"= UpperDailyMRA$Survey_Date[i], "Sex"= "Male", "Count"=0)
    UpperDailyMRA <- rbind(UpperDailyMRA, row)
  }
}
Upper_MaleDailyMRA <- UpperDailyMRA %>%
  filter(Sex == "Male")
Upper_FemaleDailyMRA <- UpperDailyMRA %>%
  filter(Sex == "Female")
ggplot(data=Upper_FemaleDailyMRA, aes(x=Survey_Date, y= Count, fill= MRA)) +
  geom_bar(stat="identity") +
  scale_x_date(breaks = Upper_FemaleDailyMRA$Survey_Date, date_labels = "%b %d") +
  facet_wrap(~Year, nrow = 1, scales = "free_x") +
  labs(x = "Survey Day", y = "Number of unique females") +
  scale_fill_brewer(palette = "YlOrBr") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "bottom") +
  ggtitle("Upper Sardine Meadow Daily Capture Totals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=5.5), axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))

ggplot(data=Upper_MaleDailyMRA, aes(x=Survey_Date, y= Count, fill= MRA)) +
  geom_bar(stat="identity") +
  facet_wrap(~Year, nrow = 1, scales = "free_x") +
  scale_x_date(breaks = Upper_MaleDailyMRA$Survey_Date, date_labels = "%b %d") +
  labs(x = "Survey Day", y = "Number of unique males") +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "bottom") +
  ggtitle("Upper Sardine Meadow Daily Capture Totals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=5.5), axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))


UpperDaily <- UpperSardineMRA %>% 
  group_by(Year, Date, Sex) %>% 
  summarise(Count = n())
for (i in 1:nrow(UpperDaily)) {
  if (!(UpperDaily$Date[i] %in% UpperDaily[UpperDaily$Sex == "Female",]$Date)) {
    row <- data.frame("Year"=as.character(year(UpperDaily$Date[i])), "Date"=UpperDaily$Date[i], "Sex"="Female", "Count"=0)
    UpperDaily <- rbind(UpperDaily, row)
  }
  if (UpperDaily$Date[i] %in% UpperDaily[UpperDaily$Sex == "Male",]$Date == FALSE){
    row <- data.frame("Year"= as.character(year(UpperDaily$Date[i])), "Date"= UpperDaily$Date[i], "Sex"= "Male", "Count"=0)
    UpperDaily <- rbind(UpperDaily, row)
  }
}

SexByDayPY_Upper <- UpperDaily %>%
  group_by(Sex, Year) %>%
  summarise_at(vars(Count), list(mean = mean, sd = sd, min = min, max = max))
SexByDay_Upper <- UpperDaily %>%
  group_by(Sex) %>%
  summarise_at(vars(Count), list(mean = mean, sd = sd, min = min, max = max))
Up_DaysinMdw <- UpperSardine %>% 
  drop_na(Pit) %>%
  group_by(Sex, Pit, Year) %>% 
  summarise(Count = n())
Up_DaysinMdwavg <- Up_DaysinMdw %>% 
  group_by(Sex) %>% 
  summarise_at(vars(Count), list(mean = mean, sd = sd, min = min, max = max))


###MIDDLE

MiddleDailyMRA <- MiddleSardineMRA %>% 
  group_by(Year, Survey_Date, MRA, Sex) %>% 
  summarise(Count = n())
for (i in 1:nrow(MiddleDailyMRA)) {
  if (!(MiddleDailyMRA$Survey_Date[i] %in% MiddleDailyMRA[MiddleDailyMRA$Sex == "Female",]$Survey_Date)) {
    row <- data.frame("Year"=as.character(year(MiddleDailyMRA$Survey_Date[i])), "Survey_Date"=MiddleDailyMRA$Survey_Date[i], "Sex"="Female", "Count"=0)
    MiddleDailyMRA <- rbind(MiddleDailyMRA, row)
  }
  if (MiddleDailyMRA$Survey_Date[i] %in% MiddleDailyMRA[MiddleDailyMRA$Sex == "Male",]$Survey_Date == FALSE){
    row <- data.frame("Year"= as.character(year(MiddleDailyMRA$Survey_Date[i])), "Survey_Date"= MiddleDailyMRA$Survey_Date[i], "Sex"= "Male", "Count"=0)
    MiddleDailyMRA <- rbind(MiddleDailyMRA, row)
  }
}
Middle_MaleDailyMRA <- MiddleDailyMRA %>%
  filter(Sex == "Male")
Middle_FemaleDailyMRA <- MiddleDailyMRA %>%
  filter(Sex == "Female")
ggplot(data=Middle_FemaleDailyMRA, aes(x=Survey_Date, y= Count, fill= MRA)) +
  geom_bar(stat="identity") +
  scale_x_date(breaks = Middle_FemaleDailyMRA$Survey_Date, date_labels = "%b %d") +
  facet_wrap(~Year, nrow = 1, scales = "free_x") +
  labs(x = "Survey Day", y = "Number of unique females") +
  scale_fill_brewer(palette = "YlOrBr") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "bottom") +
  ggtitle("Middle Sardine Meadow Daily Capture Totals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=6), axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))

ggplot(data=Middle_MaleDailyMRA, aes(x=Survey_Date, y= Count, fill= MRA)) +
  geom_bar(stat="identity") +
  facet_wrap(~Year, nrow = 1, scales = "free_x") +
  scale_x_date(breaks = Middle_MaleDailyMRA$Survey_Date, date_labels = "%b %d") +
  labs(x = "Survey Day", y = "Number of unique males") +
  scale_fill_brewer(palette = "YlGnBu") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "bottom") +
  ggtitle("Middle Sardine Meadow Daily Capture Totals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=6), axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))


MiddleDaily <- MiddleSardineMRA %>% 
  group_by(Year, Date, Sex) %>% 
  summarise(Count = n())
for (i in 1:nrow(MiddleDaily)) {
  if (!(MiddleDaily$Date[i] %in% MiddleDaily[MiddleDaily$Sex == "Female",]$Date)) {
    row <- data.frame("Year"=as.character(year(MiddleDaily$Date[i])), "Date"=MiddleDaily$Date[i], "Sex"="Female", "Count"=0)
    MiddleDaily <- rbind(MiddleDaily, row)
  }
  if (MiddleDaily$Date[i] %in% MiddleDaily[MiddleDaily$Sex == "Male",]$Date == FALSE){
    row <- data.frame("Year"= as.character(year(MiddleDaily$Date[i])), "Date"= MiddleDaily$Date[i], "Sex"= "Male", "Count"=0)
    MiddleDaily <- rbind(MiddleDaily, row)
  }
}

SexByDayPY_Middle <- MiddleDaily %>%
  group_by(Sex, Year) %>%
  summarise_at(vars(Count), list(mean = mean, sd = sd, min = min, max = max))
SexByDay_Middle <- MiddleDaily %>%
  group_by(Sex) %>%
  summarise_at(vars(Count), list(mean = mean, sd = sd, min = min, max = max))
Middle_DaysinMdw <- MiddleSardine %>% 
  drop_na(Pit) %>%
  group_by(Sex, Pit, Year) %>% 
  summarise(Count = n())
Middle_DaysinMdwavg <- Up_DaysinMdw %>% 
  group_by(Sex) %>% 
  summarise_at(vars(Count), list(mean = mean, sd = sd, min = min, max = max))


#####12 Tag stats- returns, amplexus #####
Sardine_tag <- Sardine%>%
  drop_na(Pit) %>%
  add_column(detect = 1) %>%
  arrange(Date)
CHY <- Sardine_tag %>%
  select(Year, Pit, Sex, detect) %>%
  distinct() %>%
  pivot_wider(names_from=Year, values_from=detect, values_fill = 0)
Malecapt2016to2019recaplast2 <- Sardine_tag %>%
  filter(Sex == "Male") %>%
  rename("2016" = "Y2016") #, 2017 = Y2017, 2018 = Y2018, 2019 = Y2019, 2020 = Y2020, 2021 = Y2021, 2022 = Y2022, 2023 = Y2023, 2024 = Y2024))
  filter(2016 == "1" | 2017 == "1" | 2018 == "1")
#####13 Annual Tag capture histories (CH) at each site#####
Sardine_tagSiteY <- Sardine_tag[,c("Site", "Year", "Pit", "Sex", "detect")]

UpperSardine_CHY <- Sardine_tagSiteY %>%
  filter(Site == "Upper Sardine")
UpperSardine_CHY <- UpperSardine_CHY %>%
  drop_na(Pit)
UpperSardine_CHY <- UpperSardine_CHY %>%
  distinct() %>%
  pivot_wider(names_from=Year, values_from=detect, values_fill = 0)
UpperSardine_CHY$FirstCapture <- apply(UpperSardine_CHY[-c(1:3)], 1, function(x) names(x)[min(which(x > 0))])
#UpperSardine_CHY$LastCapture <- apply(UpperSardine_CHY[c(4:11)], 1, function(x) names(x)[max(which(x > 0))])

LowerSardine_CHY <- Sardine_tagSiteY %>%
  filter(Site == "Lower Sardine")
LowerSardine_CHY <- LowerSardine_CHY %>%
  drop_na(Pit)
LowerSardine_CHY <- LowerSardine_CHY %>%
  distinct() %>%
  pivot_wider(names_from=Year, values_from=detect, values_fill = 0)
#LowerSardine_CHY$FirstCapture <- apply(LowerSardine_CHY[-c(1:3)], 1, function(x) names(x)[min(which(x > 0))])
#LowerSardine_CHY$LastCapture <- apply(LowerSardine_CHY[c(4:11)], 1, function(x) names(x)[max(which(x > 0))])

MiddleSardine_CHY <- Sardine_tagSiteY %>%
  filter(Site == "Middle Sardine")
MiddleSardine_CHY <- MiddleSardine_CHY %>%
  drop_na(Pit)
MiddleSardine_CHY <- MiddleSardine_CHY %>%
  distinct() %>%
  pivot_wider(names_from=Year, values_from=detect, values_fill = 0)
#MiddleSardine_CHY$FirstCapture <- apply(MiddleSardine_CHY[-c(1:3)], 1, function(x) names(x)[min(which(x > 0))])
#MiddleSardine_Survival$LastCapture <- apply(MiddleSardine_Survival[c(4:11)], 1, function(x) names(x)[max(which(x > 0))])



#####14 Daily Tag capture histories (CH) at each site, and by male/female#####
Sardine_D <- Sardine_tag[,c("Site", "Year", "Date", "Pit", "Sex", "detect")]

UpperSardine_D <- Sardine_D %>%
  filter(Site == "Upper Sardine")
UpperSardine_CHD <- UpperSardine_D %>%
  distinct() %>%
  pivot_wider(names_from=Date, values_from=detect, values_fill = 0)
UpperSardine_CHD$FirstCapture <- apply(UpperSardine_CHD[-c(1:3)], 1, function(x) names(x)[min(which(x > 0))])
#UpperSardine_CHD$LastCapture <- apply(UpperSardine_CHD[c(4:11)], 1, function(x) names(x)[max(which(x > 0))])
UpperCHD_Fem <- UpperSardine_CHD %>%
  filter(Sex == "Female")
UpperCHD_Male <- UpperSardine_CHD %>%
  filter(Sex == "Male")

LowerSardine_D <- Sardine_D %>%
  filter(Site == "Lower Sardine")
LowerSardine_CHD <- LowerSardine_D %>%
  distinct() %>%
  pivot_wider(names_from=Date, values_from=detect, values_fill = 0)
LowerSardine_CHD$FirstCapture <- apply(LowerSardine_CHD[-c(1:3)], 1, function(x) names(x)[min(which(x > 0))])
#LowerSardine_CHD$LastCapture <- apply(LowerSardine_CHD[c(4:11)], 1, function(x) names(x)[max(which(x > 0))])
LowerCHD_Fem <- LowerSardine_CHD %>%
  filter(Sex == "Female")
LowerCHD_Male <- LowerSardine_CHD %>%
  filter(Sex == "Male")

MiddleSardine_D <- Sardine_D %>%
  filter(Site == "Middle Sardine")
MiddleSardine_CHD <- MiddleSardine_D %>%
  distinct() %>%
  pivot_wider(names_from=Date, values_from=detect, values_fill = 0)
MiddleSardine_CHD$FirstCapture <- apply(MiddleSardine_CHD[-c(1:3)], 1, function(x) names(x)[min(which(x > 0))])
#MiddleSardine_CHD$LastCapture <- apply(MiddleSardine_CHD[c(4:11)], 1, function(x) names(x)[max(which(x > 0))])
MiddleCHD_Fem <- MiddleSardine_CHD %>%
  filter(Sex == "Female")
MiddleCHD_Male <- MiddleSardine_CHD %>%
  filter(Sex == "Male")




#####15 ROBUST POP MODELING#####
##########Robust pools detection across years but assigns between session imm/emigration rates
##########First, check the data in Datebysite, could possibly filter out dates where we only 
#############################recorded one or two toads because it's out of our target window of peak breeding? Might minutely influence detection rate?
Sardine_tagMale <- Sardine%>%
  filter(Sex== "Male") %>%
  drop_na(Pit) %>%
  add_column(detect = 1) %>%
  arrange(Date)

Datebysite <- Sardine_tag %>%
  group_by(Site, Date) %>% 
  summarise(Count = n()) 

#PITDate <- Sardine_tag[,c("Site", "Date", "Pit", "detect")]
##########UPPER###########
UpperPIT_Date <- Sardine_tagMale[(Sardine_tagMale$Site == "Upper Sardine"),c("Site", "Date", "Pit", "detect")]

Upper_CH_Date <- UpperPIT_Date %>%
  distinct() %>%
  pivot_wider(names_from=Date, values_from=detect, values_fill = 0) %>%
  unite("ch", "2016-05-31":tail(names(.),1), sep = "") %>% 
  select(!1:2)
Upper_Robust <- Upper_CH_Date %>%
  group_by(ch) %>%
  summarise(freq = n()) %>%
  as.data.frame(Upper_Robust)  

#program mark (rmark) reads the capture history as a string of 0's and 1's. this is accounted for by the user in the option to set time intervals. 
#a 0 indicates that there is no time between sessions and the population is closed, and a 1 indicates one time period and coincides with the last session in each period
Upper_Dates <- Datebysite %>%
  filter(Site == "Upper Sardine")
Uppertime.intervals <- ifelse(year(Upper_Dates$Date) == year(lag(Upper_Dates$Date)), 0,1)
Uppertime.intervals <- Uppertime.intervals[!is.na (Uppertime.intervals)]
  
run.robust=function()
{
  #random emigration, p=c varies by time and session, S by time
  GammaDoublePrime.random=list(formula=~time,share=TRUE) #gamma"=gamma' because share=TRUE
  p.time.session=list(formula=~-1+session:time,share=TRUE) #p=c because share=TRUE
  S.time=list(formula=~time)
  model.1=mark(data=Upper_Robust,model="Robust",
               time.intervals=Uppertime.intervals,
               model.parameters=list(S=S.time,GammaDoublePrime=GammaDoublePrime.random,p=p.time.session),threads=2,delete=TRUE)
  
  #random emigration, p varies by session, uses Mh but pi fixed to 1, S by time. this model is in the example Robust with MARK but it is a silly example because it uses the heterogeneity model but then fixes pi=1, which means there is no heterogeneity. probably the data were not generated under Mh. see results of model.2.b
  p.session=list(formula=~-1+session,share=TRUE)
  pi.fixed=list(formula=~1,fixed=1) #i have no idea what pi is. is pi supposed to be Phi?
  model.2.a=mark(data = Upper_Robust, model = "RDHet", 
                 time.intervals=Uppertime.intervals,
                 model.parameters=list(S=S.time,
                                       GammaDoublePrime=GammaDoublePrime.random,
                                       p=p.session,pi=pi.fixed),threads=2,delete=TRUE)
  
  #random emigration, p varies by session, uses Mh and in this case pi varies and so does p across mixtures with an additive session effect
  pi.dot=list(formula=~1)
  p.session.mixture=list(formula=~session+mixture,share=TRUE)
  model.2.b=mark(data = Upper_Robust, model = "RDHet", 
                 time.intervals=Uppertime.intervals,
                 model.parameters=list(S=S.time,
                                       GammaDoublePrime=GammaDoublePrime.random,
                                       p=p.session.mixture,pi=pi.dot),threads=2,delete=TRUE)
  
  #markov constant emigration rates, pi varies by session, p=c varies by session, S constant. this model is in the example Robust with MARK but it is a silly example because it uses the heterogeneity model but then fixes pi=1, which means there is no heterogeneity. probably the data were not generated under Mh. see results of model.3.b
  pi.session=list(formula=~session)
  p.session=list(formula=~-1+session,share=TRUE)
  S.dot=list(formula=~1)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.3.a=mark(data = Upper_Robust, model = "RDHet", 
                 time.intervals=Uppertime.intervals,
                 model.parameters=list(S=S.dot,
                                       GammaPrime=GammaPrime.dot,
                                       GammaDoublePrime=GammaDoublePrime.dot,
                                       p=p.session,pi=pi.session),threads=2,delete=TRUE)
  
  #markov constant emigration rates, pi varies by session, p=c varies by session+mixture, S constant. this is model.3.a but allows pi into the model by varying p/c by mixture
  S.dot=list(formula=~1)
  pi.session=list(formula=~session)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.3.b=mark(data = Upper_Robust, model = "RDHet", 
                 time.intervals=Uppertime.intervals,
                 model.parameters=list(S=S.dot,
                                       GammaPrime=GammaPrime.dot,
                                       GammaDoublePrime=GammaDoublePrime.dot,
                                       p=p.session.mixture,pi=pi.session),threads=2,delete=TRUE)
  
  #Huggins random emigration, p=c varies by time and session, S by time. 
  #beware that this model is not quite the same as the others above that say random emigration 
  #because the rates have been fixed for the last 2 occasions. that was done with PIMS in the 
  #MARK example and here it is done by binning the times so that times 3 and 4 are in the same bin,
  #so the time model has 3 levels (1,2, and 3-4). by doing so, the parameters become identifiable 
  #but this may not be reasonable depending on the particulars of the data. 
  #note that the same time binning must be done both for GammaPrime and GammaDoublePrime because 
  #the parameters are the same in the random emigration model. if you forget to bin one of the parameters 
  #across time, it will fit a model but it won't be what you expect as it will not share parameters. 
  #note the use of the argument "right". this controls whether binning is inclusive on the right (right=TRUE) 
  #or on the left (right=FALSE). using "right" nested in the list of design parameters is equivalent to 
  #using it as a calling argument to make.design.data or add.design.data.
  #  S.time=list(formula=~time)
  #  p.time.session=list(formula=~-1+session:time,share=TRUE)
  #  GammaDoublePrime.random=list(formula=~time,share=TRUE)
  #  model.4=mark(data = Upper_Robust, model = "RDHuggins", 
  #               time.intervals=Uppertime.intervals,design.parameters=
  #                 list(GammaDoublePrime=list(time.bins=c(1,2,5))),
  #               right=FALSE, model.parameters=
  #                 list(S=S.time,GammaDoublePrime=GammaDoublePrime.random,
  #                      p=p.time.session),threads=2,delete=TRUE)
  
  return(collect.models())
}

Upperrobust.results=run.robust()

#you will receive a warning message that the model list includes models of different types which are not compatible for comparisons of AIC. 
#that is because the runs include closed models which include N in the likelihood and Huggins models which don't include N in the likelihood. 
#that can be avoided by running the two types of models in different sets

#look at results
Upperrobust.results

round(Upperrobust.results$model.1$results$real[, 1:4], 2) #2 decimal places

#get population estimates from model
Upper_robust.popest <- round(Upperrobust.results$model.1$results$derived$'N Population Size', 2)

UpperYrs <- Sardine_tagMale %>%
  filter(Site=="Upper Sardine") %>%
  select(Year) %>%
  distinct() %>%
  arrange(Year) 
Upper_robust.popest$Year <- UpperYrs$Year

write.csv(Upper_robust.popest, file = "Upper_RPE.csv")

ggplot(Upper_robust.popest, aes(x=(Year), y=estimate)) +
  geom_point(color="darkorchid", size=1.2) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl),color="black",width=0.4) +
  theme_classic() +
  ggtitle(stringr::str_wrap("Estimated Population Size of Male Yosemite toads at Upper Sardine Across Survey Years")) +
  labs(x="Year",y="Population Size (#)") +
  theme(plot.title = element_text(size=10)) +
  theme(axis.text.y=element_text(size=10)) +
  theme(axis.text.x=element_text(angle = 300)) +
  theme(plot.title = element_text(hjust = 0.5)) #+
  #scale_x_continuous("Year", labels = as.character(Upper.RPE.graph$Year), breaks = Upper.RPE.graph$Year) +
  #scale_y_continuous(breaks = seq(0, 350, by = 50)) +
  #coord_cartesian(ylim=c(0,350))

##########LOWER###########

LowerPIT_Date <- Sardine_tag[(Sardine_tag$Site == "Lower Sardine"),c("Site", "Date", "Pit", "detect")]

Lower_CH_Date <- LowerPIT_Date %>%
  distinct() %>%
  pivot_wider(names_from=Date, values_from=detect, values_fill = 0) %>%
  unite("ch", "2016-05-31":tail(names(.),1), sep = "") %>% 
  select(!1:2)
Lower_Robust <- Lower_CH_Date %>%
  group_by(ch) %>%
  summarise(freq = n()) %>%
  as.data.frame(Lower_Robust)  

Lower_Dates <- Datebysite %>%
  filter(Site == "Lower Sardine")
Lowertime.intervals <- ifelse(year(Lower_Dates$Date) == year(lag(Lower_Dates$Date)), 0,1)
Lowertime.intervals <- Lowertime.intervals[!is.na (Lowertime.intervals)]

run.robust=function()
{
  #random emigration, p=c varies by time and session, S by time
  GammaDoublePrime.random=list(formula=~time,share=TRUE) #gamma"=gamma' because share=TRUE
  p.time.session=list(formula=~-1+session:time,share=TRUE) #p=c because share=TRUE
  S.time=list(formula=~time)
  model.1=mark(data=Lower_Robust,model="Robust",
               time.intervals=Lowertime.intervals,
               model.parameters=list(S=S.time,GammaDoublePrime=GammaDoublePrime.random,p=p.time.session),threads=2,delete=TRUE)
  
  #random emigration, p varies by session, uses Mh but pi fixed to 1, S by time. this model is in the example Robust with MARK but it is a silly example because it uses the heterogeneity model but then fixes pi=1, which means there is no heterogeneity. probably the data were not generated under Mh. see results of model.2.b
  p.session=list(formula=~-1+session,share=TRUE)
  pi.fixed=list(formula=~1,fixed=1) #i have no idea what pi is. is pi supposed to be Phi?
  model.2.a=mark(data = Lower_Robust, model = "RDHet", 
                 time.intervals=Lowertime.intervals,
                 model.parameters=list(S=S.time,
                                       GammaDoublePrime=GammaDoublePrime.random,
                                       p=p.session,pi=pi.fixed),threads=2,delete=TRUE)
  
  #random emigration, p varies by session, uses Mh and in this case pi varies and so does p across mixtures with an additive session effect
  pi.dot=list(formula=~1)
  p.session.mixture=list(formula=~session+mixture,share=TRUE)
  model.2.b=mark(data = Lower_Robust, model = "RDHet", 
                 time.intervals=Lowertime.intervals,
                 model.parameters=list(S=S.time,
                                       GammaDoublePrime=GammaDoublePrime.random,
                                       p=p.session.mixture,pi=pi.dot),threads=2,delete=TRUE)
  
  #markov constant emigration rates, pi varies by session, p=c varies by session, S constant. this model is in the example Robust with MARK but it is a silly example because it uses the heterogeneity model but then fixes pi=1, which means there is no heterogeneity. probably the data were not generated under Mh. see results of model.3.b
  pi.session=list(formula=~session)
  p.session=list(formula=~-1+session,share=TRUE)
  S.dot=list(formula=~1)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.3.a=mark(data = Lower_Robust, model = "RDHet", 
                 time.intervals=Lowertime.intervals,
                 model.parameters=list(S=S.dot,
                                       GammaPrime=GammaPrime.dot,
                                       GammaDoublePrime=GammaDoublePrime.dot,
                                       p=p.session,pi=pi.session),threads=2,delete=TRUE)
  
  #markov constant emigration rates, pi varies by session, p=c varies by session+mixture, S constant. this is model.3.a but allows pi into the model by varying p/c by mixture
  S.dot=list(formula=~1)
  pi.session=list(formula=~session)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.3.b=mark(data = Lower_Robust, model = "RDHet", 
                 time.intervals=Lowertime.intervals,
                 model.parameters=list(S=S.dot,
                                       GammaPrime=GammaPrime.dot,
                                       GammaDoublePrime=GammaDoublePrime.dot,
                                       p=p.session.mixture,pi=pi.session),threads=2,delete=TRUE)
  
  #Huggins random emigration, p=c varies by time and session, S by time. 
  #beware that this model is not quite the same as the others above that say random emigration 
  #because the rates have been fixed for the last 2 occasions. that was done with PIMS in the 
  #MARK example and here it is done by binning the times so that times 3 and 4 are in the same bin,
  #so the time model has 3 levels (1,2, and 3-4). by doing so, the parameters become identifiable 
  #but this may not be reasonable depending on the particulars of the data. 
  #note that the same time binning must be done both for GammaPrime and GammaDoublePrime because 
  #the parameters are the same in the random emigration model. if you forget to bin one of the parameters 
  #across time, it will fit a model but it won't be what you expect as it will not share parameters. 
  #note the use of the argument "right". this controls whether binning is inclusive on the right (right=TRUE) 
  #or on the left (right=FALSE). using "right" nested in the list of design parameters is equivalent to 
  #using it as a calling argument to make.design.data or add.design.data.
  #  S.time=list(formula=~time)
  #  p.time.session=list(formula=~-1+session:time,share=TRUE)
  #  GammaDoublePrime.random=list(formula=~time,share=TRUE)
  #  model.4=mark(data = Lower_Robust, model = "RDHuggins", 
  #               time.intervals=Lowertime.intervals,design.parameters=
  #                 list(GammaDoublePrime=list(time.bins=c(1,2,5))),
  #               right=FALSE, model.parameters=
  #                 list(S=S.time,GammaDoublePrime=GammaDoublePrime.random,
  #                      p=p.time.session),threads=2,delete=TRUE)
  
  return(collect.models())
}

Lowerrobust.results=run.robust()

#you will receive a warning message that the model list includes models of different types which are not compatible for comparisons of AIC. 
#that is because the runs include closed models which include N in the likelihood and Huggins models which don't include N in the likelihood. 
#that can be avoided by running the two types of models in different sets

#look at results
Lowerrobust.results

round(Lowerrobust.results$model.1$results$real[, 1:4], 2) #2 decimal places

#get population estimates from model
Lower_robust.popest <- round(Lowerrobust.results$model.1$results$derived$'N Population Size', 2)

LowerYrs <- Sardine_tag %>%
  filter(Site=="Lower Sardine") %>%
  select(Year) %>%
  distinct() %>%
  arrange(Year) 
Lower_robust.popest$Year <- LowerYrs$Year

write.csv(Lower_robust.popest, file = "Lower_RPE.csv")

ggplot(Lower_robust.popest, aes(x=(Year), y=estimate)) +
  geom_point(color="darkorchid", size=1.2) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl),color="black",width=0.4) +
  theme_classic() +
  ggtitle(stringr::str_wrap("Estimated Population Size of Yosemite toads at Lower Sardine Across Survey Years")) +
  labs(x="Year",y="Population Size (#)") +
  theme(plot.title = element_text(size=10)) +
  theme(axis.text.y=element_text(size=10)) +
  theme(axis.text.x=element_text(angle = 300)) +
  theme(plot.title = element_text(hjust = 0.5)) #+
#scale_x_continuous("Year", labels = as.character(Lower.RPE.graph$Year), breaks = Lower.RPE.graph$Year) +
#scale_y_continuous(breaks = seq(0, 350, by = 50)) +
#coord_cartesian(ylim=c(0,350))

#####16 PopEst JollySeber(js) Annual Detections#####

#the js method compresses capture history into a single 1/0 detection per year
#as such, it isn't as refined as the robust model above

UpperSardine_CHY_rmark<- UpperSardine_CHY %>% 
  unite("ch", "2016":tail(names(.),1), sep = "") %>%
  filter(!Sex =="Female")
UpperSardine_CHY_rmark<- UpperSardine_CHY_rmark %>% 
  select(!c(1:3))
Upper_Y <- UpperSardine_CHY_rmark %>%
  group_by(ch) %>%
  summarise(freq = n())
UpperSardine_CHY_rmark<-as.data.frame(Upper_Y)
UpperSardine_CHY.proc<-process.data(UpperSardine_CHY_rmark,
                                model="POPAN",)
UpperSardine_CHY.ddl<-make.design.data(UpperSardine_CHY.proc)
names(UpperSardine_CHY.proc) #ch: capture history; freq: the number of animals with that capture history
head(UpperSardine_CHY.proc$data)

run.js<-function(){
  #define model formulas for Phi
  Phi.dot<-list(formula=~1) #constant survival
  Phi.time<-list(formula=~time) #survival differs between discrete times
  #Phi.sex <- list(formula=~Sex)
  #define model formulas for p
  p.dot<-list(formula=~1) #constant detection
  ## NOTE: i did not consider constant detection in my analysis because the number of passes differed each year at this site ##
  p.time<-list(formula=~time) #detection differs between discrete times
  #p.sex <- list(formula=~Sex)
  #define model formulas for pent
  pent.dot<-list(formula=~1) #constant probability of entry
  pent.time<-list(formula=~time) #probability of entry differs between discrete times
  #pent.sex <- list(formula=~Sex)
  #define model formulas for N (note: don't confuse "N" from model with predicted population size)
  N.dot<-list(formula=~1) #models won't run if you try to vary "N" over time
  #create model list and run assortment of models
  model.list<-create.model.list("POPAN")
  results<-mark.wrapper(model.list,data=UpperSardine_CHY.proc,ddl=UpperSardine_CHY.ddl,delete=TRUE)
  
  return(results)
}

js.results<-run.js()
js.results

round(js.results$Phi.dot.p.time.pent.time.N.dot$results$real[, 1:4], 3)
Upper.js<-round(js.results$Phi.dot.p.time.pent.time.N.dot$results$derived$'N Population Size', 3)
Upper.js
rownames(Upper.js)<-c("Y2016", "Y2017","Y2018","Y2019","Y2020","Y2021","Y2022", "Y2023")
Upper.js



LowerSardine_CHY_rmark<- LowerSardine_CHY %>% 
  unite("ch", "2016":tail(names(.),1), sep = "") %>%
  filter(!Sex =="Female")
LowerSardine_CHY_rmark<- LowerSardine_CHY_rmark %>% 
  select(!c(1:3))
Lower_Y <- LowerSardine_CHY_rmark %>%
  group_by(ch) %>%
  summarise(freq = n())
LowerSardine_CHY_rmark<-as.data.frame(Lower_Y)
LowerSardine_CHY.proc<-process.data(LowerSardine_CHY_rmark,
                                    model="POPAN",)
LowerSardine_CHY.ddl<-make.design.data(LowerSardine_CHY.proc)

run.js<-function(){
  #define model formulas for Phi
  Phi.dot<-list(formula=~1) #constant survival
  Phi.time<-list(formula=~time) #survival differs between discrete times
  #Phi.sex <- list(formula=~Sex)
  #define model formulas for p
  p.dot<-list(formula=~1) #constant detection
  ## NOTE: i did not consider constant detection in my analysis because the number of passes differed each year at this site ##
  p.time<-list(formula=~time) #detection differs between discrete times
  #p.sex <- list(formula=~Sex)
  #define model formulas for pent
  pent.dot<-list(formula=~1) #constant probability of entry
  pent.time<-list(formula=~time) #probability of entry differs between discrete times
  #pent.sex <- list(formula=~Sex)
  #define model formulas for N (note: don't confuse "N" from model with predicted population size)
  N.dot<-list(formula=~1) #models won't run if you try to vary "N" over time
  #create model list and run assortment of models
  model.list<-create.model.list("POPAN")
  results<-mark.wrapper(model.list,data=LowerSardine_CHY.proc,ddl=LowerSardine_CHY.ddl,delete=TRUE)
  
  return(results)
}

js.results<-run.js()
js.results

round(js.results$Phi.dot.p.time.pent.dot.N.dot$results$real[, 1:4], 3)
Lower.js<-round(js.results$Phi.dot.p.dot.pent.dot.N.dot$results$derived$'N Population Size', 3)
Lower.js
rownames(Lower.js)<-c("Y2016", "Y2017","Y2018","Y2019","Y2020","Y2021","Y2022", "Y2023")
Lower.js



MiddleSardine_CHY_rmark<- MiddleSardine_CHY %>% 
  unite("ch", "2020":tail(names(.),1), sep = "") %>%
  filter(!Sex =="Female")
MiddleSardine_CHY_rmark<- MiddleSardine_CHY_rmark %>% 
  select(!c(1:3))
Middle_Y <- MiddleSardine_CHY_rmark %>%
  group_by(ch) %>%
  summarise(freq = n())
MiddleSardine_CHY_rmark<-as.data.frame(Middle_Y)
MiddleSardine_CHY.proc<-process.data(MiddleSardine_CHY_rmark,
                                    model="POPAN",)
MiddleSardine_CHY.ddl<-make.design.data(MiddleSardine_CHY.proc)

run.js<-function(){
  #define model formulas for Phi
  Phi.dot<-list(formula=~1) #constant survival
  Phi.time<-list(formula=~time) #survival differs between discrete times
  #Phi.sex <- list(formula=~Sex)
  #define model formulas for p
  p.dot<-list(formula=~1) #constant detection
  ## NOTE: i did not consider constant detection in my analysis because the number of passes differed each year at this site ##
  p.time<-list(formula=~time) #detection differs between discrete times
  #p.sex <- list(formula=~Sex)
  #define model formulas for pent
  pent.dot<-list(formula=~1) #constant probability of entry
  pent.time<-list(formula=~time) #probability of entry differs between discrete times
  #pent.sex <- list(formula=~Sex)
  #define model formulas for N (note: don't confuse "N" from model with predicted population size)
  N.dot<-list(formula=~1) #models won't run if you try to vary "N" over time
  #create model list and run assortment of models
  model.list<-create.model.list("POPAN")
  results<-mark.wrapper(model.list,data=MiddleSardine_CHY.proc,ddl=MiddleSardine_CHY.ddl,delete=TRUE)
  
  return(results)
}

js.results<-run.js()
js.results

round(js.results$Phi.dot.p.time.pent.dot.N.dot$results$real[, 1:4], 3)
Middle.js<-round(js.results$Phi.dot.p.dot.pent.dot.N.dot$results$derived$'N Population Size', 3)
Middle.js
rownames(Middle.js)<-c("Y2020","Y2021","Y2022", "Y2023")
Middle.js



PopEst <- read.xlsx("Sardine_PIT_Squeeky.xlsx", sheet = "PopEst", detectDates = T)
ggplot(PopEst, aes(x=Year, y=Est, fill=Site)) +
  geom_line(aes(linetype=Site), color="darkorchid", size=1.2) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),color="black",width=0.4) +
  theme_classic() +
  ggtitle("Change in Sardine Population Size Over Time (Jolly-Seber Model)") +
  labs(x="Year",y="Population Size (#)") +
  theme(
    axis.title=element_text(size=12),
    axis.text=element_text(size=12),
    title=element_text(size=12)
  )

Sardine_CHMale <- Sardine_CHY %>%
  filter (Sex == "Male")
CHMale<- Sardine_CHMale %>% 
  unite("ch", "2016":tail(names(.),1), sep = "") %>%
  select(!c(1:2))
CHMale <- CHMale %>%
  group_by(ch) %>%
  summarise(freq = n())



Sardine_CHFem <- Sardine_CHY %>%
  filter (Sex == "Female")
CHFem<- Sardine_CHFem %>% 
  unite("ch", "2016":tail(names(.),1), sep = "") %>%
  select(!c(1:2))
CHFem <- CHFem %>%
  group_by(ch) %>%
  summarise(freq = n())
Fem_rmark<-as.data.frame(CHFem)
Fem.proc<-process.data(Fem_rmark,
                               model="POPAN",)
Fem.ddl<-make.design.data(Fem.proc)
names(Fem.proc) #ch: capture history; freq: the number of animals with that capture history
head(Fem.proc$data)

run.js<-function(){
  #define model formulas for Phi
  Phi.dot<-list(formula=~1) #constant survival
  Phi.time<-list(formula=~time) #survival differs between discrete times
  #Phi.sex <- list(formula=~Sex)
  #define model formulas for p
  p.dot<-list(formula=~1) #constant detection
  ## NOTE: i did not consider constant detection in my analysis because the number of passes differed each year at this site ##
  p.time<-list(formula=~time) #detection differs between discrete times
  #p.sex <- list(formula=~Sex)
  #define model formulas for pent
  pent.dot<-list(formula=~1) #constant probability of entry
  pent.time<-list(formula=~time) #probability of entry differs between discrete times
  #pent.sex <- list(formula=~Sex)
  #define model formulas for N (note: don't confuse "N" from model with predicted population size)
  N.dot<-list(formula=~1) #models won't run if you try to vary "N" over time
  #create model list and run assortment of models
  model.list<-create.model.list("POPAN")
  results<-mark.wrapper(model.list,data=Fem.proc,ddl=Fem.ddl,delete=TRUE)
  
  return(results)
}

js.results<-run.js()
js.results

round(js.results$Phi.dot.p.time.pent.time.N.dot$results$real[, 1:4], 3)
Fem.js<-round(js.results$Phi.dot.p.time.pent.time.N.dot$results$derived$'N Population Size', 3)
Fem.js

rownames(Fem.js)<-c("Y2016", "Y2017","Y2018","Y2019","Y2020","Y2021","Y2022", "Y2023")
Fem.js
Female <- as.data.frame(Fem.js)
Female$Year <- c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)


ggplot(Female, aes(x=Year, y=estimate)) +
  geom_line(color="darkorchid", size=1.2) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl),color="black",width=0.4) +
  theme_classic() +
  ggtitle("Change in Sardine Female Population Size Over Time (Jolly-Seber Model)") +
  labs(x="Year",y="Population Size (#)") +
  theme(
    axis.title=element_text(size=12),
    axis.text=element_text(size=12),
    title=element_text(size=12)
  )

#####17 Egg Masses#####
EM <- read_xlsx("Sardine_EggMass_and_VisualObs_20241003JWelty.xlsx", sheet = "Sardine Egg Masses", trim_ws = TRUE)
EM$Year <- format(as.Date(EM$SurveyDate, format="%Y-%m-%d"),"%Y")
EM$SurveyDate <- format(as.Date(EM$SurveyDate, format="%Y-%m-%d"),"%Y-%m-%d")

EMSurveys <- EM %>%
  group_by(HigherGrouping, SurveyDate, Year, Pass) %>%
  summarise(SumEMs = sum(NumberOfMasses))
write.csv(EMSurveys, "EMSurveys.csv")
#this table still needs some work because surveys were taken simultaneously on two tablets in some cases, so those entries need to be summed together

EM_UpExamine <- EM %>%
  filter(HigherGrouping == "Upper") %>%
  select(SurveyorName, SurveyorNameOther, Year, SurveyDate, Pass, GeneralComments, EggMassSardinePond, EggMassMarkRecap, NumberOfMasses) %>%
  group_by(Year, Pass, EggMassSardinePond, EggMassMarkRecap) %>%
  summarize(EM = sum(NumberOfMasses))
EM_2023Up <- EM_UpExamine[EM_UpExamine$Year== "2023",]

EM_MiddleExamine <- EM %>%
  filter(HigherGrouping == "Middle") %>%
  select(SurveyorName, SurveyorNameOther, Year, SurveyDate, Pass, GeneralComments, EggMassMarkRecap, NumberOfMasses) %>%
  group_by(Year, Pass, EggMassMarkRecap) %>%
  summarize(EM = sum(NumberOfMasses))



#####END OF CODE#####