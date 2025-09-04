#################################################################
#########Columbia Spotted Frog Mark-Recapture####################
###Data QAQC, capture stats, and JS POPAN pop estimates##########
################Kelsey Marshall 2024#############################
#################################################################

###install packages as needed, only need to do this once per package and then r will be able to recall them
install.packages("")
#recall packages, do this each time you re-open R
library("readxl")
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(RMark)

### set working directory (make a project folder where you'll keep files you need and where you'll put any outputs like tables, figures)
setwd("C:/Users/KelseyMarshall/Documents/CSF_Rproject")      

### read in data
CSF <- read_excel("NENV_CSF2024.xls", sheet = "Compiled", trim_ws = TRUE)
#make sure PIT#s in excel are formatted as numbers with no decimals, not sci not by examining PIT column IDs with first digit = 9
CSF$PIT_Tag[8651]
CSF$PIT_Tag <- gsub("\r", "",CSF$PIT_Tag)
CSF$PIT_Tag <- gsub("\n", "",CSF$PIT_Tag)
CSF$PIT_Tag[8651]

###
#####QA/QC data#####
###to start, format data and examine to ensure groups are consistently named
###if not, correct in excel file and read back in

### examine data structure, changing fields with groups into factors makes them easier to examine and ensure you have the groups you expect
summary(CSF)

CSF$Age <- replace_na(CSF$Age, replace = "Unknown")
CSF$Age = as.factor(CSF$Age)
summary(CSF$Age)

CSF$Mark_Recap <- replace_na(CSF$Mark_Recap, replace = "Unknown")
CSF$Mark_Recap <- as.factor(CSF$Mark_Recap)
summary(CSF$Mark_Recap)

CSF$Sex <- as.factor(CSF$Sex)
summary(CSF$Sex)

CSF$Site <- as.factor(CSF$Site)
summary(CSF$Site)

###create data frame for only tagged captures
CSF_tag <- CSF %>%
  drop_na(PIT_Tag)

#find pits with fewer or greater than 15 digits
CSF_pitoddlength <- CSF_tag %>%
  filter(startsWith(PIT_Tag, "9")) %>%
  filter(nchar(PIT_Tag)!= 15)
view(CSF_pitoddlength)
#check thru and correct in excel where possible, where not possible make notes and maybe remove incorrect pit to notes

#find pits with sex discrepancies
CSF_checkPITSex <- CSF_tag[,c("PIT_Tag", "Sex")]
CSF_checkPITSex <- CSF_checkPITSex %>%
  distinct()
CSF_checkPITSex <- CSF_checkPITSex[,c("PIT_Tag")]
CSF_checkPITSex$Duplicate <- duplicated(CSF_checkPITSex$PIT_Tag)
CSF_checkPITSex <- CSF_checkPITSex[CSF_checkPITSex$Duplicate == "TRUE",]
view(CSF_checkPITSex)
#check thru and correct in excel where possible, where not possible write unknown, but keep original sex in notes



###
###QAQC ends here
###


###
#####Summary stats starts here#####
CSF$Survey_Year <- as.factor(CSF$Survey_Year)

### generate capture totals per year per site by sex and age
#sex table will give you correct Males,Females, and Adults of unknown sex
CapturesbySex <- CSF %>%
  group_by(Site, Sex, Survey_Year) %>% 
  summarise(Count = n()) %>%
  pivot_wider(, names_from = Survey_Year, values_from = Count, values_fill = 0)
#age table will give you correct total number of adults and juveniles (table with sex underestimates juvies because juvies were sex ID'd if they were big enough, so M/Fs include some juvies)
CapturesbyAge <- CSF %>%
  group_by(Site, Age, Survey_Year) %>% 
  summarise(Count = n()) %>%
  pivot_wider(, names_from = Survey_Year, values_from = Count, values_fill = 0)

###generate table of mark, recap, total
#first generate tables of total unique and new marks per year per site, then subtract # new marks from total # unique for the year
#I did it this way because over multiple passes in a single year, you could mark a frog and recapture it in the same year, skewing recapture numbers if you just summed up total recaps in a year
CSFPIT <- CSF %>%
  drop_na(PIT_Tag)
Uniq_byYear <- CSFPIT %>%
  distinct(Site, Survey_Year, PIT_Tag) %>%
  group_by(Site, Survey_Year) %>% 
  summarise(Count = n()) %>%
  pivot_wider(, names_from = Survey_Year, values_from = Count, values_fill = 0) %>%
  add_column(MRT = "TotalUnique", .before = "2004")
Mark_byYear <- CSFPIT %>%
  filter(Mark_Recap == "Mark") %>%
  group_by(Site, Survey_Year) %>% 
  summarise(Count = n()) %>%
  pivot_wider(, names_from = Survey_Year, values_from = Count, values_fill = 0) %>%
  add_column(MRT = "Mark", .before = "2004")

UniqMarkbyYear <- rbind(Uniq_byYear, Mark_byYear)
Mark_Recap_Uniq_Yr <- UniqMarkbyYear %>%
  #its easier to work with columns than rows in R as far as I can tell, so change table to long form then separate mark and total unique into 2 columns to prepare for subtraction
  pivot_longer("2004":"2024", names_to = "Year", values_to = "Count") %>%
  pivot_wider(, names_from = MRT, values_from = Count) %>%
  #subtract mark row from unique row to get number of recaptures
  mutate(Recapture = TotalUnique - Mark) %>%
  #transform back to a wide table with those groups by site through years
  pivot_longer(cols = c(TotalUnique, Mark, Recapture), names_to = "MRT", values_to = "Count") %>%
  pivot_wider(names_from = Year, values_from = Count)

###get count of all unique tags by site deployed totaled across all survey years
TotalUnique <- CSFPIT %>%
  distinct(Site, PIT_Tag) %>%
  group_by(Site) %>% 
  summarise(Count = n()) 

###Make stacked columns chart for Marks and Recaps by Year, per Site
#easiest to make stacked column bar graph with a table w/column headings = Year, MRT (filtered to just Mark Recap groups), and Count
MarkRecapxYrChart <- UniqMarkbyYear %>%
  pivot_longer("2004":"2024", names_to = "Year", values_to = "Count") %>%
  pivot_wider(, names_from = MRT, values_from = Count) %>%
  mutate(Recapture = TotalUnique - Mark) %>%
  pivot_longer(cols = c(TotalUnique, Mark, Recapture), names_to = "MRT", values_to = "Count") %>%
  filter(!MRT== "TotalUnique")
ggplot(data=MarkRecapxYrChart, aes(x=Year, y= Count, fill=MRT)) +
  geom_bar(stat="identity") +
  labs(x = "Year", y = "Number of unique adults") +
  theme_classic() +
  guides(fill = guide_legend(title = NULL)) +
  theme(axis.text.y=element_text(size=10)) +
  theme(axis.text.x=element_text(angle = 300)) +
  facet_wrap(~Site, ncol = 1, scales = "free_y")


#####Modeling population estimates starts here#####

###Population estimates, Jolly-Seber POPAN formula 
CSFPIT_TG <- CSFPIT%>%
  add_column(detect = 1) %>%
  filter(Site == "Tennessee Gulch")
CSFPIT_TG <- CSFPIT_TG[,c("Survey_Year", "PIT_Tag", "detect")]
TG_CH_Yr <- CSFPIT_TG %>%
  distinct() %>%
  pivot_wider(names_from=Survey_Year, values_from=detect, values_fill = 0) %>%
  unite("ch", "2004":tail(names(.),1), sep = "") %>% 
  select(!1)
TG_RMark <- TG_CH_Yr %>%
  group_by(ch) %>%
  summarise(freq = n()) %>%
  as.data.frame(TG_RMark)
TG.proc<-process.data(TG_RMark,
                      model="POPAN",)
TG.ddl<-make.design.data(TG.proc)
names(TG.proc) #ch: capture history; freq: the number of animals with that capture history
head(TG.proc$data)

TG.run.js<-function(){
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
  results<-mark.wrapper(model.list,data=TG.proc,ddl=TG.ddl,delete=TRUE)
  
  return(results)
}

TG.js.results<-TG.run.js()
TG.js.results
#!!!not sure where the 3 originates at the end of this next line!!!
round(TG.js.results$Phi.time.p.time.pent.time.N.dot$results$real[, 1:4], 3)
TG.js<-round(TG.js.results$Phi.time.p.time.pent.time.N.dot$results$derived$'N Population Size', 3)
TG.js
TGYrs <- CSFPIT_TG %>%
  select(Survey_Year) %>%
  distinct() %>%
  arrange(Survey_Year)
TG.js$Year <- TGYrs$Survey_Year
TG.js

ggplot(TG.js, aes(x=Year, y=estimate)) +
  geom_line(color="darkorchid", size=1.2) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl),color="black",width=0.4) +
  theme_classic() +
  ggtitle("Estimated Population of Columbia Spotted Frogs at Tennessee Gulch Over Time") +
  labs(x="Year",y="Population Size (#)") +
  theme(
    axis.title=element_text(size=12),
    axis.text=element_text(size=12),
    title=element_text(size=12)
  )
