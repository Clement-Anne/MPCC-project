#######################################################
#                                                     #
#                 MPCC project                        #
#                       -                             #
#               2. Data wrangling                     #
#                                                     #
#######################################################

#############General infos################
##Author: Clément ANNE
##Author e-mail: clement.anne90@gmail.com
##Date: May 15,2022
##########################################

################Outline###################
##
##  2.1.  Encode MPCC team adhesions
##  2.2.  Encode MPCC rider adhesions
##  2.3.  Encode MPCC former rider adhesions
##  2.4.  Other wrangling
##
##################################################

#Clear current environment
rm(list=ls())

###Libraries
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("foreign", repos = "http://cran.us.r-project.org")


library(rvest)
library(stringr)
library(tidyverse)
library(lubridate)
library(caret)
library(knitr)
library(tinytex)
library(foreign)
library(haven)


###Install tinytex if needed
if(is_tinytex()==FALSE) tinytex::install_tinytex()

##################################################
#                                                #
#         2.1.  Encode MPCC team adhesions       #
#                                                #
##################################################

load(file=file.path("rda","Rider_results_2000_to_2022_dtb_clean"))
load(file=file.path("rda","MPCC_team_members"))

###Manual check structure name changes
#Assumption: Current year encoded if adhesion before 1st of July

#Kern with missing date on the website: December 20, 2019 
# https://www.mpcc.fr/l-equipe-kern-pharma-rejoint-le-mpcc/
MPCC_team_members <- MPCC_team_members%>%
  mutate(date_MPCC=dmy(date_MPCC),
         date_MPCC=if_else(team_MPCC=="KERN PHARMA",make_date(2019,12,20),date_MPCC),
         year_MPCC=round(decimal_date(date_MPCC)))

#1. AG2R: AG2R in name since 2000
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"AG2R")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[1])%>%
  .$team%>%
  unique()

#2. Bora: 
# Team Netapp (2012)
# Team Netapp - Endura (2013-2014)
# Bora -Argon 18 (2015-2016)
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"([Bb][Oo][Rr][Aa]|[Nn]et[Aa]pp)")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[2])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#3.Cofidis: Cofidis in name since 2000
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Co]fidis")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[3])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#4. EF
# Team Slipstream (2007)
# Team Garmin Chipotle (2008)
# Garmin (-> 2015)
# Cannondale Drapac (2016-2018)
# EF (since 2019)
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"([Ss]lipstream|[Gg]armin|EF|[Cc]annondale\\-[Dd]rapac)")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[4])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#5. Groupama FDJ
#Française des jeux | FDJ 
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"(FDJ|[Ff]ran[çc]aise)")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[5])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#6. Intermarché Wanty Gobert
#Wanty
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Ww]anty")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[6])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#7. Israel Startup Nation
#Israel
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Ii]sra[eë]l")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[7])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#8. Lotto Soudal
#Lotto Soudal
#Lotto Belisol
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Ll]otto\\s([Ss]oudal|[Bb]elisol)")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[8])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#9. Team DSM
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"DSM|[Ss]unweb|[Gg]iant|[Ss]himano|[Aa]rgos|[Ss]kil")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[9])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#10. Alpecin Fenix
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Aa]lpecin")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[10])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#11. Arkea Samsic
#The team was continental non pro up to 2010
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"([Aa]rk[ée]a|Fortun[eé]o|[Bb]retagne|[Aa]rmor)")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[11])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#12. B&B
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"B&B|[Vv]ital")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[12])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#13. Bardiani
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Bb]ardiani")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[13])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#14. Bingoal
#Continental Tour before 2017
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"([Bb]ingoal|[Ww]allonie|WB)")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[14])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#15. Burgos
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Bb]urgos")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[15])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#16. Caja Rural
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Cc]aja")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[16])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#17. Androni
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Aa]ndroni")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[17])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#18. Euskaltel
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Ee]uskaltel")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[18])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#19. Human Powered Health
#Continental before 2018
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"([Hh]uman|[Rr]ally|[Oo]ptum)")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[19])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#20. Kern Pharma
#Was continental in 2020
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Kk]ern")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[20])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#21. Novo Nordisk
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Nn]ovo")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[21])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#22. Sports Vlaanderen
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Vv]laanderen")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[22])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#23. Total
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"([Tt]otal|[Dd]irect|[Ee]uropcar|[Bb]ouygues)")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[23])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)

#24. Uno X
team_tag_list_MPCC <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(team_name,"[Uu][Nn][Oo](\\s|\\-)[Xx]")==TRUE)%>%
  filter(year>=MPCC_team_members$year_MPCC[24])%>%
  .$team%>%
  unique()%>%
  append(team_tag_list_MPCC)


Rider_results_2000_to_2022_dtb_clean <- Rider_results_2000_to_2022_dtb_clean%>%
  mutate(team_MPCC=if_else(team%in%team_tag_list_MPCC,1,0))


##################################################
#                                                #
#         2.2.  Encode MPCC rider adhesions      #
#                                                #
##################################################

####Assumption: Treat rider adhesion as time-invariant since no adhesion date is available

load(file=file.path("rda","MPCC_rider_dtb_Nov13_2022"))

##############World Tour and Pro Tour

MPCC_riders_WT_ProTeams <- MPCC_rider_dtb_Nov13_2022%>%
  rename(rider_name=rider_MPCC)%>%
  filter(str_detect(division,"(UCI\\sWORLD\\sTEAM|UCI\\sPRO\\sTEAM)")==TRUE)%>%
  select(rider_name)%>%
  mutate(rider_MPCC_2022list_WTPro=1)
nrow(MPCC_riders_WT_ProTeams) #230

MPCC_riders_WT_ProTeams%>%
  .$rider_name%>%
  unique()%>%
  length() #229

MPCC_riders_WT_ProTeams%>%
  group_by(rider_name)%>%
  mutate(n=n())%>%
  filter(n!=1)%>%
  ungroup()%>%
  .$rider_name
#Benhamouda twice in the Web page


#Check merge for all riders
MPCC_riders_WT_ProTeams%>%
  inner_join(Rider_results_2000_to_2022_dtb_clean,by="rider_name")%>%
  .$rider_name%>%
  unique()%>%
  length() #187 riders merged

#Riders for which different names
MPCC_riders_WT_ProTeams%>%
  anti_join(Rider_results_2000_to_2022_dtb_clean,by="rider_name")%>%
  .$rider_name%>%
  unique()%>%
  length() #42 riders not merged

#List
Riders_to_investigate <- MPCC_riders_WT_ProTeams%>%
  anti_join(Rider_results_2000_to_2022_dtb_clean,by="rider_name")%>%
  .$rider_name%>%
  unique()

#1. Cherel
rider_name_fix <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"CHEREL")==TRUE)%>%
  .$rider_name%>%
  unique()

#2. Cosnefroy
temp<- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"COSNEFROY")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#3. Hanninen
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"HÄNNINEN")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#4. Paret Peintre
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"PARET")==TRUE & str_detect(rider_name,"Aurélien")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#5. Schar
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"SCHÄR Michael")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#6. Warbasse
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"WARBASSE")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#7. Sutterlin
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"SÜTTERLIN")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#8. Grosschartner 
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"GROßSCHARTNER")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#9. Jesus Herrada
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"HERRADA Jes")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#10. Jose Herrada
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"HERRADA Jos")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#11.Périchon
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"Pierre-Luc")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#12. Rochas
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"ROCHAS")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#13 Valgren
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"VALGREN")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#14 Badilatti
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"BADILATTI")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#15 Biniam Girmay
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"Biniam")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#16 Hvideberg
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"HVIDEBERG")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#17 A. Kragh Andersen
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"KRAGH ANDERSEN Asb")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#18 Soren Kragh Andersen
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"KRAGH ANDERSEN S")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#19 Markl
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"MÄRKL")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#20 Casper Pedersen
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"PEDERSEN C")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#21 Vermaerke
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"VERMAERKE")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#22 Rasmus TILLER
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"TILLER")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#23 Mc Nulty
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"MCNULTY")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#24 Diaz Gallego
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"AZ José Manuel")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#25 Grosu
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"GROSU")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#26 Munoz Giraldo
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"OZ Daniel")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#27 Gesbert
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"GESBERT")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#28 Aparicio Munoz
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"APARICIO")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#29 CABEDO CARDA
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"CABEDO Ós")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#30 Ezquerra Muela
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"EZQUERRA")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#31 Madrazo
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"MADRAZO")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#32 Osorio
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"OSORIO J")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#33 PENALVER
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"ALVER Man")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#34 Barcelo
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"BARCEL")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#35 Delio Fernandez
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"Delio")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#36 HAILEMICHAEL
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"HAILE")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#37 Bou Company
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"BOU ")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#38 Mate Mardones
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"Luis Ángel")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#39 COTE
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"Pier-André")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#40 Kopecky
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"Matyáš")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#41 Lozano Riba
temp <- Rider_results_2000_to_2022_dtb_clean%>%
  filter(str_detect(rider_name,"LOZANO Dav")==TRUE)%>%
  .$rider_name%>%
  unique()
rider_name_fix <-  rider_name_fix%>%
  append(temp)

#42 Kulset Magnus
#Stil in the UNo-X Dev TEam (Only present in Tour of the Alps 2022)


#Check all 41 corrections could be merged
tibble(rider_name=rider_name_fix)%>%
  inner_join(Rider_results_2000_to_2022_dtb_clean,by="rider_name")%>%
  .$rider_name%>%
  unique()%>%
  length() #OK

MPCC_riders_WT_ProTeams_fix <- tibble(rider_name=rider_name_fix)%>%
  mutate(rider_MPCC_2022list_WTPro_fix=1)



##############Continental Tour

###Only those which could be merged directly

MPCC_riders_Continental <- MPCC_rider_dtb_Nov13_2022%>%
  rename(rider_name=rider_MPCC)%>%
  filter(str_detect(division,"UCI\\sCONTINENTAL\\sTEAM")==TRUE)%>%
  select(rider_name)%>%
  mutate(rider_MPCC_2022list_cont=1)
nrow(MPCC_riders_Continental) #73

#Check merge for all riders
MPCC_riders_Continental%>%
  inner_join(Rider_results_2000_to_2022_dtb_clean,by="rider_name")%>%
  .$rider_name%>%
  unique()%>%
  length() #16 extra riders merged

MPCC_riders_Continental%>%
  anti_join(Rider_results_2000_to_2022_dtb_clean,by="rider_name")%>%
  .$rider_name%>%
  unique()%>%
  length() #57 not in Pro Tour at some point or typo errors


##########Merge

Rider_results_2000_to_2022_dtb_clean <- Rider_results_2000_to_2022_dtb_clean%>%
  left_join(MPCC_riders_WT_ProTeams,by="rider_name")%>%
  left_join(MPCC_riders_WT_ProTeams_fix,by="rider_name")%>%
  left_join(MPCC_riders_Continental,by="rider_name")
  



##################################################
#                                                #
#    2.3.  Encode MPCC former rider adhesions    #
#                                                #
##################################################

####Assumption: Treat rider adhesion as time-invariant since no adhesion date is available
#Note: Includes former women riders

load(file=file.path("rda","Former_rider_MPCC_dtb_Nov13_2022"))

nrow(Former_rider_MPCC_dtb_Nov13_2022) #193

Former_rider_MPCC_dtb_Nov13_2022%>%
  inner_join(Rider_results_2000_to_2022_dtb_clean,by="rider_name")%>%
  .$rider_name%>%
  unique()%>%
  length() #118 extra riders merged

Former_rider_MPCC_dtb_Nov13_2022%>%
  inner_join(Rider_results_2000_to_2022_dtb_clean,by="rider_name")%>%
  filter(year>=2018)%>%
  .$rider_name%>%
  unique()%>%
  length() #114 extra riders merged (rode in 2018-2022)


Former_rider_MPCC_dtb_Nov13_2022 <- Former_rider_MPCC_dtb_Nov13_2022%>%
  mutate(rider_MPCC_2022list_former=1)%>%
  rename(rider_nation_former=rider_nation)

Rider_results_2000_to_2022_dtb_clean <- Rider_results_2000_to_2022_dtb_clean%>%
  left_join(Former_rider_MPCC_dtb_Nov13_2022,by="rider_name")


##################################################
#                                                #
#              2.4.  Other wrangling             #
#                                                #
##################################################

#Set non rider commitment before 2018
Rider_results_2000_to_2022_dtb_clean2 <- Rider_results_2000_to_2022_dtb_clean%>%
  mutate(rider_MPCC_2022_list=if_else((rider_MPCC_2022list_WTPro==1 | rider_MPCC_2022list_WTPro_fix==1 | rider_MPCC_2022list_cont==1 | rider_MPCC_2022list_former==1),1,0))%>%
  mutate(rider_MPCC_2022_list=if_else(rider_MPCC_2022_list==1 & year<2018,0,rider_MPCC_2022_list))%>%
  mutate(rider_MPCC_2022_list=if_else(is.na(rider_MPCC_2022_list),0,rider_MPCC_2022_list))


save(Rider_results_2000_to_2022_dtb_clean2,file=file.path("rda","Rider_results_2000_to_2022_dtb_clean2"))
#Stata format
write_dta(Rider_results_2000_to_2022_dtb_clean2, file.path("dta","Rider_results_2000_to_2022_dtb_clean2.dta"))

load(file=file.path("rda","Rider_results_2000_to_2022_dtb_clean2"))

###############

#Rider MPCC
Rider_results_2000_to_2022_dtb_clean2%>%
  filter(year>=2018 & year<2022)%>%
  .$rider_MPCC_2022_list%>%
  table()%>%prop.table()


#Team MPCC
Rider_results_2000_to_2022_dtb_clean2%>%
  filter(year>=2008  & year<2022)%>%
  .$team_MPCC%>%
  table()%>%prop.table()
