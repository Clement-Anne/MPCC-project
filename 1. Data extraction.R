#######################################################
#                                                     #
#                 MPCC project                        #
#                       -                             #
#               1. Data extraction                    #
#                                                     #
#######################################################

#############General infos################
##Author: Clément ANNE
##Author e-mail: clement.anne90@gmail.com
##Date: May 12,2022
##########################################

################Outline###################
##
##  1.1. Team list 2007-2022   
##  1.2. Rider-team list 2007-2022
##  1.3. Unique rider list
##  1.4. Rider results
##  
##
##########################################

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

library(rvest)
library(stringr)
library(tidyverse)
library(lubridate)
library(caret)
library(knitr)
library(tinytex)

###Install tinytex if needed
if(is_tinytex()==FALSE) tinytex::install_tinytex()

##################################################
#                                                #
#         1.1. Team list 2000-2022               #
#                                                #
##################################################

#Extract function
my_extract_WT_teams <- function(year){
  #Add year to the URL
  url <- paste0("https://www.procyclingstats.com/teams.php?year=",as.character(year),"&filter=Filter")

  h <- read_html(url)
  
  Team_list_WT <- h%>%
    html_elements(".mt20")%>%
    .[[1]]%>%
    html_elements("a")%>%
    html_attr("href")
  
  Team_list_ProTour <- h%>%
    html_elements(".mt20")%>%
    .[[3]]%>%
    html_elements("a")%>%
    html_attr("href")
  
  WT_teams <- tibble(year=year,
         team=Team_list_WT,
         division="World Tour")
  ProTour_teams <- tibble(year=year,
         team=Team_list_ProTour,
         division="Pro Tour")
  
  WT_teams%>%
    bind_rows(ProTour_teams)
  
}

#Example
my_extract_WT_teams(2022)

#Team list merging
Team_list_2000_to_2022 <- map_dfr(2000:2022,my_extract_WT_teams)%>%
  mutate(team=str_remove(team,"team/"))

#635 teams
nrow(Team_list_2000_to_2022)

##################################################
#                                                #
#         1.2. Rider-team list 2000-2022         #
#                                                #
##################################################

#Extract function
my_extract_rider_list <- function(team){
  url <- paste0("https://www.procyclingstats.com/team/",team)
  
  h <- read_html(url)
  
  riders <- h%>%
    html_elements(".list.pad2")%>%
    .[[1]]%>%
    html_elements("a")%>%
    html_attr("href")
  
  tibble(rider=riders,
         team=team)
}

#Example
my_extract_rider_list("ag2r-prevoyance-2007")

#Rider list
Rider_list_2000_to_2022 <- map_dfr(Team_list_2000_to_2022$team,my_extract_rider_list)%>%
  mutate(rider=str_remove(rider,"rider/"))%>%
  mutate(year=str_extract(team,"\\d+$")%>%
           #Need to remove the last digit in case of double page for a team (e.g., Movistar 2015)
           str_extract("^\\d{4}")%>%
           as.numeric())


##################################################
#                                                #
#             1.3. Unique rider list             #
#                                                #
##################################################

#3585 unique riders
Rider_list <- unique(Rider_list_2000_to_2022$rider)

Rider_list%>%
  length()

##################################################
#                                                #
#             1.4. Rider results                 #
#                                                #
##################################################

#Extract function
my_extract_rider_results <- function(rider){
  
  url <- paste0("https://www.procyclingstats.com/rider.php?proresults=0&proresults=1&pproresults=largerorequal&stage_type=&filter=Filter&id=",rider,"&p=statistics&s=season-statistics")
  
  h <- read_html(url)
  
  nodes <- h%>%
    html_elements("table")
  
  html_table(nodes[[1]])%>%
    setNames(c("year","PCS","raced_days", "raced_km", "N_wins", "N_top_3", "N_top_10"))%>%
    mutate_all(str_replace,"-","0")%>%
    mutate_all(as.numeric)%>%
    mutate(rider=rider)%>%
    slice(1:nrow(.)-1)
  
}

#Example
my_extract_rider_results("wout-van-aert")
 
#Dtb rider results
Rider_results_2000_to_2022_dtb <- map_dfr(Rider_list,my_extract_rider_results)

##################################################
#                                                #
#             1.5. Merge with team               #
#                                                #
##################################################

Rider_results_2000_to_2022_dtb_clean <- Rider_results_2000_to_2022_dtb%>%
  right_join(Rider_list_2000_to_2022,by=c("year","rider"))

save(Rider_results_2000_to_2022_dtb_clean,file=file.path("rda","Rider_results_2000_to_2022_dtb_clean"))
