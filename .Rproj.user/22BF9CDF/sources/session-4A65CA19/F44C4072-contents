#######################################################
#                                                     #
#                 MPCC project                        #
#                       -                             #
#              3. Preliminary analysis                #
#                                                     #
#######################################################

#############General infos################
##Author: Clément ANNE
##Author e-mail: clement.anne90@gmail.com
##Date: May 15,2022
##########################################

################Outline###################
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
if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")


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
#              3.1. Preparation                  #
#                                                #
##################################################

load(file=file.path("rda","Rider_results_2000_to_2022_dtb_clean2"))


variables_to_scale_tot <- Rider_results_2000_to_2022_dtb_clean2%>%
  variable.names()%>%
  .[str_detect(.,"^(N_|PCS_)")==TRUE & str_detect(.,"_tot$")==TRUE]


Rider_results_2000_to_2022_dtb_clean2 <- Rider_results_2000_to_2022_dtb_clean2%>%
  arrange(rider,year)%>%
  group_by(rider)%>%
  #Scaled vars
  mutate(PCS_tot_perDay=PCS_tot/raced_days_tot,
         N_wins_tot_perDay=N_wins_tot/raced_days_tot,
         N_top_3_tot_perDay=N_top_3_tot/raced_days_tot,
         N_top_10_tot_perDay=N_top_10_tot/raced_days_tot)%>%
  #Lagged scaled vars
  mutate(l_PCS_tot_perDay=lag(PCS_tot_perDay),
         l_N_wins_tot_perDay=lag(N_wins_tot_perDay),
         l_N_top_3_tot_perDay=lag(N_top_3_tot_perDay),
         l_N_top_10_tot_perDay=lag(N_top_10_tot_perDay))%>%
  #Other PCS scaled and lagged
  mutate(PCS_GC_perDay=PCS_GC/raced_days_GC,
         PCS_Stages_perDay=PCS_Stages/raced_days_Stages,
         PCS_1day_perDay=PCS_1day/raced_days_1day,
         PCS_TT_perDay=PCS_TT/raced_days_TT,
         l_PCS_GC_perDay=lag(PCS_GC_perDay),
         l_PCS_Stages_perDay=lag(PCS_Stages_perDay),
         l_PCS_1day_perDay=lag(PCS_1day_perDay),
         l_PCS_TT_perDay=lag(PCS_TT_perDay))%>%
  #Lagged raced days
  mutate(l_raced_days_tot=lag(raced_days_tot))%>%
  #MPCC var       
  mutate(l_team_MPCC=lag(team_MPCC))%>%
  ungroup()


#780 obs
MPCC_new <- Rider_results_2000_to_2022_dtb_clean2%>%
  filter(year>=2008 & year<2022)%>%
  filter(raced_days_tot>=30 & l_raced_days_tot>=30)%>%
  filter(l_team_MPCC==0 & team_MPCC==1)

#330 obs  
MPCC_nomore <-Rider_results_2000_to_2022_dtb_clean2%>%
  filter(year>=2008 & year<2022)%>%
  filter(raced_days_tot>=30 & l_raced_days_tot>=30)%>%
  filter(l_team_MPCC==1 & team_MPCC==0) 

#####MPCC  new analysis


MPCC_new%>%
  mutate(PCS_gap=PCS_tot_perDay-l_PCS_tot_perDay)%>%
  select(PCS_gap,year,rider,PCS_tot_perDay,l_PCS_tot_perDay)%>%
  group_by(year)%>%
  summarize(n=n(),
            av_PCS_gap=mean(PCS_gap))

MPCC_new%>%
  mutate(PCS_gap=PCS_tot_perDay-l_PCS_tot_perDay)%>%
  select(PCS_gap,year,rider,PCS_tot_perDay,l_PCS_tot_perDay)%>%
  group_by(year)%>%
  ggplot(aes(year,PCS_gap,group=year))+
  geom_boxplot()

#####MPCC  nomore analysis


MPCC_nomore%>%
  mutate(PCS_gap=PCS_tot_perDay-l_PCS_tot_perDay)%>%
  select(PCS_gap,year,rider,PCS_tot_perDay,l_PCS_tot_perDay)%>%
  group_by(year)%>%
  summarize(n=n(),
            av_PCS_gap=mean(PCS_gap))

MPCC_nomore%>%
  mutate(PCS_gap=PCS_tot_perDay-l_PCS_tot_perDay)%>%
  select(PCS_gap,year,rider,PCS_tot_perDay,l_PCS_tot_perDay)%>%
  group_by(year)%>%
  ggplot(aes(year,PCS_gap,group=year))+
  geom_boxplot()

#####Average stats

Rider_results_2000_to_2022_dtb_clean2%>%
  summarize_at(c("PCS_tot_perDay","PCS_GC_perDay","PCS_Stages_perDay","PCS_1day_perDay","PCS_TT_perDay"),
               mean,na.rm=TRUE)%>%View()

##################################################
#                                                #
#              3.2. Key stats                    #
#                                                #
##################################################

##################################################
#          3.2.1. Raced days                     #
##################################################

######Density raced days per season

Rider_results_2000_to_2022_dtb_clean2%>%
filter(!is.na(raced_days_tot))%>%
  ggplot(aes(raced_days_tot,col=division))+
  geom_density()

Rider_results_2000_to_2022_dtb_clean2%>%
  group_by(division)%>%
  filter(!is.na(raced_days_tot))%>%
  summarize(n=n(),
            mean=mean(raced_days_tot),
            sd=sd(raced_days_tot),
            ci_inf=mean+qnorm(0.025)*sd,
            ci_sup=mean+qnorm(0.975)*sd)

Rider_results_2000_to_2022_dtb_clean2%>%
  filter(!is.na(raced_days_tot))%>%
  filter(year>2007 & year<2022)%>%
  ggplot(aes(year,raced_days_tot,group=factor(year)))+
  geom_boxplot()+
  facet_grid(division~.)

#WT
Rider_results_2000_to_2022_dtb_clean2%>%
  filter(!is.na(raced_days_tot))%>%
  filter(year>2007 & year<2022)%>%
  filter(division=="World Tour")%>%
  ggplot(aes(year,raced_days_tot,group=factor(year)))+
  geom_boxplot()
#Pro Tour
Rider_results_2000_to_2022_dtb_clean2%>%
  filter(!is.na(raced_days_tot))%>%
  filter(year>2007 & year<2022)%>%
  filter(division=="Pro Tour")%>%
  ggplot(aes(year,raced_days_tot,group=factor(year)))+
  geom_boxplot()


Rider_results_2000_to_2022_dtb_clean2%>%
  filter(raced_days_tot>=30)
  mutate(PCS_tot_perDay)

##################################################
#                 3.2.2. PCS                     #
##################################################  

Rider_results_2000_to_2022_dtb_clean2%>%
    filter(!is.na(PCS_tot))%>%
    ggplot(aes(PCS_tot,col=division))+
    geom_density()+
    scale_x_log10()
  
Rider_results_2000_to_2022_dtb_clean2%>%
    filter(!is.na(PCS_tot))%>%
    filter(raced_days_tot>=30)%>%
    ggplot(aes(PCS_tot,col=division))+
    geom_density()+
    scale_x_log10()
    
  
##################################################
#          3.2.3. Scaled PCS                     #
##################################################  

Rider_results_2000_to_2022_dtb_clean2%>%
  filter(!is.na(PCS_tot_perDay))%>%
  mutate(PCS_tot_perDay=PCS_tot_perDay)%>%
  ggplot(aes(PCS_tot_perDay,col=division))+
  geom_density()+
  scale_x_continuous(trans="log10")

Rider_results_2000_to_2022_dtb_clean2%>%
  filter(!is.na(PCS_tot_perDay))%>%
  filter(raced_days_tot>=30)%>%
  ggplot(aes(PCS_tot_perDay,col=division))+
  geom_density()+
  scale_x_log10()

           
  
  Rider_results_2000_to_2022_dtb_clean2%>%
    group_by(division)%>%
    filter(!is.na(PCS_tot_perDay))%>%
    summarize(n=n(),
              mean=mean(PCS_tot_perDay),
              sd=sd(PCS_tot_perDay),
              ci_inf=mean+qnorm(0.025)*sd,
              ci_sup=mean+qnorm(0.975)*sd)
 
  #Min 30 raced days
  Rider_results_2000_to_2022_dtb_clean2%>%
    group_by(division)%>%
    filter(!is.na(PCS_tot_perDay))%>%
    filter(raced_days_tot>=30)%>%
    summarize(n=n(),
              mean=mean(PCS_tot_perDay),
              sd=sd(PCS_tot_perDay),
              ci_inf=mean+qnorm(0.025)*sd,
              ci_sup=mean+qnorm(0.975)*sd)

  
  #Min 60 raced days
  Rider_results_2000_to_2022_dtb_clean2%>%
    group_by(division)%>%
    filter(!is.na(PCS_tot_perDay))%>%
    filter(raced_days_tot>=60)%>%
    summarize(n=n(),
              mean=mean(PCS_tot_perDay),
              sd=sd(PCS_tot_perDay),
              ci_inf=mean+qnorm(0.025)*sd,
              ci_sup=mean+qnorm(0.975)*sd)  
 
  #WT
  Rider_results_2000_to_2022_dtb_clean2%>%
    filter(!is.na(raced_days_tot))%>%
    filter(year>2007 & year<2022)%>%
    filter(division=="World Tour")%>%
    ggplot(aes(year,PCS_tot_perDay,group=factor(year)))+
    geom_boxplot()+
    scale_y_sqrt()

  
  
  ##################################################
  #          3.2.4. Age evolution                  #
  ##################################################  
  
  #Points scored per race evolution
  Rider_results_2000_to_2022_dtb_clean2%>%
    filter(!is.na(rider_age) & !is.na(PCS_1day_perDay))%>%
    filter(year>2011 & year<2022)%>%
    filter(division=="World Tour")%>%
    group_by(rider_age)%>%
    mutate(n=n())%>%
    filter(n>=100)%>%
    summarize(med_PCS_tot_perDay=median(PCS_tot_perDay))%>%
    ggplot(aes(rider_age,med_PCS_tot_perDay))+
    geom_point()+
    geom_smooth()+
    scale_y_log10()

  #Raced days evolution
  Rider_results_2000_to_2022_dtb_clean2%>%
    filter(!is.na(rider_age) & !is.na(raced_days_tot))%>%
    filter(year>2011 & year<2022)%>%
    filter(division=="World Tour")%>%
    group_by(rider_age)%>%
    mutate(n=n())%>%
    filter(n>=100)%>%
    summarize(med_raced_days_tot=median(raced_days_tot))%>%
    ggplot(aes(rider_age,med_raced_days_tot))+
    geom_point()
  
  #N riders in the WT
  Rider_results_2000_to_2022_dtb_clean2%>%
    filter(!is.na(rider_age))%>%
    filter(year>2011 & year<2022)%>%
    filter(division=="World Tour")%>%
    group_by(rider_age, year)%>%
    mutate(n=n())%>%
    ungroup()%>%
    ggplot(aes(rider_age,n,group=factor(rider_age)))+
    geom_point()  
  
  ##################################################
  #          3.2.5. Differences in subcats         #
  ##################################################  
  
  