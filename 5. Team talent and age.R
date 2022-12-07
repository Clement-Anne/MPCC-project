#######################################################
#                                                     #
#                 MPCC project                        #
#                       -                             #
#              5. Team talent and age                 #
#                                                     #
#######################################################

#############General infos################
##Author: Clément ANNE
##Author e-mail: clement.anne90@gmail.com
##Date: Dec 07,2022
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
if(!require(plm)) install.packages("plm", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")


library(rvest)
library(stringr)
library(tidyverse)
library(lubridate)
library(caret)
library(knitr)
library(tinytex)
library(foreign)
library(haven)
library(plm)
library(data.table)
library(forcats)

###Install tinytex if needed
if(is_tinytex()==FALSE) tinytex::install_tinytex()


##################################################
#                                                #
#              5.1. Preparation                  #
#                                                #
##################################################

load(file=file.path("rda","Rider_results_2000_to_2022_dtb_clean2"))

load(file=file.path("rda","Rider_results_2000_to_2022_dtb_clean2"))

t <- Rider_results_2000_to_2022_dtb_clean2 


t <- mutate(t,MPCC=ifelse(rider_MPCC_2022_list==1 | team_MPCC==1,1,0))
table(t$year,t$rider_MPCC_2022_list)
table(t$year,t$team_MPCC)
table(t$year,t$MPCC)

t <- t %>% as.data.table()
t <- t[!duplicated(t),] #M. Banhamouda duplicates obs (Novo Nordisk)
t[,N_id_year:=.N,by=c("rider_name","year")]

t[,table(N_id_year)] #Drop riders changing teams witin year (15 twice, 1 three times)


riders_transferts <- t[N_id_year>1,.(rider_name,year,team_name,PCS_tot)] %>% 
  arrange(desc(PCS_tot))


t <- t %>% 
  filter(N_id_year==1) %>%
  pdata.frame(index=c("rider_name","year"))

t$raced_days_tot_l <- plm::lag(t$raced_days_tot,k=1)
t$raced_days_tot_l2 <- plm::lag(t$raced_days_tot,k=2)
t$raced_days_tot_l3 <- plm::lag(t$raced_days_tot,k=3)
t$raced_days_tot_sum <- t$raced_days_tot_l+t$raced_days_tot_l2+t$raced_days_tot_l3 
t$PCS_tot_l <- plm::lag(t$PCS_tot,k=1)
t$PCS_tot_l2 <- plm::lag(t$PCS_tot,k=2)
t$PCS_tot_l3 <- plm::lag(t$PCS_tot,k=3)
t$PCS_tot_sum <- t$PCS_tot_l+t$PCS_tot_l2+t$PCS_tot_l3 


png(file=file.path("Outputs","Fig6a_overachieve_TeamMPCC.png"))


t %>% 
  as_tibble() %>% 
  mutate(MPCC=ifelse(rider_MPCC_2022_list==1 | team_MPCC==1,1,0)) %>% 
  filter(year %in% c("2018","2019","2020","2021","2022")) %>%
  mutate(PCS_tot_sh_pred=PCS_tot_sum/raced_days_tot_sum,
         PCS_tot_sh=PCS_tot/raced_days_tot,
         ovearachieve=PCS_tot_sh/PCS_tot_sh_pred) %>% 
  group_by(year,team_MPCC,division) %>% 
  summarize(PCS_tot_sh_pred_av=mean(PCS_tot_sh_pred,na.rm=T),
            PCS_tot_sh_av=mean(PCS_tot_sh,na.rm=T),
            overachivement=PCS_tot_sh_av/PCS_tot_sh_pred_av) %>%
  mutate(year=1999+as.numeric(year)) %>% 
  ggplot(aes(year,overachivement,col=factor(team_MPCC)))+
  geom_point()+
  geom_line()+
  facet_wrap(.~division)+
  theme_light()+
  labs(x="Year",y="Overachivement",col="Team MPCC",caption="Overachievement is the ratio between the average PCS/raced day \n and the average predicted PCS/raced days \n over the rider's past 3 seasons")

dev.off()

png(file=file.path("Outputs","Fig6b_PCS_TeamMPCC.png"))


t %>% 
  as.data.frame(keep.attributes = FALSE) %>% 
  as_tibble() %>% 
  mutate(MPCC=ifelse(rider_MPCC_2022_list==1 | team_MPCC==1,1,0)) %>% 
  filter(year %in% c("2018","2019","2020","2021","2022")) %>% 
  mutate(PCS_tot_sh_pred=PCS_tot_sum/raced_days_tot_sum,
         PCS_tot_sh=PCS_tot/raced_days_tot,
         ovearachieve=PCS_tot_sh/PCS_tot_sh_pred) %>% 
  mutate(team_MPCC=factor(team_MPCC,labels=c("Team not MPCC","Team MPCC"),ordered=TRUE)) %>% 
  group_by(year,team_MPCC,division) %>% 
  summarize(PCS_tot_sh_pred_av=mean(PCS_tot_sh_pred,na.rm=T),
            PCS_tot_sh_av=mean(PCS_tot_sh,na.rm=T),
            overachivement=PCS_tot_sh_av/PCS_tot_sh_pred_av) %>%
  mutate(year=1999+as.numeric(year)) %>%
  ggplot(aes(year,PCS_tot_sh_pred_av))+
  geom_point(aes(year,PCS_tot_sh_av))+
  geom_line(aes(year,PCS_tot_sh_av))+
  geom_point(aes(year,PCS_tot_sh_pred_av),col="grey")+
  geom_line(aes(year,PCS_tot_sh_pred_av),lty=2)+
  facet_grid(team_MPCC~division)+
  theme_light()+
  labs(x="Year",y="Average(PCS points/ N raced days)",caption="Doted line corresponds to the predicted outcomes \n based on the rider average over its past 3 seasons provided he rode in all of them") 
dev.off()


png(file=file.path("Outputs","Fig6c_overachieve_riderMPCC.png"))

t %>% 
  as_tibble() %>% 
  mutate(MPCC=ifelse(rider_MPCC_2022_list==1 | team_MPCC==1,1,0)) %>% 
  filter(year %in% c("2018","2019","2020","2021","2022")) %>% 
  mutate(PCS_tot_sh_pred=PCS_tot_sum/raced_days_tot_sum,
         PCS_tot_sh=PCS_tot/raced_days_tot,
         ovearachieve=PCS_tot_sh/PCS_tot_sh_pred) %>%
  mutate(rider_MPCC_2022_list=factor(rider_MPCC_2022_list,labels=c("Rider not MPCC","Rider MPCC"),ordered=TRUE)) %>%
  group_by(year,rider_MPCC_2022_list,division) %>% 
  summarize(PCS_tot_sh_pred_av=mean(PCS_tot_sh_pred,na.rm=T),
            PCS_tot_sh_av=mean(PCS_tot_sh,na.rm=T),
            overachivement=PCS_tot_sh_av/PCS_tot_sh_pred_av) %>%
  mutate(year=1999+as.numeric(year)) %>% 
  ggplot(aes(year,overachivement,col=factor(rider_MPCC_2022_list)))+
  geom_point()+
  geom_line()+
  facet_wrap(.~division)+
  theme_light()+
  labs(x="Year",y="Overachivement",col="Rider MPCC",caption="Overachievement is the ratio between the average PCS/raced day \n and the average predicted PCS/raced days \n over the rider's past 3 seasons")
dev.off()

png(file=file.path("Outputs","Fig6d_PCS_riderMPCC.png"))

t %>% 
  as.data.frame(keep.attributes = FALSE) %>% 
  as_tibble() %>% 
  mutate(MPCC=ifelse(rider_MPCC_2022_list==1 | team_MPCC==1,1,0)) %>% 
  filter(year %in% c("2018","2019","2020","2021","2022")) %>% 
  mutate(PCS_tot_sh_pred=PCS_tot_sum/raced_days_tot_sum,
         PCS_tot_sh=PCS_tot/raced_days_tot,
         ovearachieve=PCS_tot_sh/PCS_tot_sh_pred) %>% 
  mutate(rider_MPCC_2022_list=factor(rider_MPCC_2022_list,labels=c("Rider not MPCC","Rider MPCC"),ordered=TRUE)) %>% 
  group_by(year,rider_MPCC_2022_list,division) %>% 
  summarize(PCS_tot_sh_pred_av=mean(PCS_tot_sh_pred,na.rm=T),
            PCS_tot_sh_av=mean(PCS_tot_sh,na.rm=T),
            overachivement=PCS_tot_sh_av/PCS_tot_sh_pred_av) %>%
  mutate(year=1999+as.numeric(year)) %>%
  ggplot(aes(year,PCS_tot_sh_pred_av))+
  geom_point(aes(year,PCS_tot_sh_av))+
  geom_line(aes(year,PCS_tot_sh_av))+
  geom_point(aes(year,PCS_tot_sh_pred_av),col="grey")+
  geom_line(aes(year,PCS_tot_sh_pred_av),lty=2)+
  facet_grid(rider_MPCC_2022_list~division)+
  theme_light()+
  labs(x="Year",y="Average(PCS points/ N raced days)",caption="Doted line corresponds to the predicted outcomes \n based on the rider average over its past 3 seasons provided he rode in all of them") 
dev.off()
