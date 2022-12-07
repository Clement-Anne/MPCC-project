#######################################################
#                                                     #
#                 MPCC project                        #
#                       -                             #
#              4. Analysis Dec 2022                   #
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
#              4.1. Preparation                  #
#                                                #
##################################################

load(file=file.path("rda","Rider_results_2000_to_2022_dtb_clean2"))

t <- Rider_results_2000_to_2022_dtb_clean2 %>% 
  filter(year>=2018)

#
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
   filter(N_id_year==1)


# t <- t %>% 
#   filter(N_id_year==1) %>% 
#   pdata.frame(index=c("rider_name","year"))

#####Riders change

summary(t)


png(file=file.path("Outputs","Fig1a_Top3_TeamMPCC.png"))

#Average top 3 by year
##Team
t[,mean(N_top_3_tot/raced_days_tot,na.rm=TRUE),by=c("year","team_MPCC","division")] %>% 
  ggplot(aes(year,V1,col=factor(team_MPCC)))+
  geom_point()+
  geom_line()+
  labs(x="Year",y="Average(N top 3/ N raced days)",col="Team MPCC")+
  facet_wrap(.~division)+
  theme_light()

dev.off()


png(file=file.path("Outputs","Fig1b_Top3_RiderMPCC.png"))

##Rider
t[,mean(N_top_3_tot/raced_days_tot,na.rm=TRUE),by=c("year","rider_MPCC_2022_list","division")] %>% 
  ggplot(aes(year,V1,col=factor(rider_MPCC_2022_list)))+
  geom_point()+
  geom_line()+
  labs(x="Year",y="Average(N top 3/ N raced days)",col="Rider MPCC")+
  facet_wrap(.~division)+
  theme_light()
dev.off()

png(file=file.path("Outputs","Fig1c_PCS_TeamMPCC.png"))

#Average PCS points by year
##Team
t[,mean(PCS_tot/raced_days_tot,na.rm=TRUE),by=c("year","team_MPCC","division")] %>% 
  ggplot(aes(year,V1,col=factor(team_MPCC)))+
  geom_point()+
  geom_line()+
  labs(x="Year",y="Average(PCS points/ N raced days)",col="Team MPCC")+
  facet_wrap(.~division)+
  theme_light()
dev.off()

png(file=file.path("Outputs","Fig1d_PCS_RiderMPCC.png"))

##Rider
t[,mean(PCS_tot/raced_days_tot,na.rm=TRUE),by=c("year","rider_MPCC_2022_list","division")] %>% 
  ggplot(aes(year,V1,col=factor(rider_MPCC_2022_list)))+
  geom_point()+
  geom_line()+
  labs(x="Year",y="Average(PCS points/ N raced days)",col="Rider MPCC")+
  facet_wrap(.~division)+
  theme_light()
dev.off()

png(file=file.path("Outputs","Fig2a_PCS_WT_teamMPCC.png"))


t[division=="World Tour",mean(PCS_tot/raced_days_tot,na.rm=TRUE),by=c("year","rider_MPCC_2022_list","team_MPCC")] %>%
  mutate(rider_MPCC_2022_list=factor(rider_MPCC_2022_list,labels=c("Riders not MPCC","Riders MPCC"),ordered=T),
         team_MPCC=factor(team_MPCC,labels=c("Team not MPCC","Team MPCC"),ordered=TRUE)) %>% 
  ggplot(aes(year,V1))+
  geom_point()+
  geom_line()+
  labs(x="Year",y="Average(PCS points/ N raced days)",caption="World Tour riders only")+
  facet_grid(rider_MPCC_2022_list~team_MPCC)+
  theme_light()
dev.off()

png(file=file.path("Outputs","Fig2b_top3_WT_teamMPCC.png"))


t[division=="World Tour",mean(N_top_3_tot/raced_days_tot,na.rm=TRUE),by=c("year","rider_MPCC_2022_list","team_MPCC")] %>% 
  mutate(rider_MPCC_2022_list=factor(rider_MPCC_2022_list,labels=c("Riders not MPCC","Riders MPCC"),ordered=T),
         team_MPCC=factor(team_MPCC,labels=c("Team not MPCC","Team MPCC"),ordered=TRUE)) %>% 
  ggplot(aes(year,V1))+
  geom_point()+
  geom_line()+
  labs(x="Year",y="Average(N top 3/ N raced days)",caption="World Tour riders only")+
  facet_grid(rider_MPCC_2022_list~team_MPCC)+
  theme_light()
dev.off()

png(file=file.path("Outputs","Fig3a_PCS_split_teamMPCC.png"))


#PCS points split by category
t %>% 
  mutate(PCS_GC_sh=PCS_GC/raced_days_GC,
         PCS_Stages_sh=PCS_Stages/raced_days_Stages,
         PCS_1day_sh=PCS_1day/raced_days_1day,
         PCS_TT_sh=PCS_TT/raced_days_TT) %>% 
  pivot_longer(c("PCS_GC_sh","PCS_Stages_sh","PCS_1day_sh","PCS_TT_sh"),names_to = "PCS_points_level",values_to="PCS_sh") %>%
  mutate(PCS_points_level=case_when(PCS_points_level=="PCS_GC_sh"~ "General classification",
                                    PCS_points_level=="PCS_Stages_sh"~ "Stages",
                                    PCS_points_level=="PCS_1day_sh"~ "1 day races",
                                    PCS_points_level=="PCS_TT_sh"~ "Time Trial")) %>% 
  group_by(year,team_MPCC,division,PCS_points_level) %>% 
  summarize(PCS_sh_av=mean(PCS_sh,na.rm=T)) %>% 
  mutate(PCS_points_level=fct_reorder(factor(PCS_points_level),PCS_sh_av,.fun=median,.desc=TRUE)) %>% 
  mutate(division=factor(division,levels=c("World Tour","Pro Tour"),ordered=T)) %>%  
  ggplot(aes(year,PCS_sh_av,col=factor(team_MPCC)))+
  geom_point()+
  geom_line()+
  labs(x="Year",y="Average(PCS points/ N raced days)",col="Team MPCC")+
  facet_grid(division~PCS_points_level)+
  theme_light()
dev.off()

png(file=file.path("Outputs","Fig3b_Top3_split_teamMPCC.png"))


#Top 3 split by category
t %>% 
  mutate(N_top_3_GC_sh=N_top_3_GC/raced_days_GC,
         N_top_3_Stages_sh=N_top_3_Stages/raced_days_Stages,
         N_top_3_1day_sh=N_top_3_1day/raced_days_1day,
         N_top_3_TT_sh=N_top_3_TT/raced_days_TT) %>% 
  pivot_longer(c("N_top_3_GC_sh","N_top_3_Stages_sh","N_top_3_1day_sh","N_top_3_TT_sh"),names_to = "N_top_3_level",values_to="N_top_3_sh") %>%
  mutate(N_top_3_level=case_when(N_top_3_level=="N_top_3_GC_sh"~ "General classification",
                                 N_top_3_level=="N_top_3_Stages_sh"~ "Stages",
                                 N_top_3_level=="N_top_3_1day_sh"~ "1 day races",
                                 N_top_3_level=="N_top_3_TT_sh"~ "Time Trial")) %>% 
  group_by(year,team_MPCC,division,N_top_3_level) %>% 
  summarize(N_top_3_sh_av=mean(N_top_3_sh,na.rm=T)) %>% 
  mutate(N_top_3_level=factor(N_top_3_level,levels=c("General classification","1 day races","Time Trial","Stages"),ordered=T)) %>% 
  mutate(division=factor(division,levels=c("World Tour","Pro Tour"),ordered=T)) %>%  
  ggplot(aes(year,N_top_3_sh_av,col=factor(team_MPCC)))+
  geom_point()+
  geom_line()+
  labs(x="Year",y="Average(N top 3/ N raced days)",col="Team MPCC")+
  facet_grid(division~N_top_3_level)+
  theme_light()
dev.off()

#####Top 10 within each team

png(file=file.path("Outputs","Fig4a_PCS_split_10riders_teamMPCC.png"))


t %>% 
  group_by(team) %>% 
  mutate(PCS_GC_rank=rank(-PCS_GC,ties.method= "random"),
         PCS_tot_rank=rank(-PCS_tot,ties.method= "random"),
         PCS_Stages_rank=rank(-PCS_Stages,ties.method= "random"),
         PCS_TT_rank=rank(-PCS_TT,ties.method= "random"),
         PCS_1day_rank=rank(-PCS_1day,ties.method= "random")) %>%
  mutate(PCS_GC=ifelse(PCS_GC_rank>10,NA,PCS_GC),
         PCS_tot=ifelse(PCS_tot_rank>10,NA,PCS_tot),
         PCS_Stages=ifelse(PCS_Stages_rank>10,NA,PCS_Stages),
         PCS_TT=ifelse(PCS_TT_rank>10,NA,PCS_TT),
         PCS_1day=ifelse(PCS_1day_rank>10,NA,PCS_1day),
         raced_days_tot=ifelse(PCS_tot_rank>10,NA,raced_days_tot),
         raced_days_GC=ifelse(PCS_GC_rank>10,NA,raced_days_GC),
         raced_days_Stages=ifelse(PCS_Stages_rank>10,NA,raced_days_Stages),
         raced_days_TT=ifelse(PCS_TT_rank>10,NA,raced_days_TT),
         raced_days_1day=ifelse(PCS_1day_rank>10,NA,raced_days_1day)) %>% 
  mutate(PCS_GC_sh=PCS_GC/raced_days_GC,
         PCS_Stages_sh=PCS_Stages/raced_days_Stages,
         PCS_1day_sh=PCS_1day/raced_days_1day,
         PCS_TT_sh=PCS_TT/raced_days_TT) %>% 
  pivot_longer(c("PCS_GC_sh","PCS_Stages_sh","PCS_1day_sh","PCS_TT_sh"),names_to = "PCS_points_level",values_to="PCS_sh") %>%
  mutate(PCS_points_level=case_when(PCS_points_level=="PCS_GC_sh"~ "General classification",
                                    PCS_points_level=="PCS_Stages_sh"~ "Stages",
                                    PCS_points_level=="PCS_1day_sh"~ "1 day races",
                                    PCS_points_level=="PCS_TT_sh"~ "Time Trial")) %>% 
  group_by(year,team_MPCC,division,PCS_points_level) %>% 
  summarize(PCS_sh_av=mean(PCS_sh,na.rm=T)) %>% 
  mutate(PCS_points_level=fct_reorder(factor(PCS_points_level),PCS_sh_av,.fun=median,.desc=TRUE)) %>% 
  mutate(division=factor(division,levels=c("World Tour","Pro Tour"),ordered=T)) %>%  
  ggplot(aes(year,PCS_sh_av,col=factor(team_MPCC)))+
  geom_point()+
  geom_line()+
  labs(x="Year",y="Average(PCS points/ N raced days)",col="Team MPCC",caption="Only top 10 riders per team in each category")+
  facet_grid(division~PCS_points_level)+
  theme_light()
dev.off()

png(file=file.path("Outputs","Fig4b_PCS_split_10riders_riderMPCC.png"))


t %>% 
  group_by(team) %>% 
  mutate(PCS_GC_rank=rank(-PCS_GC,ties.method= "random"),
         PCS_tot_rank=rank(-PCS_tot,ties.method= "random"),
         PCS_Stages_rank=rank(-PCS_Stages,ties.method= "random"),
         PCS_TT_rank=rank(-PCS_TT,ties.method= "random"),
         PCS_1day_rank=rank(-PCS_1day,ties.method= "random")) %>%
  mutate(PCS_GC=ifelse(PCS_GC_rank>10,NA,PCS_GC),
         PCS_tot=ifelse(PCS_tot_rank>10,NA,PCS_tot),
         PCS_Stages=ifelse(PCS_Stages_rank>10,NA,PCS_Stages),
         PCS_TT=ifelse(PCS_TT_rank>10,NA,PCS_TT),
         PCS_1day=ifelse(PCS_1day_rank>10,NA,PCS_1day),
         raced_days_tot=ifelse(PCS_tot_rank>10,NA,raced_days_tot),
         raced_days_GC=ifelse(PCS_GC_rank>10,NA,raced_days_GC),
         raced_days_Stages=ifelse(PCS_Stages_rank>10,NA,raced_days_Stages),
         raced_days_TT=ifelse(PCS_TT_rank>10,NA,raced_days_TT),
         raced_days_1day=ifelse(PCS_1day_rank>10,NA,raced_days_1day)) %>% 
  mutate(PCS_GC_sh=PCS_GC/raced_days_GC,
         PCS_Stages_sh=PCS_Stages/raced_days_Stages,
         PCS_1day_sh=PCS_1day/raced_days_1day,
         PCS_TT_sh=PCS_TT/raced_days_TT) %>% 
  pivot_longer(c("PCS_GC_sh","PCS_Stages_sh","PCS_1day_sh","PCS_TT_sh"),names_to = "PCS_points_level",values_to="PCS_sh") %>%
  mutate(PCS_points_level=case_when(PCS_points_level=="PCS_GC_sh"~ "General classification",
                                    PCS_points_level=="PCS_Stages_sh"~ "Stages",
                                    PCS_points_level=="PCS_1day_sh"~ "1 day races",
                                    PCS_points_level=="PCS_TT_sh"~ "Time Trial")) %>% 
  group_by(year,rider_MPCC_2022_list,division,PCS_points_level) %>% 
  summarize(PCS_sh_av=mean(PCS_sh,na.rm=T)) %>% 
  mutate(PCS_points_level=fct_reorder(factor(PCS_points_level),PCS_sh_av,.fun=median,.desc=TRUE)) %>% 
  mutate(division=factor(division,levels=c("World Tour","Pro Tour"),ordered=T)) %>%  
  ggplot(aes(year,PCS_sh_av,col=factor(rider_MPCC_2022_list)))+
  geom_point()+
  geom_line()+
  labs(x="Year",y="Average(PCS points/ N raced days)",col="Rider MPCC",caption="Only top 10 riders per team in each category")+
  facet_grid(division~PCS_points_level)+
  theme_light()

dev.off()

#############


png(file=file.path("Outputs","Fig5a_PCSdistr_split_10riders_teamMPCC.png"))


t %>% 
  group_by(team) %>% 
  mutate(PCS_GC_rank=rank(-PCS_GC,ties.method= "random"),
         PCS_tot_rank=rank(-PCS_tot,ties.method= "random"),
         PCS_Stages_rank=rank(-PCS_Stages,ties.method= "random"),
         PCS_TT_rank=rank(-PCS_TT,ties.method= "random"),
         PCS_1day_rank=rank(-PCS_1day,ties.method= "random")) %>%
  mutate(PCS_GC=ifelse(PCS_GC_rank>10,NA,PCS_GC),
         PCS_tot=ifelse(PCS_tot_rank>10,NA,PCS_tot),
         PCS_Stages=ifelse(PCS_Stages_rank>10,NA,PCS_Stages),
         PCS_TT=ifelse(PCS_TT_rank>10,NA,PCS_TT),
         PCS_1day=ifelse(PCS_1day_rank>10,NA,PCS_1day),
         raced_days_tot=ifelse(PCS_tot_rank>10,NA,raced_days_tot),
         raced_days_GC=ifelse(PCS_GC_rank>10,NA,raced_days_GC),
         raced_days_Stages=ifelse(PCS_Stages_rank>10,NA,raced_days_Stages),
         raced_days_TT=ifelse(PCS_TT_rank>10,NA,raced_days_TT),
         raced_days_1day=ifelse(PCS_1day_rank>10,NA,raced_days_1day)) %>% 
  mutate(PCS_GC_sh=PCS_GC/raced_days_GC,
         PCS_Stages_sh=PCS_Stages/raced_days_Stages,
         PCS_1day_sh=PCS_1day/raced_days_1day,
         PCS_TT_sh=PCS_TT/raced_days_TT) %>% 
  pivot_longer(c("PCS_GC_sh","PCS_Stages_sh","PCS_1day_sh","PCS_TT_sh"),names_to = "PCS_points_level",values_to="PCS_sh") %>%
  mutate(PCS_points_level=case_when(PCS_points_level=="PCS_GC_sh"~ "General classification",
                                    PCS_points_level=="PCS_Stages_sh"~ "Stages",
                                    PCS_points_level=="PCS_1day_sh"~ "1 day races",
                                    PCS_points_level=="PCS_TT_sh"~ "Time Trial")) %>% 
  mutate(PCS_points_level=fct_reorder(factor(PCS_points_level),PCS_sh,.fun=median,.desc=TRUE)) %>% 
  mutate(division=factor(division,levels=c("World Tour","Pro Tour"),ordered=T)) %>%  
  ggplot(aes(factor(year),PCS_sh,fill=factor(team_MPCC)))+
  geom_boxplot()+
  scale_y_continuous(trans = "sqrt",breaks=c(1,10,20,30,50,100,200,300)) +
  labs(x="Year",y="Average(PCS points/ N raced days)",fill="Team MPCC",caption="Only top 10 riders per team in each category")+
  facet_grid(division~PCS_points_level)+
  theme_light()+
  theme(axis.text.x=element_text(angle=90))

dev.off()


png(file=file.path("Outputs","Fig5b_PCSdistr_split_10riders_riderMPCC.png"))


t %>% 
  group_by(team) %>% 
  mutate(PCS_GC_rank=rank(-PCS_GC,ties.method= "random"),
         PCS_tot_rank=rank(-PCS_tot,ties.method= "random"),
         PCS_Stages_rank=rank(-PCS_Stages,ties.method= "random"),
         PCS_TT_rank=rank(-PCS_TT,ties.method= "random"),
         PCS_1day_rank=rank(-PCS_1day,ties.method= "random")) %>%
  mutate(PCS_GC=ifelse(PCS_GC_rank>10,NA,PCS_GC),
         PCS_tot=ifelse(PCS_tot_rank>10,NA,PCS_tot),
         PCS_Stages=ifelse(PCS_Stages_rank>10,NA,PCS_Stages),
         PCS_TT=ifelse(PCS_TT_rank>10,NA,PCS_TT),
         PCS_1day=ifelse(PCS_1day_rank>10,NA,PCS_1day),
         raced_days_tot=ifelse(PCS_tot_rank>10,NA,raced_days_tot),
         raced_days_GC=ifelse(PCS_GC_rank>10,NA,raced_days_GC),
         raced_days_Stages=ifelse(PCS_Stages_rank>10,NA,raced_days_Stages),
         raced_days_TT=ifelse(PCS_TT_rank>10,NA,raced_days_TT),
         raced_days_1day=ifelse(PCS_1day_rank>10,NA,raced_days_1day)) %>% 
  mutate(PCS_GC_sh=PCS_GC/raced_days_GC,
         PCS_Stages_sh=PCS_Stages/raced_days_Stages,
         PCS_1day_sh=PCS_1day/raced_days_1day,
         PCS_TT_sh=PCS_TT/raced_days_TT) %>% 
  pivot_longer(c("PCS_GC_sh","PCS_Stages_sh","PCS_1day_sh","PCS_TT_sh"),names_to = "PCS_points_level",values_to="PCS_sh") %>%
  mutate(PCS_points_level=case_when(PCS_points_level=="PCS_GC_sh"~ "General classification",
                                    PCS_points_level=="PCS_Stages_sh"~ "Stages",
                                    PCS_points_level=="PCS_1day_sh"~ "1 day races",
                                    PCS_points_level=="PCS_TT_sh"~ "Time Trial")) %>% 
  mutate(PCS_points_level=fct_reorder(factor(PCS_points_level),PCS_sh,.fun=median,.desc=TRUE)) %>% 
  mutate(division=factor(division,levels=c("World Tour","Pro Tour"),ordered=T)) %>%  
  ggplot(aes(factor(year),PCS_sh,fill=factor(rider_MPCC_2022_list)))+
  geom_boxplot()+
  scale_y_continuous(trans = "sqrt",breaks=c(1,10,20,30,50,100,200,300)) +
  labs(x="Year",y="Average(PCS points/ N raced days)",fill="Rider MPCC",caption="Only top 10 riders per team in each category")+
  facet_grid(division~PCS_points_level)+
  theme_light()+
  theme(axis.text.x=element_text(angle=90))

dev.off()
