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
##Date: May 15,2022
##########################################

################Outline###################
##
##  1.1. Team list    
##  1.2. Rider-team list 
##  1.3. Unique rider list
##  1.4. Rider results
##  1.5. Rider results (subcategories)
##  1.6. Merge dtb with team list
##  1.7. Extract MPCC (Current teams)
##  1.8. Extract MPCC (Current riders)
##  1.9. Extract MPCC (Former riders)
##  1.10. Article list MPCC
##
##################################################
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
#         1.1. Team list 2000-2022               #
#                                                #
##################################################


#Extract function
my_extract_WT_teams <- function(year){
  #Add year to the URL
  url <- paste0("https://www.procyclingstats.com/teams.php?year=",as.character(year),"&filter=Filter")

  h <- read_html(url)
  
  #Team list tag
  Team_list_WT <- h%>%
    html_elements(".mt20")%>%
    .[[1]]%>%
    html_elements("a")%>%
    html_attr("href")
  
  #Team name
  Team_name_WT <- h%>%
    html_elements(".mt20")%>%
    .[[1]]%>%
    html_elements("a")%>%
    html_text()
  
  #Team nationality WT
  Nationality_WT <- h%>%
    html_elements(".mt20")%>%
    .[[1]]%>%
    html_elements("span")%>%
    html_attr("class")%>%
    word(-1) %>% 
    .[-1] 
  
  #Team tag ProTour
  Team_list_ProTour <- h%>%
    html_elements(".mt20")%>%
    .[[3]]%>%
    html_elements("a")%>%
    html_attr("href")
  
  #Team name ProTour
  Team_name_ProTour <- h%>%
    html_elements(".mt20")%>%
    .[[3]]%>%
    html_elements("a")%>%
    html_text()
  
  #Team nationality ProTour
  Nationality_ProTour <- h%>%
    html_elements(".mt20")%>%
    .[[3]]%>%
    html_elements("span")%>%
    html_attr("class")%>%
    word(-1)%>% 
    .[-1] 
  
  WT_teams <- tibble(year=year,
         team=Team_list_WT,
         team_name=Team_name_WT,
         nation_team=Nationality_WT,
         division="World Tour")
  ProTour_teams <- tibble(year=year,
         team=Team_list_ProTour,
         team_name=Team_name_ProTour,
         nation_team=Nationality_ProTour,
         division="Pro Tour")
  
  WT_teams%>%
    bind_rows(ProTour_teams)
  
}

#Example
my_extract_WT_teams(2022)

#Team list merging
Team_list_2000_to_2022 <- map_dfr(2000:2022,my_extract_WT_teams)%>%
  mutate(team=str_remove(team,"team/"))

#1033 teams
nrow(Team_list_2000_to_2022)

#Save
save(Team_list_2000_to_2022,file=file.path("rda","Team_list_2000_to_2022"))
#Stata format
write_dta(Team_list_2000_to_2022, file.path("dta","Team_list_2000_to_2022.dta"))


##################################################
#                                                #
#         1.2. Rider-team list 2000-2022         #
#                                                #
##################################################


#Extract function
my_extract_rider_list <- function(team){
  url <- paste0("https://www.procyclingstats.com/team/",team)
  
  h <- read_html(url)
  
  #Riders tag
  riders <- h%>%
    html_elements(".list.pad2")%>%
    .[[4]]%>%
    html_elements("a")%>%
    html_attr("href")
  
  #Riders name
  riders_name <- h%>%html_elements(".list.pad2")%>%
    .[[4]]%>%
    html_elements("a")%>%
    html_text()
  
  #Rider age
  riders_age <- h%>%html_elements(".list.pad2")%>%
    .[[4]]%>%
    html_elements(".right")%>%
    html_text()
  
  #Rider nationality
  riders_nation <- h%>%html_elements(".list.pad2")%>%
    .[[4]]%>%
    html_elements("span")%>%
    html_attr("class")%>%
    word(-1)
  
  tibble(rider=riders,
         rider_name=riders_name,
         rider_age=riders_age,
         rider_nation=riders_nation,
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

#Save
save(Rider_list_2000_to_2022,file=file.path("rda","Rider_list_2000_to_2022"))
#Stata format
write_dta(Rider_list_2000_to_2022, file.path("dta","Rider_list_2000_to_2022.dta"))

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
 
Rider_list_length <- length(Rider_list)

#Dtb rider results
Rider_results_2000_to_2022_dtb <- map_dfr(Rider_list,function(r){
  i <- which(Rider_list==r)
  print(paste0("Rider n°",as.character(i),": ",r, "(",as.character(round(100*i/Rider_list_length,2)),"% completed)"))
  my_extract_rider_results(r)
})%>%
  rename_at(2:7,paste0,"_tot") 


#Save
save(Rider_results_2000_to_2022_dtb,file=file.path("rda","Rider_results_2000_to_2022_dtb"))
#Stata format
write_dta(Rider_results_2000_to_2022_dtb, file.path("dta","Rider_results_2000_to_2022_dtb.dta"))


##################################################
#                                                #
#       1.5. Rider results (subcategories)       #
#                                                #
##################################################


#Sub category code in the url
sub_code <- tibble(sub=c("GC","Stages", "One_day_races", "TT"),
                   code=c(4,1,8,2))

#Extract function
my_extract_rider_results_sub <- function(rider,sub_code){
  
  url <- paste0("https://www.procyclingstats.com/rider.php?proresults=0&proresults=1&pproresults=largerorequal&stage_type=",as.character(sub_code),"&filter=Filter&id=",rider,"&p=statistics&s=season-statistics")
  
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

#Example (Christophe Moreau - GC)
my_extract_rider_results_sub("christophe-moreau",sub_code$code[sub_code$sub=="GC"])

######GC results
Rider_results_2000_to_2022_GC_dtb <- map2_dfr(Rider_list,sub_code$code[sub_code$sub=="GC"],function(r,sub){
  i <- which(Rider_list==r)
  print(paste0("Rider n°",as.character(i),": ",r, "(",as.character(round(100*i/Rider_list_length,2)),"% completed)"))
  my_extract_rider_results_sub(r,sub)
})%>%
  rename_at(2:7,paste0,"_GC") 

#Save
save(Rider_results_2000_to_2022_GC_dtb,file=file.path("rda","Rider_results_2000_to_2022_GC_dtb"))
#Stata format
write_dta(Rider_results_2000_to_2022_GC_dtb, file.path("dta","Rider_results_2000_to_2022_GC_dtb.dta"))

######Stages results

Rider_results_2000_to_2022_Stages_dtb <- map2_dfr(Rider_list,sub_code$code[sub_code$sub=="Stages"],function(r,sub){
  i <- which(Rider_list==r)
  print(paste0("Rider n°",as.character(i),": ",r, "(",as.character(round(100*i/Rider_list_length,2)),"% completed)"))
  my_extract_rider_results_sub(r,sub)
})%>%
  rename_at(2:7,paste0,"_Stages") 

#Save
save(Rider_results_2000_to_2022_Stages_dtb,file=file.path("rda","Rider_results_2000_to_2022_Stages_dtb"))
#Stata format
write_dta(Rider_results_2000_to_2022_Stages_dtb, file.path("dta","Rider_results_2000_to_2022_Stages_dtb.dta"))

######One day races

Rider_results_2000_to_2022_1day_dtb <- map2_dfr(Rider_list,sub_code$code[sub_code$sub=="One_day_races"],function(r,sub){
  i <- which(Rider_list==r)
  print(paste0("Rider n°",as.character(i),": ",r, "(",as.character(round(100*i/Rider_list_length,2)),"% completed)"))
  my_extract_rider_results_sub(r,sub)
})%>%
  rename_at(2:7,paste0,"_1day") 

#Save
save(Rider_results_2000_to_2022_1day_dtb,file=file.path("rda","Rider_results_2000_to_2022_1day_dtb"))
#Stata format
write_dta(Rider_results_2000_to_2022_1day_dtb, file.path("dta","Rider_results_2000_to_2022_1day_dtb.dta"))

######TT
Rider_results_2000_to_2022_TT_dtb <- map2_dfr(Rider_list,sub_code$code[sub_code$sub=="TT"],function(r,sub){
  i <- which(Rider_list==r)
  print(paste0("Rider n°",as.character(i),": ",r, "(",as.character(round(100*i/Rider_list_length,2)),"% completed)"))
  my_extract_rider_results_sub(r,sub)
})%>%
  rename_at(2:7,paste0,"_TT") 

#Save
save(Rider_results_2000_to_2022_TT_dtb,file=file.path("rda","Rider_results_2000_to_2022_TT_dtb"))
#Stata format
write_dta(Rider_results_2000_to_2022_TT_dtb, file.path("dta","Rider_results_2000_to_2022_TT_dtb.dta"))


##################################################
#                                                #
#             1.6. Merge dtb with team list      #
#                                                #
##################################################

load(file=file.path("rda","Team_list_2000_to_2022"))
load(file=file.path("rda","Rider_results_2000_to_2022_dtb"))
load(file=file.path("rda","Rider_results_2000_to_2022_GC_dtb"))
load(file=file.path("rda","Rider_results_2000_to_2022_1day_dtb"))
load(file=file.path("rda","Rider_results_2000_to_2022_Stages_dtb"))
load(file=file.path("rda","Rider_results_2000_to_2022_TT_dtb"))
load(file=file.path("rda","Rider_list_2000_to_2022"))


Rider_results_2000_to_2022_dtb_clean <- Rider_list_2000_to_2022%>%
  left_join(Team_list_2000_to_2022,by=c("team","year"))%>%
  left_join(Rider_results_2000_to_2022_dtb,by=c("year","rider"))%>%
  left_join(Rider_results_2000_to_2022_GC_dtb,by=c("year","rider"))%>%
  left_join(Rider_results_2000_to_2022_Stages_dtb,by=c("year","rider"))%>%
  left_join(Rider_results_2000_to_2022_1day_dtb,by=c("year","rider"))%>%
  left_join(Rider_results_2000_to_2022_TT_dtb,by=c("year","rider"))
  


save(Rider_results_2000_to_2022_dtb_clean,file=file.path("rda","Rider_results_2000_to_2022_dtb_clean"))
#Stata format
write_dta(Rider_results_2000_to_2022_dtb_clean, file.path("dta","Rider_results_2000_to_2022_dtb_clean.dta"))

load(file=file.path("rda","Rider_results_2000_to_2022_dtb_clean"))


##################################################
#                                                #
#       1.7. Extract MPCC (Current teams)        #
#                                                #
##################################################

####Adhesion rules:
#-If info: Day of adhesion
#-If only year of adhesion: 1st of July
####

########World Teams

url <- "https://www.mpcc.fr/uci-world-teams/"
h <- read_html(url)

#Member names
MPCC_member_names_WT <- h%>%
  html_elements(".membres_caracteristiques")%>%
  html_elements("h2")%>%
  html_text

MPCC_member_date_WT <- h%>%
  html_elements(".membres_caracteristiques")%>%
  html_elements(".focus")%>%
  html_text()%>%
  as_tibble()%>%
  filter(str_detect(value,"^[0123]")==TRUE)%>%
  .$value

MPCC_member_WT <- tibble(team_MPCC=MPCC_member_names_WT,
                         date_MPCC=MPCC_member_date_WT,
                         division="WT")

######Pro Teams
  
url <- "https://www.mpcc.fr/uci-pro-teams/"
h <- read_html(url)  

#Member names
MPCC_member_names_ProTour <- h%>%
  html_elements(".membres_caracteristiques")%>%
  html_elements("h2")%>%
  html_text

MPCC_member_date_ProTour <- h%>%
  html_elements(".membres_caracteristiques")%>%
  html_elements(".focus")%>%
  html_text()%>%
  as_tibble()%>%
  filter(str_detect(value,"^[0123]")==TRUE)%>%
  .$value

MPCC_member_ProTour <- tibble(team_MPCC=MPCC_member_names_ProTour,
                         date_MPCC=MPCC_member_date_ProTour,
                         division="Pro Tour")

MPCC_team_members <- MPCC_member_WT%>%
  bind_rows(MPCC_member_ProTour)

save(MPCC_team_members,file=file.path("rda","MPCC_team_members"))
#Stata format
write_dta(MPCC_team_members, file.path("dta","MPCC_team_members.dta"))

load(file=file.path("rda","MPCC_team_members"))


##################################################
#                                                #
#       1.8. Extract MPCC (Current riders)       #
#                                                #
##################################################


url <- "https://www.mpcc.fr/coureurs/"
h <- read_html(url)

###Division nodes
nodes_division <- h%>%
  html_elements(".membres_div-niveau-equipe")

length_nodes_division <- length(nodes_division)

###Data extraction
MPCC_rider_dtb_Nov13_2022 <- map_dfr(1:length_nodes_division,function(i){
  division_name <- nodes_division[[i]]%>%
    html_elements("h3")%>%
    html_text()
  
  #Teams in the division
  nodes_team <- nodes_division[[i]]%>%
    html_elements(".membres_div-equipe")
  
  length_nodes_team <- length(nodes_team)
  
    map_dfr(1:length_nodes_team,function(j){
    
      team_name <- nodes_team[[j]]%>%
      html_elements("h4")%>%
      html_text()
    
      #team members
      rider_names <- nodes_team[[j]]%>%
        html_elements(".membres_div-coureur")%>%
        html_elements(".membres_nom-coureur")%>%
        html_text()
      
      #Nationality members
      rider_nation <- nodes_team[[j]]%>%
        html_elements(".membres_div-coureur")%>%
        html_elements(".membres_nationalite-coureur")%>%
        html_text()
      
      tibble(team_MPCC=team_name,
             rider_MPCC=rider_names,
             division=division_name,
             nation=rider_nation)
  })

})


save(MPCC_rider_dtb_Nov13_2022,file=file.path("rda","MPCC_rider_dtb_Nov13_2022"))
#Stata format
write_dta(MPCC_rider_dtb_Nov13_2022, file.path("dta","MPCC_rider_dtb_Nov13_2022.dta"))

load(file=file.path("rda","MPCC_rider_dtb_Nov13_2022"))


##################################################
#                                                #
#       1.9. Extract MPCC (Former riders)        #
#                                                #
##################################################

url <- "https://www.mpcc.fr/anciens-coureurs/"
h <- read_html(url)

former_rider_names <- h%>%
  html_elements("span.membres_nom-coureur")%>%
  html_text()
former_rider_nation <- h%>%
  html_elements(".membres_nationalite-coureur")%>%
  html_text()

Former_rider_MPCC_dtb_Nov13_2022 <- tibble(rider_name=former_rider_names,
                                           rider_nation=former_rider_nation)

save(Former_rider_MPCC_dtb_Nov13_2022,file=file.path("rda","Former_rider_MPCC_dtb_Nov13_2022"))
#Stata format
write_dta(Former_rider_MPCC_dtb_Nov13_2022, file.path("dta","Former_rider_MPCC_dtb_Nov13_2022.dta"))

load(file=file.path("rda","Former_rider_MPCC_dtb_Nov13_2022"))


##################################################
#                                                #
#         1.10. Article list MPCC                #
#                                                #
##################################################


article_cat <- c("adhesions","communique-de-presse","statistiques","tests-de-cortisolemie","divers")

my_extract_articles_cat <- function(n){
  
  print(as.character(n))
  url <- paste0("https://www.mpcc.fr/",n)
  h <- read_html(url)
  
  links_1 <- h%>%
    html_elements(".t-entry-title.h5")%>%
    html_elements("a")%>%
    html_attr("href")
  
  title_1 <-  h%>%
    html_elements(".t-entry-title.h5")%>%
    html_elements("a")%>%
    html_text()
  
  date_1 <- h%>%
    html_elements(".t-entry-date")%>%
    html_text()
  
  articles_1 <- tibble(links=links_1,
                       title=title_1,
                       date=date_1)
  
  articles_i <- map_dfr(2:10,function(i){
    url <- paste0("https://www.mpcc.fr/adhesions/?upage=",as.character(i))
    h <- read_html(url)
    
    links <- h%>%
      html_elements(".t-entry-title.h5")%>%
      html_elements("a")%>%
      html_attr("href")
    
    title <-  h%>%
      html_elements(".t-entry-title.h5")%>%
      html_elements("a")%>%
      html_text()
    
    date <- h%>%
      html_elements(".t-entry-date")%>%
      html_text()
    
    tibble(links,title,date)
  })
  article_list <- articles_1%>%
    bind_rows(articles_i)%>%
    mutate(article_cat=n)
  
  length_article_list <- nrow(article_list)
  
  article_text <- map_dfr(1:length_article_list,function(j){
    url <- article_list$links[j]
    h <- read_html(url)
    
    article_text <- h%>%
      html_element("div.col-lg-9")%>%
      html_text()
    
    tibble(article_text)
  })
  
  articles_MPCC_dtb_adhesion <- article_list%>%
    bind_cols(article_text=article_text)
}

article_MPCC_dtb <- map_dfr(article_cat,my_extract_articles_cat)


save(article_MPCC_dtb,file=file.path("rda","article_MPCC_dtb"))
#Stata format
write_dta(article_MPCC_dtb, file.path("dta","article_MPCC_dtb.dta"))

load(file=file.path("rda","article_MPCC_dtb"))


#########################################Attempt to filter by key words

article_MPCC_dtb%>%
  mutate(N_equipe=str_count(article_text,"équipe|formation"))%>%
  mutate(N_WT=str_count(article_text,"[Ww]orld\\s?[tT](our|eam)"))%>%
  mutate(N_ProTour=str_count(article_text,"[Pp]ro\\s?[tT](our|eam)"))%>%
  mutate(N_continental=str_count(article_text,"[Cc]ontinental"))%>%
  mutate(N_feminin=str_count(article_text,"([Ff]éminin|[Ww]omen)"))%>%
  mutate(N_masculin=str_count(article_text,"[Mm]asculin"))%>%
  mutate(N_young=str_count(article_text,"([Jj]eune|[Ee]spoir|[Jj]unior)"))%>%
  filter(N_equipe!=0 & sum(N_WT,N_ProTour,N_continental,N_masculin)!=0)%>%
  nrow()






