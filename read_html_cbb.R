
#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(tidyverse, rvest, stringr, magrittr, lubridate)


#----------------------#
# Scrape data from web #
#----------------------#

get_url_teams <- function(n_years){
  i = 1
  url_teams = list()
  url_teams[i] = paste0("https://www.sports-reference.com/cbb/seasons/",year(Sys.Date()),"-school-stats.html")
  for (i in 1:n_years){
    url_teams[i+1] = paste0("https://www.sports-reference.com/cbb/seasons/",year(Sys.Date())-i,"-school-stats.html")
  }
  return(url_teams)
}
url_teams <- get_url_teams(5)

get_cbb_teams <- function(url_teams){
  cbb_teams <- data_frame()
  i = 1
  for (i in 1:length(url_teams)){
    cbb <- url_teams[[i]] %>% read_html() %>% 
      #html_nodes(xpath = "/html/body/div[2]/div[5]/div[2]/div[2]/div/table") %>% 
      html_nodes("table") %>%
      .[[1]] %>%
      html_table()
    cbb[35] = (year(Sys.Date()) - (i-1))    
    cbb_teams <- rbind(cbb_teams,cbb)
  }
  cbb_teams %<>% dplyr::rename("year" = V35)
  #remove blank spacer column
  cbb_teams <- cbb_teams[,-17]
  #remove spacer rows
  cbb_teams %<>% data.frame() %>% 
    #filter out spacer rows
    filter(cbb_teams[,2] != "School" & cbb_teams[,2] != "") 
  #make fresh headers
  names(cbb_teams) <- c(
    "rank","school","ovrall_g","ovrall_wins","ovrall_loss","ovrall_w_l_perc","SRS","SOS",
    "conf_w","conf_l","home_w","home_l","away_w","away_l","pts_tm","pts_opp",
    "mp","fg","fga","fg_perc","three_p","three_pa","three_p_perc","ft","fta",
    "fta_perc","orb","trb","ast","stl","blk","tov","pf","year"
  )
  return(cbb_teams)
}
cbb_teams <- get_cbb_teams(url_teams)


#----------------#
# combine & prep #
#----------------#
#use the NCAA text from the site to make a tourney field
# cbb_df <- cbb_teams %>%
#   arrange(school) %>%
#   mutate(tourny = if_else(str_detect(school, "NCAA"),1,0),
#          school = str_remove(school,"NCAA"),
#          school = str_trim(school),
#          row_num = row_number())
# 
# feather::write_feather(cbb_df,"data/cbb_df.feather")
cbb_df <- feather::read_feather("data/cbb_df.feather")
#-----------------------------------------#
# clean for use with dlm-1223 data rounds #
#-----------------------------------------#
#pull in the tourney rounds and seeds data we need
#https://github.com/dlm1223/march-madness 
dlm_TourneySeeds <- feather::read_feather("data/dlm-1223_data/TourneySeeds.feather")
dlm_TourneySlots <- feather::read_feather("data/dlm-1223_data/TourneySlots.feather")
dlm_TourneyRounds <- feather::read_feather("data/dlm-1223_data/TourneyRounds.feather")

dlm_Teams <- feather::read_feather("data/dlm-1223_data/Teams.feather")
dlm_Teams %<>% filter(LastD1Season >= 2015) %>% mutate(Team_Full = str_to_lower(Team_Full)) %>% select(TeamID,Team_Full)

#make small frames to matchup team naming
cbb_team <- cbb_df %>% select(row_num, school) %>% mutate(school = str_to_lower(school)) %>% distinct() %>% arrange(school)
dlm_team <- dlm_Teams %>% select(Team_Full) %>% distinct() %>% arrange(Team_Full)


#find the mismatches that need attention - there are 64
#some will be removed because they have not been in NCAA and others renamed to match
dlm_team_names <- anti_join(cbb_team,dlm_team, by = c("school" = "Team_Full"))
`%not_in%` <- purrr::negate(`%in%`)
cbb_team_c <- cbb_team %>% 
  filter(school %not_in% c("alabama-birmingham","cal state long beach",
                        "central florida","detroit mercy","florida international",
                        "fort wayne","green bay","purdue-fort wayne","south carolina upstate",
                        "merrimack")) %>%
  mutate(school = str_replace(school, "-"," ")) %>%
  mutate(school = dplyr::recode(school,
                                `albany (ny)` = "albany",
                                #is = want; match cbb to teams
                                "bowling green state" = "bowling green" ,
                                "grambling" = "grambling state",
                                "long island university" = "long island",
                                "louisiana state" = "lsu",
                                "louisiana" = "louisiana lafayette",
                                "loyola (il)" = "loyola chicago" ,
                                "loyola (md)" = "loyola md",
                                "maryland baltimore county" = "umbc",
                                "miami (fl)" = "miami" ,
                                "miami (oh)" = "miami oh" ,
                                "mississippi" = "mississippi state" ,
                                "mount st. mary's" = "mount st marys" ,
                                "massachusetts lowell" = "massachusetts",
                                "milwaukee" = "wisconsin milwaukee",
                                "middle tennessee" = "middle tennessee state",
                                "nevada las vegas" = "nevada",
                                "north carolina state" = "north carolina" ,
                                "north carolina greensboro" = "north carolina a&t",
                                "north carolina greensboro" = "unc greensboro" ,
                                "north carolina asheville" = "unc asheville",
                                "north carolina wilmington" = "unc wilmington",
                                "prairie view" = "prairie view a&m",
                                "southern california" = "usc",
                                "st. francis (ny)" = "st francis ny",
                                "st. john's (ny)" = "st johns",
                                "st. bonaventure" = "st bonaventure",
                                "stephen f. austin" = "stephen f austin",
                                "saint peter's" = "st peters",
                                "saint joseph's" = "saint josephs",
                                "saint francis (pa)" = "st francis pa",
                                "saint mary's (ca)" = "st marys",
                                "south florida" = "usf",
                                "southern methodist" = "smu",
                                "texas rio grande valley" = "texas rgv",
                                "texas el paso" = "utep",
                                "texas arlington" = "ut arlington",
                                "texas christian" = "tcu",
                                "university of california" = "california",
                                "virginia commonwealth" = "virginia",
                                "vmi" = "virginia military"
                                )) 
dlm_team_names <- anti_join(cbb_team_c,dlm_team, by = c("school" = "Team_Full")) %>% select(-row_num) %>% distinct()

#get ranks, seeds and positions
cbb_team_c %<>% 
  left_join(cbb_df,by = c("row_num"="row_num")) %>%
  select(-school.y) %>%
  rename(school = school.x) %>%
  left_join(dlm_Teams,by = c("school" = "Team_Full")) %>%
  left_join(dlm_TourneySeeds, by = c("TeamID"="TeamID","year" = "Season")) %>%
  left_join(cbb, )
  
#get highest round data
#https://www.kaggle.com/andrewsundberg/college-basketball-dataset/data#
cbb <- read_csv("data/cbb.csv",)
cbb %<>% select(TEAM, CONF, POSTSEASON, SEED, YEAR) %>% 
  mutate(row_num = row_number(),
         TEAM = str_to_lower(TEAM),
         TEAM = str_replace(TEAM,"st\\.$","state"),
         TEAM = str_remove_all(TEAM,"\\'"),
         TEAM = str_remove_all(TEAM,"\\.")) %>%
  filter(TEAM %not_in% c("cal st fullerton","cal st northridge",
                           "fort wayne","green bay","purdue-fort wayne")) %>%
  mutate(TEAM = dplyr::recode(TEAM,
                                #is = want; match cbb to teams
                              "arkansas little rock" = "arkansas",
                              "byu" = "brigham young",
                              "central connecticut" = "central connecticut state",
                              "cal baptist" = "california baptist",
                              "cal st bakersfield" = "california",
                              "the citadel" = "citadel",
                              "liu brooklyn" = "long island",
                              "miami fl" = "miami" ,
                              "mississippi" = "mississippi state" ,
                              "milwaukee" = "wisconsin milwaukee",
                              "middle tennessee" = "middle tennessee state",
                              "nebraska omaha" = "nebraska",
                              "north carolina state" = "north carolina" ,
                              "penn" = "penn state",
                              "saint marys" = "st marys",
                              "saint peters" = "st peters",
                              "south florida" = "usf",
                              "southern miss" = "southern mississippi",
                              "vmi" = "virginia military",
                              "umkc" = "missouri kansas city",
                              "texas a&m corpus chris" = "texas a&m corpus christi",
                              "ut rio grande valley" = "texas rgv",
                              "utsa" = "texas san antonio"
  )) 
#does this match one of the other sets? ~of course not...
#65 mismatch
# cbb_names <- cbb %>%
#   select(TEAM) %>%
#   distinct() %>%
#   anti_join(cbb_team, by = c("TEAM" = "school"))
#27 mismatch
cbb_names <- cbb %>%
  select( TEAM) %>% #row_num,
  distinct() %>%
  anti_join(dlm_team, by = c("TEAM" = "Team_Full"))

#-------------------#
# prep for modeling #
#-------------------#
#goal: predict the highest round 


#--------------#
# housekeeping #
#--------------#

rm(list = c("url_ovrvw","url_teams","cbb","get_cbb_ovrvw","get_cbb_teams","get_url_ovrvw","get_url_teams"))
 