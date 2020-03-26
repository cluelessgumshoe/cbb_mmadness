
#----------#
# Analysis #
#----------#
#Time frame is last ten years (not including current year (2020))
#Included only teams haveing been to tourey within time frame

#-------#   
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(tidyverse, rvest, stringr, magrittr, lubridate,purrr,feather)

#----------------------#
# Useful Functions     #
#----------------------#
#this one makes cleaning up team names below easier by removing teams not D1 in our range of years etc.
`%not_in%` <- purrr::negate(`%in%`)

#This function, credit to the link below helps reorder columns in the big frame after all the joins
#the biggest benefit is that in this case where we have >40 columns, we can pick only the ones we want to
#act on to name instad of having to name all like with this: data.table::setcolorder()
##https://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-a-data-frame 
shuffle_columns <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]],
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first",
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}

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
url_teams <- get_url_teams(11)

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
#feather::write_feather(cbb_teams,"data/cbb_teams.feather")
cbb_teams <- read_feather("data/cbb_teams.feather")
#----------------#
# combine & prep #
#----------------#
#use the NCAA text from the site to make a tourney field
cbb_df <- cbb_teams %>%
  arrange(school) %>%
  mutate(tourny = if_else(str_detect(school, "NCAA"),1,0),
         school = str_remove(school,"NCAA"),
         school = str_trim(school),
         row_num = row_number())

feather::write_feather(cbb_df,"data/cbb_df.feather")
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
dlm_Teams %<>% filter(LastD1Season >= 2011) %>% mutate(Team_Full = str_to_lower(Team_Full)) %>% select(TeamID,Team_Full)

#make small frames to matchup team naming
cbb_team <- cbb_df %>% 
  #select(row_num, school) %>% #used this while renaming schools with the anti-joins
  select(-row_num) %>%
  mutate(school = str_to_lower(school)) %>% 
  distinct() %>% 
  arrange(school)

dlm_team <- dlm_Teams %>% 
  #select(Team_Full) %>% #used this while renaming schools with the anti-joins
  distinct() %>% 
  arrange(Team_Full)


#find the mismatches that need attention - there are 64
#some will be removed because they have not been in NCAA and others renamed to match - once all matched up this should net 0 records on the anti-join
#dlm_team_names <- anti_join(cbb_team,dlm_team, by = c("school" = "Team_Full"))
#the not in function is used here
cbb_team %<>% 
  filter(school %not_in% c("alabama-birmingham","cal state long beach",
                        "central florida","detroit mercy","florida international",
                        "fort wayne","green bay","purdue-fort wayne","south carolina upstate",
                        "merrimack","winston-salem")) %>%
  mutate(school = str_replace(school, "-"," ")) %>%
  mutate(school = dplyr::recode(school,
                                `albany (ny)` = "albany",
                                #is = want; match cbb to teams
                                "bowling green state" = "bowling green" ,
                                "centenary (la)" = "centenary",
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
                                "north carolina state" = "nc state" ,
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
                                )) %>% distinct()
#used the anti-join to make sure all schools were dealt with while aligning names - should yield zero rows
#dlm_team_names <- anti_join(cbb_team,dlm_team, by = c("school" = "Team_Full"))  %>% distinct()

#get highest round data
#https://www.kaggle.com/andrewsundberg/college-basketball-dataset/data#
cbb <- read_csv("data/cbb.csv")
cbb %<>% select(TEAM, CONF, POSTSEASON, SEED, YEAR) %>% 
  mutate(
         TEAM = str_to_lower(TEAM),
         TEAM = str_replace(TEAM,"st\\.$","state"),
         TEAM = str_remove_all(TEAM,"\\'"),
         TEAM = str_remove_all(TEAM,"\\.")) %>%
  filter(TEAM %not_in% c("cal st fullerton","cal st northridge",
                           "fort wayne","green bay","purdue-fort wayne","winston-salem")) %>%
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
                              "north carolina state" = "nc state" ,
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
#does this match one of the other sets? ~of course not...- once all matched up this should net 0 records on the anti-join
#65 mismatch
# cbb_names <- cbb %>%
#   select(TEAM) %>%
#   distinct() %>%
#   anti_join(cbb_team, by = c("TEAM" = "school"))
#27 mismatch
# cbb_names <- cbb %>%
#   #select( TEAM) %>% #used this while renaming schools with the anti-joins
#   distinct() %>%
#   anti_join(dlm_team, by = c("TEAM" = "Team_Full"))

#-------------------#
# make a big frame  #
#-------------------#
#get ranks, seeds and positions
cbb_team_c <- cbb_team %>% 
  left_join(dlm_Teams,by = c("school" = "Team_Full")) %>%
  left_join(dlm_TourneySeeds, by = c("TeamID"="TeamID","year" = "Season")) %>%
  #left_join(dlm_TourneyRounds, by = c("Seed"="Seed","year" = "Season")) %>%  
  left_join(cbb, by = c("school" = "TEAM","year" = "YEAR"))  %>%
  #select(-row_num.y,-row_num.x) %>%
  rename(seed_num = SEED,
         seed_bracket = Seed) %>%
  arrange(school,year) %>%
  mutate(result = case_when(
    is.na(POSTSEASON) ~ 0,
    POSTSEASON == "R68" ~ 1,
    POSTSEASON == "R64" ~ 2,
    POSTSEASON == "R32" ~ 3,
    POSTSEASON == "S16" ~ 4,
    POSTSEASON == "E8" ~ 5,
    POSTSEASON == "F4" ~ 6,
    POSTSEASON == "2nd" ~ 7,
    POSTSEASON == "2ND" ~ 7,
    POSTSEASON == "Champions" ~ 8,
    TRUE ~ 0
  ),
  row_num = row_number()) %>%
  filter(year != 2020) %>%
  select(-CONF,-POSTSEASON,-seed_bracket,-seed_num) %>%
  distinct()
#just in case - will throw a helpful error if any are still same name and didn't join right after you run the next two lines
names(cbb_team_c) %<>% stringr::str_remove_all(".x")
names(cbb_team_c) %<>% stringr::str_remove_all(".y")
names(cbb_team_c) %<>% stringr::str_to_lower()

#Useful function to re-order (imported above)
cbb_team_c %<>% .[shuffle_columns(names(cbb_team_c), "teamid first")] 
cbb_team_c %<>% .[shuffle_columns(names(cbb_team_c), "school after teamid")] 
cbb_team_c %<>% .[shuffle_columns(names(cbb_team_c), "rank after school")]
#cbb_team_c %<>% .[shuffle_columns(names(cbb_team_c), "seed_num after rank")]
#cbb_team_c %<>% .[shuffle_columns(names(cbb_team_c), "seed_bracket after seed_num")]
cbb_team_c %<>% .[shuffle_columns(names(cbb_team_c), "result after rank")]
cbb_team_c %<>% .[shuffle_columns(names(cbb_team_c), "year before school")] 

cbb_team_c %<>%   mutate_at(.funs = list(as.numeric),.vars = vars(rank:tour)) 

feather::write_feather(cbb_team_c,"data/cbb_team_c.feather")
#---------------------#
# housekeeping is fun #
#---------------------#

rm(list = c("dlm_team","dlm_Teams","dlm_TourneyRounds",
            "dlm_TourneySeeds","dlm_TourneySlots","url_teams",
            "cbb_teams","cbb_team","get_cbb_teams","get_url_teams",
            "cbb","cbb_df"))
