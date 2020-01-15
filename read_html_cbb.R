
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

# get cbb overview data for the current and previous 5 years

get_url_ovrvw <- function(n_years){
  i = 1
  url_ovrvw = list()
  url_ovrvw[i] = paste0("https://www.sports-reference.com/cbb/seasons/",year(Sys.Date()),".html")
  for (i in 1:n_years){
    url_ovrvw[i+1] = paste0("https://www.sports-reference.com/cbb/seasons/",year(Sys.Date())-i,".html")
  }
  return(url_ovrvw)
}
url_ovrvw <- get_url_ovrvw(5)

get_cbb_ovrvw <- function(url_ovrvw){
  cbb_ovrvw <- data_frame()
  i = 1
  for (i in 1:length(url_ovrvw)){
    cbb <- url_ovrvw[[i]] %>% read_html() %>% 
    #html_nodes(xpath = "/html/body/div[2]/div[5]/div[2]/div[2]/div/table") %>% 
    html_nodes("table") %>%
    .[[1]] %>%
    html_table()
    cbb[14] = (year(Sys.Date()) - (i-1))    
    cbb_ovrvw <- rbind(cbb_ovrvw,cbb)
  }
  cbb_ovrvw %<>% dplyr::rename("year" = V14)
  return(cbb_ovrvw)
}
cbb_ovrvw <- get_cbb_ovrvw(url_ovrvw)

# get cbb teams data for the current and previous 5 years

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
  cbb_teams %<>% data.frame() %>% 
    #filter out spacer rows
    filter(cbb_teams[,2] != "School" & cbb_teams[,2] != "") %>% 
    #remove blank spacer column
    select(-cbb_teams[,17])
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


url_teams <- "https://www.sports-reference.com/cbb/seasons/2019-school-stats.html"
cbb_teams <- url_teams %>% read_html() %>% 
  #html_nodes(xpath = "/html/body/div[2]/div[5]/div[2]/div[2]/div/table") %>% 
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

cbb_teams %<>% data.frame() %>% 
  #filter out spacer rows
  filter(Var.2 != "School" & Var.2 != "") %>% 
  #remove blank spacer column
  select(-Var.17)
#make fresh headers
names(cbb_teams) <- c(
  "rank","school","ovrall_g","ovrall_wins","ovrall_loss","ovrall_w_l_perc","SRS","SOS",
  "conf_w","conf_l","home_w","home_l","away_w","away_l","pts_tm","pts_opp",
  "mp","fg","fga","fg_perc","three_p","three_pa","three_p_perc","ft","fta",
  "fta_perc","orb","trb","ast","stl","blk","tov","pf"
)

#--------------#
# housekeeping #
#--------------#

rm(list = c("url_ovrvw","url_teams"))


