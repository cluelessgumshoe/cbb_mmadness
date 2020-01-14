
#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(tidyverse, rvest, stringr, magrittr)


#----------------------#
# Scrape data from web #
#----------------------#

# Scrape wikipedia table
url_ovrvw <- "https://www.sports-reference.com/cbb/seasons/2019.html"

cbb_2019_ovrvw <- url_ovrvw %>% read_html() %>% 
  #html_nodes(xpath = "/html/body/div[2]/div[5]/div[2]/div[2]/div/table") %>% 
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

cbb_2019_ovrvw %<>% data.frame()

url_teams <- "https://www.sports-reference.com/cbb/seasons/2019-school-stats.html"
cbb_2019_teams <- url_teams %>% read_html() %>% 
  #html_nodes(xpath = "/html/body/div[2]/div[5]/div[2]/div[2]/div/table") %>% 
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

cbb_2019_teams %<>% data.frame() %>% 
  #filter out spacer rows
  filter(Var.2 != "School" & Var.2 != "") %>% 
  #remove blank spacer column
  select(-Var.17)
#make fresh headers
names(cbb_2019_teams) <- c(
  "rank","school","ovrall_g","ovrall_wins","ovrall_loss","ovrall_w_l_perc","SRS","SOS",
  "conf_w","conf_l","home_w","home_l","away_w","away_l","pts_tm","pts_opp",
  "mp","fg","fga","fg_perc","three_p","three_pa","three_p_perc","ft","fta",
  "fta_perc","orb","trb","ast","stl","blk","tov","pf"
)


