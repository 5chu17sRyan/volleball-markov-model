library(lubridate)
library(rvest)
library(tidyverse)
library(jsonlite)

#08-31-2019
start_date <- as.Date("08-31-2019", format = "%m-%d-%Y")
end_date <- as.Date("11-30-2019", format = "%m-%d-%Y")

index_date <- start_date

games_2020_21 <- data.frame(
  winning_team = NULL,
  losing_team = NULL,
  date = NULL,
  play_by_play_record = NULL,
  is_whitte_playing = NULL
)

core_url <- "https://www.ncaa.com"
play_by_play_extension <- "/play-by-play"

while(index_date <= end_date){
  #Create url
  month <- substr(index_date,6,7)
  day <- substr(index_date,9,10)
  year <- substr(index_date,1,4)
  
  big_ten_url <- paste0("https://www.ncaa.com/scoreboard/volleyball-women/d1/", year, "/", month, "/", day, "/big-ten")
  
  #Get xml table nodes
  link_elements <- big_ten_url %>% 
    read_html() %>%
    html_nodes(".gamePod-link") 
  
  ##### If there are any games #####
  if(length(link_elements) != 0)
  {
    ohio_state_index <- grep("ohio-st", link_elements)
    ###### If there is an ohio state game #####
    if(length(ohio_state_index) != 0)
    {
      #Get ohio state link
      ohio_state_href <- link_elements[grep("ohio-st", link_elements)] %>%
        html_attr("href")
      
      ohio_state_url <- paste0(core_url, ohio_state_href, play_by_play_extension)
      
      #Scrape game webpage
      divs <- ohio_state_url %>%
        read_html() %>%
        html_nodes("div")
        html_nodes(xpath = '/html/body/div[1]/div/main/div/div/div/div[3]/div/div[2]/div/div[1]/div[2]/table') 

      indecies <- html_attr(divs, "class") == "gamecenterContent layout--content-left"
      indecies[is.na(indecies)] <- F
      
      table_parent <- divs[indecies]
        
      descendent1 <- html_children(table_parent)[2]
      
      descendent2 <- html_children(descendent1)
      
      html_children(descendent2)
      
        html_attr("class")
        pluck(1) %>%
        html_table()
      
      length(table)
      
    }
  }
}