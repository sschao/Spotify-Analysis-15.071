#load in the Rvest and magrittr package

library(rvest)
library(magrittr)
library(dplyr)
library(caret)
#set dataframes aagain
data_info <- read.csv("~/Desktop/song_info.csv")
data_info$album_names <- gsub(" ", "_", data_info$album_names)
albums <- as.vector(data_info$album_names)
length(albums)

#test this for random subset
set.seed(345)
r <- runif(200, 0, 12014)
albums_rand <- albums[r]

##create functions
###
###
##

#function to check if site is valid
check_site <- function(link){
  check <- GET(link)
  status <- status_code(check)
}

#function to create link
create_link <- function(i){
  wiki_link <-"https://en.wikipedia.org/wiki/" 
  string <- toString(albums[i])
  link <-paste(wiki_link, string, sep = "") 
  paste(link)
}


#function to extract date
get_date <- function(webpage){
  
  table <- webpage %>%
    html_nodes("table.haudio") %>%
    html_table(header=F, fill = T)
  #keep the data
  if (length(table) == 0){
    date_per_song[i] <- "not found"
  } else {
    table <- table[[1]]
    #add the table to a dataframe
    dict <- as.data.frame(table)
    date_per_song[i] <- subset(dict, dict$X1 == "Released", select = c(X2)) 

  }
  
}

#new version of web scraping
date_per_song <- vector()
name <- vector()
for (i in seq(1,length(albums_rand)){
  print(i)
  string <- toString(albums_rand[i])
  #create link
  link <- create_link(i)
  print(link)
  #check if link is valid
  status <- check_site(link)
  #create valid webpage
  if(status != 404 & status != 400){
    webpage <- read_html(link)
    date_per_song[i] <- get_date(webpage)[[1]]
    if (get_date(webpage)[[1]] == "not found"){
      link <- paste(link, "_(album)", sep= "")
      print(link)
      #check if new link is valid
      status <- check_site(link)
      print(status)
      #create valid webpage
      if(status != 404 & status != 400){
        webpage <- read_html(link)
        date_per_song[i] <- get_date(webpage)[[1]]
      }
    }
    name[i] <- string
    }else { 
    date_per_song[i] <- "not found"
    name[i] <- string
    }
}

print(date_per_song) 

j <- 0
for (i in seq(1,200)){
  if (date_per_song[[i]] == "not found"){
    j <- j + 1 
  }
}
print(j/200) 



#example
#####
#####
#####
#read HTML code from the website
webpage <- read_html("https://en.wikipedia.org/wiki/Come_What[ever]_May")


table <- webpage %>%
  html_nodes("table.haudio") %>%
  html_table(header=F, fill = T)

table <- table[[1]]

#add the table to a dataframe
dict <- as.data.frame(table)
date_per_song <- subset(dict, dict$X1 == "Released", select = c(X2)) 


#check if site is valid
check <- GET("https://en.wikipedia.org/wiki/Come_What[ever]_May")
status <- status_code(check)
status == 400
