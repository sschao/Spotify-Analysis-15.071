#load in the Rvest and magrittr package
library(rvest)
library(magrittr)
library(dplyr)
library(caret)
library(lubridate)
library(httr)
#set dataframes aagain(using dataframe created by spotify.R)
#make into one word with underscore
data$album_names <- gsub(" ", "_", data$album_names)
albums <- as.vector(unique(data$album_names))
length(albums)

#test this for random subset
set.seed(456)
r <- runif(500, 0, length(albums))
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
  string <- toString(albums_rand[i])
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

#new
#run with length(albums) instead of length(albums_rand) to get result for all albums
date_per_song <- vector()
name <- vector()
for (i in seq(1,length(albums_rand))){
  print(i)
  string <- toString(albums_rand[i])
  #create link
  link <- create_link(i)
  #check if link is valid
  status <- check_site(link)
  #create valid webpage
  if(status != 404 & status != 400){
    webpage <- read_html(link)
    date_per_song[i] <- get_date(webpage)[[1]]
    if (get_date(webpage)[[1]] == "not found"){
      link <- paste(link, "_(album)", sep= "")
      #print(link)
      #check if new link is valid
      status <- check_site(link)
      #print(status)
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

name[489]
albums_rand[489]

print(date_per_song) 
#check accuracy of web scraping
j <- 0
for (i in seq(1,length(albums_rand))){
  if (date_per_song[[i]] == "not found"){
    j <- j + 1 
  }
}
print(j/length(albums_rand)) 

#clean date
#take out parenthesis
date <- vector()
date_cleaned <- vector()
for (i in seq(1, length(albums_rand))){
  if (date_per_song[i] != "not found"){
    date_cleaned[i] <- gsub("\\s*\\([^\\)]+\\)","",date_per_song[[i]])
    date[i] <- as.character(mdy(date_cleaned[i]))
  }else{
    date_cleaned[i] <- "not found"
    date[i] <- "not found"
  }
}
  
#create data with name and date  
date_name_data <- data.frame(gsub(" ", "_", name),date)
colnames(date_name_data)<- c("album_names", "song_date")
#if NA then put "not found"
date_name_data$song_date[is.na(date_name_data$song_date)] <- "not found"


#data_info$playlist <-NULL
#data_info <- unique(data_info)
#merge it with original dataset
merged <- merge(date_name_data,data,by="album_names",all=TRUE)
merged<- unique(merged)
#that dataset will be used for time series analysis once it is completed






