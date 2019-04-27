
#load in the Rvest and magrittr package

library(rvest)
library(magrittr)
library(dplyr)
library(caret)
#set dataframes aagain
data_info <- read.csv("~/Desktop/song_info.csv")
data_info$album_names <- gsub(" ", "_", data_info$album_names)
albums <- as.vector(unique(data_info$album_names))
length(albums)

#test this for random subset
set.seed(666)
r <- runif(200, 0, 12014)
albums_rand <- albums[r]

date_per_song <- vector()
name <- vector()
for (i in seq(1,200)){
  print(i)
  #create link
  wiki_link <-"https://en.wikipedia.org/wiki/" 
  string <- toString(albums_rand[i])
  link <-paste(wiki_link, string, sep = "") 
  library(httr)
  #check if link is valid
  check <- GET(link)
  status <- status_code(check)
  if(status != 404 & status != 400){
    webpage <- read_html(link)
  }
  else{
    date_per_song[i] <- "not found"
    name[i] <- string
    next # jump to next iteration
  }
  
  #make table of scraped data
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
    name[i] <- string
  }
  
}

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
