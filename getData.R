# Get word frequencies

# Packages
# install.packages('tidytext')
# install.packages('dplyr')
# install.packages('stringr')
# install.packages('ggplot2')
# install.packages('rvest')
setwd('~/Desktop/book-burst/')
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(rvest)

# Get chapters to loop through
root <- "https://info201.github.io/"
landing.page <- read_html(root)
chapters <- landing.page %>% html_nodes('.summary > .chapter') %>% html_attr('data-path')

# data 
data <- data.frame(chapter=vector(), level.two=vector(), level.three=vector(), value=vector())

# Loop through chapters and scrape text
for(chapter in chapters) {
  print(chapter == "index.html")
  if(chapter == "index.html") next
  url <- paste0(root, chapter)
  print(url)
  # Read in web page
  page <- read_html(url)
  
  # Second level headers
  level.two <- page %>% html_nodes('.section.level2')
  level.two.text <- level.two %>% html_text()
  
  # Get level 3 headers for each level2 section
  ids <- level.two %>% html_attr('id')
  for(i in 1:length(ids)) {
    print(paste('level 2:', i))
    # Only this level 2 element
    tmp.level.two <- level.two[i]
    level.two.chars <- tmp.level.two %>% html_text() %>% nchar()
    # Set value as number of words in level 2 if no level 3
    if(length(level.two[[i]] %>% html_nodes(".section.level3")) == 0) {
      this.row <- data.frame(chapter=chapter, level.two=ids[i], level.three="remainder", value=level.two.chars)
      data <- rbind(data, this.row)
      return
    } else {
      # Get level 3s of this level 2
      level.threes <- level.two[[i]] %>% html_nodes(".section.level3")
      level.three.ids <- level.threes %>% html_attr('id')
      remainder <- level.two.chars
      for(l3 in 1:length(level.three.ids)) {
        value <- level.threes[l3] %>% html_text() %>% nchar()
        remainder <- remainder - value
        id <- level.threes[l3] %>% html_attr('id')
        this.row <- data.frame(chapter=chapter, level.two=ids[i], level.three=id, value=value)
        data <- rbind(data, this.row)
      }
      # Set remaining value
      if(remainder !=0) {
        this.row <- data.frame(chapter=chapter, level.two=ids[i], level.three="remainder", value=remainder)
        data <- rbind(data, this.row)
      }
    } 
  }
}  

data$chapter <- sub(".html", "", data$chapter)
data <- data %>% 
    rename(level_two = level.two, level_three = level.three)
write.csv(x = data, file="./data/word-count.csv", row.names = FALSE)
