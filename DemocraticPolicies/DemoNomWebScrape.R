#Group of packages including stringr for manipulating character strings
library(tidyverse)
#Package for data scraping
library(rvest)
#For making nice tables
library(knitr)
library(kableExtra)

#Grab all the html information from wikepedia
url <- "https://en.wikipedia.org/wiki/Political_positions_of_the_2020_Democratic_Party_presidential_primary_candidates"
webpage <- read_html(url)

#This grabs all the tables to display in my app and puts them in a list
tbls <- webpage %>%
  html_table(fill = TRUE) %>%
  .[3:19]

#The html tables have a lot of references denote by "[...]" which are being removed
tbls <- lapply(tbls, function(x){
  data.frame(apply(x, 2, function(y){
    y.out <- gsub("\\[.*?\\]", "", y)
    gsub("\n","",y.out)
  }))
})
#After using the apply function and retransforming to a data.frame it replaced 
#all spaces in the column names with a ".". This the spaces back in
tbls <- lapply(tbls, function(x){
  colnames(x) <- gsub("\\.", " ", colnames(x))
  x
})

#This grabs all the relevant table names from the wikepedia page
tblNames <- webpage %>%
  html_nodes("h3") %>%
  html_text() %>%
  .[1:17]

#remove the edit link text
tblNames <- gsub("\\[.*?\\]", "", tblNames)

#name the tables by the wikepedia table names
names(tbls) <- tblNames

#Saves the list in a .RDS for quick upload into the shiny app
saveRDS(tbls, "DemocraticNomineePolicies.RDS")

#################################################################
DemTabs <- tbls
DemTabNames <- names(DemTabs)
DemNames <- as.character(DemTabs[[1]]$Candidate)
DemSR <- as.character(DemTabs[[1]]$`Still running `)

#Transposing the tables and binding them all to 1 big table
qbcList <- lapply(DemTabNames, function(x){
  y <- data.frame(t(DemTabs[[x]][,-(1)]))
  colnames(y) <- DemTabs[[x]][,1]
  cbind(Category = rep(x, nrow(y)), y)
})
qbc <- do.call("rbind", qbcList)
qbs <- qbc[-(grep("Still running", row.names(qbc))[-1]),]
qbs$Category <- factor(qbs$Category, levels = c("Still Running", levels(qbs$Category)))
qbs$Category[1] <- "Still Running"

#Change the backgrounds conditionally
colorFun <- function(x){
  ifelse(grepl("Yes", x), "green",
         ifelse(grepl("No", x), "red",
                ifelse(grepl("Other", x), "grey", "white")))
}

#Convert table into html coded and change the background colors based on answers
qbs <- qbs %>%
  rownames_to_column(var = "Topic")%>%
  mutate_at(vars(-Topic, -Category),function(x){
    cell_spec(x, background = colorFun(as.character(x)))
  }) 

#Save the data
saveRDS(qbs, "qbc.RDS")


