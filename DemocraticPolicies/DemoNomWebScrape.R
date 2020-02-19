#Group of packages including stringr for manipulating character strings
library(tidyverse)
#Package for data scraping
library(rvest)
#For making nice tables
library(knitr)
library(kableExtra)
#Shiny stuff
library(shiny)
#A function to color survey answers
source("functions/ColorFun.R")

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
saveRDS(tbls, "data/DemocraticNomineePolicies.RDS")

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

qbcCols <- Reduce(intersect, lapply(qbcList, colnames))

qbc <- do.call("rbind", lapply(qbcList, function(x) x[qbcCols]))



qbs <- qbc[-(grep("Still running", row.names(qbc))[-1]),]
qbs$Category <- factor(qbs$Category, levels = c("Still Running", levels(qbs$Category)))
qbs$Category[1] <- "Still Running"



#Convert table into html coded and change the background colors based on answers
qbs <- qbs %>%
  rownames_to_column(var = "Topic")%>%
  mutate_at(vars(-Topic, -Category),function(x){
    cell_spec(x, background = colorFun(as.character(x)))
  }) 

#Save the data
saveRDS(qbs, "data/qbc.RDS")




#Question List

qbc2 <- qbc[-(grep("Still running", row.names(qbc))[-1]),]
# saveRDS(qbc2, "qbc2.RDS")

rQ <- list()
for(j in unique(qbc2$Category)){
  rQ[[j]] <- h2(j)
  
  Qs <- rownames(qbc2)[qbc2$Category == j]
  if(j == unique(qbc2$Category)[1]) Qs <- Qs[-1]
  for(i in Qs){
    tn <- paste("rQ", which(rownames(qbc2) == i)[1] - 1, sep = "")
    rQ[[i]] <- radioButtons(tn, i, choices = c("Yes", "Unsure", "No"), selected = "", inline = TRUE)
  }
}



# rQ <- lapply(paste("rQ",1:length(row.names(qbc2)[-1]), sep = ""), function(x){
#   radioButtons(x, x, choices = c("Yes", "Unsure", "No"), selected = "", inline = TRUE)
# })

saveRDS(rQ, "data/rQ.RDS")


####CLuster

qbcLM <- t(apply(qbc2[-1], 1, function(x){
  ifelse(grepl("Yes", x), 1, 
         ifelse(grepl("No", x), -1,
                ifelse(x %in% c("?", "Open", "Unclear"), 0, 1)))
}))

qbcLM <- apply(qbcLM,2,as.numeric)

qbLM2 <- cbind(as.character(qbc2[,1]), data.frame(qbcLM))
colnames(qbLM2) <- colnames(qbc2)
row.names(qbLM2) <- row.names(qbc2)

tqbcLM <- t(qbLM2[-1])

saveRDS(tqbcLM, "data/tqbcLM.RDS")


