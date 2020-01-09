library(tidyverse)

url <- "https://en.wikipedia.org/wiki/Political_positions_of_the_2020_Democratic_Party_presidential_primary_candidates"
webpage <- read_html(url)

tbls <- webpage %>%
  html_table(fill = TRUE) %>%
  .[3:19]

tbls <- lapply(tbls, function(x){
  apply(x, 2, function(y){gsub("\\[.*?\\]", "", y)})
})

tbls <- lapply(tbls, data.frame)

tbls <- lapply(tbls, function(x){
  colnames(x) <- gsub("\\.", " ", colnames(x))
  x
})

tblNames <- webpage %>%
  html_nodes("h3") %>%
  html_text() %>%
  .[1:17]

tblNames <- gsub("\\[.*?\\]", "", tblNames)

names(tbls) <- tblNames

saveRDS(tbls, "DemocraticNomineePolicies.RDS")