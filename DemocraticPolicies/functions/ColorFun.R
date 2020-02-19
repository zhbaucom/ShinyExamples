colorFun <- function(x){
  ifelse(grepl("Yes", x), "green",
         ifelse(grepl("No", x), "red", "white"))
}