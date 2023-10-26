library(rvest)
library(tidyverse)
library(quanteda)

vectorize_item <- function(my_char_vec){
  a <- unlist(strsplit(my_char_vec, "\nDozent/-in:\n"))
  a <- unlist(strsplit(a, "\nZeit und Ort:\n"))
  a <- unlist(strsplit(a, "\nwöchentlich "))
  a <- unlist(strsplit(a, "\t\t\n"))
  a <- unlist(strsplit(a, ", Raum:"))
  a <- unlist(strsplit(a, "\nZielgruppe:\n "))
  a <- unlist(strsplit(a, "\nZielgruppen:\n" ))
  a <- unlist(strsplit(a, "\n\n\nKommentar:\n\n"))
  return(a)
}

# ------------------------------------------------------------------------------

link2 = "https://studip.uni-giessen.de/evv/extern.php?parent_id=fd5f4402d02fdb4aaf687f04323852d5"
page = read_html(link2)
link = "https://studip.uni-giessen.de/evv/extern.php?parent_id=d7f5ba03c2cc44c075b7a773ca11b44a#v6ecfd24271469cbee0b5b3d4f6479d75"
page = read_html(link)

page = read_html("https://studip.uni-giessen.de/evv/extern.php?parent_id=d7f5ba03c2cc44c075b7a773ca11b44a")


lectureitems0 = page %>% html_elements(".lectureItem0") %>% html_text2()
lectureitems1 = page %>% html_elements(".lectureItem1") %>% html_text2()
items <- c(rbind(lectureitems1, lectureitems0))

#items[1]
#items[12]
#items[28]

items_list <- sapply(items, vectorize_item)
items_list[12]



df <- do.call(rbind.data.frame, items_list)

names(df)[1] <- "title"
names(df)[2] <- "lecturer"
names(df)[3] <- "startdate"
names(df)[4] <- "day_time"
names(df)[5] <- "nedftdate"
names(df)[6] <- "place"
names(df)[7] <- "audience"
names(df)[8] <- "comment"

view(df)


?gsub
rstudioapi::getSourceEditorContext()$path
write.csv(df, paste(gsub("evv.R", "evv.csv",rstudioapi::getSourceEditorContext()$path)), row.names=FALSE)

