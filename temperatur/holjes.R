library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(ggrepel)
#Antal rader som ska skippas från csv filen, inkl rubrikraden
skiprowsattop = 11
title = "Köldmängd 2021/2022"
#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn
file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_102540_20221025_191742.csv"
holjes <-
  read.csv2(
    file,
    header = FALSE,
    
    dec = ".",
    skip = skiprowsattop,
    colClasses = c("NULL", "NULL", NA, NA, "NULL", "NULL", "NULL"),
    col.names = c(
      "NULL",
      "NULL",
      "Datum",
      "Medeltemp",
      "NULL",
      "NULL",
      "NULL",
      "NULL"
    )
  )

holjes$Ort = "Höljes"

temp <- holjes


temp <- filter(temp, as.Date(Datum) >= "2021-10-01")


#Behåll endast vintermånader
# temp <-
#   subset(temp, month(temp$Datum) %in% c(10, 11, 12, 1, 2, 3, 4))


# #Skapa säsong
# temp$Season = ifelse(month(temp$Datum) >= 10,
#                      as.character(year(temp$Datum) + 1),
#                      as.character(year(temp$Datum)))
#Fiktivt datumfält för grafen, allt data i en tidserie
temp$mmdd = ifelse(
  month(temp$Datum) >= 10,
  paste("1999", month(temp$Datum), day(temp$Datum), sep = "-"),
  paste("2000", month(temp$Datum), day(temp$Datum), sep = "-")
)



temp$koldmangd = 0
#Sortera på ort och datum
temp <- temp %>%
  arrange(Ort, Datum)

tmpkold <- 0
tmpSeason <- ""
#Räkna ut graddagar
for (row in 1:nrow(temp)) {
  #Ny ort köldmängd = 0
  if (tmpSeason != temp[row, "Ort"])
  {
    tmpkold = 0
    tmpSeason = temp[row, "Ort"]
  }
  else
  {
    if (tmpkold - temp[row, "Medeltemp"] > 0)
    {
      tmpkold = tmpkold - temp[row, "Medeltemp"]
    }
    else
    {
      tmpkold = 0
    }
    temp[row, "koldmangd"] = tmpkold
  }
}
# De år som ska framträda med färg
temp_filtered <- subset(temp, as.Date(Datum) >= "2017-10-01")

temp_filtered <-
  subset(temp_filtered, month(temp$Datum) %in% c(11))

temp_skate <- subset(temp, as.Date(Datum) == "2021-11-13" | as.Date(Datum) == "2021-11-21" | as.Date(Datum) == "2021-11-23" | as.Date(Datum) == "2021-11-26" )





max_val <- max(temp_filtered$koldmangd)

tempgraph <-
  ggplot(temp_filtered,
         aes(
           x = as.Date(mmdd, "%Y-%m-%d"),
           y = koldmangd,
           group = Ort,
           color = Ort
         )) +
  
  geom_line(size = 1) +
  
  scale_y_continuous(
    name = "Graddagar",
    labels = scales::comma,
    breaks = seq(0, max_val, by = 10),
    limits = c(0, max_val)
  ) +
  scale_x_date(name = "November",
               labels = date_format("%d"),
               date_breaks = "1 day",
               date_minor_breaks = "1 day") +
  ggtitle(title) +
  
  geom_point(size=3,data=temp_skate, colour = "blue" )+
  # 
  # geom_label_repel(
  #   aes(label = Ort),
  #   data = temp_filtered %>% filter(Datum == max(Datum)),
  #   size = 3,
  #   max.overlaps = 10
  # ) +
  theme(
    legend.position = c(.25, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)

  )
tempgraph