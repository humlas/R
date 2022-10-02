library(ggplot2) 
library(dplyr) 
library(lubridate) 
library(scales) 
library(hrbrthemes) 
#Antal rader som ska skippas från csv filen, inkl rubrikraden 
skiprowsattop = 11 
title = "Köldmängd Floda A" 
#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn 
file = "C:\\temp\\smhi-opendata_2_96040_20201201_132456.csv" 
temp <- 
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
#Behåll endast vintermånader 
temp <- 
  subset(temp, month(temp$Datum) %in% c(11, 12, 1, 2, 3, 4)) 
#Skapa säsong 
temp$Season = ifelse(month(temp$Datum) >= 10, 
                     as.character(year(temp$Datum) + 1), 
                     as.character(year(temp$Datum))) 
#Fiktivt datumfält för grafen, allt data i en tidserie 
temp$mmdd = ifelse( 
  month(temp$Datum) >= 10, 
  paste("1999", month(temp$Datum), day(temp$Datum), sep = "-"), 
  paste("2000", month(temp$Datum), day(temp$Datum), sep = "-") 
) 
temp$koldmangd = 0 
#Sortera på datum 
temp <- temp[order(temp$Datum),] 
tmpkold <- 0 
tmpSeason <- "" 
#Räkna ut graddagar 
for (row in 1:nrow(temp)) { 
  #Ny säsong köldmängd = 0 
  if (tmpSeason != temp[row, "Season"]) 
  { 
    tmpkold = 0 
    tmpSeason = temp[row, "Season"] 
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
temp_filtered <- subset(temp, as.Date(Datum) >= "2015-10-01") 
tempgraph <- 
  ggplot() + 
  geom_line( 
    aes( 
      x = as.Date(mmdd, "%Y-%m-%d"), 
      y = koldmangd, 
      group = Season 
    ), 
    data = temp, 
    colour = alpha("grey", 0.7) 
  ) + 
  geom_line( 
    aes( 
      x = as.Date(mmdd, "%Y-%m-%d"), 
      y = koldmangd, 
      group = Season, 
      color = Season 
    ), 
    data = temp_filtered, 
    size = 1 
  ) + 
   
  scale_y_continuous(name = "Graddagar", labels = scales::comma) + 
  scale_x_date(name = "Månad", 
               labels = date_format("%b"), 
               date_breaks = "1 month") + 
  ggtitle(title) + 
  theme_ipsum() + 
   
  theme( 
    legend.position = c(.25, .95), 
    legend.justification = c("right", "top"), 
    legend.box.just = "right", 
    legend.margin = margin(6, 6, 6, 6) 
     
  ) 
tempgraph