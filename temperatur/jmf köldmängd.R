library(ggplot2) 
library(dplyr) 
library(lubridate) 
library(scales) 
library(hrbrthemes) 
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

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_92410_20220913_130946.csv" 
arvika <- 
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

arvika$Ort = "Arvika"

skiprowsattop = 11 

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_132170_20221025_191831.csv" 
storlien <- 
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

storlien$Ort = "Storlien"


skiprowsattop = 11

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_180940_20221025_191923.csv" 
kiruna <- 
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

kiruna$Ort = "Kiruna"


skiprowsattop = 10 

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_64320_20221025_192104.csv" 

tingsryd <- 
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

tingsryd$Ort = "Tingsryd"




skiprowsattop = 10 

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_106570_20221025_193146.csv" 

amot <- 
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

amot$Ort = "Åmot"




skiprowsattop = 11 

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_96550_20221025_193354.csv" 

sala <- 
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

sala$Ort = "Sala"




skiprowsattop = 10 

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_85390_20221025_193419.csv" 

kvarn <- 
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

kvarn$Ort = "Kvarn"






temp <- union_all(holjes, arvika)

temp <- union_all(temp, storlien)

temp <- union_all(temp, kiruna)

temp <- union_all(temp, tingsryd)

temp <- union_all(temp, amot)

temp <- union_all(temp, sala)

temp <- union_all(temp, kvarn)


temp <- filter(temp, as.Date(Datum) >= "2021-10-01")


#Behåll endast vintermånader 
 temp <- 
   subset(temp, month(temp$Datum) %in% c(11, 12, 1, 2, 3, 4)) 

 # temp <- 
 #   subset(temp, month(temp$Datum) %in% c(11,12)) 
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
  arrange(Ort,Datum)

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
tempgraph <- 
  ggplot(temp_filtered, aes( 
    x = as.Date(mmdd, "%Y-%m-%d"), 
    y = koldmangd, 
    group = Ort, 
    color = Ort) ) + 
 
  geom_line(size = 1 ) + 
  
  scale_y_continuous(name = "Graddagar", labels = scales::comma) + 
  scale_x_date(name = "Månad", 
               labels = date_format("%b"), 
               date_breaks = "1 month") + 
  ggtitle(title) + 
  
  geom_label_repel(aes(label = Ort),
                   data = temp_filtered %>% filter(Datum == max(Datum)),
                   size = 3, max.overlaps = 10) +
 
  
  theme( 
    legend.position = c(.25, .95), 
    legend.justification = c("right", "top"), 
    legend.box.just = "right", 
    legend.margin = margin(6, 6, 6, 6) 
    
  ) 
tempgraph