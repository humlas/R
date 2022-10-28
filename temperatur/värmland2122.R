library(ggplot2) 
library(dplyr) 
library(lubridate) 
library(scales) 
library(hrbrthemes) 
#Antal rader som ska skippas från csv filen, inkl rubrikraden 
skiprowsattop = 11 
title = "Köldmängd Värmland 2021/22" 
#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn 
file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_94180_20220913_132907.csv" 
kristinehamn <- 
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

kristinehamn$Ort = "Kristinehamn"

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

skiprowsattop = 10 

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_103100_20220913_141250.csv" 
Gustavsfors <- 
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

Gustavsfors$Ort = "Gustavsfors"


skiprowsattop = 10 

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_94390_20220913_141153.csv" 
Daglosen <- 
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

Daglosen$Ort = "Daglösen"


skiprowsattop = 11 

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_92130_20220913_141200.csv" 
Blomskog <- 
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

Blomskog$Ort = "Blomskog"




skiprowsattop = 11 

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_93220_20220913_142336.csv" 
Karlstad <- 
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

Karlstad$Ort = "Karlstad Flygplats"




skiprowsattop = 10 

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_102170_20220913_142553.csv" 
ostmark <- 
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

ostmark$Ort = "Östmark"



skiprowsattop = 11 

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_93520_20220913_142742.csv" 
Sunne <- 
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

Sunne$Ort = "Sunne"





temp <- union_all(kristinehamn, arvika)

temp <- union_all(temp, Gustavsfors)

temp <- union_all(temp, Daglosen)

temp <- union_all(temp, Blomskog)

temp <- union_all(temp, Karlstad)

temp <- union_all(temp, ostmark)

temp <- union_all(temp, Sunne)


temp <- filter(temp, as.Date(Datum) >= "2021-10-01")


#Behåll endast vintermånader 
#temp <- 
#  subset(temp, month(temp$Datum) %in% c(11, 12, 1, 2, 3, 4)) 

temp <- 
  subset(temp, month(temp$Datum) %in% c(11, 12)) 
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
  ggplot() + 
  geom_line(
    aes(
      x = as.Date(mmdd, "%Y-%m-%d"),
      y = koldmangd,
      group = Ort
    ),
    data = temp,
    colour = alpha("grey", 0.7)
  ) +
  geom_line( 
    aes( 
      x = as.Date(mmdd, "%Y-%m-%d"), 
      y = koldmangd, 
      group = Ort, 
      color = Ort 
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