library(ggplot2) 
library(dplyr) 
library(lubridate) 
library(scales) 
library(ggrepel)
#Antal rader som ska skippas från csv filen, inkl rubrikraden 
skiprowsattop = 11 
title = "Köldmängd Kiruna Flygplats" 
#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn 



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


skiprowsattop = 11 

file = "C:\\Users\\anders.johansson\\Downloads\\smhi-opendata_2_180940_20221109_064740.csv"

kiruna2 <- 
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

kiruna2$Ort = "Kiruna"






temp <- union_all(kiruna, kiruna2)

#Behåll endast vintermånader 
 temp <- 
   subset(temp, month(temp$Datum) %in% c(10, 11)) 

 # temp <- 
 #   subset(temp, month(temp$Datum) %in% c(11,12)) 
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
#Sortera på ort och datum 
temp <- temp %>%
  arrange(Season,Datum)

tmpkold <- 0 
tmpSeason <- "" 
#Räkna ut graddagar 
for (row in 1:nrow(temp)) { 
  #Ny ort köldmängd = 0 
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


aa <- temp %>% group_by(Season) %>% summarise(value = max(koldmangd))

bb <- aa %>% arrange(value)


# De år som ska framträda med färg 
#temp_filtered <- subset(temp, as.Date(Datum) >= "2017-10-01") 
temp_filtered <- subset(temp, Season %in% c(2023,1959,2012,2000,2019,2021)) 

max_val <- max(temp$koldmangd)

png("kiruna.png", units="in", width=7, height=5, res=300)
# insert ggplot code


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
    size = 0.7
  ) +
  
  geom_line(size = 1 ) + 
  
  scale_y_continuous(name = "Graddagar", labels = scales::comma, breaks = seq(0, max_val, by = 100), limits=c(0, max_val)) + 
  scale_x_date(name = "Månad", 
               labels = date_format("%b"), 
               date_breaks = "1 month") + 
  ggtitle(title) + 

  labs(caption = "Lufttemperatur, medelvärde 1 dygn, Kiruna Flygplats 1957 -") +

  
  theme( 
    legend.position = c(.25, .95), 
    legend.justification = c("right", "top"), 
    legend.box.just = "right", 
    legend.margin = margin(6, 6, 6, 6) 
    
  ) 
tempgraph

dev.off()