library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(hrbrthemes)

#Antal rader som ska skippas från csv filen, inkl rubrikraden
skiprowsattop = 10

title = "Köldmängd Sala A"

#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn
file = "C:\\temp\\smhi-opendata_2_96560_20201204_081317.csv"

#Data sista 4 månaderna
#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn
filelast4 = "C:\\temp\\smhi-opendata_2_96560_20201204_081319.csv"



tempold <-
  read.csv2(
    file,
    header = FALSE,
    dec = ".",
    skip = skiprowsattop,
    colClasses = c("NULL", "NULL", "Date", "numeric", "NULL", "NULL", "NULL"),
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


templast4 <-
  read.csv2(
    filelast4,
    header = FALSE,
    dec = ".",
    skip = skiprowsattop,
    colClasses = c("NULL", "NULL", "Date", "numeric", "NULL", "NULL", "NULL"),
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

temp <- merge(tempold,templast4, all=TRUE)


#Behåll endast vintermånader
#temp <-
#  subset(temp, month(temp$Datum) %in% c(11, 12, 1, 2, 3, 4))
temp <-
  subset(temp, month(temp$Datum) %in% c(11, 12))



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
temp_filtered <- subset(temp, as.Date(Datum) >= "2019-10-01")

tempgraph <-
  ggplot() +
  # geom_line(
  #   aes(
  #     x = as.Date(mmdd, "%Y-%m-%d"),
  #     y = koldmangd,
  #     group = Season
  #   ),
  #   data = temp,
  #   colour = alpha("grey", 0.7)
  # ) +
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
  scale_x_date(name = "Vecka",
               labels = date_format("%V"),
               date_breaks = "1 week") +
  ggtitle(title) +
  theme_ipsum() +
  
  theme(
    legend.position = c(.25, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
    
  )

tempgraph
