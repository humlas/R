library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(hrbrthemes)

#Antal rader som ska skippas från csv filen, inkl rubrikraden
skiprowsattop = 11

title = "Köldygn Arvika A"

#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn
file = "C:\\temp\\smhi-opendata_2_92410_20201206_173014.csv"

#Data sista 4 månaderna
#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn
filelast4 = "C:\\temp\\smhi-opendata_2_92410_20201206_173016.csv"



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
temp <-
subset(temp, month(temp$Datum) %in% c(10, 11, 12, 1, 2, 3, 4))


#Skapa säsong
temp$Season = ifelse(month(temp$Datum) >= 10,
                    paste(as.character(year(temp$Datum)),substr(as.character(year(temp$Datum) + 1),3,4), sep = "/"),
                    paste(as.character(year(temp$Datum) - 1),substr(as.character(year(temp$Datum)),3,4), sep = "/"))

#Fiktivt datumfält för grafen, allt data i en tidserie
temp$mmdd = ifelse(
  month(temp$Datum) >= 10,
  paste("1999", month(temp$Datum), day(temp$Datum), sep = "-"),
  paste("2000", month(temp$Datum), day(temp$Datum), sep = "-")
)


temp$koldygn = 0

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
    if (temp[row, "Medeltemp"] <= 0)
    {
      tmpkold = tmpkold + 1
    }
    else
    {
      tmpkold = 0
    }
    temp[row, "koldygn"] = tmpkold
  }
}

temp_vinter <- subset(temp, koldygn == 5)

m1 <-
  temp_vinter %>% 
  group_by(Season) %>% 
  filter(row_number()==1)

m1$mmdd <- as.Date(m1$mmdd) - 4



# De år som ska framträda med färg
temp_filtered <- subset(temp, as.Date(Datum) >= "2020-10-01")

tempgraph <-
  ggplot( data= m1, aes(
    x = as.Date(mmdd, "%Y-%m-%d"),
    y = koldygn, label=Season)) +
 
  geom_point() +
  
  geom_text( angle = 45, hjust = 0) +
  

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
