library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(hrbrthemes)

#Antal rader som ska skippas från csv filen, inkl rubrikraden
skiprowsattop = 11

title = "Arvika, november"

root = "C:\\Users\\anders.johansson\\Documents\\L�ngf�rdsskridsko\\V�der\\"


#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn

file = "smhi-opendata_2_92410_20211031_211621.csv"

file <- paste(root,file, sep="")



#Data sista 4 månaderna
#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn
#filelast4 = "C:\\temp\\smhi-opendata_2_�stmark4.csv"

#filelast4 = "C:\\temp\\smhi-opendata_2_97200_20201213_191909.csv" #Bromma





temp <-
  read.csv2(
    file,
    header = FALSE,
    dec = ".",
    skip = 12,
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


# 
# 
# templast4 <-
#   read.csv2(
#     filelast4,
#     header = FALSE,
#     dec = ".",
#     skip = skiprowsattop,
#     colClasses = c("NULL", "NULL", "Date", "numeric", "NULL", "NULL", "NULL"),
#     col.names = c(
#       "NULL",
#       "NULL",
#       "Datum",
#       "Medeltemp",
#       "NULL",
#       "NULL",
#       "NULL",
#       "NULL"
#     )
#   )
# 
# temp <- merge(tempold,templast4, all=TRUE)


#Behåll endast vintermånader
#temp <-
#  subset(temp, month(temp$Datum) %in% c(11, 12,1,2,3,4))
# temp <-
#   subset(temp, month(temp$Datum) %in% c(11,12,1,2,3,4))

temp <-
  subset(temp, month(temp$Datum) %in% c(11))



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
temp_filtered <- subset(temp, as.Date(Datum) >= "2017-10-01")

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
    size = 1.2
  ) +
  scale_y_continuous(name = "Graddagar", labels = scales::comma) +
  #scale_y_continuous(name = "Graddagar", labels = scales::comma, limits = c(0, 400)) +
  scale_x_date(name = "M�nad",
               labels = date_format("%d"),
               date_breaks = "1 day") +
  ggtitle(title) +
  theme_ipsum() +
 
  
  theme(
    legend.position = c(.25, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  
    
  ) 

tempgraph
