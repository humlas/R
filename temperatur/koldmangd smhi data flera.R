library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(hrbrthemes)

#Antal rader som ska skippas från csv filen, inkl rubrikraden
skiprowsattop = 11



root = "C:\\Users\\anders.johansson\\Documents\\L�ngf�rdsskridsko\\V�der\\"


#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn

file = "smhi-opendata_2_92410_20211031_211621.csv"


bromma = "smhi-opendata_2_97200_20211031_214255.csv"

linkoping = "smhi-opendata_2_85240_20211101_161558.csv"

vasteras = "smhi-opendata_2_96350_20211101_162037.csv"

karlstad = "smhi-opendata_2_93220_20211101_162440.csv"

gavle = "smhi-opendata_2_107420_20211101_162714.csv"

landvetter = "smhi-opendata_2_72420_20211101_213400.csv"

mora = "smhi-opendata_2_104580_20211101_213756.csv"



file <- paste(root,file, sep="")

temp <-
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


bromma <- paste(root,bromma, sep="")

skiprowsattop = 10

tempbromma <-
  read.csv2(
    bromma,
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


linkoping <- paste(root,linkoping, sep="")

skiprowsattop = 10

templinkoping <-
  read.csv2(
    linkoping,
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



vasteras <- paste(root,vasteras, sep="")

skiprowsattop = 13

tempvasteras <-
  read.csv2(
    vasteras,
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



karlstad <- paste(root,karlstad, sep="")

skiprowsattop = 11

tempkarlstad <-
  read.csv2(
    karlstad,
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



gavle <- paste(root,gavle, sep="")

skiprowsattop = 10

tempgavle <-
  read.csv2(
    gavle,
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



landvetter <- paste(root,landvetter, sep="")

skiprowsattop = 12

templandvetter <-
  read.csv2(
    landvetter,
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



mora <- paste(root,mora, sep="")

skiprowsattop = 15

tempmora <-
  read.csv2(
    mora,
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





temp$Ort = "Arvika"
tempbromma$Ort = "Bromma"
templinkoping$Ort = "Link�ping"
tempvasteras$Ort = "V�ster�s"
tempkarlstad$Ort = "Karlstad"
tempgavle$Ort = "G�vle"
templandvetter$Ort = "Landvetter"
tempmora$Ort = "Mora"





#Behåll endast vintermånader


temp <- union_all(temp, tempbromma)

temp <- union_all(temp, templinkoping)

temp <- union_all(temp, tempvasteras)

temp <- union_all(temp, tempkarlstad)

temp <- union_all(temp, tempgavle)

temp <- union_all(temp, templandvetter)

temp <- union_all(temp, tempmora)

title = "K�ldm�ngd 2006/07"

temp <- subset(temp, as.Date(Datum) >= "2006-10-01" & as.Date(Datum) <= "2007-06-01")

temp <-
  subset(temp, month(temp$Datum) %in% c(12,1))




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
temp <- temp[order(temp$Ort, temp$Datum),]

tmpkold <- 0
tmpSeason <- ""
tmpOrt <- ""

#Räkna ut graddagar
for (row in 1:nrow(temp)) {
  #Ny säsong köldmängd = 0
  if (tmpSeason != temp[row, "Season"] || tmpOrt != temp[row, "Ort"])
  {
    tmpkold = 0
    tmpSeason = temp[row, "Season"]
    tmpOrt = temp[row, "Ort"]
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
#temp_filtered <- subset(temp, as.Date(Datum) >= "2020-10-01")

tempgraph <-
  ggplot() +
  geom_line(
    aes(
      x = as.Date(mmdd, "%Y-%m-%d"),
      y = koldmangd,
      group = Ort,
      color = Ort
    ),
    data = temp,
    size = 1.2
  ) +
  
  scale_y_continuous(name = "Graddagar", labels = scales::comma) +
  #scale_y_continuous(name = "Graddagar", labels = scales::comma, limits = c(0, 400)) +
  scale_x_date(name = "M�nad",
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
