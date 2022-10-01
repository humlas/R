
library(RODBC)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(scales)
library(ggrepel)
library(lubridate)
library(flextable)
library(officer)
library(mailR)
library(rJava)
library(xlsx)
library(janitor)
library(stringr)


con <-
  odbcDriverConnect(
    'driver={SQL Server};server=andjoh;database=skate;trusted_connection=true'
  )


query <- "SELECT CONVERT(DATE, rt.Datum) AS Datum, rt.Koldmangd, rt.Sasong,
IIF(MONTH(rt.Datum) IN (10,11,12),DATEFROMPARTS(1999, DATEPART(MONTH, rt.Datum), DATEPART(DAY, rt.Datum)),DATEFROMPARTS(2000, DATEPART(MONTH, rt.Datum), DATEPART(DAY, rt.Datum))) AS MMDD
FROM dbo.GustavforsASingelDate AS rt
WHERE MONTH(rt.Datum) IN (10, 11,12,1,2,3)
ORDER BY rt.Datum"

res <- sqlQuery(con, gsub("\\n\\s+", " ", query))

close(con)


#Årets data
#==================================================================================
#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn
file = "C:\\temp\\smhi-opendata_2_103100_20201203_120614.csv"

temp <-
  read.csv2(
    file,
    header = FALSE,
    dec = ".",
    skip = 10,
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
  subset(temp, month(temp$Datum) %in% c(10, 11, 12))

#Skapa säsong
temp$Season = ifelse(month(temp$Datum) >= 10,
                    paste(as.character(year(temp$Datum)),as.character(year(temp$Datum) + 1),sep = "/"),
                    paste(as.character(year(temp$Datum)-1),as.character(year(temp$Datum)),sep = "/"))

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
#==================================================================================



res_filtered <- subset(res, trimws(Sasong) == "2019/2020")
#res_filtered <- subset(res, as.Date(Datum) >= "2015-10-01")

p2 <-
  ggplot() +
  #geom_line(aes(x = as.Date(MMDD, "%Y-%m-%d"),y = Koldmangd,group = Sasong), data=res, colour = alpha("grey",0.7)) +
  geom_line(aes(x = as.Date(MMDD, "%Y-%m-%d"),y = Koldmangd,color=Sasong), data=res_filtered, size=1) +
  geom_line(aes(x = as.Date(mmdd, "%Y-%m-%d"),y = koldmangd,color=Season), data=temp, size=1) +
  # geom_point(aes(
  #   x = as.Date(mmdd, "%Y-%m-%d"),
  #   y = koldmangd,
  #   group = Season,
  #   color = Season
  # ),
  #data = subset(temp, koldmangd>0),
  #size = 1) +
  # geom_text(aes(  x = as.Date(mmdd, "%Y-%m-%d"),
  #                 y = koldmangd,label=koldmangd), data = subset(temp, koldmangd>0) ,hjust=1, vjust=0) +
  # scale_y_continuous(name = "Graddagar", labels = scales::comma) +
  #scale_x_date(name = "Månad", labels = date_format("%b"), date_breaks = "1 month") +
  scale_x_date(name = "Vecka",
               labels = date_format("%V"),
               date_breaks = "1 week") +
  ggtitle("Köldmängd Gustavsfors") +
  theme_ipsum() +
 
  theme(
    legend.position = c(.25, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
    
  )

p2




