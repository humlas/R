
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
FROM dbo.Stockholm_BrommaSingleDate AS rt
WHERE MONTH(rt.Datum) IN (10, 11,12,1,2,3, 4)

ORDER BY rt.Datum"

res <- sqlQuery(con, gsub("\\n\\s+", " ", query))

close(con)


#res_filtered <- subset(res, trimws(Sasong) %in% c("1995/1996","1985/1986", "1986/1987", "2002/2003") | year(Datum) >= 2015)

res_filtered <- subset(res, trimws(Sasong) %in% c("2007/2008"))

#res_filtered <- subset(res, as.Date(Datum) >= "2015-10-01")

p2 <-
  ggplot() +
  geom_line(aes(x = as.Date(MMDD, "%Y-%m-%d"),y = Koldmangd,group = Sasong), data=res, colour = alpha("grey",0.7)) +
  geom_line(aes(x = as.Date(MMDD, "%Y-%m-%d"),y = Koldmangd,group = Sasong,color=Sasong), data=res_filtered, size=1) +
 
  scale_y_continuous(name = "Graddagar", labels = scales::comma) +
  scale_x_date(name = "Månad", labels = date_format("%b"), date_breaks = "1 month") +
  ggtitle("Köldmängd Bromma") +
  theme_ipsum() +
 
  theme(
    legend.position = c(.25, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
    
  )

p2




