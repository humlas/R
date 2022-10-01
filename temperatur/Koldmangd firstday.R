
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


query <- "
WITH FirstDate AS (
    SELECT sbsd.Datum,
	       sbsd.Sasong, 
	       sbsd.Koldmangd,
           ROW_NUMBER() OVER(PARTITION BY sbsd.Sasong 
                                 ORDER BY sbsd.Koldmangd) AS rk
      FROM dbo.Stockholm_BrommaSingleDate AS sbsd
	  WHERE sbsd.Koldmangd >= 40
	  AND NOT sbsd.Sasong IS NULL)
SELECT fd.Sasong, fd.Datum, fd.Koldmangd
  FROM FirstDate AS fd
 WHERE fd.rk = 1"

res <- sqlQuery(con, gsub("\\n\\s+", " ", query))

close(con)

DF2 <- res %>%
  mutate(Year=format(Datum,"%Y"),
         Date_day=as.POSIXlt(Datum, origin = "1960-01-01")$yday)%>%
  group_by(Sasong) %>%
  mutate(Col = mean(Date_day, na.rm = T),Mean_date=format(as.Date(paste0(Year,"-01-01"))+Col,"%m-%d"))%>%
  select(Datum,Sasong,Mean_date)



DF2 <- res %>%
  mutate(Year=format(Datum,"%Y"))


DF2




