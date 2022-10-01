library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(hrbrthemes)
library(ggrepel)

#Antal rader som ska skippas från csv filen, inkl rubrikraden
skiprowsattop = 11

title = "Arvika"

root = "C:\\Users\\anders.johansson\\Documents\\L�ngf�rdsskridsko\\V�der\\"


#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn
file = "smhi-opendata_2_92410_20211031_211621.csv" #Arvika
#file = "C:\\temp\\smhi-opendata_2_97200_20201203_161217.csv" #Bromma
#file = "C:\\temp\\smhi-opendata_2_104580_20201208_152814.csv" #Mora
#file = "C:\\temp\\smhi-opendata_2_103090_20201208_154042.csv" #Gustavsfors
#file = "C:\\temp\\smhi-opendata_2_93250_20201208_154922.csv" #Karlstad historisk
#file = "C:\\temp\\smhi-opendata_2_93220_20201208_175729.csv" #Karlstad historisk

file <- paste(root,file, sep="")








#Data sista 4 månaderna
#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn
#filelast4 = "C:\\temp\\smhi-opendata_2_92410_20201206_173016.csv" #Arvika

#filelast4 = "C:\\temp\\smhi-opendata_2_97200_20201213_191909.csv" #Bromma





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

#temp <- merge(tempold,templast4, all=TRUE)

temp <- tempold



#Behåll endast vintermånader
temp <-
subset(temp, month(temp$Datum) %in% c(10,11,12,1,2,3,4))




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


#temp <- subset(temp, as.Date(mmdd) <= '1999-12-12')





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




medelvinter <- temp %>%
  group_by(Season) %>%
  summarise(
    koldmangd = max(koldmangd, na.rm = TRUE)
    
  )



a <- medelvinter %>%

  summarise(
    koldmangd = median(koldmangd, na.rm = TRUE)
    
  )

medianall = a[1, "koldmangd"]

medelvinter <- medelvinter %>% 
  mutate(mycolor = ifelse(koldmangd>medianall$koldmangd, "#0000FF","#FF0000"))



#round(koldmangd, digits = 0))

gg <- ggplot(medelvinter, aes(x=Season, y=koldmangd,label=round(koldmangd, digits = 0))) +
  geom_segment( aes(x=Season, xend=Season, y=medianall$koldmangd, yend=koldmangd, color=mycolor), size=3, alpha=0.9) +
  
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  xlab("S�song") +
  ylab("Graddagar") +
   geom_text_repel(
     nudge_x      = 0.15,
     direction    = "y",
     hjust        = 0,
     segment.size = 0.2
    
   ) +
  geom_hline(yintercept=medianall$koldmangd, color = "blue", linetype="dashed") +
  # geom_hline(yintercept=0, color = "red", linetype="dashed") +
  scale_colour_identity() +
  labs(title = title,
       subtitle = paste("S�songer jmf median max uppn�dd k�ldm�ngd", round(medianall$koldmangd,digits=1),"graddagar"),
       caption = "Data: SMHI")
  




  

gg
