library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(hrbrthemes)

#Antal rader som ska skippas från csv filen, inkl rubrikraden
skiprowsattop = 11

title = "Bromma"

#csv fil från smhi-opendata innehållande Lufttemperatur, medelvärde 1 dygn
#file = "C:\\temp\\smhi-opendata_2_92410_20201206_173014.csv" #Arvika
file = "C:\\temp\\smhi-opendata_2_97200_20201203_161217.csv" #Bromma
#file = "C:\\temp\\smhi-opendata_2_104580_20201208_152814.csv" #Mora
#file = "C:\\temp\\smhi-opendata_2_103090_20201208_154042.csv" #Gustavsfors
#file = "C:\\temp\\smhi-opendata_2_93250_20201208_154922.csv" #Karlstad historisk
#file = "C:\\temp\\smhi-opendata_2_93220_20201208_175729.csv" #Karlstad historisk









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

#temp <- merge(tempold,templast4, all=TRUE)

temp <- tempold



#Behåll endast vintermånader
temp <-
subset(temp, month(temp$Datum) %in% c(12, 1, 2))


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


medelvinter <- temp %>%
  group_by(Season) %>%
  summarise(
    Medel = mean(Medeltemp, na.rm = TRUE)
    
  )



a <- temp %>%

  summarise(
    Medel = mean(Medeltemp, na.rm = TRUE)
    
  )

medelall = a[1, "Medel"]

medelvinter <- medelvinter %>% 
  mutate(mycolor = ifelse(Medel>medelall, "#FF0000", "#0000FF"))





gg <- ggplot(medelvinter, aes(x=Season, y=Medel, label=round(Medel, digits = 1))) +
  geom_segment( aes(x=Season, xend=Season, y=medelall, yend=Medel, color=mycolor), size=3, alpha=0.9) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) +
  xlab("Säsong") +
  ylab("Temperatur") +
  geom_text(hjust = 0) +
 # geom_hline(yintercept=medelall, color = "blue", linetype="dashed") +
  geom_hline(yintercept=0, color = "red", linetype="dashed") +
  scale_colour_identity() +
  labs(title = title,
       subtitle = paste("Medeltemperatur för dec, jan och feb jämfört med", round(medelall,digits=1),"grader. Medel för hela serien"),
       caption = "Data: SMHI")




  

gg
