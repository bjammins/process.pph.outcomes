library(scales)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)
library(graphics)

setwd('L:/PPH Project/data/Final Raw data/')
df <- read.csv("process_measures_05_05_2016.csv", head=TRUE)

## Paste a month to the character and covert the period to a date
df$Period <- as.Date(paste('01', df$Period, sep = " "), format='%d %b %Y')

#Remove 2016 period dates
df <- df[df$Period < "2016-01-01", ]

##  Calculate means and medians
timeser <- df %>% 
  filter(Complete. == "true" & is.na(Hospital.ID)==FALSE) %>% 
  group_by(Hospital.Name, Period) %>% 
  summarize(QBL=(Q27/(Q22+Q23))*100, RA_admit=(Q24/(Q22+Q23)*100), RA_pre=(Q25/(Q22+Q23)*100), RA_post=(Q26/(Q22+Q23)*100))

timeser <- timeser %>%
  filter(QBL<=100 & RA_admit<=100 & RA_pre <=100 & RA_post <=100)


process_summary <- timeser %>% 
  group_by(Period) %>% 
  summarize(QBL_med=median(QBL), QBL_mean=mean(QBL), 
            RA_admit_med=median(RA_admit), RA_admit_mean=mean(RA_admit), 
            RA_pre_med=median(RA_pre), RA_pre_mean=mean(RA_pre), 
            RA_post_med=median(RA_post), RA_post_mean=mean(RA_post))



#Plotting Admission Risk Assessment
ggplot(data=timeser,aes(Period,RA_admit)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='gray', n=1000) + 
  scale_fill_continuous(name="Density", low="pink",high="darkred", breaks=c(1e-05,2.5e-05,4e-05), labels=c("some hospitals","many hospitals", "most hospitals")) +
  guides(alpha="none") +
  geom_point() + theme_minimal()+ ylab("Percent")+ggtitle("Admission Risk Assessment") +
  scale_x_date(limits = c(as.Date("2014-07-01"), as.Date("2015-12-01")), 
               labels = date_format("%m/%y"),
               breaks=seq(as.Date("2014-07-01"), as.Date("2015-12-01"), by="2 months"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(data = process_summary, aes(Period, RA_admit_med, color = "median"),size = 1.25) +
  geom_line(data = process_summary, aes(Period, RA_admit_mean, color = "mean"),size = 1.25) +
  scale_colour_manual(name="", values = c(median="black", mean="blue")) 

#### Plotting QBL

ggplot(data=timeser,aes(Period,QBL)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='gray', n=1000) + 
  scale_fill_continuous(name="Density", low="pink",high="darkred", breaks=c(2e-05,4e-05,6e-05), labels=c("some hospitals","many hospitals", "most hospitals")) +
  guides(alpha="none") +
  geom_point() + theme_minimal()+ ylab("Percent")+ggtitle("Quantification of Blood Loss") +
  scale_x_date(limits = c(as.Date("2014-07-01"), as.Date("2015-12-01")), 
               labels = date_format("%m/%y"),
               breaks=seq(as.Date("2014-07-01"), as.Date("2015-12-01"), by="2 months"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(data = process_summary, aes(Period, QBL_med, color = "median"),size = 1.25) +
  geom_line(data = process_summary, aes(Period, QBL_mean, color = "mean"),size = 1.25) +
  scale_colour_manual(name="", values = c(median="black", mean="blue")) 

#Plotting Pre Birth RA
ggplot(data=timeser,aes(Period,RA_pre)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='gray', n=1000) + 
  scale_fill_continuous(name="Density", low="pink",high="darkred", breaks=c(1e-05,2.5e-05,4e-05), labels=c("some hospitals","many hospitals", "most hospitals")) +
  guides(alpha="none") +
  geom_point() + theme_minimal()+ ylab("Percent")+ggtitle("Pre-Birth Risk Assessment") +
  scale_x_date(limits = c(as.Date("2014-07-01"), as.Date("2015-12-01")), 
               labels = date_format("%m/%y"),
               breaks=seq(as.Date("2014-07-01"), as.Date("2015-12-01"), by="2 months"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(data = process_summary, aes(Period, RA_pre_med, color = "median"),size = 1.25) +
  geom_line(data = process_summary, aes(Period, RA_pre_mean, color = "mean"),size = 1.25) +
  scale_colour_manual(name="", values = c(median="black", mean="blue")) 

#Plotting Post Birth RA
ggplot(data=timeser,aes(Period,RA_post)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='gray', n=1000) + 
  scale_fill_continuous(name="Density", low="pink",high="darkred", breaks=c(1e-05,2.5e-05,4e-05), labels=c("some hospitals","many hospitals", "most hospitals")) +
  guides(alpha="none") +
  geom_point() + theme_minimal()+ ylab("Percent")+ggtitle("Post-Birth Risk Assessment") +
  scale_x_date(limits = c(as.Date("2014-07-01"), as.Date("2015-12-01")), 
               labels = date_format("%m/%y"),
               breaks=seq(as.Date("2014-07-01"), as.Date("2015-12-01"), by="2 months"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(data = process_summary, aes(Period, RA_post_med, color = "median"),size = 1.25) +
  geom_line(data = process_summary, aes(Period, RA_post_mean, color = "mean"),size = 1.25) +
  scale_colour_manual(name="", values = c(median="black", mean="blue")) 

#other alternate methods

sp <- ggplot(timeser, aes(x=Period, y=QBL)) +geom_point() 
# Gradient color
sp + stat_density_2d(aes(fill = ..level..), geom="polygon")
# Change the gradient color
sp + stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="white", high="red")


Lab.palette <- colorRampPalette(c("white", "lightblue", "blue","black"), space = "Lab")
smoothScatter(timeser$Period, timeser$QBL, colramp = Lab.palette,xaxt="n", yaxt="n")

days <- seq(min(timeser$Period), max(timeser$Period), by = "month")
percent <- seq(0, 100, by = 20)
#x-axis
axis(1, at=days,
     labels=strftime(days, "%b-%y"),
     tick=FALSE)
#y-axis
axis(2, at=percent,tick=FALSE)
