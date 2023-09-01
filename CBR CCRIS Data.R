#Script to produce estimates of future Central Background Registry (CBR) application 
#for licensing needs based on renewal dates

library(tidyverse)
library(here)
library(openxlsx)
library(readxl)
library(lubridate)

#Read in data from Excel file
CBR <- read_xlsx(here("data","CBR App Project.xlsx"))%>%
  #filter out any rows with an expiration date from more than 1 month ago
  #there are a number of entries with old expiration dates, which seems anomalous
  filter(`Expiration Date` > Sys.Date() %m-% months(6))%>%
  #Filter out any with duplicated registry number that are also active
  #this removes entries with recent reapplications, keeping the later renewal date
  filter(!((duplicated(`Registry Number`) | duplicated(`Registry Number`, fromLast = TRUE)) & Status == "Active"))%>%
  #Add following two expiration dates, assuming 5-year expiration interval and on-time renewal
  mutate(`Expiration Date 2` = `Expiration Date` %m+% years(5) ,
         `Expiration Date 3` = `Expiration Date` %m+% years(10))

#Gather all renewal dates into a single data vector and put it into a data frame for plotting
CBR_Renewals <- data.frame(AllRenewalDates = c(CBR$`Expiration Date`, CBR$`Expiration Date 2`, CBR$`Expiration Date 3`))

#Generate histogram of renewal dates
CBRPlot <- ggplot(CBR_Renewals, aes(x = AllRenewalDates))+
    geom_histogram(colour = 4, fill = "white", 
                   #binwidth is per second for time objects, so set to sec*min*hour*days (2-weeks currently)
                   binwidth = (60*60*24*14))+
    #Add 1-month breaks and format break labels as Mon - Yr
    scale_x_datetime(date_breaks = "1 month",
                     date_labels = "%b-%Y")+
    #Add breaks at intervals of 200 (NOTE: must manually adjust for new data)
    scale_y_continuous(breaks = c(200, 400, 600, 800, 1000, 1200, 1400))+
    #Zoom in on plot x-axis from current date to 10 years from current dates
    #For some reason, needed to expand y axis otherwise top bar cut off (NOTE: must manually adjust for new data)
    coord_cartesian(xlim =c(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()%m+% years(10))), ylim = c(0,1400),expand = FALSE)+
    #Use the next line just for getting appropriate limits in ggbuild below
    #xlim(as.POSIXct(as.Date("2023-05-02")), as.POSIXct(as.Date("2023-05-02") %m+% years(10)))+
    xlab("Month")+
    ylab("Count")+
    ggtitle("CBR Renewal Rate 10-Year Forecast (2-Week Bins)")+
    #Use "minimal" theme
    theme_bw()+
    #Angle x-axis break labels and center plot title
    theme(axis.text.x = element_text(angle=45,
                                     hjust = 1,
                                     vjust = 1),
          plot.title = element_text(hjust = 0.5))

#Save bin information from plot for additional analysis
CBRPlot_Data <- ggplot_build(CBRPlot)[["data"]][[1]]

#Get basic descriptive stats
mean(CBRPlot_Data$count)
median(CBRPlot_Data$count)
min(CBRPlot_Data$count)
max(CBRPlot_Data$count)

#Save file
ggsave(here("output","CBR Renewal 10-yr Forecast_v2.jpeg"),
       height = 10, width = 30)
