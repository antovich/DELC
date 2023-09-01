#Light cleaning and visualization for TA log data from FCCO

library(tidyverse)
library(here)

#Read in TA log data
TA_data <- read_csv(here('data','FCCO TA log_1aug21-17nov21_Downloaded by AliciaM.csv'), name_repair = "universal")%>%
  #Add variable in which the subtopic for session focus is removed to limit the number of categories
  mutate(SessionFocus1_Primary = recode(as.factor(as.vector(sapply(Session.Focus.1,str_replace,  ":.+", ""))), "System Navigation " = "System Navigation"),
         #Mark non-numeric data as NA (some entries include "min" etc.)
         ContactTime_Numeric = as.numeric(as.character(Contact.Time)),
         #Convert date string to data format
         DateFormat = as.Date(Date.of.Contact, tryFormats = c("%m/%d/%Y")),
         #Create var to find notes with mention of Preschool Promise
         PreschoolPromiseNote= str_detect(Discussion.Notes, "(?i)preschool promise"))%>%
  #Create var with month in place of date
  mutate(Month = ifelse(DateFormat<as.Date("09/01/2021", "%m/%d/%Y"), "Aug",
          ifelse(DateFormat<as.Date("10/01/2021", "%m/%d/%Y"), "Sept",
            ifelse(DateFormat<as.Date("11/01/2021", "%m/%d/%Y"), "Oct",
              ifelse(DateFormat<as.Date("12/01/2021", "%m/%d/%Y"), "Nov",NA))))
  )

#Sum contact time by region and session focus
GroupSum <- TA_data%>%
  group_by(Region, SessionFocus1_Primary)%>%
  summarize(sum = sum(ContactTime_Numeric, na.rm = TRUE))

#Plot sum of contact time by region and session topic (columns ordered by total contact time)
ggplot(GroupSum, aes(x = reorder(Region, -sum), y = sum, fill = SessionFocus1_Primary))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Hub", y="Contact Time in Min", fill = "Session Topic")

ggsave(here("figures", "Total TA Contact Time by Topic & Hub.jpeg"), dpi=600)

#Plot proportion of contact time dedicated to each session topic by region (columns ordered by total contact time)
ggplot(GroupSum, aes(x = reorder(Region, -sum), y = sum, fill = SessionFocus1_Primary))+
  geom_bar(stat = "identity", position = "fill")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Hub", y="Proportion of Contact Time", fill = "Session Topic")

ggsave(here("figures", "Prop TA Contact Time by Topic & Hub.jpeg"), dpi=600)

#Plot sum of all contact time by region (columns ordered by total contact time)
ggplot(GroupSum, aes(x = reorder(Region, -sum), y = sum, fill = reorder(Region, -sum)))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "")+
  xlab("Hub")+
  ylab("Contact Time in Min")

ggsave(here("figures", "Total TA Contact Time Hub.jpeg"), dpi=600)


#Sum contact time by region and session focus
GroupSumMonth <- TA_data%>%
  group_by(Region, Month)%>%
  summarize(sum = sum(ContactTime_Numeric, na.rm = TRUE))%>%
  mutate(Month = factor(Month, levels = c("Aug", "Sept", "Oct", "Nov")))

#Plot sum of all contact time by region (columns ordered by total contact time)
ggplot(GroupSumMonth, aes(x = reorder(Region, -sum), y = sum, fill = reorder(Region, -sum)))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "")+
  xlab("Hub")+
  ylab("Contact Time")+
  facet_wrap("Month", ncol= 1)

ggsave(here("figures", "Total TA Contact Time Hub by Month.jpeg"), dpi=600, width = 3, height = 5, units = "in")

GroupCountPP <- TA_data%>%
  group_by(Region)%>%
  summarize(Count = sum(PreschoolPromiseNote, na.rm = TRUE))

ggplot(GroupCountPP, aes(x = reorder(Region, -Count), y = Count, fill = reorder(Region, -Count)))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "")+
  xlab("Hub")+
  ylab("Count of Preschool Promise Mentions\nin TA Notes")

ggsave(here("figures", "Count of Preschool Promise Mentions by Hub.jpeg"), dpi=600)

GroupHoursPP <- TA_data%>%
  #Create var for contact time only when preschool promise is mentioned in notes
  mutate(PPContact = as.numeric(PreschoolPromiseNote)*ContactTime_Numeric)%>%
  group_by(Region)%>%
  summarize(sum = sum(PPContact, na.rm = TRUE))

#Plot sum of contact time with mention of Preschool Promise in notes by region (columns ordered by contact time)
ggplot(GroupHoursPP, aes(x = reorder(Region, -sum), y = sum, fill = reorder(Region, -sum)))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "")+
  xlab("Hub")+
  ylab("Contact Time in Min for TA\nw/ Preschool Promise Mentions in Notes")

ggsave(here("figures", "Total TA Contact Time with PP Mention by Hub.jpeg"), dpi=600)


#Sum contact time by region and session focus
GroupSumContact <- TA_data%>%
  group_by(Region, Contact.Initiator)%>%
  summarize(sum = sum(ContactTime_Numeric, na.rm = TRUE))

#Plot proportion of contact time dedicated to each session topic by region (columns ordered by total contact time)
ggplot(GroupSumContact, aes(x = reorder(Region, -sum), y = sum, fill = as.factor(Contact.Initiator)))+
  geom_bar(stat = "identity", position = "fill")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Hub", y="Proportion of Contact Time", fill = "Session Initiator")

ggsave(here("figures", "Prop TA Contact Time by Contact Init & Hub.jpeg"), dpi=600)

GroupSumContactMethod <- TA_data%>%
  group_by(Region, Modality)%>%
  summarize(sum = sum(ContactTime_Numeric, na.rm = TRUE))

#Plot proportion of contact time dedicated to each session topic by region (columns ordered by total contact time)
ggplot(GroupSumContactMethod, aes(x = reorder(Region, -sum), y = sum, fill = as.factor(Modality)))+
  geom_bar(stat = "identity", position = "fill")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Hub", y="Proportion of Contact Time", fill = "Session Modality")

ggsave(here("figures", "Prop TA Contact Time by Contact Modality & Hub.jpeg"), dpi=600)

