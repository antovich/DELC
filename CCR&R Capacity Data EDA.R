#Light cleaning and visualization for capacity data from FCCO

library(tidyverse)
library(here)

#Read in FCCO Active files
#Get CSV file names
FCCO_data <- dir(here('data'),pattern = "*.csv")%>%
  #Filter to just FCCO_Active files
  .[str_detect(.,"FCCO_Active")]%>%
  #Make list named (for use with .id in map_dfr)
  setNames(nm = .)%>%
  #Read in files from list above and concatenate by rows keeping filename as id
  map_dfr(.,~read_csv(here("data",.), name_repair = "universal"), .id = "file_name")%>%
  mutate(Month = str_remove(str_remove(FCCO_data$file_name, "FCCO_Active_"),".csv"))

#Get sum of desired and licensed capacities by region and month
#filtered for active status
CapSum <- FCCO_data%>%
  filter(Status == "Active")%>%
  group_by(Region, Month)%>%
  summarize(DesCap = sum(Desired.Capacity, na.rm = TRUE), LicCap = sum(Licensed.Capacity, na.rm = TRUE),Open = sum(Total.Openings, na.rm = TRUE))
 
#Plot desired capacity by region and month 
ggplot(CapSum, aes(x= factor(Month, levels = c("apr2021","may2021","jun2021","jul2021","aug2021","sep2021","oct2021","nov2021")), 
                   y = DesCap,
                   color = Region))+
  geom_point()+
  geom_line(group = "Region")+
  facet_wrap("Region")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "")+
  labs(x = "Month", y = "Desired Capacity")

ggsave(here("figures","Desired Capacity by Month and Region.jpeg"),dpi = 600, unit="in", width =6, height = 4)

#Plot desired capacity by region and month (free y axis scale)
ggplot(CapSum, aes(x= factor(Month, levels = c("apr2021","may2021","jun2021","jul2021","aug2021","sep2021","oct2021","nov2021")), 
                   y = DesCap,
                   color = Region))+
  geom_point()+
  geom_line(group = "Region")+
  facet_wrap("Region", scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "")+
  labs(x = "Month", y = "Desired Capacity")

ggsave(here("figures","Desired Capacity by Month and Region_free scale.jpeg"),dpi = 600, unit="in", width =10, height = 4)

#Plot desired capacity by region and month (single plot)
ggplot(CapSum, aes(x= factor(Month, levels = c("apr2021","may2021","jun2021","jul2021","aug2021","sep2021","oct2021","nov2021")), 
                   y = DesCap,
                   color = Region,
                   group = Region))+
  geom_point()+
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "")+
  labs(x = "Month", y = "Desired Capacity")

ggsave(here("figures","Desired Capacity by Month and Region_single plot.jpeg"),dpi = 600, unit="in", width =6, height = 4)


#Plot license capacity by region and month 
ggplot(CapSum, aes(x= factor(Month, levels = c("apr2021","may2021","jun2021","jul2021","aug2021","sep2021","oct2021","nov2021")), 
                   y = LicCap,
                   color = Region))+
  geom_point()+
  geom_line(group = "Region")+
  facet_wrap("Region")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "")+
  labs(x = "Month", y = "Licensed Capacity")

ggsave(here("figures","Licensed Capacity by Month and Region.jpeg"),dpi = 600, unit="in", width =6, height = 4)


#Plot openings by region and month 
ggplot(CapSum, aes(x= factor(Month, levels = c("apr2021","may2021","jun2021","jul2021","aug2021","sep2021","oct2021","nov2021")), 
                   y = Open,
                   color = Region))+
  geom_point()+
  geom_line(group = "Region")+
  facet_wrap("Region")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "")+
  labs(x = "Month", y = "Openings")

ggsave(here("figures","Openings by Month and Region.jpeg"),dpi = 600, unit="in", width =6, height = 4)



#Get sum of desired and licensed capacities by provider type, region, and month
#filtered for active status
CapSumProvider <- FCCO_data%>%
  filter(Status == "Active")%>%
  group_by(Region, Month, Provider.Type)%>%
  summarize(DesCap = sum(Desired.Capacity, na.rm = TRUE), LicCap = sum(Licensed.Capacity, na.rm = TRUE),Open = sum(Total.Openings, na.rm = TRUE))

#Plot desired capacity by region, month, and provider type
ggplot(CapSumProvider, 
       aes(x= factor(Month, levels = c("apr2021","may2021","jun2021","jul2021","aug2021","sep2021","oct2021","nov2021")), 
           y = DesCap,
           color = Provider.Type,
           group = Provider.Type))+
  geom_line()+
  geom_point()+
  facet_wrap("Region")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Month", y = "Desired Capacity", fill = "Provider Type")

ggsave(here("figures","Desired Capacity by Month-Region-ProviderType.jpeg"),dpi = 600, unit="in", width =6, height = 4)

#Plot desired capacity by provider type and month (single plot with separate lines by region)
ggplot(CapSumProvider, aes(x= factor(Month, levels = c("apr2021","may2021","jun2021","jul2021","aug2021","sep2021","oct2021","nov2021")), 
                           y = DesCap,
                           color = Provider.Type,
                           group = interaction(Region, Provider.Type)))+
  geom_point()+
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Month", y = "Desired Capacity", color = "Provider Type")

ggsave(here("figures","Desired Capacity by Month and Provider Type_single plot_withRegion.jpeg"),dpi = 600, unit="in", width =6, height = 4)


#Plot desired capacity by region, month, and provider type (free y axis scale)
ggplot(CapSumProvider, 
       aes(x= factor(Month, levels = c("apr2021","may2021","jun2021","jul2021","aug2021","sep2021","oct2021","nov2021")), 
           y = DesCap,
           color = Provider.Type,
           group = Provider.Type))+
  geom_line()+
  geom_point()+
  facet_wrap("Region", scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Month", y = "Desired Capacity", color = "Provider Type")

ggsave(here("figures","Desired Capacity by Month-Region-ProviderType_free scale.jpeg"),dpi = 600, unit="in", width =10, height = 4)
