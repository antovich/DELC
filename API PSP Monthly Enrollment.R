#Creating cleaning report for PSP Monthly Enrollment Report
#Two ideas to work on - GUI and saving directly to Share Point

#Load packages
library(jsonlite)
library(httr)
library(tidyverse)
library(here)
library(writexl)
library(openxlsx)

#For filtering by time window - will be inclusive of the listed dates
#Date of most recent data cleaning pull (YYYY-MM-DD)
StartDate <- as.Date("2022-12-08")
#Current data or end of time window
EndDate <- as.Date("2023-01-08")

#For testing only; DO NOT RUN
data <- read.xlsx(here("Monthly Report 22-23_01-08-23.xlsx"), check.names = FALSE, sep.names = " ", detectDates = TRUE)%>%
  mutate(Created = convertToDateTime(Created),
         GranteeSiteOverviewPairs = paste(sub("_.*", "", `Grantee Name`),
                                          `License (linking to tracker)`, sep = "__"))%>%
  mutate(Created_Date = as.POSIXct(.$Created, tz = "UTC", format='%Y-%m-%dT%H:%M:%SZ')%>%format(.,tz="America/Los_Angeles", format = '%Y-%m-%d'))

#Month Reporting
#For filtering by month
MonthReport <- "Jun. 2023"

#SmartSheet ID for "Monthly Report 22-23" 
#(located under file -> properties -> Sheet ID in SmartSheet)
SheetID <- '1917713787447172'

#API access token 
#(generate under account -> personal settings -> API Access -> Generate new access token)
#note that this appears to be a general token for all sheets a user can access
AccessToken <- 'rMpwW5WoaHhTcV58Wh5qRriZByEElOFSBRSYL'

#API request for SmartSheet data - Set Sheet ID above
SmartSheet <- GET(paste0("https://api.smartsheet.com/2.0/sheets/",SheetID),
    #Access token set above
    add_headers(Authorization = paste0("Bearer ", AccessToken)))

#Get the SmartSheet content, convert from raw to char format and convert from json to nested lists
API_file = fromJSON(rawToChar(SmartSheet$content))

# #Combine data rows using value displayed in SmartSheet into a single vector
# data <- bind_rows(API_file[["rows"]][["cells"]])$displayValue %>%
  
# #Combine data rows using value directly from SmartSheet into a single vector
data <- bind_rows(API_file[["rows"]][["cells"]])$value %>%
  #convert vector of rows into a matrix with nrow = number of columns in SmartSheet data
  matrix(ncol = length(API_file[["columns"]]$id), byrow = TRUE)%>%
  #convert to dataframe
  data.frame()%>%
  #rename columns using SmartSheet column titles
  rename_with(.cols = c(1:length(API_file[["columns"]]$id)), ~API_file[["columns"]]$title)%>%
  #change data type based on SmartSheet data type designation
  #mutate_at(which(API_file[["columns"]]$type == "TEXT_NUMBER"), as.numeric)%>%
  mutate_at(which(API_file[["columns"]]$type == "PICKLIST"), as.factor)%>%
  #add column for matching site and grantee
  mutate(GranteeSiteOverviewPairs = paste(sub("_.*", "", `Grantee Name`),
                                          `License (linking to tracker)`, sep = "__"))

# #make column names valid (no spaces/special characters)
# data_rename <- data %>% rename_with(make.names)
# #Compare data imported csv downloaded from SmartSheets and API downloaded data
# all_equal(data_rename, data_csv)

#SmartSheet ID for "Monthly Report 22-23" 
#(located under file -> properties -> Sheet ID in SmartSheet)
SheetID_GranteeSiteOverview <- '2031819819378564'

#API request for SmartSheet data - Set Sheet ID above
SmartSheet_GranteeSiteOverview  <- GET(paste0("https://api.smartsheet.com/2.0/sheets/",SheetID_GranteeSiteOverview),
                  #Access token set above
                  add_headers(Authorization = paste0("Bearer ", AccessToken)))

#Get the SmartSheet content, convert from raw to char format and convert from json to nested lists
API_file_GranteeSiteOverview  = fromJSON(rawToChar(SmartSheet_GranteeSiteOverview$content))

# #Combine data rows using value displayed in SmartSheet into a single vector
# data <- bind_rows(API_file_GranteeSiteOverview[["rows"]][["cells"]])$displayValue %>%

# #Combine data rows using value directly from SmartSheet into a single vector
data_GranteeSiteOverview  <- bind_rows(API_file_GranteeSiteOverview[["rows"]][["cells"]])$value %>%
  #convert vector of rows into a matrix with nrow = number of columns in SmartSheet data
  matrix(ncol = length(API_file_GranteeSiteOverview[["columns"]]$id), byrow = TRUE)%>%
  #convert to dataframe
  data.frame()%>%
  #rename columns using SmartSheet column titles
  rename_with(.cols = c(1:length(API_file_GranteeSiteOverview[["columns"]]$id)), ~API_file_GranteeSiteOverview[["columns"]]$title)%>%
  #change data type based on SmartSheet data type designation
  mutate_at(which(API_file_GranteeSiteOverview[["columns"]]$type == "PICKLIST"), as.factor)

#Create variable that combines grantee ID and site license 
#from Grantee Site Overview sheet to match with same info from Monthly Enrollment sheet
GranteeSiteOverviewMatches <- unique(paste(sub("_.*", "", data_GranteeSiteOverview$`Grantee Name`),
                                           data_GranteeSiteOverview$`License ID / Site ID`, sep = "__"))

data_ToBeCleaned <- data %>%
  #Add cleaning vars
  mutate(
    #Create row label to align with Smart Sheet
    `Smart Sheet Row` = 1:nrow(.),
    #Get date from Created column
    #API from Smart Sheet formats at UTC, instead of local time (though local is displayed in Smart Sheet)
    #So use POSIXct to note format and convert to local time; could recover time if needed but just pulling date for now
    Created_Date = as.POSIXct(.$Created, tz = "UTC", format='%Y-%m-%dT%H:%M:%SZ')%>%format(.,tz="America/Los_Angeles", format = '%Y-%m-%d'),
    #Check for duplicates in "Month Reporting" and "License (linking to tracker)" vars
    #True = duplicate, False = not duplicated
    Duplicates = 
      duplicated(paste0(data$`License (linking to tracker)`, data$`Month Reporting`))|
      #mark original as duplicate as well
      duplicated(paste0(data$`License (linking to tracker)`, data$`Month Reporting`), fromLast = TRUE),
    #This matches pairs of licenses/PSP IDs from the grantee site overview sheet to the enrollment sheet
    #to ensure that these pairings are correct 
    #True = Mismatch, False = Matched
    GranteeSiteMismatch = 
      case_when(GranteeSiteOverviewPairs %in% GranteeSiteOverviewMatches ~ FALSE,
                #When the above isn't met, set value to TRUE
                TRUE ~ TRUE),
    #Check whether there are negative numbers reported in selected columns
    #True = negative number in one or more of selected columns, False = no negative numbers in those columns
    #May produce warning NAs due to coercion if non-numeric response is provided
    NegNumbers =
      case_when(as.numeric(`Site PSP Slots | Espacios del sitio de PSP`) < 0 ~ TRUE, 
                as.numeric(`Children Enrolled | Niños inscritos`) < 0 ~ TRUE,
                as.numeric(`ADA`) < 0 ~ TRUE,
                as.numeric(`Monthly Hours`) < 0 ~ TRUE,
                as.numeric(`# of PSP Vacancies over 30 days`) < 0 ~ TRUE,
                #When the above criteria isn't met, set value to TRUE
                TRUE ~ FALSE),
    #Check whether the numeric columns above have non-numeric entries
    #had to use regex to detect allowable number formats in strings
    NonNumeric = 
      case_when(str_detect(`Site PSP Slots | Espacios del sitio de PSP`, "^-?\\d*(\\.\\d+)?$") == FALSE | 
                str_detect(`Children Enrolled | Niños inscritos`, "^-?\\d*(\\.\\d+)?$") == FALSE |
                str_detect(`ADA`, "^-?\\d*(\\.\\d+)?$") == FALSE |
                str_detect(`Monthly Hours`, "^-?\\d*(\\.\\d+)?$") == FALSE |
                str_detect(`# of PSP Vacancies over 30 days`, "^-?\\d*(\\.\\d+)?$") == FALSE ~ TRUE,
                #When the above criteria isn't met, set value to TRUE
                TRUE ~ FALSE),
    
    #Checks whether internal slot count matches reported slot count
    #True = mismatch between internal and reported slot numbers, False = the internal and external numbers are the same
    #May produce warning NAs due to coercion if non-numeric response is provided (e.g., #NO MATCH for Slots (internal))
    SlotDiscrepancy = 
      case_when(as.numeric(`Slots (internal)`) != as.numeric(`Site PSP Slots | Espacios del sitio de PSP`) ~ TRUE,
                TRUE ~ FALSE),
    #Checks whether monthly hours are below 50
    #True = less than 50 hours, False = 50 hours or more or non numeric
    MonthlyUnder50 = 
      case_when(as.numeric(`Monthly Hours`) < 50 ~ TRUE,
                TRUE ~ FALSE),
    #Checks whether % enrollment falls below three thresholds (25, 50, 75%) 
    #will produce warning- NAs introduced by coercion 
    #because of the "DIVIDE BY ZERO" entries, which can't be converted to numeric
    UnderEnroll = 
      case_when(as.numeric(`% Enrollment (internal)`) < 75 & as.numeric(`% Enrollment (internal)`) >= 50  ~ "<75%",
                as.numeric(`% Enrollment (internal)`) < 50 & as.numeric(`% Enrollment (internal)`) >= 25~ "<50%",
                as.numeric(`% Enrollment (internal)`) < 25 ~ "<25%",
                as.numeric(`% Enrollment (internal)`) >= 75 ~ "75% or greater"),
    
    #Checks whether a vacancy was reported
    #True = vacancy reported (si/yes), False = no vacancy reported, 
    Vacancy = 
      case_when(`Vacancies` == "Yes/Si" ~ TRUE,
                TRUE ~ FALSE)
)

data_ToBeCleaned_Window <- data_ToBeCleaned %>%
  #Filter by date row was created using StartDate and EndData variables above
  filter(Created_Date >= StartDate & Created_Date <= EndDate)

#Only for testing use; DO NOT RUN
TestData_Window <- filter(data_ToBeCleaned, Created_Date > StartDate & Created_Date <= EndDate)%>%
  mutate(`Tally of Follow-ups Needed` = rowSums(select(.,Duplicates, GranteeSiteMismatch, NegNumbers, NonNumeric, SlotDiscrepancy)))%>%
  select(`Grantee Name`, `Site`,`Month Reporting`, "Date Created" = Created_Date, `Smart Sheet Row`, "Preferred Language" = `Preferred Language (internal)`, 
         "Duplicate Reports" = Duplicates, "Incorrect Grantee/Site Combo" = GranteeSiteMismatch,
         "Unexpected Negative Numbers" = NegNumbers,"Unexpected Non-Numeric" = NonNumeric,"Site Slot Discrepancy" = SlotDiscrepancy,
         `Tally of Follow-ups Needed`)
write_xlsx(TestData_Window, here(paste0("PSP Monthly Enrollment Report - Data Cleaning TEST COMPARISON ",format(Sys.Date(), "%m-%d-%Y"),".xlsx")))


data_ToBeCleaned_Month <- data_ToBeCleaned %>%
  filter(`Month Reporting` == MonthReport)


#Output
GranteeDataCleaningReport <- data_ToBeCleaned_Month %>%
  mutate(`Tally of Follow-ups Needed` = rowSums(select(.,Duplicates, GranteeSiteMismatch, NegNumbers, NonNumeric, SlotDiscrepancy)))%>%
  select(`Grantee Name`, `Site`,`Month Reporting`, "Date Created" = Created_Date, `Smart Sheet Row`, "Preferred Language" = `Preferred Language (internal)`, 
         "Duplicate Reports" = Duplicates, "Incorrect Grantee/Site Combo" = GranteeSiteMismatch,
         "Unexpected Negative Numbers" = NegNumbers,"Unexpected Non-Numeric" = NonNumeric,"Site Slot Discrepancy" = SlotDiscrepancy,
         `Tally of Follow-ups Needed`)

#Write to Excel file including current date
write_xlsx(GranteeDataCleaningReport, here(paste0("PSP Monthly Enrollment Report - Data Cleaning ",MonthReport," ",format(Sys.Date(), "%m-%d-%Y"),".xlsx")))

#Filter to group with less than 50 hours in the month
#note that this is for month-specific data
PSPReview_MonthLess50 <- data_ToBeCleaned_Month %>%
  filter(MonthlyUnder50)%>%
  select(`Grantee Name`, Site)

#Create header for this specific report
Header1 <- data.frame(`Month` = c(MonthReport), 
              `Total Number of Submissions` = c(nrow(data_ToBeCleaned_Month)), 
              `Number of Submissions Flagged` = c(nrow(PSPReview_MonthLess50)),
              check.names = FALSE)
#Create workbook for this report
WB1 <- createWorkbook()
SH1 <- addWorksheet(WB1, paste("Report", substr(MonthReport, 1,3)))
#place header in sheet
writeData(WB1, SH1, x = Header1)
#place data in sheet, a few rows down
writeData(WB1, SH1, x = PSPReview_MonthLess50, startRow = 4)
saveWorkbook(WB1, here(paste0("PSP Monthly Enrollment Report - Under 50 Hrs ",MonthReport," ",format(Sys.Date(), "%m-%d-%Y"),".xlsx")),
             overwrite = TRUE)

#Separate out by % underenrollment
PSPReview_PctEnroll25 <- data_ToBeCleaned_Month %>%
  filter(UnderEnroll == "<25%")%>%
  select(`Grantee Name`, Site)

PSPReview_PctEnroll50 <- data_ToBeCleaned_Month %>%
  filter(UnderEnroll == "<50%")%>%
  select(`Grantee Name`, Site)

PSPReview_PctEnroll75 <- data_ToBeCleaned_Month %>%
  filter(UnderEnroll == "<75%")%>%
  select(`Grantee Name`, Site)

#Create header for this specific report
Header2 <- data.frame(`Month` = c(MonthReport), 
                      `Total Number of Submissions` = c(nrow(data_ToBeCleaned_Month)),
                      check.names = FALSE)
#Create workbook for this report
WB2 <- createWorkbook()
SH2 <- addWorksheet(WB2, paste("Report", substr(MonthReport, 1,3)))
#place header in sheet
writeData(WB2, SH2, x = Header2)
writeData(WB2, SH2, x = paste("Number of Submissions Flagged - Under 25:", nrow(PSPReview_PctEnroll25)), startRow = 4)
#place data in sheet, a few rows down
writeData(WB2, SH2, x = PSPReview_PctEnroll25, startRow = 5)

writeData(WB2, SH2, x = paste("Number of Submissions Flagged - Under 50:", nrow(PSPReview_PctEnroll50)), startRow = 5+nrow(PSPReview_PctEnroll25)+2)
#place data in sheet, a few rows down
writeData(WB2, SH2, x = PSPReview_PctEnroll50, startRow = 5+nrow(PSPReview_PctEnroll25)+3)

writeData(WB2, SH2, x = paste("Number of Submissions Flagged - Under 75:", nrow(PSPReview_PctEnroll75)), startRow = 5+nrow(PSPReview_PctEnroll25)+nrow(PSPReview_PctEnroll50)+5)
#place data in sheet, a few rows down
writeData(WB2, SH2, x = PSPReview_PctEnroll75, startRow = 5+nrow(PSPReview_PctEnroll25)+nrow(PSPReview_PctEnroll50)+6)


saveWorkbook(WB2, here(paste0("PSP Monthly Enrollment Report - Underenrollment ",MonthReport," ",format(Sys.Date(), "%m-%d-%Y"),".xlsx")),
             overwrite = TRUE)
  
PSPReview_Vacancies <- data_ToBeCleaned_Month %>%
  filter(Vacancies == "Yes/Si")%>%
  select(`Grantee Name`, Site, `# of PSP Vacancies over 30 days`, `Vacancy comments`)
  
Header3 <- data.frame(`Month` = c(MonthReport), 
                      `Total Number of Submissions` = c(nrow(data_ToBeCleaned_Month)),
                      `Number of Submissions Flagged` = c(nrow(PSPReview_Vacancies)),
                      check.names = FALSE)

WB3 <- createWorkbook()
SH3 <- addWorksheet(WB3, paste("Report", substr(MonthReport, 1,3)))
#place header in sheet
writeData(WB3, SH3, x = Header3)
writeData(WB3, SH3, x = PSPReview_Vacancies, startRow = 4)
saveWorkbook(WB3, here(paste0("PSP Monthly Enrollment Report - Vacancies ",MonthReport," ", format(Sys.Date(), "%m-%d-%Y"),".xlsx")),
             overwrite = TRUE)
