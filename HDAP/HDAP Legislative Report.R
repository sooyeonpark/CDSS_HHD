library(plyr)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(xlsx)
library(lubridate)

hdap_leg = hdap

#Converts the "Exit Date", "Project Start Date", and "Date of Birth" columns from a character or factor type to a Date type.
hdap_leg$`Exit Date` <- as.Date(hdap_leg$`Exit Date`, format = "%Y-%m-%d")
hdap_leg$`Project Start Date` <- as.Date(hdap_leg$`Project Start Date`, format = "%Y-%m-%d")
hdap_leg$`Date of Birth` <- as.Date(hdap_leg$`Date of Birth`, format = "%Y-%m-%d")
hdap_leg$`Housing Stabilized/Retained Date` <- as.Date(hdap_leg$`Housing Stabilized/Retained Date`, format = "%Y-%m-%d")
hdap_leg$Age <- time_length(difftime(hdap_leg$`Project Start Date`, hdap_leg$`Date of Birth`), "years")

hdap_leg$enrolled_fy <- ifelse(hdap_leg$`Project Start Date` > "2017-11-30" & hdap_leg$`Project Start Date` <= "2018-06-30", "2017-18", 
                           ifelse(hdap_leg$`Project Start Date` > "2018-06-30" & hdap_leg$`Project Start Date` <= "2019-06-30", "2018-19", 
                                  ifelse(hdap_leg$`Project Start Date` > "2019-06-30" & hdap_leg$`Project Start Date` <= "2020-06-30", "2019-20", 
                                         ifelse(hdap_leg$`Project Start Date` > "2020-06-30" & hdap_leg$`Project Start Date` <= "2021-06-30", "2020-21", 
                                                ifelse(hdap_leg$`Project Start Date` > "2021-06-30" & hdap_leg$`Project Start Date` <= "2022-06-30", "2021-22", 
                                                       ifelse(hdap_leg$`Project Start Date` > "2022-06-30" & hdap_leg$`Project Start Date` <= "2023-06-30", "2022-23", 
                                                              ifelse(hdap_leg$`Project Start Date` > "2023-06-30" & hdap_leg$`Project Start Date` <= "2024-06-30", "2023-24", NA)))))))

hdap_leg$exited_fy <- ifelse(hdap_leg$`Exit Date` > "2017-11-30" & hdap_leg$`Exit Date` <= "2018-06-30", "2017-18", 
                         ifelse(hdap_leg$`Exit Date` > "2018-06-30" & hdap_leg$`Exit Date` <= "2019-06-30", "2018-19", 
                                ifelse(hdap_leg$`Exit Date` > "2019-06-30" & hdap_leg$`Exit Date` <= "2020-06-30", "2019-20", 
                                       ifelse(hdap_leg$`Exit Date` > "2020-06-30" & hdap_leg$`Exit Date` <= "2021-06-30", "2020-21", 
                                              ifelse(hdap_leg$`Exit Date` > "2021-06-30" & hdap_leg$`Exit Date` <= "2022-06-30", "2021-22", 
                                                     ifelse(hdap_leg$`Exit Date` > "2022-06-30" & hdap_leg$`Exit Date` <= "2023-06-30", "2022-23", 
                                                            ifelse(hdap_leg$`Exit Date` > "2023-06-30" & hdap_leg$`Exit Date` <= "2024-06-30", "2023-24", NA)))))))

#Coding NEW exit destination buckets
perm_housing <- c("Permanent Housing (except RRH) for Formerly Homeless Persons", 
                  "Rental without Housing Subsidy", "Owner without Housing Subsidy", 
                  "Rental with VASH", "Rental with Other Housing Subsidy", 
                  "Owner with Housing Subsidy", "Living with Family Permanently",
                  "Living with Friend Permanently", "Rental with GPD TIP", 
                  "Rental with RRH/Equivalent Subsidy", "Retained Housing")
temp_housing <- c("Transitional Housing", "Living with Family Temporarily", 
                  "Living with Friend Temporarily", 
                  "Hotel/Motel Paid without Emergency Shelter Voucher", 
                  "Residential Project/Halfway House with No Homeless Criteria")
deceased_other <- c("Other", "Deceased")
homeless <- c("Emergency Shelter", "Place Not Meant for Habitation", "Safe Heaven")
unknown <- c("Client Does Not Know", "Client Refused", "No Exit Interview Completed")
institutions <- c("Psychiatric Hospital/Facility", "Substance Abuse Treatment Facility/Detox Center", 
                  "Hospital/Residential Medical Facility", "Jail/Prison/Juvenile Facility",
                  "Foster Care/Group Home", "Long-Term Care Facility/Nursing Home")


#Add data for NEW exit categories
hdap_leg$exit_status_new <- as.factor(ifelse(hdap_leg$Destination %in% perm_housing, "Permanent Housing", 
                                         ifelse(hdap_leg$Destination %in% temp_housing, "Temporary Housing",
                                                ifelse(hdap_leg$Destination %in% deceased_other, "Other",
                                                       ifelse(hdap_leg$Destination %in% homeless, "Homelessness",
                                                              ifelse(hdap_leg$Destination %in% unknown, "Unknown",
                                                                     ifelse(hdap_leg$Destination %in% institutions, "Institutions", NA)))))))


#Categorize age groups of participants
hdap_leg$`Age Group` <- as.factor(ifelse(hdap_leg$Age < 18, '17 or younger', 
                                     ifelse(hdap_leg$Age >= 18 & hdap_leg$Age < 25, '18-24', 
                                            ifelse(hdap_leg$Age >= 25 & hdap_leg$Age < 30, '25-29', 
                                                   ifelse(hdap_leg$Age >= 30 & hdap_leg$Age < 35, '30-34', 
                                                           ifelse(hdap_leg$Age >= 35 & hdap_leg$Age < 40, '35-39', 
                                                                   ifelse(hdap_leg$Age >= 40 & hdap_leg$Age < 45, '40-44', 
                                                                           ifelse(hdap_leg$Age >= 45 & hdap_leg$Age < 50, '45-49', 
                                                                                   ifelse(hdap_leg$Age >= 50 & hdap_leg$Age < 55, '50-54', 
                                                                                           ifelse(hdap_leg$Age >= 55 & hdap_leg$Age < 60, '55-59', 
                                                                                                   ifelse(hdap_leg$Age >= 60 & hdap_leg$Age < 65, '60-64', 
                                                                                                           ifelse(hdap_leg$Age >= 65 & hdap_leg$Age < 70, '65-69', 
                                                                                                                   ifelse(hdap_leg$Age >= 70, '70 or older', 
                                                                                                                     "Not reported")))))))))))))

###################################################
##At risk of experiencing homelessness population##
###################################################
                              
hdap_atrisk <- hdap_leg %>% filter(`At risk of experiencing homelessness` == "Yes" & 
                                 enrolled_fy == "2023-24") 

##HDAP Specified Population
hdap_atrisk_hdappop <- hdap_atrisk %>% select(contains("HDAP Target Population")) %>% 
  summarise_at(vars(`HDAP Target Population - GA/GR`:`HDAP Target Population - Other Low/No Income`), 
               ~ sum(. == "Yes")) %>% pivot_longer(cols = starts_with("HDAP"), 
                                                   names_to = "HDAP Target Population", 
                                                   values_to = "Total") %>% 
  mutate(percentage = round((Total / nrow(hdap_atrisk)) * 100, 0))

##Race
hdap_atrisk_race <- hdap_atrisk %>% count(Race) %>% mutate(race_prop = round((n / sum(n)) * 100, 0))

##Ethnicity
hdap_atrisk_eth <- hdap_atrisk %>% count(Ethnicity) %>% mutate(eth_prop = round((n / sum(n)) * 100, 0))

##Age
hdap_atrisk_age <- hdap_atrisk %>% count(`Age Group`) %>% mutate(percentage = round((n / sum(n)) * 100, 0))

##Gender
hdap_atrisk_gender <- hdap_atrisk %>% count(`Gender Identity`) %>% mutate(percentage = round((n / sum(n)) * 100, 0))

########################################
##Experiencing homelessness population##
########################################

hdap_experiencing <- hdap_leg %>% filter(`Experiencing homelessness` == "Yes" & 
                                       enrolled_fy == "2023-24") 

##HDAP Specified Population
hdap_experiencing_hdappop <- hdap_experiencing %>% select(contains("HDAP Target Population")) %>% 
  summarise_at(vars(`HDAP Target Population - GA/GR`:`HDAP Target Population - Other Low/No Income`), 
               ~ sum(. == "Yes", na.rm = TRUE)) %>% 
  pivot_longer(cols = starts_with("HDAP"), names_to = "HDAP Target Population", 
               values_to = "Total") %>% 
  mutate(percentage = round((Total / nrow(hdap_experiencing)) * 100, 0))

##Race
hdap_experiencing_race <- hdap_experiencing %>% count(Race) %>% mutate(race_prop = round((n / sum(n)) * 100, 0))

#Ethnicity
hdap_experiencing_eth <- hdap_experiencing %>% count(Ethnicity) %>% mutate(race_prop = round((n / sum(n)) * 100, 0))

##Age
hdap_experiencing_age <- hdap_experiencing %>% count(`Age Group`) %>% mutate(percentage = round((n / sum(n)) * 100, 0))

##Gender
hdap_experiencing_gender <- hdap_experiencing %>% count(`Gender Identity`) %>% mutate(percentage = round((n / sum(n)) * 100, 0))

###################################
##Chronically Homeless population##
###################################

hdap_chronically <- hdap_leg %>% filter(`Chronically Homeless` == "Yes" & 
                                      enrolled_fy == "2023-24") 

hdap_chronically_hdappop <- hdap_chronically %>% select(contains("HDAP Target Population")) %>% 
  summarise_at(vars(`HDAP Target Population - GA/GR`:`HDAP Target Population - Other Low/No Income`), 
               ~ sum(. == "Yes", na.rm = TRUE)) %>% pivot_longer(cols = starts_with("HDAP"), 
                                                   names_to = "HDAP Target Population", 
                                                   values_to = "Total") %>% 
  mutate(percentage = round((Total / nrow(hdap_chronically)) * 100, 0))

##Race
hdap_chronically_race <- hdap_chronically %>% count(Race) %>% mutate(race_prop = round((n / sum(n)) * 100, 0))

#Ethnicity
hdap_chronically_eth <- hdap_chronically %>% count(Ethnicity) %>% mutate(race_prop = round((n / sum(n)) * 100, 0))

##Age
hdap_chronically_age <- hdap_chronically %>% count(`Age Group`) %>% mutate(percentage = round((n / sum(n)) * 100, 0))

##Gender
hdap_chronically_gender <- hdap_chronically %>% count(`Gender Identity`) %>% mutate(percentage = round((n / sum(n)) * 100, 0))

##% of enrollees in homelessness status at entry

hdap_homeless_contradiction <- hdap_leg %>% filter(`At risk of experiencing homelessness` == "Yes" & 
                                                   `Experiencing homelessness` == "Yes")

hdap_homeless <- anti_join(hdap_leg, hdap_homeless_contradiction)

hdap_homeless_prop <- hdap_homeless %>% 
  select(c(enrolled_fy, `At risk of experiencing homelessness`, `Experiencing homelessness`, 
           `Chronically Homeless`)) %>% group_by(enrolled_fy) %>% 
  summarise_at(vars(`At risk of experiencing homelessness`:`Chronically Homeless`), 
               ~ sum(. == "Yes", na.rm = TRUE))

hdap_homeless_prop_all <- hdap_leg %>% 
  select(c(enrolled_fy, `At risk of experiencing homelessness`, `Experiencing homelessness`, 
           `Chronically Homeless`)) %>% group_by(enrolled_fy) %>% 
  summarise_at(vars(`At risk of experiencing homelessness`:`Chronically Homeless`), 
               ~ sum(. == "Yes", na.rm = TRUE))

new_hdap <- hdap_leg %>% subset(`At risk of experiencing homelessness` = "Yes" & 
                   hdap_leg$`Experiencing homelessness` == "Yes")

######################################################################
##Table 1. HDAP Overall Program Participant Demographics in FY 23-24##
######################################################################

hdap_overall <- hdap_leg %>% filter(!is.na(enrolled_fy))

##HDAP Specified Population
hdap_hdappop_overall <- hdap_overall %>% select(contains("HDAP Target Population")) %>% 
  summarise_at(vars(`HDAP Target Population - GA/GR`:`HDAP Target Population - Other Low/No Income`), 
               ~ sum(. == "Yes", na.rm = TRUE)) %>% 
  pivot_longer(cols = starts_with("HDAP"), names_to = "HDAP Target Population", 
               values_to = "Total") %>% 
  mutate(percentage = round((Total / nrow(hdap_overall)) * 100, 0))

#Race
hdap_race_overall <- hdap_overall %>% count(Race) %>% mutate(race_prop = round((n / sum(n)) * 100, 0))

#Ethnicity
hdap_eth_overall <- hdap_overall %>% count(Ethnicity) %>% mutate(race_prop = round((n / sum(n)) * 100, 0))

#Age
hdap_age_overall <- hdap_leg %>% filter(!is.na(enrolled_fy)) %>% 
  count(`Age Group`) %>% mutate(percentage = round((n / sum(n)) * 100, 0))

##Gender
hdap_gender_overall <- hdap_overall %>% count(`Gender Identity`) %>% mutate(percentage = round((n / sum(n)) * 100, 0))


########################################################################
##2. Number of HDAP Participants Served by Population Types in Statute##
########################################################################

hdap_pop_fy <- hdap %>% group_by(enrolled_fy) %>% 
  select(c(enrolled_fy, contains("HDAP Target Population"))) %>% 
  summarise_at(vars(`HDAP Target Population - GA/GR`:`HDAP Target Population - Other Low/No Income`), 
               ~ sum(. == "Yes", na.rm = TRUE)) %>% t()


##########################################
##3. Number of HDAP Participants by Race##
##########################################

hdap_leg$Race <- hdap_leg$Race %>% replace_na("Client Refused")

hdap_race_fy <- hdap %>% group_by(enrolled_fy) %>% count(Race) %>% 
  mutate(percentage = round((n / sum(n)) * 100, 0))

View(hdap_race_overall)


###############################################
##4. Number of HDAP Participants by Ethnicity##
###############################################

hdap_leg$Ethnicity <- hdap_leg$Ethnicity %>% replace_na("Client Refused")

hdap_eth_fy <- hdap %>% group_by(enrolled_fy) %>% count(Ethnicity) %>% 
  mutate(percentage = round((n / sum(n)) * 100, 0))

View(hdap_eth_overall)


#########################################
##5. Number of HDAP Participants by Age##
#########################################

hdap_age_fy <- hdap %>% group_by(enrolled_fy) %>% count(`Age Group`) %>% 
  mutate(percentage = round((n / sum(n)) * 100, 0))


View(hdap_age_overall)


#####################################################
##6. Number of HDAP Participants by Gender Identity##
#####################################################

hdap_leg$`Gender Identity` <- hdap_leg$`Gender Identity` %>% replace_na("Client Refused")

hdap_gender_fy <- hdap %>% group_by(enrolled_fy) %>% count(`Gender Identity`) %>% 
  mutate(percentage = round((n / sum(n)) * 100, 0))


####################################################
##7. Number of HDAP Participants by Housing Status##
####################################################

hdap_leg$temp_movein <- ifelse(hdap_leg$`Housing Move-In Date - Temporary Housing` > "2017-11-30" & 
                             hdap_leg$`Housing Move-In Date - Temporary Housing` <= "2018-06-30", "2017-18", 
                           ifelse(hdap_leg$`Housing Move-In Date - Temporary Housing` > "2018-06-30" & 
                                     hdap_leg$`Housing Move-In Date - Temporary Housing` <= "2019-06-30", "2018-19", 
                                  ifelse(hdap_leg$`Housing Move-In Date - Temporary Housing` > "2019-06-30" & 
                                           hdap_leg$`Housing Move-In Date - Temporary Housing` <= "2020-06-30", "2019-20", 
                                         ifelse(hdap_leg$`Housing Move-In Date - Temporary Housing` > "2020-06-30" & 
                                                 hdap_leg$`Housing Move-In Date - Temporary Housing` <= "2021-06-30", "2020-21", 
                                               ifelse(hdap_leg$`Housing Move-In Date - Temporary Housing` > "2021-06-30" & 
                                                        hdap_leg$`Housing Move-In Date - Temporary Housing` <= "2022-06-30", "2021-22", 
                                                      ifelse(hdap_leg$`Housing Move-In Date - Temporary Housing` > "2022-06-30" & 
                                                               hdap_leg$`Housing Move-In Date - Temporary Housing` <= "2023-06-30", "2022-23", 
                                                             ifelse(hdap_leg$`Housing Move-In Date - Temporary Housing` > "2023-06-30" & 
                                                                      hdap_leg$`Housing Move-In Date - Temporary Housing` <= "2024-06-30", "2023-24", 
                                                                    ifelse(hdap_leg$`Housing Move-In Date - Temporary Housing`<= "2017-11-30" | 
                                                                             hdap_leg$`Housing Move-In Date - Temporary Housing` > "2024-06-30", "Non-Implementation", NA))))))))

hdap_leg$perm_movein <- ifelse(hdap_leg$`Housing Move-In Date - Permanent Housing` > "2017-11-30" & 
                             hdap_leg$`Housing Move-In Date - Permanent Housing` <= "2018-06-30", "2017-18", 
                           ifelse(hdap_leg$`Housing Move-In Date - Permanent Housing` > "2018-06-30" & 
                                    hdap_leg$`Housing Move-In Date - Permanent Housing` <= "2019-06-30", "2018-19", 
                                   ifelse(hdap_leg$`Housing Move-In Date - Permanent Housing` > "2019-06-30" & 
                                            hdap_leg$`Housing Move-In Date - Permanent Housing` <= "2020-06-30", "2019-20", 
                                          ifelse(hdap_leg$`Housing Move-In Date - Permanent Housing` > "2020-06-30" & 
                                                   hdap_leg$`Housing Move-In Date - Permanent Housing` <= "2021-06-30", "2020-21", 
                                                 ifelse(hdap_leg$`Housing Move-In Date - Permanent Housing` > "2021-06-30" & 
                                                          hdap_leg$`Housing Move-In Date - Permanent Housing` <= "2022-06-30", "2021-22", 
                                                        ifelse(hdap_leg$`Housing Move-In Date - Permanent Housing` > "2022-06-30" & 
                                                                 hdap_leg$`Housing Move-In Date - Permanent Housing` <= "2023-06-30", "2022-23", 
                                                               ifelse(hdap_leg$`Housing Move-In Date - Permanent Housing` > "2023-06-30" & 
                                                                        hdap_leg$`Housing Move-In Date - Permanent Housing` <= "2024-06-30", "2023-24", 
                                                                      ifelse(hdap_leg$`Housing Move-In Date - Permanent Housing` <= "2017-11-30" & 
                                                                               hdap_leg$`Housing Move-In Date - Permanent Housing` > "2024-06-30", "Non-Implementation", NA))))))))

hdap_perm_movein_only <- hdap %>% filter(is.na(`Housing Move-In Date - Temporary Housing`) & 
                                           !is.na(`Housing Move-In Date - Permanent Housing`)) %>% 
  count(perm_movein)

hdap_temp_movein_only <- hdap %>% filter(!is.na(`Housing Move-In Date - Temporary Housing`) & 
                                           is.na(`Housing Move-In Date - Permanent Housing`)) %>% 
  count(temp_movein) %>% mutate(percentage = round((n / sum(n)) * 100, 0))

hdap_movein_both <- hdap %>% filter(!is.na(`Housing Move-In Date - Temporary Housing`) & 
                                      !is.na(`Housing Move-In Date - Permanent Housing`)) %>% 
  select(c(perm_movein, temp_movein)) ##Add percentage column

#############################################################
##10. Number of HDAP Participants by Housing Status at Exit##
#############################################################

hdap_exits <- hdap %>% filter(!is.na(exited_fy) & !is.na(exit_status_new) & 
                                County != "Los Angeles") %>% group_by(exited_fy) %>% 
  count(exit_status_new) %>% mutate(percentage = round((n / sum(n)) * 100, 0))

hdap_exits_overall <- hdap %>% filter(!is.na(exited_fy) & !is.na(exit_status_new) & 
                                        County != "Los Angeles") %>% 
  count(exit_status_new) %>% mutate(percentage = round((n / sum(n)) * 100, 0))

hdap_exits_total_fy <- hdap %>% filter(!is.na(exited_fy) & !is.na(exit_status_new) & 
                                         County != "Los Angeles") %>% 
  count(exited_fy)

rm(hdap_leg, perm_housing,temp_housing,deceased_other,homeless,unknown,institutions)