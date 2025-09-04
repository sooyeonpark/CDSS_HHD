library(readxl)
library(writexl)
library(stringr)
library(sqldf)
library(openxlsx)
source("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/R Scripts/HSAPS Functions.R")

#make sure to regularly update the data set name
hsaps_ab120 = read_xlsx("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/Archive/FY 23-24/Q4/hsaps_cleaned_2024-09-13.xlsx")

#make sure to keep adding the code lines to cover further fiscal years
hsaps_ab120$fy_enrolled = ifelse(hsaps_ab120$`Case Start Date`>"2018-06-30"&hsaps_ab120$`Case Start Date`<="2019-06-30","1819",
                               ifelse(hsaps_ab120$`Case Start Date`>"2019-06-30"&hsaps_ab120$`Case Start Date`<="2020-06-30","1920",
                                      ifelse(hsaps_ab120$`Case Start Date`>"2020-06-30"&hsaps_ab120$`Case Start Date`<="2021-06-30","2021",
                                             ifelse(hsaps_ab120$`Case Start Date`>"2021-06-30"&hsaps_ab120$`Case Start Date`<="2022-06-30","2122",
                                                    ifelse(hsaps_ab120$`Case Start Date`>"2022-06-30"&hsaps_ab120$`Case Start Date`<="2023-06-30","2223",
                                                          ifelse(hsaps_ab120$`Case Start Date`>"2023-06-30"&hsaps_ab120$`Case Start Date`<="2024-06-30","2324",''))))))
hsaps_ab120$fy_exit = ifelse(hsaps_ab120$`Case Closure Date`>"2018-06-30"&hsaps_ab120$`Case Closure Date`<="2019-06-30","1819",
                           ifelse(hsaps_ab120$`Case Closure Date`>"2019-06-30"&hsaps_ab120$`Case Closure Date`<="2020-06-30","1920",
                                  ifelse(hsaps_ab120$`Case Closure Date`>"2020-06-30"&hsaps_ab120$`Case Closure Date`<="2021-06-30","2021",
                                         ifelse(hsaps_ab120$`Case Closure Date`>"2021-06-30"&hsaps_ab120$`Case Closure Date`<="2022-06-30","2122",
                                                ifelse(hsaps_ab120$`Case Closure Date`>"2022-06-30"&hsaps_ab120$`Case Closure Date`<="2023-06-30","2223",
                                                      ifelse(hsaps_ab120$`Case Closure Date`>"2023-06-30"&hsaps_ab120$`Case Closure Date`<="2024-06-30","2324",''))))))

#breakdown of the regions
socal = c("Imperial","Orange","Riverside","San Bernardino","San Diego","Ventura")
sanjoaquin_valley = c("San Joaquin","Stanislaus","Merced","Madera","Fresno","Kern","Kings","Tulare")
bay_area = c("Alameda","Contra Costa","Marin","Napa","San Francisco","San Mateo","Santa Clara","Solano","Sonoma")
sac_area = c("El Dorado","Placer","Sacramento","Sutter","Yolo","Yuba")
central_coast = c("Monterey","San Benito","San Luis Obispo","Santa Barbara","Santa Cruz")
balance = c("Alpine","Amador","Butte","Calaveras","Colusa","Del Norte","Glenn","Humboldt","Inyo","Lake",
            "Lassen","Mariposa","Mendocino","Modoc","Mono","Nevada","Plumas","Shasta","Sierra","Siskiyou",
            "Tehama","Trinity","Tuolumne")
hsaps_ab120$region = ifelse(hsaps_ab120$Reporting_Agency %in% socal,"Southern California",
                          ifelse(hsaps_ab120$Reporting_Agency %in% sanjoaquin_valley,"San Joaquin Valley",
                                 ifelse(hsaps_ab120$Reporting_Agency %in% bay_area,"Bay Area",
                                        ifelse(hsaps_ab120$Reporting_Agency %in% sac_area,"Sacramento Area",
                                               ifelse(hsaps_ab120$Reporting_Agency %in% central_coast,"Central Coast",
                                                      ifelse(hsaps_ab120$Reporting_Agency %in% balance,"Balance of the State",
                                                             ifelse(hsaps_ab120$Reporting_Agency == "Los Angeles","Los Angeles County","")))))))

# Number enrolled by fy
table(hsaps_ab120$fy_enrolled)
table(hsaps_ab120$region,hsaps_ab120$fy_enrolled)
table(hsaps_ab120$Reporting_Agency,hsaps_ab120$fy_enrolled)
#^ YYYY shows the fiscal year; for example, 1819 means FY 18-19

# Specify the file path for approval data to excel sheet
file_path <- "C:/Users/mboyd2/OneDrive - California Department of Social Services (CDSS)/output.xlsx"

table_a <- table(hsaps_ab120$fy_enrolled)

# Write the DataTable to an Excel file. Update data table and sheet name if you do not want to overwrite data on specified file path
write.xlsx(table_a, file = file_path, sheetName = "Sheet1", overwrite = TRUE)

# Number exit by fy and region
table(hsaps_ab120$fy_exit)
table(hsaps_ab120$region,hsaps_ab120$fy_exit)
table(hsaps_ab120$Reporting_Agency,hsaps_ab120$fy_exit)
#^ YYYY shows the fiscal year; for example, 1819 means FY 18-19

##exit type by fy and region
##MB 9/19/24: Updated script from using grep function to count Living Situation at exit to using C function to count exact string values for Living Situation at Exit
#make sure to change the dates to include further fiscal years in the future
hsaps_ab120_exit = hsaps_exit_destination_region(hsaps_ab120,"2018-06-30","2024-06-30")
hsaps_ab120_exit$type = ifelse(hsaps_ab120_exit$exit %in% c("Other", "Deceased", "Living With Family", "Client Entered Into Temp Conservatorship", "Deceased 12/17/2022", "Deceased 06/10/2023"), "Other",
                               ifelse(hsaps_ab120_exit$exit %in% c("Data Not Collected", "Not Exited", "Unknown", "Client Doesn't Know", "7", "No Data Collected", "#", "Unknown - Phone Disconnected Unable To Contact", "0", "Moved Out Of State", "Client Enrolled/Then Declined Services", "Unknown/Client Left Home Moved Into His Car And Whereabouts Unknown", "Linked To Another Program", "Moved To Az", "Hh Transitioned To Hdap Under Robert Johnson", "Email From Aps Confirming They Would Assist Client With Habitability Items.", "Non-Compliant", "Aps Denied", "One Time Assistance Due To Winter Weather.", "Discontnued/No Aps", "No Aps", "Moved To Atwater", "Client Still Residing In Home. Has Not Been Served With Eviction.", " ", "N/A", "Client Discontinued 05/31/2024. Funds Issued Were Thru Arpa And Not Hs"), "Unknown",
                                      ifelse(hsaps_ab120_exit$exit %in% c("Other Permanent Housing", "Rent Leaseholder", "Owner", "Homeowner- With Others No Rent", "Owner Lives Alone", "With Others No Rent", "With Others Rent", "Owner With Others Rent", "Rental Leaseholder", "Rent Leasholder", "Continued At Same Residence", "Other Pemanent Housing", "Still Housed", "Self-Resolved Housing", "Remained At Current Residence", "Rental By Client", "Owned By Client/No Ongoing Housing Subsidy", "Rent/Leaseholder No Subsidy - Moved To Nevada", "Passed Away In June - Rent Leaseholder", "Can Afford Rent On Own", "Remained In Same Housing", "Retained Housing With Ces - Icms", "Permanent-Shared Housing", "Permanent Housing", "Moved Into Affordable Perm Housing", "Secured Affordable Perm. Housing", "Client Secured Housing", "Moved Back To Rental Trailer", "Housed At Yosemite Manor"), "Permanent Housing",
                                             ifelse(hsaps_ab120_exit$exit %in% c("Permanent- Residential Program", "Temporary- Residential Program", "Residential Care Facility", "Skilled Nursing Facility", "Long-Term Care Facility/Nursing Home", "Tempoaray Residential Program", "Entered Long Term Care", "Entered Nursing Home", "Board And Care Facility", "Temporary-Residential Program"), "Instituition",
                                                    ifelse(hsaps_ab120_exit$exit %in% c("Temporary Housing", "Hotel With Rights", "Hotel No Rights", "Interim Housing", "Teamporary Houising", "Temporary Shelter", "Emergency Shelter/Including Hotel/Motel Paid Fwith Emergency Shelter Voucher/Host Home Shelter", "Client Was Moved To Louisiana With The Assistance Of Aps. Hs Program Has Been Discontinued But Motel Payment Was Not Issued Until Q2 Therefore Cm Active On Workbook", "Motel"), "Temporary Housing",
                                                           ifelse(hsaps_ab120_exit$exit %in% c("Homeless", "Homeless Sheltered", "Homeless Unsheltered", "Transitional Housing Fhomeless Persons Including Homeless Youth", "Homeless Working With Calif Fhousing Needs"), "Homeless",
                                                                  "Unknown"))))))
hsaps_ab120_exit_region=aggregate(hsaps_ab120_exit$count,list(hsaps_ab120_exit$region,hsaps_ab120_exit$fy,hsaps_ab120_exit$type),sum)
names(hsaps_ab120_exit_region)=c("region","fy","exit_type","count")
hsaps_ab120_exit_region = hsaps_ab120_exit_region[order(hsaps_ab120_exit_region$region,hsaps_ab120_exit_region$fy),]

# Specify the file path for exit data to excel sheet
file_path <- "C:/Users/mboyd2/OneDrive - California Department of Social Services (CDSS)/output.xlsx"

# Write the DataTable to an Excel file
write.xlsx(hsaps_ab120_exit_region, file = file_path, sheetName = "Sheet1", overwrite = TRUE)

##exit type by fy and county
#make sure to change the dates to include further fiscal years in the future
hsaps_ab120_exit = hsaps_exit_destination_county(hsaps_ab120,"2018-06-30","2024-03-30")
hsaps_ab120_exit$type = ifelse(hsaps_ab120_exit$exit %in% c("Other", "Deceased", "Living With Family", "Client Entered Into Temp Conservatorship", "Deceased 12/17/2022", "Deceased 06/10/2023"), "Other",
                               ifelse(hsaps_ab120_exit$exit %in% c("Data Not Collected", "Not Exited", "Unknown", "Client Doesn't Know", "7", "No Data Collected", "#", "Unknown - Phone Disconnected Unable To Contact", "0", "Moved Out Of State", "Client Enrolled/Then Declined Services", "Unknown/Client Left Home Moved Into His Car And Whereabouts Unknown", "Linked To Another Program", "Moved To Az", "Hh Transitioned To Hdap Under Robert Johnson", "Email From Aps Confirming They Would Assist Client With Habitability Items.", "Non-Compliant", "Aps Denied", "One Time Assistance Due To Winter Weather.", "Discontnued/No Aps", "No Aps", "Moved To Atwater", "Client Still Residing In Home. Has Not Been Served With Eviction.", " ", "N/A"), "Unknown",
                                      ifelse(hsaps_ab120_exit$exit %in% c("Other Permanent Housing", "Rent Leaseholder", "Owner", "Homeowner- With Others No Rent", "Owner Lives Alone", "With Others No Rent", "With Others Rent", "Owner With Others Rent", "Rental Leaseholder", "Rent Leasholder", "Continued At Same Residence", "Other Pemanent Housing", "Still Housed", "Self-Resolved Housing", "Remained At Current Residence", "Rental By Client", "Owned By Client/No Ongoing Housing Subsidy", "Rent/Leaseholder No Subsidy - Moved To Nevada", "Passed Away In June - Rent Leaseholder", "Can Afford Rent On Own", "Remained In Same Housing", "Retained Housing With Ces - Icms", "Permanent-Shared Housing", "Permanent Housing", "Moved Into Affordable Perm Housing", "Secured Affordable Perm. Housing", "Client Secured Housing", "Moved Back To Rental Trailer", "Housed At Yosemite Manor"), "Permanent Housing",
                                             ifelse(hsaps_ab120_exit$exit %in% c("Permanent- Residential Program", "Temporary- Residential Program", "Residential Care Facility", "Skilled Nursing Facility", "Long-Term Care Facility/Nursing Home", "Tempoaray Residential Program", "Entered Long Term Care", "Entered Nursing Home", "Board And Care Facility", "Temporary-Residential Program"), "Instituition",
                                                    ifelse(hsaps_ab120_exit$exit %in% c("Temporary Housing", "Hotel With Rights", "Hotel No Rights", "Interim Housing", "Teamporary Houising", "Temporary Shelter", "Emergency Shelter/Including Hotel/Motel Paid Fwith Emergency Shelter Voucher/Host Home Shelter", "Client Was Moved To Louisiana With The Assistance Of Aps. Hs Program Has Been Discontinued But Motel Payment Was Not Issued Until Q2 Therefore Cm Active On Workbook", "Motel"), "Temporary Housing",
                                                           ifelse(hsaps_ab120_exit$exit %in% c("Homeless", "Homeless Sheltered", "Homeless Unsheltered", "Transitional Housing Fhomeless Persons Including Homeless Youth", "Homeless Working With Calif Fhousing Needs"), "Homeless",
                                                                  "Unknown"))))))
hsaps_ab120_exit_county=aggregate(hsaps_ab120_exit$count,list(hsaps_ab120_exit$county,hsaps_ab120_exit$fy,hsaps_ab120_exit$type),sum)
names(hsaps_ab120_exit_county)=c("county","fy","exit_type","count")
hsaps_ab120_exit_county = hsaps_ab120_exit_county[order(hsaps_ab120_exit_county$county,hsaps_ab120_exit_county$fy),]

# Specify the file path for exit data to excel sheet
file_path <- "C:/Users/mboyd2/OneDrive - California Department of Social Services (CDSS)/output.xlsx"

# Write the DataTable to an Excel file
write.xlsx(hsaps_ab120_exit_county, file = file_path, sheetName = "Sheet1", overwrite = TRUE)

rm(hsaps_ab120,hsaps_ab120_exit,hsaps_ab120_int_cost,balance,bay_area,central_coast,sac_area,sanjoaquin_valley,socal,
   hsaps_ab120_exit_county,hsaps_ab120_exit,hsaps_ab120_exit_region)
