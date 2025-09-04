#packages and functions we need to run this script
library(writexl)
library(stringr)
library(plyr)
library(openxlsx)
source("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/R Scripts/HSAPS Functions.R")
#flagging to make sure all the entries are in the data
#after entering the quarter information in "hsaps_period.csv", import this document back
quarter = read.csv("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/R Scripts/Quarter_Period.csv")
participant_list = read.xlsx("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Data Integration/BUCP_datasets/HHD_participant_dataset_20241206 - abridged.xlsx")
names(participant_list) = c("First Name_nows","Middle Name_nows","Last Name_nows","Date of Birth","Case ID","Program","ID")

##Pre FY22-23 Q3 compiling -> NO LONGER NEEDED
# #ensuring no rows are left out...
# hsaps_monthly_reports = hsaps_compiling_monthly_reports(hsaps_monthly_reports,list("FY21-22 2021-12 (Dec).xlsx","FY21-22 2021-11 (Nov).xlsx","FY21-22 2021-10 (Oct).xlsx","FY21-22 2021-09 (Sep).xlsx",
#                                                                                    "FY21-22 2021-08 (Aug).xlsx","FY21-22 2021-07 (Jul).xlsx","FY20-21 2021-06 (Jun).xlsx","FY20-21 2021-05 (May).xlsx",
#                                                                                    "FY20-21 2021-04 (Apr).xlsx","FY20-21 2021-03 (Mar).xlsx","FY20-21 2021-02 (Feb).xlsx","FY20-21 2021-01 (Jan).xlsx",
#                                                                                    "FY20-21 2020-12 (Dec).xlsx","FY20-21 2020-11 (Nov).xlsx","FY20-21 2020-10 (Oct).xlsx","FY20-21 2020-09 (Sep).xlsx",
#                                                                                    "FY20-21 2020-08 (Aug).xlsx","FY20-21 2020-07 (Jul).xlsx","FY19-20 2020-06 (Jun).xlsx","FY19-20 2020-05 (May).xlsx","FY19-20 2020-04 (Apr).xlsx"))
# hsaps_monthly_reports = unique(rbind(tehama_2223_q3,hsaps_monthly_reports))
# hsaps_quarterly_reports = hsaps_compiling_quarterly_reports(hsaps_quarterly_reports,rev(list(c("HSAPS FY21-22 Q3 2022-6-3.xlsm","2022-01-01"),c("HSAPS FY21-22 Q4 2022-9-7.xlsm","2022-04-01"),
#                                                                                              c("HSAPS FY22-23 Q1 2022-12-19.xlsm","2022-07-01"),c("HSAPS FY22-23 Q2 2023-4-19.xlsm","2022-10-01"),
#                                                                                              c("HSAPS FY22-23 Q3 6-28-23.xlsm","2023-01-01"))))
# hsaps_quarterly_reports = hsaps_quarterly_reports[-which(hsaps_quarterly_reports$Reporting_Agency=="Tehama"
#                                                          & hsaps_quarterly_reports$Report_Month=="2023-01-01"),]
# hsaps_quarterly_reports = hsaps_quarterly_reports[-which(hsaps_quarterly_reports$Reporting_Agency=="San Bernardino"
#                                                         & hsaps_quarterly_reports$Report_Month=="2022-07-01"),]
# hsaps_quarterly_reports = unique(rbind.fill(hsaps_quarterly_reports[-which(hsaps_quarterly_reports$Reporting_Agency=="Placer"
#                                                                            & hsaps_quarterly_reports$Report_Month=="2023-01-01"),],placer_2223_q3,san_bernardino_q3))
# for(j in grep("date",tolower(names(hsaps_quarterly_reports)))){
#   hsaps_quarterly_reports[,j] = as.character(hsaps_quarterly_reports[,j])
# }
# hsaps = unique(rbind.fill(hsaps_quarterly_reports,hsaps_monthly_reports))
# hsaps$Reporting_Agency = gsub("^Select Agency","Santa Cruz",hsaps$Reporting_Agency)
# hsaps$Reporting_Agency = gsub(" County","",hsaps$Reporting_Agency)

##post FY22-23 Q3 compiling
#make sure to change the dataset to the most recent one
hsaps_backup = read.xlsx("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/^hsaps_cleaned_2025-01-22.xlsx",sep.names = " ")
#sac and placer county told us to disregard older data
# hsaps_backup = hsaps_backup[-grep("^Sacr",hsaps_backup$Reporting_Agency),]
# hsaps_backup = hsaps_backup[-grep("Placer",hsaps_backup$Reporting_Agency),]
#please make sure to use the most recent quarterly report
hsaps_new_data = hsaps_gate_keeping(hsaps_backup,"HSAPS FY24-25 2024-12-19.xlsm","2024-07-01")
hsaps_backup = hsaps_new_data[[1]]
hsaps_new_data_include = hsaps_new_data[[2]]
hsaps_new_data_exclude = hsaps_new_data[[3]]
# hsaps_new_data_case_start_date_disc = hsaps_new_data[[4]]
hsaps = unique(rbind.fill(hsaps_backup,hsaps_new_data_include))
hsaps$Reporting_Agency = gsub(" County","",hsaps$Reporting_Agency)
hsaps = hsaps[,grep("^Reporting_Ag",names(hsaps)):grep("^Comments$",names(hsaps))]

#fix all the date vars to format "yyyy-mm-dd"
for(j in grep("date",tolower(names(hsaps)))){
  if(length(grep(" [0-9]+:[0-9]+:[0-9]+.*",hsaps[,j]))>0){
    hsaps[,j] = gsub(" [0-9]+:[0-9]+:[0-9]+.*","",hsaps[,j])
  }
  hsaps[,j] = as.character(as.Date(hsaps[,j]))
}

#flagging the data with no case start date
hsaps_no_start_date = hsaps[which(is.na(hsaps$`Case Start Date`)),]
hsaps = hsaps[which(!is.na(hsaps$`Case Start Date`)),]

#dealing with the dataset without case start date
hsaps_no_start_date_duplicate = hsaps_no_start_date[(duplicated(hsaps_no_start_date[c("Reporting_Agency","Report_Month","First Name","Last Name")])
                                                     & duplicated(hsaps_no_start_date[c("Reporting_Agency","Report_Month","First Name","Last Name")],fromlast=T)
                                                     & duplicated(hsaps_no_start_date[c("Reporting_Agency","Report_Month","First Name","Last Name")],fromlast=T)
                                                     & duplicated(hsaps_no_start_date[c("Reporting_Agency","Report_Month","First Name","Last Name")],fromlast=T)),c("Reporting_Agency","Report_Month","First Name","Last Name")]
#getting the list of duplicate pairs without case start date within the same reporting month
hsaps_no_start_date_duplicate = unique(merge(hsaps_no_start_date_duplicate,hsaps_no_start_date,all.x=T))
#removing the list of duplicate pairs without case start date within the same reporting period
hsaps_no_start_date = hsaps_duplicate_data_remove(hsaps_no_start_date,hsaps_no_start_date_duplicate)
#order the rest of the duplicate pairs by reporting month
hsaps_no_start_date = hsaps_no_start_date[order(hsaps_no_start_date$Report_Month,decreasing = T),]
#getting the most recent data
hsaps_no_start_date_unique = hsaps_no_start_date[!(duplicated(hsaps_no_start_date[c("First Name","Last Name","Reporting_Agency")])
                                                   & duplicated(hsaps_no_start_date[c("First Name","Last Name","Reporting_Agency")],fromlast=T)
                                                   & duplicated(hsaps_no_start_date[c("First Name","Last Name","Reporting_Agency")],fromlast=T)),]

#trying to see which participants have intervention data even without start date -> include in the data pool
hsaps_no_start_date_unique$no_intervention = 0
for(i in 1:nrow(hsaps_no_start_date_unique)){
  hsaps_no_start_date_unique$no_intervention[i] = ifelse(all(is.na(hsaps_no_start_date_unique[i,grep("Intervention [0-9]{1} - Type",names(hsaps_no_start_date_unique))]))
                                                         |all(tolower(hsaps_no_start_date_unique[i,grep("Intervention [0-9]{1} - Type",names(hsaps_no_start_date_unique))])=="no intervention"),
                                                         1,hsaps_no_start_date_unique$no_intervention[i])
}

#no test entries
if(length(grep("Test",hsaps$`First Name`))>0){
  hsaps = hsaps[-grep("Test",hsaps$`First Name`),]
}
hsaps$`Case Start Date` = gsub("\\.[0-9]+$","",hsaps$`Case Start Date`)

#checking duplicate data
#hsaps_duplicate_data = hsaps_duplicate_data_consolidate(hsaps) #rows with the same first & last name, project start date
#code above takes too long to run it somehow, so we had to find another way to deal with this.
#alternative way below:
#flagging the list of first name and last names with the same birth date and cast start date pairs
hsaps_duplicate_data = hsaps[(duplicated(hsaps[c("Reporting_Agency","First Name","Last Name","Date of Birth","Case Start Date")])
                             & duplicated(hsaps[c("Reporting_Agency","First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
                             & duplicated(hsaps[c("Reporting_Agency","First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
                             & duplicated(hsaps[c("Reporting_Agency","First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
                             & duplicated(hsaps[c("Reporting_Agency","First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)),c("Reporting_Agency","First Name","Last Name","Date of Birth","Case Start Date")]
#finding all the data rows within hsaps that have the same first/last name, dob, and date pairs
hsaps_duplicate_data = unique(merge(hsaps_duplicate_data,hsaps,all.x=T))
#flaggihng all out
hsaps = hsaps_duplicate_data_remove(hsaps,hsaps_duplicate_data)
#determining which one to add back in -> the most recent rows
hsaps_duplicate_data_ordered = hsaps_duplicate_data[order(hsaps_duplicate_data$Report_Month,decreasing = T),]
#duplicate data within the same reporting period
hsaps_duplicate_data_same_period = hsaps_duplicate_data_ordered[(duplicated(hsaps_duplicate_data_ordered[c("Report_Month","First Name","Last Name","Date of Birth","Case Start Date")])
                                                                & duplicated(hsaps_duplicate_data_ordered[c("Report_Month","First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
                                                                & duplicated(hsaps_duplicate_data_ordered[c("Report_Month","First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
                                                                & duplicated(hsaps_duplicate_data_ordered[c("Report_Month","First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
                                                                & duplicated(hsaps_duplicate_data_ordered[c("Report_Month","First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)),
                                                              c("Reporting_Agency","Report_Month","First Name","Last Name","Date of Birth","Case Start Date")]
#finding all the data rows within hsaps that have the same first/last name, dob, and date pairs
hsaps_duplicate_data_same_period = unique(merge(hsaps_duplicate_data_same_period,hsaps_duplicate_data_ordered,all.x=T))

#removing duplicate data from the same reporting period
hsaps_duplicate_data_ordered = hsaps_duplicate_data_remove(hsaps_duplicate_data_ordered,hsaps_duplicate_data_same_period)

#list to sort through among duplicates within the same reporting period
hsaps_duplicate_sort_backup = subset(read.xlsx("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/Cleaning In Progress/hsaps_duplicate_data_same_period_sorted.xlsx",sheet="duplicates_sort_needed",sep.names = " "),!grepl("^Include",Reporting_Agency))
hsaps_duplicate_sort = hsaps_duplicate_data_same_period[-grep("remove",tolower(hsaps_duplicate_data_same_period$`Remove Case`)),]
hsaps_duplicate_sort_needed = hsaps_duplicate_sort[(duplicated(hsaps_duplicate_sort[c("Reporting_Agency","First Name","Last Name","Date of Birth","Case Start Date")])
                                                   & duplicated(hsaps_duplicate_sort[c("Reporting_Agency","First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
                                                   & duplicated(hsaps_duplicate_sort[c("Reporting_Agency","First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
                                                   & duplicated(hsaps_duplicate_sort[c("Reporting_Agency","First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
                                                   & duplicated(hsaps_duplicate_sort[c("Reporting_Agency","First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)),
                                                  c("Reporting_Agency","First Name","Last Name","Date of Birth","Case Start Date")]
hsaps_duplicate_sort_needed = merge(hsaps_duplicate_sort_needed,hsaps_duplicate_sort,all.x=T)
hsaps_duplicate_sort_include = hsaps_duplicate_data_remove(hsaps_duplicate_sort,hsaps_duplicate_sort_needed)
hsaps_duplicate_case_continued = unique(subset(read.xlsx("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/Cleaning In Progress/hsaps_duplicate_data_same_period_sorted.xlsx",sheet="duplicates_case_continued",sep.names = " ")[,c(1,3:6)],
                                        !grepl("^Include",Reporting_Agency)))
hsaps_duplicate_case_continued = subset(merge(hsaps_duplicate_case_continued,hsaps_duplicate_sort_needed,all.x=T),Report_Month=="2024-07-01")
hsaps_duplicate_case_continued$case_continued = "Yes"
hsaps_duplicate_sort_needed = hsaps_duplicate_data_remove(hsaps_duplicate_sort_needed,hsaps_duplicate_case_continued)

if(nrow(hsaps_duplicate_sort_needed)!=0){
  write_xlsx(hsaps_duplicate_sort_needed[,c(1,grep("Report_M",names(hsaps_duplicate_sort_needed)),grep("First N",names(hsaps_duplicate_sort_needed)):grep("Case Start",names(hsaps_duplicate_sort_needed)),
                                            grep("Remove C",names(hsaps_duplicate_sort_needed)):ncol(hsaps_duplicate_sort_needed))],"hsaps_duplicates_same_period_sort_needed.xlsx")
}
# write_xlsx(hsaps_duplicate_data_same_period,"Home Safe(HSAPS)/Cleaning/hsaps_duplicates_same_period.xlsx")
#^comparing this way, we would know which rows (county,f/l names,case start date) we want to look into

#after going through some list of duplicates within the same reporting period, run below
hsaps_duplicate_data_same_period_include = read.xlsx("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/Cleaning In Progress/hsaps_duplicate_data_same_period_sorted.xlsx",sheet="duplicates_to_be_included",sep.names = " ")[,1:79]
hsaps_duplicate_data_same_period_include = hsaps_duplicate_data_same_period_include[hsaps_duplicate_data_same_period_include$Report_Month=="2024-07-01",]
# for(j in grep("date",tolower(names(hsaps_duplicate_data_same_period_include)))){
#   hsaps_duplicate_data_same_period_include[,j] = as.character(hsaps_duplicate_data_same_period_include[,j][[1]])
# }
# names(hsaps_duplicate_data_same_period_include) = gsub("\r\n","",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub("^Item [0-9]+: ","",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub(" \\(optional\\)$","",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub("^Home Safe ","",names(hsaps_duplicate_data_same_period_include)) #to match with var name "Case Start Date"
# names(hsaps_duplicate_data_same_period_include) = gsub("^HSAPS ","",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub("Home Safe ","",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub("\\?$","",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub("Home Safe$","HSAPS",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub(" to HSAPS$","",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub("-up","-Up",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub("  -"," -",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub("/\\s","/",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub("\\s\\s","\\s",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub("by Other-","by Other -",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub("from be","from Be",names(hsaps_duplicate_data_same_period_include))
# names(hsaps_duplicate_data_same_period_include) = gsub("Reported Incident","Report",names(hsaps_duplicate_data_same_period_include))
hsaps_duplicate_data_ordered = unique(rbind(hsaps_duplicate_data_ordered,hsaps_duplicate_data_same_period_include,hsaps_duplicate_sort_include))
hsaps_duplicate_data_ordered = hsaps_duplicate_data_ordered[order(hsaps_duplicate_data_ordered$Report_Month,decreasing=T),]
# write_xlsx(hsaps_duplicate_data_ordered,"Home Safe(HSAPS)/Cleaning/hsaps_duplicate_data_ordered.xlsx")

#using the duplicated() functions again to filter out all the older data
hsaps_duplicate_data_include = hsaps_duplicate_data_ordered[!(duplicated(hsaps_duplicate_data_ordered[c("First Name","Last Name","Date of Birth","Case Start Date")])
                                                              & duplicated(hsaps_duplicate_data_ordered[c("First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
                                                              & duplicated(hsaps_duplicate_data_ordered[c("First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
                                                              & duplicated(hsaps_duplicate_data_ordered[c("First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)),]

#including data rows
hsaps = unique(rbind(hsaps,hsaps_duplicate_data_include,hsaps_no_start_date_unique[hsaps_no_start_date_unique$no_intervention==0,-ncol(hsaps_no_start_date_unique)]))
hsaps$case_continued = "No"
hsaps = rbind(hsaps,hsaps_duplicate_case_continued)
hsaps = hsaps[!is.na(hsaps$Reporting_Agency),]
  
#make sure to adjust the date below ("as.Date(date)") every quarter -> have at least two quarter discrepancy (to account for non-submission)
hsaps_noncumulative = subset(hsaps,as.Date(Report_Month)<as.Date('2024-07-01'))[,c(grep("Reporting_",names(hsaps)),
                                                                                   grep("Report_Mon",names(hsaps)),
                                                                                   grep(" Name$",names(hsaps)),
                                                                                   grep("of Birth$",names(hsaps)),grep("Case Start",names(hsaps)))]
hsaps_noncumulative = merge(hsaps_noncumulative,quarter[,c("Report_Month","Quarter")],all.x=T)
write_xlsx(hsaps_noncumulative,"//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/Cleaning In Progress/hsaps_dropped.xlsx")

#removing entries with "Remove" mark
if(length(grep("remove",tolower(hsaps$`Remove Case`)))>0){
  hsaps = hsaps[-grep("remove",tolower(hsaps$`Remove Case`)),]
}

#adapting the cleaning for categorical vars
hsaps$`Remove Case` = gsub("blank","",tolower(hsaps$`Remove Case`))
for(j in grep("Name$",names(hsaps))){
  hsaps[,j] = str_to_title(hsaps[,j])
}
for(j in c(grep("^Location of P",names(hsaps)):grep("^Living Situation Upon Entry",names(hsaps)),
           grep("^Client Homeless",names(hsaps)):grep("^Discharge from",names(hsaps)),
           grep("Other - Financial",names(hsaps)):grep("^Previous APS",names(hsaps)),
           grep("CES",names(hsaps)),seq(grep("1 - Type$",names(hsaps)),grep("6 - Type",names(hsaps)),by=4),
           grep("at Exit$",names(hsaps)),grep("up - method",tolower(names(hsaps))),
           grep("Up - Living Situation$",names(hsaps)),grep("Up - Homeless",names(hsaps)))){
  hsaps[,j] = tolower(hsaps[,j])
  hsaps[,j] = gsub("does not","doesn't",hsaps[,j])
  hsaps[,j] = gsub(" /","/",hsaps[,j])
  hsaps[,j] = gsub("/ ","/",hsaps[,j])
  hsaps[,j] = gsub(" and/or ","/",hsaps[,j])
  # hsaps[,j] = gsub(", ","/",hsaps[,j])
  # hsaps[,j] = gsub(" or ","/",hsaps[,j])
  # hsaps[,j] = gsub("or ","",hsaps[,j])
  hsaps[,j] = gsub("hetere","hetero",hsaps[,j])
  hsaps[,j] = gsub("temporary -","temporary-",hsaps[,j])
  hsaps[,j] = gsub("permanent -","permanent-",hsaps[,j])
  hsaps[,j] = gsub("african-american","african american",hsaps[,j])
  hsaps[,j] = gsub("w/","with ",hsaps[,j])
  hsaps[,j] = gsub("\r\r\n","",hsaps[,j])
  hsaps[,j] = gsub("\r$","",hsaps[,j])
  hsaps[,j] = gsub("the last 3","the last three",hsaps[,j])
  hsaps[,j] = gsub("three years/longer","three years or longer",hsaps[,j])
  hsaps[,j] = gsub("prito","prior to",hsaps[,j])
  hsaps[,j] = gsub("prior home safe","prior to home safe",hsaps[,j])
  hsaps[,j] = gsub("–","-",hsaps[,j])
  hsaps[,j] = gsub("^relocation assistance$","relocation assistance/storage",hsaps[,j])
  hsaps[,j] = gsub("ukno","unkno",hsaps[,j])
  hsaps[,j] = gsub("unko","unkno",hsaps[,j])
  hsaps[,j] = gsub("no homeless","not homeless",hsaps[,j])
  hsaps[,j] = gsub("^refused$","client refused",hsaps[,j])
  hsaps[,j] = gsub("yes-before","yes - before",hsaps[,j])
  hsaps[,j] = gsub("facilitiy","facility",hsaps[,j])
  hsaps[,j] = gsub("medical professional","medical personnel",hsaps[,j])
  # hsaps[,j] = gsub("mental health professional","mental health personnel",hsaps[,j])
  hsaps[,j] = gsub("owners","owner",hsaps[,j])
  hsaps[,j] = gsub("not rights","no rights",hsaps[,j])
  hsaps[,j] = gsub("lives with","with",hsaps[,j])
  hsaps[,j] = gsub("paying ","",hsaps[,j])
  hsaps[,j] = gsub("not rent","no rent",hsaps[,j])
  hsaps[,j] = gsub("inervention","intervention",hsaps[,j])
  hsaps[,j] = gsub("verified- program","verified - program",hsaps[,j])
  hsaps[,j] = gsub(" \\[i.e./doubled up\\]","",hsaps[,j])
  hsaps[,j] = str_to_title(hsaps[,j])
  hsaps[,j] = gsub("Or","or",hsaps[,j])
}

#consolidating Living Situation at Entry/Exit var
for(j in c(grep("Upon Entry",names(hsaps)),grep("at Exit",names(hsaps)))){
  hsaps[,j] = gsub("\\(","",hsaps[,j])
  hsaps[,j] = gsub("\\)","",hsaps[,j])
  hsaps[,j] = gsub("Others-","Others",hsaps[,j])
  hsaps[,j] = gsub("/Double Up$","",hsaps[,j])
}

# ##Implementing fix from grantees; was already done
# #implementing grantees' fix
# # key_variable_cleaned = rbind(key_variable_cleaned1,key_variable_cleaned2[,names(key_variable_cleaned1)],key_variable_cleaned3[,names(key_variable_cleaned1)])
# key_variable_cleaned = read.csv("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/R Scripts/key_var_cleaned.csv", check.names = FALSE)
# key_variable_cleaned_fn = subset(key_variable_cleaned,grepl("First Name",`Cleaning Comments`))
# key_variable_cleaned_ln = subset(key_variable_cleaned,grepl("Last Name",`Cleaning Comments`))
# key_variable_cleaned_dob = subset(key_variable_cleaned,grepl("DOB",`Cleaning Comments`))
# key_variable_cleaned_csd = subset(key_variable_cleaned,grepl("Case Start",`Cleaning Comments`))
# names(key_variable_cleaned_fn)[4]=paste0(names(key_variable_cleaned_fn)[4],'_cleaned')
# names(key_variable_cleaned_ln)[3]=paste0(names(key_variable_cleaned_ln)[3],'_cleaned')
# names(key_variable_cleaned_dob)[6]=paste0(names(key_variable_cleaned_dob)[6],'_cleaned')
# names(key_variable_cleaned_csd)[7]=paste0(names(key_variable_cleaned_csd)[7],'_cleaned')
# hsaps = unique(merge(hsaps,key_variable_cleaned_ln[,c(1,3:4,6:7)],all.x=T))
# hsaps$`Last Name_cleaned` = ifelse(is.na(hsaps$`Last Name_cleaned`),hsaps$`Last Name`,hsaps$`Last Name_cleaned`)
# hsaps$`Last Name` = hsaps$`Last Name_cleaned`
# hsaps = unique(merge(hsaps,key_variable_cleaned_fn[,c(1,3:4,6:7)],all.x=T))
# hsaps$`First Name_cleaned` = ifelse(is.na(hsaps$`First Name_cleaned`),hsaps$`First Name`,hsaps$`First Name_cleaned`)
# hsaps$`First Name`=hsaps$`First Name_cleaned`
# hsaps = unique(merge(hsaps,key_variable_cleaned_csd[,c(1,3:4,6:7)],all.x=T))
# hsaps$`Case Start Date_cleaned` = ifelse(is.na(hsaps$`Case Start Date_cleaned`),hsaps$`Case Start Date`,hsaps$`Case Start Date_cleaned`)
# hsaps$`Case Start Date`=hsaps$`Case Start Date_cleaned`
# hsaps = unique(merge(hsaps,key_variable_cleaned_dob[,c(1,3:4,6)],all.x=T))
# hsaps$`Date of Birth_cleaned` = ifelse(is.na(hsaps$`Date of Birth_cleaned`),hsaps$`Date of Birth`,hsaps$`Date of Birth_cleaned`)
# hsaps$`Date of Birth`=hsaps$`Date of Birth_cleaned`
# hsaps$`Last Name_cleaned`=NULL
# hsaps$`First Name_cleaned`=NULL
# hsaps$`Date of Birth_cleaned`=NULL
# hsaps$`Case Start Date_cleaned`=NULL
# 
# hsaps_duplicate_data0 = hsaps[(duplicated(hsaps[c("First Name","Last Name","Date of Birth","Case Start Date")])
#                                & duplicated(hsaps[c("First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
#                                & duplicated(hsaps[c("First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
#                                & duplicated(hsaps[c("First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)),c("Reporting_Agency","First Name","Last Name","Date of Birth","Case Start Date")]
# #finding all the data rows within hsaps that have the same first/last name, dob, and date pairs
# hsaps_duplicate_data0 = unique(merge(hsaps_duplicate_data0,hsaps,all.x=T))
# #flaggihng all out
# hsaps = hsaps_duplicate_data_remove(hsaps,hsaps_duplicate_data0)
# hsaps_duplicate_data_ordered0 = hsaps_duplicate_data0[order(hsaps_duplicate_data0$Report_Month,decreasing = T),]
# hsaps_duplicate_data_include0 = hsaps_duplicate_data_ordered0[!(duplicated(hsaps_duplicate_data_ordered0[c("First Name","Last Name","Date of Birth","Case Start Date")])
#                                                                 & duplicated(hsaps_duplicate_data_ordered0[c("First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
#                                                                 & duplicated(hsaps_duplicate_data_ordered0[c("First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)
#                                                                 & duplicated(hsaps_duplicate_data_ordered0[c("First Name","Last Name","Date of Birth","Case Start Date")],fromlast=T)),]
# hsaps = unique(rbind(hsaps,hsaps_duplicate_data_include0))
# rm(hsaps_duplicate_data0,hsaps_duplicate_data_include0,hsaps_duplicate_data_ordered0)

#archiving "cleaned" data
#anonymizing the data through generating participant id's and case numbers
hsaps_participant = unique(hsaps[,grep(" Name$",names(hsaps)):grep("Date of B",names(hsaps))])
hsaps_participant$`First Name_nows` = trimws(tolower(hsaps_participant$`First Name`))
hsaps_participant$`Last Name_nows` = trimws(tolower(hsaps_participant$`Last Name`))
hsaps_participant = unique(merge(hsaps_participant,participant_list[,c(1,3:4,grep("^id$",tolower(names(participant_list))))],all.x=T))
hsaps_id_assign = unique(hsaps_participant[is.na(hsaps_participant$ID),-grep("^id$",tolower(names(hsaps_participant)))])
hsaps_id_assign$ID = as.character(sample(c(1000000:9999999)[-which(c(1000000:9999999) %in% hsaps_participant$ID)],nrow(hsaps_id_assign)))
hsaps_participant = rbind(hsaps_participant[-which(paste0(hsaps_participant$`First Name`,hsaps_participant$`Last Name`,hsaps_participant$`Date of Birth`)
                                                   %in% paste0(hsaps_id_assign$`First Name`,hsaps_id_assign$`Last Name`,hsaps_id_assign$`Date of Birth`)),],hsaps_id_assign)
hsaps = merge(hsaps,hsaps_participant[,-grep("_nows$",names(hsaps_participant))],all.x=T)
hsaps = hsaps[order(hsaps$Reporting_Agency,hsaps$ID,hsaps$`Case Start Date`),]
hsaps$case = ifelse(is.na(hsaps$`Case Start Date`),NA,1)
for(i in 2:nrow(hsaps)){
  if(!is.na(hsaps$case[i])){
    if(hsaps$ID[i]==hsaps$ID[i-1]){
      hsaps$case[i] = hsaps$case[i-1]+1
    }
  }
}
hsaps$case = as.character(hsaps$case)

#assigning 1 to new cases without case start date
hsaps_case = hsaps[is.na(hsaps$`Case Start Date`)&!is.na(hsaps$Reporting_Agency),]
hsaps = hsaps[!is.na(hsaps$`Case Start Date`),]
hsaps_case$case = ifelse(hsaps_case$ID %in% hsaps$ID,hsaps_case$case,1)
hsaps = rbind(hsaps,hsaps_case)
rm(hsaps_case)
hsaps$age = ifelse(!is.na(hsaps$`Case Start Date`),floor(elapsed_months(hsaps$`Case Start Date`,hsaps$`Date of Birth`)/12),
                   floor(elapsed_months(hsaps$`Intervention 1 - Date`,hsaps$`Date of Birth`)/12))
hsaps = hsaps[order(hsaps$Reporting_Agency,hsaps$`Last Name`,hsaps$`First Name`),]

#saving the data for those running the cleaning script
write_xlsx(hsaps[order(hsaps$Reporting_Agency,hsaps$`Last Name`),c(1:grep("Comment",names(hsaps)),grep("^id$",tolower(names(hsaps))):grep("^case$",names(hsaps)))],paste0("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/^hsaps_cleaned_",Sys.Date(),".xlsx"))
# write_xlsx(hsaps[order(hsaps$Reporting_Agency,hsaps$id),c(1,grep("^id$",tolower(names(hsaps))),grep("^case$",names(hsaps)),grep("^age$",names(hsaps)),grep("Case Start",names(hsaps)),
#                                                           grep("^Location of",names(hsaps)):grep("Comment",names(hsaps)))],"//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/HSAPS Data for UCSF.xlsx")
#saving datasets needed for dqr generation
write_xlsx(hsaps_new_data_exclude,"//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/Cleaning In Progress/hsaps_new_data_exclude.xlsx")
# write_xlsx(hsaps_new_data_case_start_date_disc,"//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/Cleaning In Progress/hsaps_new_data_csd_disc.xlsx")
write_xlsx(hsaps_no_start_date_duplicate,"//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/Cleaning In Progress/hsaps_no_start_date_duplicate.xlsx")

##further processing for Tableau dataset
#setting up categories
na = c("client doesn't know","client refused","data not collected")
na_re = c("client doesn't know","client prefers not to answer","data not collected")
gi = c("female","male","different identity","transgender","non-binary",na_re)
race1 = tolower(c("american indian, alaska Native, or Indigenous","Asian or asian american",
                  "Black, African American, or African","Native Hawaiian or Pacific Islander","other",
                  "White",na_re))
ethn = c("non-hispanic/latina/e/o","hispanic/latina/e/o","cuban","puerto rican","mexican/chicano",
         "other hispanic/latino","not hispanic/latino",na_re)
ms = c("married","not married/living with partner","divorced","separated","widowed","never married","data not collected","unknown/not provided")
so = c("straight/heterosexual","gay/lesbian","bisexual","questioning",na)
pl = c("english","spanish","mandarin/cantonese","vietnamese","tagalog","korean","other","data not collected")
medi_calcare = c("yes","no","client doesn't know","data not collected")
veteran_cl3yrs_evics_discharge = c("yes","no",na)
ls_entry = c("homeless","temporary housing","temporary - residential program","permanent - residential program","rent leaseholder",
             "owner","other permanent housing","other","data not collected","with others rent","with others no rent",
             "hotel no rights","hotel with rights","owner lives alone","owner with others rent","owner with others no rent",
             "skilled nursing facility","residential care facility","board and care facility")
abuse_neglect_prevaps_oldcols = c("yes","no","unknown")
rs = c("professional service provider","educator","financial service provider","law enforcement","property owner","mandated reporter",
       "medical personnel","mental health professional","institutional employee","social worker","unknown",
       "other community professional","community professional","clergy","self","family member","no relationship","anonymous")
ces = c("no","yes - before home safe","yes - after home safe","data not collected")
interv_type = c("no intervention","enhanced case management","mortgage payment","rent back-pay","rent payment","housing navigation",
                "temporary housing","emergency shelter","security deposit","utilities","relocation assistance/storage",
                "home habitability","legal services","caregiver services/respite care","other","deep cleaning/hoarding assistance",
                "deep cleaning","home repair","external housing navigation")
ls_exit_fu_ls = c(ls_entry,"deceased","not exited")
fu_method = c("unable to verify","hmis","aps system","verified - program staff","verified - external staff")
fu_homeless = c("yes","no","client doesn't know","unknown")

hsaps_tableau = hsaps_categorical_fix_tableau(hsaps,"Gender Identity",gi,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Race 1",race1,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Race 2",race1,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Ethnicity",ethn,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Current Marital Status",ms,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Sexual Orientation",so,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Medi-Cal",medi_calcare,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Medicare",medi_calcare,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Preferred Language",pl,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Veteran Status",veteran_cl3yrs_evics_discharge,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Living Situation Upon Entry",ls_entry,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Client Homeless Within the Last Three Years",veteran_cl3yrs_evics_discharge,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Previous Evictions or Foreclosures",veteran_cl3yrs_evics_discharge,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Current Eviction or Foreclosures",veteran_cl3yrs_evics_discharge,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Discharge from Institution in the Last Six Months",veteran_cl3yrs_evics_discharge,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Abuse by Other - Financial",abuse_neglect_prevaps_oldcols,"Unknown")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Abuse by Other - Non-Financial",abuse_neglect_prevaps_oldcols,"Unknown")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Self-Neglect",abuse_neglect_prevaps_oldcols,"Unknown")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Reporting Source",rs,"Unknown")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Previous APS Involvement",abuse_neglect_prevaps_oldcols,"Unknown")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Client Referred to CES",ces,"Data Not Collected")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Intervention 1 - Type",interv_type,"No Intervention")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Intervention 2 - Type",interv_type,"No Intervention")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Intervention 3 - Type",interv_type,"No Intervention")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Intervention 4 - Type",interv_type,"No Intervention")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Intervention 5 - Type",interv_type,"No Intervention")
hsaps_tableau = hsaps_categorical_fix_tableau(hsaps_tableau,"Intervention 6 - Type",interv_type,"No Intervention")
hsaps_tableau$`Living Situation at Exit` = ifelse(!is.na(hsaps_tableau$`Living Situation at Exit`)
                                                  & !(tolower(hsaps_tableau$`Living Situation at Exit`)%in%ls_exit_fu_ls),"Data Not Collected",hsaps_tableau$`Living Situation at Exit`)
hsaps_tableau$`Six Month Follow-Up - Method` = ifelse(!is.na(hsaps_tableau$`Six Month Follow-Up - Method`)
                                                      & !(tolower(hsaps_tableau$`Six Month Follow-Up - Method`)%in%fu_method),"Unable To Verify",hsaps_tableau$`Six Month Follow-Up - Method`)
hsaps_tableau$`Six Month Follow-Up - Living Situation` = ifelse(!is.na(hsaps_tableau$`Six Month Follow-Up - Living Situation`)
                                                                & !(tolower(hsaps_tableau$`Six Month Follow-Up - Living Situation`)%in%ls_exit_fu_ls),"Data Not Collected",hsaps_tableau$`Six Month Follow-Up - Living Situation`)
hsaps_tableau$`Six Month Follow-Up - Homelessness` = ifelse(!is.na(hsaps_tableau$`Six Month Follow-Up - Homelessness`)
                                                               & !(tolower(hsaps_tableau$`Six Month Follow-Up - Homelessness`)%in%fu_homeless),"Unknown",hsaps_tableau$`Six Month Follow-Up - Homelessness`)
hsaps_tableau$`Twelve Month Follow-Up - Method` = ifelse(!is.na(hsaps_tableau$`Twelve Month Follow-Up - Method`)
                                                      & !(tolower(hsaps_tableau$`Twelve Month Follow-Up - Method`)%in%fu_method),"Unable To Verify",hsaps_tableau$`Twelve Month Follow-Up - Method`)
hsaps_tableau$`Twelve Month Follow-Up - Living Situation` = ifelse(!is.na(hsaps_tableau$`Twelve Month Follow-Up - Living Situation`)
                                                                & !(tolower(hsaps_tableau$`Twelve Month Follow-Up - Living Situation`)%in%ls_exit_fu_ls),"Data Not Collected",hsaps_tableau$`Twelve Month Follow-Up - Living Situation`)
hsaps_tableau$`Twelve Month Follow-Up - Homelessness` = ifelse(!is.na(hsaps_tableau$`Twelve Month Follow-Up - Homelessness`)
                                                         & !(tolower(hsaps_tableau$`Twelve Month Follow-Up - Homelessness`)%in%fu_homeless),"Unknown",hsaps_tableau$`Twelve Month Follow-Up - Homelessness`)
write_xlsx(hsaps_tableau[order(hsaps_tableau$Reporting_Agency,hsaps_tableau$`Last Name`),c(1,grep("^id$",names(hsaps_tableau)),grep("^case$",names(hsaps_tableau)),grep("^age$",names(hsaps_tableau)),grep("Case Start",names(hsaps_tableau)),
                                                                                           grep("^Location of",names(hsaps_tableau)):grep("Comment",names(hsaps_tableau)))],
           paste0("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/^HSAPS Data for Tableau_",Sys.Date(),".xlsx"))

#UCSF Data Save
write_xlsx(hsaps[order(hsaps$Reporting_Agency),c(grep("reporting_age",tolower(names(hsaps))),grep("^id$",tolower(names(hsaps))),grep("^case$",names(hsaps)),grep("^age$",names(hsaps)),grep("Case Start",names(hsaps)),
                                                 grep("^Location of",names(hsaps)):grep("Comment",names(hsaps)))],
           paste0("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/HSAPS Data for UCSF_",Sys.Date(),".xlsx"))

#clean up
rm(hsaps_duplicate_data_ordered,hsaps_duplicate_data_same_period,fu_homeless,interv_type,ls_exit_fu_ls,
   hsaps_no_start_date_duplicate,hsaps_duplicate_sort_needed,hsaps_duplicate_sort_include,hsaps_new_data_include,fu_method,
   hsaps_new_data_exclude,hsaps_id_assign,na,gi,race1,ethn,ms,so,pl,veteran_cl3yrs_evics_discharge,ls_entry,
   abuse_neglect_prevaps_oldcols,rs,ces)
