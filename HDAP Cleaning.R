loading_packages()
source("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/HDAP PII 21 Reports/R Scripts/HDAP Functions.R")
cat("Loading county codes...\n")
county_code = read_xlsx("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/HDAP PII 21 Reports/R Scripts/county_region.xlsx")
participant_list_hdap = read.xlsx("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Data Integration/BUCP_datasets/files_used_for_hhdrs/hdap_with_new_hhd_ids_2_15_25.xlsx")
names(participant_list) = c("First Name_nows","Middle Name_nows","Last Name_nows","Date of Birth","Case ID","Program","id")

#compiling upto FY22-23
# hdap = data.frame()
# hdap = hdap_compiling_reports(hdap,rev(list(c("HDAP PII FY 18-19 Q2 5-3-2019.xlsx","PII"),c("HDAP PII FY 18-19 Q3 8-9-2019.xlsx","PII"),
#                                             c("HDAP PII FY 18-19 Q4 11-7-2019.xlsx","PII"),c("HDAP PII FY 19-20 Q1 2-7-2020.xlsx","PII"),
#                                             c("HDAP PII FY 19-20 Q2 4-9-2020.xlsx","PII"),c("HDAP PII FY 19-20 Q3 6-18-2020.xlsx","PII"),
#                                             c("HDAP PII FY 19-20 Q4 10-6-2020.xlsx","PII"),c("HDAP PII FY 20-21 Q1 12-23-2020.xlsx","PII"),
#                                             c("HDAP PII FY20-21 Q2 2021-04-16.xlsx","Data"),c("HDAP PII FY20-21 Q3 2021-05-25.xlsx","Data"),
#                                             c("HDAP PII FY20-21 Q4 2022-02-08.xlsx","Data"),c("HDAP PII FY21-22 Q1 2022-02-08.xlsx","Data"),
#                                             c("HDAP PII FY21-22 Q2 2022-04-27.xlsx","Data"),c("HDAP PII FY21-22 Q3 2022-06-22.xlsx","Data"),
#                                             c("HDAP PII FY21-22 Q4 2022-10-13.xlsx","Data"),c("HDAP PII FY22-23 Q1 2022-12-21.xlsx","Data"),
#                                             c("HDAP PII FY22-23 Q2 2023-03-21.xlsx","Data"),c("HDAP PII FY22-23 Q3 2023-07-03.xlsx","Data"),
#                                             c("HDAP PII FY22-23 Q4 2023-09-05.xlsx","Data"))))
# for(j in c(grep("date",tolower(names(hdap))),grep("^quarter",tolower(names(hdap))))){
#   hdap[,j] = as.character(hdap[,j])
# }
# hdap = merge(hdap,county_code,by.x="County",by.y="code",all.x=T)
# hdap$County = hdap$county_name
# for(j in grep("Name",names(hdap))){
#   hdap[,j] = str_to_title(hdap[,j])
# }
# names(hdap) = gsub("  "," ",names(hdap))
# hdap$`Social Security Number` = gsub("-","",hdap$`Social Security Number`)
#Systemic error on modoc fy22-23 q2 report -> remove the entries since new entries can completely replace these ones
# hdap = hdap[-which(hdap$County=="Modoc"&grepl("-",hdap$`Middle Name (Optional)`)),]
# hdap_backup = hdap

##post FY22-23 compiling
cat("Reading backup dataset...\n")
hdap_backup = read.xlsx("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/HDAP PII/hdap_cleaned_2025-04-25.xlsx",sep.names = " ")
#pls change the report name to the most recent one
cat("Compiling new data...\n")
hdap_new_data = hdap_gate_keeping(hdap_backup,"FY 24-25/HDAP PII FY24-25 Q2 IH Compilation v2_USE.xlsx","Data")
hdap_backup = hdap_new_data[[1]]
hdap_new_data_include = hdap_new_data[[2]]
hdap_new_data_include$`Quarter Start` = "2024-10-01"
hdap_new_data_exclude = hdap_new_data[[3]]
hdap_new_data_exclude$`Quarter Start` = "2024-10-01"
# hdap_new_data_case_start_date_disc = hdap_new_data[[4]]
hdap_new_data_exclude = hdap_new_data_exclude[-grep("duplicate",tolower(hdap_new_data_exclude$`HMIS ID (Optional)`)),]
cat("Merging old and new data...\n")
hdap = unique(rbind.fill(hdap_backup,hdap_new_data_include,hdap_new_data_exclude))
hdap$id = NULL
hdap$case = NULL
hdap$`New/Update` = NULL
cat("Formatting date variables...\n")
#fix all the date vars to format "yyyy-mm-dd"
for(j in grep("date",tolower(names(hdap)))){
  if(length(grep(" [0-9]+:[0-9]+:[0-9]+.*",hdap[,j]))>0){
    hdap[,j] = gsub(" [0-9]+:[0-9]+:[0-9]+.*","",hdap[,j])
  }
  hdap[,j] = as.character(as.Date(hdap[,j]))
}

#flagging the data with no case start date
cat("Flagging data without case start date...\n")
hdap_no_start_date = hdap[which(is.na(hdap$`Project Start Date`)),]
hdap = hdap[which(!is.na(hdap$`Project Start Date`)),]

#dealing with the dataset without case start date
cat("Processing duplicates without case start date...\n")
hdap_no_start_date_duplicate = hdap_no_start_date[(duplicated(hdap_no_start_date[c("County","Quarter Start","First Name","Last Name")])
                                                   & duplicated(hdap_no_start_date[c("County","Quarter Start","First Name","Last Name")],fromlast=T)
                                                   & duplicated(hdap_no_start_date[c("County","Quarter Start","First Name","Last Name")],fromlast=T)
                                                   & duplicated(hdap_no_start_date[c("County","Quarter Start","First Name","Last Name")],fromlast=T)),c("County","Quarter Start","First Name","Last Name")]
if(nrow(hdap_no_start_date_duplicate)>0){
  #getting the list of duplicate pairs without case start date within the same reporting month
  hdap_no_start_date_duplicate = unique(merge(hdap_no_start_date_duplicate,hdap_no_start_date,all.x=T))
  #removing the list of duplicate pairs without case start date within the same reporting period
  hdap_no_start_date = hdap_duplicate_data_remove(hdap_no_start_date,hdap_no_start_date_duplicate)
  #order the rest of the duplicate pairs by reporting month
  hdap_no_start_date = hdap_no_start_date[order(hdap_no_start_date$`Quarter Start`,decreasing = T),]
  #getting the most recent data
  hdap_no_start_date_unique = hdap_no_start_date[!(duplicated(hdap_no_start_date[c("First Name","Last Name","County")])
                                                   & duplicated(hdap_no_start_date[c("First Name","Last Name","County")],fromlast=T)
                                                   & duplicated(hdap_no_start_date[c("First Name","Last Name","County")],fromlast=T)),]
}
write_xlsx(hdap_no_start_date_duplicate,"//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/HDAP PII 21 Reports/Cleaning in Progress/hdap_no_start_date_duplicate.xlsx")
if(nrow(hdap_no_start_date_duplicate)==0){
  hdap_no_start_date_unique = hdap_no_start_date
}

#checking duplicate data
#flagging the list of first name and last names with the same birth date and project start date pairs
cat("Processing duplicate data...\n")
hdap$`Project Start Date` = gsub("\\.[0-9]+$","",hdap$`Project Start Date`)
hdap_duplicate_data = hdap[(duplicated(hdap[c("County","First Name","Last Name","Date of Birth","Project Start Date")])
                            & duplicated(hdap[c("County","First Name","Last Name","Date of Birth","Project Start Date")],fromlast=T)
                            & duplicated(hdap[c("County","First Name","Last Name","Date of Birth","Project Start Date")],fromlast=T)
                            & duplicated(hdap[c("County","First Name","Last Name","Date of Birth","Project Start Date")],fromlast=T)
                            & duplicated(hdap[c("County","First Name","Last Name","Date of Birth","Project Start Date")],fromlast=T)),c("County","Last Name","First Name","Date of Birth","Project Start Date")]
#finding all the data rows within hdap that have the same first/last name, dob, and date pairs
hdap_duplicate_data = unique(merge(hdap_duplicate_data,hdap,all.x=T))
#flaggihng all out
hdap = hdap_duplicate_data_remove(hdap,hdap_duplicate_data)
#determining which one to add back in -> the most recent rows
hdap_duplicate_data_ordered = hdap_duplicate_data[order(hdap_duplicate_data$`Quarter Start`,decreasing = T),]
#duplicate data within the same reporting period
hdap_duplicate_data_same_period = hdap_duplicate_data_ordered[(duplicated(hdap_duplicate_data_ordered[c("Quarter Start","First Name","Last Name","Date of Birth","Project Start Date")])
                                                               & duplicated(hdap_duplicate_data_ordered[c("Quarter Start","First Name","Last Name","Date of Birth","Project Start Date")],fromlast=T)
                                                               & duplicated(hdap_duplicate_data_ordered[c("Quarter Start","First Name","Last Name","Date of Birth","Project Start Date")],fromlast=T)
                                                               & duplicated(hdap_duplicate_data_ordered[c("Quarter Start","First Name","Last Name","Date of Birth","Project Start Date")],fromlast=T)
                                                               & duplicated(hdap_duplicate_data_ordered[c("Quarter Start","First Name","Last Name","Date of Birth","Project Start Date")],fromlast=T)),
                                                               c("County","Quarter Start","First Name","Last Name","Date of Birth","Project Start Date")]
#finding all the data rows within hdap that have the same first/last name, dob, and date pairs
hdap_duplicate_data_same_period = unique(merge(hdap_duplicate_data_same_period,hdap_duplicate_data_ordered,all.x=T))

#sorting through which rows among the data above to include -> the least number of blanks; if the number of blanks the same, flag
hdap_duplicate_data_same_period$na_num = 0
for(i in 1:nrow(hdap_duplicate_data_same_period)){
  hdap_duplicate_data_same_period$na_num[i] = length(which(is.na(hdap_duplicate_data_same_period[i,])))
}

hdap_duplicate_data_same_period$include = ''
for(i in 1:nrow(hdap_duplicate_data_same_period)){
  if(!is.na(hdap_duplicate_data_same_period$`First Name`[i])&!is.na(hdap_duplicate_data_same_period$`Last Name`[i])){
    index = which(hdap_duplicate_data_same_period$County==hdap_duplicate_data_same_period$County[i]
                  & hdap_duplicate_data_same_period$`First Name`==hdap_duplicate_data_same_period$`First Name`[i]
                  & hdap_duplicate_data_same_period$`Last Name`==hdap_duplicate_data_same_period$`Last Name`[i])
  }
  else if(is.na(hdap_duplicate_data_same_period$`First Name`[i])){
    index = which(hdap_duplicate_data_same_period$County==hdap_duplicate_data_same_period$County[i]
                  & hdap_duplicate_data_same_period$`Last Name`==hdap_duplicate_data_same_period$`Last Name`[i]
                  & hdap_duplicate_data_same_period$`Date of Birth`==hdap_duplicate_data_same_period$`Date of Birth`[i])
  }
  else if(is.na(hdap_duplicate_data_same_period$`Last Name`[i])){
    index = which(hdap_duplicate_data_same_period$County==hdap_duplicate_data_same_period$County[i]
                  & hdap_duplicate_data_same_period$`First Name`==hdap_duplicate_data_same_period$`First Name`[i]
                  & hdap_duplicate_data_same_period$`Date of Birth`==hdap_duplicate_data_same_period$`Date of Birth`[i])
  }
  if(i != index[1]){
    next
  }
  min_na = which.min(hdap_duplicate_data_same_period$na_num[index])+i-1
  hdap_duplicate_data_same_period$include[min_na]='y'
  if(min_na == which.max(hdap_duplicate_data_same_period$na_num[index])+i-1){
    hdap_duplicate_data_same_period$include[min_na:(min_na+length(index)-1)] = 'flag'
  }
}

#removing duplicate data from the same reporting period
hdap_duplicate_data_ordered = hdap_duplicate_data_remove(hdap_duplicate_data_ordered,hdap_duplicate_data_same_period)

#after going through some list of duplicates within the same reporting period, run below
hdap_duplicate_data_same_period_include = subset(hdap_duplicate_data_same_period,include=="y")
hdap_duplicate_data_same_period_tbd = subset(hdap_duplicate_data_same_period,include=="flag")
hdap_duplicate_data_ordered = unique(rbind(hdap_duplicate_data_ordered,hdap_duplicate_data_same_period_include[,names(hdap_duplicate_data_ordered)]))
hdap_duplicate_data_ordered = hdap_duplicate_data_ordered[order(hdap_duplicate_data_ordered$`Quarter Start`,decreasing=T),]
# write_xlsx(hdap_duplicate_data_ordered,"HDAP/Cleaning/hdap_duplicate_data_ordered.xlsx")

#using the duplicated() functions again to filter out all the older data
hdap_duplicate_data_include = hdap_duplicate_data_ordered[!(duplicated(hdap_duplicate_data_ordered[c("First Name","Last Name","Date of Birth","Project Start Date")])
                                                            & duplicated(hdap_duplicate_data_ordered[c("First Name","Last Name","Date of Birth","Project Start Date")],fromlast=T)
                                                            & duplicated(hdap_duplicate_data_ordered[c("First Name","Last Name","Date of Birth","Project Start Date")],fromlast=T)
                                                            & duplicated(hdap_duplicate_data_ordered[c("First Name","Last Name","Date of Birth","Project Start Date")],fromlast=T)),]

#including data rows
cat("Finalizing and archiving data...\n")
hdap = unique(rbind(hdap,hdap_duplicate_data_include,hdap_no_start_date_unique))

#changing numeric categorical responses to texts
unknown = paste0(8:9,"-",c("Client Does Not Know","Client Refused"))
na = "99-Data Not Collected"
mf = paste0(0:1,"-",c("Female","Male"))
race = c(paste0(1:6,"-",c("American Indian, Alaska Native, or Indigenous","Asian or Asian American","Black, African American, or african",
                          "Native Hawaiian or Pacific Islander","White","Multiracial")),unknown)
eth = c(paste0(1:2,"-",c("Non-Hispanic/Non-Latino","Hispanic/Latina/E/O")),unknown)
gender = c(mf,"2-Transgender Female","3-Transgender Male","4-Non-Binary",unknown,"10-Another Gender Identity")
sex = c(mf,"2-Non-Binary","9-Decline to State")
so = c(paste0(0:4,"-",c("Straight","Gay/Lesbian","Bisexual","Queer","Another Sexual Orientation")),unknown)
target = paste0(0:1,"-",c("No","Yes"))
vet = c(target,unknown) #same as disabled et al
ls = c(paste0(c(1:7,12:16,18:27),'-',
              c("Emergency Shelter, including hotel or motel paid for with emergency shelter voucher","Transitional Housing for homeless persons","Permanent Housing (other than RRH) for Formerly Homeless Persons",
                "Psychiatric Hospital or other psychiatric Facility","Substance Abuse Treatment Facility or Detox Center","Hospital or other residential non-psychiatric medical Facility",
                "Jail, Prison, or Juvenile detention Facility","Staying or living in a family member's room, apartment, or house","Staying or living in a friend's room, apartment, or house",
                "Hotel or Motel Paid for without Emergency Shelter Voucher","Foster Care home or foster care Group Home","Place Not Meant for Habitation",
                "Safe Haven","Rental by client with VASH subsidy","Rental by client with Other ongoing Housing Subsidy",
                "Owned by client with ongoing Housing Subsidy","Rental by client, no ongoing Housing Subsidy","Owned by client, no ongoing Housing Subsidy",
                "Long-Term Care Facility or Nursing Home","Rental by client with GPD TIP subsidy","Residential Project or Halfway House with No Homeless Criteria",
                "Interim Housing")),unknown)
previous_stay = c(paste0(c(2:5,10:11),"-",c("One week or more, but less than one month","One Month or more, but less than 90 Days",
                  "90 days or more, but less than one year","One year or longer","One Night or Less","Two to six nights")),unknown,na)
homeless_count = c(paste0(1:4,"-",c("One time","Two times","Three Times","Four or more times")),unknown,na)
homeless_length = c(unknown,na,"101-One Month",paste0(102:113,'-',c(2:12,"More than twelve")," Months"))
disabl_type = paste0(1:5,"-",c("SSI/SSP","SSDI","CAPI","Veteran's Benefits","Other"))
denial_reason = paste0(0:11,"-",c("Loss of Contact","Capable to Re/Enter Workforce","Insufficient Medical Evidence",
                                  "Lack of Follow-Through with Treatment Plan","Lack of Follow-Through with Application Process",
                                  "Prior Denial of Benefits","Did Not Meet Disability Criteria","Lack of Work Credits",
                                  "Not Disabled Prior to Last Insured","Excess Resources","Unknown","Other"))
exit = c(paste0(c(1:2,4:7,10:25,28:32),"-",c("Emergency Shelter, including hotel or motel paid for with emergency shelter voucher","Transitional Housing for homeless persons",
                                             "Psychiatric Hospital or other psychiatric Facility","Substance Abuse Treatment Facility or Detox Center","Hospital or other residential non-psychiatric medical Facility",
                                             "Jail, Prison, or Juvenile detention","Rental by client, no ongoing Housing Subsidy","Owned by client, no ongoing Housing Subsidy",
                                             "Staying or living with family, temporary tenure","Staying or living with friends, temporary tenure",
                                             "Hotel or Motel Paid for without Emergency Shelter Voucher","Foster Care home or foster care Group Home","Place Not Meant for Habitation",
                                             "Other","Safe Haven","Rental by client with VASH subsidy","Rental by client with Other ongoing Housing Subsidy","Owned by client with ongoing Housing Subsidy",
                                             "Staying or living with family, permanent tenure","Staying or living with friends, permanent tenure",
                                             "Deceased","Long-Term Care Facility or Nursing Home","Rental by client with GPD TIP subsidy",
                                             "Residential Project or Halfway House with No Homeless Criteria","No Exit Interview Completed",
                                             "Rental by client with RRH or Equivalent Subsidy","Retained Housing (was at risk of homelessness)")),unknown)
hdap = hdap_numeric_responses_to_texts(hdap,"race",race)
hdap = hdap_numeric_responses_to_texts(hdap,"ethni",eth)
hdap = hdap_numeric_responses_to_texts(hdap,"gender i",gender)
hdap = hdap_numeric_responses_to_texts(hdap,"sex ",sex)
hdap = hdap_numeric_responses_to_texts(hdap,"sexual o",so)
hdap = hdap_numeric_responses_to_texts(hdap,c("target pop","experiencing homeless","exit due to"),target)
hdap = hdap_numeric_responses_to_texts(hdap,c("veter","disabling","chronically hom"),vet)
hdap = hdap_numeric_responses_to_texts(hdap,"living situation",ls)
hdap = hdap_numeric_responses_to_texts(hdap,"previous st",previous_stay)
hdap = hdap_numeric_responses_to_texts(hdap,"homeless count",homeless_count)
hdap = hdap_numeric_responses_to_texts(hdap,"length of hom",homeless_length)
hdap = hdap_numeric_responses_to_texts(hdap,"type applied",disabl_type)
hdap = hdap_numeric_responses_to_texts(hdap,"denial$",denial_reason)
hdap = hdap_numeric_responses_to_texts(hdap,"destin",exit)
hdap$`Sexual Orientation` = gsub("/Heterosexual","",hdap$`Sexual Orientation`)
hdap$Race = ifelse(grepl("^American",hdap$Race),"American Indian, Alaska Native, or Indigenous",
                   ifelse(grepl("^Asian",hdap$Race),"Asian or Asian American",
                          ifelse(grepl("^Black",hdap$Race),"Black, African American, or African",
                                 ifelse(grepl("^Native",hdap$Race),"Native Hawaiian or Pacific Islander",
                                        ifelse(grepl("^Client Refu",hdap$Race),"Client prefers not to answer",hdap$Race)))))
hdap$Ethnicity = gsub("^Hispanic/Latino","Hispanic/Latina/E/O",hdap$Ethnicity)
hdap[which(grepl("Towe",hdap$`Last Name`)&grepl("Danny",hdap$`First Name`)),"Date of Birth"]="1954-06-32"

#archiving "cleaned" data
hdap_participant = unique(hdap[,c(grep(" Name$",names(hdap)),grep("Date of B",names(hdap)))])
hdap_participant$`First Name_nows` = trimws(tolower(hdap_participant$`First Name`))
hdap_participant$`Last Name_nows` = trimws(tolower(hdap_participant$`Last Name`))
hdap_participant = unique(merge(hdap_participant,participant_list[,c(1,3:4,grep("^id$",tolower(names(participant_list))))],all.x=T))
hdap_participant = unique(merge(hdap_participant,rbind(unique(hdap_participant_discrepant_backup[which(!is.na(hdap_participant_discrepant_backup$id)),5:8]),
                                                       hdap_backup[,c(grep(" Name$",names(hdap_backup)),grep("Date of Bir",names(hdap_backup)),grep("^id$",tolower(names(hdap_backup))))]),all.x=T))
hdap_participant$ID[which(!is.na(hdap_participant$id)&is.na(hdap_participant$ID))]=hdap_participant$id[which(!is.na(hdap_participant$id)&is.na(hdap_participant$ID))]
hdap_participant$id[which(is.na(hdap_participant$id)&!is.na(hdap_participant$ID))]=hdap_participant$ID[which(is.na(hdap_participant$id)&!is.na(hdap_participant$ID))]
hdap_id_assign = unique(hdap_participant[is.na(hdap_participant$id),c(1:3)])
hdap_id_assign$id = as.character(sample(c(1000000:9999999)[-which(c(1000000:9999999) %in% hdap_participant$id)],nrow(hdap_id_assign)))
hdap_participant = rbind(hdap_participant[-which(paste0(hdap_participant$`First Name`,hdap_participant$`Last Name`,hdap_participant$`Date of Birth`)
                                                   %in% paste0(hdap_id_assign$`First Name`,hdap_id_assign$`Last Name`,hdap_id_assign$`Date of Birth`)),names(hdap_id_assign)],hdap_id_assign)
hdap = merge(hdap,hdap_participant,all.x=T)
hdap$case = 1
hdap = hdap[order(hdap$County,hdap$id,hdap$`Project Start Date`),]
for(i in 2:nrow(hdap)){
  if(hdap$id[i]==hdap$id[i-1]){
    hdap$case[i] = hdap$case[i-1]+1
  }
}
hdap$case = as.character(hdap$case)
hdap$Quarter = NULL
if(length(which(grepl("n/a",tolower(hdap$`First Name`))
                & grepl("n/a",tolower(hdap$`Last Name`))
                & is.na(hdap$`Date of Birth`))>0)){
  hdap = hdap[-which(grepl("n/a",tolower(hdap$`First Name`))
                   & grepl("n/a",tolower(hdap$`Last Name`))
                   & is.na(hdap$`Date of Birth`)),]
}
hdap$age = floor(elapsed_months(hdap$`Project Start Date`,hdap$`Date of Birth`)/12)
hdap$`Social Security Number` = ifelse(is.na(hdap$`Social Security Number`),"000000000",
                                       ifelse(grepl("^[0-9]{1}$",hdap$`Social Security Number`),paste0("00000000",hdap$`Social Security Number`),
                                              ifelse(grepl("^[0-9]{2}$",hdap$`Social Security Number`),paste0("0000000",hdap$`Social Security Number`),
                                                     ifelse(grepl("^[0-9]{3}$",hdap$`Social Security Number`),paste0("000000",hdap$`Social Security Number`),
                                                            ifelse(grepl("^[0-9]{4}$",hdap$`Social Security Number`),paste0("00000",hdap$`Social Security Number`),
                                                                   ifelse(grepl("^[0-9]{5}$",hdap$`Social Security Number`),paste0("0000",hdap$`Social Security Number`),
                                                                          ifelse(grepl("^[0-9]{6}$",hdap$`Social Security Number`),paste0("000",hdap$`Social Security Number`),
                                                                                 ifelse(grepl("^[0-9]{7}$",hdap$`Social Security Number`),paste0("00",hdap$`Social Security Number`),
                                                                                        ifelse(grepl("^[0-9]{8}$",hdap$`Social Security Number`),paste0("0",hdap$`Social Security Number`),hdap$`Social Security Number`)))))))))
hdap$`Social Security Number` = gsub("[a-z]","0",tolower(hdap$`Social Security Number`))
hdap$`Social Security Number` = gsub("^-","0",hdap$`Social Security Number`)
hdap$`Social Security Number` = gsub("-","",hdap$`Social Security Number`)
hdap$`Social Security Number` = gsub(" \\(0+\\)","00",hdap$`Social Security Number`)
hdap$`Social Security Number` = gsub("\\?","0",hdap$`Social Security Number`)

##MAKE SURE YOU PUT THE EXISTING DATA FILE TO "ARCHIVE" FOLDER BEFORE SAVING THIS NEW DATASET
write_xlsx(hdap[order(hdap$County,hdap$`Last Name`,hdap$`First Name`),],paste0("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/HDAP PII 21 Reports/^hdap_cleaned_",Sys.Date(),".xlsx"))
#saving datasets needed for dqr generation
write_xlsx(hdap_new_data_exclude,"//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/HDAP PII 21 Reports/Cleaning In Progress/hdap_new_data_exclude.xlsx")

# #saving the data for those not running cleaning scripts
# for(j in grep("date",tolower(names(hdap)))){
#   hdap[,j] = as.Date(hdap[,j])
# }
# write_xlsx(hdap[order(hdap$County),],paste0("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/HDAP PII 21 Reports/HDAP Data_",Sys.Date(),".xlsx"))
# write_xlsx(hdap[order(hdap$county_name),-c(grep("Name$",names(hdap)),grep("Birth$",names(hdap)),grep("Social Security",names(hdap)))],"S:/Home Safe/Data and Evaluation/UCSF Evaluation/Data for UCSF/Home Safe Data.xlsx")

#clean up
cat("Cleaning up environment...\n")
rm(hdap_no_start_date_duplicate,hdap_no_start_date,hdap_no_start_date_unique,hdap_duplicate_data,
   race,eth,gender,unknown,sex,so,target,vet,mf,ls,previous_stay,homeless_count,homeless_length,
   disabl_type,denial_reason,exit,hdap_duplicate_data_same_period,na,min_na,index,hdap_new_data)
cat("Process completed.\n")