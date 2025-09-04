hdap_old = read.xlsx("//Cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/HDAP PII 21 Reports/Archive/hdap_cleaned_2024-03-08.xlsx",sep.names = " ")
hdap_compare = hdap_old[,c(1:4,30,grep("Permanent Housing",names(hdap_old)))]
#^we don't use project start date for matching bc it can be updated in the newer data..sigh*
names(hdap_compare)[5:ncol(hdap_compare)] = paste0(names(hdap_compare)[5:ncol(hdap_compare)],"_old")
hdap_ich = read.xlsx("//Cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/HDAP PII 21 Reports/Archive/hdap_cleaned_2024-07-16.xlsx",sep.names = " ")
hdap_compare = merge(hdap_compare,hdap_ich[,c(1:4,30,grep("Permanent Housing",names(hdap_ich)))],all.x=T)
hdap_compare = hdap_compare[-which(is.na(hdap_compare$`Housing Move-In Date - Permanent Housing_old`)&is.na(hdap_compare$`Housing Move-In Date - Permanent Housing`)),]
hdap_compare = hdap_compare[-which(hdap_compare$`Housing Move-In Date - Permanent Housing_old`==hdap_compare$`Housing Move-In Date - Permanent Housing`),]
hdap_compare_manual_check = hdap_compare[which(hdap_compare$`Project Start Date_old`!=hdap_compare$`Project Start Date`),]
hdap_compare = hdap_compare[which(hdap_compare$`Project Start Date_old`==hdap_compare$`Project Start Date`),]

#filtering out rows from manual check that don't need to be checked
hdap_compare_manual_check$remove = 0
for(i in 1:nrow(hdap_compare_manual_check)){
  match = which(paste0(hdap_compare_manual_check$County,hdap_compare_manual_check$`Last Name`,hdap_compare_manual_check$`First Name`,hdap_compare_manual_check$`Date of Birth`)
                %in% paste0(hdap_compare_manual_check$County[i],hdap_compare_manual_check$`Last Name`[i],hdap_compare_manual_check$`First Name`[i],hdap_compare_manual_check$`Date of Birth`[i]))
  if(length(match)==1
     | hdap_compare_manual_check$`Project Start Date_old`[i] %in% hdap_compare_manual_check$`Project Start Date`[match][-which(match==i)]){
    hdap_compare_manual_check$remove[i] = 1
  }
}
hdap_compare_manual_check_flag = hdap_compare_manual_check[hdap_compare_manual_check$remove==0,]
hdap_compare_manual_check_flag = merge(hdap_compare_manual_check_flag,hdap_compare_manual_check,all.x=T)
length(which(hdap_compare$`Housing Move-In Date - Permanent Housing_old`<="2017-11-30"&is.na(hdap_compare$`Housing Move-In Date - Permanent Housing`)))
length(which(hdap_compare$`Housing Move-In Date - Permanent Housing`<="2017-11-30"&is.na(hdap_compare$`Housing Move-In Date - Permanent Housing_old`)))
hdap_compare_date2date = hdap_compare[which(!is.na(hdap_compare$`Housing Move-In Date - Permanent Housing_old`)&!is.na(hdap_compare$`Housing Move-In Date - Permanent Housing`)),]
length(which(hdap_compare_date2date$`Housing Move-In Date - Permanent Housing_old`<="2017-11-30"&hdap_compare_date2date$`Housing Move-In Date - Permanent Housing`>"2017-11-30"))
length(which(hdap_compare_date2date$`Housing Move-In Date - Permanent Housing_old`>"2017-11-30"&hdap_compare_date2date$`Housing Move-In Date - Permanent Housing`<="2017-11-30"))
length(which(hdap_compare_date2date$`Housing Move-In Date - Permanent Housing_old`>"2017-11-30"&hdap_compare_date2date$`Housing Move-In Date - Permanent Housing`>"2017-11-30"))
#no date -> date: 334
length(which(is.na(hdap_compare$`Housing Move-In Date - Permanent Housing_old`)&!is.na(hdap_compare$`Housing Move-In Date - Permanent Housing`)
             & hdap_compare$`Housing Move-In Date - Permanent Housing`>"2017-11-30"&hdap_compare$`Housing Move-In Date - Permanent Housing`<="2024-03-31"))
table(hdap_compare[which(is.na(hdap_compare$`Housing Move-In Date - Permanent Housing_old`)&!is.na(hdap_compare$`Housing Move-In Date - Permanent Housing`)
                         & hdap_compare$`Housing Move-In Date - Permanent Housing`>"2017-11-30"&hdap_compare$`Housing Move-In Date - Permanent Housing`<="2024-03-31"),"County"])
#date -> no date: 183
length(which(!is.na(hdap_compare$`Housing Move-In Date - Permanent Housing_old`)&is.na(hdap_compare$`Housing Move-In Date - Permanent Housing`)
             & hdap_compare$`Housing Move-In Date - Permanent Housing_old`>"2017-11-30" & hdap_compare$`Housing Move-In Date - Permanent Housing_old`<="2024-03-31"))
table(hdap_compare[which(is.na(hdap_compare$`Housing Move-In Date - Permanent Housing`) & !is.na(hdap_compare$`Housing Move-In Date - Permanent Housing_old`)
                         & hdap_compare$`Housing Move-In Date - Permanent Housing_old`>"2017-11-30"&hdap_compare$`Housing Move-In Date - Permanent Housing_old`<="2024-03-31"),"County"])
hdap_date_nodate = hdap_compare[which(!is.na(hdap_compare$`Housing Move-In Date - Permanent Housing_old`)&is.na(hdap_compare$`Housing Move-In Date - Permanent Housing`)
                                      & hdap_compare$`Housing Move-In Date - Permanent Housing_old`>"2017-11-30" & hdap_compare$`Housing Move-In Date - Permanent Housing_old`<="2024-03-31"),]
#how many cases no longer exist? -> all cases exist but dates dropped: 183
length(which(!(paste0(hdap_date_nodate$County,hdap_date_nodate$`Last Name`,hdap_date_nodate$`First Name`,hdap_date_nodate$`Date of Birth`,hdap_date_nodate$`Project Start Date`)
             %in% paste0(hdap_ich$County,hdap_ich$`Last Name`,hdap_ich$`First Name`,hdap_ich$`Date of Birth`,hdap_ich$`Project Start Date`))))
table(hdap_date_nodate[which(!(paste0(hdap_date_nodate$County,hdap_date_nodate$`Last Name`,hdap_date_nodate$`First Name`,hdap_date_nodate$`Date of Birth`,hdap_date_nodate$`Project Start Date`)
                               %in% paste0(hdap_ich$County,hdap_ich$`Last Name`,hdap_ich$`First Name`,hdap_ich$`Date of Birth`,hdap_ich$`Project Start Date`))),"County"])
# length(which((paste0(hdap_date_nodate$County,hdap_date_nodate$`Last Name`,hdap_date_nodate$`First Name`,hdap_date_nodate$`Date of Birth`,hdap_date_nodate$`Project Start Date`)
#                %in% paste0(hdap_fy2324_q3_exclude_old$County,hdap_fy2324_q3_exclude_old$`Last Name`,hdap_fy2324_q3_exclude_old$`First Name`,hdap_fy2324_q3_exclude_old$`Date of Birth`,hdap_fy2324_q3_exclude_old$`Project Start Date`))))

##Disability App
#Application A
hdap_compare = hdap_old[,c(1:4,30,grep("Disability Benefit \\(A\\)",names(hdap_old))[2:5])]
names(hdap_compare)[5:9] = paste0(names(hdap_compare)[5:9],"_old")
hdap_compare = merge(hdap_compare,hdap_ich[,c(1:4,30,grep("Disability Benefit \\(A\\)",names(hdap_ich))[2:5])],all.x=T)
hdap_compare1 = hdap_compare[-which(is.na(hdap_compare$`Disability Benefit (A) - Initial Application Submission Date_old`)&is.na(hdap_compare$`Disability Benefit (A) - Initial Application Submission Date`)),c(1:4,grep("^Project",names(hdap_compare)),grep("Initial",names(hdap_compare)))]
hdap_compare1 = hdap_compare1[-which(hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date_old`==hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date`),]
hdap_compare_manual_check = hdap_compare1[which(hdap_compare1$`Project Start Date_old`!=hdap_compare1$`Project Start Date`),]
hdap_compare1 = hdap_compare1[which(hdap_compare1$`Project Start Date_old`==hdap_compare1$`Project Start Date`),]

hdap_compare_manual_check$remove = 0
for(i in 1:nrow(hdap_compare_manual_check)){
  match = which(paste0(hdap_compare_manual_check$County,hdap_compare_manual_check$`Last Name`,hdap_compare_manual_check$`First Name`,hdap_compare_manual_check$`Date of Birth`)
                %in% paste0(hdap_compare_manual_check$County[i],hdap_compare_manual_check$`Last Name`[i],hdap_compare_manual_check$`First Name`[i],hdap_compare_manual_check$`Date of Birth`[i]))
  if(length(match)==1
     | hdap_compare_manual_check$`Project Start Date_old`[i] %in% hdap_compare_manual_check$`Project Start Date`[match][-which(match==i)]){
    hdap_compare_manual_check$remove[i] = 1
  }
}
hdap_compare_manual_check_flag = hdap_compare_manual_check[hdap_compare_manual_check$remove==0,]
hdap_compare_manual_check_flag = merge(hdap_compare_manual_check_flag,hdap_compare_manual_check,all.x=T)
length(which(hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date_old`<="2017-11-30"&is.na(hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date`)))
length(which(hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date`<="2017-11-30"&is.na(hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date_old`)))

#no date -> date
length(which(is.na(hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date_old`)&!is.na(hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date`)))
table(hdap_compare1[which(is.na(hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date_old`)&!is.na(hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date`)),"County"])
#date -> no date
length(which(!is.na(hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date_old`)&is.na(hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date`)))
table(hdap_compare1[which(!is.na(hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date_old`)&is.na(hdap_compare1$`Disability Benefit (A) - Initial Application Submission Date`)),"County"])

hdap_compare2 = hdap_compare[-which(is.na(hdap_compare$`Disability Benefit (A) - Reconsideration Submission Date_old`)&is.na(hdap_compare$`Disability Benefit (A) - Reconsideration Submission Date`)),c(1:4,grep("^Project",names(hdap_compare)),grep("Reconsi",names(hdap_compare)))]
hdap_compare2 = hdap_compare2[-which(hdap_compare2$`Disability Benefit (A) - Reconsideration Submission Date_old`==hdap_compare2$`Disability Benefit (A) - Reconsideration Submission Date`),]
hdap_compare_manual_check = hdap_compare2[which(hdap_compare2$`Project Start Date_old`!=hdap_compare2$`Project Start Date`),]
hdap_compare2 = hdap_compare2[which(hdap_compare2$`Project Start Date_old`==hdap_compare2$`Project Start Date`),]

hdap_compare_manual_check$remove = 0
for(i in 1:nrow(hdap_compare_manual_check)){
  match = which(paste0(hdap_compare_manual_check$County,hdap_compare_manual_check$`Last Name`,hdap_compare_manual_check$`First Name`,hdap_compare_manual_check$`Date of Birth`)
                %in% paste0(hdap_compare_manual_check$County[i],hdap_compare_manual_check$`Last Name`[i],hdap_compare_manual_check$`First Name`[i],hdap_compare_manual_check$`Date of Birth`[i]))
  if(length(match)==1
     | hdap_compare_manual_check$`Project Start Date_old`[i] %in% hdap_compare_manual_check$`Project Start Date`[match][-which(match==i)]){
    hdap_compare_manual_check$remove[i] = 1
  }
}
hdap_compare_manual_check_flag = hdap_compare_manual_check[hdap_compare_manual_check$remove==0,]
hdap_compare_manual_check_flag = merge(hdap_compare_manual_check_flag,hdap_compare_manual_check,all.x=T)
#date -> no date
length(which(is.na(hdap_compare2$`Disability Benefit (A) - Reconsideration Submission Date_old`)&!is.na(hdap_compare2$`Disability Benefit (A) - Reconsideration Submission Date`)))
table(hdap_compare2[which(is.na(hdap_compare2$`Disability Benefit (A) - Reconsideration Submission Date_old`)&!is.na(hdap_compare2$`Disability Benefit (A) - Reconsideration Submission Date`)),"County"])
#no date -> date
length(which(!is.na(hdap_compare2$`Disability Benefit (A) - Reconsideration Submission Date_old`)&is.na(hdap_compare2$`Disability Benefit (A) - Reconsideration Submission Date`)))
table(hdap_compare2[which(!is.na(hdap_compare2$`Disability Benefit (A) - Reconsideration Submission Date_old`)&is.na(hdap_compare2$`Disability Benefit (A) - Reconsideration Submission Date`)),"County"])

hdap_compare3 = hdap_compare[-which(is.na(hdap_compare$`Disability Benefit (A) - Subsequent Benefit Appeal Submission Date_old`)&is.na(hdap_compare$`Disability Benefit (A) - Subsequent Benefit Appeal Submission Date`)),c(1:5,grep("Subseq",names(hdap_compare)))]
hdap_compare3 = hdap_compare3[-which(hdap_compare3$`Disability Benefit (A) - Subsequent Benefit Appeal Submission Date_old`==hdap_compare3$`Disability Benefit (A) - Subsequent Benefit Appeal Submission Date`),]
hdap_compare_manual_check = hdap_compare3[which(hdap_compare3$`Project Start Date_old`!=hdap_compare3$`Project Start Date`),]
hdap_compare3 = hdap_compare3[which(hdap_compare3$`Project Start Date_old`==hdap_compare3$`Project Start Date`),]
#no date -> date
length(which(is.na(hdap_compare3$`Disability Benefit (A) - Subsequent Benefit Appeal Submission Date_old`)&!is.na(hdap_compare3$`Disability Benefit (A) - Subsequent Benefit Appeal Submission Date`)))
table(hdap_compare3[which(is.na(hdap_compare3$`Disability Benefit (A) - Subsequent Benefit Appeal Submission Date_old`)&!is.na(hdap_compare3$`Disability Benefit (A) - Subsequent Benefit Appeal Submission Date`)),"County"])
#date -> no date
length(which(!is.na(hdap_compare3$`Disability Benefit (A) - Subsequent Benefit Appeal Submission Date_old`)&is.na(hdap_compare3$`Disability Benefit (A) - Subsequent Benefit Appeal Submission Date`)))
table(hdap_compare3[which(!is.na(hdap_compare3$`Disability Benefit (A) - Subsequent Benefit Appeal Submission Date_old`)&is.na(hdap_compare3$`Disability Benefit (A) - Subsequent Benefit Appeal Submission Date`)),"County"])

hdap_compare4 = hdap_compare[-which(is.na(hdap_compare$`Disability Benefit (A) - Most Recent Appeal Submission Date_old`)&is.na(hdap_compare$`Disability Benefit (A) - Most Recent Appeal Submission Date`)),c(1:5,grep("Most Rece",names(hdap_compare)))]
hdap_compare4 = hdap_compare4[-which(hdap_compare4$`Disability Benefit (A) - Most Recent Appeal Submission Date_old`==hdap_compare4$`Disability Benefit (A) - Most Recent Appeal Submission Date`),]
hdap_compare_manual_check = hdap_compare4[which(hdap_compare4$`Project Start Date_old`!=hdap_compare4$`Project Start Date`),]
hdap_compare4 = hdap_compare4[which(hdap_compare4$`Project Start Date_old`==hdap_compare4$`Project Start Date`),]
#no date -> date
length(which(is.na(hdap_compare4$`Disability Benefit (A) - Most Recent Appeal Submission Date_old`)&!is.na(hdap_compare4$`Disability Benefit (A) - Most Recent Appeal Submission Date`)))
table(hdap_compare4[which(is.na(hdap_compare4$`Disability Benefit (A) - Most Recent Appeal Submission Date_old`)&!is.na(hdap_compare4$`Disability Benefit (A) - Most Recent Appeal Submission Date`)),"County"])
#date -> no date
length(which(!is.na(hdap_compare4$`Disability Benefit (A) - Most Recent Appeal Submission Date_old`)&is.na(hdap_compare4$`Disability Benefit (A) - Most Recent Appeal Submission Date`)))
table(hdap_compare4[which(!is.na(hdap_compare4$`Disability Benefit (A) - Most Recent Appeal Submission Date_old`)&is.na(hdap_compare4$`Disability Benefit (A) - Most Recent Appeal Submission Date`)),"County"])

#Application B
hdap_compare = hdap_old[,c(1:4,30,grep("Disability Benefit \\(B\\)",names(hdap_old))[2:5])]
names(hdap_compare)[5:9] = paste0(names(hdap_compare)[5:9],"_old")
hdap_compare = merge(hdap_compare,hdap_ich[,c(1:4,30,grep("Disability Benefit \\(B\\)",names(hdap_ich))[2:5])],all.x=T)
hdap_compare1 = hdap_compare[-which(is.na(hdap_compare$`Disability Benefit (B) - Initial Application Submission Date_old`)&is.na(hdap_compare$`Disability Benefit (B) - Initial Application Submission Date`)),c(1:4,grep("^Project",names(hdap_compare)),grep("Initial",names(hdap_compare)))]
hdap_compare1 = hdap_compare1[-which(hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date_old`==hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date`),]
hdap_compare_manual_check = hdap_compare1[which(hdap_compare1$`Project Start Date_old`!=hdap_compare1$`Project Start Date`),]
hdap_compare1 = hdap_compare1[which(hdap_compare1$`Project Start Date_old`==hdap_compare1$`Project Start Date`),]

hdap_compare_manual_check$remove = 0
for(i in 1:nrow(hdap_compare_manual_check)){
  match = which(paste0(hdap_compare_manual_check$County,hdap_compare_manual_check$`Last Name`,hdap_compare_manual_check$`First Name`,hdap_compare_manual_check$`Date of Birth`)
                %in% paste0(hdap_compare_manual_check$County[i],hdap_compare_manual_check$`Last Name`[i],hdap_compare_manual_check$`First Name`[i],hdap_compare_manual_check$`Date of Birth`[i]))
  if(length(match)==1
     | hdap_compare_manual_check$`Project Start Date_old`[i] %in% hdap_compare_manual_check$`Project Start Date`[match][-which(match==i)]){
    hdap_compare_manual_check$remove[i] = 1
  }
}
hdap_compare_manual_check_flag = hdap_compare_manual_check[hdap_compare_manual_check$remove==0,]
hdap_compare_manual_check_flag = merge(hdap_compare_manual_check_flag,hdap_compare_manual_check,all.x=T)
length(which(hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date_old`<="2017-11-30"&is.na(hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date`)))
length(which(hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date`<="2017-11-30"&is.na(hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date_old`)))

#no date -> date
length(which(is.na(hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date_old`)&!is.na(hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date`)))
table(hdap_compare1[which(is.na(hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date_old`)&!is.na(hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date`)),"County"])
#date -> no date
length(which(!is.na(hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date_old`)&is.na(hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date`)))
table(hdap_compare1[which(!is.na(hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date_old`)&is.na(hdap_compare1$`Disability Benefit (B) - Initial Application Submission Date`)),"County"])

hdap_compare2 = hdap_compare[-which(is.na(hdap_compare$`Disability Benefit (B) - Reconsideration Submission Date_old`)&is.na(hdap_compare$`Disability Benefit (B) - Reconsideration Submission Date`)),c(1:5,grep("Reconsid",names(hdap_compare)))]
hdap_compare2 = hdap_compare2[-which(hdap_compare2$`Disability Benefit (B) - Reconsideration Submission Date_old`==hdap_compare2$`Disability Benefit (B) - Reconsideration Submission Date`),]
#no date -> date
length(which(is.na(hdap_compare2$`Disability Benefit (B) - Reconsideration Submission Date_old`)&!is.na(hdap_compare2$`Disability Benefit (B) - Reconsideration Submission Date`)))
table(hdap_compare2[which(is.na(hdap_compare2$`Disability Benefit (B) - Reconsideration Submission Date_old`)&!is.na(hdap_compare2$`Disability Benefit (B) - Reconsideration Submission Date`)),"County"])
#date -> no date
length(which(!is.na(hdap_compare2$`Disability Benefit (B) - Reconsideration Submission Date_old`)&is.na(hdap_compare2$`Disability Benefit (B) - Reconsideration Submission Date`)))
table(hdap_compare2[which(!is.na(hdap_compare2$`Disability Benefit (B) - Reconsideration Submission Date_old`)&is.na(hdap_compare2$`Disability Benefit (B) - Reconsideration Submission Date`)),"County"])

hdap_compare3 = hdap_compare[-which(is.na(hdap_compare$`Disability Benefit (B) - Subsequent Benefit Appeal Submission Date_old`)&is.na(hdap_compare$`Disability Benefit (B) - Subsequent Benefit Appeal Submission Date`)),c(1:5,grep("Subseq",names(hdap_compare)))]
hdap_compare3 = hdap_compare3[-which(hdap_compare3$`Disability Benefit (B) - Subsequent Benefit Appeal Submission Date_old`==hdap_compare3$`Disability Benefit (B) - Subsequent Benefit Appeal Submission Date`),]
hdap_compare_manual_check = hdap_compare3[which(hdap_compare3$`Project Start Date_old`!=hdap_compare3$`Project Start Date`),]

#no date -> date
length(which(is.na(hdap_compare3$`Disability Benefit (B) - Subsequent Benefit Appeal Submission Date_old`)&!is.na(hdap_compare3$`Disability Benefit (B) - Subsequent Benefit Appeal Submission Date`)))
table(hdap_compare3[which(is.na(hdap_compare3$`Disability Benefit (B) - Subsequent Benefit Appeal Submission Date_old`)&!is.na(hdap_compare3$`Disability Benefit (B) - Subsequent Benefit Appeal Submission Date`)),"County"])
#date -> no date
length(which(!is.na(hdap_compare3$`Disability Benefit (B) - Subsequent Benefit Appeal Submission Date_old`)&is.na(hdap_compare3$`Disability Benefit (B) - Subsequent Benefit Appeal Submission Date`)))
table(hdap_compare3[which(!is.na(hdap_compare3$`Disability Benefit (B) - Subsequent Benefit Appeal Submission Date_old`)&is.na(hdap_compare3$`Disability Benefit (B) - Subsequent Benefit Appeal Submission Date`)),"County"])

hdap_compare4 = hdap_compare[-which(is.na(hdap_compare$`Disability Benefit (B) - Most Recent Appeal Submission Date_old`)&is.na(hdap_compare$`Disability Benefit (B) - Most Recent Appeal Submission Date`)),c(1:5,grep("Most Rece",names(hdap_compare)))]
hdap_compare4 = hdap_compare4[-which(hdap_compare4$`Disability Benefit (B) - Most Recent Appeal Submission Date_old`==hdap_compare4$`Disability Benefit (B) - Most Recent Appeal Submission Date`),]
hdap_compare_manual_check = hdap_compare4[which(hdap_compare4$`Project Start Date_old`!=hdap_compare4$`Project Start Date`),]
#no date -> date
length(which(is.na(hdap_compare4$`Disability Benefit (B) - Most Recent Appeal Submission Date_old`)&!is.na(hdap_compare4$`Disability Benefit (B) - Most Recent Appeal Submission Date`)))
table(hdap_compare4[which(is.na(hdap_compare4$`Disability Benefit (B) - Most Recent Appeal Submission Date_old`)&!is.na(hdap_compare4$`Disability Benefit (B) - Most Recent Appeal Submission Date`)),"County"])
#date -> no date
length(which(!is.na(hdap_compare4$`Disability Benefit (B) - Most Recent Appeal Submission Date_old`)&is.na(hdap_compare4$`Disability Benefit (B) - Most Recent Appeal Submission Date`)))
table(hdap_compare4[which(!is.na(hdap_compare4$`Disability Benefit (B) - Most Recent Appeal Submission Date_old`)&is.na(hdap_compare4$`Disability Benefit (B) - Most Recent Appeal Submission Date`)),"County"])

hdap_compare = hdap_old[,c(1:4,30,grep("Approval Date$",names(hdap_old)))]
names(hdap_compare)[5:7] = paste0(names(hdap_compare)[5:7],"_old")
hdap_compare = merge(hdap_compare,hdap_ich[,c(1:4,30,grep("Approval Date$",names(hdap_ich)))],all.x=T)
hdap_compare_a = hdap_compare[-which(is.na(hdap_compare$`Disability Benefit (A) - Approval Date_old`)&is.na(hdap_compare$`Disability Benefit (A) - Approval Date`)),c(1:5,grep("\\(A",names(hdap_compare)))]
hdap_compare_a = hdap_compare_a[-which(hdap_compare_a$`Disability Benefit (A) - Approval Date_old`==hdap_compare_a$`Disability Benefit (A) - Approval Date`),]
hdap_compare_manual_check = hdap_compare_a[which(hdap_compare_a$`Project Start Date_old`!=hdap_compare_a$`Project Start Date`),]
#no date -> date
length(which(is.na(hdap_compare_a$`Disability Benefit (A) - Approval Date_old`)&!is.na(hdap_compare_a$`Disability Benefit (A) - Approval Date`)))
#date -> no date
length(which(!is.na(hdap_compare_a$`Disability Benefit (A) - Approval Date_old`)&is.na(hdap_compare_a$`Disability Benefit (A) - Approval Date`)))
#date -> date
length(which(!is.na(hdap_compare_a$`Disability Benefit (A) - Approval Date_old`)&!is.na(hdap_compare_a$`Disability Benefit (A) - Approval Date`)))
which(hdap_compare_a$`Disability Benefit (A) - Approval Date_old`<="2017-11-30"&hdap_compare_a$`Disability Benefit (A) - Approval Date`<="2017-11-30")
which(hdap_compare_a$`Disability Benefit (A) - Approval Date_old`<="2017-11-30"&hdap_compare_a$`Disability Benefit (A) - Approval Date`>"2017-11-30")
which(hdap_compare_a$`Disability Benefit (A) - Approval Date_old`>"2017-11-30"&hdap_compare_a$`Disability Benefit (A) - Approval Date`<="2017-11-30")
which(hdap_compare_a$`Disability Benefit (A) - Approval Date_old`>"2017-11-30"&hdap_compare_a$`Disability Benefit (A) - Approval Date`>"2017-11-30")

hdap_compare_b = hdap_compare[-which(is.na(hdap_compare$`Disability Benefit (B) - Approval Date_old`)&is.na(hdap_compare$`Disability Benefit (B) - Approval Date`)),c(1:5,grep("\\(B",names(hdap_compare)))]
hdap_compare_b = hdap_compare_b[-which(hdap_compare_b$`Disability Benefit (B) - Approval Date_old`==hdap_compare_b$`Disability Benefit (B) - Approval Date`),]
length(which(is.na(hdap_compare_b$`Disability Benefit (B) - Approval Date_old`)&!is.na(hdap_compare_b$`Disability Benefit (B) - Approval Date`)))
length(which(!is.na(hdap_compare_b$`Disability Benefit (B) - Approval Date_old`)&is.na(hdap_compare_b$`Disability Benefit (B) - Approval Date`)))
length(which(!is.na(hdap_compare_b$`Disability Benefit (B) - Approval Date_old`)&!is.na(hdap_compare_b$`Disability Benefit (B) - Approval Date`)))
length(which(hdap_compare_b$`Disability Benefit (B) - Approval Date_old`<="2017-11-30"&hdap_compare_b$`Disability Benefit (B) - Approval Date`<="2017-11-30"))
lenngth(which(hdap_compare_b$`Disability Benefit (B) - Approval Date_old`<="2017-11-30"&hdap_compare_b$`Disability Benefit (B) - Approval Date`>"2017-11-30"))
length(which(hdap_compare_b$`Disability Benefit (B) - Approval Date_old`>"2017-11-30"&hdap_compare_b$`Disability Benefit (B) - Approval Date`<="2017-11-30"))
length(which(hdap_compare_b$`Disability Benefit (B) - Approval Date_old`>"2017-11-30"&hdap_compare_b$`Disability Benefit (B) - Approval Date`>"2017-11-30"))
