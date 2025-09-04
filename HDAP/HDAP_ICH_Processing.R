library(openxlsx)
library(sqldf)
hdap_ich = read.xlsx("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/HDAP PII 21 Reports/^hdap_cleaned_2024-07-03.xlsx",sep.names = " ")
hdap_ich = subset(hdap,`Project Start Date`<="2024-03-31")
length(which(hdap_ich$`Housing Move-In Date - Permanent Housing`<="2024-03-31"
             & hdap_ich$`Housing Move-In Date - Permanent Housing`>"2017-11-30"))

#num of disability app
length(which(!is.na(hdap_ich$`Disability Benefit (A) - Type Applied For`)
             & !is.na(hdap_ich$`Disability Benefit (A) - Initial Application Submission Date`))) +
  length(which(!is.na(hdap_ich$`Disability Benefit (B) - Type Applied For`)
               & !is.na(hdap_ich$`Disability Benefit (B) - Initial Application Submission Date`)))

#num of disability app approved
length(which(hdap_ich$`Disability Benefit (A) - Approval Date`<="2024-03-31"))+length(which(hdap_ich$`Disability Benefit (B) - Approval Date`<="2024-03-31"))

rm(hdap_ich)
