library(readxl)
#obtain the most recent dataset -> make sure to update the data set
hsaps_ich = read_xlsx("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/^hsaps_cleaned_2024-05-09.xlsx")
hsaps = hsaps_ich

#num received services -> make sure to change the date properly before running the code
#FOR WITHIN A FISCAL YEAR; please make sure to change the date properly before running the code
hsaps_ich$intervention_received_fy = "No"
for(i in 1:nrow(hsaps_ich)){
  for(j in grep("Intervention [0-9]+ - Date",names(hsaps_ich))){
    hsaps_ich$intervention_received_fy[i] = ifelse(is.na(hsaps_ich[i,j]),hsaps_ich$intervention_received_fy[i],
                                                ifelse(!is.na(hsaps_ich[i,(j-2)])
                                                       & hsaps_ich[i,(j-2)] != "No Intervention"
                                                       & hsaps_ich[i,j]<="2022-06-30"
                                                       & hsaps_ich[i,j]>"2021-06-30",
                                                       "Yes",hsaps_ich$intervention_received_fy[i]))
  }
}
length(which(hsaps_ich$intervention_received_fy=="Yes"))

#SINCE IMPLEMENTATION; please make sure to change the date properly before running the code
hsaps_ich$intervention_received_overall = "No"
for(i in 1:nrow(hsaps_ich)){
  for(j in grep("Intervention [0-9]+ - Date",names(hsaps_ich))){
    hsaps_ich$intervention_received_overall[i] = ifelse(is.na(hsaps_ich[i,j]),hsaps_ich$intervention_received_overall[i],
                                                        ifelse(!is.na(hsaps_ich[i,(j-2)])
                                                        & hsaps_ich[i,(j-2)] != "No Intervention"
                                                        & hsaps_ich[i,j]<="2023-06-30",
                                                        "Yes",hsaps_ich$intervention_received_overall[i]))
  }
}
length(which(hsaps_ich$intervention_received_overall=="Yes"))

#num of instances receiving financial assistance for FISCAL YEAR-> rent, mortgage, deposit, rent back
hsaps_ich_intervention = subset(hsaps_ich,intervention_received_fy=="Yes")
length(grep("rent",tolower(hsaps_ich_intervention$`Intervention 1 - Type`)))+length(grep("deposit",tolower(hsaps_ich_intervention$`Intervention 1 - Type`)))+
  length(grep("rent",tolower(hsaps_ich_intervention$`Intervention 2 - Type`)))+length(grep("deposit",tolower(hsaps_ich_intervention$`Intervention 2 - Type`)))+
  length(grep("rent",tolower(hsaps_ich_intervention$`Intervention 3 - Type`)))+length(grep("deposit",tolower(hsaps_ich_intervention$`Intervention 3 - Type`)))+
  length(grep("rent",tolower(hsaps_ich_intervention$`Intervention 4 - Type`)))+length(grep("deposit",tolower(hsaps_ich_intervention$`Intervention 4 - Type`)))+
  length(grep("rent",tolower(hsaps_ich_intervention$`Intervention 5 - Type`)))+length(grep("deposit",tolower(hsaps_ich_intervention$`Intervention 5 - Type`)))+
  length(grep("rent",tolower(hsaps_ich_intervention$`Intervention 6 - Type`)))+length(grep("deposit",tolower(hsaps_ich_intervention$`Intervention 6 - Type`)))+
  length(grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 1 - Type`)))+length(grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 2 - Type`)))+length(grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 3 - Type`)))+
  length(grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 4 - Type`)))+length(grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 5 - Type`)))+length(grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 6 - Type`)))
table(hsaps_ich_intervention[unique(c(grep("rent",tolower(hsaps_ich_intervention$`Intervention 1 - Type`)),grep("deposit",tolower(hsaps_ich_intervention$`Intervention 1 - Type`)),
                                      grep("rent",tolower(hsaps_ich_intervention$`Intervention 2 - Type`)),grep("deposit",tolower(hsaps_ich_intervention$`Intervention 2 - Type`)),
                                      grep("rent",tolower(hsaps_ich_intervention$`Intervention 3 - Type`)),grep("deposit",tolower(hsaps_ich_intervention$`Intervention 3 - Type`)),
                                      grep("rent",tolower(hsaps_ich_intervention$`Intervention 4 - Type`)),grep("deposit",tolower(hsaps_ich_intervention$`Intervention 4 - Type`)),
                                      grep("rent",tolower(hsaps_ich_intervention$`Intervention 5 - Type`)),grep("deposit",tolower(hsaps_ich_intervention$`Intervention 5 - Type`)),
                                      grep("rent",tolower(hsaps_ich_intervention$`Intervention 6 - Type`)),grep("deposit",tolower(hsaps_ich_intervention$`Intervention 6 - Type`)),
                                      grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 1 - Type`)),grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 2 - Type`)),grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 3 - Type`)),
                                      grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 4 - Type`)),grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 5 - Type`)),grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 6 - Type`)))),"Reporting_Agency"])

#num of instances receiving financial assistance for OVERALL-> rent, mortgage, deposit, rent back
hsaps_ich_intervention = subset(hsaps_ich,intervention_received_overall=="Yes")
length(grep("rent",tolower(hsaps_ich_intervention$`Intervention 1 - Type`)))+length(grep("deposit",tolower(hsaps_ich_intervention$`Intervention 1 - Type`)))+
  length(grep("rent",tolower(hsaps_ich_intervention$`Intervention 2 - Type`)))+length(grep("deposit",tolower(hsaps_ich_intervention$`Intervention 2 - Type`)))+
  length(grep("rent",tolower(hsaps_ich_intervention$`Intervention 3 - Type`)))+length(grep("deposit",tolower(hsaps_ich_intervention$`Intervention 3 - Type`)))+
  length(grep("rent",tolower(hsaps_ich_intervention$`Intervention 4 - Type`)))+length(grep("deposit",tolower(hsaps_ich_intervention$`Intervention 4 - Type`)))+
  length(grep("rent",tolower(hsaps_ich_intervention$`Intervention 5 - Type`)))+length(grep("deposit",tolower(hsaps_ich_intervention$`Intervention 5 - Type`)))+
  length(grep("rent",tolower(hsaps_ich_intervention$`Intervention 6 - Type`)))+length(grep("deposit",tolower(hsaps_ich_intervention$`Intervention 6 - Type`)))+
  length(grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 1 - Type`)))+length(grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 2 - Type`)))+length(grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 3 - Type`)))+
  length(grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 4 - Type`)))+length(grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 5 - Type`)))+length(grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 6 - Type`)))
table(hsaps_ich_intervention[unique(c(grep("rent",tolower(hsaps_ich_intervention$`Intervention 1 - Type`)),grep("deposit",tolower(hsaps_ich_intervention$`Intervention 1 - Type`)),
                                      grep("rent",tolower(hsaps_ich_intervention$`Intervention 2 - Type`)),grep("deposit",tolower(hsaps_ich_intervention$`Intervention 2 - Type`)),
                                      grep("rent",tolower(hsaps_ich_intervention$`Intervention 3 - Type`)),grep("deposit",tolower(hsaps_ich_intervention$`Intervention 3 - Type`)),
                                      grep("rent",tolower(hsaps_ich_intervention$`Intervention 4 - Type`)),grep("deposit",tolower(hsaps_ich_intervention$`Intervention 4 - Type`)),
                                      grep("rent",tolower(hsaps_ich_intervention$`Intervention 5 - Type`)),grep("deposit",tolower(hsaps_ich_intervention$`Intervention 5 - Type`)),
                                      grep("rent",tolower(hsaps_ich_intervention$`Intervention 6 - Type`)),grep("deposit",tolower(hsaps_ich_intervention$`Intervention 6 - Type`)),
                                      grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 1 - Type`)),grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 2 - Type`)),grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 3 - Type`)),
                                      grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 4 - Type`)),grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 5 - Type`)),grep("mortgage",tolower(hsaps_ich_intervention$`Intervention 6 - Type`)))),"Reporting_Agency"])

#filter by required time frame -> make sure to change the date properly berfore running the code
hsaps_ich = subset(hsaps,`Case Start Date`<="2023-06-30")
nrow(unique(hsaps_ich[,c(1,grep("Name$",names(hsaps_ich)),grep("Birth$",names(hsaps_ich)))]))
hsaps_ich = subset(hsaps,`Case Start Date`<="2023-06-30"&`Case Start Date`>"2022-06-30")
nrow(unique(hsaps_ich[,c(1,grep("Name$",names(hsaps_ich)),grep("Birth$",names(hsaps_ich)))]))

#num referred to CES -> somehow don't filter by case start date
length(grep("^y",tolower(hsaps_ich$`Client Referred to CES`)))

#retained housing after 6 months
hsaps_ich_6mos_followup = subset(hsaps,`Case Closure Date`<="2023-03-31")
table(hsaps_ich_6mos_followup$`Six Month Follow-Up - Living Situation`)

rm(hsaps_ich_6mos_followup,hsaps_ich_intervention)
