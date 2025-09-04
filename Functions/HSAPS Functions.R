hsaps_exporting_data_with_proper_col_types = function(dt_name,report_date){
  library(readxl)
  library(stringr)
  cat("Processing the data for sheet:", dt_name, "\n")
  dt = read_excel(paste0("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/",dt_name),sheet = "Data")
  coltype = as.character(dt[1,])
  coltype = tolower(coltype)
  coltype = gsub("new","text",coltype)
  coltype = gsub("4 digit ","",coltype)
  coltype = ifelse(is.na(coltype),"text",ifelse(grepl("integer",coltype),"numeric",coltype))
  dt_new = read_excel(paste0("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/",dt_name),sheet = "Data",col_types=coltype)[-1,]
  names(dt_new)[1:2] = as.character(dt[2,1:2])
  dt_new = dt_new[-1,]
  dt_new = data.frame(dt_new,check.names = F)
  names(dt_new) = gsub("^Item [0-9]+:\r\n","",names(dt_new))
  names(dt_new) = gsub("\r\n","",names(dt_new))
  names(dt_new) = gsub("^Item [0-9]+: ","",names(dt_new))
  names(dt_new) = gsub(" \\(optional\\)$","",names(dt_new))
  names(dt_new) = gsub("^Home Safe ","",names(dt_new)) #to match with var name "Case Start Date"
  names(dt_new) = gsub("^HSAPS ","",names(dt_new))
  names(dt_new) = gsub("Home Safe ","",names(dt_new))
  names(dt_new) = gsub("\\?$","",names(dt_new))
  names(dt_new) = gsub("Home Safe$","HSAPS",names(dt_new))
  names(dt_new) = gsub(" to HSAPS$","",names(dt_new))
  names(dt_new) = gsub("-up","-Up",names(dt_new))
  names(dt_new) = gsub("  -"," -",names(dt_new))
  names(dt_new) = gsub("/\\s","/",names(dt_new))
  names(dt_new) = gsub("\\s\\s","\\s",names(dt_new))
  names(dt_new) = gsub("by Other-","by Other -",names(dt_new))
  names(dt_new) = gsub("from be","from Be",names(dt_new))
  names(dt_new) = gsub("Reported Incident","Report",names(dt_new))
  dt_new$Report_Month = report_date
  dt_new$Reporting_Agency = str_to_title(dt_new$Reporting_Agency)
  dt_new$`First Name` = str_to_title(dt_new$`First Name`)
  dt_new$`Last Name` = str_to_title(dt_new$`Last Name`)
  return(dt_new)
}

hsaps_gate_keeping = function(dt,new_dt_name,new_dt_quarter){
  cat("Running the gate-keeping function...\n")
  new_dt = hsaps_exporting_data_with_proper_col_types(new_dt_name,new_dt_quarter)
  new_dt = new_dt[!is.na(new_dt$Reporting_Agency),]
  cat("Formatting date fields...\n")
  new_dt$`Date of Birth` = gsub(" [0-9]+:[0-9]+:[0-9]+$","",as.character(new_dt$`Date of Birth`))
  new_dt$`Case Start Date` = gsub(" [0-9]+:[0-9]+:[0-9]+$","",as.character(new_dt$`Case Start Date`))
  new_dt$`Case Closure Date` = gsub(" [A-Z,a-z]+$","",as.character(new_dt$`Case Closure Date`))
  new_dt$`APS Report Date` = gsub(" [0-9]+:[0-9]+:[0-9]+$","",as.character(new_dt$`APS Report Date`))
  
  #part 1: sorting through cases btw no case start dates to yes case start dates
  cat("Sorting through cases with updated Project Start Dates...\n")
  csd_updated = c()
  dt_no_csd = dt[is.na(dt$`Case Start Date`),]
  dt_yes_csd = dt[!is.na(dt$`Case Start Date`),]
  cat("Checking for matches with missing Project Start Dates...\n")
  for(i in 1:nrow(dt_no_csd)){
    match = which(tolower(paste0(new_dt$Reporting_Agency,new_dt$`Last Name`,new_dt$`First Name`,new_dt$`Date of Birth`))
                  %in% tolower(paste0(dt_no_csd$Reporting_Agency[i],dt_no_csd$`Last Name`[i],dt_no_csd$`First Name`[i],dt_no_csd$`Date of Birth`[i])))
    if(length(match)==0){
      #new participant without case start date!
      csd_updated = c(csd_updated,i)
    }
    else if(length(match)>1){
      if(dt_no_csd$Report_Month[i]>max(new_dt$Report_Month[match])
         & !is.na(dt_no_csd$`Case Closure Date`[i])
         & all(is.na(new_dt$`Case Closure Date`[match]))){
        csd_updated = c(csd_updated,i)
      }
    }
    else{
      if(dt_no_csd$Report_Month[i]>new_dt$Report_Month[match]
         & !is.na(dt_no_csd$`Case Closure Date`[i])
         & is.na(new_dt$`Case Closure Date`[match])){
        csd_updated = c(csd_updated,i)
      }
    }
  }
  if(length(csd_updated)>0){
    cat("Updating the dataset with cases having missing Project Start Dates...\n")
    dt_no_csd_removed = rbind(dt_yes_csd,dt_no_csd[csd_updated,])
  }
  
  #part 2: Gate Keeping
  cat("Performing gate-keeping checks on the new dataset...\n")
  match_to_exclude = c()
  index_to_include = c()
  index_to_exclude = c()
  for(i in 1:nrow(new_dt)){
    match_target = tolower(paste0(new_dt$Reporting_Agency[i],new_dt$`First Name`[i],new_dt$`Last Name`[i],new_dt$`Date of Birth`[i]))
    match = which(tolower(paste0(dt_no_csd_removed$Reporting_Agency,dt_no_csd_removed$`First Name`,dt_no_csd_removed$`Last Name`,dt_no_csd_removed$`Date of Birth`))==match_target)
    #if there is no match
    if(length(match)==0){
      #sorting through cases with updated dob's (either Jan 1st to an updated date or no date to an updated date)
      match_wo_dob = which(tolower(paste0(dt_no_csd_removed$Reporting_Agency,dt_no_csd_removed$`First Name`,dt_no_csd_removed$`Last Name`,dt_no_csd_removed$`Case Start Date`))==tolower(paste0(new_dt$Reporting_Agency[i],new_dt$`First Name`[i],new_dt$`Last Name`[i],new_dt$`Case Start Date`[i])))
      #if there is match_wo_dob but no match
      if(length(match_wo_dob)>0){
        #before doing so, however, need to check if dob was updated
        for(m in 1:length(match_wo_dob)){
          if((!grepl("01-01$",new_dt$`Date of Birth`[i]) & grepl("01-01$",dt_no_csd_removed$`Date of Birth`[match_wo_dob[m]]))
             | (is.na(dt_no_csd_removed$`Date of Birth`[match_wo_dob[m]]) & !is.na(new_dt$`Date of Birth`[i]))){
            dt_no_csd_removed$`Date of Birth`[match_wo_dob[m]]=new_dt$`Date of Birth`[i]
          }
        }
      }
    }
    #after updating DOB, let's see if there is a match
    match = which(tolower(paste0(dt_no_csd_removed$Reporting_Agency,dt_no_csd_removed$`First Name`,dt_no_csd_removed$`Last Name`,dt_no_csd_removed$`Date of Birth`))==match_target)
    #if there still isn't a match,
    if(length(match)==0){
      #need to check if there is typo
      if((tolower(paste0(new_dt$Reporting_Agency[i],new_dt$`First Name`[i],new_dt$`Date of Birth`[i])) %in%
          tolower(paste0(dt_no_csd_removed$Reporting_Agency,dt_no_csd_removed$`First Name`,dt_no_csd_removed$`Date of Birth`)))
         | (tolower(paste0(new_dt$Reporting_Agency[i],new_dt$`Date of Birth`[i],new_dt$`Last Name`[i])) %in% 
            tolower(paste0(dt_no_csd_removed$Reporting_Agency,dt_no_csd_removed$`Date of Birth`,dt_no_csd_removed$`Last Name`)))
         | (tolower(paste0(new_dt$Reporting_Agency[i],new_dt$`First Name`[i],new_dt$`Last Name`[i])) %in% 
            tolower(paste0(dt_no_csd_removed$Reporting_Agency,dt_no_csd_removed$`First Name`,dt_no_csd_removed$`Last Name`)))){
        index_to_exclude = c(index_to_exclude,i)
      }
      #include new participants!
      if(!(i %in% index_to_exclude)){
        index_to_include = c(index_to_include,i)
      }
    }
    
    #if there is at least one match btw the newest report and the clean data set
    else if(length(match)>0){
      for(m in 1:length(match)){
        #check if the cases are the same even with different case start dates -> remove the older row
        if((!is.na(new_dt$`Case Start Date`[i])
            & !is.na(dt_no_csd_removed$`Case Closure Date`[match[m]])
            & !is.na(new_dt$`Case Closure Date`[i])
            & dt_no_csd_removed$`Case Start Date`[match[m]]!=new_dt$`Case Start Date`[i]
            & dt_no_csd_removed$`Case Closure Date`[match[m]]==new_dt$`Case Closure Date`[i])
           | (!is.na(new_dt$`Case Start Date`[i])
              & !is.na(dt_no_csd_removed$`APS Report Date`[match[m]])
              & !is.na(new_dt$`APS Report Date`[i])
              & dt_no_csd_removed$`Case Start Date`[match[m]]!=new_dt$`Case Start Date`[i]
              & dt_no_csd_removed$`APS Report Date`[match[m]]==new_dt$`APS Report Date`[i])
           | (!is.na(new_dt$`Case Start Date`[i])
              & dt_no_csd_removed$`Case Start Date`[match[m]]!=new_dt$`Case Start Date`[i]
              & !is.na(dt_no_csd_removed$`Case Closure Date`[match[m]])
              & new_dt$`Case Start Date`[i]<dt_no_csd_removed$`Case Closure Date`[match[m]])
           | (!is.na(new_dt$`Case Start Date`[i])
              & dt_no_csd_removed$`Case Start Date`[match[m]]!=new_dt$`Case Start Date`[i]
              & is.na(dt_no_csd_removed$`Case Closure Date`[match[m]])
              & !is.na(new_dt$`Case Closure Date`[i]))){
          # case_start_date_disc = c(case_start_date_disc,i)
          match_to_exclude = c(match_to_exclude,match[m])
        }
      }
      index_to_include = c(index_to_include,i)
    }
  }
  
  cat("Finalizing included and excluded datasets...\n")
  if(length(match_to_exclude)>0){
    dt_no_csd_removed = dt_no_csd_removed[-match_to_exclude,]
  }
  new_dt_include = new_dt[unique(index_to_include),]
  new_dt_exclude = new_dt[unique(index_to_exclude),]
  # new_dt_case_start_date_disc = new_dt[unique(case_start_date_disc),]
  return(list(dt_no_csd_removed,new_dt_include,new_dt_exclude))
}

hsaps_compiling_monthly_reports = function(compiled_dt,file_list){
  library(plyr)
  library(stringr)
  for(l in 1:length(file_list)){
    dt = read_excel(paste0("P:/Reports/HSAPS19/05_Deliverables/Data Table Releases/Recompiled 20230627/",file_list[[l]][1]),
                    col_types = c("text", "date", "text","text", "numeric", "date", "text", 
                                  "text", "numeric", "text", "text","text", "text", "text", "text", "numeric", 
                                  "text", "text", "text", "text", "text","text", "text", "numeric", "numeric", 
                                  "numeric", "text", "numeric", "text","text", "text", "numeric", "text",
                                  "text", "text", "text", "text", "text","text", "text", "text", "text", "text",
                                  "text", "text", "text", "text", "text","text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text","text", "text", "text", "date", "text", 
                                  "text", "text", "text", "text", "text","text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "numeric","text", "text", "text", "text", "text", 
                                  "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                  "numeric", "date", "text", "date", "numeric", "text", "text", "date", 
                                  "numeric", "text", "text", "text", "text", "date", "numeric", "text", 
                                  "text", "text", "text", "date", "text", "text", "text", "text", "text", "date", 
                                  "numeric", "text", "text", "text", "text", "date", "numeric", "text", 
                                  "text", "text", "text", "date", "numeric", "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", "date", "text", "date", "text", "text", 
                                  "text", "text", "date", "text", "text", "text", "numeric", "text"))
    compiled_dt = rbind.fill(compiled_dt,dt)
  }
  compiled_dt = unique(compiled_dt)
  compiled_dt$Reporting_Agency = str_to_title(compiled_dt$`Reporting Agency`)
  compiled_dt$`Reporting Agency` = NULL
  compiled_dt$`First Name` = str_to_title(compiled_dt$`First Name`)
  compiled_dt$`Last Name` = str_to_title(compiled_dt$`Last Name`)
  names(compiled_dt) = gsub("\r\n","",names(compiled_dt))
  names(compiled_dt) = gsub("^Home Safe ","",names(compiled_dt)) #to match with var name "Case Start Date"
  names(compiled_dt) = gsub("^HSAPS ","",names(compiled_dt))
  names(compiled_dt) = gsub("Home Safe ","",names(compiled_dt))
  names(compiled_dt) = gsub("Home Safe$","HSAPS",names(compiled_dt))
  names(compiled_dt) = gsub(" to HSAPS$","",names(compiled_dt))
  names(compiled_dt) = gsub("-up","-Up",names(compiled_dt))
  names(compiled_dt) = gsub("  -"," -",names(compiled_dt))
  names(compiled_dt) = gsub("by Other-","by Other -",names(compiled_dt))
  names(compiled_dt) = gsub("from be","from Be",names(compiled_dt))
  names(compiled_dt) = gsub("Reported Incident","Report",names(compiled_dt))
  names(compiled_dt) = gsub("/\\s","/",names(compiled_dt))
  return(compiled_dt)
}

hsaps_compiling_quarterly_reports = function(dt,file_list){
  library(plyr)
  library(stringr)
  for(l in 1:length(file_list)){
    hsaps_dt = hsaps_exporting_data_with_proper_col_types(file_list[[l]][1],file_list[[l]][2])
    dt = rbind.fill(dt,hsaps_dt)
  }
  dt = unique(dt)
  return(dt)
}

hsaps_duplicate_data_consolidate = function(dt){
  duplicate = data.frame()
  for(i in 1:nrow(dt)){
    duplicate_fnln_check = grep(paste0('^',dt$`First Name`[i],dt$`Last Name`[i],'$'),paste0(dt$`First Name`,dt$`Last Name`)) #dt$id[(i+1):nrow(dt)]?
    #print(duplicate_fnln_check)
    if(length(duplicate_fnln_check) == 1){
      i = i+1
    }
    else{
      for(j in 1:(length(duplicate_fnln_check)-1)){
        #checking the project dates among duplicate id's
        duplicate_pd_check = grep(paste0('^',as.character(dt$`Case Start Date`[duplicate_fnln_check[j]]),'$'),as.character(dt$`Case Start Date`[duplicate_fnln_check]))
        #checking the dobs among duplicate id's
        duplicate_dob_check = grep(paste0('^',as.character(dt$`Date of Birth`[duplicate_fnln_check[j]]),'$'),as.character(dt$`Date of Birth`[duplicate_fnln_check]))
        #if the visit or age is not unique, pull them out
        if(length(duplicate_pd_check)>1 & length(duplicate_dob_check)>1){
          #use the var that has the least num of na's for "var a %in% var b" code
          dup_index = which(duplicate_dob_check %in% duplicate_pd_check)
          duplicate = rbind(duplicate,dt[duplicate_fnln_check[dup_index],])
        }
      }
    }
    duplicate = unique(duplicate)
  }
  return(duplicate)
}

hsaps_duplicate_data_remove = function(dt,duplicate_dt){
  duplicate = duplicate_dt
  if(nrow(duplicate)>0 | !all(is.na(duplicate))){
    duplicate_index = which(paste0(dt$Reporting_Agency,dt$`First Name`,dt$`Last Name`,dt$`Case Start Date`,dt$`Date of Birth`)
                            %in% paste0(duplicate$Reporting_Agency,duplicate$`First Name`,duplicate$`Last Name`,duplicate$`Case Start Date`,duplicate$`Date of Birth`))
    #print(duplicate_index)
    dt = dt[-duplicate_index,]
  }
  return(dt)
}

hsaps_duplicate_rows_check = function(dt,discrepant_dt){
  dt[,82] = gsub("-"," ",dt[,82])
  for(i in seq(1,nrow(dt),by=2)){
    different = which(tolower(dt[i,])!=tolower(dt[(i+1),]))
    if(length(different)>=1){
      discrepant_row = data.frame(i,different)
      discrepant_dt = rbind.fill(discrepant_dt,discrepant_row)
    }
  }
  return(discrepant_dt)
}

hsaps_enrolled_num = function(dt,begin,end){
  #num of individuals = number of people served
  #num of cases = number of cases served
  #num of perm housed = case closure date exists & living situation at exit says, "perm housing", "others", 
  dt_enrolled = subset(dt,`Case Start Date`>begin&`Case Start Date`<=end&!grepl("Select",Reporting_Agency))
  dt_individual = unique(dt_enrolled[!is.na(dt_enrolled$`Case Start Date`),c("Reporting_Agency","First Name","Last Name","Date of Birth")])
  dt_individual_past = subset(dt,`Case Start Date`<=begin)
  dt_individual_new = dt_individual[which(!(paste0(dt_individual$Reporting_Agency,dt_individual$`First Name`,dt_individual$`Last Name`,dt_individual$`Date of Birth`)
                                            %in% paste0(dt_individual_past$Reporting_Agency,dt_individual_past$`First Name`,dt_individual_past$`Last Name`,dt_individual_past$`Date of Birth`))),]
  print(paste0("Newly enrolled individuals: ",nrow(unique(dt_individual_new[,2:4]))))
  print(paste0("New cases: ",nrow(unique(dt_enrolled[!is.na(dt_enrolled$`Case Start Date`),]))))
  county = readline(prompt = "Specific county number you are looking for?? If not, leave blank:")
  if(county!=''){
    print(table(dt_enrolled$Reporting_Agency[tolower(dt_enrolled$Reporting_Agency)==tolower(county)]))
  }
  else{
    print(table(dt_enrolled$Reporting_Agency))
  }
}

hsaps_race_ethnicity = function(dt,begin,end){
  dt_enrolled = subset(dt,`Case Start Date`>begin&`Case Start Date`<=end&!grepl("Select",Reporting_Agency))
  print(paste0("Race: ",round(length(grep("african",tolower(dt_enrolled$`Race 1`)))*100/nrow(dt_enrolled),2),"% of Black/African American, ",
               round(length(grep("asian",tolower(dt_enrolled$`Race 1`)))*100/nrow(dt_enrolled),2),"% of Asian/Asian American, ",
               round(length(grep("indian",tolower(dt_enrolled$`Race 1`)))*100/nrow(dt_enrolled),2),"% of American Indian/Alaskan Native, ",
               round(length(grep("pacific",tolower(dt_enrolled$`Race 1`)))*100/nrow(dt_enrolled),2),"% of Native Hawaiian/Pacific Islander, ",
               round(length(grep("white",tolower(dt_enrolled$`Race 1`)))*100/nrow(dt_enrolled),2),"% of White; ",
               round(length(grep("other",tolower(dt_enrolled$`Race 1`)))*100/nrow(dt_enrolled),2),"% of Other",
               "Ethnicity: ",length(unique(c(grep("mexican",tolower(dt_enrolled$Ethnicity)),grep("^hispanic",tolower(dt_enrolled$Ethnicity)),grep("other hispani",tolower(dt_enrolled$Ethnicity))))),
               " Hispanic/Latin(a)(o)(x), ",length(unique(c(grep("non-h",tolower(dt_enrolled$Ethnicity)),grep("not hisp",tolower(dt_enrolled$Ethnicity))))),
               " of Non-Hispanic/Latin(a)(o)(x)"))
}

hsaps_homelessness_entry = function(dt,begin,end){
  dt_enrolled = subset(dt,`Case Start Date`>begin&`Case Start Date`<=end&!grepl("Select",Reporting_Agency))
  index = unique(c(grep("permanent",tolower(dt_enrolled$`Living Situation Upon Entry`)),grep("owner",tolower(dt_enrolled$`Living Situation Upon Entry`)),
                   grep("rent",tolower(dt_enrolled$`Living Situation Upon Entry`)),grep("facility",tolower(dt_enrolled$`Living Situation Upon Entry`)),
                   grep("family",tolower(dt_enrolled$`Living Situation Upon Entry`)),grep("relative",tolower(dt_enrolled$`Living Situation Upon Entry`)),
                   grep("temporary- residential",tolower(dt_enrolled$`Living Situation Upon Entry`)),grep("^other$",tolower(dt_enrolled$`Living Situation Upon Entry`))))
  print(paste0("# cases at risk of homelessness: ",length(index)))
  county = readline(prompt="Speficic county number you are looking for? If not, please leave blank:")
  if(county != ''){
    print(table(dt_enrolled[index,"Reporting_Agency"][tolower(dt_enrolled$Reporting_Agency[index])==tolower(county)]))
  }
  else{
    print(table(dt_enrolled[index,"Reporting_Agency"]))
  }
  index2 = unique(c(grep("homeless",tolower(dt_enrolled$`Living Situation Upon Entry`)),grep("hotel",tolower(dt_enrolled$`Living Situation Upon Entry`)),
                    grep("temporary housing$",tolower(dt_enrolled$`Living Situation Upon Entry`))))
  print(paste0("# cases experiencing homelessness: ",length(index2)))
  county = readline(prompt="Speficic county number you are looking for? If not, please leave blank:")
  if(county != ''){
    print(table(dt_enrolled[index2,"Reporting_Agency"][tolower(dt_enrolled$Reporting_Agency[index2])==tolower(county)]))
  }
  else{
    print(table(dt_enrolled[index2,"Reporting_Agency"]))
  }
}

hsaps_homelessness_exit = function(dt,begin,end){
  dt_exit = hsaps_exit_destination(dt,begin,end)
  dt_exit$exit_category = ifelse(grepl("permanent",tolower(dt_exit$exit))
                                 | grepl("rent",tolower(dt_exit$exit))
                                 | grepl("owner",tolower(dt_exit$exit))
                                 | grepl("other permanent",tolower(dt_exit$exit))
                                 | grepl("remain",tolower(dt_exit$exit))
                                 | grepl("retain",tolower(dt_exit$exit)),"Permanent",
                                 ifelse(grepl("temporary",tolower(dt_exit$exit))
                                        | grepl("hotel",tolower(dt_exit$exit)),"Temporary",
                                        ifelse(grepl("homeless",tolower(dt_exit$exit)),"Homeless",
                                               ifelse(grepl("not collected",tolower(dt_exit$exit))
                                                      | grepl("unknown",tolower(dt_exit$exit)),"Unknown",
                                                      ifelse(grepl("^other$",tolower(dt_exit$exit))
                                                             | grepl("deceased",tolower(dt_exit$exit))
                                                             | grepl("facility",tolower(dt_exit$exit)),"Other","Unknown")))))
  dt_exit = aggregate(dt_exit$count,list(dt_exit$Reporting_Agency,dt_exit$exit_category),sum)
  names(dt_exit) = c("county","exit_category","count")
  return(dt_exit)
}

hsaps_exit = function(dt,begin,end){
  dt_exit = subset(dt,`Case Closure Date`>begin&`Case Closure Date`<=end&!grepl("Select",Reporting_Agency))
  print(paste0("# exits: ",nrow(dt_exit)))
  county = readline(prompt = "Specific county number you are looking for? If not, leave blank:")
  if(county!=''){
    print(table(dt_exit$Reporting_Agency[tolower(dt_exit$Reporting_Agency)==tolower(county)]))
  }
  else{
    print(table(dt_exit$Reporting_Agency))
  }
}

hsaps_perm_housed = function(dt,begin,end){
  dt_exit = subset(dt,`Case Closure Date`>begin&`Case Closure Date`<=end&!grepl("Select",Reporting_Agency))
  dt_perm_housed = dt_exit[unique(c(grep("permanent",tolower(dt_exit$`Living Situation at Exit`)),grep("owner",tolower(dt_exit$`Living Situation at Exit`)),
                                    grep("rent",tolower(dt_exit$`Living Situation at Exit`)))),]
  print(paste0("Perm housed: ",nrow(dt_perm_housed)))
  print(table(dt_perm_housed$Reporting_Agency))
}

#need to update the function to include further fiscal years
hsaps_fy_region = function(dt){
  dt$fy_enrolled = ifelse(dt$`Case Start Date`>"2018-06-30"&dt$`Case Start Date`<="2019-06-30","1819",
                                 ifelse(dt$`Case Start Date`>"2019-06-30"&dt$`Case Start Date`<="2020-06-30","1920",
                                        ifelse(dt$`Case Start Date`>"2020-06-30"&dt$`Case Start Date`<="2021-06-30","2021",
                                               ifelse(dt$`Case Start Date`>"2021-06-30"&dt$`Case Start Date`<="2022-06-30","2122",
                                                      ifelse(dt$`Case Start Date`>"2022-06-30"&dt$`Case Start Date`<="2023-06-30","2223",'')))))
  dt$fy_exit = ifelse(dt$`Case Closure Date`>"2018-06-30"&dt$`Case Closure Date`<="2019-06-30","1819",
                             ifelse(dt$`Case Closure Date`>"2019-06-30"&dt$`Case Closure Date`<="2020-06-30","1920",
                                    ifelse(dt$`Case Closure Date`>"2020-06-30"&dt$`Case Closure Date`<="2021-06-30","2021",
                                           ifelse(dt$`Case Closure Date`>"2021-06-30"&dt$`Case Closure Date`<="2022-06-30","2122",
                                                  ifelse(dt$`Case Closure Date`>"2022-06-30"&dt$`Case Closure Date`<="2023-06-30","2223",'')))))
  socal = c("Imperial","Orange","Riverside","San Bernardino","San Diego","Ventura")
  sanjoaquin_valley = c("San Joaquin","Stanislaus","Merced","Madera","Fresno","Kern","Kings","Tulare")
  bay_area = c("Alameda","Contra Costa","Marin","Napa","San Francisco","San Mateo","Santa Clara","Solano","Sonoma")
  sac_area = c("El Dorado","Placer","Sacramento","Sutter","Yolo","Yuba")
  central_coast = c("Monterey","San Benito","San Luis Obispo","Santa Barbara","Santa Cruz")
  balance = c("Alpine","Amador","Butte","Calaveras","Colusa","Del Norte","Glenn","Humboldt","Inyo","Lake",
              "Lassen","Mariposa","Mendocino","Modoc","Mono","Nevada","Plumas","Shasta","Sierra","Siskiyou",
              "Tehama","Trinity","Tuolumne")
  dt$region = ifelse(dt$Reporting_Agency %in% socal,"Southern California",
                            ifelse(dt$Reporting_Agency %in% sanjoaquin_valley,"San Joaquin Valley",
                                   ifelse(dt$Reporting_Agency %in% bay_area,"Bay Area",
                                          ifelse(dt$Reporting_Agency %in% sac_area,"Sacramento Area",
                                                 ifelse(dt$Reporting_Agency %in% central_coast,"Central Coast",
                                                        ifelse(dt$Reporting_Agency %in% balance,"Balance of the State",
                                                               ifelse(dt$Reporting_Agency == "Los Angeles","Los Angeles County","")))))))
  return(dt)
}

hsaps_active_case = function(dt,begin,end){
  print(length(which(dt$`Case Start Date`<=begin&dt$`Case Closure Date`>begin&dt$`Case Closure Date`<=end)))
}

hsaps_exit_destination = function(dt,begin,end){
  dt = subset(dt[-which(!is.na(dt$`Case Closure Date`)&dt$`Living Situation at Exit`=="Not Exited"),],
              `Case Closure Date`>begin&`Case Closure Date`<=end)
  dt$`Living Situation at Exit` = ifelse(is.na(dt$`Living Situation at Exit`),"Data not collected",dt$`Living Situation at Exit`)
  dt = aggregate(dt$`Case Closure Date`,list(dt$Reporting_Agency,dt$`Living Situation at Exit`),length)
  names(dt) = c("county","exit","count")
  county = readline(prompt = "Specific counties you are looking for? If not, leave blank:")
  if(county!=''){
    dt$include=""
    for(i in 1:nrow(dt)){
      dt$include[i] = ifelse(grepl(tolower(dt$Reporting_Agency[i]),tolower(county)),"y",dt$include[i])
    }
    return(dt[dt$include=="y",-length(dt)])
  }
  else{
    return(dt)
  }
}

hsaps_exit_destination_county = function(dt,begin,end){
  dt = subset(dt[-which(!is.na(dt$`Case Closure Date`)&dt$`Living Situation at Exit`=="Not Exited"),],
              `Case Closure Date`>begin&`Case Closure Date`<=end)
  dt$`Living Situation at Exit` = ifelse(is.na(dt$`Living Situation at Exit`),"Data not collected",dt$`Living Situation at Exit`)
  dt = aggregate(dt$`Case Closure Date`,list(dt$Reporting_Agency,dt$`Living Situation at Exit`,dt$fy_exit),length)
  names(dt) = c("county","exit","fy","count")
  return(dt)
}

hsaps_exit_destination_region = function(dt,begin,end){
  dt = subset(dt[-which(!is.na(dt$`Case Closure Date`)&dt$`Living Situation at Exit`=="Not Exited"),],
              `Case Closure Date`>begin&`Case Closure Date`<=end)
  dt$`Living Situation at Exit` = ifelse(is.na(dt$`Living Situation at Exit`),"Data not collected",dt$`Living Situation at Exit`)
  dt = aggregate(dt$`Case Closure Date`,list(dt$region,dt$`Living Situation at Exit`,dt$fy_exit),length)
  names(dt) = c("region","exit","fy","count")
  return(dt)
}

hsaps_intervention_count = function(dt,begin,end){
  dt$intervention=0
  for(i in 1:nrow(dt)){
    for(j in grep("Intervention [0-9]+ - Date",names(dt))){
      dt$intervention_fy1819[i] = ifelse(is.na(dt[i,j]),dt$intervention_fy1819[i],
                                         ifelse(!is.na(dt[i,(j-2)]) & dt[i,(j-2)] != "No Intervention"
                                                & dt[i,j]>begin & dt[i,j]<=end,
                                         dt$intervention_fy1819[i]+1,dt$intervention_fy1819[i]))
    }
  }
  print(sum(dt$intervention))
}

hsaps_intervention_type = function(dt,begin,end){
  dt$no_intervention = 0
  dt$intervention_check = 0
  for(i in 1:nrow(dt)){
    dt$no_intervention[i] = ifelse(all(is.na(dt[i,grep("Intervention [0-9]{1} - Type",names(dt))]))
                                          |all(tolower(dt[i,grep("Intervention [0-9]{1} - Type",names(dt))])=="no intervention"),
                                          1,dt$no_intervention[i])
    dt$intervention_check[i] = ifelse(any(dt[i,grep("Intervention [0-9]{1} - Date",names(dt))]>begin
                                          & dt[i,grep("Intervention [0-9]{1} - Date",names(dt))]<=end),1,dt$intervention_check[i])
  }
  dt_int = subset(dt,no_intervention==0)
  dt_int = dt_int[which(dt_int$`Case Start Date`>begin & dt_int$`Case Start Date`<=end
                        |(is.na(dt_int$`Case Start Date`)&dt_int$intervention_check==1)),]
}

#initially considering served cases as those with csd & those without csd but with intervention info
#decided not to include cases without case start date with some intervention info due to reasons below:
#1) data accuracy issue (some interventions prior to case start date), 2) negligible number of cases (~95 cases from beginning till FY21-22)
#returning numbers for quick reporting
hsaps_enrollment_period = function(dt,begin,end){
  dt_exit = subset(dt,`Case Closure Date`>begin&`Case Closure Date`<=end&!grepl("Select",Reporting_Agency))
  dt_perm_housed = dt_exit[unique(c(grep("permanent",tolower(dt_exit$`Living Situation at Exit`)),grep("owner",tolower(dt_exit$`Living Situation at Exit`)),
                                    grep("rent",tolower(dt_exit$`Living Situation at Exit`)))),]
  dt_perm_housed$enrollment_period = as.Date(dt_perm_housed$`Case Closure Date`)-as.Date(dt_perm_housed$`Case Start Date`)
  print(median(dt_perm_housed$enrollment_period,na.rm=T))
}

#returning the filtered dataset, not just values
hsaps_new_participant = function(dt,case_individual,var,begin,end){
  if(case_individual=='individual'){
    dt_new = unique(dt[which(var>begin&var<=end),c(grep("region",names(dt)),grep("Name$",names(dt)),grep("Birth$",names(dt)))])
    dt_compare = dt[var<=begin&!is.na(var),]
    dt_new = dt_new[which(!(paste0(dt_new$region,dt_new$`First Name`,dt_new$`Last Name`,dt_new$`Date of Birth`)
                                 %in% paste0(dt_compare$region,dt_compare$`First Name`,dt_compare$`Last Name`,dt_compare$`Date of Birth`))),]
  }
  else{
    dt_new = dt[which(var>begin&var<=end),]
  }
  return(dt_new)
}

hsaps_data_auditing_categorical = function(disc,dt,var,category,var_name){
  #compare all the responses to tolower(category)
  #var in dt[,var] form
  var_lower = tolower(var)
  discrepancy = dt[which(!var_lower %in% category),c("Reporting_Agency","Report_Month","First Name","Last Name","Date of Birth","Case Start Date")]
  discrepancy$value = var[which(!var_lower %in% category)]
  discrepancy$discrepant_var = var_name
  disc_combined = rbind(disc,discrepancy)
  return(disc_combined)
}

#fixing non-acin options
hsaps_categorical_fix_tableau = function(dt,var_name,category,assign){
  #compare all the responses to tolower(category)
  #var in dt[,var] form
  dt[,var_name] = ifelse(is.na(dt[,var_name])|dt[,var_name]=='',dt[,var_name],
                         ifelse(tolower(dt[,var_name]) %in% category,dt[,var_name],assign))
  return(dt)
}

##for DQR Generation
elapsed_months <- function(end_date, start_date) {
  if(any(as.character(end_date)=='',na.rm=T)){
    end_date[which(end_date=='')] = NA
  }
  if(any(as.character(start_date)=='',na.rm=T)){
    start_date[which(start_date=='')] = NA
  }
  ed = as.Date(end_date)
  sd = as.Date(start_date)
  result = as.numeric(12/365.25 * (ed-sd))
  result = round(result,2)
  return(result)
}

error_table_generation = function(error_dt,dt,error_type,item,item_value,error_desc,priority){
  dt$`Error Type`=error_type
  dt$Item = item
  dt$`Item Value`=item_value
  dt$`Error Description`=error_desc
  dt$`Additional Information/Instructions`=''
  dt$`Error Priority`=priority
  dt$`Fixed (Y/N)`=''
  dt$`Comments or questions from grantees`=''
  error_table = rbind(error_dt,dt)
  return(error_table)
}

ucsf_dyadic_interview_list = function(dt,county_list){
  dt_interview = data.frame()
  for(i in county_list){
    dt_interview = dt[grepl(i,dt$Reporting_Agency)&!is.na(dt$`Case Closure Date`),c(grep("Reporting_A",names(dt)),grep("^ID$",names(dt)),grep(" Name$",names(dt)),grep("Date of B",names(dt)))]
    write_xlsx(dt_interview,paste0("//Cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/UCSF Evaluation/Dyadic Interview Data/HSAPS_",i,"_UCSF_Interview.xlsx"))
  }
}

hsaps_missing_percentages = function(dt,ccd_var,missing_var_labels){
  missing_rate_na = c()
  missing_rate_label = c()
  for(j in 1:ncol(dt)){
    if(!grepl("follow_up",tolower(names(dt)[j]))
       & !grepl("exit$",tolower(names(dt)[j]))){
      missing_rate_na = c(missing_rate_na,round(length(which(is.na(dt[,j])))/length(dt[,j]),2)*100)
      missing_rate_label = c(missing_rate_label,round(length(which(dt[,j] %in% missing_var_labels))/length(dt[,j]),2)*100)
    }
    else if(grepl("exit$",tolower(names(dt)[j]))){
      dt_modified = dt[!is.na(ccd_var),]
      missing_rate_na = c(missing_rate_na,round(length(which(is.na(dt_modified[,j])))/length(dt_modified[,j]),2)*100)
      missing_rate_label = c(missing_rate_label,round(length(which(dt_modified[,j] %in% missing_var_labels))/length(dt_modified[,j]),2)*100)
    }
    else if(grepl("^six_month",tolower(names(dt)[j]))){
      dt_modified = dt[which(ccd_var<"2023-04-01"),]
      missing_rate_na = c(missing_rate_na,round(length(which(is.na(dt_modified[,j])))/length(dt_modified[,j]),2)*100)
      missing_rate_label = c(missing_rate_label,round(length(which(dt_modified[,j] %in% missing_var_labels))/length(dt_modified[,j]),2)*100)
    }
    else if(grepl("^twelve_",tolower(names(dt)[j]))){
      dt_modified = dt[which(ccd_var<"2022-10-01"),]
      missing_rate_na = c(missing_rate_na,round(length(which(is.na(dt_modified[,j])))/length(dt_modified[,j]),2)*100)
      missing_rate_label = c(missing_rate_label,round(length(which(dt_modified[,j] %in% missing_var_labels))/length(dt_modified[,j]),2)*100)
    }
  }
  return(list(missing_rate_na,missing_rate_label))
}
