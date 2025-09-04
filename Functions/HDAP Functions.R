hdap_exporting_data_with_proper_col_types = function(dt_name,sheet_name){
  library(readxl)
  library(stringr)
  cat("Processing the data for sheet:", sheet_name, "\n")
  if(sheet_name=="Data"){
    dt = read_excel(paste0("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/HDAP PII/Compiled Reports/",dt_name),sheet = sheet_name)
    coltype = as.character(dt[1,])
    coltype = tolower(coltype)
    coltype = gsub("general","date",coltype)
    coltype = gsub("integer","numeric",coltype)
    coltype = gsub("ssn","text",coltype)
    cat("Handling missing column types...\n")
    if(length(which(is.na(coltype)))>0){coltype[which(is.na(coltype))] = c("text","date","date","text")}
    cat("Reading the data again with proper column types...\n")
    dt_new = read_excel(paste0("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/HDAP PII/Compiled Reports/",dt_name),sheet = sheet_name,col_types=coltype)[-1,]
    names(dt_new) = as.character(dt[2,])
    dt_new = dt_new[-1,]
  }
  else if(sheet_name=="PII"){
    cat("Reading the PII data...\n")
    coltype = tolower(as.character(read_excel("//cdss6psas2/RADD/Housing & Homelessness/Reports/HDAP PII/05_Deliverables/HDAP Compiled Data for Housing/HDAP PII FY21-22 Q1 2022-02-08.xlsx",sheet = "Data")[1,1:68]))
    names = as.character(read_excel("//cdss6psas2/RADD/Housing & Homelessness/Reports/HDAP PII/05_Deliverables/HDAP Compiled Data for Housing/HDAP PII FY21-22 Q1 2022-02-08.xlsx",sheet = "Data")[2,1:68])
    coltype = gsub("general","date",coltype)
    coltype = gsub("integer","numeric",coltype)
    coltype = gsub("ssn","text",coltype)
    if(length(which(is.na(coltype)))>0){coltype[which(is.na(coltype))] = c("text","date","date")}
    dt_new = read_excel(paste0("//cdss6psas2/RADD/Housing & Homelessness/Reports/HDAP PII/05_Deliverables/HDAP Compiled Data for Housing/",dt_name),sheet = sheet_name,col_types = coltype)[-1,]
    names(dt_new) = names
    dt_new = dt_new[-1,]
    names(dt_new) = gsub("^REPORT PERIOD","Quarter",names(dt_new))
    names(dt_new) = gsub("START","Start",names(dt_new))
    names(dt_new) = gsub("END","End",names(dt_new))
  }
  cat("Finalizing column names and data formatting...\n")
  dt_new = data.frame(dt_new,check.names=F)
  names(dt_new) = gsub("  "," ",names(dt_new))
  names(dt_new) = gsub("^Item [0-9]+:\r\n","",names(dt_new))
  names(dt_new) = gsub("^Item [0-9]+: ","",names(dt_new))
  names(dt_new) = gsub("^\r\n","",names(dt_new))
  names(dt_new) = gsub("^ ","",names(dt_new))
  names(dt_new) = gsub("\\)-","\\) -",names(dt_new))
  names(dt_new) = gsub("First Benefit Appeal","Subsequent Benefit Appeal",names(dt_new))
  names(dt_new) = gsub("/ ","/",names(dt_new))
  dt_new$`First Name` = str_to_title(dt_new$`First Name`)
  dt_new$`Last Name` = str_to_title(dt_new$`Last Name`)
  dt_new$`Social Security Number`[grep("^[0-9]{8}$",dt_new$`Social Security Number`)]=paste0("0",dt_new$`Social Security Number`[grep("^[0-9]{8}$",dt_new$`Social Security Number`)])
  return(dt_new)
}

hdap_gate_keeping = function(dt,new_dt_name,sheet_name){
  cat("Running the gate-keeping function...\n")
  new_dt = hdap_exporting_data_with_proper_col_types(new_dt_name,sheet_name)
  new_dt = new_dt[!is.na(new_dt$County),]
  cat("Formatting date fields...\n")
  new_dt$`Date of Birth` = gsub(" [0-9]+:[0-9]+:[0-9]+$","",as.character(new_dt$`Date of Birth`))
  new_dt$`Project Start Date` = gsub(" [0-9]+:[0-9]+:[0-9]+$","",as.character(new_dt$`Project Start Date`))
  new_dt$`Exit Date` = gsub(" [a-z,A-Z]+$","",new_dt$`Exit Date`)
  
  cat("Merging with county codes...\n")
  new_dt = merge(new_dt,county_code[,1:2],by.x="County",by.y="code",all.x=T)
  new_dt$County = new_dt$county_name
  new_dt$county_name = NULL
  
  #part 1: sorting through cases btw no Project Start dates to yes Project Start dates
  cat("Sorting through cases with updated Project Start Dates...\n")
  dt_no_csd = dt[is.na(dt$`Project Start Date`),]
  dt_yes_csd = dt[!is.na(dt$`Project Start Date`),]
  cat("Checking for matches with missing Project Start Dates...\n")
  csd_updated = c()
  for(i in 1:nrow(dt_no_csd)){
    match = which(tolower(paste0(new_dt$County,new_dt$`Last Name`,new_dt$`First Name`,new_dt$`Date of Birth`))
                  %in% tolower(paste0(dt_no_csd$County[i],dt_no_csd$`Last Name`[i],dt_no_csd$`First Name`[i],dt_no_csd$`Date of Birth`[i])))
    if(length(match)==0){
      #new paticipant without project start date!
      csd_updated = c(csd_updated,i)
    }
    else if(length(match)>1){
      if(dt_no_csd$`Quarter Start`[i]>max(new_dt$`Quarter Start`[match])
         & !is.na(dt_no_csd$`Exit Date`[i])
         & all(is.na(new_dt$`Exit Date`[match]))){
        csd_updated = c(csd_updated,i)
      }
    }
    else{
      if(dt_no_csd$`Quarter Start`[i]>new_dt$`Quarter Start`[match]
         & !is.na(dt_no_csd$`Exit Date`[i])
         & is.na(new_dt$`Exit Date`[match])){
        csd_updated = c(csd_updated,i)
      }
    }
  }
  if(length(csd_updated)>0){
    cat("Updating the dataset with cases having missing Project Start Dates...\n")
    dt_no_csd_removed = rbind(dt_yes_csd,dt_no_csd[unique(csd_updated),])
  }
  
  #part 2: gate keeping
  cat("Performing gate-keeping checks on the new dataset...\n")
  match_to_exclude = c()
  index_to_include = c()
  index_to_exclude = c()
  case_start_date_disc = c()
  for(i in 1:nrow(new_dt)){
    match_target = tolower(paste0(new_dt$County[i],new_dt$`First Name`[i],new_dt$`Last Name`[i],new_dt$`Date of Birth`[i]))
    match = which(tolower(paste0(dt_no_csd_removed$County,dt_no_csd_removed$`First Name`,dt_no_csd_removed$`Last Name`,dt_no_csd_removed$`Date of Birth`))==match_target)
    #if there is no match
    if(length(match)==0){
      match_wo_dob = which(tolower(paste0(dt_no_csd_removed$County,dt_no_csd_removed$`First Name`,dt_no_csd_removed$`Last Name`,dt_no_csd_removed$`Project Start Date`))==tolower(paste0(new_dt$County[i],new_dt$`First Name`[i],new_dt$`Last Name`[i],new_dt$`Project Start Date`[i])))
      #sorting through cases with updated dob's (either Jan 1st to an updated date or no date to an updated date)
      if(length(match_wo_dob)>0){
        for(m in 1:length(match_wo_dob)){
          if((!grepl("01-01$",new_dt$`Date of Birth`[i]) & grepl("01-01$",dt_no_csd_removed$`Date of Birth`[match_wo_dob[m]]))
             | (is.na(dt_no_csd_removed$`Date of Birth`[match_wo_dob[m]]) & !is.na(new_dt$`Date of Birth`[i]))){
            dt_no_csd_removed$`Date of Birth`[match_wo_dob[m]]=new_dt$`Date of Birth`[i]
          }
        }
      }
    }
    #after updating DOB, let's see if there is a match
    match = which(tolower(paste0(dt_no_csd_removed$County,dt_no_csd_removed$`First Name`,dt_no_csd_removed$`Last Name`,dt_no_csd_removed$`Date of Birth`))==match_target)
    #if there still isn't a match,
    if(length(match)==0){
      if((tolower(paste0(new_dt$County[i],new_dt$`First Name`[i],new_dt$`Date of Birth`[i])) %in%
          tolower(paste0(dt_no_csd_removed$County,dt_no_csd_removed$`First Name`,dt_no_csd_removed$`Date of Birth`)))
         | (tolower(paste0(new_dt$County[i],new_dt$`Date of Birth`[i],new_dt$`Last Name`[i])) %in% 
            tolower(paste0(dt_no_csd_removed$County,dt_no_csd_removed$`Date of Birth`,dt_no_csd_removed$`Last Name`)))
         | (tolower(paste0(new_dt$County[i],new_dt$`First Name`[i],new_dt$`Last Name`[i])) %in% 
            tolower(paste0(dt_no_csd_removed$County,dt_no_csd_removed$`First Name`,dt_no_csd_removed$`Last Name`)))){
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
        #check if the cases are the same even with different Project Start dates
        if((!is.na(new_dt$`Project Start Date`[i])
            & !is.na(dt_no_csd_removed$`Exit Date`[match[m]])
            & !is.na(new_dt$`Exit Date`[i])
            & dt_no_csd_removed$`Project Start Date`[match[m]]!=new_dt$`Project Start Date`[i]
            & dt_no_csd_removed$`Exit Date`[match[m]]==new_dt$`Exit Date`[i])
           | (!is.na(new_dt$`Project Start Date`[i])
              & dt_no_csd_removed$`Project Start Date`[match[m]]!=new_dt$`Project Start Date`[i]
              & !is.na(dt_no_csd_removed$`Exit Date`[match[m]])
              & new_dt$`Project Start Date`[i]<dt_no_csd_removed$`Exit Date`[match[m]])
           | (!is.na(new_dt$`Project Start Date`[i])
              & dt_no_csd_removed$`Project Start Date`[match[m]]!=new_dt$`Project Start Date`[i]
              & is.na(dt_no_csd_removed$`Exit Date`[match[m]])
              & !is.na(new_dt$`Exit Date`[i]))){
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
  # new_dt_case_start_date_disc = dt[unique(match_to_exclude),]
  return(list(dt_no_csd_removed,new_dt_include,new_dt_exclude))
}

# hdap_gate_keeping_revised = function(dt,new_dt_name,new_dt_sheet){
#   new_dt = hdap_exporting_data_with_proper_col_types(new_dt_name,new_dt_sheet)
#   new_dt$`Date of Birth` = gsub(" [0-9]+:[0-9]+:[0-9]+$","",as.character(new_dt$`Date of Birth`))
#   new_dt$`Project Start Date` = gsub(" [0-9]+:[0-9]+:[0-9]+$","",as.character(new_dt$`Project Start Date`))
#   new_dt = merge(new_dt,county_code[,1:2],by.x="County",by.y="code",all.x=T)
#   new_dt$County = new_dt$county_name
#   new_dt$county_name = NULL
#   for(j in grep("Name",names(new_dt))){
#     new_dt[,j] = str_to_title(new_dt[,j])
#   }
#   names(new_dt) = gsub("  "," ",names(new_dt))
#   index_to_include = c()
#   case_start_date_disc = c()
#   for(i in 1:nrow(new_dt)){
#     match_target = tolower(paste0(new_dt$County[i],new_dt$`First Name`[i],new_dt$`Last Name`[i],new_dt$`Date of Birth`[i]))
#     match = which(match_target==tolower(paste0(dt$County,dt$`First Name`,dt$`Last Name`,dt$`Date of Birth`)))
#     #if there is no match
#     if(length(match)==0){
#       #need to check if there is typo
#       if((tolower(paste0(new_dt$County[i],new_dt$`First Name`[i],new_dt$`Last Name`[i])) %in%
#           tolower(paste0(dt$County,dt$`First Name`,dt$`Last Name`)))
#          | (tolower(paste0(new_dt$County[i],new_dt$`First Name`[i],new_dt$`Date of Birth`[i])) %in%
#             tolower(paste0(dt$County,dt$`First Name`,dt$`Date of Birth`)))
#          | (tolower(paste0(new_dt$County[i],new_dt$`Date of Birth`[i],new_dt$`Last Name`[i])) %in%
#             tolower(paste0(dt$County,dt$`Date of Birth`,dt$`Last Name`)))){
#         next
#       }
#       #include new participants!
#       else{
#         index_to_include = c(index_to_include,i)
#       }
#     }
#     #if there is at least one match btw the newest report and the clean data set
#     if(length(match)>0){
#       if(!is.na(new_dt$`Project Start Date`[i])
#          & all(dt$`Project Start Date`[match]!=new_dt$`Project Start Date`[i],na.rm=T)){
#         #check if Project Start Dates are close together (within a month)
#         for(l in match){
#           if(!is.na(dt$`Project Start Date`[l])
#              & abs(as.Date(dt$`Project Start Date`[l])-as.Date(new_dt$`Project Start Date`[i]))<=30
#              & abs(as.Date(dt$`Project Start Date`[l])-as.Date(new_dt$`Project Start Date`[i]))>0){
#             case_start_date_disc = c(case_start_date_disc,i)
#           }
#         }
#       }
#       #otherwise, assume that it is all valid case information
#       if(!(i %in% case_start_date_disc)){
#         index_to_include = c(index_to_include,i)
#       }
#     }
#   }
#   new_dt_include = new_dt[unique(index_to_include),]
#   new_dt_exclude = new_dt[-unique(index_to_include),]
#   new_dt_case_start_date_disc = new_dt[unique(case_start_date_disc),]
#   return(list(new_dt_include,new_dt_exclude,new_dt_case_start_date_disc))
# }
                
# hdap_gate_keeping_initial = function(dt,new_dt_name,new_dt_sheet){
#   new_dt = hdap_exporting_data_with_proper_col_types(new_dt_name,new_dt_sheet)
#   new_dt = new_dt[!is.na(new_dt$County),]
#   new_dt$`Date of Birth` = gsub(" [0-9]+:[0-9]+:[0-9]+$","",as.character(new_dt$`Date of Birth`))
#   new_dt$`Project Start Date` = gsub(" [0-9]+:[0-9]+:[0-9]+$","",as.character(new_dt$`Project Start Date`))
#   new_dt = merge(new_dt,county_code[,1:2],by.x="County",by.y="code",all.x=T)
#   new_dt$County = new_dt$county_name
#   new_dt$county_name = NULL
#   for(j in grep("Name",names(new_dt))){
#     new_dt[,j] = str_to_title(new_dt[,j])
#   }
#   index_to_include = c()
#   for(i in 1:nrow(new_dt)){
#     if(paste0(new_dt$County[i],new_dt$`First Name`[i],new_dt$`Last Name`[i],new_dt$`Date of Birth`[i]) %in%
#        paste0(dt$County,dt$`First Name`,dt$`Last Name`,dt$`Date of Birth`)){
#       index_to_include = c(index_to_include,i)
#     }
#     else{
#       if((paste0(new_dt$County[i],new_dt$`First Name`[i],new_dt$`Last Name`[i]) %in%
#           paste0(dt$County,dt$`First Name`,dt$`Last Name`))
#           | (paste0(new_dt$County[i],new_dt$`First Name`[i],new_dt$`Date of Birth`[i]) %in%
#             paste0(dt$County,dt$`First Name`,dt$`Date of Birth`))
#           | (paste0(new_dt$County[i],new_dt$`Date of Birth`[i],new_dt$`Last Name`[i]) %in%
#             paste0(dt$County,dt$`Date of Birth`,dt$`Last Name`))){
#         next
#       }
#       index_to_include = c(index_to_include,i)
#     }
#   }
#   new_dt_include = new_dt[index_to_include,]
#   new_dt_exclude = new_dt[-index_to_include,]
#   return(list(new_dt_include,new_dt_exclude))
# }
                                              
hdap_compiling_reports = function(dt,file_list){
  library(plyr)
  library(stringr)
  for(l in 1:length(file_list)){
    hdap_dt = hdap_exporting_data_with_proper_col_types(file_list[[l]][1],file_list[[l]][2])
    dt = rbind.fill(dt,hdap_dt)
  }
  dt = unique(dt)
  return(dt)
}

hdap_duplicate_data_consolidate = function(dt){
  duplicate = data.frame()
  for(i in 1:nrow(dt)){
    duplicate_fnln_check = grep(paste0('^',dt$county_name[i],dt$`First Name`[i],dt$`Last Name`[i],'$'),paste0(dt$county_name,dt$`First Name`,dt$`Last Name`)) #dt$id[(i+1):nrow(dt)]?
    if(length(duplicate_fnln_check) == 1){
      i = i+1
    }
    else{
      for(j in 1:(length(duplicate_fnln_check)-1)){
        #checking the ssn's among duplicate id's
        duplicate_ssn_check = grep(paste0('^',dt$`Social Security Number`[duplicate_fnln_check[j]],'$'),dt$`Social Security Number`[duplicate_fnln_check])
        #checking the project dates among duplicate id's
        duplicate_pd_check = grep(paste0('^',as.character(dt$`Project Start Date`[duplicate_fnln_check[j]]),'$'),as.character(dt$`Project Start Date`[duplicate_fnln_check]))
        #checking the dobs among duplicate id's
        duplicate_dob_check = grep(paste0('^',as.character(dt$`Date of Birth`[duplicate_fnln_check[j]]),'$'),as.character(dt$`Date of Birth`[duplicate_fnln_check]))
        #if the visit or age is not unique, pull them out
        if(length(duplicate_pd_check)>1 & length(duplicate_ssn_check)>1 & length(duplicate_dob_check)>1){
          duplicate = rbind(duplicate,dt[duplicate_fnln_check[duplicate_pd_check],],
                            dt[duplicate_fnln_check[duplicate_ssn_check],],
                            dt[duplicate_fnln_check[duplicate_dob_check],])
        }
      }
    }
    duplicate = unique(duplicate)
  }
  return(duplicate)
}

hdap_duplicate_data_remove = function(dt,duplicate_dt){
  duplicate = duplicate_dt
  if(nrow(duplicate)>0 | !all(is.na(duplicate))){
    duplicate_index = which(paste0(dt$`First Name`,dt$`Last Name`,dt$`Project Start Date`,dt$`Date of Birth`)
                            %in% paste0(duplicate$`First Name`,duplicate$`Last Name`,duplicate$`Project Start Date`,duplicate$`Date of Birth`))
    dt = dt[-duplicate_index,]
  }
  return(dt)
}

hdap_numeric_responses_to_texts = function(dt,var_list,text_category){
  for(v in var_list){
    for(j in grep(v,tolower(names(dt)))){
      for(i in 1:nrow(dt)){
        # dt[i,j] = ifelse(any(grepl(paste0('^',dt[i,j],'-'),text_category)),text_category[grep(paste0('^',dt[i,j],'-'),text_category)],
        #                  ifelse(any(grepl(dt[i,j],gsub("^[0-9]+-","",text_category))),dt[i,j],NA))
        dt[i,j] = ifelse(any(grepl(paste0('^',dt[i,j],'-'),text_category)),text_category[grep(paste0('^',dt[i,j],'-'),text_category)],dt[i,j])
      }
      dt[,j] = gsub("[0-9]+-","",dt[,j])
    }
  }
  return(dt)
}

hdap_data_auditing_categorical = function(disc,dt,var,category,var_name){
  #compare all the responses to category
  #var in dt[,var] form
  category_sub = gsub("[0-9]+-","",category)
  if(class(var)=="data.frame"){
    for(j in 1:ncol(var)){
      discrepancy = dt[which(!(var[[j]] %in% category_sub)),c("County","First Name","Last Name","Date of Birth","Project Start Date")]
      discrepancy$value = var[which(!var[[j]] %in% category_sub),j]
      discrepancy$discrepant_var = var_name[j]
      if(j==1){
        disc_combined = rbind(disc,discrepancy)
      }
      else{
        disc_combined = rbind(disc_combined,discrepancy)
      }
    }
  }
  else{
    discrepancy = dt[which(!var %in% category_sub),c("County","First Name","Last Name","Date of Birth","Project Start Date")]
    discrepancy$value = var[which(!var %in% category_sub)]
    discrepancy$discrepant_var = var_name
    disc_combined = rbind(disc,discrepancy)
  }
  return(disc_combined)
}

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
