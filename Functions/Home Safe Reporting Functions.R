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
  county = readline(prompt = "Specific county number you are looking for:")
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
               round(length(grep("other",tolower(dt_enrolled$`Race 1`)))*100/nrow(dt_enrolled),2),"% of Other; ",
               "Ethnicity: ",length(unique(c(grep("mexican",tolower(dt_enrolled$Ethnicity)),grep("^hispanic",tolower(dt_enrolled$Ethnicity)),grep("other hispani",tolower(dt_enrolled$Ethnicity))))),
               " Hispanic/Latin(a)(o)(x), ",length(unique(c(grep("non-h",tolower(dt_enrolled$Ethnicity)),grep("not hisp",tolower(dt_enrolled$Ethnicity))))),
               " of Non-Hispanic/Latin(a)(o)(x)"))
}

hsaps_homelessness = function(dt,begin,end){
  dt_enrolled = subset(dt,`Case Start Date`>begin&`Case Start Date`<=end&!grepl("Select",Reporting_Agency))
  print(paste0("# cases at risk of homelessness: ",length(unique(c(grep("permanent",tolower(dt_enrolled$`Living Situation Upon Entry`)),grep("owner",tolower(dt_enrolled$`Living Situation Upon Entry`)),
                                                                   grep("rent",tolower(dt_enrolled$`Living Situation Upon Entry`)),grep("facility",tolower(dt_enrolled$`Living Situation Upon Entry`)),
                                                                   grep("family",tolower(dt_enrolled$`Living Situation Upon Entry`)),grep("relative",tolower(dt_enrolled$`Living Situation Upon Entry`)),
                                                                   grep("temporary residential",tolower(dt_enrolled$`Living Situation Upon Entry`)),grep("^other$",tolower(dt_enrolled$`Living Situation Upon Entry`)))))))
  print(paste0("# cases experiencing homelessness: ",length(unique(c(grep("homeless",tolower(dt_enrolled$`Living Situation Upon Entry`)),grep("hotel",tolower(dt_enrolled$`Living Situation Upon Entry`)),
                                                                     grep("temporary housing$",tolower(dt_enrolled$`Living Situation Upon Entry`)))))))
}

hsaps_exit = function(dt,begin,end){
  dt_exit = subset(dt,`Case Closure Date`>begin&`Case Closure Date`<=end&!grepl("Select",Reporting_Agency))
  print(paste0("# exits: ",nrow(dt_exit)))
  print(table(dt_exit$Reporting_Agency))
}

hsaps_perm_housed = function(dt,begin,end){
  dt_exit = subset(dt,`Case Closure Date`>begin&`Case Closure Date`<=end&!grepl("Select",Reporting_Agency))
  dt_perm_housed = dt_exit[unique(c(grep("permanent",tolower(dt_exit$`Living Situation at Exit`)),grep("owner",tolower(dt_exit$`Living Situation at Exit`)),
                                    grep("rent",tolower(dt_exit$`Living Situation at Exit`)))),]
  print(paste0("Perm housed: ",nrow(dt_perm_housed)))
  print(table(dt_perm_housed$Reporting_Agency))
}

hsaps_enrollment_duration = function(dt,begin,end){
  dt_exit = subset(dt,`Case Closure Date`>begin&`Case Closure Date`<=end&!grepl("Select",Reporting_Agency))
  dt_exit$enrollment_period = as.Date(dt_exit$`Case Closure Date`)-as.Date(dt_exit$`Case Start Date`)
  dt_perm_housed = dt_exit[unique(c(grep("permanent",tolower(dt_exit$`Living Situation at Exit`)),grep("owner",tolower(dt_exit$`Living Situation at Exit`)),
                                    grep("rent",tolower(dt_exit$`Living Situation at Exit`)))),]
  print(paste0("Median # of days for all program exits: ",median(dt_exit$enrollment_period,na.rm=T)))
  print(paste0("Median # of days for permanently housed exits: ",median(dt_perm_housed$enrollment_period,na.rm=T)))
}
