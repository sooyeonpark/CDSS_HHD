duplicate_data_consolidate = function(dt,age_var){
  #age_var = variable name in char for the age within dt
  duplicate = data.frame()
  for(i in 1:nrow(dt)){
    duplicate_id_check = grep(paste0('^',dt$id[i],'$'),dt$id) #dt$id[(i+1):nrow(dt)]?
    #print(duplicate_id_check)
    if(length(duplicate_id_check) == 1){
      i = i+1
    }
    else{
      for(j in 1:(length(duplicate_id_check)-1)){
        #checking the ages among duplicate id's
        duplicate_age_check = grep(paste0('^',dt[,age_var][duplicate_id_check[j]],'$'),dt[,age_var][duplicate_id_check])
        #checking the visit among duplicate id's
        duplicate_visit_check = grep(paste0('^',dt[,"visit"][duplicate_id_check[j]],'$'),dt[,"visit"][duplicate_id_check])
        #if the visit or age is not unique, pull them out
        if(length(duplicate_age_check)>1 | length(duplicate_visit_check) > 1){
          duplicate = rbind(duplicate,dt[duplicate_id_check[duplicate_age_check],],
                            dt[duplicate_id_check[duplicate_visit_check],])
        }
      }
    }
    duplicate = unique(duplicate)
  }
  return(duplicate)
}

duplicate_data_remove = function(dt,age_var){
  duplicate = duplicate_data_consolidate(dt,age_var)
  if(nrow(duplicate)>0 | !all(is.na(duplicate))){
    duplicate_index = which(dt$id %in% duplicate$id & dt$visit %in% duplicate$visit & dt[,age_var] %in% duplicate[,age_var])
    print(duplicate_index)
    dt = dt[-duplicate_index,]
  }
  return(dt)
}

combining_multiple_data = function(dt,dt_list,measure_name,measure_name_list){
  #dt = original data table
  #dt2 = new data table planning to be merged into dt (can be a list) -> measure_name2 can be a list as well
  if(ncol(dt) != 2){
    dt$measure = measure_name
  }
  for(l in 1:length(dt_list)){
    dt2 = dt_list[[l]]
    dt2$measure = measure_name_list[[l]]
    dt = rbind(dt[,c("id","measure")],dt2[,c("id","measure")])
    dt = unique(dt)
  }
  library(plyr)
  dt = ddply(dt,.(id),summarize,measure_list = paste0(measure,collapse=", "))
  return(dt)
}
