filtering_relevant_cols = function(dt,pattern,year){
  #pattern = list (to accommodate multiple patterns)
  dt_new = dt[,1:2]
  for(l in 1:length(pattern)){
    dt_new = cbind(dt_new,dt[,grep(pattern[l],names(dt))])
    names(dt_new)[ncol(dt_new)] = gsub("\\\\.","",pattern[l])
  }
  names(dt_new) = gsub("\\^X","",names(dt_new))
  dt_new$newly_housed_num = as.numeric(dt_new[,"18a"])+as.numeric(dt_new[,"20b"])
  names(dt_new)[3] = "served_num"
  dt_new$year = year
  return(dt_new[,c("County","served_num","newly_housed_num","year")])
}

hsp_changing_col_names = function(dt){
  names(dt)[1:4] = as.character(dt[which(!is.na(dt[,1]))[1],1:4])
  names(dt)[5:ncol(dt)] = as.character(dt[1,5:ncol(dt)])
  dt = data.frame(dt[-(1:2),])
}

hsp_overview_filling_col_names = function(dt){
  dt = data.frame(dt)
  #filling in names
  for(j in 1:ncol(dt)){
    while(is.na(as.character(dt[2,j]))){
      dt[2,j] = as.character(dt[2,j-1])
      j = j-1
    }
  }
  for(j in 1:ncol(dt)){
    if(!is.na(as.character(dt[3,j]))){
      dt[2,j] = paste0(dt[2,j],': ',dt[3,j])
    }
  }
  names(dt) = as.character(dt[2,])
  dt = dt[-c(1:3),]
}

hsp_overview_chars_to_numeric = function(dt){
  #changing chars to numeric
  for(j in grep("^HSP Allocation",names(dt)):grep("^Point In Time Cases in",names(dt))){
    dt[,j] = round(as.numeric(dt[,j]),2)
  }
}
