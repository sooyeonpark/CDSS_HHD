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

#when using data containing only app cohort data (BRAIN/GAIN/(N)APP)
# fxage_app = function(df, id_var, date_var){
#   app_cohort_subj_dob = sqlQuery(new_con2,"SELECT t1.`SUBJ ID` AS subj_id,t1.`SUBJ DOB` AS dob
#                              from tblSubject AS t1 where t1.`SUBJ TYPE`='Primary';",stringsAsFactors=F)
# 
#   df <- merge(df, app_cohort_subj_dob, by.x=names(df)[which(names(df)==id_var)], by.y='subj_id', all.x=TRUE)
#   #calculating age
#   df$age = elapsed_months(df[,which(names(df)==date_var)], df$dob)
#   #print(head(df$age))
#   if(length(which(is.na(df$age)))>0){
#     df = df[-which(is.na(df$age)),]
#   }
#   df <- df[, -which(names(df)=='dob')]
#   return(df)
# }

#when using only staar data
# fxage_star = function(df, id_var, date_var){
#   star_subj_dob = sqlQuery(con5,"SELECT t1.`SUBJ ID` AS subj_id,t1.`SUBJ DOB` AS dob
#                             from tblSubject_Contact AS t1;",stringsAsFactors=F)
# 
#   df <- merge(df, star_subj_dob, by.x=names(df)[which(names(df)==id_var)], by.y='subj_id', all.x=TRUE)
#   #calculating age
#   df$age = elapsed_months(df[,which(names(df)==date_var)], df$dob)
#   #print(head(df$age))
#   if(length(which(is.na(df$age)))>0){
#     df = df[-which(is.na(df$age)),]
#   }
#   df <- df[, -which(names(df)=='dob')]
#   return(df)
# }

#when using combined data (both app cohorts and staar)
fxage = function(df, id_var, date_var){
  df <- merge(df, subj[,c('subj_id','dob')], by.x=names(df)[which(names(df)==id_var)], by.y='subj_id', all.x=TRUE)
  #calculating age
  df$age = elapsed_months(df[,which(names(df)==date_var)], df$dob)
  #print(head(df$age))
  if(length(which(is.na(df$age)))>0){
    non_primary_index = which((df[,id_var] %in% app_cohort_subj[,'subj id']==T) & (df[,id_var] %in% subj[,'subj_id']==F)
                              & is.na(df$age))
    if(length(non_primary_index)>0){
      df = df[-non_primary_index,]
    }
  }
  df <- df[, -which(names(df)=='dob')]
  return(df)
}

#obtaining parents' ages
fxage_parent = function(df, id_var, id_var2, date_var){
  #id_var2 = which id did the data table use for parents
  #^(the ones with "-MTH/-FAT" at the end or the same as subj_id?) values either subj_id or parent_id
  #the value should be either parent_id or subj_id
  
  #pulling their dob's from tblGuardian
  parents = sqlQuery(new_con2,"select t1.`Parent STS Number` as parent_id, t1.`Subject STS Number` as subj_id,
                     t1.`Parent Relationship` as resp, t1.`Parent DOB` as `parent dob` from tblGuardian_local t1",stringsAsFactors=F)

  #changing the date column format
  parents$`parent dob`=gsub(" ","",parents$`parent dob`)
  date = strsplit(parents$`parent dob`,"/")
  for(i in 1:length(date)){
    parents$dob[i] = paste0(date[[i]][3],"-",date[[i]][1],"-",date[[i]][2])
  }
  parents$dob = ifelse(parents$dob=="NA-NA-NA",NA,parents$dob)
  rm(date)
  
  #merging the data
  names(parents)[which(names(parents)==id_var2)]='id'
  df <- merge(df, parents[,c('id','resp','dob')], all.x=TRUE)
  #calculating age
  df$age = elapsed_months(df[,which(names(df)==date_var)], df$dob)
  #getting rid of non-primary participant rows
  if(length(which(is.na(df$age)))>0){
    non_primary_index = which((df[,id_var] %in% app_cohort_subj[,'subj id']==T) & is.na(df$age))
    if(length(non_primary_index)>0){
      df = df[-non_primary_index,]
    }
  }
  #assigning name to the age var
  df <- df[, -which(names(df)=='dob')]
  return(df)
}
