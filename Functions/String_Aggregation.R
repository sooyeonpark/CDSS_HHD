string_aggregation = function(dt,var_pattern,category,var){
  for(i in 1:nrow(dt)){
    check = which(dt[i,grep(var_pattern,names(dt))]=="Checked")
    if(length(check)==1){
      dt[i,var] = category[check]
    }
    else if(length(check)>1){
      dt[i,var] = ''
      for(l in 1:length(check)){
        dt[i,var] = paste0(dt[i,var],', ',category[check[l]])
      }
    }
  }
  dt[,var] = gsub("^, ","",dt[,var])
  return(dt)
}
#ex. after defining med_hist$head_issues = '', med_hist = string_aggregation(med_hist,"^head_brain_",head,"head_issues")
