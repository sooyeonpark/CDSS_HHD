loading_packages = function(){
  library(readxl)
  library(writexl)
  library(openxlsx)
  library(plyr)
  library(stringr)
  library(sqldf)
}

categorical_fix = function(dt,var_name){
  print(table(dt[,var_name]))
  wanna_fix = readline("Do you want to fix the variable responses further?\n")
  while(tolower(wanna_fix)=="y"){
    fix_what = readline("Which pattern do you want to address?\n")
    fix_how = readline("Which value do you want to assign?\n")
    dt[grep(fix_what,dt[,var_name]),var_name] = fix_how
    print(table(dt[,var_name]))
    wanna_fix = readline("Do you want to fix the variable responses further?")
  }
  return(dt)
  #one can apply hsaps_categorical_fix_tableau() function after this function
}

participant_list_yes = function(data1,data2){
  data1_list = c()
  data2_list = c()
  for(j in 1:ncol(data1)){
    data1_list = paste0(data1_list,tolower(data1[,j]))
  }
  # print(head(data1_list))
  for(j in 1:ncol(data2)){
    data2_list = paste0(data2_list,tolower(data2[,j]))
  }
  # print(head(data2_list))
  if(all(data1_list %in% data2_list)){
    print("All Match!!!")
  }
  else{
    return(which(data1_list %in% data2_list))
  }
}

participant_list_no = function(data1,data2){
  data1_list = c()
  data2_list = c()
  for(j in 1:ncol(data1)){
    data1_list = paste0(data1_list,tolower(data1[,j]))
  }
  # print(head(data1_list))
  for(j in 1:ncol(data2)){
    data2_list = paste0(data2_list,tolower(data2[,j]))
  }
  # print(head(data2_list))
  if(!all(data1_list %in% data2_list)){
    return(which(!(data1_list %in% data2_list)))
  }
  # else{
  #   print("All Match!!!")
  # }
}

fy_assignment = function(dt,var,new_var){
  dt[,new_var] = ifelse(dt[,var]>"2018-06-30"&dt[,var]<="2019-06-30","1819",
                          ifelse(dt[,var]>"2019-06-30"&dt[,var]<="2020-06-30","1920",
                                 ifelse(dt[,var]>"2020-06-30"&dt[,var]<="2021-06-30","2021",
                                        ifelse(dt[,var]>"2021-06-30"&dt[,var]<="2022-06-30","2122",
                                               ifelse(dt[,var]>"2022-06-30"&dt[,var]<="2023-06-30","2223",
                                                      ifelse(dt[,var]>"2023-06-30"&dt[,var]<="2024-06-30","2324",
                                                             ifelse(dt[,var]>"2024-06-30"&dt[,var]<="2025-06-30","2425",'')))))))
  return(dt)
}

fy_hhdrs_assignment = function(dt,var,new_var){
  dt[,new_var] = ifelse(dt[,var]>"2017-06-30"&dt[,var]<="2018-06-30","2017-18",
                        ifelse(dt[,var]>"2018-06-30"&dt[,var]<="2019-06-30","2018-19",
                               ifelse(dt[,var]>"2019-06-30"&dt[,var]<="2020-06-30","2019-20",
                                      ifelse(dt[,var]>"2020-06-30"&dt[,var]<="2021-06-30","2020-21",
                                             ifelse(dt[,var]>"2021-06-30"&dt[,var]<="2022-06-30","2021-22",
                                                    ifelse(dt[,var]>"2022-06-30"&dt[,var]<="2023-06-30","2022-23",
                                                           ifelse(dt[,var]>"2023-06-30"&dt[,var]<="2024-06-30","2023-24",
                                                                  ifelse(dt[,var]>"2024-06-30"&dt[,var]<="2025-06-30","2024-25",''))))))))
  return(dt)
}

cy_assignment = function(dt,var,new_var){
  dt[,new_var] = ifelse(dt[,var]>"2016-12-31"&dt[,var]<="2017-12-31","2017",
                        ifelse(dt[,var]>"2017-12-31"&dt[,var]<="2018-12-31","2018",
                               ifelse(dt[,var]>"2018-12-31"&dt[,var]<="2019-12-31","2019",
                                      ifelse(dt[,var]>"2019-12-31"&dt[,var]<="2020-12-31","2020",
                                             ifelse(dt[,var]>"2020-12-31"&dt[,var]<="2021-12-31","2021",
                                                    ifelse(dt[,var]>"2021-12-31"&dt[,var]<="2022-12-31","2022",
                                                           ifelse(dt[,var]>"2022-12-31"&dt[,var]<="2023-12-31","2023",
                                                                  ifelse(dt[,var]>"2023-12-31"&dt[,var]<="2024-12-31","2024",
                                                                         ifelse(dt[,var]>"2024-12-31"&dt[,var]<="2025-12-31","2025",'')))))))))
  return(dt)
}

data_lookup_name = function(dt,fn_var,ln_var){
  fn = readline(prompt="First name pattern:")
  ln = readline(prompt="Last name pattern:")
  print(dt[which(grepl(fn,tolower(dt[,fn_var]))&grepl(ln,tolower(dt[,ln_var]))),])
}
