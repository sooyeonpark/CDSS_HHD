library(readxl)
#obtain the most recent dataset
hsaps_dem = hsaps_exporting_data_with_proper_col_types("HSAPS FY22-23 2023-4-19.xlsm","2023-04-19")
hsaps_dem = subset(hsaps,`Case Start Date`<="2022-12-31") #filtering dataset based on given time frame

#demographic
hsaps_dem$age_begin = floor(elapsed_months(hsaps_dem$`Home Safe Case Start Date`,hsaps_dem$`Date of Birth`)/12)
hsaps_dem$age_end = floor(elapsed_months(hsaps_dem$`Home Safe Case Closure Date`,hsaps_dem$`Date of Birth`)/12)
hsaps_dem_2122$age = ifelse(hsaps_dem_2122$age_begin<18,"<18",
                        ifelse(hsaps_dem_2122$age_begin<26,"18-25",
                               ifelse(hsaps_dem_2122$age_begin<36,"26-35",
                                      ifelse(hsaps_dem_2122$age_begin<46,"36-45",
                                             ifelse(hsaps_dem_2122$age_begin<56,"46-55",
                                                    ifelse(hsaps_dem_2122$age_begin<66,"56-65",
                                                           ifelse(hsaps_dem_2122$age_begin<76,"66-75",
                                                                  ifelse(hsaps_dem_2122$age_begin<86,"76-85",
                                                                         ifelse(hsaps_dem_2122$age_begin<96,"86-95",
                                                                                ifelse(hsaps_dem_2122$age_begin<106,"96-105",
                                                                                       ifelse(hsaps_dem_2122$age_begin>=106,">105",NA)))))))))))

write.csv(table(hsaps_dem_2122$age),"demographics.csv",row.names=F)
write.table(table(tolower(hsaps_dem_2122$`Race 1`)),"demographics.csv",row.names=F,sep=",",append=T)
write.table(table(tolower(hsaps_dem_2122$`Gender Identity`)),"demographics.csv",row.names=F,sep=",",append=T)
write.table(table(tolower(hsaps_dem_2122$`Sexual Orientation`)),"demographics.csv",row.names=F,sep=",",append=T)
write.table(table(tolower(hsaps_dem_2122$`Current Marital Status`)),"demographics.csv",row.names=F,sep=",",append=T)
write.table(table(tolower(hsaps_dem_2122$`Preferred Language`)),"demographics.csv",row.names=F,sep=",",append=T)
