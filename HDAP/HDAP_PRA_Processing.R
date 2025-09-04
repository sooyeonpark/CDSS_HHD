library(readxl)
library(sqldf)
hdap_pra = hdap_exporting_data_with_proper_col_types("HDAP PII FY22-23 Q1 2022-12-21.xlsx","Data")

hdap_current = subset(hdap_pra,`Project Start Date`>"2021-06-30"&`Project Start Date`<="2022-06-30")
names(hdap_current)=tolower(names(hdap_current))
hdap_output1 = aggregate(hdap_current$county,list(hdap_current$county),length)

#num of housed - Item 31
hdap_perm_housed_current = subset(hdap_pra,`Housing Move-In Date - Permanent Housing`>"2021-06-30"&`Housing Move-In Date - Permanent Housing`<="2022-06-30")
names(hdap_perm_housed_current)=tolower(names(hdap_perm_housed_current))
hdap_perm_housed_current = merge(hdap_perm_housed_current,county_code,by.x="county",by.y="code",all.x=T)
hdap_output2 = aggregate(hdap_perm_housed_current$county,list(hdap_perm_housed_current$county),length)

#num of clients w retained housing
hdap_retained_current = subset(hdap_pra,`Housing Stabilized/ Retained Date`>"2021-06-30"&`Housing Stabilized/ Retained Date`<="2022-06-30")
names(hdap_retained_current)=tolower(names(hdap_retained_current))
hdap_output3 = aggregate(hdap_retained_current$county,list(hdap_retained_current$county),length)
length(which(hdap_retained_current$`housing move-in date - permanent housing`==hdap_retained_current$`housing stabilized/ retained date`))

#consolidation
names(hdap_output1) = c("county","enrolled_count")
names(hdap_output2) = c("county","perm_housed_count")
names(hdap_output3) = c("county","retained_housed_count")
hdap_output1 = merge(hdap_output1,county_code,by.x="county",by.y="code",all.x=T)
hdap_output2 = merge(hdap_output2,county_code,by.x="county",by.y="code",all.x=T)
hdap_output3 = merge(hdap_output3,county_code,by.x="county",by.y="code",all.x=T)
hdap_output = merge(hdap_output1,hdap_output2,all=T)
hdap_output = merge(hdap_output,hdap_output3,all=T)
hdap_output[which(is.na(hdap_output$county_name)),"county_name"] = hdap_output$county[which(is.na(hdap_output$county_name))]
#assuming NA's as 0's, not missing
for(j in 1:ncol(hdap_output)){
  hdap_output[,j] = ifelse(is.na(hdap_output[,j]),0,hdap_output[,j])
}

sum(hdap_output$enrolled_count)
sum(hdap_output$perm_housed_count)
sum(hdap_output$retained_housed_count)

#save file
write.csv(hdap_output[,-1],"HDAP/Data Drill_PRA/hdap_pra_output.csv",row.names=F)

##comparing to the previous year
hdap_past = subset(hdap_pra,`Project Start Date`>"2020-06-30"&`Project Start Date`<="2021-06-30")
names(hdap_past)=tolower(names(hdap_past))
hdap_output1_past = aggregate(hdap_past$county,list(hdap_past$county),length)

#num of housed - Item 31
hdap_perm_housed_past = subset(hdap_pra,`Housing Move-In Date - Permanent Housing`>"2020-06-30"&`Housing Move-In Date - Permanent Housing`<="2021-06-30")
names(hdap_perm_housed_past)=tolower(names(hdap_perm_housed_past))
hdap_perm_housed_past = merge(hdap_perm_housed_past,county_code,by.x="county",by.y="code",all.x=T)
hdap_output2_past = aggregate(hdap_perm_housed_past$county,list(hdap_perm_housed_past$county),length)

#num of clients w retained housing
hdap_retained_past = subset(hdap_pra,`Housing Stabilized/ Retained Date`>"2020-06-30"&`Housing Stabilized/ Retained Date`<="2021-06-30")
names(hdap_retained_past)=tolower(names(hdap_retained_past))
hdap_output3_past = aggregate(hdap_retained_past$county,list(hdap_retained_past$county),length)
length(which(hdap_retained_past$`housing move-in date - permanent housing`==hdap_retained_past$`housing stabilized/ retained date`))

#consolidation
names(hdap_output1_past) = c("county","enrolled_count")
names(hdap_output2_past) = c("county","perm_housed_count")
names(hdap_output3_past) = c("county","retained_housed_count")
hdap_output1_past = merge(hdap_output1_past,county_code,by.x="county",by.y="code",all.x=T)
hdap_output2_past = merge(hdap_output2_past,county_code,by.x="county",by.y="code",all.x=T)
hdap_output3_past = merge(hdap_output3_past,county_code,by.x="county",by.y="code",all.x=T)
hdap_output_past = merge(hdap_output1_past,hdap_output2_past,all=T)
hdap_output_past = merge(hdap_output_past,hdap_output3_past,all=T)
hdap_output_past[which(is.na(hdap_output_past$county_name)),"county_name"] = hdap_output_past$county[which(is.na(hdap_output_past$county_name))]

#assuming NA's as 0's, not missing
for(j in 1:ncol(hdap_output_past)){
  hdap_output_past[,j] = ifelse(is.na(hdap_output_past[,j]),0,hdap_output_past[,j])
}

names(hdap_output_past)[3:5] = paste0(names(hdap_output_past)[3:5],'_past')
hdap_output = merge(hdap_output,hdap_output_past,all.x=T)
hdap_output$enrollment_change = round(hdap_output$enrolled_count/hdap_output$enrolled_count_past,2)
hdap_output$perm_housed_change = round(hdap_output$perm_housed_count/hdap_output$perm_housed_count_past,2)
# hdap_output$county_name[which(hdap_output$enrollment_change>2)]
# hdap_output$county_name[which(hdap_output$enrollment_change<0.5)]
# hdap_output$county_name[which(hdap_output$perm_housed_change>2)]
# hdap_output$county_name[which(hdap_output$perm_housed_change<0.5)]

#clean up
rm(hdap_output1,hdap_output2,hdap_output3,hdap_current,hdap_perm_housed_current,hdap_retained_current,hdap_output_past,
   hdap_past,hdap_perm_housed_past,hdap_retained_past,hdap_output1_past,hdap_output2_past,hdap_output3_past)

#comparing with LY's result
# hdap_approved_ly = read_excel("S:/HDAP/HDAP Data/HDAP PII 21 Reports/HDAP data for FY 21-22 Comparison.xlsx",sheet="approval",col_types = c(rep("text",4),rep("date",4)))[-1,c(1,3:6)]
# hdap_housed_ly = read_excel("S:/HDAP/HDAP Data/HDAP PII 21 Reports/HDAP data for FY 21-22 Comparison.xlsx",sheet="housed",col_types = c(rep("text",4),rep("date",3)))[-(1:2),-2]
# names(hdap_approved_ly)=c("county","last name","first name","date of birth","project start date")
# names(hdap_housed_ly)=c("county","last name","first name","date of birth","project start date","housing move-in date - permanent housing")
# hdap_approved_ly = unique(hdap_approved_ly)
# hdap_housed_ly = unique(hdap_housed_ly)
# hdap_current$sp="yes"
# hdap_perm_housed_2122$sp="yes"
# hdap_approved_ly = merge(hdap_approved_ly,hdap_current[,c(1,3:4,7,30,ncol(hdap_current))],all.x=T)
##REASONS FOR DISCREPANCY
#1. DATE subsetting within the code (>="2021-07-01" != ">2021-06-30")
#2. In the midst of converting county codes to county names ("DCR" and "NICHA" converted to NA, not included in the counting)