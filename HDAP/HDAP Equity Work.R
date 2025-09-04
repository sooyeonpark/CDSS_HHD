loading_packages()
hdap_equity = hdap
hdap_equity = merge(hdap_equity,county_code[,2:3],by.x="County",by.y="county_name",all.x=T)
names(hdap_equity)[ncol(hdap_equity)]="region"

hdap_equity=fy_assignment(hdap_equity,"Project Start Date","fy_enrolled")
hdap_equity=cy_assignment(hdap_equity,"Project Start Date","cy_enrolled")
hdap_equity=fy_assignment(hdap_equity,"Exit Date","fy_exit")
hdap_equity=fy_assignment(hdap_equity,"Housing Move-In Date - Permanent Housing","fy_perm")
hdap_equity=fy_assignment(hdap_equity,"Housing Move-In Date - Temporary Housing","fy_temp")
hdap_equity=fy_assignment(hdap_equity,"Housing Stabilized/Retained Date","fy_stab")
hdap_equity=fy_assignment(hdap_equity,"Disability Benefit (A) - Initial Application Submission Date","fy_db_a_initial")
hdap_equity=fy_assignment(hdap_equity,"Disability Benefit (A) - Approval Date","fy_db_a_approval")
hdap_equity=fy_assignment(hdap_equity,"Disability Benefit (A) - Final Denial Date","fy_db_a_denial")
hdap_equity=fy_assignment(hdap_equity,"Disability Benefit (B) - Initial Application Submission Date","fy_db_b_initial")
hdap_equity=fy_assignment(hdap_equity,"Disability Benefit (B) - Approval Date","fy_db_b_approval")
hdap_equity=fy_assignment(hdap_equity,"Disability Benefit (B) - Final Denial Date","fy_db_b_denial")

##Preliminary script
# hdap_race = subset(hdap,!is.na(Race)&!grepl("^Client",Race))
# length(which(!is.na(hdap_race$`Living Situation at Entry`)))
# length(which(!is.na(hdap_race$Destination)))
# hdap_lse = hdap_equity[which(!is.na(hdap_equity$`Living Situation at Entry`)),]
# hdap_dest = hdap_equity[which(!is.na(hdap_equity$Destination)),c(1:5,grep("Race",names(hdap)),grep("^Dest",names(hdap)))]
# write.csv(round(table(hdap_equity$Race,hdap_equity$`Living Situation at Entry`)/28894*100,2),"HDAP/HDAP Equity Work_Preliminary.csv")
# write.table(round(table(hdap_equity$Race,hdap_equity$Destination)/20731*100,2),sep = ",","HDAP/HDAP Equity Work_Preliminary.csv",append=T)
# write.table(round(table(hdap_equity$Race)/32250*100,2),sep = ",","HDAP/HDAP Equity Work_Preliminary.csv",row.names=F,append=T)
# write.table(round(table(hdap_lse$Race)/28894*100,2),sep = ",","HDAP/HDAP Equity Work_Preliminary.csv",row.names=F,append=T)
# write.table(round(table(hdap_dest$Race)/20731*100,2),sep = ",","HDAP/HDAP Equity Work_Preliminary.csv",row.names=F,append=T)

#experiencing homelessness against race
length(which(!is.na(hdap_equity$`Experiencing homelessness`)&!is.na(hdap_equity$Race)))
write.table(table(hdap_equity$Race,hdap_equity$`Experiencing homelessness`),sep = ",","HDAP/HDAP Equity Work_Preliminary.csv",append=T)

#bucketing entry and exit locations
hdap_equity$exit_bucketed = ifelse(is.na(hdap_equity$Destination),NA,
                                 ifelse(grepl("^Perm",hdap_equity$`Destination`)
                                       | grepl("^Own",hdap_equity$`Destination`)
                                       | grepl("^Rental",hdap_equity$`Destination`)
                                       | grepl("permanent",hdap_equity$`Destination`)
                                       | grepl("^Retained",hdap_equity$`Destination`),"Permanent Housing",
                                       ifelse(grepl("temporary",hdap_equity$`Destination`)
                                              | grepl("^Transitional",hdap_equity$`Destination`)
                                              | grepl("^Hotel",hdap_equity$`Destination`)
                                              | grepl("^Residential project",hdap_equity$`Destination`),"Temporary Housing",
                                              ifelse(grepl("^Emergency",hdap_equity$`Destination`)
                                                     | grepl("^Place",hdap_equity$`Destination`)
                                                     | grepl("Safe H",hdap_equity$`Destination`),"Homeless",
                                                     ifelse(grepl("^Client",hdap_equity$`Destination`)
                                                            | grepl("^No",hdap_equity$`Destination`),"Unknown",
                                                            ifelse(grepl("Other",hdap_equity$Destination)
                                                                   |grepl("Dece",hdap_equity$Destination),"Other","Institutions"))))))
hdap_equity_nuk = subset(hdap_equity,exit_bucketed != "Unknown" & !grepl("^Client",Race))
write.table(table(hdap_equity_nuk$Race,hdap_equity_nuk$exit_bucketed),sep = ",","HDAP/HDAP Equity Work_Preliminary.csv",append=T)

hdap_equity$Race = gsub("[0-9]+","Data Not Collected",hdap_equity$Race)
hdap_equity$Ethnicity = gsub("[0-9]+","Data Not Collected",hdap_equity$Ethnicity)
hdap_equity$Race[grep("^cli",tolower(hdap_equity$Race))]="Unknown/Refused"
hdap_equity$Ethnicity[grep("^cli",tolower(hdap_equity$Ethnicity))]="Unknown/Refused"
##comparing hdap data with acs and hmis data
hdap_equity_acs_hmis = read.xlsx("C:/Users/sypark/Desktop/Data Processing/HDAP/Equity/HDIS Equity opendataportal_racial_equity.xlsx",sep.names="_")
hdap_equity=cy_assignment(hdap_equity,"Project Start Date","cy_enrolled")
names(hdap_equity_acs_hmis) = tolower(names(hdap_equity_acs_hmis))
hdap_equity_acs_hmis$percent_general_population_acs = hdap_equity_acs_hmis$percent_general_population_acs*100
hdap_equity_acs_hmis$percent_peh_hmis = hdap_equity_acs_hmis$percent_peh_hmis*100
coc_list = unique(hdap_equity_acs_hmis$location)[-grep("\\(",unique(hdap_equity_acs_hmis$location))]
hdap_equity$location = ''
for(i in 1:nrow(hdap_equity)){
  hdap_equity$location[i] = ifelse(any(grepl(hdap_equity$County[i],coc_list)),coc_list[grep(hdap_equity$County[i],coc_list)],hdap_equity$location)
}
hdap_acs_hmis=sqldf("select cy_enrolled,location,count(location) as coc_freq from hdap_equity
                     where cy_enrolled is not null and cy_enrolled != ''
                     group by cy_enrolled,location")
hdap_acs_hmis_no_urd=sqldf("select cy_enrolled,location,count(location) as coc_freq from hdap_equity
                            where cy_enrolled is not null and cy_enrolled != ''
                            and Race != 'Unknown/Refused' and Ethnicity != 'Unknown/Refused'
                            group by cy_enrolled,location")
hdap_acs_hmis_race=sqldf("select cy_enrolled,location,Race as raeth,count(Race) as freq from hdap_equity
                          where Race is not null and cy_enrolled is not null and cy_enrolled != ''
                          group by cy_enrolled,location,Race")
hdap_acs_hmis_race_no_urd=sqldf("select cy_enrolled,location,Race as raeth,count(Race) as freq from hdap_equity
                                 where Race is not null and cy_enrolled is not null and cy_enrolled != ''
                                 and Race != 'Unknown/Refused' and Ethnicity != 'Unknown/Refused'
                                 group by cy_enrolled,location,Race")
hdap_acs_hmis_eth = sqldf("select cy_enrolled,location,Ethnicity as raeth,count(Ethnicity) as freq from hdap_equity
                          where Ethnicity is not null and cy_enrolled is not null and cy_enrolled != ''
                          group by cy_enrolled,location,Ethnicity")
hdap_acs_hmis_eth_no_urd = sqldf("select cy_enrolled,location,Ethnicity as raeth,count(Ethnicity) as freq from hdap_equity
                                  where Ethnicity is not null and cy_enrolled is not null and cy_enrolled != ''
                                  and Race != 'Unknown/Refused' and Ethnicity != 'Unknown/Refused'
                                  group by cy_enrolled,location,Ethnicity")
hdap_acs_hmis_raeth = rbind(hdap_acs_hmis_race,hdap_acs_hmis_eth)
hdap_acs_hmis_raeth = merge(hdap_acs_hmis_raeth,hdap_acs_hmis,all.x=T)
hdap_acs_hmis_raeth$prop = round(hdap_acs_hmis_raeth$freq/hdap_acs_hmis_raeth$coc_freq*100,1)
hdap_acs_hmis_raeth_no_urd = rbind(hdap_acs_hmis_race_no_urd,hdap_acs_hmis_eth_no_urd)
hdap_acs_hmis_raeth_no_urd = merge(hdap_acs_hmis_raeth_no_urd,hdap_acs_hmis_no_urd,all.x=T)
hdap_acs_hmis_raeth_no_urd$prop_no_urd = round(hdap_acs_hmis_raeth_no_urd$freq/hdap_acs_hmis_raeth_no_urd$coc_freq*100,1)

hdap_equity_acs_hmis$demog=tolower(hdap_equity_acs_hmis$demog)
hdap_acs_hmis_raeth$raeth=tolower(hdap_acs_hmis_raeth$raeth)
hdap_acs_hmis_raeth_no_urd$raeth=tolower(hdap_acs_hmis_raeth_no_urd$raeth)

hdap_equity_acs_hmis_raeth = merge(hdap_acs_hmis_raeth[,c(1:3,6)],hdap_equity_acs_hmis[,-2],by.x=c("cy_enrolled","location","raeth"),by.y=c("calendar_year","location","demog"),all=T)
hdap_equity_acs_hmis_raeth = subset(hdap_equity_acs_hmis_raeth,!grepl("non-hispanic",raeth))
hdap_equity_acs_hmis_raeth_no_urd = merge(hdap_acs_hmis_raeth_no_urd[,c(1:3,6)],hdap_equity_acs_hmis[,-2],by.x=c("cy_enrolled","location","raeth"),by.y=c("calendar_year","location","demog"),all=T)
hdap_equity_acs_hmis_raeth_no_urd = subset(hdap_equity_acs_hmis_raeth_no_urd,!grepl("non-hispanic",raeth))
write_xlsx(hdap_equity_acs_hmis,"//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/Equity/hdap_acs_hmis_comparison_draft.xlsx")
write_xlsx(hdap_equity_acs_hmis_raeth,"//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/Equity/hdap_acs_hmis_raeth.xlsx")
write_xlsx(hdap_equity_acs_hmis_raeth_no_urd,"//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/Equity/hdap_acs_hmis_raeth_no_URD.xlsx")
rm(hdap_acs_hmis,hdap_acs_hmis_eth,hdap_acs_hmis_eth_no_urd,hdap_acs_hmis_no_urd,hdap_acs_hmis_race,hdap_acs_hmis_race_no_urd)

#combining hdap disability benefit list -> do we want to combine c, d, and e as well?
hdap_equity_dba = hdap_equity[!is.na(hdap_equity$`Disability Benefit (A) - Type Applied For`),-c(grep("\\(B\\)",names(hdap_equity)),grep("\\(C\\)",names(hdap_equity)),
                                                                                                 grep("\\(D\\)",names(hdap_equity)),grep("\\(E\\)",names(hdap_equity)))]
hdap_equity_dbb = hdap_equity[!is.na(hdap_equity$`Disability Benefit (B) - Type Applied For (Optional)`),-c(grep("\\(A\\)",names(hdap_equity)),grep("\\(C\\)",names(hdap_equity)),
                                                                                                            grep("\\(D\\)",names(hdap_equity)),grep("\\(E\\)",names(hdap_equity)))]
# hdap_equity_dbc = hdap_equity[!is.na(hdap_equity$`Disability Benefit (C) - Type Applied For (Optional)`),-c(grep("\\(A\\)",names(hdap_equity)),grep("\\(B\\)",names(hdap_equity)),
#                                                                                                             grep("\\(D\\)",names(hdap_equity)),grep("\\(E\\)",names(hdap_equity)))]
# hdap_equity_dbd = hdap_equity[!is.na(hdap_equity$`Disability Benefit (D) - Type Applied For (Optional)`),-c(grep("\\(A\\)",names(hdap_equity)),grep("\\(B\\)",names(hdap_equity)),
#                                                                                                             grep("\\(C\\)",names(hdap_equity)),grep("\\(E\\)",names(hdap_equity)))]
# hdap_equity_dbe = hdap_equity[!is.na(hdap_equity$`Disability Benefit (E) - Type Applied For (Optional)`),-c(grep("\\(A\\)",names(hdap_equity)),grep("\\(B\\)",names(hdap_equity)),
#                                                                                                             grep("\\(D\\)",names(hdap_equity)),grep("\\(C\\)",names(hdap_equity)))]
names(hdap_equity_dba) = gsub("\\(A\\) ","",names(hdap_equity_dba))
names(hdap_equity_dbb) = gsub("\\(B\\) ","",names(hdap_equity_dbb))
# names(hdap_equity_dbc) = gsub("\\(C\\) ","",names(hdap_equity_dbc))
# names(hdap_equity_dbd) = gsub("\\(D\\) ","",names(hdap_equity_dbd))
# names(hdap_equity_dbe) = gsub("\\(E\\) ","",names(hdap_equity_dbe))
names(hdap_equity_dbb) = gsub("Benefit - Type Applied For \\(Optional\\)","Benefit - Type Applied For",names(hdap_equity_dbb))
# names(hdap_equity_dbc) = gsub("Benefit - Type Applied For \\(Optional\\)","Benefit - Type Applied For",names(hdap_equity_dbc))
# names(hdap_equity_dbd) = gsub("Benefit - Type Applied For \\(Optional\\)","Benefit - Type Applied For",names(hdap_equity_dbd))
# names(hdap_equity_dbe) = gsub("Benefit - Type Applied For \\(Optional\\)","Benefit - Type Applied For",names(hdap_equity_dbe))
hdap_equity_db = rbind(hdap_equity_dba,hdap_equity_dbb)
write_xlsx(hdap_equity_db,"//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/HDAP/HDAP Data/Equity/hdap_equity_db.xlsx")

##from 4/16/2025
# hdap_equity = merge(hdap_equity,county_code[,2:3],by.x="County",by.y="county_name",all.x=T)
# names(hdap_equity)[ncol(hdap_equity)]="region"
# table(hdap_equity$region)

##as of 4/16, we decided to build the tables based on counties and merge region and coc later

##Perm Housed
#with race only by county and fy
hdap_equity_race_only=sqldf("select County,Race,fy_perm, count(Race) as freq from hdap_equity 
                       where Race is not null and fy_perm is not null
                       group by County, Race, fy_perm")
hdap_equity_race_only = subset(hdap_equity_race_only,fy_perm != '')
hdap_equity_race_only=reshape(hdap_equity_race_only,idvar=c("County","Race"),timevar = "fy_perm",direction = "wide")
names(hdap_equity_race_only) = gsub("freq.","",names(hdap_equity_race_only))
hdap_equity_race_only$`2425`=NULL
hdap_equity_race_only = hdap_equity_race_only[,c(1:2,order(names(hdap_equity_race_only))[-(7:8)])]
for(j in grep("[0-9]",names(hdap_equity_race_only))){
  hdap_equity_race_only[which(is.na(hdap_equity_race_only[,j])),j] = 0
}

#with race only by region and fy
hdap_equity_race_only=sqldf("select region,Race,fy_perm, count(Race) as freq from hdap_equity
                            where Race is not null and fy_perm is not null and region is not null
                            group by region, Race, fy_perm")
hdap_equity_race_only = subset(hdap_equity_race_only,fy_perm != '' & region != '')
hdap_equity_race_only=reshape(hdap_equity_race_only,idvar=c("region","Race"),timevar = "fy_perm",direction = "wide")
names(hdap_equity_race_only) = gsub("freq.","",names(hdap_equity_race_only))
hdap_equity_race_only$`2425`=NULL
hdap_equity_race_only = hdap_equity_race_only[,c(1:2,order(names(hdap_equity_race_only))[-(7:8)])]
for(j in grep("[0-9]",names(hdap_equity_race_only))){
  hdap_equity_race_only[which(is.na(hdap_equity_race_only[,j])),j] = 0
}

#with race and eth by county and fy
hdap_equity_race_eth_combined_perm=sqldf("select County,Ethnicity,Race,fy_perm, count(Race) as freq from hdap_equity
                                         where Race is not null and fy_perm is not null and county is not null and Ethnicity is not null
                                         group by county, Ethnicity, Race, fy_perm")
hdap_equity_race_eth_combined_perm = subset(hdap_equity_race_eth_combined_perm,fy_perm != '' & County != '')
hdap_equity_race_eth_combined_perm=reshape(hdap_equity_race_eth_combined_perm,idvar=c("County","Race","Ethnicity"),timevar = "fy_perm",direction = "wide")
names(hdap_equity_race_eth_combined_perm) = gsub("freq.","perm_",names(hdap_equity_race_eth_combined_perm))
hdap_equity_race_eth_combined_perm$`perm_2425`=NULL
hdap_equity_race_eth_combined_perm = hdap_equity_race_eth_combined_perm[,c(1:3,grep("_1819",names(hdap_equity_race_eth_combined_perm)),
                                                                           grep("_1920",names(hdap_equity_race_eth_combined_perm)),
                                                                           grep("_2021",names(hdap_equity_race_eth_combined_perm)),
                                                                           grep("_2122",names(hdap_equity_race_eth_combined_perm)),
                                                                           grep("_2223",names(hdap_equity_race_eth_combined_perm)),
                                                                           grep("_2324",names(hdap_equity_race_eth_combined_perm)))]
for(j in grep("[0-9]",names(hdap_equity_race_eth_combined_perm))){
  hdap_equity_race_eth_combined_perm[which(is.na(hdap_equity_race_eth_combined_perm[,j])),j] = 0
}

#with race and eth by region and fy
hdap_equity_race_eth_combined_perm=sqldf("select region,Ethnicity,Race,fy_perm, count(Race) as freq from hdap_equity
                                         where Race is not null and fy_perm is not null and region is not null and Ethnicity is not null
                                         group by region, Ethnicity, Race, fy_perm")
hdap_equity_race_eth_combined_perm = subset(hdap_equity_race_eth_combined_perm,fy_perm != '' & region != '')
hdap_equity_race_eth_combined_perm=reshape(hdap_equity_race_eth_combined_perm,idvar=c("region","Race","Ethnicity"),timevar = "fy_perm",direction = "wide")
names(hdap_equity_race_eth_combined_perm) = gsub("freq.","perm_",names(hdap_equity_race_eth_combined_perm))
hdap_equity_race_eth_combined_perm$`perm_2425`=NULL
hdap_equity_race_eth_combined_perm = hdap_equity_race_eth_combined_perm[,c(1:3,grep("_1819",names(hdap_equity_race_eth_combined_perm)),
                                                                           grep("_1920",names(hdap_equity_race_eth_combined_perm)),
                                                                           grep("_2021",names(hdap_equity_race_eth_combined_perm)),
                                                                           grep("_2122",names(hdap_equity_race_eth_combined_perm)),
                                                                           grep("_2223",names(hdap_equity_race_eth_combined_perm)),
                                                                           grep("_2324",names(hdap_equity_race_eth_combined_perm)))]
for(j in grep("[0-9]",names(hdap_equity_race_eth_combined_perm))){
  hdap_equity_race_eth_combined_perm[which(is.na(hdap_equity_race_eth_combined_perm[,j])),j] = 0
}

##Temp Housed
#with race and eth by region and fy
hdap_equity_race_eth_combined_temp=sqldf("select region,Ethnicity,Race,fy_temp, count(Race) as freq from hdap_equity
                                         where Race is not null and fy_temp is not null and region is not null and Ethnicity is not null
                                         group by region, Ethnicity, Race, fy_temp")
hdap_equity_race_eth_combined_temp = subset(hdap_equity_race_eth_combined_temp,fy_temp != '' & region != '')
hdap_equity_race_eth_combined_temp=reshape(hdap_equity_race_eth_combined_temp,idvar=c("region","Race","Ethnicity"),timevar = "fy_temp",direction = "wide")
names(hdap_equity_race_eth_combined_temp) = gsub("freq.","temp_",names(hdap_equity_race_eth_combined_temp))
hdap_equity_race_eth_combined_temp$temp_2425=NULL
hdap_equity_race_eth_combined_temp = hdap_equity_race_eth_combined_temp[,c(1:3,grep("_1819",names(hdap_equity_race_eth_combined_temp)),
                                                                           grep("_1920",names(hdap_equity_race_eth_combined_temp)),
                                                                           grep("_2021",names(hdap_equity_race_eth_combined_temp)),
                                                                           grep("_2122",names(hdap_equity_race_eth_combined_temp)),
                                                                           grep("_2223",names(hdap_equity_race_eth_combined_temp)),
                                                                           grep("_2324",names(hdap_equity_race_eth_combined_temp)))]
for(j in grep("[0-9]",names(hdap_equity_race_eth_combined_temp))){
  hdap_equity_race_eth_combined_temp[which(is.na(hdap_equity_race_eth_combined_temp[,j])),j] = 0
}

#with race and eth by county and fy
hdap_equity_race_eth_combined_temp=sqldf("select County,Ethnicity,Race,fy_temp, count(Race) as freq from hdap_equity
                                         where Race is not null and fy_temp is not null and County is not null and Ethnicity is not null
                                         group by County, Ethnicity, Race, fy_temp")
hdap_equity_race_eth_combined_temp = subset(hdap_equity_race_eth_combined_temp,fy_temp != '' & County != '')
hdap_equity_race_eth_combined_temp=reshape(hdap_equity_race_eth_combined_temp,idvar=c("County","Race","Ethnicity"),timevar = "fy_temp",direction = "wide")
names(hdap_equity_race_eth_combined_temp) = gsub("freq.","temp_",names(hdap_equity_race_eth_combined_temp))
hdap_equity_race_eth_combined_temp$temp_2425=NULL
hdap_equity_race_eth_combined_temp = hdap_equity_race_eth_combined_temp[,c(1:3,grep("_1819",names(hdap_equity_race_eth_combined_temp)),
                                                                           grep("_1920",names(hdap_equity_race_eth_combined_temp)),
                                                                           grep("_2021",names(hdap_equity_race_eth_combined_temp)),
                                                                           grep("_2122",names(hdap_equity_race_eth_combined_temp)),
                                                                           grep("_2223",names(hdap_equity_race_eth_combined_temp)),
                                                                           grep("_2324",names(hdap_equity_race_eth_combined_temp)))]
for(j in grep("[0-9]",names(hdap_equity_race_eth_combined_temp))){
  hdap_equity_race_eth_combined_temp[which(is.na(hdap_equity_race_eth_combined_temp[,j])),j] = 0
}

##Housing Stabilized
#with race and eth by region and fy
hdap_equity_race_eth_combined_stab=sqldf("select region,Ethnicity,Race,fy_stab, count(Race) as freq from hdap_equity
                                         where Race is not null and fy_stab is not null and region is not null and Ethnicity is not null
                                         group by region, Ethnicity, Race, fy_stab")
hdap_equity_race_eth_combined_stab = subset(hdap_equity_race_eth_combined_stab,fy_stab != '' & region != '')
hdap_equity_race_eth_combined_stab=reshape(hdap_equity_race_eth_combined_stab,idvar=c("region","Race","Ethnicity"),timevar = "fy_stab",direction = "wide")
names(hdap_equity_race_eth_combined_stab) = gsub("freq.","stab_",names(hdap_equity_race_eth_combined_stab))
hdap_equity_race_eth_combined_stab$stab_2425=NULL
hdap_equity_race_eth_combined_stab = hdap_equity_race_eth_combined_stab[,c(1:3,grep("_1819",names(hdap_equity_race_eth_combined_stab)),
                                                                           grep("_1920",names(hdap_equity_race_eth_combined_stab)),
                                                                           grep("_2021",names(hdap_equity_race_eth_combined_stab)),
                                                                           grep("_2122",names(hdap_equity_race_eth_combined_stab)),
                                                                           grep("_2223",names(hdap_equity_race_eth_combined_stab)),
                                                                           grep("_2324",names(hdap_equity_race_eth_combined_stab)))]
for(j in grep("[0-9]",names(hdap_equity_race_eth_combined_stab))){
  hdap_equity_race_eth_combined_stab[which(is.na(hdap_equity_race_eth_combined_stab[,j])),j] = 0
}

#with race and eth by county and fy
hdap_equity_race_eth_combined_stab=sqldf("select County,Ethnicity,Race,fy_stab, count(Race) as freq from hdap_equity
                                         where Race is not null and fy_stab is not null and County is not null and Ethnicity is not null
                                         group by County, Ethnicity, Race, fy_stab")
hdap_equity_race_eth_combined_stab = subset(hdap_equity_race_eth_combined_stab,fy_stab != '' & County != '')
hdap_equity_race_eth_combined_stab=reshape(hdap_equity_race_eth_combined_stab,idvar=c("County","Race","Ethnicity"),timevar = "fy_stab",direction = "wide")
names(hdap_equity_race_eth_combined_stab) = gsub("freq.","stab_",names(hdap_equity_race_eth_combined_stab))
hdap_equity_race_eth_combined_stab$stab_2425=NULL
hdap_equity_race_eth_combined_stab = hdap_equity_race_eth_combined_stab[,c(1:3,grep("_1819",names(hdap_equity_race_eth_combined_stab)),
                                                                           grep("_1920",names(hdap_equity_race_eth_combined_stab)),
                                                                           grep("_2021",names(hdap_equity_race_eth_combined_stab)),
                                                                           grep("_2122",names(hdap_equity_race_eth_combined_stab)),
                                                                           grep("_2223",names(hdap_equity_race_eth_combined_stab)),
                                                                           grep("_2324",names(hdap_equity_race_eth_combined_stab)))]
for(j in grep("[0-9]",names(hdap_equity_race_eth_combined_stab))){
  hdap_equity_race_eth_combined_stab[which(is.na(hdap_equity_race_eth_combined_stab[,j])),j] = 0
}

hdap_equity_housing = unique(merge(hdap_equity_race_eth_combined_perm,hdap_equity_race_eth_combined_temp,all=T))
hdap_equity_housing = unique(merge(hdap_equity_housing,hdap_equity_race_eth_combined_stab,all.x=T))
for(j in grep("[0-9]",names(hdap_equity_housing))){
  hdap_equity_housing[which(is.na(hdap_equity_housing[,j])),j] = 0
}

rm(hdap_lse,hdap_equity,hdap_equity,hdap_equity_race_eth_combined_perm,hdap_equity_race_eth_combined_temp,
   hdap_equity_race_only,hdap_equity_race_eth_combined_all)
