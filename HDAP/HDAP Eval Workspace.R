loading_packages()
##Leg question response propagation
hdap_leg = read.xlsx("HDAP/Eval/Leg Questions.xlsx",sheet = "Sheet1")
for(j in 3:ncol(hdap_leg)){
  hdap_leg[,j] = ifelse(is.na(hdap_leg[,j]),"No",
                        ifelse(grepl("[A-Z,a-z]{4,100}",hdap_leg[,j]),"Yes",hdap_leg[,j]))
}
write_xlsx(hdap_leg[,-1],"HDAP/Eval/Leg Questions_Propagated.xlsx")

##Demographic table generation
hdap_eval = read.csv("//Cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Data Integration/BUCP_datasets/newest_bucp_files/HDAP_program_data_20250826.csv",stringsAsFactors = F)
hdap_eval$project_start_date = ifelse(grepl("^[0-9]{1}/",hdap_eval$project_start_date),paste0("0",hdap_eval$project_start_date),hdap_eval$project_start_date)
hdap_eval$date_of_birth = ifelse(grepl("^[0-9]{1}/",hdap_eval$date_of_birth),paste0("0",hdap_eval$date_of_birth),hdap_eval$date_of_birth)
hdap_eval$date_of_birth = gsub("(.*)/(.*)/(.*)","\\3-\\1-\\2",hdap_eval$date_of_birth)
hdap_eval$project_start_date = gsub("(.*)/(.*)/(.*)","\\3-\\1-\\2",hdap_eval$project_start_date)
hdap_eval = fy_hhdrs_assignment(hdap_eval,"project_start_date","fy_enrolled")
hdap_eval$age = floor(elapsed_months(hdap_eval$project_start_date,hdap_eval$date_of_birth)/12)
hdap_eval$age_cat = ifelse(hdap_eval$age<18,"<18",
                           ifelse(hdap_eval$age<=25,"18-25",
                                  ifelse(hdap_eval$age<=30,"25-30",
                                         ifelse(hdap_eval$age<=40,"30-40",
                                                ifelse(hdap_eval$age<=50,"40-50",
                                                       ifelse(hdap_eval$age<=60,"50-60",
                                                              ifelse(hdap_eval$age<=70,"60-70",
                                                                     ifelse(hdap_eval$age<=80,"70-80",">80"))))))))

names(hdap_eval) = gsub("_._","_",names(hdap_eval))
names(hdap_eval) = gsub("\\.","",names(hdap_eval))
hdap_eval_race_eth = sqldf("select fy_enrolled,county,race,count(race) as race_count from hdap_eval
                       where fy_enrolled!='' and race!=''
                       group by fy_enrolled,county,race")
hdap_eval_gender = sqldf("select fy_enrolled,county,gender_identity,count() as gender_identity_count from hdap_eval
                       where fy_enrolled!='' and gender_identity!=''
                       group by fy_enrolled,county,gender_identity")
hdap_eval_sex_or = sqldf("select fy_enrolled,county,sexual_orientation,count(sexual_orientation) as sexual_orientation_count from hdap_eval
                       where fy_enrolled!='' and sexual_orientation!=''
                       group by fy_enrolled,county,sexual_orientation")
hdap_eval_age_cat = sqldf("select fy_enrolled,county,age_cat,count(age_cat) as age_cat_count from hdap_eval
                       where fy_enrolled!='' and age_cat!=''
                       group by fy_enrolled,county,age_cat")
hdap_eval_tar_pop1 = sqldf("select fy_enrolled,county,count(hdap_target_population_gagr) as ga_gr_count
                            from hdap_eval where fy_enrolled!='' and hdap_target_population_gagr='Yes'
                            group by fy_enrolled,county")
hdap_eval_tar_pop2 = sqldf("select fy_enrolled,county,count(hdap_target_population_calworks) as calworks_count
                            from hdap_eval where fy_enrolled!='' and hdap_target_population_calworks='Yes'
                            group by fy_enrolled,county")
hdap_eval_tar_pop3 = sqldf("select fy_enrolled,county,count(hdap_target_population_diverted_from_jailprison) as jail_prison_count
                            from hdap_eval where fy_enrolled!='' and hdap_target_population_diverted_from_jailprison='Yes'
                            group by fy_enrolled,county")
hdap_eval_tar_pop4 = sqldf("select fy_enrolled,county,count(hdap_target_population_low_income_veteran) as low_income_vet_count
                            from hdap_eval where fy_enrolled!='' and hdap_target_population_low_income_veteran='Yes'
                            group by fy_enrolled,county")
hdap_eval_tar_pop5 = sqldf("select fy_enrolled,county,count(hdap_target_population_discharged_from_institution) as inst_count
                            from hdap_eval where fy_enrolled!='' and hdap_target_population_discharged_from_institution='Yes'
                            group by fy_enrolled,county")
hdap_eval_tar_pop6 = sqldf("select fy_enrolled,county,count(hdap_target_population_other_lowno_income) as lack_income_count
                            from hdap_eval where fy_enrolled!='' and hdap_target_population_other_lowno_income='Yes'
                            group by fy_enrolled,county")
hdap_eval_tar_pop = merge(hdap_eval_tar_pop1,hdap_eval_tar_pop2,all=T)
hdap_eval_tar_pop = merge(hdap_eval_tar_pop,hdap_eval_tar_pop3,all=T)
hdap_eval_tar_pop = merge(hdap_eval_tar_pop,hdap_eval_tar_pop4,all=T)
hdap_eval_tar_pop = merge(hdap_eval_tar_pop,hdap_eval_tar_pop5,all=T)
hdap_eval_tar_pop = merge(hdap_eval_tar_pop,hdap_eval_tar_pop6,all=T)

rm(hdap_eval_tar_pop1,hdap_eval_tar_pop2,hdap_eval_tar_pop3,hdap_eval_tar_pop4,hdap_eval_tar_pop5,hdap_eval_tar_pop6)
