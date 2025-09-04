hdap_research = hdap
hdap_research$`Project Start Date`[grepl("^3",hdap_research$`Project Start Date`)]="2019-06-28"
hdap_research$duration = elapsed_months(hdap_research$`Exit Date`,hdap_research$`Project Start Date`)
hdap_research = subset(hdap_research,`Project Start Date`<="2024-12-31"&duration>0)
hist(hdap_research$duration) #other than 3 cases, the rest is less than 362 months
median(hdap_research$duration) #9.95; almost 10 months
hdap_research$perm_housed = ifelse(grepl("permanent",tolower(hdap_research$Destination))
                                   | grepl("rental",tolower(hdap_research$Destination))
                                   | grepl("^own",tolower(hdap_research$Destination))
                                   | grepl("^retained",tolower(hdap_research$Destination)),1,
                                   ifelse(grepl("emergency shel",tolower(hdap_research$Destination))
                                          | grepl("transitional housing",tolower(hdap_research$Destination))
                                          | grepl("temporary",tolower(hdap_research$Destination))
                                          | grepl("hotel",tolower(hdap_research$Destination))
                                          | grepl("safe heaven",tolower(hdap_research$Destination))
                                          | grepl("residential project",tolower(hdap_research$Destination)),0,
                                          ifelse(grepl("facility",tolower(hdap_research$Destination))
                                                 | grepl("foster care",tolower(hdap_research$Destination)),0,
                                                 ifelse(grepl("not meant for habitation",tolower(hdap_research$Destination)),0,NA))))
hdap_research$exit_cat = ifelse(grepl("permanent",tolower(hdap_research$Destination))
                                | grepl("rental",tolower(hdap_research$Destination))
                                | grepl("^own",tolower(hdap_research$Destination))
                                | grepl("^retained",tolower(hdap_research$Destination)),4,
                                ifelse(grepl("emergency shel",tolower(hdap_research$Destination))
                                       | grepl("transitional housing",tolower(hdap_research$Destination))
                                       | grepl("temporary",tolower(hdap_research$Destination))
                                       | grepl("hotel",tolower(hdap_research$Destination))
                                       | grepl("safe heaven",tolower(hdap_research$Destination))
                                       | grepl("residential project",tolower(hdap_research$Destination)),3,
                                       ifelse(grepl("facility",tolower(hdap_research$Destination))
                                              | grepl("foster care",tolower(hdap_research$Destination)),2,
                                              ifelse(grepl("not meant for habitation",tolower(hdap_research$Destination)),1,NA))))
hdap_research$entry_cat = ifelse(grepl("permanent",tolower(hdap_research$`Living Situation at Entry`))
                                 | grepl("rental",tolower(hdap_research$`Living Situation at Entry`))
                                 | grepl("^own",tolower(hdap_research$`Living Situation at Entry`))
                                 | grepl("^retained",tolower(hdap_research$`Living Situation at Entry`)),4,
                                 ifelse(grepl("emergency shel",tolower(hdap_research$`Living Situation at Entry`))
                                        | grepl("transitional housing",tolower(hdap_research$`Living Situation at Entry`))
                                        | grepl("temporary",tolower(hdap_research$`Living Situation at Entry`))
                                        | grepl("hotel",tolower(hdap_research$`Living Situation at Entry`))
                                        | grepl("safe heaven",tolower(hdap_research$`Living Situation at Entry`))
                                        | grepl("residential project",tolower(hdap_research$`Living Situation at Entry`)),3,
                                        ifelse(grepl("facility",tolower(hdap_research$`Living Situation at Entry`))
                                               | grepl("foster care",tolower(hdap_research$`Living Situation at Entry`)),2,
                                               ifelse(grepl("not meant for habitation",tolower(hdap_research$`Living Situation at Entry`)),1,NA))))
plot(jitter(hdap_research_use$duration)[hdap_research_use$duration<300],jitter(hdap_research_use$exit_cat)[hdap_research_use$duration<300])

library(lme4)
library(emmeans)
hdap_research_use = subset(hdap_research,!is.na(duration)&!is.na(exit_cat))
summary(fit0 <- lmer(exit_cat ~ 1 + (1|County) + (1|case), data=hdap_research_use))
summary(fit1 <- lmer(exit_cat ~ 1 + duration + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
anova(fit0,fit1) #duration has significant fixed effect to exit category

hdap_research_use = subset(hdap_research,!is.na(duration)&!is.na(exit_cat)&!is.na(entry_cat))
summary(fit1.1 <- glmer(exit_cat ~ 1 + duration + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit3 <- glmer(exit_cat ~ 1 + duration + entry_cat  + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
anova(fit1.1,fit3)
summary(fit4 <- glmer(exit_cat ~ 1 + duration + entry_cat  + duration*entry_cat + (1|County) + (1|case), data=hdap_research_use)) #when putting interaction effect, main effects are defaultly added
anova(fit3,fit4)
#^ test of the interaction -> NO moderation effect; this means the relationship between duration and exit_cat is not influenced by entry_cat
#conclusion: duration of the program stay and living situation at entry both affect exit category independently (fixed effects).
#However, each of their relationship does not change based on the variables to each other.
#It seems that entry category affects the relationship a bit more than duration but both are very significant.

#age as a moderator
hdap_research_use = subset(hdap_research,!is.na(duration)&!is.na(exit_cat)&age>0)
summary(fit1.2 <- glmer(exit_cat ~ 1 + duration + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit5 <- glmer(exit_cat ~ 1 + duration + age  + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit6 <- glmer(exit_cat ~ 1 + duration + age  + duration*age + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) 
anova(fit1.2,fit5) #not significant
anova(fit5,fit6) #no interaction effect

#ethnicity as a moderator
hdap_research$eth_cat = ifelse(grepl("^Non",hdap_research$Ethnicity),1,
                               ifelse(grepl("^His",hdap_research$Ethnicity),0,NA))
hdap_research_use = subset(hdap_research,!is.na(duration)&!is.na(exit_cat)&!is.na(eth_cat))
summary(fit1.3 <- lmer(exit_cat ~ 1 + duration + (1|County) + (1|case), data=hdap_research_use)) #put random effect last
summary(fit7 <- lmer(exit_cat ~ 1 + duration + eth_cat  + (1|County) + (1|case), data=hdap_research_use)) #put random effect last
summary(fit8 <- lmer(exit_cat ~ 1 + duration + eth_cat  + duration*eth_cat + (1|County) + (1|case), data=hdap_research_use)) #when putting interaction effect, main effects are defaultly added
anova(fit1.3,fit7) #no fixed effect for ethnicity
anova(fit7,fit8) #no moderation effect for ethnicity
#I wonder if it is because it is a categorical variable?

#race as a moderator
hdap_research$race_cat = ifelse(grepl("^white",tolower(hdap_research$Race)),1,
                                ifelse(grepl("^ameri",tolower(hdap_research$Race)),2,
                                       ifelse(grepl("^asia",tolower(hdap_research$Race)),3,
                                              ifelse(grepl("^black",tolower(hdap_research$Race)),4,
                                                     ifelse(grepl("^multi",tolower(hdap_research$Race)),5,
                                                            ifelse(grepl("^nativ",tolower(hdap_research$Race)),6,NA))))))
hdap_research_use = subset(hdap_research,!is.na(duration)&!is.na(exit_cat)&!is.na(race_cat))
summary(fit1.4 <- glmer(exit_cat ~ 1 + duration + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit9 <- glmer(exit_cat ~ 1 + duration + race_cat  + (1|County) + (1|case), data=hdap_research_use),family=binomial(link="logit")) #put random effect last
summary(fit10 <- glmer(exit_cat ~ 1 + duration + race_cat  + duration*race_cat + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #when putting interaction effect, main effects are defaultly added
anova(fit1.4,fit9) #significant fixed effect for race
anova(fit9,fit10) #no moderation effect for race

#sex as a moderator
hdap_research$sex_cat = ifelse(grepl("^Fem",hdap_research$`Sex Listed on Birth Certificate`),1,
                               ifelse(grepl("^Mal",hdap_research$`Sex Listed on Birth Certificate`),0,NA))
hdap_research_use = subset(hdap_research,!is.na(duration)&!is.na(exit_cat)&!is.na(sex_cat))
summary(fit1.5 <- glmer(exit_cat ~ 1 + duration + (1|County) + (1|case), data=hdap_research_use)) #put random effect last
summary(fit11 <- glmer(exit_cat ~ 1 + duration + sex_cat  + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit12 <- glmer(exit_cat ~ 1 + duration + sex_cat  + duration*sex_cat + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #when putting interaction effect, main effects are defaultly added
anova(fit1.5,fit11) #fixed effect for sex
anova(fit11,fit12) #no moderation effect for sex
multinom(exit_cat~duration+sex_cat,data=hdap_research_use)
multinom(exit_cat~duration+sex_cat+duration*sex_cat,data=hdap_research_use)
anova(multinom(exit_cat~duration+sex_cat+(1|County)+(1|case),data=hdap_research_use),
      multinom(exit_cat~duration+sex_cat+duration*sex_cat+(1|County)+(1|case),data=hdap_research_use))

#gender identity?
hdap_research$gender_cat = ifelse(grepl("^Fem",hdap_research$`Gender Identity`),1,
                               ifelse(grepl("^Mal",hdap_research$`Gender Identity`),0,
                                      ifelse(grepl("^Client",hdap_research$`Gender Identity`)|is.na(hdap_research$`Gender Identity`),NA,2)))
hdap_research_use = subset(hdap_research,!is.na(duration)&!is.na(exit_cat)&!is.na(gender_cat))
summary(fit1.6 <- lmer(exit_cat ~ 1 + duration + (1|County) + (1|case), data=hdap_research_use)) #put random effect last
summary(fit13 <- lmer(exit_cat ~ 1 + duration + gender_cat  + (1|County) + (1|case), data=hdap_research_use)) #put random effect last
summary(fit14 <- lmer(exit_cat ~ 1 + duration + gender_cat  + duration*gender_cat + (1|County) + (1|case), data=hdap_research_use)) #when putting interaction effect, main effects are defaultly added
anova(fit1.6,fit13) #yes fixed effect for gender identity
anova(fit13,fit14) #yes moderation effect for gender identity
#sex having the same result as gender identity -> no lgbtq bias

##making outcome var binomial and model fitting
hdap_research_use = subset(hdap_research,!is.na(duration)&!is.na(perm_housed))
summary(fit0 <- glmer(perm_housed ~ 1 + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit")))
summary(fit1 <- glmer(perm_housed ~ 1 + duration + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
anova(fit0,fit1) #duration has significant fixed effect to exit category

hdap_research_use = subset(hdap_research,!is.na(duration)&!is.na(perm_housed)&!is.na(entry_cat))
summary(fit1.1 <- glmer(perm_housed ~ 1 + duration + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit3 <- glmer(perm_housed ~ 1 + duration + entry_cat  + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
anova(fit1.1,fit3) #fixed effect from entry_location
summary(fit4 <- glmer(perm_housed ~ 1 + duration + entry_cat  + duration*entry_cat + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #when putting interaction effect, main effects are defaultly added
anova(fit3,fit4)  #no moderation effect

hdap_research_use = subset(hdap_research,!is.na(duration)&!is.na(perm_housed)&age>0)
summary(fit1.2 <- glmer(perm_housed ~ 1 + duration + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit5 <- glmer(perm_housed ~ 1 + duration + age  + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit6 <- glmer(perm_housed ~ 1 + duration + age  + duration*age + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) 
anova(fit1.2,fit5) #not significant
anova(fit5,fit6) #no interaction effect

hdap_research_use = subset(hdap_research,!is.na(duration)&!is.na(perm_housed)&!is.na(eth_cat))
summary(fit1.3 <- glmer(perm_housed ~ 1 + duration + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit7 <- glmer(perm_housed ~ 1 + duration + eth_cat  + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit8 <- glmer(perm_housed ~ 1 + duration + eth_cat  + duration*eth_cat + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #when putting interaction effect, main effects are defaultly added
anova(fit1.3,fit7) #no fixed effect for ethnicity
anova(fit7,fit8) #no moderation effect for ethnicity

hdap_research_use = subset(hdap_research,!is.na(duration)&!is.na(perm_housed)&!is.na(race_cat))
summary(fit1.4 <- glmer(perm_housed ~ 1 + duration + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit9 <- glmer(perm_housed ~ 1 + duration + race_cat  + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit10 <- glmer(perm_housed ~ 1 + duration + race_cat  + duration*race_cat + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #when putting interaction effect, main effects are defaultly added
anova(fit1.4,fit9) #no fixed effect for race
anova(fit9,fit10) #YES moderation effect for race
multinom(perm_housed~duration+race_cat,data=hdap_research_use)
multinom(perm_housed~duration+race_cat+duration*race_cat,data=hdap_research_use)
anova(multinom(exit_cat~duration+sex_cat+(1|County)+(1|case),data=hdap_research_use),
      multinom(exit_cat~duration+sex_cat+duration*sex_cat+(1|County)+(1|case),data=hdap_research_use))
em_fit = emmeans(fit10,~race_cat*duration,at=list(duration=c(10,25,50,75),race_cat=c(1,2,3,4,5,6)))
emmip(em_fit,race_cat~duration,xlab="Duration",ylab="Exit Outcome",tlab="Race")

hdap_research_use = subset(hdap_research,!is.na(duration)&!is.na(perm_housed)&!is.na(sex_cat))
summary(fit1.5 <- glmer(perm_housed ~ 1 + duration + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit11 <- glmer(perm_housed ~ 1 + duration + sex_cat  + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #put random effect last
summary(fit12 <- glmer(perm_housed ~ 1 + duration + sex_cat  + duration*sex_cat + (1|County) + (1|case), data=hdap_research_use,family=binomial(link="logit"))) #when putting interaction effect, main effects are defaultly added
anova(fit1.5,fit11) #fixed effect for sex
anova(fit11,fit12) #no moderation effect for sex

#how about relationship between entry and exit cat and moderation effect with demographics?

rm(fit0,fit1,fit1.1,fit1.2,fit1.3,fit1.4,fit2,fit2.1,fit3,fit4,fit5,fit6,
   fit7,fit8,fit9,fit10)