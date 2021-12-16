require('readxl')
data <- read_xlsx("F:/Semester 3/Stat 1/Project/data.xlsx")
summary(data)

data$`Survival, days`[data$`Survival, days` == 'lost'] <- 0
data$`Survival, days` <- as.numeric(data$`Survival, days`)
data$Death[data$Death == 'lost'] <- 0
data$Death <- as.numeric(data$Death)
df <- data
df$`Gender, 1 male, 2 female`[is.na(df$`Gender, 1 male, 2 female`)] <- 1
df <- df[, colnames(df) != 'SNAQ']
df <- df[, colnames(df) != 'MNA']
df <- df[, colnames(df) != 'NRS_2002']
df <- df[, colnames(df) != 'SGA_grade']
colnames(df)[14] <- "Preop total protein, g/l"

df['3-year_Survival'] = rep(0, 1187)
df$`3-year_Survival`[df$`Survival, days` > 3*365] <- 1
df['8-year_Survival'] = rep(0, 1187)
df$`8-year_Survival`[df$`Survival, days` > 8*365] <- 1
df['Malnurished'] <- rep(0, 1187)
df$Malnurished[df$MUST != 0] <- 1
df['age_ind_3'] <- rep(0, 1187)
df$age_ind_3[df$`Age, years` > 60.5] <- 1
df['age_ind_8'] <- rep(0, 1187)
df$age_ind_8[df$`Age, years` > 62.5] <- 1 
df['age_ind'] <- rep(0, 1187)
df$age_ind[df$`Age, years` > 59.5] <- 1 
df['lvef_ind'] <- rep(0, 1187)
df$lvef_ind[df$`Left ventricle ejection fraction, %` > 57.55] <- 1
df['icu_ind_3'] <- rep(0, 1187)
df$icu_ind_3[df$`Intansive care unit stay, days` > 5.5] <- 1
df['icu_ind_8'] <- rep(0, 1187)
df$icu_ind_8[df$`Intansive care unit stay, days` > 3.5] <- 1
df['cpb_ind_3'] <- rep(0, 1187)
df$cpb_ind_3[df$`Cardiopulmonary bypass time, min` > 119] <- 1
df['cpb_ind_8'] <- rep(0, 1187)
df$cpb_ind_8[df$`Cardiopulmonary bypass time, min` > 102.5] <- 1
df['cpb_ind'] <- rep(0, 1187)
df$cpb_ind[df$`Cardiopulmonary bypass time, min` > 111.5] <- 1
df['acc_ind'] <- rep(0, 1187)
df$acc_ind[df$`Aortic cross clamp time, min` > 73.5] <- 1
df['crp_ind_3'] <- rep(0, 1187)
df$crp_ind_3[df$`C reactive protein, mg/L` > 3.05] <- 1
df['crp_ind_8'] <- rep(0, 1187)
df$crp_ind_8[df$`C reactive protein, mg/L` > 2.25] <- 1
df['crp_ind'] <- rep(0, 1187)
df$crp_ind[df$`C reactive protein, mg/L` > 1.85] <- 1
df['alb_ind'] <- rep(0, 1187)
df$alb_ind[df$`Preop albumine, g/l` > 42.5] <- 1

require('survival')
require('survminer')

#Kaplan-Meier Curves for Overall Survival

#OS MUST
df_mal<-df[complete.cases(df$MUST),]
survival_obj <- Surv(time = df_mal$`Survival, days`, event = df_mal$Death)
malnur_diff <- survdiff(survival_obj ~ Malnurished, data = df_mal)
malnur_diff
malnur_fit <- survfit(survival_obj ~ Malnurished, data = df_mal)
ggsurvplot(malnur_fit, data = df_mal, conf.int = TRUE, pval = TRUE,legend.labs=c("Without Risk of Malnutrition","With Risk of Malnutrition"))

#OS Age
df_age<-df[complete.cases(df$`Age, years`),]
survival_obj <- Surv(time = df_age$`Survival, days`, event = df_age$Death)
age_diff <- survdiff(survival_obj ~ age_ind, data = df_age)
age_diff
age_fit <- survfit(survival_obj ~ age_ind, data = df_age)
ggsurvplot(age_fit, data = df_age, conf.int = TRUE, pval = TRUE,legend.labs=c("Age<=59.5","Age>59.5"))

#OS LVEF
df_lvef<-df[complete.cases(df$`Left ventricle ejection fraction, %`),]
survival_obj <- Surv(time = df_lvef$`Survival, days`, event = df_lvef$Death)
lvef_diff <- survdiff(survival_obj ~ lvef_ind, data = df_lvef)
lvef_diff
lvef_fit <- survfit(survival_obj ~ lvef_ind, data = df_lvef)
ggsurvplot(lvef_fit, data = df_lvef, conf.int = TRUE, pval = TRUE,legend.labs=c("LVEF<=57.55","LVEF>57.55"))

#OS ICU Stay
df_icu<-df[complete.cases(df$`Intansive care unit stay, days`),]
survival_obj <- Surv(time = df_icu$`Survival, days`, event = df_icu$Death)
icu_diff <- survdiff(survival_obj ~ icu_ind_8, data = df_icu)
icu_diff
icu_fit <- survfit(survival_obj ~ icu_ind_8, data = df_icu)
ggsurvplot(icu_fit, data = df_icu, conf.int = TRUE, pval = TRUE,legend.labs=c("ICU Stay <= 3.5","ICU Stay > 3.5"))

#OS CPB
df_cpb<-df[complete.cases(df$`Cardiopulmonary bypass time, min`),]
survival_obj <- Surv(time = df_cpb$`Survival, days`, event = df_cpb$Death)
cpb_diff <- survdiff(survival_obj ~ cpb_ind, data = df_cpb)
cpb_diff
cpb_fit <- survfit(survival_obj ~ cpb_ind, data = df_cpb)
ggsurvplot(cpb_fit, data = df_cpb, conf.int = TRUE, pval = TRUE,legend.labs=c("CPB <= 111.5","CPB>111.5"))

#OS ACC
df_acc<-df[complete.cases(df$`Aortic cross clamp time, min`),]
survival_obj <- Surv(time = df_acc$`Survival, days`, event = df_acc$Death)
acc_diff <- survdiff(survival_obj ~ acc_ind, data = df_acc)
acc_diff
acc_fit <- survfit(survival_obj ~ acc_ind, data = df_acc)
ggsurvplot(acc_fit, data = df_acc, conf.int = TRUE, pval = TRUE,legend.labs=c("ACC<=73.5","ACC>73.5"))

#OS CRP
df_crp<-df[complete.cases(df$`C reactive protein, mg/L`),]
survival_obj <- Surv(time = df_crp$`Survival, days`, event = df_crp$Death)
crp_diff <- survdiff(survival_obj ~ crp_ind, data = df_crp)
crp_diff
crp_fit <- survfit(survival_obj ~ crp_ind, data = df_crp)
ggsurvplot(crp_fit, data = df_crp, conf.int = TRUE, pval = TRUE,legend.labs=c("CRP<=1.85","CRP>1.85"))

#OS Albumin
df_alb<-df[complete.cases(df$`Preop albumine, g/l`),]
survival_obj <- Surv(time = df_alb$`Survival, days`, event = df_alb$Death)
alb_diff <- survdiff(survival_obj ~ alb_ind, data = df_alb)
alb_diff
alb_fit <- survfit(survival_obj ~ alb_ind, data = df_alb)
ggsurvplot(alb_fit, data = df_alb, conf.int = TRUE, pval = TRUE,legend.labs=c("Albumin<=42.5","Albumin>42.5"))


#Kaplan-Meier Curves for 3-yr survival
df['3y'] = rep(0, 1187)
df$`3y` = df$`Survival, days`
df$`3y`[df$`Survival, days` > 3*365] <- 1095
df['3d'] = rep(0, 1187)
df$`3d` = df$Death
df$`3d`[df$`Survival, days` > 3*365] <- 0

#3yr MUST
df_mal<-df[complete.cases(df$MUST),]
survival_3yr_obj <- Surv(time = df_mal$`3y`, event = df_mal$`3d`)
malnur_3yr_diff <- survdiff(survival_3yr_obj ~ Malnurished, data = df_mal)
malnur_3yr_diff
malnur_3yr_fit <- survfit(survival_3yr_obj ~ Malnurished, data = df_mal)
ggsurvplot(malnur_3yr_fit, data = df_mal,  pval = TRUE, legend.labs=c("Without Risk of Malnutrition","With Risk of Malnutrition"))

#3yr Age
df_age<-df[complete.cases(df$`Age, years`),]
survival_3yr_obj <- Surv(time = df_age$`3y`, event = df_age$`3d`)
age_3yr_diff <- survdiff(survival_3yr_obj ~ age_ind_3, data = df_age)
age_3yr_diff
age_3yr_fit <- survfit(survival_3yr_obj ~ age_ind_3, data = df_age)
ggsurvplot(age_3yr_fit, data = df_age,  pval = TRUE,legend.labs=c("Age<=60.5","Age>60.5"))

#3yr LVEF
df_lvef<-df[complete.cases(df$`Left ventricle ejection fraction, %`),]
survival_3yr_obj <- Surv(time = df_lvef$`3y`, event = df_lvef$`3d`)
lvef_3yr_diff <- survdiff(survival_3yr_obj ~ lvef_ind, data = df_lvef)
lvef_3yr_diff
lvef_3yr_fit <- survfit(survival_3yr_obj ~ lvef_ind, data = df_lvef)
ggsurvplot(lvef_3yr_fit, data = df_lvef,  pval = TRUE,legend.labs=c("LVEF<=57.55","LVEF>57.55"))

#3yr ICU Stay
df_icu<-df[complete.cases(df$`Intansive care unit stay, days`),]
survival_3yr_obj <- Surv(time = df_icu$`3y`, event = df_icu$`3d`)
icu_3yr_diff <- survdiff(survival_3yr_obj ~ icu_ind_3, data = df_icu)
icu_3yr_diff
icu_3yr_fit <- survfit(survival_3yr_obj ~ icu_ind_3, data = df_icu)
ggsurvplot(icu_3yr_fit, data = df_icu,  pval = TRUE,legend.labs=c("ICU Stay <= 5.5","ICU Stay > 5.5"))

#3yr CPB
df_cpb<-df[complete.cases(df$`Cardiopulmonary bypass time, min`),]
survival_3yr_obj <- Surv(time = df_cpb$`3y`, event = df_cpb$`3d`)
cpb_3yr_diff <- survdiff(survival_3yr_obj ~ cpb_ind_3, data = df_cpb)
cpb_3yr_diff
cpb_3yr_fit <- survfit(survival_3yr_obj ~ cpb_ind_3, data = df_cpb)
ggsurvplot(cpb_3yr_fit, data = df_cpb,  pval = TRUE,legend.labs=c("CPB<=119","CPB>119"))

#3yr ACC
df_acc<-df[complete.cases(df$`Aortic cross clamp time, min`),]
survival_3yr_obj <- Surv(time = df_acc$`3y`, event = df_acc$`3d`)
acc_3yr_diff <- survdiff(survival_3yr_obj ~ acc_ind, data = df_acc)
acc_3yr_diff
acc_3yr_fit <- survfit(survival_3yr_obj ~ acc_ind, data = df_acc)
ggsurvplot(acc_3yr_fit, data = df_acc,  pval = TRUE,legend.labs=c("ACC<=73.5","ACC>73.5"))

#3yr CRP
df_crp<-df[complete.cases(df$`C reactive protein, mg/L`),]
survival_3yr_obj <- Surv(time = df_crp$`3y`, event = df_crp$`3d`)
crp_3yr_diff <- survdiff(survival_3yr_obj ~ crp_ind_3, data = df_crp)
crp_3yr_diff
crp_3yr_fit <- survfit(survival_3yr_obj ~ crp_ind_3, data = df_crp)
ggsurvplot(crp_3yr_fit, data = df_crp,  pval = TRUE,legend.labs=c("CRP<=3.05","CRP>3.05"))

#3yr Albumin
df_alb<-df[complete.cases(df$`Preop albumine, g/l`),]
survival_3yr_obj <- Surv(time = df_alb$`3y`, event = df_alb$`3d`)
alb_3yr_diff <- survdiff(survival_3yr_obj ~ alb_ind, data = df_alb)
alb_3yr_diff
alb_3yr_fit <- survfit(survival_3yr_obj ~ alb_ind, data = df_alb)
ggsurvplot(alb_3yr_fit, data = df_alb,  pval = TRUE,legend.labs=c("Albumin<=42.5","Albumin>42.5"))

#Kaplan-Meier Curves for 8-yr survival
df_8yr <- subset(df, df$`Survival, days` <= 8*365)
df['8y'] = rep(0, 1187)
df$`8y` = df$`Survival, days`
df$`8y`[df$`Survival, days` > 8*365] <- 2920
df['8d'] = rep(0, 1187)
df$`8d` = df$Death
df$`8d`[df$`Survival, days` > 8*365] <- 0

#8yr MUST
df_mal<-df[complete.cases(df$MUST),]
survival_8yr_obj <- Surv(time = df_mal$`8y`, event = df_mal$`8d`)
malnur_8yr_diff <- survdiff(survival_8yr_obj ~ Malnurished, data = df_mal)
malnur_8yr_diff
malnur_8yr_fit <- survfit(survival_8yr_obj ~ Malnurished, data = df_mal)
ggsurvplot(malnur_8yr_fit, data = df_mal,  pval = TRUE,legend.labs=c("Without Risk of Malnutrition","With Risk of Malnutrition"))

#8yr Age
df_age<-df[complete.cases(df$`Age, years`),]
survival_8yr_obj <- Surv(time = df_age$`8y`, event = df_age$`8d`)
age_8yr_diff <- survdiff(survival_8yr_obj ~ age_ind_8, data = df_age)
age_8yr_diff
age_8yr_fit <- survfit(survival_8yr_obj ~ age_ind_8, data = df_age)
ggsurvplot(age_8yr_fit, data = df_age,  pval = TRUE,legend.labs=c("Age<=62.5","Age>62.5"))

#8yr LVEF
df_lvef<-df[complete.cases(df$`Left ventricle ejection fraction, %`),]
survival_8yr_obj <- Surv(time = df_lvef$`8y`, event = df_lvef$`8d`)
lvef_8yr_diff <- survdiff(survival_8yr_obj ~ lvef_ind, data = df_lvef)
lvef_8yr_diff
lvef_8yr_fit <- survfit(survival_8yr_obj ~ lvef_ind, data = df_lvef)
ggsurvplot(lvef_8yr_fit, data = df_lvef,  pval = TRUE,legend.labs=c("LVEF<=57.55","LVEF>57.55"))

#8yr ICU Stay
df_icu<-df[complete.cases(df$`Intansive care unit stay, days`),]
survival_8yr_obj <- Surv(time = df_icu$`8y`, event = df_icu$`8d`)
icu_8yr_diff <- survdiff(survival_8yr_obj ~ icu_ind_8, data = df_icu)
icu_8yr_diff
icu_8yr_fit <- survfit(survival_8yr_obj ~ icu_ind_8, data = df_icu)
ggsurvplot(icu_8yr_fit, data = df_icu,  pval = TRUE,legend.labs=c("ICU Stay <= 3.5","ICU Stay > 3.5"))

#8yr CPB
df_cpb<-df[complete.cases(df$`Cardiopulmonary bypass time, min`),]
survival_8yr_obj <- Surv(time = df_cpb$`8y`, event = df_cpb$`8d`)
cpb_8yr_diff <- survdiff(survival_8yr_obj ~ cpb_ind_8, data = df_cpb)
cpb_8yr_diff
cpb_8yr_fit <- survfit(survival_8yr_obj ~ cpb_ind_8, data = df_cpb)
ggsurvplot(cpb_8yr_fit, data = df_cpb,  pval = TRUE,legend.labs=c("CPB<=102.5","CPB>102.5"))

#8yr ACC
df_acc<-df[complete.cases(df$`Aortic cross clamp time, min`),]
survival_8yr_obj <- Surv(time = df_acc$`8y`, event = df_acc$`8d`)
acc_8yr_diff <- survdiff(survival_8yr_obj ~ acc_ind, data = df_acc)
acc_8yr_diff
acc_8yr_fit <- survfit(survival_8yr_obj ~ acc_ind, data = df_acc)
ggsurvplot(acc_8yr_fit, data = df_acc,  pval = TRUE,legend.labs=c("ACC<=73.5","ACC>73.5"))

#8yr CRP
df_crp<-df[complete.cases(df$`C reactive protein, mg/L`),]
survival_8yr_obj <- Surv(time = df_crp$`8y`, event = df_crp$`8d`)
crp_8yr_diff <- survdiff(survival_8yr_obj ~ crp_ind_8, data = df_crp)
crp_8yr_diff
crp_8yr_fit <- survfit(survival_8yr_obj ~ crp_ind_8, data = df_crp)
ggsurvplot(crp_8yr_fit, data = df_crp,  pval = TRUE,legend.labs=c("CRP<=2.25","CRP>2.25"))

#8yr Albumin
df_alb<-df[complete.cases(df$`Preop albumine, g/l`),]
survival_8yr_obj <- Surv(time = df_alb$`8y`, event = df_alb$`8d`)
alb_8yr_diff <- survdiff(survival_8yr_obj ~ alb_ind, data = df_alb)
alb_8yr_diff
alb_8yr_fit <- survfit(survival_8yr_obj ~ alb_ind, data = df_alb)
ggsurvplot(alb_8yr_fit, data = df_alb,  pval = TRUE,legend.labs=c("Albumin<=42.5","Albumin>42.5"))

df_cad <- subset(df, df$`Primary pathology, 1_coronary_artery_disease 2_heart_valve_disease` == 1)
df_hvd <- subset(df, df$`Primary pathology, 1_coronary_artery_disease 2_heart_valve_disease` == 2)

# Kaplan-Meier OS curves for patients with HVD
df_mal_hvd<-df_hvd[complete.cases(df_hvd$MUST),]
surv_hvd_obj <- Surv(time = df_mal_hvd$`Survival, days`, event = df_mal_hvd$Death)
hvd_fit <- survfit(surv_hvd_obj ~ Malnurished, data = df_mal_hvd)
ggsurvplot(hvd_fit, data = df_mal_hvd, conf.int = TRUE, pval = TRUE,legend.labs=c("Without Risk of Malnutrition","With Risk of Malnutrition"))
surv_hvd_diff <- survdiff(surv_hvd_obj ~ Malnurished, data = df_mal_hvd)
surv_hvd_diff

# Kaplan-Meier OS curves for patients with CAD
df_mal_cad<-df_cad[complete.cases(df_cad$MUST),]
surv_cad_obj <- Surv(time = df_mal_cad$`Survival, days`, event = df_mal_cad$Death)
cad_fit <- survfit(surv_cad_obj ~ Malnurished, data = df_mal_cad)
ggsurvplot(cad_fit, data = df_mal_cad, conf.int = TRUE, pval = TRUE,legend.labs=c("Without Risk of Malnutrition","With Risk of Malnutrition"))
surv_cad_diff <- survdiff(surv_cad_obj ~ Malnurished, data = df_mal_cad)
surv_cad_diff

#Kaplan-Meier 3yr curves for patients with HVD
df_mal_hvd<-df_hvd[complete.cases(df_hvd$MUST),]
surv_3_hvd_obj <- Surv(time = df_mal_hvd$`3y`, event = df_mal_hvd$`3d`)
surv_3_hvd_diff <- survdiff(surv_3_hvd_obj ~ Malnurished, data = df_mal_hvd)
surv_3_hvd_diff
hvd_3_fit <- survfit(surv_3_hvd_obj ~ Malnurished, data = df_mal_hvd)
ggsurvplot(hvd_3_fit, data = df_mal_hvd,  pval = TRUE,legend.labs=c("Without Risk of Malnutrition","With Risk of Malnutrition"))

#Kaplan-Meier 3yr curves for patients with CAD
df_mal_cad<-df_cad[complete.cases(df_cad$MUST),]
surv_3_cad_obj <- Surv(time = df_mal_cad$`3y`, event = df_mal_cad$`3d`)
surv_3_cad_diff <- survdiff(surv_3_cad_obj ~ Malnurished, data = df_mal_cad)
surv_3_cad_diff
cad_3_fit <- survfit(surv_3_cad_obj ~ Malnurished, data = df_mal_cad)
ggsurvplot(cad_3_fit, data = df_mal_cad,  pval = TRUE,legend.labs=c("Without Risk of Malnutrition","With Risk of Malnutrition"))

#Kaplan-Meier 8yr curves for patients with HVD
df_mal_hvd<-df_hvd[complete.cases(df_hvd$MUST),]
surv_8_hvd_obj <- Surv(time = df_mal_hvd$`8y`, event = df_mal_hvd$`8d`)
surv_8_hvd_diff <- survdiff(surv_8_hvd_obj ~ Malnurished, data = df_mal_hvd)
surv_8_hvd_diff
hvd_8_fit <- survfit(surv_8_hvd_obj ~ Malnurished, data = df_mal_hvd)
ggsurvplot(hvd_8_fit, data = df_mal_hvd,  pval = TRUE,legend.labs=c("Without Risk of Malnutrition","With Risk of Malnutrition"))

#Kaplan-Meier 8yr curves for patients with CAD
df_mal_cad<-df_cad[complete.cases(df_cad$MUST),]
surv_8_cad_obj <- Surv(time = df_mal_cad$`8y`, event = df_mal_cad$`8d`)
surv_8_cad_diff <- survdiff(surv_8_cad_obj ~ Malnurished, data = df_mal_cad)
surv_8_cad_diff
cad_8_fit <- survfit(surv_8_cad_obj ~ Malnurished, data = df_mal_cad)
ggsurvplot(cad_8_fit, data = df_mal_cad,  pval = TRUE,legend.labs=c("Without Risk of Malnutrition","With Risk of Malnutrition"))
