require('readxl')
data <- read_xlsx("F:/Semester 3/Stat 1/Project/data.xlsx")

data$`Survival, days`[data$`Survival, days` == 'lost'] <- NA
data$`Survival, days` <- as.numeric(data$`Survival, days`)
data$Death[data$Death == 'lost'] <- NA
data$Death <- as.numeric(data$Death)
df <- data

df$`Gender, 1 male, 2 female`[is.na(df$`Gender, 1 male, 2 female`)] <- 1
df <- df[, colnames(df) != 'SNAQ']
df <- df[, colnames(df) != 'MNA']
df <- df[, colnames(df) != 'NRS_2002']
df <- df[, colnames(df) != 'SGA_grade']

df['3yr_time'] <- rep(0, 1187)
df$`3yr_time` <- df$`Survival, days`
df$`3yr_time`[df$`Survival, days` > 3*365] <- 3*365
df['3yr_event'] <- rep(0, 1187)
df$`3yr_event` <- df$Death
df$`3yr_event`[df$`Survival, days` > 3*365] <- 0


require('survival')
require('survminer')

## UNIVARIATE ANALYSIS FOR 3 YR SURVIVAL ##

# Uni-variate(Logistic Euroscore) Cox Regression Model for 3 yr survival 
df_log <- df[complete.cases(df$`Logistic Euroscore`),]
survival_3_yr <- Surv(time = df_log$`3yr_time`, event = df_log$`3yr_event`)
model_euro_3 <- coxph(survival_3_yr ~ df_log$`Logistic Euroscore`)
summary(model_euro_3)
ph_test <- cox.zph(model_euro_3)
ggcoxzph(ph_test)

# Uni-variate(CPB) Cox Regression Model for 3 yr survival
df_cpb <- df[complete.cases(df$`Cardiopulmonary bypass time, min`),]
survival_3_yr <- Surv(time = df_cpb$`3yr_time`, event = df_cpb$`3yr_event`)
df_cpb['cpb_ind_3'] <- rep(0, 1187)
df_cpb$cpb_ind_3[df_cpb$`Cardiopulmonary bypass time, min` > 119] <- 1
model_cpb_3 <- coxph(survival_3_yr ~ df_cpb$cpb_ind_3)
summary(model_cpb_3)
ph_test <- cox.zph(model_cpb_3)
ggcoxzph(ph_test)

# Uni-variate(MUST) Cox Regression Model for 3 yr survival
df_mal <- df[complete.cases(df$MUST),]
survival_3_yr <- Surv(time = df_mal$`3yr_time`, event = df_mal$`3yr_event`)
df_mal['must_ind_3'] <- rep(0, 1187)
df_mal$must_ind_3[df_mal$MUST > 0] <- 1
model_must_3 <- coxph(survival_3_yr ~ df_mal$must_ind_3)
summary(model_must_3)
ph_test <- cox.zph(model_must_3)
ggcoxzph(ph_test)

# Uni-variate(CRP) Cox Regression Model for 3 yr survival
df_crp <- df[complete.cases(df$`C reactive protein, mg/L`),]
survival_3_yr <- Surv(time = df_crp$`3yr_time`, event = df_crp$`3yr_event`)
df_crp['crp_ind_3'] <- rep(0, 592)
df_crp$crp_ind_3[df_crp$`C reactive protein, mg/L` > 3.05] <- 1
model_crp_3 <- coxph(survival_3_yr ~ df_crp$crp_ind_3)
summary(model_crp_3)
ph_test <- cox.zph(model_crp_3)
ggcoxzph(ph_test)

# Uni-variate(Albumin) Cox Regression Model for 3 yr survival
df_alb <- df[complete.cases(df$`Preop albumine, g/l`),]
survival_3_yr <- Surv(time = df_alb$`3yr_time`, event = df_alb$`3yr_event`)
df_alb['alb_ind_3'] <- rep(0, 991)
df_alb$alb_ind_3[df_alb$`Preop albumine, g/l` > 42.5] <- 1
model_alb_3 <- coxph(survival_3_yr ~ df_alb$alb_ind_3)
summary(model_alb_3)
ph_test <- cox.zph(model_alb_3)
ggcoxzph(ph_test)

# Uni-variate(Age) Cox Regression Model for 3 yr survival
df_age <- df[complete.cases(df$`Age, years`),]
survival_3_yr <- Surv(time = df_age$`3yr_time`, event = df_age$`3yr_event`)
df_age['age_ind_3'] <- rep(0, 1187)
df_age$age_ind_3[df_age$`Age, years` > 60.5] <- 1
model_age_3 <- coxph(survival_3_yr ~ df_age$age_ind_3)
summary(model_age_3)
ph_test <- cox.zph(model_age_3)
ggcoxzph(ph_test)

# Uni-variate(ACC) Cox Regression Model for 3 yr survival
df_acc <- df[complete.cases(df$`Aortic cross clamp time, min`),]
survival_3_yr <- Surv(time = df_acc$`3yr_time`, event = df_acc$`3yr_event`)
df_acc['acc_ind_3'] <- rep(0, 1187)
df_acc$acc_ind_3[df_acc$`Aortic cross clamp time, min` > 73.5] <- 1
model_acc_3 <- coxph(survival_3_yr ~ df_acc$acc_ind_3)
summary(model_acc_3)
ph_test <- cox.zph(model_acc_3)
ggcoxzph(ph_test)

# Uni-variate(ICU Stay) Cox Regression Model for 3 yr survival 
df_icu <- df[complete.cases(df$`Intansive care unit stay, days`),]
survival_3_yr <- Surv(time = df_icu$`3yr_time`, event = df_icu$`3yr_event`)
df_icu['icu_ind_3'] <- rep(0, 1187)
df_icu$icu_ind_3[df_icu$`Intansive care unit stay, days`> 5.5] <- 1
model_icu_3 <- coxph(survival_3_yr ~ df_icu$`Intansive care unit stay, days`)
summary(model_icu_3)
ph_test <- cox.zph(model_icu_3)
ggcoxzph(ph_test)


## UNIVARIATE ANALYSIS FOR 8 YR SURVIVAL ##

df['8yr_time'] <- rep(0, 1187)
df$`8yr_time` <- df$`Survival, days`
df$`8yr_time`[df$`Survival, days` > 8*365] <- 8*365
df['8yr_event'] <- rep(0, 1187)
df$`8yr_event` <- df$Death
df$`8yr_event`[df$`Survival, days` > 8*365] <- 0

# Uni-variate(Logistic Euroscore) Cox Regression Model for 8 yr survival 
df_log <- df[complete.cases(df$`Logistic Euroscore`),]
survival_8_yr <- Surv(time = df_log$`8yr_time`, event = df_log$`8yr_event`)
model_euro_8 <- coxph(survival_8_yr ~ df_log$`Logistic Euroscore`)
summary(model_euro_8)
ph_test <- cox.zph(model_euro_8)
ggcoxzph(ph_test)

# Uni-variate(CPB) Cox Regression Model for 8 yr survival
df_cpb <- df[complete.cases(df$`Cardiopulmonary bypass time, min`),]
survival_8_yr <- Surv(time = df_cpb$`8yr_time`, event = df_cpb$`8yr_event`)
df_cpb['cpb_ind_8'] <- rep(0, 1187)
df_cpb$cpb_ind_8[df_cpb$`Cardiopulmonary bypass time, min` > 102.5] <- 1
model_cpb_8 <- coxph(survival_8_yr ~ df_cpb$cpb_ind_8)
summary(model_cpb_8)
ph_test <- cox.zph(model_cpb_8)
ggcoxzph(ph_test)

# Uni-variate(MUST) Cox Regression Model for 8 yr survival
df_mal <- df[complete.cases(df$MUST),]
survival_8_yr <- Surv(time = df_mal$`8yr_time`, event = df_mal$`8yr_event`)
df_mal['must_ind_8'] <- rep(0, 1187)
df_mal$must_ind_8[df_mal$MUST > 0] <- 1
model_must_8 <- coxph(survival_8_yr ~ df_mal$must_ind_8)
summary(model_must_8)
ph_test <- cox.zph(model_must_8)
ggcoxzph(ph_test)

# Uni-variate(CRP) Cox Regression Model for 8 yr survival
df_crp <- df[complete.cases(df$`C reactive protein, mg/L`),]
survival_8_yr <- Surv(time = df_crp$`8yr_time`, event = df_crp$`8yr_event`)
df_crp['crp_ind_8'] <- rep(0, 592)
df_crp$crp_ind_8[df_crp$`C reactive protein, mg/L` > 2.25] <- 1
model_crp_8 <- coxph(survival_8_yr ~ df_crp$crp_ind_8)
summary(model_crp_8)
ph_test <- cox.zph(model_crp_8)
ggcoxzph(ph_test)

# Uni-variate(Albumin) Cox Regression Model for 8 yr survival
df_alb <- df[complete.cases(df$`Preop albumine, g/l`),]
survival_8_yr <- Surv(time = df_alb$`8yr_time`, event = df_alb$`8yr_event`)
df_alb['alb_ind_8'] <- rep(0, 991)
df_alb$alb_ind_8[df_alb$`Preop albumine, g/l` > 42.5] <- 1
model_alb_8 <- coxph(survival_8_yr ~ df_alb$alb_ind_8)
summary(model_alb_8)
ph_test <- cox.zph(model_alb_8)
ggcoxzph(ph_test)

# Uni-variate(Age) Cox Regression Model for 8 yr survival
df_age <- df[complete.cases(df$`Age, years`),]
survival_8_yr <- Surv(time = df_age$`3yr_time`, event = df_age$`3yr_event`)
df_age['age_ind_8'] <- rep(0, 1187)
df_age$age_ind_8[df_age$`Age, years` > 62.5] <- 1
model_age_8 <- coxph(survival_8_yr ~ df_age$age_ind_8)
summary(model_age_8)
ph_test <- cox.zph(model_age_8)
ggcoxzph(ph_test)

# Uni-variate(ACC) Cox Regression Model for 8 yr survival
df_acc <- df[complete.cases(df$`Aortic cross clamp time, min`),]
survival_8_yr <- Surv(time = df_acc$`8yr_time`, event = df_acc$`8yr_event`)
df_acc['acc_ind_8'] <- rep(0, 1187)
df_acc$acc_ind_8[df_acc$`Aortic cross clamp time, min` > 73.5] <- 1
model_acc_8 <- coxph(survival_8_yr ~ df_acc$acc_ind_8)
summary(model_acc_8)
ph_test <- cox.zph(model_acc_8)
ggcoxzph(ph_test)

# Uni-variate(ICU Stay) Cox Regression Model for 8 yr survival 
df_icu <- df[complete.cases(df$`Intansive care unit stay, days`),]
survival_8_yr <- Surv(time = df_icu$`8yr_time`, event = df_icu$`8yr_event`)
df_icu['icu_ind_8'] <- rep(0, 1187)
df_icu$icu_ind_8[df_icu$`Intansive care unit stay, days`> 3.5] <- 1
model_icu_8 <- coxph(survival_8_yr ~ df_icu$`Intansive care unit stay, days`)
summary(model_icu_8)
ph_test <- cox.zph(model_icu_8)
ggcoxzph(ph_test)



## UNIVARIATE ANALYSIS FOR OS ##

# Uni-variate(Logistic Euroscore) Cox Regression Model for OS
df_log <- df[complete.cases(df$`Logistic Euroscore`),]
survival <- Surv(time = df_log$`Survival, days`, event = df_log$Death)
model_euro_os <- coxph(survival ~ df_log$`Logistic Euroscore`)
summary(model_euro_os)
ph_test <- cox.zph(model_euro_os)
ggcoxzph(ph_test)

# Uni-variate(CPB) Cox Regression Model for OS
df_cpb <- df[complete.cases(df$`Cardiopulmonary bypass time, min`),]
survival <- Surv(time = df_cpb$`Survival, days`, event = df_cpb$Death)
df_cpb['cpb_ind_os'] <- rep(0, 1187)
df_cpb$cpb_ind_os[df_cpb$`Cardiopulmonary bypass time, min` > 111.5] <- 1
model_cpb_os <- coxph(survival ~ df_cpb$cpb_ind_os)
summary(model_cpb_os)
ph_test <- cox.zph(model_cpb_os)
ggcoxzph(ph_test)

# Uni-variate(MUST) Cox Regression Model for OS
df_mal <- df[complete.cases(df$MUST),]
survival <- Surv(time = df_mal$`Survival, days`, event = df_mal$Death)
df_mal['must_ind_os'] <- rep(0, 1187)
df_mal$must_ind_os[df_mal$MUST > 0] <- 1
model_must_os <- coxph(survival ~ df_mal$must_ind_os)
summary(model_must_os)
ph_test <- cox.zph(model_must_os)
ggcoxzph(ph_test)

# Uni-variate(CRP) Cox Regression Model for OS
df_crp <- df[complete.cases(df$`C reactive protein, mg/L`),]
survival <- Surv(time = df_crp$`Survival, days`, event = df_crp$Death)
df_crp['crp_ind_os'] <- rep(0, 592)
df_crp$crp_ind_os[df_crp$`C reactive protein, mg/L` > 1.85] <- 1
model_crp_os <- coxph(survival ~ df_crp$crp_ind_os)
summary(model_crp_os)
ph_test <- cox.zph(model_crp_os)
ggcoxzph(ph_test)

# Uni-variate(Albumin) Cox Regression Model for OS
df_alb <- df[complete.cases(df$`Preop albumine, g/l`),]
survival <- Surv(time = df_alb$`Survival, days`, event = df_alb$Death)
df_alb['alb_ind_os'] <- rep(0, 991)
df_alb$alb_ind_os[df_alb$`Preop albumine, g/l` > 42.5] <- 1
model_alb_os <- coxph(survival ~ df_alb$alb_ind_os)
summary(model_alb_os)
ph_test <- cox.zph(model_alb_os)
ggcoxzph(ph_test)

# Uni-variate(Age) Cox Regression Model for OS
df_age <- df[complete.cases(df$`Age, years`),]
survival <- Surv(time = df_age$`3yr_time`, event = df_age$`3yr_event`)
df_age['age_ind_os'] <- rep(0, 1187)
df_age$age_ind_os[df_age$`Age, years` > 59.5] <- 1
model_age_os <- coxph(survival ~ df_age$age_ind_os)
summary(model_age_os)
ph_test <- cox.zph(model_age_os)
ggcoxzph(ph_test)

# Uni-variate(ACC) Cox Regression Model for OS
df_acc <- df[complete.cases(df$`Aortic cross clamp time, min`),]
survival <- Surv(time = df_acc$`Survival, days`, event = df_acc$Death)
df_acc['acc_ind_8'] <- rep(0, 1187)
df_acc$acc_ind_8[df_acc$`Aortic cross clamp time, min` > 73.5] <- 1
model_acc_os <- coxph(survival ~ df_acc$acc_ind_8)
summary(model_acc_os)
ph_test <- cox.zph(model_acc_os)
ggcoxzph(ph_test)

# Uni-variate(ICU Stay) Cox Regression Model for OS
df_icu <- df[complete.cases(df$`Intansive care unit stay, days`),]
survival <- Surv(time = df_icu$`Survival, days`, event = df_icu$Death)
df_icu['icu_ind_8'] <- rep(0, 1187)
df_icu$icu_ind_8[df_icu$`Intansive care unit stay, days`> 3.5] <- 1
model_icu_os <- coxph(survival ~ df_icu$`Intansive care unit stay, days`)
summary(model_icu_os)
ph_test <- cox.zph(model_icu_os)
ggcoxzph(ph_test)


## MULTIVARIATE ANALYSIS ##
df <- df[complete.cases(df$`Aortic cross clamp time, min`),]
df <- df[complete.cases(df$`Logistic Euroscore`),]
df <- df[complete.cases(df$`Cardiopulmonary bypass time, min`),]
df <- df[complete.cases(df$`C reactive protein, mg/L`),]
df <- df[complete.cases(df$`Preop albumine, g/l`),]
df <- df[complete.cases(df$MUST),]
df <- df[complete.cases(df$`Age, years`),]
df <- df[complete.cases(df$`Intansive care unit stay, days`),]
df['icu_ind_3'] <- rep(0, 467)
df$icu_ind_3[df$`Intansive care unit stay, days`> 5.5] <- 1
df['icu_ind'] <- rep(0, 467)
df$icu_ind[df$`Intansive care unit stay, days`> 3.5] <- 1

df['cpb_ind_3'] <- rep(0, 467)
df$cpb_ind_3[df$`Cardiopulmonary bypass time, min` > 119] <- 1
df['cpb_ind_8'] <- rep(0, 467)
df$cpb_ind_8[df$`Cardiopulmonary bypass time, min` > 102.5] <- 1
df['cpb_ind_os'] <- rep(0, 467)
df$cpb_ind_os[df$`Cardiopulmonary bypass time, min` > 111.5] <- 1

df['must_ind'] <- rep(0, 467)
df$must_ind[df$MUST > 0] <- 1

df['crp_ind_3'] <- rep(0, 467)
df$crp_ind_3[df$`C reactive protein, mg/L` > 3.05] <- 1
df['crp_ind_8'] <- rep(0, 467)
df$crp_ind_8[df$`C reactive protein, mg/L` > 2.25] <- 1
df['crp_ind_os'] <- rep(0, 467)
df$crp_ind_os[df$`C reactive protein, mg/L` > 1.85] <- 1

df['alb_ind'] <- rep(0, 467)
df$alb_ind[df$`Preop albumine, g/l` > 42.5] <- 1

df['age_ind_3'] <- rep(0, 467)
df$age_ind_3[df$`Age, years` > 60.5] <- 1
df['age_ind_8'] <- rep(0, 467)
df$age_ind_8[df$`Age, years` > 62.5] <- 1
df['age_ind_os'] <- rep(0, 467)
df$age_ind_os[df$`Age, years` > 59.5] <- 1

df['acc_ind'] <- rep(0, 467)
df$acc_ind[df$`Aortic cross clamp time, min` > 73.5] <- 1

#For 3yr

survival_3_yr <- Surv(time = df$`3yr_time`, event = df$`3yr_event`)

# Full mixed Cox Model
model_mixed_3 <- coxph(survival_3_yr ~ df$`Logistic Euroscore` + df$cpb_ind_3 + df$must_ind + df$crp_ind_3 
                       + df$alb_ind + df$age_ind_3 + df$acc_ind + df$icu_ind_3)
summary(model_mixed_3)


model_mixed_3 <- coxph(survival_3_yr ~ df$`Logistic Euroscore` + df$cpb_ind_3 + df$must_ind + df$crp_ind_3 
                       + df$alb_ind + df$acc_ind + df$icu_ind_3)

summary(model_mixed_3)

model_mixed_3 <- coxph(survival_3_yr ~ df$cpb_ind_3 + df$crp_ind_3 + df$alb_ind + df$acc_ind + df$must_ind + df$icu_ind_3)
summary(model_mixed_3)

model_mixed_3 <- coxph(survival_3_yr ~ df$cpb_ind_3 + df$crp_ind_3 + df$alb_ind + df$acc_ind + df$icu_ind_3)
summary(model_mixed_3)

model_mixed_3 <- coxph(survival_3_yr ~ df$cpb_ind_3 + df$crp_ind_3 + df$alb_ind + df$icu_ind_3)
summary(model_mixed_3)

model_mixed_3 <- coxph(survival_3_yr ~ df$crp_ind_3 + df$alb_ind + df$icu_ind_3)
summary(model_mixed_3) # Optimal model by step-wise eliminating variables with p>0.05
ph_test <- cox.zph(model_mixed_3)
ggcoxzph(ph_test)

# Optimal Cox Model
model_optimal_3 <- coxph(survival_3_yr ~ df$`Logistic Euroscore` + df$cpb_ind_3 + df$alb_ind)
summary(model_optimal_3)
ph_test <- cox.zph(model_optimal_3)
ggcoxzph(ph_test)


#For 8yr

survival_8_yr <- Surv(time = df$`8yr_time`, event = df$`8yr_event`)

# Full mixed Cox Model
model_mixed_8 <- coxph(survival_8_yr ~ df$`Logistic Euroscore` + df$cpb_ind_8 + df$must_ind + df$crp_ind_8 
                       + df$alb_ind + df$age_ind_8 + df$acc_ind + df$icu_ind)
summary(model_mixed_8)

model_mixed_8 <- coxph(survival_8_yr ~ df$`Logistic Euroscore` + df$cpb_ind_8 + df$must_ind + df$crp_ind_8 
                       + df$alb_ind + df$acc_ind + df$icu_ind)
summary(model_mixed_8)

model_mixed_8 <- coxph(survival_8_yr ~ df$`Logistic Euroscore` + df$cpb_ind_8 + df$crp_ind_8 + df$alb_ind
                       + df$acc_ind + df$icu_ind)
summary(model_mixed_8)

model_mixed_8 <- coxph(survival_8_yr ~ df$`Logistic Euroscore` + df$cpb_ind_8 + df$alb_ind + df$crp_ind_8 + df$icu_ind)
summary(model_mixed_8)

model_mixed_8 <- coxph(survival_8_yr ~ df$`Logistic Euroscore` + df$cpb_ind_8 + df$alb_ind + df$crp_ind_8)
summary(model_mixed_8)

model_mixed_8 <- coxph(survival_8_yr ~ df$`Logistic Euroscore` + df$cpb_ind_8 + df$alb_ind)
summary(model_mixed_8) # Optimal model by step-wise elimination of variables with p>0.05
ph_test <- cox.zph(model_mixed_8)
ggcoxzph(ph_test)

# Optimal Cox Model
model_optimal_8 <- coxph(survival_8_yr ~ df$`Logistic Euroscore` + df$cpb_ind_8 + df$alb_ind)
summary(model_optimal_8)
ph_test <- cox.zph(model_optimal_8)
ggcoxzph(ph_test)

#For OS

survival<- Surv(time = df$`Survival, days`, event = df$Death)

# Full mixed Cox Model
# Backward Wald Selection
model_mixed_os <- coxph(survival ~ df$`Logistic Euroscore` + df$cpb_ind_os + df$must_ind + df$crp_ind_os 
                        + df$alb_ind + df$age_ind_os + df$acc_ind + df$icu_ind)
summary(model_mixed_os)

model_mixed_os <- coxph(survival ~ df$`Logistic Euroscore` + df$cpb_ind_os + df$must_ind + df$crp_ind_os 
                        + df$alb_ind + df$age_ind_os + df$acc_ind)
summary(model_mixed_os)


model_mixed_os <- coxph(survival ~ df$`Logistic Euroscore` + df$cpb_ind_os + df$crp_ind_os + df$alb_ind 
                        + df$age_ind_os + df$acc_ind)
summary(model_mixed_os)

model_mixed_os <- coxph(survival ~ df$`Logistic Euroscore` + df$cpb_ind_os + df$crp_ind_os + df$alb_ind
                        + df$acc_ind)
summary(model_mixed_os)

model_mixed_os <- coxph(survival ~ df$`Logistic Euroscore` + df$cpb_ind_os + df$crp_ind_os + df$alb_ind)
summary(model_mixed_os)

model_mixed_os <- coxph(survival ~ df$cpb_ind_os + df$crp_ind_os + df$alb_ind)
summary(model_mixed_os) # Optimal model by step-wise eliminating variables with p>0.05
ph_test <- cox.zph(model_mixed_os)
ggcoxzph(ph_test)

# Optimal Cox Model
model_optimal_os <- coxph(survival ~ df$`Logistic Euroscore` + df$cpb_ind_os + df$alb_ind)
summary(model_optimal_os)
ph_test <- cox.zph(model_optimal_os)
ggcoxzph(ph_test)

