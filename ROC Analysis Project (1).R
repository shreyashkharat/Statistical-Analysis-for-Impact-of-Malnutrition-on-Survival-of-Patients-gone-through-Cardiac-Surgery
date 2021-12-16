install.packages("pROC")
library(pROC)

require('readxl')
data <- read_xlsx("F:/Semester 3/Stat 1/Project/data.xlsx")
View(data)
data$Death[data$Death == 'lost'] <- NA
data$Death <- as.numeric(data$Death)

#Mixed cohort OS

par(pty='s')

#Age
data_age<-data[complete.cases(data$`Age, years`),]
roc_age<-roc(data_age$Death,data_age$`Age, years`)
roc_age
plot(roc_age,col='blue',legacy.axes=TRUE, grid= TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_age,"best")$threshold,roc_age$auc,wilcox.test(data_age$`Age, years`~data_age$Death, data=data_age)$p.value),col='blue')
coords(roc_age,"best")
wilcox.test(data_age$`Age, years`~data_age$Death, data=data_age)

#Albumin
data_alb<-data[complete.cases(data$`Preop albumine, g/l`),]
roc_alb<-roc(data_alb$Death,data_alb$`Preop albumine, g/l`)
roc_alb
plot(roc_alb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_alb,"best")$threshold,roc_alb$auc,wilcox.test(data_alb$`Preop albumine, g/l`~data_alb$Death, data=data_alb)$p.value),col='blue')
coords(roc_alb,"best")
wilcox.test(data_alb$`Preop albumine, g/l`~data_alb$Death, data=data_alb)

#CPB
data_cpb<-data[complete.cases(data$`Cardiopulmonary bypass time, min`),]
roc_cpb<-roc(data_cpb$Death,data_cpb$`Cardiopulmonary bypass time, min`)
roc_cpb
plot(roc_cpb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_cpb,"best")$threshold,roc_cpb$auc,wilcox.test(data_cpb$`Cardiopulmonary bypass time, min`~data_cpb$Death, data=data_cpb)$p.value),col='blue')
coords(roc_cpb,"best")
wilcox.test(data_cpb$`Cardiopulmonary bypass time, min`~data_cpb$Death, data=data_cpb)


#ACC
data_acc<-data[complete.cases(data$`Aortic cross clamp time, min`),]
roc_acc<-roc(data_acc$Death,data_acc$`Aortic cross clamp time, min`)
roc_acc
plot(roc_acc,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_acc,"best")$threshold,roc_acc$auc,wilcox.test(data_acc$`Aortic cross clamp time, min`~data_acc$Death, data=data_acc)$p.value),col='blue')
coords(roc_acc,"best")
wilcox.test(data_acc$`Aortic cross clamp time, min`~data_acc$Death, data=data_acc)

#CRP
data_crp<-data[complete.cases(data$`C reactive protein, mg/L`),]
roc_crp<-roc(data_crp$Death,data_crp$`C reactive protein, mg/L`)
roc_crp
plot(roc_crp,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_crp,"best")$threshold,roc_crp$auc,wilcox.test(data_crp$`C reactive protein, mg/L`~data_crp$Death, data=data_crp)$p.value),col='blue')
coords(roc_crp,"best")
wilcox.test(data_crp$`C reactive protein, mg/L`~data_crp$Death, data=data_crp)


#Thrombocytes
data_thrombo<-data[complete.cases(data$`Preop trombocites`),]
roc_thrombo<-roc(data_thrombo$Death,data_thrombo$`Preop trombocites`)
roc_thrombo
plot(roc_thrombo,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_thrombo,"best")$threshold,roc_thrombo$auc,wilcox.test(data_thrombo$`Preop trombocites`~data_thrombo$Death, data=data_thrombo)$p.value),col='blue')
coords(roc_thrombo,"best")
wilcox.test(data_thrombo$`Preop trombocites`~data_thrombo$Death, data=data_thrombo)

#LVEF
data_lvef<-data[complete.cases(data$`Left ventricle ejection fraction, %`),]
roc_lvef<-roc(data_lvef$Death,data_lvef$`Left ventricle ejection fraction, %`)
roc_lvef
plot(roc_lvef,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_lvef,"best")$threshold,roc_lvef$auc,wilcox.test(data_lvef$`Left ventricle ejection fraction, %`~data_lvef$Death, data=data_lvef)$p.value),col='blue')
coords(roc_lvef,"best")
wilcox.test(data_lvef$`Left ventricle ejection fraction, %`~data_lvef$Death, data=data_lvef)

#ICU time
data_icu<-data[complete.cases(data$`Intansive care unit stay, days`),]
roc_icu<-roc(data_icu$Death,data_icu$`Intansive care unit stay, days`)
roc_icu
plot(roc_icu,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_icu,"best")$threshold,roc_icu$auc,wilcox.test(data_icu$`Intansive care unit stay, days`~data_icu$Death, data=data_icu)$p.value),col='blue')
coords(roc_icu,"best")
wilcox.test(data_icu$`Intansive care unit stay, days`~data_icu$Death, data=data_icu)

data_cad <- subset(data, data$`Primary pathology, 1_coronary_artery_disease 2_heart_valve_disease` == 1)
data_hvd <- subset(data, data$`Primary pathology, 1_coronary_artery_disease 2_heart_valve_disease` == 2)

#HVD cohort OS

#Age
data_hvd_age<-data_hvd[complete.cases(data_hvd$`Age, years`),]
roc_hvd_age<-roc(data_hvd_age$Death,data_hvd_age$`Age, years`)
roc_hvd_age
plot(roc_hvd_age,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_hvd_age,"best")$threshold,roc_hvd_age$auc,wilcox.test(data_hvd_age$`Age, years`~data_hvd_age$Death, data=data_hvd_age)$p.value),col='blue')
coords(roc_hvd_age,"best")
wilcox.test(data_hvd_age$`Age, years`~data_hvd_age$Death, data=data_hvd_age)

#Albumin
data_hvd_alb<-data_hvd[complete.cases(data_hvd$`Preop albumine, g/l`),]
roc_hvd_alb<-roc(data_hvd_alb$Death,data_hvd_alb$`Preop albumine, g/l`)
roc_hvd_alb
plot(roc_hvd_alb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_hvd_alb,"best")$threshold,roc_hvd_alb$auc,wilcox.test(data_hvd_alb$`Preop albumine, g/l`~data_hvd_alb$Death, data=data_hvd_alb)$p.value),col='blue')
coords(roc_hvd_alb,"best")
wilcox.test(data_hvd_alb$`Preop albumine, g/l`~data_hvd_alb$Death, data=data_hvd_alb)

#CPB
data_hvd_cpb<-data_hvd[complete.cases(data_hvd$`Cardiopulmonary bypass time, min`),]
roc_hvd_cpb<-roc(data_hvd_cpb$Death,data_hvd_cpb$`Cardiopulmonary bypass time, min`)
roc_hvd_cpb
plot(roc_hvd_cpb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_hvd_cpb,"best")$threshold,roc_hvd_cpb$auc,wilcox.test(data_hvd_cpb$`Cardiopulmonary bypass time, min`~data_hvd_cpb$Death, data=data_hvd_cpb)$p.value),col='blue')
coords(roc_hvd_cpb,"best")
wilcox.test(data_hvd_cpb$`Cardiopulmonary bypass time, min`~data_hvd_cpb$Death, data=data_hvd_cpb)

#ACC
data_hvd_acc<-data_hvd[complete.cases(data_hvd$`Aortic cross clamp time, min`),]
roc_hvd_acc<-roc(data_hvd_acc$Death,data_hvd_acc$`Aortic cross clamp time, min`)
roc_hvd_acc
plot(roc_hvd_acc,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_hvd_acc,"best")$threshold,roc_hvd_acc$auc,wilcox.test(data_hvd_acc$`Aortic cross clamp time, min`~data_hvd_acc$Death, data=data_hvd_acc)$p.value),col='blue')
coords(roc_hvd_acc,"best")
wilcox.test(data_hvd_acc$`Aortic cross clamp time, min`~data_hvd_acc$Death, data=data_hvd_acc)

#CRP
data_hvd_crp<-data_hvd[complete.cases(data_hvd$`C reactive protein, mg/L`),]
roc_hvd_crp<-roc(data_hvd_crp$Death,data_hvd_crp$`C reactive protein, mg/L`)
roc_hvd_crp
plot(roc_hvd_crp,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_hvd_crp,"best")$threshold,roc_hvd_crp$auc,wilcox.test(data_hvd_crp$`C reactive protein, mg/L`~data_hvd_crp$Death, data=data_hvd_crp)$p.value),col='blue')
coords(roc_hvd_crp,"best")
wilcox.test(data_hvd_crp$`C reactive protein, mg/L`~data_hvd_crp$Death, data=data_hvd_crp)


#Thrombocytes
data_hvd_thrombo<-data_hvd[complete.cases(data_hvd$`Preop trombocites`),]
roc_hvd_thrombo<-roc(data_hvd_thrombo$Death,data_hvd_thrombo$`Preop trombocites`)
roc_hvd_thrombo
plot(roc_hvd_thrombo,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_hvd_thrombo,"best")$threshold,roc_hvd_thrombo$auc,wilcox.test(data_hvd_thrombo$`Preop trombocites`~data_hvd_thrombo$Death, data=data_hvd_thrombo)$p.value),col='blue')
coords(roc_hvd_thrombo,"best")
wilcox.test(data_hvd_thrombo$`Preop trombocites`~data_hvd_thrombo$Death, data=data_hvd_thrombo)

#LVEF
data_hvd_lvef<-data_hvd[complete.cases(data_hvd$`Left ventricle ejection fraction, %`),]
roc_hvd_lvef<-roc(data_hvd_lvef$Death,data_hvd_lvef$`Left ventricle ejection fraction, %`)
roc_hvd_lvef
plot(roc_hvd_lvef,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_hvd_lvef,"best")$threshold,roc_hvd_lvef$auc,wilcox.test(data_hvd_lvef$`Left ventricle ejection fraction, %`~data_hvd_lvef$Death, data=data_hvd_lvef)$p.value),col='blue')
coords(roc_hvd_lvef,"best")
wilcox.test(data_hvd_lvef$`Left ventricle ejection fraction, %`~data_hvd_lvef$Death, data=data_hvd_lvef)

#ICU time
data_hvd_icu<-data_hvd[complete.cases(data_hvd$`Intansive care unit stay, days`),]
roc_hvd_icu<-roc(data_hvd_icu$Death,data_hvd_icu$`Intansive care unit stay, days`)
roc_hvd_icu
plot(roc_hvd_icu,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_hvd_icu,"best")$threshold,roc_hvd_icu$auc,wilcox.test(data_hvd_icu$`Intansive care unit stay, days`~data_hvd_icu$Death, data=data_hvd_icu)$p.value),col='blue')
coords(roc_hvd_icu,"best")
wilcox.test(data_hvd_icu$`Intansive care unit stay, days`~data_hvd_icu$Death, data=data_hvd_icu)


#CAD cohort OS

#Age
data_cad_age<-data_cad[complete.cases(data_cad$`Age, years`),]
roc_cad_age<-roc(data_cad_age$Death,data_cad_age$`Age, years`)
roc_cad_age
plot(roc_cad_age,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_cad_age,"best")$threshold,roc_cad_age$auc,wilcox.test(data_cad_age$`Age, years`~data_cad_age$Death, data=data_cad_age)$p.value),col='blue')
coords(roc_cad_age,"best")
wilcox.test(data_cad_age$`Age, years`~data_cad_age$Death, data=data_cad_age)

#Albumin
data_cad_alb<-data_cad[complete.cases(data_cad$`Preop albumine, g/l`),]
roc_cad_alb<-roc(data_cad_alb$Death,data_cad_alb$`Preop albumine, g/l`)
roc_cad_alb
plot(roc_cad_alb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_cad_alb,"best")$threshold,roc_cad_alb$auc,wilcox.test(data_cad_alb$`Preop albumine, g/l`~data_cad_alb$Death, data=data_cad_alb)$p.value),col='blue')
coords(roc_cad_alb,"best")
wilcox.test(data_cad_alb$`Preop albumine, g/l`~data_cad_alb$Death, data=data_cad_alb)

#CPB
data_cad_cpb<-data_cad[complete.cases(data_cad$`Cardiopulmonary bypass time, min`),]
roc_cad_cpb<-roc(data_cad_cpb$Death,data_cad_cpb$`Cardiopulmonary bypass time, min`)
roc_cad_cpb
plot(roc_cad_cpb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_cad_cpb,"best")$threshold,roc_cad_cpb$auc,wilcox.test(data_cad_cpb$`Cardiopulmonary bypass time, min`~data_cad_cpb$Death, data=data_cad_cpb)$p.value),col='blue')
coords(roc_cad_cpb,"best")
wilcox.test(data_cad_cpb$`Cardiopulmonary bypass time, min`~data_cad_cpb$Death, data=data_cad_cpb)

#ACC
data_cad_acc<-data_cad[complete.cases(data_cad$`Aortic cross clamp time, min`),]
roc_cad_acc<-roc(data_cad_acc$Death,data_cad_acc$`Aortic cross clamp time, min`)
roc_cad_acc
plot(roc_cad_acc,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_cad_acc,"best")$threshold,roc_cad_acc$auc,wilcox.test(data_cad_acc$`Aortic cross clamp time, min`~data_cad_acc$Death, data=data_cad_acc)$p.value),col='blue')
coords(roc_cad_acc,"best")
wilcox.test(data_cad_acc$`Aortic cross clamp time, min`~data_cad_acc$Death, data=data_cad_acc)

#CRP
data_cad_crp<-data_cad[complete.cases(data_cad$`C reactive protein, mg/L`),]
roc_cad_crp<-roc(data_cad_crp$Death,data_cad_crp$`C reactive protein, mg/L`)
roc_cad_crp
plot(roc_cad_crp,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_cad_crp,"best")$threshold,roc_cad_crp$auc,wilcox.test(data_cad_crp$`C reactive protein, mg/L`~data_cad_crp$Death, data=data_cad_crp)$p.value),col='blue')
coords(roc_cad_crp,"best")
wilcox.test(data_cad_crp$`C reactive protein, mg/L`~data_cad_crp$Death, data=data_cad_crp)

#Thrombocytes
data_cad_thrombo<-data_cad[complete.cases(data_cad$`Preop trombocites`),]
roc_cad_thrombo<-roc(data_cad_thrombo$Death,data_cad_thrombo$`Preop trombocites`)
roc_cad_thrombo
plot(roc_cad_thrombo,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_cad_thrombo,"best")$threshold,roc_cad_thrombo$auc,wilcox.test(data_cad_thrombo$`Preop trombocites`~data_cad_thrombo$Death, data=data_cad_thrombo)$p.value),col='blue')
coords(roc_cad_thrombo,"best")
wilcox.test(data_cad_thrombo$`Preop trombocites`~data_cad_thrombo$Death, data=data_cad_thrombo)

#LVEF
data_cad_lvef<-data_cad[complete.cases(data_cad$`Left ventricle ejection fraction, %`),]
roc_cad_lvef<-roc(data_cad_lvef$Death,data_cad_lvef$`Left ventricle ejection fraction, %`)
roc_cad_lvef
plot(roc_cad_lvef,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_cad_lvef,"best")$threshold,roc_cad_lvef$auc,wilcox.test(data_cad_lvef$`Left ventricle ejection fraction, %`~data_cad_lvef$Death, data=data_cad_lvef)$p.value),col='blue')
coords(roc_cad_lvef,"best")
wilcox.test(data_cad_lvef$`Left ventricle ejection fraction, %`~data_cad_lvef$Death, data=data_cad_lvef)

#ICU time
data_cad_icu<-data_cad[complete.cases(data_cad$`Intansive care unit stay, days`),]
roc_cad_icu<-roc(data_cad_icu$Death,data_cad_icu$`Intansive care unit stay, days`)
roc_cad_icu
plot(roc_cad_icu,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_cad_icu,"best")$threshold,roc_cad_icu$auc,wilcox.test(data_cad_icu$`Intansive care unit stay, days`~data_cad_icu$Death, data=data_cad_icu)$p.value),col='blue')
coords(roc_cad_icu,"best")
wilcox.test(data_cad_icu$`Intansive care unit stay, days`~data_cad_icu$Death, data=data_cad_icu)

#8 year calculation
data$`Survival, days`[data$`Survival, days`== 'lost'] <- NA
data$`Survival, days` <- as.numeric(data$`Survival, days`)
data['8d'] = rep(0, 1187)
data$`8d` = data$Death
data$`8d`[data$`Survival, days` > 8*365] <- 0

#Mixed cohort 8yr

#Age
data_age<-data[complete.cases(data$`Age, years`),]
roc_8_age<-roc(data_age$`8d`,data_age$`Age, years`)
roc_8_age
plot(roc_8_age,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8_age,"best")$threshold,roc_8_age$auc,wilcox.test(data_age$`Age, years`~data_age$`8d`, data=data_age)$p.value),col='blue')
coords(roc_8_age,"best")
wilcox.test(data_age$`Age, years`~data_age$`8d`, data=data_age)

#Albumin
data_alb<-data[complete.cases(data$`Preop albumine, g/l`),]
roc_8_alb<-roc(data_alb$`8d`,data_alb$`Preop albumine, g/l`)
roc_8_alb
plot(roc_8_alb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8_alb,"best")$threshold,roc_8_alb$auc,wilcox.test(data_alb$`Preop albumine, g/l`~data_alb$`8d`, data=data_alb)$p.value),col='blue')
coords(roc_8_alb,"best")
wilcox.test(data_alb$`Preop albumine, g/l`~data_alb$`8d`, data=data_alb)

#CPB
data_cpb<-data[complete.cases(data$`Cardiopulmonary bypass time, min`),]
roc_8_cpb<-roc(data_cpb$`8d`,data_cpb$`Cardiopulmonary bypass time, min`)
roc_8_cpb
plot(roc_8_cpb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8_cpb,"best")$threshold,roc_8_cpb$auc,wilcox.test(data_cpb$`Cardiopulmonary bypass time, min`~data_cpb$`8d`, data=data_cpb)$p.value),col='blue')
coords(roc_8_cpb,"best")
wilcox.test(data_cpb$`Cardiopulmonary bypass time, min`~data_cpb$`8d`, data=data_cpb)


#ACC
data_acc<-data[complete.cases(data$`Aortic cross clamp time, min`),]
roc_8_acc<-roc(data_acc$`8d`,data_acc$`Aortic cross clamp time, min`)
roc_8_acc
plot(roc_8_acc,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8_acc,"best")$threshold,roc_8_acc$auc,wilcox.test(data_acc$`Aortic cross clamp time, min`~data_acc$`8d`, data=data_acc)$p.value),col='blue')
coords(roc_8_acc,"best")
wilcox.test(data_acc$`Aortic cross clamp time, min`~data_acc$`8d`, data=data_acc)

#CRP
data_crp<-data[complete.cases(data$`C reactive protein, mg/L`),]
roc_8_crp<-roc(data_crp$`8d`,data_crp$`C reactive protein, mg/L`)
roc_8_crp
plot(roc_8_crp,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8_crp,"best")$threshold,roc_8_crp$auc,wilcox.test(data_crp$`C reactive protein, mg/L`~data_crp$`8d`, data=data_crp)$p.value),col='blue')
coords(roc_8_crp,"best")
wilcox.test(data_crp$`C reactive protein, mg/L`~data_crp$`8d`, data=data_crp)


#Thrombocytes
data_thrombo<-data[complete.cases(data$`Preop trombocites`),]
roc_8_thrombo<-roc(data_thrombo$`8d`,data_thrombo$`Preop trombocites`)
roc_8_thrombo
plot(roc_8_thrombo,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8_thrombo,"best")$threshold,roc_8_thrombo$auc,wilcox.test(data_thrombo$`Preop trombocites`~data_thrombo$`8d`, data=data_thrombo)$p.value),col='blue')
coords(roc_8_thrombo,"best")
wilcox.test(data_thrombo$`Preop trombocites`~data_thrombo$`8d`, data=data_thrombo)

#LVEF
data_lvef<-data[complete.cases(data$`Left ventricle ejection fraction, %`),]
roc_8_lvef<-roc(data_lvef$`8d`,data_lvef$`Left ventricle ejection fraction, %`)
roc_8_lvef
plot(roc_8_lvef,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8_lvef,"best")$threshold,roc_8_lvef$auc,wilcox.test(data_lvef$`Left ventricle ejection fraction, %`~data_lvef$`8d`, data=data_lvef)$p.value),col='blue')
coords(roc_8_lvef,"best")
wilcox.test(data_lvef$`Left ventricle ejection fraction, %`~data_lvef$`8d`, data=data_lvef)

#ICU time
data_icu<-data[complete.cases(data$`Intansive care unit stay, days`),]
roc_8_icu<-roc(data_icu$`8d`,data_icu$`Intansive care unit stay, days`)
roc_8_icu
plot(roc_8_icu,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8_icu,"best")$threshold,roc_8_icu$auc,wilcox.test(data_icu$`Intansive care unit stay, days`~data_icu$Death, data=data_icu)$p.value),col='blue')
coords(roc_8_icu,"best")
wilcox.test(data_icu$`Intansive care unit stay, days`~data_icu$Death, data=data_icu)

data_cad <- subset(data, data$`Primary pathology, 1_coronary_artery_disease 2_heart_valve_disease` == 1)
data_hvd <- subset(data, data$`Primary pathology, 1_coronary_artery_disease 2_heart_valve_disease` == 2)


#HVD cohort 8yr

#Age
data_hvd_age<-data_hvd[complete.cases(data_hvd$`Age, years`),]
roc_8hvd_age<-roc(data_hvd_age$`8d`,data_hvd_age$`Age, years`)
roc_8hvd_age
plot(roc_8hvd_age,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8hvd_age,"best")$threshold,roc_8hvd_age$auc,wilcox.test(data_hvd_age$`Age, years`~data_hvd_age$`8d`, data=data_hvd_age)$p.value),col='blue')
coords(roc_8hvd_age,"best")
wilcox.test(data_hvd_age$`Age, years`~data_hvd_age$`8d`, data=data_hvd_age)

#Albumin
data_hvd_alb<-data_hvd[complete.cases(data_hvd$`Preop albumine, g/l`),]
roc_8hvd_alb<-roc(data_hvd_alb$`8d`,data_hvd_alb$`Preop albumine, g/l`)
roc_8hvd_alb
plot(roc_8hvd_alb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8hvd_alb,"best")$threshold,roc_8hvd_alb$auc,wilcox.test(data_hvd_alb$`Preop albumine, g/l`~data_hvd_alb$`8d`, data=data_hvd_alb)$p.value),col='blue')
coords(roc_8hvd_alb,"best")
wilcox.test(data_hvd_alb$`Preop albumine, g/l`~data_hvd_alb$`8d`, data=data_hvd_alb)

#CPB
data_hvd_cpb<-data_hvd[complete.cases(data_hvd$`Cardiopulmonary bypass time, min`),]
roc_8hvd_cpb<-roc(data_hvd_cpb$`8d`,data_hvd_cpb$`Cardiopulmonary bypass time, min`)
roc_8hvd_cpb
plot(roc_8hvd_cpb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8hvd_cpb,"best")$threshold,roc_8hvd_cpb$auc,wilcox.test(data_hvd_cpb$`Cardiopulmonary bypass time, min`~data_hvd_cpb$`8d`, data=data_hvd_cpb)$p.value),col='blue')
coords(roc_8hvd_cpb,"best")
wilcox.test(data_hvd_cpb$`Cardiopulmonary bypass time, min`~data_hvd_cpb$`8d`, data=data_hvd_cpb)

#ACC
data_hvd_acc<-data_hvd[complete.cases(data_hvd$`Aortic cross clamp time, min`),]
roc_8hvd_acc<-roc(data_hvd_acc$`8d`,data_hvd_acc$`Aortic cross clamp time, min`)
roc_8hvd_acc
plot(roc_8hvd_acc,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8hvd_acc,"best")$threshold,roc_8hvd_acc$auc,wilcox.test(data_hvd_acc$`Aortic cross clamp time, min`~data_hvd_acc$`8d`, data=data_hvd_acc)$p.value),col='blue')
coords(roc_8hvd_acc,"best")
wilcox.test(data_hvd_acc$`Aortic cross clamp time, min`~data_hvd_acc$`8d`, data=data_hvd_acc)

#CRP
data_hvd_crp<-data_hvd[complete.cases(data_hvd$`C reactive protein, mg/L`),]
roc_8hvd_crp<-roc(data_hvd_crp$`8d`,data_hvd_crp$`C reactive protein, mg/L`)
roc_8hvd_crp
plot(roc_8hvd_crp,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8hvd_crp,"best")$threshold,roc_8hvd_crp$auc,wilcox.test(data_hvd_crp$`C reactive protein, mg/L`~data_hvd_crp$`8d`, data=data_hvd_crp)$p.value),col='blue')
coords(roc_8hvd_crp,"best")
wilcox.test(data_hvd_crp$`C reactive protein, mg/L`~data_hvd_crp$`8d`, data=data_hvd_crp)

#Thrombocytes
data_hvd_thrombo<-data_hvd[complete.cases(data_hvd$`Preop trombocites`),]
roc_8hvd_thrombo<-roc(data_hvd_thrombo$`8d`,data_hvd_thrombo$`Preop trombocites`)
roc_8hvd_thrombo
plot(roc_8hvd_thrombo,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8hvd_thrombo,"best")$threshold,roc_8hvd_thrombo$auc,wilcox.test(data_hvd_thrombo$`Preop trombocites`~data_hvd_thrombo$`8d`, data=data_hvd_thrombo)$p.value),col='blue')
coords(roc_8hvd_thrombo,"best")
wilcox.test(data_hvd_thrombo$`Preop trombocites`~data_hvd_thrombo$`8d`, data=data_hvd_thrombo)

#LVEF
data_hvd_lvef<-data_hvd[complete.cases(data_hvd$`Left ventricle ejection fraction, %`),]
roc_8hvd_lvef<-roc(data_hvd_lvef$`8d`,data_hvd_lvef$`Left ventricle ejection fraction, %`)
roc_8hvd_lvef
plot(roc_8hvd_lvef,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8hvd_lvef,"best")$threshold,roc_8hvd_lvef$auc,wilcox.test(data_hvd_lvef$`Left ventricle ejection fraction, %`~data_hvd_lvef$`8d`, data=data_hvd_lvef)$p.value),col='blue')
coords(roc_8hvd_lvef,"best")
wilcox.test(data_hvd_lvef$`Left ventricle ejection fraction, %`~data_hvd_lvef$`8d`, data=data_hvd_lvef)

#ICU time
data_hvd_icu<-data_hvd[complete.cases(data_hvd$`Intansive care unit stay, days`),]
roc_8hvd_icu<-roc(data_hvd_icu$`8d`,data_hvd_icu$`Intansive care unit stay, days`)
roc_8hvd_icu
plot(roc_8hvd_icu,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8hvd_icu,"best")$threshold,roc_8hvd_icu$auc,wilcox.test(data_hvd_icu$`Intansive care unit stay, days`~data_hvd_icu$Death, data=data_hvd_icu)$p.value),col='blue')
coords(roc_8hvd_icu,"best")
wilcox.test(data_hvd_icu$`Intansive care unit stay, days`~data_hvd_icu$Death, data=data_hvd_icu)


#CAD cohort 8yr

#Age
data_cad_age<-data_cad[complete.cases(data_cad$`Age, years`),]
roc_8cad_age<-roc(data_cad_age$`8d`,data_cad_age$`Age, years`)
roc_8cad_age
plot(roc_8cad_age,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8cad_age,"best")$threshold,roc_8cad_age$auc,wilcox.test(data_cad_age$`Age, years`~data_cad_age$`8d`, data=data_cad_age)$p.value),col='blue')
coords(roc_8cad_age,"best")
wilcox.test(data_cad_age$`Age, years`~data_cad_age$`8d`, data=data_cad_age)

#Albumin
data_cad_alb<-data_cad[complete.cases(data_cad$`Preop albumine, g/l`),]
roc_8cad_alb<-roc(data_cad_alb$`8d`,data_cad_alb$`Preop albumine, g/l`)
roc_8cad_alb
plot(roc_8cad_alb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8cad_alb,"best")$threshold,roc_8cad_alb$auc,wilcox.test(data_cad_alb$`Preop albumine, g/l`~data_cad_alb$`8d`, data=data_cad_alb)$p.value),col='blue')
coords(roc_8cad_alb,"best")
wilcox.test(data_cad_alb$`Preop albumine, g/l`~data_cad_alb$`8d`, data=data_cad_alb)

#CPB
data_cad_cpb<-data_cad[complete.cases(data_cad$`Cardiopulmonary bypass time, min`),]
roc_8cad_cpb<-roc(data_cad_cpb$`8d`,data_cad_cpb$`Cardiopulmonary bypass time, min`)
roc_8cad_cpb
plot(roc_8cad_cpb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8cad_cpb,"best")$threshold,roc_8cad_cpb$auc,wilcox.test(data_cad_cpb$`Cardiopulmonary bypass time, min`~data_cad_cpb$`8d`, data=data_cad_cpb)$p.value),col='blue')
coords(roc_8cad_cpb,"best")
wilcox.test(data_cad_cpb$`Cardiopulmonary bypass time, min`~data_cad_cpb$`8d`, data=data_cad_cpb)

#ACC
data_cad_acc<-data_cad[complete.cases(data_cad$`Aortic cross clamp time, min`),]
roc_8cad_acc<-roc(data_cad_acc$`8d`,data_cad_acc$`Aortic cross clamp time, min`)
roc_8cad_acc
plot(roc_8cad_acc,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8cad_acc,"best")$threshold,roc_8cad_acc$auc,wilcox.test(data_cad_acc$`Aortic cross clamp time, min`~data_cad_acc$`8d`, data=data_cad_acc)$p.value),col='blue')
coords(roc_8cad_acc,"best")
wilcox.test(data_cad_acc$`Aortic cross clamp time, min`~data_cad_acc$`8d`, data=data_cad_acc)

#CRP
data_cad_crp<-data_cad[complete.cases(data_cad$`C reactive protein, mg/L`),]
roc_8cad_crp<-roc(data_cad_crp$`8d`,data_cad_crp$`C reactive protein, mg/L`)
roc_8cad_crp
plot(roc_8cad_crp,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8cad_crp,"best")$threshold,roc_8cad_crp$auc,wilcox.test(data_cad_crp$`C reactive protein, mg/L`~data_cad_crp$`8d`, data=data_cad_crp)$p.value),col='blue')
coords(roc_8cad_crp,"best")
wilcox.test(data_cad_crp$`C reactive protein, mg/L`~data_cad_crp$`8d`, data=data_cad_crp)

#Thrombocytes
data_cad_thrombo<-data_cad[complete.cases(data_cad$`Preop trombocites`),]
roc_8cad_thrombo<-roc(data_cad_thrombo$`8d`,data_cad_thrombo$`Preop trombocites`)
roc_8cad_thrombo
plot(roc_8cad_thrombo,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8cad_thrombo,"best")$threshold,roc_8cad_thrombo$auc,wilcox.test(data_cad_thrombo$`Preop trombocites`~data_cad_thrombo$`8d`, data=data_cad_thrombo)$p.value),col='blue')
coords(roc_8cad_thrombo,"best")
wilcox.test(data_cad_thrombo$`Preop trombocites`~data_cad_thrombo$`8d`, data=data_cad_thrombo)

#LVEF
data_cad_lvef<-data_cad[complete.cases(data_cad$`Left ventricle ejection fraction, %`),]
roc_8cad_lvef<-roc(data_cad_lvef$`8d`,data_cad_lvef$`Left ventricle ejection fraction, %`)
roc_8cad_lvef
plot(roc_8cad_lvef,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8cad_lvef,"best")$threshold,roc_8cad_lvef$auc,wilcox.test(data_cad_lvef$`Left ventricle ejection fraction, %`~data_cad_lvef$`8d`, data=data_cad_lvef)$p.value),col='blue')
coords(roc_8cad_lvef,"best")
wilcox.test(data_cad_lvef$`Left ventricle ejection fraction, %`~data_cad_lvef$`8d`, data=data_cad_lvef)

#ICU time
data_cad_icu<-data_cad[complete.cases(data_cad$`Intansive care unit stay, days`),]
roc_8cad_icu<-roc(data_cad_icu$`8d`,data_cad_icu$`Intansive care unit stay, days`)
roc_8cad_icu
plot(roc_8cad_icu,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_8cad_icu,"best")$threshold,roc_8cad_icu$auc,wilcox.test(data_cad_icu$`Intansive care unit stay, days`~data_cad_icu$Death, data=data_cad_icu)$p.value),col='blue')
coords(roc_8cad_icu,"best")
wilcox.test(data_cad_icu$`Intansive care unit stay, days`~data_cad_icu$Death, data=data_cad_icu)


#3 year calculation
#data$`Survival, days`[data$`Survival, days`== 'lost'] <- NA
#data$`Survival, days` <- as.numeric(data$`Survival, days`)
data['3d'] = rep(0, 1187)
data$`3d` = data$Death
data$`3d`[data$`Survival, days` > 3*365] <- 0

#Mixed cohort 3yr

#Age
data_age<-data[complete.cases(data$`Age, years`),]
roc_3_age<-roc(data_age$`3d`,data_age$`Age, years`)
roc_3_age
plot(roc_3_age,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3_age,"best")$threshold,roc_3_age$auc,wilcox.test(data_age$`Age, years`~data_age$`3d`, data=data_age)$p.value),col='blue')
coords(roc_3_age,"best")
wilcox.test(data_age$`Age, years`~data_age$`3d`, data=data_age)

#Albumin
data_alb<-data[complete.cases(data$`Preop albumine, g/l`),]
roc_3_alb<-roc(data_alb$`3d`,data_alb$`Preop albumine, g/l`)
roc_3_alb
plot(roc_3_alb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3_alb,"best")$threshold,roc_3_alb$auc,wilcox.test(data_alb$`Preop albumine, g/l`~data_alb$`3d`, data=data_alb)$p.value),col='blue')
coords(roc_3_alb,"best")
wilcox.test(data_alb$`Preop albumine, g/l`~data_alb$`3d`, data=data_alb)

#CPB
data_cpb<-data[complete.cases(data$`Cardiopulmonary bypass time, min`),]
roc_3_cpb<-roc(data_cpb$`3d`,data_cpb$`Cardiopulmonary bypass time, min`)
roc_3_cpb
plot(roc_3_cpb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3_cpb,"best")$threshold,roc_3_cpb$auc,wilcox.test(data_cpb$`Cardiopulmonary bypass time, min`~data_cpb$`3d`, data=data_cpb)$p.value),col='blue')
coords(roc_3_cpb,"best")
wilcox.test(data_cpb$`Cardiopulmonary bypass time, min`~data_cpb$`3d`, data=data_cpb)


#ACC
data_acc<-data[complete.cases(data$`Aortic cross clamp time, min`),]
roc_3_acc<-roc(data_acc$`3d`,data_acc$`Aortic cross clamp time, min`)
roc_3_acc
plot(roc_3_acc,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3_acc,"best")$threshold,roc_3_acc$auc,wilcox.test(data_acc$`Aortic cross clamp time, min`~data_acc$`3d`, data=data_acc)$p.value),col='blue')
coords(roc_3_acc,"best")
wilcox.test(data_acc$`Aortic cross clamp time, min`~data_acc$`3d`, data=data_acc)

#CRP
data_crp<-data[complete.cases(data$`C reactive protein, mg/L`),]
roc_3_crp<-roc(data_crp$`3d`,data_crp$`C reactive protein, mg/L`)
roc_3_crp
plot(roc_3_crp,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3_crp,"best")$threshold,roc_3_crp$auc,wilcox.test(data_crp$`C reactive protein, mg/L`~data_crp$`3d`, data=data_crp)$p.value),col='blue')
coords(roc_3_crp,"best")
wilcox.test(data_crp$`C reactive protein, mg/L`~data_crp$`3d`, data=data_crp)


#Thrombocytes
data_thrombo<-data[complete.cases(data$`Preop trombocites`),]
roc_3_thrombo<-roc(data_thrombo$`3d`,data_thrombo$`Preop trombocites`)
roc_3_thrombo
plot(roc_3_thrombo,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3_thrombo,"best")$threshold,roc_3_thrombo$auc,wilcox.test(data_thrombo$`Preop trombocites`~data_thrombo$`3d`, data=data_thrombo)$p.value),col='blue')
coords(roc_3_thrombo,"best")
wilcox.test(data_thrombo$`Preop trombocites`~data_thrombo$`3d`, data=data_thrombo)

#LVEF
data_lvef<-data[complete.cases(data$`Left ventricle ejection fraction, %`),]
roc_3_lvef<-roc(data_lvef$`3d`,data_lvef$`Left ventricle ejection fraction, %`)
roc_3_lvef
plot(roc_3_lvef,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3_lvef,"best")$threshold,roc_3_lvef$auc,wilcox.test(data_lvef$`Left ventricle ejection fraction, %`~data_lvef$`3d`, data=data_lvef)$p.value),col='blue')
coords(roc_3_lvef,"best")
wilcox.test(data_lvef$`Left ventricle ejection fraction, %`~data_lvef$`3d`, data=data_lvef)

#ICU time
data_icu<-data[complete.cases(data$`Intansive care unit stay, days`),]
roc_3_icu<-roc(data_icu$`3d`,data_icu$`Intansive care unit stay, days`)
roc_3_icu
plot(roc_3_icu,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3_icu,"best")$threshold,roc_3_icu$auc,wilcox.test(data_icu$`Intansive care unit stay, days`~data_icu$Death, data=data_icu)$p.value),col='blue')
coords(roc_3_icu,"best")
wilcox.test(data_icu$`Intansive care unit stay, days`~data_icu$Death, data=data_icu)

data_cad <- subset(data, data$`Primary pathology, 1_coronary_artery_disease 2_heart_valve_disease` == 1)
data_hvd <- subset(data, data$`Primary pathology, 1_coronary_artery_disease 2_heart_valve_disease` == 2)

#HVD cohort 3yr

#Age
data_hvd_age<-data_hvd[complete.cases(data_hvd$`Age, years`),]
roc_3hvd_age<-roc(data_hvd_age$`3d`,data_hvd_age$`Age, years`)
roc_3hvd_age
plot(roc_3hvd_age,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3hvd_age,"best")$threshold,roc_3hvd_age$auc,wilcox.test(data_hvd_age$`Age, years`~data_hvd_age$`3d`, data=data_hvd_age)$p.value),col='blue')
coords(roc_3hvd_age,"best")
wilcox.test(data_hvd_age$`Age, years`~data_hvd_age$`3d`, data=data_hvd_age)

#Albumin
data_hvd_alb<-data_hvd[complete.cases(data_hvd$`Preop albumine, g/l`),]
roc_3hvd_alb<-roc(data_hvd_alb$`3d`,data_hvd_alb$`Preop albumine, g/l`)
roc_3hvd_alb
plot(roc_3hvd_alb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3hvd_alb,"best")$threshold,roc_3hvd_alb$auc,wilcox.test(data_hvd_alb$`Preop albumine, g/l`~data_hvd_alb$`3d`, data=data_hvd_alb)$p.value),col='blue')
coords(roc_3hvd_alb,"best")
wilcox.test(data_hvd_alb$`Preop albumine, g/l`~data_hvd_alb$`3d`, data=data_hvd_alb)

#CPB
data_hvd_cpb<-data_hvd[complete.cases(data_hvd$`Cardiopulmonary bypass time, min`),]
roc_3hvd_cpb<-roc(data_hvd_cpb$`3d`,data_hvd_cpb$`Cardiopulmonary bypass time, min`)
roc_3hvd_cpb
plot(roc_3hvd_cpb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3hvd_cpb,"best")$threshold,roc_3hvd_cpb$auc,wilcox.test(data_hvd_cpb$`Cardiopulmonary bypass time, min`~data_hvd_cpb$`3d`, data=data_hvd_cpb)$p.value),col='blue')
coords(roc_3hvd_cpb,"best")
wilcox.test(data_hvd_cpb$`Cardiopulmonary bypass time, min`~data_hvd_cpb$`3d`, data=data_hvd_cpb)

#ACC
data_hvd_acc<-data_hvd[complete.cases(data_hvd$`Aortic cross clamp time, min`),]
roc_3hvd_acc<-roc(data_hvd_acc$`3d`,data_hvd_acc$`Aortic cross clamp time, min`)
roc_3hvd_acc
plot(roc_3hvd_acc,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3hvd_acc,"best")$threshold,roc_3hvd_acc$auc,wilcox.test(data_hvd_acc$`Aortic cross clamp time, min`~data_hvd_acc$`3d`, data=data_hvd_acc)$p.value),col='blue')
coords(roc_3hvd_acc,"best")
wilcox.test(data_hvd_acc$`Aortic cross clamp time, min`~data_hvd_acc$`3d`, data=data_hvd_acc)

#CRP
data_hvd_crp<-data_hvd[complete.cases(data_hvd$`C reactive protein, mg/L`),]
roc_3hvd_crp<-roc(data_hvd_crp$`3d`,data_hvd_crp$`C reactive protein, mg/L`)
roc_3hvd_crp
plot(roc_3hvd_crp,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3hvd_crp,"best")$threshold,roc_3hvd_crp$auc,wilcox.test(data_hvd_crp$`C reactive protein, mg/L`~data_hvd_crp$`3d`, data=data_hvd_crp)$p.value),col='blue')
coords(roc_3hvd_crp,"best")
wilcox.test(data_hvd_crp$`C reactive protein, mg/L`~data_hvd_crp$`3d`, data=data_hvd_crp)

#Thrombocytes
data_hvd_thrombo<-data_hvd[complete.cases(data_hvd$`Preop trombocites`),]
roc_3hvd_thrombo<-roc(data_hvd_thrombo$`3d`,data_hvd_thrombo$`Preop trombocites`)
roc_3hvd_thrombo
plot(roc_3hvd_thrombo,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3hvd_thrombo,"best")$threshold,roc_3hvd_thrombo$auc,wilcox.test(data_hvd_thrombo$`Preop trombocites`~data_hvd_thrombo$`3d`, data=data_hvd_thrombo)$p.value),col='blue')
coords(roc_3hvd_thrombo,"best")
wilcox.test(data_hvd_thrombo$`Preop trombocites`~data_hvd_thrombo$`3d`, data=data_hvd_thrombo)

#LVEF
data_hvd_lvef<-data_hvd[complete.cases(data_hvd$`Left ventricle ejection fraction, %`),]
roc_3hvd_lvef<-roc(data_hvd_lvef$`3d`,data_hvd_lvef$`Left ventricle ejection fraction, %`)
roc_3hvd_lvef
plot(roc_3hvd_lvef,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3hvd_lvef,"best")$threshold,roc_3hvd_lvef$auc,wilcox.test(data_hvd_lvef$`Left ventricle ejection fraction, %`~data_hvd_lvef$`3d`, data=data_hvd_lvef)$p.value),col='blue')
coords(roc_3hvd_lvef,"best")
wilcox.test(data_hvd_lvef$`Left ventricle ejection fraction, %`~data_hvd_lvef$`3d`, data=data_hvd_lvef)

#ICU time
data_hvd_icu<-data_hvd[complete.cases(data_hvd$`Intansive care unit stay, days`),]
roc_3hvd_icu<-roc(data_hvd_icu$`3d`,data_hvd_icu$`Intansive care unit stay, days`)
roc_3hvd_icu
plot(roc_3hvd_icu,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3hvd_icu,"best")$threshold,roc_3hvd_icu$auc,wilcox.test(data_hvd_icu$`Intansive care unit stay, days`~data_hvd_icu$Death, data=data_hvd_icu)$p.value),col='blue')
coords(roc_3hvd_icu,"best")
wilcox.test(data_hvd_icu$`Intansive care unit stay, days`~data_hvd_icu$Death, data=data_hvd_icu)

#CAD cohort 3yr

#Age
data_cad_age<-data_cad[complete.cases(data_cad$`Age, years`),]
roc_3cad_age<-roc(data_cad_age$`3d`,data_cad_age$`Age, years`)
roc_3cad_age
plot(roc_3cad_age,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3cad_age,"best")$threshold,roc_3cad_age$auc,wilcox.test(data_cad_age$`Age, years`~data_cad_age$`3d`, data=data_cad_age)$p.value),col='blue')
coords(roc_3cad_age,"best")
wilcox.test(data_cad_age$`Age, years`~data_cad_age$`3d`, data=data_cad_age)

#Albumin
data_cad_alb<-data_cad[complete.cases(data_cad$`Preop albumine, g/l`),]
roc_3cad_alb<-roc(data_cad_alb$`3d`,data_cad_alb$`Preop albumine, g/l`)
roc_3cad_alb
plot(roc_3cad_alb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3cad_alb,"best")$threshold,roc_3cad_alb$auc,wilcox.test(data_cad_alb$`Preop albumine, g/l`~data_cad_alb$`3d`, data=data_cad_alb)$p.value),col='blue')
coords(roc_3cad_alb,"best")
wilcox.test(data_cad_alb$`Preop albumine, g/l`~data_cad_alb$`3d`, data=data_cad_alb)

#CPB
data_cad_cpb<-data_cad[complete.cases(data_cad$`Cardiopulmonary bypass time, min`),]
roc_3cad_cpb<-roc(data_cad_cpb$`3d`,data_cad_cpb$`Cardiopulmonary bypass time, min`)
roc_3cad_cpb
plot(roc_3cad_cpb,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3cad_cpb,"best")$threshold,roc_3cad_cpb$auc,wilcox.test(data_cad_cpb$`Cardiopulmonary bypass time, min`~data_cad_cpb$`3d`, data=data_cad_cpb)$p.value),col='blue')
coords(roc_3cad_cpb,"best")
wilcox.test(data_cad_cpb$`Cardiopulmonary bypass time, min`~data_cad_cpb$`3d`, data=data_cad_cpb)

#ACC
data_cad_acc<-data_cad[complete.cases(data_cad$`Aortic cross clamp time, min`),]
roc_3cad_acc<-roc(data_cad_acc$`3d`,data_cad_acc$`Aortic cross clamp time, min`)
roc_3cad_acc
plot(roc_3cad_acc,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3cad_acc,"best")$threshold,roc_3cad_acc$auc,wilcox.test(data_cad_acc$`Aortic cross clamp time, min`~data_cad_acc$`3d`, data=data_cad_acc)$p.value),col='blue')
coords(roc_3cad_acc,"best")
wilcox.test(data_cad_acc$`Aortic cross clamp time, min`~data_cad_acc$`3d`, data=data_cad_acc)

#CRP
data_cad_crp<-data_cad[complete.cases(data_cad$`C reactive protein, mg/L`),]
roc_3cad_crp<-roc(data_cad_crp$`3d`,data_cad_crp$`C reactive protein, mg/L`)
roc_3cad_crp
plot(roc_3cad_crp,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3cad_crp,"best")$threshold,roc_3cad_crp$auc,wilcox.test(data_cad_crp$`C reactive protein, mg/L`~data_cad_crp$`3d`, data=data_cad_crp)$p.value),col='blue')
coords(roc_3cad_crp,"best")
wilcox.test(data_cad_crp$`C reactive protein, mg/L`~data_cad_crp$`3d`, data=data_cad_crp)

#Thrombocytes
data_cad_thrombo<-data_cad[complete.cases(data_cad$`Preop trombocites`),]
roc_3cad_thrombo<-roc(data_cad_thrombo$`3d`,data_cad_thrombo$`Preop trombocites`)
roc_3cad_thrombo
plot(roc_3cad_thrombo,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3cad_thrombo,"best")$threshold,roc_3cad_thrombo$auc,wilcox.test(data_cad_thrombo$`Preop trombocites`~data_cad_thrombo$`3d`, data=data_cad_thrombo)$p.value),col='blue')
coords(roc_3cad_thrombo,"best")
wilcox.test(data_cad_thrombo$`Preop trombocites`~data_cad_thrombo$`3d`, data=data_cad_thrombo)

#LVEF
data_cad_lvef<-data_cad[complete.cases(data_cad$`Left ventricle ejection fraction, %`),]
roc_3cad_lvef<-roc(data_cad_lvef$`3d`,data_cad_lvef$`Left ventricle ejection fraction, %`)
roc_3cad_lvef
plot(roc_3cad_lvef,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3cad_lvef,"best")$threshold,roc_3cad_lvef$auc,wilcox.test(data_cad_lvef$`Left ventricle ejection fraction, %`~data_cad_lvef$`3d`, data=data_cad_lvef)$p.value),col='blue')
coords(roc_3cad_lvef,"best")
wilcox.test(data_cad_lvef$`Left ventricle ejection fraction, %`~data_cad_lvef$`3d`, data=data_cad_lvef)

#ICU time
data_cad_icu<-data_cad[complete.cases(data_cad$`Intansive care unit stay, days`),]
roc_3cad_icu<-roc(data_cad_icu$`3d`,data_cad_icu$`Intansive care unit stay, days`)
roc_3cad_icu
plot(roc_3cad_icu,col='blue',legacy.axes=TRUE,grid=TRUE,print.thres=TRUE)
text(0.3,0.1,sprintf('Cut-off >%g\nAUC:%g\np-value:%f',coords(roc_3cad_icu,"best")$threshold,roc_3cad_icu$auc,wilcox.test(data_cad_icu$`Intansive care unit stay, days`~data_cad_icu$Death, data=data_cad_icu)$p.value),col='blue')
coords(roc_3cad_icu,"best")
wilcox.test(data_cad_icu$`Intansive care unit stay, days`~data_cad_icu$Death, data=data_cad_icu)

