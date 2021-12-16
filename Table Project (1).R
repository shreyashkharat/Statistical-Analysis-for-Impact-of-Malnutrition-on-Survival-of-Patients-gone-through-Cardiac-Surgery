require('readxl')
data <- read_xlsx("F:/Semester 3/Stat 1/Project/data.xlsx")

data_mal<-subset(data, data$MUST>0)
data_no_mal<-subset(data, data$MUST==0)
nrow(data_mal)
nrow(data_no_mal)

#age,yr
mean(data$`Age, years`)
sd(data$`Age, years`)

mean(data_mal$`Age, years`)
sd(data_mal$`Age, years`)

mean(data_no_mal$`Age, years`)
sd(data_no_mal$`Age, years`)

#age>= 65,yr
data65yr<-subset(data, data$`Age, years`>=65)
nrow(data65yr)
100*nrow(data65yr)/nrow(data)

data65yrmal<-subset(data_mal, data_mal$`Age, years`>=65)
nrow(data65yrmal)
100*nrow(data65yrmal)/nrow(data_mal)

data65yrnomal<-subset(data_no_mal, data_no_mal$`Age, years`>=65)
nrow(data65yrnomal)
100*nrow(data65yrnomal)/nrow(data_no_mal)


#Female sex
female<-nrow(subset(data,data$`Gender, 1 male, 2 female`==2))
female
100*female/nrow(data)

female_mal<-nrow(subset(data_mal,data_mal$`Gender, 1 male, 2 female`==2))
female_mal
100*female_mal/nrow(data_mal)

female_no_mal<-nrow(subset(data_no_mal,data_no_mal$`Gender, 1 male, 2 female`==2))
female_no_mal
100*female_no_mal/nrow(data_no_mal)

#LVEF<35%
lvef<-nrow(subset(data,data$`Left ventricle ejection fraction, %`<35))
lvef
100*lvef/nrow(data)

lvef_mal<-nrow(subset(data_mal,data_mal$`Left ventricle ejection fraction, %`<35))
lvef_mal
100*lvef_mal/nrow(data_mal)

lvef_no_mal<-nrow(subset(data_no_mal,data_no_mal$`Left ventricle ejection fraction, %`<35))
lvef_no_mal
100*lvef_no_mal/nrow(data_no_mal)


#NYHA class
#0
nyha0<-nrow(subset(data,data$`NYHA class`==0))
nyha0
100*nyha0/nrow(data)

nyha0_mal<-nrow(subset(data_mal,data_mal$`NYHA class`==0))
nyha0_mal
100*nyha0_mal/nrow(data_mal)

nyha0_no_mal<-nrow(subset(data_no_mal,data_no_mal$`NYHA class`==0))
nyha0_no_mal
100*nyha0_no_mal/nrow(data_no_mal)

#I
nyhaI<-nrow(subset(data,data$`NYHA class`==1))
nyhaI
100*nyhaI/nrow(data)

nyhaI_mal<-nrow(subset(data_mal,data_mal$`NYHA class`==1))
nyhaI_mal
100*nyhaI_mal/nrow(data_mal)

nyhaI_no_mal<-nrow(subset(data_no_mal,data_no_mal$`NYHA class`==1))
nyhaI_no_mal
100*nyhaI_no_mal/nrow(data_no_mal)

#II
nyhaII<-nrow(subset(data,data$`NYHA class`==2))
nyhaII
100*nyhaII/nrow(data)

nyhaII_mal<-nrow(subset(data_mal,data_mal$`NYHA class`==2))
nyhaII_mal
100*nyhaII_mal/nrow(data_mal)

nyhaII_no_mal<-nrow(subset(data_no_mal,data_no_mal$`NYHA class`==2))
nyhaII_no_mal
100*nyhaII_no_mal/nrow(data_no_mal)

#III
nyhaIII<-nrow(subset(data,data$`NYHA class`==3))
nyhaIII
100*nyhaIII/nrow(data)

nyhaIII_mal<-nrow(subset(data_mal,data_mal$`NYHA class`==3))
nyhaIII_mal
100*nyhaIII_mal/nrow(data_mal)

nyhaIII_no_mal<-nrow(subset(data_no_mal,data_no_mal$`NYHA class`==3))
nyhaIII_no_mal
100*nyhaIII_no_mal/nrow(data_no_mal)

#IV
nyhaIV<-nrow(subset(data,data$`NYHA class`==4))
nyhaIV
100*nyhaIV/nrow(data)

nyhaIV_mal<-nrow(subset(data_mal,data_mal$`NYHA class`==4))
nyhaIV_mal
100*nyhaIV_mal/nrow(data_mal)

nyhaIV_no_mal<-nrow(subset(data_no_mal,data_no_mal$`NYHA class`==4))
nyhaIV_no_mal
100*nyhaIV_no_mal/nrow(data_no_mal)

#Logistic Euroscore
data_logi<-data[complete.cases(data$`Logistic Euroscore`),]
mean(data_logi$`Logistic Euroscore`)
sd(data_logi$`Logistic Euroscore`)

data_logi_mal<-subset(data_logi, data_logi$MUST>0)
data_logi_no_mal<-subset(data_logi, data_logi$MUST==0)
mean(data_logi_mal$`Logistic Euroscore`)
sd(data_logi_mal$`Logistic Euroscore`)

mean(data_logi_no_mal$`Logistic Euroscore`)
sd(data_logi_no_mal$`Logistic Euroscore`)

#primary diagnosis
#CAD
datacad<-subset(data, data$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==1)
nrow(datacad)
100*nrow(datacad)/nrow(data)

datacad_mal<-subset(data_mal, data_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==1)
nrow(datacad_mal)
100*nrow(datacad_mal)/nrow(data_mal)

datacad_no_mal<-subset(data_no_mal, data_no_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==1)
nrow(datacad_no_mal)
100*nrow(datacad_no_mal)/nrow(data_no_mal)

#mitral stenosis
datamit_ste<-subset(data, data$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==2)
nrow(datamit_ste)
100*nrow(datamit_ste)/nrow(data)

datamit_ste_mal<-subset(data_mal, data_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==2)
nrow(datamit_ste_mal)
100*nrow(datamit_ste_mal)/nrow(data_mal)

datamit_ste_no_mal<-subset(data_no_mal, data_no_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==2)
nrow(datamit_ste_no_mal)
100*nrow(datamit_ste_no_mal)/nrow(data_no_mal)

#mitral insufficiency
datamit_ins<-subset(data, data$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==3)
nrow(datamit_ins)
100*nrow(datamit_ins)/nrow(data)

datamit_ins_mal<-subset(data_mal, data_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==3)
nrow(datamit_ins_mal)
100*nrow(datamit_ins_mal)/nrow(data_mal)

datamit_ins_no_mal<-subset(data_no_mal, data_no_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==3)
nrow(datamit_ins_no_mal)
100*nrow(datamit_ins_no_mal)/nrow(data_no_mal)

#aortic stenosis
dataao_ste<-subset(data, data$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==4)
nrow(dataao_ste)
100*nrow(dataao_ste)/nrow(data)

dataao_ste_mal<-subset(data_mal, data_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==4)
nrow(dataao_ste_mal)
100*nrow(dataao_ste_mal)/nrow(data_mal)

dataao_ste_no_mal<-subset(data_no_mal, data_no_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==4)
nrow(dataao_ste_no_mal)
100*nrow(dataao_ste_no_mal)/nrow(data_no_mal)

#aortic insufficiency
dataao_ins<-subset(data, data$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==5)
nrow(dataao_ins)
100*nrow(dataao_ins)/nrow(data)

dataao_ins_mal<-subset(data_mal, data_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==5)
nrow(dataao_ins_mal)
100*nrow(dataao_ins_mal)/nrow(data_mal)

dataao_ins_no_mal<-subset(data_no_mal, data_no_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==5)
nrow(dataao_ins_no_mal)
100*nrow(dataao_ins_no_mal)/nrow(data_no_mal)

#triscuspid insufficiency
datatri_ins<-subset(data, data$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==6)
nrow(datatri_ins)
100*nrow(datatri_ins)/nrow(data)

datatri_ins_mal<-subset(data_mal, data_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==6)
nrow(datatri_ins_mal)
100*nrow(datatri_ins_mal)/nrow(data_mal)

datatri_ins_no_mal<-subset(data_no_mal, data_no_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==6)
nrow(datatri_ins_no_mal)
100*nrow(datatri_ins_no_mal)/nrow(data_no_mal)

#pulmonary artery stenosis
datapul_art_ste<-subset(data, data$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==8)
nrow(datapul_art_ste)
100*nrow(datapul_art_ste)/nrow(data)

datapul_art_ste_mal<-subset(data_mal, data_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==8)
nrow(datapul_art_ste_mal)
100*nrow(datapul_art_ste_mal)/nrow(data_mal)

datapul_art_ste_no_mal<-subset(data_no_mal, data_no_mal$`Primary diagnisis, 1_Coronary_artery_disease, 2_mitral_stenosis, 3_mitral_regurgitation, 4_aortic_stenosis, 5_aortic_regurgitation, 6_tricuspid_regurgitation, 7_pathology_of_aorta, 8_pulmonary_artery`==8)
nrow(datapul_art_ste_no_mal)
100*nrow(datapul_art_ste_no_mal)/nrow(data_no_mal)

#BMI
data_bmi<-data[complete.cases(data$BMI__kg_m2),]
mean(data_bmi$BMI__kg_m2)
sd(data_bmi$BMI__kg_m2)

data_bmi_mal<-subset(data_bmi, data_bmi$MUST>0)
data_bmi_no_mal<-subset(data_bmi, data_bmi$MUST==0)
mean(data_bmi_mal$BMI__kg_m2)
sd(data_bmi_mal$BMI__kg_m2)

mean(data_bmi_no_mal$BMI__kg_m2)
sd(data_bmi_no_mal$BMI__kg_m2)

