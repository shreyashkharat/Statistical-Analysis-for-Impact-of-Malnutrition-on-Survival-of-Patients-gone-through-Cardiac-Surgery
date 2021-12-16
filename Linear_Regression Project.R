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

# Linear regression on albumin levels and CRP
model_abl_crp <- lm(df$`Preop albumine, g/l` ~ df$`C reactive protein, mg/L`)
summary(model_abl_crp)
cor(df$`Preop albumine, g/l`,df$`C reactive protein, mg/L`,use="complete.obs")

# Linear regression on albumin levels and MUST
model_abl_must <- lm(df$`Preop albumine, g/l` ~ df$MUST)
summary(model_abl_must)
cor(df$`Preop albumine, g/l`,df$MUST,use="complete.obs")

# Linear regression on albumin levels and NYHA
model_abl_nyha <- lm(df$`Preop albumine, g/l` ~ df$`NYHA class`)
summary(model_abl_nyha)
cor(df$`Preop albumine, g/l`,df$`NYHA class`,use="complete.obs")

df$`Logistic Euroscore`[is.na(df$`Logistic Euroscore`)] <- mean(df$`Logistic Euroscore`, na.rm = TRUE)
df$`Preop. total lymphocite count, cells/microliters`[is.na(df$`Preop. total lymphocite count, cells/microliters`)] <- mean(df$`Preop. total lymphocite count, cells/microliters`, na.rm = TRUE)
df$`Preop total protein, g/l`[is.na(df$`Preop total protein, g/l`)] <- mean(df$`Preop total protein, g/l`, na.rm = TRUE)
df$`Preop albumine, g/l`[is.na(df$`Preop albumine, g/l`)] <- mean(df$`Preop albumine, g/l`, na.rm = TRUE)
df$`Preop trombocites`[is.na(df$`Preop trombocites`)] <- mean(df$`Preop trombocites`, na.rm = TRUE)
df$`C reactive protein, mg/L`[is.na(df$`C reactive protein, mg/L`)] <- mean(df$`C reactive protein, mg/L`, na.rm = TRUE)
df$`Left ventricle ejection fraction, %`[is.na(df$`Left ventricle ejection fraction, %`)] <- mean(df$`Left ventricle ejection fraction, %`, na.rm = TRUE)
df$`Hospitalisation, days`[is.na(df$`Hospitalisation, days`)] <- mean(df$`Hospitalisation, days`, na.rm = TRUE)

# Linear regression on albumin levels and CRP
model_abl_crp <- lm(df$`Preop albumine, g/l` ~ df$`C reactive protein, mg/L`)
summary(model_abl_crp)
cor(df$`Preop albumine, g/l`,df$`C reactive protein, mg/L`)

# Linear regression on albumin levels and MUST
model_abl_must <- lm(df$`Preop albumine, g/l` ~ df$MUST)
summary(model_abl_must)
cor(df$`Preop albumine, g/l`,df$MUST)

# Linear regression on albumin levels and NYHA
model_abl_nyha <- lm(df$`Preop albumine, g/l` ~ df$`NYHA class`)
summary(model_abl_nyha)
cor(df$`Preop albumine, g/l`,df$`NYHA class`)


