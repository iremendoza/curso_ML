library(randomForest)
library(data.table)
library(pROC)

# https://www.kaggle.com/trongnghia96/bank-analysis
# age (numeric)
# job : type of job (categorical: "admin.", "unknown", "unemployed", "management", "housemaid", "entrepreneur", "student", "blue-collar", "self-employed", "retired", "technician", "services")
# marital : marital status (categorical: "married", "divorced", "single"; note: "divorced" means divorced or widowed)
# education (categorical: "unknown", "secondary", "primary", "tertiary")
# default: has credit in default? (binary: "yes", "no")
# balance: average yearly balance, in euros (numeric)
# housing: has housing loan? (binary: "yes", "no")
# loan: has personal loan? (binary: "yes", "no")
#
# contact: contact communication type (categorical: "unknown", "telephone", "cellular")
# day: last contact day of the month (numeric)
# month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
# duration: last contact duration, in seconds (numeric)
# campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
# pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
# previous: number of contacts performed before this campaign and for this client (numeric)
# poutcome: outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")
# y - has the client subscribed a term deposit? (binary: "yes","no")
 
# Carga de datos
#


datosdefault <- read.csv("bancos_clean.csv")


# Convertimos la variable de respuesta en categórica
datosdefault$y <- as.factor(datosdefault$y)
# Eliminamos las columnas X y poutcome
drops <- c("X","poutcome")
datosdefault <- datosdefault[ , !(names(datosdefault) %in% drops)]

# Fujamos el valor de la semilla random para que la división train/test sea reproducible
set.seed(10)
# Elegimos al azar el 20% de los números de fila para crear el testing set
test.ids <- sample(1:nrow(datosdefault), nrow(datosdefault)*0.2)

# Training set
datosmodelo.train <- datosdefault[-test.ids,]
# Testing set
datosmodelo.test <- datosdefault[test.ids,]

# Entrenamos un modelo randomforest
#
modeloRF <- randomForest(x = datosmodelo.train[, !(names(datosmodelo.train) %in% c("y"))], 
                           y = datosmodelo.train$y, 
                           method = "class",
                           ntree = 100, do.trace = F)
# Predecimos sobre test
#
pred.rf <- predict(modeloRF, datosmodelo.test, type = "prob")

myrocRF <- roc(response = datosmodelo.test$y, predictor = as.numeric(pred.rf[,2]), 
             percent = T, plot = T, ci = T, smooth = F)
plot(myrocRF, col= "green", main = paste0("RF AUC: ", myrocRF$auc/100))

# Ahora hacemos lo mismo pero con regresión logística

modeloLogistica <- glm(y ~.,family=binomial(link='logit'),data=datosmodelo.train)

pred.Logistica <- predict(modeloLogistica,
                          newdata=datosmodelo.test,type='response')
fitted.results <- ifelse(pred.Logistica > 0.5,1,0)

myrocLogistica <- roc(response = datosmodelo.test$y, predictor = as.numeric(pred.Logistica), 
             percent = T, plot = T, ci = T, smooth = F)
plot(myrocLogistica, col = "red", main = paste0("Logistica AUC: ", myrocLogistica$auc/100))

# Interpretabilidad. Peso de las variables, según test ANOVA
anova(modeloLogistica, test="Chisq")