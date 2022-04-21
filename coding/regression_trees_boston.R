# 1. Ejemplo de regresion trees utilizando el paquete tree
library(tree)
set.seed(1)

# Cargamos el paquete MASS que contiene el valor de las casas en 
# las afueras de Boston
library(MASS)

summary(Boston)

train <- sample(1:nrow(Boston), nrow(Boston)*0.7)

tree.boston <- tree(medv ~ . , Boston[train, ])

summary(tree.boston)

# Vemos que solo se han utilizado 3 variables: lstat, rm, dis
plot(tree.boston)
text(tree.boston, pretty = 0)

# La variable que m?s pesa es rm, el n?mero de dormitorios.
# La variable lstat mide el porcentaje de individuos con status 
# socio-economico bajo
# El arbol indica que el valor mas bajo de 
# lstat corresponde con las casas mas caras.
# La variable dis indica distancia al centro de la ciudad

# hagamos una prediccion
yhat <- predict(tree.boston, newdata = Boston[-train, ])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test, main = "Predicci?n con ?rbol")

# RMSE
errorcuad <- sqrt(sum((yhat - boston.test)^2)/length(boston.test))
print(paste("Error cuadratico ",errorcuad))

