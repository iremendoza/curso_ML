# Basado en https://blog.rstudio.com/2017/09/05/keras-for-r/

# Instalar el paquete keras
# Hay que instalar también el paquete tensorflow y dependencias
# Para ello importar keras con library(keras) e invocar
# install_keras()

library(keras)
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# Pinta el digito n de la coleccion
plot_digito <- function(coleccion,n){
  #digit <- x_train[1,,]      # select the  training image
  digit <- coleccion[n,,]      # select the 200th training image
  plot(as.raster(digit, max = 255)) # plot it!
}

plot_digito(x_train,2)


# reshape
dim(x_train) <- c(nrow(x_train), 784)
dim(x_test) <- c(nrow(x_test), 784)
# rescale (0-1)
x_train <- x_train / 255
x_test <- x_test / 255
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 128, activation = "relu", input_shape = c(784)) %>%  #capa de entrada
  layer_dropout(rate = 0.4) %>% #entrena solo un porcentaje de los pesos
  layer_dense(units = 128, activation = "relu") %>% #segunda capa
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = "softmax") #activación para una función de clasificación

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 10, batch_size = 64, #se puede cambiar por 5, para ver las funciones de
  validation_split = 0.2
)

plot(history)

model %>% evaluate(x_test, y_test,verbose = 0)

predicciones <- model %>% predict(x_test) %>% k_argmax()


print("Valores")
print(mnist$test$y[1:20])
print("Predicciones")
print(predicciones[1:20])
my.name <- readline(prompt="Puse INTRO para continuar")
plot_digito(mnist$test$x,9)