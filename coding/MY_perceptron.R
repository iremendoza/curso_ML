# Clasificacion de puntos en un espacio bidimensional mediante
# un perceptron 
library(ggplot2)

unit_step <- function(x)  # Funcion de activacion
 return(x>0)
 
learning_rate <- 0.05      # Tasa de aprendizaje
n <- 200                  # Numero de iteraciones
bias <- 1                 # Si wx * x + wy *y + wbias * bias > 0 devuelve 1, si no devuelve 0
# Asigancion manunal de los puntos de entrenamiento
training_data <- data.frame("xp"=c(),"yp"=c(),"clase"=c())
training_data <- rbind(training_data,data.frame("xp"=1,"yp"=0,"clase"=0))
training_data <- rbind(training_data,data.frame("xp"=-1,"yp"=2,"clase"=0))
training_data <- rbind(training_data,data.frame("xp"=1,"yp"=3,"clase"=1))
training_data <- rbind(training_data,data.frame("xp"=2,"yp"=-0.2,"clase"=1))
training_data <- rbind(training_data,data.frame("xp"=2,"yp"=1,"clase"=1))
training_data <- rbind(training_data,data.frame("xp"=2.3,"yp"=-3,"clase"=0))
numpuntos <- nrow(training_data)
# Scatter plot de los puntos de entrenamiento. 
gpuntos <- ggplot(data=training_data,aes(x=xp,y=yp,color=as.factor(clase)))+geom_point(size=3)
print(gpuntos)

w <- runif(3,-0.1,0.1) # inicializamos al azar wx, wx y wbias
errors <- c()   # Listas auxiliares para dibujar la evolucion del error de entrenamiento
pesos <- data.frame("wx"=c(),"wy"=c(),"wbias"=c())    # y los pesos

# Entrenamiento del perceptrón
for (i in 1:n){                     # El bucle se ejecuta n veces
  puntoazar <- training_data[sample(numpuntos, 1), ]  # Se elige un punto al azar
  expected <- puntoazar$clase            # Clase deseada
  result = w[1]*puntoazar$x+w[2]*puntoazar$y+w[3]*bias 
  
  error = (expected - unit_step(result))   # error: 1 [prediccion 0/realidad 1]; 0 es acierto; -1 [pr 1/real -1]
  
  errors = c(errors,error)               # Se añade el valor del error a la lista errores para la gráfica
  if (error !=0){
    for (j in 1:2)                       # Actualizacion de los pesos si hay error
      w[j] <- w[j] + learning_rate * error * puntoazar[,j]   # En caso de error, se suma o resta en función del signo de la variable error
    w[3] <- w[3] + learning_rate * error * bias
    print(paste("puntox",puntoazar$x,"puntoy",puntoazar$y,"prediccion",result,"realidad",expected,"error",error))
    }
  pesos <- rbind(pesos,data.frame("wx"=w[1],"wy"=w[2],"wbias"=w[3]))   # se actualiza la lista de pesos para la gráfica
}

# Grafico de errores
dferrores <- data.frame("errores"=errors,paso=seq(1:n))
gerrores <- ggplot(data=dferrores,aes(x=paso,y=errores))+geom_line()
print(gerrores)

# Grafico de pesos
dfpesos <- data.frame("w"=pesos[,1],"paso"= seq(1:n),"nombre"="wx")
dfpesos <- rbind(dfpesos,data.frame("w"=pesos[,2],"paso"= seq(1:n),"nombre"="wy"))
dfpesos <- rbind(dfpesos,data.frame("w"=pesos[,3],"paso"= seq(1:n),"nombre"="wbias"))
gpesos <- ggplot(data=dfpesos,aes(x=paso,y=w,color=as.factor(nombre)))+geom_line(size=1)
print(gpesos)

# Finalmente, se superpone a la grafica de puntos la linea del perceptron
gperceptron <- gpuntos + geom_abline(intercept = -(w[3]/w[2]), slope = -(w[1]/w[2]),  
                                    color = "green")
print(gperceptron)
