# Función: balance_of
# 
# Descripción:
#   Calcula la desviación estándar de un vector numérico.
# 
# Entradas:
#   balance: Vector numérico del cual se desea calcular la desviación estándar.
# 
# Salida:
#   Desviación estándar del vector 'balance'.
balance_of<-function(balance){
  acum <- 0
  mean <- mean(balance)
  
  for (i in balance) {
    acum <- acum + (i - mean)^2
  }
  acum <- acum/length(balance)
  acum <- sqrt(acum)
  return(acum)
}