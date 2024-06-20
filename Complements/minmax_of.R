# Función: minmax_of
# 
# Descripción:
#   Encuentra el valor máximo en un vector dado.
# 
# Entradas:
#   balance: Vector numérico del cual se desea encontrar el valor máximo.
# 
# Salida:
#   Valor máximo encontrado en el vector 'balance'.
minmax_of<-function(balance){
  max <- 1
  for (i in balance) {
    if(i > max){
      max <- i
    }
  }
  return(max)
}