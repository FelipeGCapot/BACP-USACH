# Función: calculate_credits
# 
# Descripción:
#   Calcula el total de créditos sumando los valores de la columna 'credit' de un objeto 'data'.
# 
# Entradas:
#   data: dataframe con la información de los cursos.
# 
# Salida:
#   Valor entero que representa el total de créditos.
calculate_credits<-function(data){
  val <- 0
  for (i in data$credit) {
    val <- val + i
  }
  return(val)
}