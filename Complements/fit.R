# Función: fit
# 
# Descripción:
#   Calcula el balance de créditos asignados a períodos agrupados de dos en dos.
# 
# Entradas:
#   sol: Vector que representa la solución de asignación de cursos a períodos.
#   period: Número total de períodos disponibles para asignar cursos.
#   data: dataframe con la información de los cursos.
# 
# Salida:
#   Vector que representa el balance de créditos asignados a cada par de períodos.
fit<-function(sol, period, data){
  balance_aux <- matrix(0,ncol = period)
  for (i in 1:length(sol)) {
    pos <- sol[i]
    value <- data$credit[i]
    balance_aux[pos] <- balance_aux[pos] + value
  }
  if (period%%2 == 1){
    balance_aux <- cbind(balance_aux, balance_aux[period])
    period <- period + 1
  }
  balance <- matrix(0,ncol = (period/2))
  count <- 1
  for (i in 1:period) {
    balance[count] <- balance[count] + balance_aux[i]
    if (i%%2 == 0){
      count <- count + 1
    }
  }
  return(balance)
}