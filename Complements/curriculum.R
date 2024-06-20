# Función: curriculum
# 
# Descripción:
#   Calcula el balance de créditos asignados a cada período según una solución dada.
# 
# Entradas:
#   sol: Vector que representa la solución de asignación de cursos a períodos.
#   period: Número total de períodos disponibles.
#   data: dataframe con la información de los cursos.
# 
# Salida:
#   Vector que representa el balance de créditos asignados a cada período.
curriculum<-function(sol, period, data){
  balance <- matrix(0,ncol = period)
  for (i in 1:length(sol)) {
    pos <- sol[i]
    value <- data$credit[i]
    balance[pos] <- balance[pos] + value
  }
  return(balance)
}