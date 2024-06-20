# Función: improve_sol
# 
# Descripción:
#   Mejora la solución de asignación de cursos a períodos optimizando la distribución de carga de créditos.
# 
# Entradas:
#   sol: Vector que representa la solución actual de asignación de cursos a períodos.
#   period: Número total de períodos disponibles para asignar cursos.
#   data: Objeto que contiene información sobre los cursos, donde 'data$credit' son los créditos de cada curso.
#   alone: Vector de índices de cursos marcados como 'alone', es decir, que deben ser reubicados para optimizar la distribución de carga.
# 
# Salida:
#   Vector 'sol' actualizado con la asignación optimizada de cursos a períodos.
improve_sol<-function(sol, period, data, alone){
  carga <- curriculum(sol, period, data)
  for (i in alone) {
    carga[sol[i]] <- carga[sol[i]] - data$credit[i]
  }
  for (i in alone) {
    aux <- carga[1]
    aux_pos <- 1
    for (j in 1:period) {
      if(j%%2 == 1){
        if(carga[j] < aux){
          aux <- carga[j]
          aux_pos <- j
        }
      }
    }
    for (j in 1:period) {
      if(j%%2 == 0){
        if(carga[j] < aux){
          aux <- carga[j]
          aux_pos <- j
        }
      }
    }
    carga[aux_pos] <- carga[aux_pos] + data$credit[i]
    sol[i] <- aux_pos
  }
  return(sol)
}