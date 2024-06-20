# Función: create_alone
# 
# Descripción:
#   Calcula si existe un camino a otros vértices del grafo para cada uno de los vértices, 
#   representado con un 0 si existe alguna relación y 1 si no existe relación
# 
# Entradas:
#   data: Listado de los nombres de los cursos, sus creditos y el periodo al cual estan asignados en la vida real.
#   relation: Listado de relaciones entre los vertices del grafo.
#
# Salida:
#   Una lista donde cada entrada representa si existe reacion con alguno de los vértices 
#   del grafo, 0 si tienen alguna relacion, 1 si no tienen.
create_alone<-function(data,relation){
  pendientes <- numeric()
  count <- 0
  sum<- 0
  for (i in 1:length(relation$Alone)) {
    if(relation$Alone[i] > 0){
      sum <- sum + 1
      pendientes <- c(pendientes,i)
    }
  }
  if(sum == 0){
    return(NULL)
  } else {
    alone <- numeric()
    while (!(is_empty(pendientes))){
      count <- count +1
      aux <- 0
      aux_pos <- 0
      for(i in 1:length(pendientes)){
        if(data$credit[pendientes[i]] > aux){
          aux <- data$credit[pendientes[i]]
          aux_pos <- i
        }
      }
      alone <- c(alone, pendientes[aux_pos])
      pendientes <- pendientes[-aux_pos]
    }
  }
  return(alone)
}