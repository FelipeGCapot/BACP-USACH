# Función: create_relation
# 
# Descripción:
#   Calcula la longitud de todos los caminos más cortos desde o hacia los
#   vértices del grafo para cada uno de los vértices, representado con un 0 si no existe
#   relación u otro valor en caso de si existir
# 
# Entradas:
#   g: El grafo dado.
#   cant_course: La cantidad de cursos en el grafo.
#
# Salida:
#   Una matriz cuadrada donde cada entrada representa la longitud del camino más corto entre los vértices 
#   del grafo.
create_relation <- function(g,cant_course){
  r_in <- matrix(0,nrow = cant_course,ncol = cant_course)
  r_out <- matrix(0,nrow = cant_course,ncol = cant_course)
  r_all <- matrix(0,nrow = cant_course,ncol = cant_course)
  r_alone <- matrix(0,ncol = cant_course)
  for (i in 1:cant_course) {
    aux_in <- all_shortest_paths(
      g,
      from = i,
      mode = "in",
      weights = NULL
    )
    aux_out <- all_shortest_paths(
      g,
      from = i,
      mode = "out",
      weights = NULL
    )
    aux_all <- all_shortest_paths(
      g,
      from = i,
      mode = "all",
      weights = NULL
    )
    r_in[i,] <- aux_in$nrgeo
    r_out[i,] <- aux_out$nrgeo
    r_all[i,] <- aux_all$nrgeo
  }
  for (i in 1:cant_course) {
    count <- 0
    for (j in 1:cant_course) {
      if(r_all[i,j] > 0){
        count <- count+1
      }
    }
    if(count == 1){
      r_alone[i] <- 1
    }
  }
  relation <- list("In" = r_in, "Out" = r_out, "Alone" = r_alone)
  
  return(relation)
}