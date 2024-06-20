# Función: create_critical_paths
# 
# Descripción:
#   Calcula los caminos críticos basados en las relaciones entre los cursos.
# 
# Entradas:
#   m: Matriz de relaciones entre cursos.
#   cant_course: Cantidad de cursos.
# 
# Salida:
#   Una lista que indica los caminos críticos para cada curso.
create_critical_paths <- function(m, cant_course){
  list <- matrix(0,ncol = cant_course)
  aux <- 1
  val <- 0
  for (i in 1:cant_course) {
    for (j in 1:cant_course) {
      if(m[i,j] > 0){
        val <- val + 1
      }
    }
    if(val == 1){
      list[i] <- aux
    }
    val <- 0
  }
  while (validate_list(list)) {
    aux <- aux + 1
    l_aux <- matrix(0,ncol = cant_course)
    for (i in 1:cant_course) {
      val <- 1
      for (j in 1:cant_course) {
        if(m[i,j] > 0){
          if(i != j){
            if(list[j] == 0){
              val <- 0         
            }      
          }
        }
      }
      if(val == 1 && list[i] == 0){
        l_aux[i] <- aux
      }
    }
    for (i in 1:cant_course) {
      if(l_aux[i] > 0){
        list[i] <- l_aux[i]
      }
    }
  }
  return(list)
}