# Función: check_sol
# 
# Descripción:
#   Verifica si una solución dada cumple con las relaciones de requisitos entre cursos.
# 
# Entradas:
#   sol: Vector que representa la solución propuesta, donde cada elemento indica el período en el que se asigna un curso.
#   data: dataframe con la información de los cursos.
#   relations: dataframe que contiene las relaciones de requisitos entre cursos.
# 
# Salida:
#   Imprime un mensaje si se encuentra un problema en las relaciones de requisitos.
check_sol <- function(sol, data, relations){
  flag <- FALSE
  for (i in 1:length(relations$prerequisite)) {
    pre <- relations$prerequisite[i]
    cou <- relations$course[i]
    pos_pre <- pos_by_name(data$courses, pre)
    pos_cou <- pos_by_name(data$courses, cou)
    if(sol[pos_pre] >= sol[pos_cou]){
      print("Problema")
      print(pre)
      print(cou)
      flag <- TRUE
    }
  }
  if(flag){
    print(sol)
  }
}