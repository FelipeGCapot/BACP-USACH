# Función: check_rel_data
# 
# Descripción:
#   Verifica si los cursos especificados en las relaciones existen en la lista de cursos.
#   Imprime los cursos que no existen como prerequisitos o cursos.
# 
# Entradas:
#   relations: dataframe que contiene las relaciones de requisitos entre cursos.
#   data: dataframe con la información de los cursos.
# 
# Salida:
#   Imprime los cursos que no existen como prerequisitos o cursos en las relaciones.
check_rel_data <- function(){
  for(i in 1:length(relations$prerequisite)){
    flag <- TRUE
    for(j in data$courses){
      if(relations$prerequisite[i] == j){
        flag <- FALSE
      }
    }
    if(flag){
      print("pre")
      print(relations$prerequisite[i])
    }
    flag <- TRUE
    for(j in data$courses){
      if(relations$course[i] == j){
        flag <- FALSE
      }
    }
    if(flag){
      print("pos")
      print(relations$course[i])
    }
  }
}