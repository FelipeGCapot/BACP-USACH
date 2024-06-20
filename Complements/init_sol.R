# Función: init_sol
# 
# Descripción:
#   Genera una solución donde se asignan posiciones válidas aleatorias a los cursos
#   siguiendo restricciones como prerequisitos y la duración maxima de la carrera.
# 
# Entradas:
#   data: dataframe con la información de los cursos.
#   paths: lista de las rutas criticas de cada curso.
#   period: duracion maxima de la carrera
#   relations: dataframe que contiene las relaciones de requisitos entre cursos.
#   alone: lista con los cursos que no se relacionan con ningun otro curso.
# 
# Salida:
#   Retorna una lista con los periodos en donde fueron asignados los cursos.
init_sol<-function(data, paths, period, relations, alone){
  
  periods <- create_sol(data, paths, period, relations, alone)
  
  return(periods)
}