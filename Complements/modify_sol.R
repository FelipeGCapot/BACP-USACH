# Función: modify_sol
# 
# Descripción:
#   Modifica una solución existente de asignación de cursos a períodos, reorganizando según un nivel dado y las relaciones entre cursos.
# 
# Entradas:
#   sol: Vector que representa la solución actual de asignación de cursos a períodos.
#   level: Nivel a partir del cual se reorganiza la asignación de cursos.
#   data: dataframe con la información de los cursos.
#   paths: lista de las rutas criticas de cada curso.
#   period: Número total de períodos disponibles.
#   relations: dataframe que contiene las relaciones de requisitos entre cursos.
# 
# Salida:
#   Vector que representa la solución modificada de asignación de cursos a períodos.
modify_sol<-function(sol, level, data, paths,period, relations){
  periods <- sol
  pendientes <- list()
  
  for (i in 1:length(sol)) {
    if(periods[i] <= level){
      pendientes[length(pendientes) + 1] <- data$courses[i]
      periods[i] <- 0
    }
  }
  
  if(is_empty(pendientes)){
    return(sol)
  }
  #print(pendientes)
  #print(periods)
  
  while (!(is_empty(pendientes))) {
    new_pos_course <- sample(1:length(pendientes),1,replace=F)
    new_course <- pendientes[new_pos_course]
    #crear lista#
    l_aux <- new_course
    l <- new_course
    while (!(is_empty(l))) {
      for (i in 1:length(relations$prerequisite)) {
        if(relations$prerequisite[i] == l[1]){
          aux <- relations$course[i]
          pos_aux <- pos_by_name(data$courses, aux)
          if(periods[pos_aux] == 0){
            #check <- TRUE
            for (j in l_aux) {
              if(j == aux){
                l_aux <- delete_by_pos(l_aux, pos_aux, data)
                #check <- FALSE
              }
            }
            l_aux[length(l_aux) + 1] <- aux
            l[length(l) + 1] <- aux
          }
        }
      }
      l <- l[-1]
    }
    #insertar lista#
    while (!(is_empty(l_aux))) {
      #print(pendientes)
      new_course <- l_aux[length(l_aux)]
      check <- TRUE
      pos_place <- 0
      for (i in 1:length(relations$prerequisite)) {
        if(relations$prerequisite[i] == l_aux[length(l_aux)]){
          aux <- relations$course[i]
          pos_aux <- pos_by_name(data$courses, aux)
          if(check){
            if(periods[pos_aux] > pos_place){
              pos_place <- periods[pos_aux]
            }
          } else {
            if(periods[pos_aux] < pos_place){
              pos_place <- periods[pos_aux]
            }
          }
          check <- FALSE
        }
      }
      if(check){
        pos_place <- period
      } else{
        pos_place <- pos_place - 1
      }
      # elegir posicion
      pos_course <- pos_by_name(data$courses, new_course)
      #print(pos_course)
      if(paths[pos_course] == pos_place){
        pos_period <- pos_place
      } else{
        pos_period <- sample(paths[pos_course]:pos_place,1,replace=F)
      }
      # asignar
      periods[pos_course] <- pos_period
      #count_course[pos_period] <- count_course[pos_period] + 1
      #print(pos_course)
      #print(pendientes)
      #print(periods)
      pendientes <- delete_by_pos(pendientes, pos_course, data)
      last <- length(l_aux)
      l_aux <- l_aux[-last]
    }
  }
  sol <- periods
  return(sol)
}