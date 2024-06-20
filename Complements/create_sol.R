# Función: create_sol
# 
# Descripción:
#   Genera una solución donde se asignan posiciones válidas aleatorias a los cursos
#   siguiendo restricciones como prerequisitos y la duración maxima de la carrera.
# 
# Entradas:
#   data: dataframe con la información de los cursos.
#   paths: lista de las rutas criticas de cada curso.
#   period: Número total de períodos disponibles.
#   relations: dataframe que contiene las relaciones de requisitos entre cursos.
#   alone: lista con los cursos que no se relacionan con ningun otro curso.
# 
# Salida:
#   Retorna una lista con los periodos en donde fueron asignados los cursos.

create_sol<-function(data, paths, period, relations, alone){
  # iniciar
  periods <- matrix(0,ncol = length(paths))
  carga <- matrix(0,ncol = period)
  #count_course <- matrix(0,ncol = period)
  pendientes <- data$courses
  # seleccionar mejor
  pos_course <- get_pos_high(paths)
  # elegir posicion
  pos_period <- sample(paths[pos_course]:period,1,replace=F)
  # asignar
  periods[pos_course] <- pos_period
  carga[pos_period] <- carga[pos_period] + data$credit[pos_course]
  #count_course[pos_period] <- count_course[pos_period] + 1
  pendientes <- delete_by_pos(pendientes, pos_course, data)
  # seleccionar otro
  while (!(is_empty(pendientes))) {
    new_pos_course <- sample(1:length(pendientes),1,replace=F)
    new_course <- pendientes[new_pos_course]
    #crear lista#
    l_aux <- list(new_course)
    l <- list(new_course)
    while (!(is_empty(l))) {
      for (i in 1:length(relations$prerequisite)) {
        if(relations$prerequisite[i] == l[1]){
          aux <- relations$course[i]
          #print(aux)
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
    #print(l_aux)
    #insertar lista#
    while (!(is_empty(l_aux))) {
      new_course <- l_aux[length(l_aux)]
      check <- TRUE
      pos_place <- 0
      pos_place <- paths[pos_by_name(data$courses, new_course)]
      for (i in 1:length(relations$prerequisite)) {
        if(relations$prerequisite[i] == l_aux[length(l_aux)]){
          aux <- relations$course[i]
          pos_aux <- pos_by_name(data$courses, aux)
          #if(periods[pos_aux] > pos_place){
          #  pos_place <- periods[pos_aux]
          #}
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
      if(paths[pos_course] == pos_place){
        pos_period <- pos_place
      } else{
        pos_period <- sample(paths[pos_course]:pos_place,1,replace=F)
      }
      # asignar
      periods[pos_course] <- pos_period
      carga[pos_period] <- carga[pos_period] + data$credit[pos_course]
      #count_course[pos_period] <- count_course[pos_period] + 1
      pendientes <- delete_by_pos(pendientes, pos_course, data)
      last <- length(l_aux)
      l_aux <- l_aux[-last]
    }
  }
  for (i in alone) {
    carga[periods[i]] <- carga[periods[i]] - data$credit[i]
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
    periods[i] <- aux_pos
  }
  return(periods)
}