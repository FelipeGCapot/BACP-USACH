# Función: init_pob
# 
# Descripción:
#   Inicializa una población de soluciones para un problema específico.
#   data: dataframe con la información de los cursos.
#   critical_paths: lista de las rutas criticas de cada curso.
#   period: Número total de períodos disponibles.
#   relations: dataframe que contiene las relaciones de requisitos entre cursos.
#   population_size: Tamaño de la población.
#   alone: lista con los cursos que no se relacionan con ningun otro curso.
# 
# Salida:
#   Lista de tamaño population_size donde cada elemento es una solución inicializada.
init_pob <- function(data, critical_paths, period, relations, population_size, alone){
  
  pob <- list()
  
  for (i in 1:population_size) {
    
    sol <- init_sol(data, critical_paths, period, relations, alone)
    
    pob[[i]] <- sol
    
  }
  
  return(pob)
  
}