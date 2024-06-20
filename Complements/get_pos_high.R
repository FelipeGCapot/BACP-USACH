# Función: get_pos_high
# 
# Descripción:
#   Encuentra la posición del elemento más alto en una lista.
# 
# Entradas:
#   list: Vector numérico donde se busca el elemento más alto.
# 
# Salida:
#   Retorna la posición del elemento más alto en la lista.
get_pos_high<-function(list){
  sol <- 1
  for (i in 1:length(list)) {
    if(list[i] >= list[sol]){
      sol <- i
    }
  }
  return(sol)
}