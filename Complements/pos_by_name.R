# Función: pos_by_name
# 
# Descripción:
#   Encuentra la posición de un elemento específico por su nombre en una lista.
# 
# Entradas:
#   list: Lista donde se buscará el nombre.
#   name: Nombre del elemento que se desea encontrar.
# 
# Salida:
#   Retorna la posición del primer elemento que coincida con `name` en la lista, o NULL si no se encuentra.
pos_by_name<-function(list, name){
  for (i in 1:length(list)) {
    if(list[i] == name){
      return(i)
    }
  }
}