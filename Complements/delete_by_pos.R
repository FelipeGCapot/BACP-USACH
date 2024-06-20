# Función: delete_by_pos
# 
# Descripción:
#   Elimina un elemento de una lista según su posición y un dato de referencia.
# 
# Entradas:
#   list: Lista donde se buscará y eliminará el elemento.
#   pos: Posición del elemento a eliminar.
#   data: Objeto que contiene información adicional para identificar el elemento a eliminar.
# 
# Salida:
#   Retorna la lista actualizada después de eliminar el elemento en la posición `pos`.
delete_by_pos<-function(list, pos, data){
  if(is.null(list)){
    return(list)
  }
  del <- data$courses[pos]
  for (i in 1:length(list)) {
    if(list[i] == del){
      list <- list[-i]
      return(list)
    }
  }
}