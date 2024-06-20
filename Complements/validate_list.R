# Función: validate_list
# 
# Descripción:
#   Verifica si hay algún elemento igual a 0 en la lista `list`, esto se utiliza 
#   para verificar si algún elemento de la lista aún no fue asignado.
# 
# Entradas:
#   list: Lista de valores.
# 
# Salida:
#   TRUE si hay algún elemento igual a 0, FALSE de lo contrario.
validate_list <- function(list){
  for (i in list) {
    if(i == 0){
      return(TRUE)
    }
  }
  return(FALSE)
}