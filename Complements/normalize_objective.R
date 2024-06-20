# Función: normalize_objective
# 
# Descripción:
#   Normaliza un valor dentro de un rango especificado utilizando la fórmula min-max.
# 
# Entradas:
#   val: Valor que se desea normalizar.
#   min_credit: Valor mínimo del rango.
#   max_credit: Valor máximo del rango.
# 
# Salida:
#   Valor normalizado entre 0 y 1.
normalize_objective<- function(val,min_credit,max_credit)  {
  z <- (val - min_credit)/(max_credit - min_credit)
  return(z)
}