# Función: hypervolume_function
# 
# Descripción:
#   Calcula el índice de hipervolumen basado en dos criterios dados.
# 
# Entradas:
#   p1: Primer criterio para el cálculo del índice de hipervolumen.
#   p2: Segundo criterio para el cálculo del índice de hipervolumen.
# 
# Salida:
#   Valor del índice de hipervolumen calculado.
hypervolume_function <- function(p1,p2){
  vector <- rbind(c(p1,p2))
  hypervolume <- computeHV(t(vector), ref.point = c(2,2))
  hypervolume <- 4 - hypervolume
  return(hypervolume)
}