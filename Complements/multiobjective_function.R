# Función: multiobjective_function
# 
# Descripción:
#   Evalúa una solución multiobjetivo en base a dos criterios: desviación estándar y equilibrio de créditos.
# 
# Entradas:
#   sol: Vector de asignaciones de periodos para cada curso.
#   period: Número de periodos disponibles.
#   data: Datos del curso que incluyen créditos.
#   min_c, max_c: Valores mínimos y máximos esperados para el criterio de equilibrio de créditos.
#   min_l, max_l: Valores mínimos y máximos esperados para el criterio de desviación estándar.
# 
# Salida:
#   Valor del índice de hipervolumen calculado.
multiobjective_function <- function(sol,period,data,min_c,max_c,min_l,max_l){
  p1 <- minmax_of(curriculum(sol, period, data))
  p1 <- normalize_objective(p1,min_l,max_l)
  p2 <- balance_of(fit(sol, period, data))
  p2 <- normalize_objective(p2,min_c,max_c)
  vector <- rbind(c(p1,p2))
  hypervolume <- computeHV(t(vector), ref.point = c(2,2))
  hypervolume <- 4 - hypervolume
  return(hypervolume)
}