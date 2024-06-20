# Función: select_pos
# 
# Descripción:
#   Selecciona la posición de un elemento en un vector `balance` considerando
#   la que tenga menor balance y minmax.
# 
# Entradas:
#   balance: lista de balances de las soluciones.
#   minmax: lista de minmax de las soluciones.
# 
# Salida:
#   Retorna la posición del elemento seleccionado en el vector `balance`.
select_pos <- function(balance,minmax){
  pos <- 1
  valor_b <- balance[1]
  valor_m <- minmax[1]
  if(length(balance) > 1){
    for(i in 1:length(balance)){
      if(valor_b == balance[i]){
        if(valor_m > minmax[i]){
          valor_b <- balance[i]
          valor_m <- minmax[i]
          pos <- i
        }
      }
      if(valor_b < balance[i]){
        valor_b <- balance[i]
        valor_m <- minmax[i]
        pos <- i
      }
    }
  }
  return(pos)
}