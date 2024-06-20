# Función: crossover_sol
# 
# Descripción:
#   Realiza el cruce (crossover) entre dos soluciones para generar una nueva solución.
# 
# Entradas:
#   sol1: Primera solución para el cruce.
#   sol2: Segunda solución para el cruce.
#   relation: Matriz de relaciones entre cursos.
#   period: Número de períodos disponibles.
# 
# Salida:
#   Nueva solución generada mediante el cruce.
crossover_sol <- function(sol1, sol2, relation, period){
  
  cursos <- length(sol1)
  
  cross_sol <- matrix(0,ncol = cursos)
  
  check <- FALSE
  
  for (i in period:1) {
    
    for (j in 1:cursos) {
      
      sol_select <- sample(1:2,1,replace=F) 
      
      if(sol1[j] == i){
        
        if(cross_sol[j] == 0){
          
          for (k in 1:cursos) {
            
            if(relation[j,k] > 0){
              
              if(cross_sol[k] == 0){
                
                if(sol_select == 1){
                  
                  for(x in 1:cursos){
                    
                    if(relation[k,x] > 0){
                      
                      if ((cross_sol[x] >= sol1[k]) & (k != x)) {
                        check <- TRUE
                      }
                    }
                  }
                  if(check){
                    cross_sol[k] <- sol2[k]
                    #print("sol2")
                    #print(k)
                  } else{
                    cross_sol[k] <- sol1[k]
                    #print("sol1")
                    #print(k)
                  }
                  check <- FALSE
                  
                } else{
                  
                  for(x in 1:cursos){
                    
                    if(relation[k,x] > 0){
                      
                      if ((cross_sol[x] >= sol2[k]) & (k != x)) {
                        check <- TRUE
                      }
                    }
                  }
                  if(check){
                    cross_sol[k] <- sol1[k]
                    #print("sol1")
                    #print(k)
                  } else{
                    cross_sol[k] <- sol2[k]
                    #print("sol2")
                    #print(k)
                  }
                  check <- FALSE
                }
              }  
            }
          }
        }
      }
    }
  }
  
  return(cross_sol)
  
}