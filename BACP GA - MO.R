################ Paquetes ################

library(tidyverse)
library(knitr)
library(igraph)
library(ggraph)
library(readxl)
library(openxlsx)
library("ecr")

################ FUNCIONES COMPLEMENTARIAS ################

source("Complements/create_relation.R")
source("Complements/create_alone.R")
source("Complements/validate_list.R")
source("Complements/create_critical_paths.R")
source("Complements/select_pos.R")
source("Complements/get_pos_high.R")
source("Complements/delete_by_pos.R")
source("Complements/pos_by_name.R")
source("Complements/create_sol.R")
source("Complements/modify_sol.R")
source("Complements/check_rel_data.R")
source("Complements/check_sol.R")
source("Complements/init_sol.R")
source("Complements/calculate_credits.R")
source("Complements/normalize_objective.R")
source("Complements/curriculum.R")
source("Complements/fit.R")
source("Complements/improve_sol.R")
source("Complements/minmax_of.R")
source("Complements/balance_of.R")
source("Complements/multiobjective_function.R")
source("Complements/hypervolume_function.R")
source("Complements/init_pob.R")
source("Complements/crossover_sol.R")
source("Complements/genetic_algorithm.R")

################ Parametros de entrada ################

# bacp8.xlsx
# bacp10.xlsx
# bacp12.xlsx
# Ingenieria_Civil_Industrial.xlsx
# Ingenieria_Civil_Informatica_Diurno_Antiguo.xlsx
# Ingenieria_Civil_Informatica_Diurno_Nuevo.xlsx
# Ingenieria_Civil_Informatica_Vespertino.xlsx
# Ingenieria_Civil_Metalurgia.xlsx
# Ingenieria_Civil_Quimica.xlsx
# Ingenieria_Ejecucion_Informatica_Diurno.xlsx
# Ingenieria_Ejecucion_Informatica_Vespertino.xlsx

input <- "Data/bacp8.xlsx"
output <- "Datos GA Prueba.xlsx"
crossover_prob <- 0.4
mutation_prob <- 0.4
population_size <- 250
numb_generations <- 20
eval <- TRUE

# ################ TEST ################

# eval = TRUE en caso de querer graficar y evaluar soluciones.
if(eval){
  dirstudio <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(dirstudio) 
  
  archive_name <- input
  
  data <- read_excel(archive_name, sheet = 1)
  
  relations <- read_excel(archive_name, sheet = 2)
  
  variable <- read_excel(archive_name, sheet = 3)
  
  cant_course <- length(data$courses)
  
  period <- variable$value[1]
  
  #max_course <- variable$value[5]
  
  #check_rel_data()
  
  g <- graph_from_data_frame(relations, directed=TRUE, vertices=data)
  #print(g, e=TRUE, v=TRUE)
  
  #plot(g, layout=layout_with_fr, vertex.size=4,
  #     vertex.label.dist=1.5, vertex.color="red", edge.arrow.size=0.1)
  
  relation <- create_relation(g,cant_course)
  
  alone <- create_alone(data,relation)
  
  critical_paths <- create_critical_paths(relation$In, cant_course)
  
  min_credit <- 0
  
  max_credit <- calculate_credits(data)
  
  min_load <- max_credit/period
  
  max_load <- 1.5*min_load
  
  algorithm <- character()
  ga <- numeric()
  tiempo <- numeric()
  minmax <- numeric()
  balance <- numeric()
  minmax_n <- numeric()
  balance_n <- numeric()
  hypervolume <- numeric()
  list_sol <- list()
  gra_Gen <- character()
  gra_MM <- numeric()
  gra_Bal <- numeric()
  pivote <- 1
}

# se ejecuta el algoritmo 11 veces para tener una muestra representativa.
for(i in 1:11){

  record <- proc.time() 
    
  test <- genetic_algorithm(input, crossover_prob, mutation_prob, population_size, numb_generations, eval)

  record <- proc.time()-record
  
  print(i)
  print(record)

  if(eval){
    
    sol <- matrix(0,nrow = population_size,ncol = 2)
    soluciones <- matrix(0,nrow = population_size,ncol = 2)
    
    for (j in 1:population_size) {
      p1 <- minmax_of(curriculum(test$sol$period[[j]], period, data))
      p1_n <- normalize_objective(p1,min_load,max_load)
      p2 <- balance_of(fit(test$sol$period[[j]], period, data))
      p2_n <- normalize_objective(p2,min_credit,max_credit)
      sol[j,1] <- p1
      sol[j,2] <- p2
      soluciones[j,1] <- p1_n
      soluciones[j,2] <- p2_n
    }
    ordenamiento_nodominado <- doNondominatedSorting(t(soluciones))$ranks
    sol_rank=sol[which((ordenamiento_nodominado==1)),]
    soluciones_rank=soluciones[which((ordenamiento_nodominado==1)),]
    
    if(length(soluciones_rank) == 2){
      list_sol[[pivote]] <- test$sol$period[[1]]
      pivote <- pivote + 1
      hv <- hypervolume_function(soluciones_rank[1],soluciones_rank[2])
      algorithm <- c(algorithm,"GA")
      ga <- c(ga, i)
      tiempo <- c(tiempo, record[3])
      minmax <- c(minmax, sol_rank[1])
      balance <- c(balance, sol_rank[2])
      minmax_n <- c(minmax_n, soluciones_rank[1])
      balance_n <- c(balance_n, soluciones_rank[2])
      hypervolume <- c(hypervolume, hv)
    }else{
      hv = 4 - computeHV(t(soluciones_rank), ref.point = c(2,2))
      for(k in 1:length(soluciones_rank[,1])){
        list_sol[[pivote]] <- test$sol$period[[k]]
        pivote <- pivote + 1
        algorithm <- c(algorithm,"GA")
        ga <- c(ga, i)
        tiempo <- c(tiempo, record[3])
        minmax <- c(minmax, sol_rank[k,1])
        balance <- c(balance, sol_rank[k,2])
        minmax_n <- c(minmax_n, soluciones_rank[k,1])
        balance_n <- c(balance_n, soluciones_rank[k,2])
        hypervolume <- c(hypervolume, hv)
      }
    }
  }
  if(length(soluciones_rank) == 2){
    plot(soluciones_rank[1], soluciones_rank[2], 
         xlab = "Normalized minmax", ylab = "Normalized balance",
         main = "Frontera de pareto",
         pch  = 16,
         col = "black")
  }else{
    new_sol <- soluciones_rank
    sort_sol <- sort(soluciones_rank[,1])
    for(i in 1:length(soluciones_rank[,1])){
      new_sol[i,1] <-  sort_sol[i]
      for(j in 1:length(soluciones_rank[,2])){
        if(sort_sol[i] == soluciones_rank[j,1]){
          new_sol[i,2] <- soluciones_rank[j,2]
        }
      }
    }
    plot(new_sol[,1], new_sol[,2], 
         xlab = "Normalized minmax", ylab = "Normalized balance",
         main = "Frontera de pareto",
         type = "l", lwd = 2, lty = 5,
         pch  = 16,
         col = "black")
  }
}

if(eval){

  table <- data.frame(
    
    "Algoritmo" = algorithm,
    "Ejecucion" = ga,
    "Tiempo" = tiempo,
    "Minmax" = minmax,
    "Balance" = balance,
    "Normalized minmax" = minmax_n,
    "Normalized balance" = balance_n,
    "Hypervolume" = hypervolume
    
  )
  
  #print(minmax_of(curriculum(data$period, period, data)))
  
  #print(balance_of(fit(data$period, period, data)))
  
  write.xlsx(table, output)
  
  #mi_df <- data.frame(
  #  "fit" = test$evalfit,
  #  "generacion" = test$gen
  #)
  
  #boxplot(fit ~ generacion, data = mi_df)
  
  name <- "Ejemplo GA App.xlsx"
  
  pos <- select_pos(balance,minmax)
  # pos <- 1 # en caso de elegir una solución en especifico
  
  app_df <- data.frame(
    "courses" = data$courses,
    "credit" = data$credit,
    "period" = t(test$sol$period[[pos]])
  )
  
  write.xlsx(app_df, name)
}
