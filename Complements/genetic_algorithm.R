# Función: genetic_algorithm
# 
# Descripción:
#   implementa un algoritmo genético multiobjetivo para resolver un problema de 
#   asignación de cursos en períodos académicos, optimizando dos objetivos 
#   principales: minimización del máximo crédito y balance de carga entre períodos.
# 
# Entradas:
#   input: nombre del archivo de entrada.
#   crossover_prob: número entre 0 y 1 para la proporción de la población generada por crossover.
#   mutation_prob: número entre 0 y 1 para la proporción de la población generada por mutacion.
#   population_size: tamaño de la población.
#   numb_generations: cantidad de generaciones
#   eval: parametro true o false en caso que se necesite graficar.
# 
# Salida:
#   Soluciones encontradas luego de aplicar el algoritmo genético multiobjetivo.
genetic_algorithm <- function(input, crossover_prob, mutation_prob, population_size, numb_generations, eval){
  
  dirstudio <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(dirstudio) 
  
  archive_name <- input
  
  data <- read_excel(archive_name, sheet = 1)
  
  relations <- read_excel(archive_name, sheet = 2)
  
  variable <- read_excel(archive_name, sheet = 3)
  
  cant_course <- length(data$courses)
  
  period <- variable$value[1]
  
  crossover <- population_size*crossover_prob
  
  mutation <- population_size*mutation_prob
  
  mix <- population_size - (crossover + mutation)
  
  gra_plot <- TRUE
  
  g <- graph_from_data_frame(relations, directed=TRUE, vertices=data)
  #print(g, e=TRUE, v=TRUE)
  
  plot(g, layout=layout_with_fr, vertex.size=4,
       vertex.label.dist=1.5, vertex.color="red", edge.arrow.size=0.1)
  
  relation <- create_relation(g,cant_course)
  
  alone <- create_alone(data,relation)
  
  critical_paths <- create_critical_paths(relation$In, cant_course)
  
  min_credit <- 0
  
  max_credit <- calculate_credits(data)
  
  min_load <- max_credit/period
  
  max_load <- 2*min_load
  
  inisol <- init_sol(data, critical_paths, period, relations, alone)
  
  # initialize
  
  inipob <- init_pob(data, critical_paths, period, relations, population_size, alone)
  
  last_gen_pob <- inipob
  
  bestfit <- 1
  
  if(eval){
    evalfit <- numeric()
    evalbest <- numeric()
    gen <- numeric()
  }
  
  for(i in 1:population_size){
    sol <- last_gen_pob[[i]]
    testfit <- multiobjective_function(sol,period,data,min_credit,max_credit,min_load,max_load)
    if(testfit <= bestfit){
      bestsol <- sol
      bestfit <- testfit
    }
    if(eval){
      evalfit <- c(evalfit, testfit)
      evalbest <- c(evalbest, bestfit)
      gen <- c(gen, 0)
    }
  }
  
  # evaluate 
  
  for(generation in 1:numb_generations){
    
    new_pob <- last_gen_pob
    
    count <- population_size
    
    # crossover
    for (i in 1:crossover) {
      
      count <- count+1
      
      sol_select <- sample(1:population_size,2,replace=F)
      
      sol <- crossover_sol(last_gen_pob[[sol_select[1]]], last_gen_pob[[sol_select[2]]], relation$In,period)
      
      sol <- improve_sol(sol, period, data, alone)
      
      new_pob[[count]] <- sol
      
    }
    
    # mutation
    for (i in 1:mutation) {
      
      count <- count+1
      
      sol_select <- sample(1:population_size,1,replace=F)
      
      sol <- last_gen_pob[[sol_select]]
      
      level <- sample(2:period, 1)
      
      if (level == period) {
        sol <- init_sol(data, critical_paths, period, relations, alone)
      } else {
        sol <- modify_sol(sol,level,data, critical_paths,period, relations)
        sol <- improve_sol(sol, period, data, alone)
      }
      
      new_pob[[count]] <- sol
      
    }
    
    # random
    for (i in 1:mix) {
      
      count <- count+1
      
      sol <- init_sol(data, critical_paths, period, relations, alone)
      
      new_pob[[count]] <- sol
      
    }
    
    # select
    soluciones <- matrix(0,nrow = count,ncol = 2)
    
    for (i in 1:count) {
      p1 <- minmax_of(curriculum(new_pob[[i]], period, data))
      p1 <- normalize_objective(p1,min_load,max_load)
      p2 <- balance_of(fit(new_pob[[i]], period, data))
      p2 <- normalize_objective(p2,min_credit,max_credit)
      soluciones[i,1] <- p1
      soluciones[i,2] <- p2
    }
    
    ordenamiento_nodominado <- doNondominatedSorting(t(soluciones))$ranks
    
    ranks <- matrix(0,nrow = count,ncol = 2)
    
    level <- 1
    pos <- 0
    
    while (validate_list(ranks[,1])) {
      for (i in 1:count) {
        if(ordenamiento_nodominado[i] == level){
          pos <- pos+1
          ranks[pos,1] <- level
          ranks[pos,2] <- i
        }
      }
      level <- level+1
    }
    
    last_gen_pob <- list()
    
    if(ranks[population_size,1] == ranks[population_size+1,1]){
      
      corta_en = ranks[population_size,1]
      soluciones_rank=soluciones[which((ordenamiento_nodominado==corta_en)),]
      distancia_aglomeracion=computeCrowdingDistance(t(soluciones_rank))
      last_level <- matrix(0,nrow = length(distancia_aglomeracion),ncol = 2)
      last_level[,1] <- distancia_aglomeracion
      pos <- 0
      for (i in 1:count) {
        if(ranks[i,1] == ranks[population_size,1]){
          pos <- pos+1
          last_level[pos,2] <- ranks[i,2]
        }
      }
      for(i in 1:population_size){
        if(ranks[i,1] < ranks[population_size,1]){
          last_gen_pob[i] <- new_pob[ranks[i,2]]
          point <- soluciones[ranks[i,2],]
          testfit <- hypervolume_function(point[1],point[2])
          if(testfit <= bestfit){
            bestsol <- new_pob[[ranks[i,2]]]
            bestfit <- testfit
          }
          if(eval){
            evalfit <- c(evalfit, testfit)
            evalbest <- c(evalbest, bestfit)
            gen <- c(gen, generation)
          }
          
        } else {
          aux <- -1
          pos <- 0
          for(j in 1:length(last_level[,1])){
            if(last_level[j,1] > aux){
              pos <- j
              aux <- last_level[j,1]
            }
          }
          last_gen_pob[i] <- new_pob[last_level[pos,2]]
          point <- soluciones[last_level[pos,2],]
          testfit <- hypervolume_function(point[1],point[2])
          if(testfit <= bestfit){
            bestsol <- new_pob[[last_level[pos,2]]]
            bestfit <- testfit
          }
          if(eval){
            evalfit <- c(evalfit, testfit)
            evalbest <- c(evalbest, bestfit)
            gen <- c(gen, generation)
          }
          last_level<-last_level[-pos,]
        }
      }
      
    } else {
      
      for(i in 1:population_size){
        last_gen_pob[i] <- new_pob[ranks[i,2]]
      }
    }
    
    if(generation == 1 | generation == 5 | generation == 10 | generation == 15 | generation == 20){
      
      sol <- matrix(0,nrow = population_size,ncol = 2)
      soluciones <- matrix(0,nrow = population_size,ncol = 2)
      
      for (j in 1:population_size) {
        p1 <- minmax_of(curriculum(last_gen_pob[[j]], period, data))
        p1_n <- normalize_objective(p1,min_load,max_load)
        p2 <- balance_of(fit(last_gen_pob[[j]], period, data))
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
        if(generation == 1){
          gra_Gen <- c(gra_Gen, "Gen01")
        }
        if(generation == 5){
          gra_Gen <- c(gra_Gen, "Gen05")
        }
        if(generation == 10){
          gra_Gen <- c(gra_Gen, "Gen10")
        }
        if(generation == 15){
          gra_Gen <- c(gra_Gen, "Gen15")
        }
        if(generation == 20){
          gra_Gen <- c(gra_Gen, "Gen20")
        }
        gra_MM <- c(gra_MM, soluciones_rank[1])
        gra_Bal <- c(gra_Bal, soluciones_rank[2])
      }else{
        for(k in 1:length(soluciones_rank[,1])){
          if(generation == 1){
            gra_Gen <- c(gra_Gen, "Gen01")
          }
          if(generation == 5){
            gra_Gen <- c(gra_Gen, "Gen05")
          }
          if(generation == 10){
            gra_Gen <- c(gra_Gen, "Gen10")
          }
          if(generation == 15){
            gra_Gen <- c(gra_Gen, "Gen15")
          }
          if(generation == 20){
            gra_Gen <- c(gra_Gen, "Gen20")
          }
          gra_MM <- c(gra_MM, soluciones_rank[k,1])
          gra_Bal <- c(gra_Bal, soluciones_rank[k,2])
        }
      }
    }
    
  }
  gra_df <- data.frame(
    "generacion" = gra_Gen,
    "minmax" = gra_MM,
    "RECM" = gra_Bal
  )
  
  plot_balance <- ggplot(data = gra_df,
                         mapping = aes(x = minmax,
                                       y = RECM,
                                       color = generacion))+
    geom_point() +
    geom_line() +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(n.breaks = 10) +
    ggtitle("Convergencia Frontera Pareto") +
    xlab("Normalized MinMax") +
    ylab("Normalized RECM") +
    theme(
      panel.background = element_rect(fill = "white",
                                      colour = "white",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "lightgrey"),
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "lightgrey")
    )
  print(plot_balance)
  
  
  solution <- list("courses" = data$courses, "credit" = data$credit, 
                   "period" = last_gen_pob)
  
  #returning the solution
  if(eval)
    return(list(sol=solution, best=bestsol, fit=bestfit, evalfit=evalfit, evalbest=evalbest, gen=gen))
  else
    return(list(sol=solution, best=bestsol, fit=bestfit))
  
}
