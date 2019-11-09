library(dplyr)




Formatear <- function(variable) {
  nueva <- period(hour=(variable%/%60), minute=(variable%%60%/%1), second=((variable%%1*60)%/%1))
  return(nueva)
}

Generador_de_Cola <- function(my_row, n=60) {
  cc <- my_row["CC"]
  restaurante <- my_row["Restaurante"]
  t_dia <-as.factor(my_row["FinDe"])
  miu <- as.numeric(my_row['Miu'])
  inv_lambda <- as.numeric(my_row['Inv_Lambda'])
  sd_inv_lambda <- as.numeric(my_row['Sd_Inv_Lambda'])
  
  lambda <- as.numeric(my_row['Lambda'])
  
  #Inicializacion de parametros
  # Este vector consiste de 
  # Id|Tiempo Entre Llegadas|Llegadas|Servicio|Inicio|Tiempo en Cola|Tiempo en Servicio|Final|Tiempo en Sistema|
  ultimo_cliente<- c(0,0,0,0,0,0,0,0)
  historia_restaurante <- c(ultimo_cliente)
  
  
  clientes_por_minuto <- rpois(n, lambda)
  llegadas <- c()
  for (j in 1:length(clientes_por_minuto)) {
    # Se generan horas de llegadas de forma aleatoria entre cada minuto 
    horas <- runif(clientes_por_minuto[j], j-1, j) %>% round(digits=2) %>% sort()
    llegadas <- c(llegadas,horas)
  }
  
  
  # for(i in 1:n) {
  for(i in 1:length(llegadas)) {
    llegada <- llegadas[i]
    e_llegadas <- llegada-ultimo_cliente[3]
    
    servicio <- rexp(n = 1, rate = miu) %>% round(digits=2)
    inicio <- ifelse(llegada>ultimo_cliente[7],llegada,ultimo_cliente[7])
    cola <- (inicio-llegada) %>% round(2)
    
    final <- inicio + servicio
    en_sistema <- final-llegada
    
    cliente_nuevo <- c(i, e_llegadas, llegada, inicio, cola, servicio, final, en_sistema)
    
    historia_restaurante <- rbind(historia_restaurante, cliente_nuevo)
    
    ultimo_cliente <- cliente_nuevo
  }
  
  historia_restaurante <- historia_restaurante[(2:nrow(historia_restaurante)),]
  historia_restaurante <- cbind(cc, restaurante, t_dia, historia_restaurante) 
  
  colnames(historia_restaurante) <- c("CC", "Restaurante", "FinDe","id", "Entre_Llegadas", "Llegada", "Inicio", "Cola", "Servicio", "Final", "En_Sistema")
  
  historia_restaurante <- as_data_frame(historia_restaurante) 
  
  # historia_restaurante<- historia_restaurante %>%
  #   mutate_at(c("Entre_Llegadas", "Llegada", "Inicio", "Cola", "Servicio", "Final", "En_Sistema"), ~Formatear(.))
    
  return(historia_restaurante)
} 


Simulaciones <- apply(datos_a_usar, 1, Generador_de_Cola, n=120)


Simulaciones[[19]] %>% View()




  
  