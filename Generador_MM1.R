library(dplyr)



n <- 100
min_t_ll <-0
max_t_ll <- 5.88333333

miu <- 3.330682
sd_miu <- 1.321530
lambda <- 1.4075000


Generador_MM1 <- function(n, max_t_ll, min_t_ll, miu, sd_miu, inv_lambda, sd_inv_lambda) {
  #Inicializacion de parametros
  # Este vector consiste de 
  ultimo_cliente<- c(0,0,0,0,0,0,0,0)
  historia_restaurante <- c(ultimo_cliente)
  for(i in 1:n) {
    # e_llegadas <- min_t_ll+ runif(1)* (max_t_ll-min_t_ll)
    e_llegadas <- abs(rnorm(1 ,mean = inv_lambda, sd = sd_inv_lambda))
    # e_llegadas <- rpois(1,lambda)
    llegada <- ultimo_cliente[3]+ e_llegadas
    servicio <- abs(rnorm(1, mean = miu, sd = sd_miu))
    # servicio <- rexp(n = 1, rate = miu)
    # inicio <- ifelse(llegada>ultimo_cliente[7],llegada,llegada+ abs(ultimo_cliente[6]-servicio))
    inicio <- ifelse(llegada>ultimo_cliente[7],llegada,ultimo_cliente[7])
    cola <- inicio-llegada
    
    final <- inicio + servicio
    en_sistema <- final-llegada
    
    cliente_nuevo <- c(i, e_llegadas, llegada, inicio, cola, servicio, final, en_sistema)
    
    historia_restaurante <- rbind(historia_restaurante, cliente_nuevo)
    
    ultimo_cliente <- cliente_nuevo
  }
  
  colnames(historia_restaurante) <- c("id", "Entre_Llegadas", "Llegada", "Inicio", "Cola", "Servicio", "Final", "En_Sistema")
  historia_restaurante <- historia_restaurante[(2:nrow(historia_restaurante)),]
  return(as_data_frame(historia_restaurante))
}

hola<- Generador_MM1(n, max_t_ll, min_t_ll, miu, lambda)
hola$Llegada <- period(hour=(hola$Llegada%/%60), minute=(hola$Llegada%%60%/%1), second=((hola$Llegada%%1*60)%/%1))




  
  
  