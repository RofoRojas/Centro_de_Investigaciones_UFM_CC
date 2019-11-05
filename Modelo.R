# install.packages('readxl')
library(readxl)
library(dplyr)
library(lubridate)
# Solo para borrar todo el espacio
rm(list = ls())

## Ingresar a todos los Centros Comerciales
for (folder in list.files("Data")) {
  ## Ingresar a todos los Restaurantes
  for (archivo in list.files(paste("Data", folder, sep ="/" ))) {
    ## Ingresar a todos los dias
    for (sheet in 1:length(excel_sheets(paste("Data", folder, archivo, sep = "/")))) {
      # Solo para poder ver que estoy sacando
      print(paste("Leyendo: Data/", folder,'/', archivo,':sheet',sheet, sep = ""))
      
      # Importar tabla de datos
      temp_table <- read_excel(path = paste("Data", folder, archivo, sep = "/"), sheet = sheet, skip = 3,
                               # Se coloca la esquina superior derecha se colocan los tipos de datos
                               range = cell_limits(c(4, NA), c(NA, 4)),col_types = c("numeric", "date", "date", "date"))
      # Renombramos las columnas
      colnames(temp_table) = c('Ingreso', 'Llegada', 'Inicio', 'Final')
      
      # Se dejan unicamente las filas completas pues el codigo anterior lee mas de las que existen y se laguean las llegadas
      temp_table<- temp_table  %>% filter(complete.cases(temp_table)) %>% mutate(Llegada_Anterior= lag(Llegada,1)) %>% 
        # Se estiman los diferentes tiempos que se tienen de la toma de datos que son relevantes para las colas.
        mutate(T_Llegadas=as.duration(Llegada-Llegada_Anterior), T_Cola= as.duration(Inicio-Llegada), 
               T_Servicio=as.duration(Final-Inicio), T_Sistema= as.duration(Final-Llegada)) %>% 
        # Transformar a minutos
        mutate(T_Llegadas=T_Llegadas/dminutes(1), T_Cola= T_Cola/dminutes(1), T_Servicio=T_Servicio/dminutes(1), T_Sistema= T_Sistema/dminutes(1))
      
      
      # Se obtienen los datos mas relevantes de cada archivo y se resumen en esta tabla
      datos_utiles <- temp_table  %>% 
        summarise(CC= folder, Restaurante= substr(archivo, 6, nchar(archivo)-5), Fecha= date(Llegada[1]),Cantidad= n(),
                  Intervalo= (Llegada[1]%--%Llegada[nrow(temp_table)]), 
                  # Datos para poder realizar simulacion
                  Inv_Lambda= mean(T_Llegadas, na.rm = T), Sd_Inv_Lambda = sd(T_Llegadas, na.rm=T) , Miu= mean(T_Servicio), Sd_Miu= sd(T_Servicio),
                  Min_T_Llegadas=min(T_Llegadas, na.rm = T), Max_T_Llegadas= max(T_Llegadas, na.rm = T),
                  # Promedios de tiempos calculados
                  T_Cola= mean(T_Cola), T_Sistema=mean(T_Sistema)) %>% 
        mutate(D_Semana=wday(Fecha,week_start = 1)) %>% mutate(FinDe = ifelse(D_Semana>=6,1,0)) %>% 
        mutate(Tiempo_Tot= as.duration(Intervalo)/dhours(1)) %>% 
        mutate(Lambda= Cantidad/(Tiempo_Tot*60))
      
      # Si no existe crear tabla datos_iniciales
      if (exists("datos_iniciales")) {
        datos_iniciales <-rbind(datos_iniciales, datos_utiles)
      } else {
        datos_iniciales <- datos_utiles
      }
    }
  }
}

datos_a_usar <- datos_iniciales %>%
  mutate(LambdaS=(Cantidad*Lambda), MiuS=(Cantidad*Miu), Var_Miu= (Sd_Miu^2), Inv_LambdaS=(Cantidad*Inv_Lambda), 
         Var_Inv_Lambda=(Sd_Inv_Lambda^2)) %>% group_by(CC, Restaurante, FinDe) %>% 
  # Resumen de los datos relevantes por grupo
  summarise(Cantidad = sum(Cantidad), Lambda= sum(LambdaS)/sum(Cantidad),
            Miu= sum(MiuS)/sum(Cantidad), Sd_Miu=sqrt(sum(Var_Miu)), 
            Min_T_Llegadas= min(Min_T_Llegadas), Max_T_Llegadas=max(Max_T_Llegadas), 
            Inv_Lambda=sum(Inv_LambdaS)/sum(Cantidad), Sd_Inv_Lambda = sqrt(sum(Var_Inv_Lambda)),
            Tiempo_Tot=sum(Tiempo_Tot))

# Para poder calcular la distribucion por cada centro comercial
Totales <- datos_a_usar %>% mutate(por_hora = Cantidad/Tiempo_Tot) %>% group_by(CC, FinDe) %>% 
  summarise(Total=sum(por_hora))

datos_a_usar<- datos_a_usar %>% mutate(por_hora = Cantidad/Tiempo_Tot) %>% full_join(Totales,by=c("CC", "FinDe")) %>% 
  mutate(Distribucion=por_hora/Total) %>% 
  # Reordenar columnas
  select(CC, Restaurante, FinDe, Lambda, Miu,  Sd_Miu, Min_T_Llegadas, Max_T_Llegadas, Inv_Lambda, Sd_Inv_Lambda, Distribucion, Cantidad, Tiempo_Tot)



Caracteristicas_de_Operacion <- function(my_row) {
  # Declaracion de variables
  cc <- my_row["CC"]
  restaurante <- my_row["Restaurante"]
  t_dia <-as.numeric(my_row["FinDe"])
  lambda <- as.numeric(my_row["Lambda"])
  miu <- as.numeric(my_row["Miu"])
  
  
  # La probabilidad de que no haya unidades en el sistema:
  P0 = 1 - (lambda)/(miu)
  # El número promedio de unidades en la línea de espera:
  Lq = (lambda)^2 / (miu)*(miu - lambda)
  # El número promedio de unidades en el sistema:
  L = Lq + (lambda)/(miu)
  # El tiempo promedio que la unidad pasa en la línea de espera:
  Wq = (Lq)/(lambda)
  # El tiempo promedio que una unidad pasa en el sistema:
  W = Wq + 1/miu
  # La probabilidad de que una unidad que llega no tenga que esperar a ser atendida:
  Pw = (lambda)/(miu)
  # La probabilidad de que haya n unidades en el sistema:
  # n = #definir n
  # Pn = ((lambda/miu)^n)*P0
  
  Caracteristicas<- c(cc, restaurante, miu, lambda, t_dia, P0, Lq, L, Wq, W, Pw)
  return(Caracteristicas)
}


# Utilizamos un apply para correrlo a travez de todas las filas
Caracteristicas <- as_data_frame(t(apply(datos_a_usar ,1, Caracteristicas_de_Operacion)))
# Renombramos las Columnas
colnames(Caracteristicas) <- c("CC", "Restaurante", "Miu", "Lambda", "FinDe", "P0", "Lq", "L", "Wq", "W", "Pw")
# Cambiamos el tipo de datos en las columnas para proximo manejo
Caracteristicas <- Caracteristicas %>% mutate_at(c("Miu", "Lambda", "P0","Lq", "Lq", "L", "Wq", "W", "Pw"), as.numeric) %>% 
  mutate_at("FinDe", as.factor)

# Lo escribo a un archivo que puede ser utilizado despues
write.csv(Caracteristicas, file = "Resultados/Caracteristicas de Operacion.csv", row.names = FALSE)


Generador_MM1 <- function(n, miu, sd_miu, inv_lambda, sd_inv_lambda) {
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

simulaciones <- list()

for(i in 1:nrow(datos_a_usar)){
  
}






folder <- ""
archivo <- "DatosMcdonalds.xlsx"
sheet<- 1


