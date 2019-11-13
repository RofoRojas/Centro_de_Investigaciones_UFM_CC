# install.packages('readxl')
library(readxl)
library(dplyr)
library(lubridate)

# Solo para borrar todo el espacio de trabajo antes de crear variables
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
      temp_table<- temp_table  %>% filter(complete.cases(temp_table)) %>% mutate(Llegada_Anterior= lag(Llegada,1), Final_Anterior= lag(Final,1)) %>% 
        # Se estiman los diferentes tiempos que se tienen de la toma de datos que son relevantes para las colas.
        mutate(T_Llegadas=as.duration(Llegada-Llegada_Anterior), T_Cola= as.duration(Inicio-Llegada), 
               T_Servicio=as.duration(Final-Inicio), T_Sistema= as.duration(Final-Llegada),
               
               Libre= as.duration(ifelse(Llegada>Final_Anterior, Final-Llegada,0))) %>% 
        # Transformar a minutos
        mutate(T_Llegadas=T_Llegadas/dminutes(1), T_Cola= T_Cola/dminutes(1), T_Servicio=T_Servicio/dminutes(1), T_Sistema= T_Sistema/dminutes(1),
               
               Libre= Libre/dminutes(1), Hizo_Cola=ifelse(T_Cola!=0, 1, 0))
      # Con estos ciclos, calculo cual es el promedio de personas en el sistema y el promedio de personas en Cola
      En_Sistema<-c(0)
      En_Cola<-c(0)
      for (i in 2:nrow(temp_table)) {
        contador_s <- 0
        contador_c<- ifelse(temp_table$T_Cola[i]>0,1,0)
        for (j in 1:(i-1)) {
          contador_s <- contador_s + ifelse(temp_table$Final[j]>temp_table$Llegada[i],1,0)
          contador_c <- contador_c + ifelse(temp_table$Inicio[j]>temp_table$Llegada[i],1,0)
        }
        En_Sistema<- c(En_Sistema, contador_s)
        En_Cola<- c(En_Cola, contador_c)
      }
      temp_table$En_Sistema<- En_Sistema
      temp_table$En_Cola <- En_Cola
      
      
      # Se obtienen los datos mas relevantes de cada archivo y se resumen en esta tabla
      datos_utiles <- temp_table  %>% 
        summarise(CC= folder, Restaurante= substr(archivo, 6, nchar(archivo)-5), Fecha= date(Llegada[1]),Cantidad= n(),
                  Intervalo= (Llegada[1]%--%Llegada[nrow(temp_table)]), Intervalo2= (Llegada[1]%--%Final[nrow(temp_table)]),
                  # Datos para poder realizar simulacion
                  Inv_Lambda= mean(T_Llegadas, na.rm = T), Sd_Inv_Lambda = sd(T_Llegadas, na.rm=T) , Miu= mean(T_Servicio), Sd_Miu= sd(T_Servicio),
                  Min_T_Llegadas=min(T_Llegadas, na.rm = T), Max_T_Llegadas= max(T_Llegadas, na.rm = T), Max_T_Cola= max(T_Cola, na.rm = T),
                  # Promedios de tiempos calculados
                  T_Cola= mean(T_Cola), T_Sistema_Prom=mean(T_Sistema),
                  T_Sistema_Total= sum(T_Sistema), Libre=sum(Libre, na.rm = T), Hizo_Cola= sum(Hizo_Cola),
                  En_Cola= mean(En_Cola), En_Sistema= mean(En_Sistema)) %>% 
        mutate(D_Semana=wday(Fecha,week_start = 1)) %>% mutate(FinDe = ifelse(D_Semana>=6,1,0)) %>% 
        mutate(Tiempo_Tot= as.duration(Intervalo)/dhours(1)) %>% 
        mutate(Lambda= Cantidad/(Tiempo_Tot*60)) %>% 
        mutate(Total_Minutos= as.duration(Intervalo2)/dminutes(1)) 
      
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
  # S es utilizada para marcar la diferencia de las variables con pesos
  mutate(LambdaS=(Cantidad*Lambda), MiuS=(Cantidad*Miu), Var_Miu= (Sd_Miu^2), Inv_LambdaS=(Cantidad*Inv_Lambda), 
         Var_Inv_Lambda=(Sd_Inv_Lambda^2), En_ColaS= Cantidad*En_Cola, En_SistemaS= Cantidad*En_Sistema,
         T_ColaS= Cantidad*T_Cola, T_Sistema_PromS= Cantidad*T_Sistema_Prom) %>% group_by(CC, Restaurante, FinDe) %>% 
  # Resumen de los datos relevantes por agrupacion establecida
  summarise(Cantidad = sum(Cantidad), Lambda= sum(LambdaS)/sum(Cantidad),
            Miu= 1/(sum(MiuS)/sum(Cantidad)), Sd_Miu=1/(sqrt(sum(Var_Miu))), 
            Min_T_Llegadas= min(Min_T_Llegadas), Max_T_Llegadas=max(Max_T_Llegadas), Max_T_Cola= max(Max_T_Cola),
            Inv_Lambda=sum(Inv_LambdaS)/sum(Cantidad), Sd_Inv_Lambda = sqrt(sum(Var_Inv_Lambda)),
            Inv_Miu = (sum(MiuS)/sum(Cantidad)),
            T_Cola= sum(T_ColaS)/sum(Cantidad), T_Sistema_Prom = sum(T_Sistema_PromS)/sum(Cantidad),
            Tiempo_Tot=sum(Tiempo_Tot),
            # Caracteristicas de operación calculadas
            Total_Minutos= sum(Total_Minutos), Libre = sum(Libre), P_No_Cola= 1-sum(Hizo_Cola)/sum(Cantidad),
            En_Cola=sum(En_ColaS)/sum(Cantidad), En_Sistema=sum(En_SistemaS)/sum(Cantidad)) %>% 
  mutate(Por_Ocioso= Libre/Total_Minutos)

# Para poder calcular la distribucion por cada centro comercial
Totales <- datos_a_usar %>% mutate(por_hora = Cantidad/Tiempo_Tot) %>% group_by(CC, FinDe) %>% 
  summarise(Total=sum(por_hora))

datos_a_usar<- as_data_frame(datos_a_usar) %>% mutate(por_hora = Cantidad/Tiempo_Tot) %>% full_join(Totales,by=c("CC", "FinDe")) %>% 
  mutate(Distribucion=por_hora/Total) %>% 
  # Reordenar columnas para mostrarlas ordenadas
  select(CC, Restaurante, FinDe, Lambda, Miu,  Distribucion, P_No_Cola, Por_Ocioso, T_Cola, T_Sistema_Prom, Max_T_Cola, En_Cola, En_Sistema,
         Sd_Miu, Min_T_Llegadas, Max_T_Llegadas, Inv_Miu, Inv_Lambda, Sd_Inv_Lambda, Cantidad, Tiempo_Tot)



# Se escribe a un archivo donde se puede ver esta tabla a utilizar
write.csv(datos_a_usar, file = "Resultados/Datos Finales.csv", row.names = FALSE)  

Caracteristicas_de_Operacion <- function(my_row) {
  # Declaracion de variables
  cc <- my_row["CC"]
  restaurante <- my_row["Restaurante"]
  t_dia <-as.numeric(my_row["FinDe"])
  lambda <- as.numeric(my_row["Inv_Lambda"])
  miu <- as.numeric(my_row["Inv_Miu"])
  distribucion <- my_row["Distribucion"]
  
  
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
  
  Caracteristicas<- c(cc, restaurante, miu, lambda, distribucion, t_dia, P0, Lq, L, Wq, W, Pw)
  return(Caracteristicas)
}

# Utilizamos un apply para correrlo a travez de todas las filas
Caracteristicas <- as_data_frame(t(apply(datos_a_usar ,1, Caracteristicas_de_Operacion)))
# Renombramos las Columnas
colnames(Caracteristicas) <- c("CC", "Restaurante", "Miu", "Lambda", "Distribucion", "FinDe", "P0", "Lq", "L", "Wq", "W", "Pw")
# Cambiamos el tipo de datos en las columnas para proximo manejo
Caracteristicas <- Caracteristicas %>% mutate_at(c("Miu", "Lambda", "Distribucion", "P0","Lq", "Lq", "L", "Wq", "W", "Pw"), as.numeric) %>% 
  mutate_at("FinDe", as.factor)

# Lo escribo a un archivo que puede ser utilizado despues
write.csv(Caracteristicas, file = "Resultados/Caracteristicas de Operacion.csv", row.names = FALSE)


# La probabilidad de que haya n unidades en el sistema:
Tabla_de_Probabilidades <- function(my_row) {
  # Obtener datos de la fila
  cc <- my_row["CC"]
  restaurante <- my_row["Restaurante"]
  t_dia <-as.numeric(my_row["FinDe"])
  lambda <- as.numeric(my_row["Lambda"])
  miu <- as.numeric(my_row["Miu"])
  P0 <-as.numeric(my_row["P0"])
  
  # Generar probabilidades de los diferentes N
  propabilidad <- c()
  for (n in 0:10) {
    #definir n
    Pn = ((lambda/miu)^n)*P0
    propabilidad <- c(propabilidad, Pn)
  }
  propabilidad <- c(cc, restaurante, t_dia, propabilidad)
}

# Crear Tabla con probabilidad de todos los restaurantes
Probabilidades_de_N <- as_data_frame(t(apply(Caracteristicas ,1, Tabla_de_Probabilidades)))
# Renombrar las columnas y convertirlas en numericas
colnames(Probabilidades_de_N) <- c("CC", "Restaurante", "FinDe", 0:10)
Probabilidades_de_N <- Probabilidades_de_N %>% mutate_at(3:13, as.numeric) %>% 
  mutate_at("FinDe", as.factor)

# Lo escribo a un archivo que puede ser utilizado despues
write.csv(Probabilidades_de_N, file = "Resultados/Probabilidades de N.csv", row.names = FALSE)                            

# Funcion para formatear las variables con duración en minutos y colocarlo en terminos comprensibles
Formatear <- function(variable) {
  nueva <- period(hour=(variable%/%60), minute=(variable%%60%/%1), second=((variable%%1*60)%/%1))
  return(nueva)
}

# Generador de Cola para cada restaurante, ingresar la fila y los minutos, una hora por default
Generador_de_Cola <- function(my_row, n=60) {
  # Extraer datos de la fila
  cc <- my_row["CC"]
  restaurante <- my_row["Restaurante"]
  t_dia <-as.factor(my_row["FinDe"])
  miu <- as.numeric(my_row['Miu'])
  lambda <- as.numeric(my_row['Lambda'])
  
  #Inicializacion de parametros
  # Este vector consiste de 
  # Id|Tiempo Entre Llegadas|Llegadas|Servicio|Inicio|Tiempo en Cola|Tiempo en Servicio|Final|Tiempo en Sistema|
  ultimo_cliente<- c(0,0,0,0,0,0,0,0)
  historia_restaurante <- c(ultimo_cliente)
  
  # Crear clientes por cada minuto de la simulación
  clientes_por_minuto <- rpois(n, lambda)
  llegadas <- c()
  for (j in 1:length(clientes_por_minuto)) {
    # Se generan horas de llegadas de forma aleatoria entre cada minuto 
    horas <- runif(clientes_por_minuto[j], j-1, j) %>% round(digits=2) %>% sort()
    llegadas <- c(llegadas,horas)
  }
  
  # Por cada llegada creada calculo el resto de los datos para completar la simulación, cliente por cliente
  for(i in 1:length(llegadas)) {
    # Calculos realizado
    llegada <- llegadas[i]
    e_llegadas <- llegada-ultimo_cliente[3]
    servicio <- rexp(n = 1, rate = miu) %>% round(digits=2)
    inicio <- ifelse(llegada>ultimo_cliente[7],llegada,ultimo_cliente[7])
    cola <- (inicio-llegada) %>% round(2)
    final <- inicio + servicio
    en_sistema <- final-llegada
    
    # Reasigno las variables que van cambiando y agrego la fila a la historia
    cliente_nuevo <- c(i, e_llegadas, llegada, inicio, cola, servicio, final, en_sistema)
    historia_restaurante <- rbind(historia_restaurante, cliente_nuevo)
    ultimo_cliente <- cliente_nuevo
  }
  
  # Reformatear la tabla
  historia_restaurante <- historia_restaurante[(2:nrow(historia_restaurante)),]
  historia_restaurante <- cbind(cc, restaurante, t_dia, historia_restaurante) 
  
  colnames(historia_restaurante) <- c("CC", "Restaurante", "FinDe","id", "Entre_Llegadas", "Llegada", "Inicio", "Cola", "Servicio", "Final", "En_Sistema")
  historia_restaurante <- as_data_frame(historia_restaurante) 
  
  # Cambiar las variables de tiempo 
  historia_restaurante<- historia_restaurante %>%
    mutate_at(c("Entre_Llegadas", "Llegada", "Inicio", "Cola", "Servicio", "Final", "En_Sistema"), as.numeric)
  historia_restaurante<- historia_restaurante %>%
    mutate_at(c("Entre_Llegadas", "Llegada", "Inicio", "Cola", "Servicio", "Final", "En_Sistema"), ~Formatear(.))

  return(historia_restaurante)
} 

# Generar una lista con todas las simulaciones
Simulaciones <- apply(datos_a_usar, 1, Generador_de_Cola, n=168)


as_data_frame(Simulaciones[[19]]) %>% View()

