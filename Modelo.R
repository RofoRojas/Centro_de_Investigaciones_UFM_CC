library(readxl)
library(dplyr)
library(lubridate)

## Ingresar a todos los Centros Comerciales
for (folder in list.files("Data")) {
  ## Ingresar a todos los Restaurantes
  for (archivo in list.files(paste("Data", folder, sep ="/" ))) {
    ## Ingresar a todos los dias
    for (sheet in 1:length(excel_sheets(paste("Data", folder, archivo, sep = "/")))) {
      
      print(paste("Leyendo: Data/", folder,'/', archivo,':sheet',sheet, sep = ""))
      temp_table <- read_excel(path = paste("Data", folder, archivo, sep = "/"), sheet = sheet, skip = 3,
                               range = cell_limits(c(4, NA), c(NA, 4)),col_types = c("numeric", "date", "date", "date"))
      colnames(temp_table) = c('Ingreso', 'Llegada', 'Inicio', 'Final')
      
      temp_table<- temp_table  %>% filter(complete.cases(temp_table)) %>% mutate(Llegada_Anterior= lag(Llegada,1)) %>% 
        mutate(T_Llegadas=as.duration(Llegada-Llegada_Anterior), T_Cola= as.duration(Inicio-Llegada), 
               T_Servicio=as.duration(Final-Inicio), T_Sistema= as.duration(Final-Llegada)) %>% 
        # Transformar a minutos
        mutate(T_Llegadas=T_Llegadas/dminutes(1), T_Cola= T_Cola/dminutes(1), T_Servicio=T_Servicio/dminutes(1), T_Sistema= T_Sistema/dminutes(1))
      
      datos_utiles <- temp_table  %>% 
        summarise(CC= folder, Rest= substr(archivo, 6, nchar(archivo)-5), Fecha= date(Llegada[1]),Cantidad= n(),
                  Intervalo= (Llegada[1]%--%Llegada[nrow(temp_table)]), 
                  Lambda= mean(T_Llegadas, na.rm = T), Miu= mean(T_Servicio), Sd_Miu= sd(T_Servicio),
                  Min_T_Llegadas=min(T_Llegadas, na.rm = T), Max_T_Llegadas= max(T_Llegadas, na.rm = T),
                  
                  T_Cola= mean(T_Cola), T_Sistema=mean(T_Sistema)) %>% 
        mutate(D_Semana=wday(Fecha,week_start = 1)) %>% mutate(FinDe = ifelse(D_Semana>=6,1,0)) %>% 
        mutate(Tiempo_Tot= as.duration(Intervalo)/dhours(1))
      
      
      if (exists("datos_iniciales")) {
        datos_iniciales <-rbind(datos_iniciales, datos_utiles)
      } else {
        datos_iniciales <- datos_utiles
      }
    }
  }
}

rm(datos_iniciales)




datos_a_usar <- datos_iniciales %>% filter(CC=="Miraflores", Rest=="BurgerKing") %>% 
  mutate(LambdaS=(Cantidad*Lambda), MiuS=(Cantidad*Miu), Var_Miu= (Sd_Miu^2)) %>% 
  group_by(CC, Rest, FinDe) %>% summarise(Cantidad = sum(Cantidad), Lambda= sum(LambdaS)/sum(Cantidad),
                                          Miu= sum(MiuS)/sum(Cantidad), Sd_Miu=sqrt(sum(Var_Miu)), 
                                          Min_T_Llegadas= min(Min_T_Llegadas), Max_T_Llegadas=max(Max_T_Llegadas), 
                                          Tiempo_Tot=sum(Tiempo_Tot))





str(datos_utiles) 
temp_table
str(temp_table)
#  Ingreso | Llegada | Inicio | Final


generar_datos_utiles <- function(folder, archivo, sheet) {
  print(paste("Leyendo: Data/", folder,'/', archivo,':sheet',sheet, sep = ""))
  temp_table <- read_excel(path = paste("Data", folder, archivo, sep = "/"), sheet = sheet, skip = 3,
                           range = cell_limits(c(4, NA), c(NA, 4)),col_types = c("numeric", "date", "date", "date"))
  colnames(temp_table) = c('Ingreso', 'Llegada', 'Inicio', 'Final')
  
  temp_table<- temp_table  %>% filter(complete.cases(temp_table)) %>%  mutate(Llegada_Anterior= lag(Llegada,1)) %>% 
    mutate(T_Llegadas=as.duration(Llegada-Llegada_Anterior), T_Cola= as.duration(Inicio-Llegada), 
           T_Servicio=as.duration(Final-Inicio), T_Sistema= as.duration(Final-Llegada)) %>% 
    # Transformar a minutos
    mutate(T_Llegadas=T_Llegadas/dminutes(1), T_Cola= T_Cola/dminutes(1), T_Servicio=T_Servicio/dminutes(1), T_Sistema= T_Sistema/dminutes(1))
  
  datos_utiles <- temp_table  %>% 
    summarise(CC= folder, Rest= substr(archivo, 6, nchar(archivo)-5), Fecha= date(Llegada[1]),Cantidad= n(),
              Intervalo= (Llegada[1]%--%Llegada[nrow(temp_table)]), 
              Lambda= mean(T_Llegadas, na.rm = T), Miu= mean(T_Servicio), Sd_Miu= sd(T_Servicio),
              Min_T_Llegadas=min(T_Llegadas, na.rm = T), Max_T_Llegadas= max(T_Llegadas, na.rm = T),
              
              T_Cola= mean(T_Cola), T_Sistema=mean(T_Sistema)) %>% 
    mutate(D_Semana=wday(Fecha,week_start = 1)) %>% mutate(FinDe = ifelse(D_Semana>=6,1,0)) %>% 
    mutate(Tiempo_Tot= as.duration(Intervalo)/dhours(1))
  
  
  return(datos_utiles)
}

generar_datos_utiles("Pradera Z10", "DatosTacoBell.xlsx",  3)





