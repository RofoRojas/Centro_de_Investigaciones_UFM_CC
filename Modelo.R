library(readxl)
library(dplyr)
library(lubridate)

## Ingresar a todos los Centros Comerciales
for (folder in list.files("Data")) {
  ## Ingresar a todos los Restaurantes
  for (archivo in list.files(paste("Data", folder, sep ="/" ))) {
    ## Ingresar a todos los dias
    for (sheet in 1:length(excel_sheets(paste("Data", folder, archivo, sep = "/")))) {
      
      temp_table <- read_excel(path = paste("Data", folder, archivo, sep = "/"), sheet = sheet, skip = 3) %>% 
        mutate(Llegada_Anterior= lag(Llegada,1)) %>% 
        mutate(T_Llegadas=Llegada-Llegada_Anterior, T_Cola= Inicio-Llegada, T_Servicio=Final-Inicio, T_Sistema= Final-Llegada) 
      
      datos_utiles <- temp_table  %>% 
        summarise(CC= folder, Rest= substr(archivo, 1, nchar(archivo)-5), Fecha= date(Llegada[1]), Cantidad= n(), 
                  Tiempo= Llegada[nrow(temp_table)]-Llegada[1], Lambda= mean(T_Llegadas, na.rm = T), 
                  Miu= mean(T_Servicio), T_Cola= mean(T_Cola), T_Sistema=mean(T_Sistema))
      
      if (exists("datos_finales")) {
        datos_finales <-rbind(datos_finales, datos_utiles)
      } else {
        datos_finales <- datos_utiles
      }
    }
  }
}





str(datos_utiles) 
temp_table
str(temp_table)
#  Ingreso | Llegada | Inicio | Final

temp_table <- read_excel(path = paste("Data", folder, archivo, sep = "/"), sheet = sheet, skip = 3) %>% 
  mutate(Llegada_Anterior= lag(Llegada,1)) %>% 
  mutate(T_Llegadas=Llegada-Llegada_Anterior, T_Cola= Inicio-Llegada, T_Servicio= Final-Inicio , T_Sistema= Final-Llegada) 

datos_utiles <- temp_table  %>% 
  summarise(CC= folder, Rest= substr(archivo, 1, nchar(archivo)-5), Fecha= date(Llegada[1]), Cantidad= n(), 
            Tiempo= Llegada[nrow(temp_table)]-Llegada[1], Lambda= mean(T_Llegadas, na.rm = T), 
            Miu= mean(T_Servicio), T_Cola= mean(T_Cola), T_Sistema=mean(T_Sistema))


folder <- "MIRA"
archivo <- "KFC.xlsx"
  