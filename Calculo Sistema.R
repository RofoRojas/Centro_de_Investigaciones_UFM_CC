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

