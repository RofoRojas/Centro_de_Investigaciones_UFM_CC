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
  
  Caracteristicas<- c(cc, restaurante, t_dia, P0, Lq, L, Wq, W, Pw)
  return(Caracteristicas)
}









