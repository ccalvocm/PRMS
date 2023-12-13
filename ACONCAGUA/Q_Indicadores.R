# el objetivo de este script es poder simplificar lineas
# debido a que el paquete hydroGOF esta por expirar.
# este script sólo considera los indicadores KGE, NSE,
# r2 y BR2

# Septiembre 2023, Jose Martinez Retamal.

f.r2 <- function(x.sim,x.obs){
  #' @title Indicador de correlacion de Pearson cuadrado
  #' @description Entrega el valor de r2
  #' @param x.sim Valores simulados - Dataframe, numerico
  #' @param x.obs Valores observados - Dataframe, numerico
  #' @return Valor numerico

  z.obs <- as.data.frame(as.numeric(na.omit(x.obs)))
  z.sim <- as.data.frame(as.numeric(na.omit(x.sim)))
  
  lobs <- nrow(z.obs)
  lsim <- nrow(z.sim)
  
  if (lobs!=lsim){
    print("ERROR!! - Series simuladas y observadas poseen distintas dimensiones",quote=F)
  } else {
    ans <- cor(x=z.obs,y=z.sim)
    ans <- ans^2
  }
  
  res <- round(ans,digits=3)
  return(as.numeric(res))
}

f.br2 <- function(x.sim,x.obs){
  #' @title Indicador de correlacion de Pearson disminuido
  #' @description Entrega el valor de br2
  #' @param x.sim Valores simulados - Dataframe, numerico
  #' @param x.obs Valores observados - Dataframe, numerico
  #' @return Valor numerico
  
  z.obs <- as.data.frame(as.numeric(na.omit(x.obs)))
  z.sim <- as.data.frame(as.numeric(na.omit(x.sim)))
  
  m.obs <- mean(z.obs[,1])
  m.sim <- mean(z.sim[,1])
  
  lobs <- nrow(z.obs)
  lsim <- nrow(z.sim)
  
  if (lobs!=lsim){
    print("ERROR!! - Series simuladas y observadas poseen distintas dimensiones",quote=F)
  } else {
    ans <- cor(x=z.obs,y=z.sim)
    ans <- ans^2
  }
  
  fac <- min(c(m.obs/m.sim, m.sim/m.obs))
  
  res <- round(fac*ans,digits=3)
  return(as.numeric(res))
}

f.kge <- function(x.sim,x.obs){
  #' @title Indicador de Eficiencia de Kling-Gupta
  #' @description Entrega el valor KGE
  #' @param x.sim Valores simulados - Dataframe, numerico
  #' @param x.obs Valores observados - Dataframe, numerico
  #' @return Valor numerico
  
  z.obs <- as.data.frame(as.numeric(na.omit(x.obs)))
  z.sim <- as.data.frame(as.numeric(na.omit(x.sim)))
  
  lobs <- nrow(z.obs)
  lsim <- nrow(z.sim)
  
  m.obs <- mean(z.obs[,1])
  m.sim <- mean(z.sim[,1])
  
  s.obs <- sd(z.obs[,1])
  s.sim <- sd(z.sim[,1])
  
  if (lobs!=lsim){
    print("ERROR!! - Series simuladas y observadas poseen distintas dimensiones",quote=F)
  } else {
    Q1 <- cor(x=z.obs,y=z.sim)
    Q1 <- (Q1^2 -1)^2
    Q2 <- ((s.sim/s.obs)-1)^2
    Q3 <- ((m.sim/m.obs)-1)^2
    Q4 <- sqrt(Q1 + Q2 + Q3) 
  }
  
  res <- round(1-Q4,digits=3)
  return(as.numeric(res))
}

f.nse <- function(x.sim,x.obs){
  #' @title Indicador de Eficiencia de Nash-Sutcliffe
  #' @description Entrega el valor NSE
  #' @param x.sim Valores simulados - Dataframe, numerico
  #' @param x.obs Valores observados - Dataframe, numerico
  #' @return Valor numerico
  
  z.obs <- as.data.frame(as.numeric(na.omit(x.obs)))
  z.sim <- as.data.frame(as.numeric(na.omit(x.sim)))
  
  lobs <- nrow(z.obs)
  lsim <- nrow(z.sim)
  
  m.obs <- mean(z.obs[,1])
  m.sim <- mean(z.sim[,1])
  
  s.obs <- sd(z.obs[,1])
  s.sim <- sd(z.sim[,1])
  
  if (lobs!=lsim){
    print("ERROR!! - Series simuladas y observadas poseen distintas dimensiones",quote=F)
  } else {
    Q1 = 0
    Q2 = 0
    for (i in 1:lobs){
      Q1 = Q1 + (z.obs[i,1] - z.sim[i,1])^2
      Q2 = Q2 + (z.obs[i,1] - m.obs)^2
    }
    Q3 <- Q1/Q2
  }
  
  res <- round(1-Q3,digits=3)
  return(as.numeric(res))
}
