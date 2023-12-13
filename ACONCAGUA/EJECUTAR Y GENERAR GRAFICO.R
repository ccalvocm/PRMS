gc(); graphics.off(); cat("\f")

library(rstudioapi)
library(hydroGOF)
library(stringr)
library(zoo)
library(stats)

rm(list=ls())
mainDir <- dirname(getActiveDocumentContext()$path)

#nombre del proyecto

proyecto <- "putaendo"
out_prms_file <- paste0(proyecto,".statvar")

#llamar al cmd y que ejecute el modo NoGUI
{
  shell("cmd","nogui.bat")

  {
  #revisar el archivo de resultados
  datos_salida <- file.path(mainDir,'output',out_prms_file)
  tabla_salida <- read.table(datos_salida,sep=" ",skip=4)
  resumen <- tabla_salida[,c(2,3,4,8,9,10)]
  colnames(resumen) <- c("anho","mes","dia","obs","sim","snowdepth")
  resumen[resumen==-999] <- NA
  resumen$date <- as.Date(paste0(resumen$anho,"-",resumen$mes,"-",resumen$dia))
  resumen$tsm <- substr(resumen$date,1,7)
  datos_nieve <- data.frame(resumen$tsm,resumen$snowdepth)
  datos_nieve <- aggregate(datos_nieve[,2], list(datos_nieve[,1]),mean)
  resumen <- na.omit(resumen)
  }

  {
  #comparativas a nivel diario:
  tabla_dia <- resumen[,c(7,4,5)]
  tabla_dia[,-1] <- tabla_dia[,-1]/35.341 #conversion de ft3/s a m3/s
  indicadores_daily <- gof(tabla_dia$sim,tabla_dia$obs)
  qmax_daily <- max(tabla_dia[,-1])
  
  #Generar figura
  plot(tabla_dia$obs~tabla_dia$date,col=NA,xlab=NA,las=2,ylab="[m3/s]",
       main="Caudales Medios Diarios 1990-2010",ylim=c(0,qmax_daily))
  grid(nx=NA,ny=NULL,col="black",lty=2)
  lines(tabla_dia$obs~tabla_dia$date,col="blue",lwd=2)
  lines(tabla_dia$sim~tabla_dia$date,col="red",lwd=2,lty=3)
  legend("topright",lwd=2,col=c("blue","red"),legend=c("Obs","Sim"))
  
  png(file.path(mainDir,"Caudales_Medios_Diarios.png"),width=1280,height=720,pointsize = 16)
  plot(tabla_dia$obs~tabla_dia$date,col=NA,xlab=NA,las=2,ylab="[m3/s]",
       main="Caudales Medios Diarios 1990-2010",ylim=c(0,qmax_daily))
  grid(nx=NA,ny=NULL,col="black",lty=2)
  lines(tabla_dia$obs~tabla_dia$date,col="blue",lwd=2)
  lines(tabla_dia$sim~tabla_dia$date,col="red",lwd=2,lty=3)
  legend("topright",lwd=2,col=c("blue","red"),legend=c("Obs","Sim"))
  dev.off()
  
  
  }

  {
  #comparativas a nivel mensual
  tabla_mes <- resumen[,c(8,4,5)]
  tabla_mes[,-1] <- tabla_mes[,-1]/35.341 #conversion de ft3/s a m3/s
  tabla_mes <- aggregate(tabla_mes[,-1],list(tabla_mes[,1]),mean)
  tabla_mes[,1] <- as.Date(paste0(tabla_mes[,1],"-15"))
  colnames(tabla_mes)[1] <- "tsm"
  indicadores_monthly <- gof(sim=tabla_mes$sim,obs=tabla_mes$obs)
  qmax_monthly <- max(tabla_mes[,-1])
  
  #generar figura
  plot(tabla_mes$obs~tabla_mes$tsm,col=NA,xlab=NA,las=2,ylab="[m3/s]",
       main="Caudales Medios Mensuales 1990-2010",ylim=c(0,qmax_monthly))
  grid(nx=NA,ny=NULL,col="black",lty=2)
  lines(tabla_mes$obs~tabla_mes$tsm,col="blue",lwd=2)
  lines(tabla_mes$sim~tabla_mes$tsm,col="red",lwd=2,lty=2)
  
  png(file.path(mainDir,"Caudales_Medios_Mensuales.png"),width=1280,height=720,pointsize = 16)
  plot(tabla_mes$obs~tabla_mes$tsm,col=NA,xlab=NA,las=2,ylab="[m3/s]",
       main="Caudales Medios Mensuales 1990-2010",ylim=c(0,qmax_monthly))
  grid(nx=NA,ny=NULL,col="black",lty=2)
  lines(tabla_mes$obs~tabla_mes$tsm,col="blue",lwd=2)
  lines(tabla_mes$sim~tabla_mes$tsm,col="red",lwd=2,lty=2)
  legend("topright",lwd=2,col=c("blue","red"),legend=c("Obs","Sim"))
  dev.off()
  
  }
  
  {#Analisis estacional
    datos_est <- as.data.frame(matrix(0,ncol=7,nrow=12))
    colnames(datos_est) <- c("mes","o15","o50","o85",
                             "s15","s50","s85")
    tabla_mes$mes <- as.numeric(substr(tabla_mes$tsm,6,7))
    for (k in 1:12){
      tms <- subset(tabla_mes,tabla_mes$mes==k)
      datos_est[k,1] <- k
      datos_est$o15[k] <- quantile(tms$obs,0.15)
      datos_est$o50[k] <- quantile(tms$obs,0.50)
      datos_est$o85[k] <- quantile(tms$obs,0.85)
      datos_est$s15[k] <- quantile(tms$sim,0.15)
      datos_est$s50[k] <- quantile(tms$sim,0.50)
      datos_est$s85[k] <- quantile(tms$sim,0.85)
    }
    
    qmax <- max(datos_est[,-1])
    
    #seccion de nieve
    datos_nieve[,2] <- datos_nieve[,2]*2.54 #pasar de in a cm
    datos_nieve$mes <- as.numeric(substr(datos_nieve[,1],6,7))
    snm <- aggregate(datos_nieve[,2],list(datos_nieve$mes),mean)
    
    par(mar=c(5.1,4.1,4.1,4.1))
    plot(1,col=NA,xlim=c(1,12),ylim=c(0,qmax),ylab="[m3/s]",
         xaxt='n',xlab="mes calendario",main=c("Analisis Estacional p15-p50-p85",substr(Sys.time(),1,19)) )
    axis(1, at=seq(1:12)); grid(nx=NA,ny=NULL,col="black",lty=2)
    polygon(c(seq(1:12),rev(seq(1:12))),
            c(datos_est$o85,rev(datos_est$o15)),
            lty=0,col=rgb(0,0,1,alpha=0.4))
    polygon(c(seq(1:12),rev(seq(1:12))),
            c(datos_est$s85,rev(datos_est$s15)),
            lty=0,col=rgb(1,0,0,alpha=0.4))
    lines(datos_est$o50~seq(1:12),col="blue",lwd=2)
    lines(datos_est$s50~seq(1:12),col="red",lwd=2,lty=1)
    legend("topleft",lwd=2,col=c("blue","red"),legend=c("Obs","Sim"))
    par(new=T)
    plot(1,col=NA,xlim=c(1,12),ylim=c(0,1.1*max(snm[,2])),axes=F,main=NA,xlab=NA,ylab=NA)
    axis(side=4); mtext("Espesor medio nieve [cm]", side = 4, line = 3,col="darkgreen")
    lines(snm$x~seq(1:12),lwd=2,col="darkgreen",lty=2)
    
    png(file.path(mainDir,"Analisis_estacional.png"),width=1280,height=720,pointsize = 16)
    par(mar=c(5.1,4.1,4.1,4.1))
    plot(1,col=NA,xlim=c(1,12),ylim=c(0,qmax),ylab="[m3/s]",
         xaxt='n',xlab="mes calendario",main=c("Analisis Estacional p15-p50-p85",substr(Sys.time(),1,19)) )
    axis(1, at=seq(1:12)); grid(nx=NA,ny=NULL,col="black",lty=2)
    polygon(c(seq(1:12),rev(seq(1:12))),
            c(datos_est$o85,rev(datos_est$o15)),
            lty=0,col=rgb(0,0,1,alpha=0.4))
    polygon(c(seq(1:12),rev(seq(1:12))),
            c(datos_est$s85,rev(datos_est$s15)),
            lty=0,col=rgb(1,0,0,alpha=0.4))
    lines(datos_est$o50~seq(1:12),col="blue",lwd=2)
    lines(datos_est$s50~seq(1:12),col="red",lwd=2,lty=1)
    legend("topleft",lwd=2,col=c("blue","red"),legend=c("Obs","Sim"))
    par(new=T)
    plot(1,col=NA,xlim=c(1,12),ylim=c(0,1.1*max(snm[,2])),axes=F,main=NA,xlab=NA,ylab=NA)
    axis(side=4); mtext("Espesor medio nieve [cm]", side = 4, line = 3,col="darkgreen")
    lines(snm$x~seq(1:12),lwd=2,col="darkgreen",lty=2)
    dev.off()
    
  }
  
  print(c("=== Indicadores de Eficiencia Modelacion ===",""),quote=F)
  print(" ")
  
  cres <- as.data.frame(cbind(indicadores_daily,indicadores_monthly))
  colnames(cres) <- c("diario","mensual")
  cres <- cres[c(6,9,17,18,19),]
  print(cres)
}
