library(rstudioapi)
library(zoo)
library(stringr)
library(stringi)
library(readtext)

#script para calibrar parametros
main <- dirname(getActiveDocumentContext()$path)
source(file.path(main,'Q_Indicadores.R'))
#Realizar reglas de intercambio
reglas <- read.csv(file.path(main,'REGLAS_HRU.csv'))
ordenes <- sort(unique(reglas$ORDEN))
#para posterior analisis de caudal
nobs <- 11      #Cantidad de estaciones en el modelo
nsegment <- 13  #Cantidad de segmentos river

#llamar archivo parametros
par.file <- readLines(file.path(main,'input',"mhinarv.params"))
parlist <- read.csv(file.path(main,'PARAMETERS.csv'))
parlist$optimal <- c(NA)

for (k in 1:length(ordenes)){
  dbs <- which(reglas$ORDEN==ordenes[k])
  
for (unidad in dbs){#Datos del HRU
hru.level <- reglas$HRU[unidad]
obs.par <- reglas$OBS[unidad]
seg.par <- reglas$SEG[unidad]
est.par <- reglas$ESTAC[unidad]

#set to defaults
for (z in 1:nrow(parlist)){
  #parametro, HRU a modificar y variables
  param <- parlist[z,1]
  line.param <- which(par.file==param)
  val.line <- line.param+4+hru.level
  par.file[val.line] <- parlist$default[z]
  print(paste(param,'-',parlist$default[z]),quote=F)
  stri_write_lines(as.vector(par.file),file.path(main,'input',"mhinarv.params"),encoding = "UTF-8")
}

#prueba de concepto (para ver si todo esta ok)
ruta <- file.path(main)
ruta <- str_replace_all(ruta,"/","\\\\")
system("cmd.exe",input= paste("cd",ruta,"&& nogui.bat"))
#shell("cmd",file.path(main,"nogui.bat"))

#calibration process
for (iteracion in 1:3){
  for (z in 1:nrow(parlist)){
  #parametro, HRU a modificar y variables
  param <- parlist[z,1]
  min.val <- parlist$min[z]
  max.val <- parlist$max[z]
  delta.val <- (max.val - min.val)/19
  rank <- seq(min.val,max.val,delta.val)
  
  line.param <- which(par.file==param)
  val.line <- line.param+4+hru.level
  
  indicadores <- as.data.frame(matrix(0,ncol=3, nrow=length(rank)))
  colnames(indicadores) <- c("Parameter Value","Daily Ef","Monthly Ef")
  indicadores[,1] <- rank
  
  for (i in 1:length(rank)){
    value <- rank[i]
    cat("\f"); print(value); Sys.sleep((2))
    par.file[val.line] <- value
    stri_write_lines(as.vector(par.file),file.path(main,'input',"mhinarv.params"),encoding = "UTF-8")
    system("cmd.exe",input= paste("cd",ruta,"&& nogui.bat"))
    #shell("cmd","nogui.bat",intern = F)
    datos_salida <- read.table(file.path(main,'output','mhinarv.statvar'),skip=1+nobs+nsegment,sep=" ")
    donde_obs <- 7+nsegment+obs.par
    donde_sim <- 7+seg.par
    resumen <- datos_salida[,c(2,3,4,donde_sim,donde_obs)]
    colnames(resumen) <- c("year","month","day","sim","obs")
    resumen[resumen<0] <- NA
    resumen <- na.omit(resumen)
    resumen$qm <- substr(as.Date(paste(resumen$year,resumen$month,resumen$day,sep="-")),1,7)
    qd <- f.kge(x.sim=resumen$sim,x.obs=resumen$obs)
    
    am <- aggregate(resumen[,c(4,5)],list(resumen$qm),mean)
    qm <- f.kge(x.sim=am$sim,x.obs=am$obs)
    
    indicadores[i,2] <- qd
    indicadores[i,3] <- qm
    
  };{
    plot(c(1),col=NA,xlim=c(min.val, max.val),
         ylim=c(min(indicadores[,-1])-0.1,0.1+max(indicadores[,-1])),
         xlab=colnames(indicadores)[1],ylab="Indicator",main=paste("Indicator Variation:",param,"/ HRU",hru.level) )
    grid(lty=2,col="darkgrey");box(lwd=2)
    lines(indicadores$`Daily Ef`~indicadores$`Parameter Value`,lwd=2,col="red")
    lines(indicadores$`Monthly Ef`~indicadores$`Parameter Value`,lwd=2,col="blue")
  }
  
  VX <- indicadores[which(indicadores$`Monthly Ef`==max(indicadores$`Monthly Ef`))[1],1]
  parlist$optimal[z] <- VX
  par.file[val.line] <- VX
  print(paste0("Optimal value for ",param," in this iteration: ",VX),quote=F); Sys.sleep(1)
  stri_write_lines(as.vector(par.file),file.path(main,'input',"mhinarv.params"),encoding = "UTF-8")
  };{
    resumen$ts <- as.Date(paste0(resumen$qm,"-15"))
    plot(resumen$sim*(1/35.341)~resumen$ts,col=NA,xlab=NA,
         main=c("Caudales Mensuales HRU",paste(est.par,"/ HRU",hru.level)),
         ylab="[m3/s]",ylim=c(0,max(c(resumen$sim,resumen$obs)/35.341 )))
    lines(resumen$sim*(1/35.341)~resumen$ts,lwd=2,col="blue")
    lines(resumen$obs*(1/35.341)~resumen$ts,lwd=2,col="red",lty=2)
    legend("topright",fill=c("blue","red"),
           legend=c(paste0("EF.m: ",max(indicadores$`Monthly Ef`)),paste0("EF.d: ",max(indicadores$`Daily Ef`))))
    
    nsal <- paste0("QM_HRU_",hru.level,".png")
    png(file.path(main,nsal),width=1280,height=720,pointsize=16)
      plot(resumen$sim*(1/35.341)~resumen$ts,col=NA,xlab=NA,
           main=c("Caudales Mensuales HRU",paste(est.par,"/ HRU",hru.level)),
           ylab="[m3/s]",ylim=c(0,max(c(resumen$sim,resumen$obs)/35.341 )))
    lines(resumen$sim*(1/35.341)~resumen$ts,lwd=2,col="blue")
    lines(resumen$obs*(1/35.341)~resumen$ts,lwd=2,col="red",lty=2)
    legend("topright",fill("blue","red"),
           legend=c(paste0("EF.m: ",max(indicadores$`Monthly Ef`)),paste0("EF.d: ",max(indicadores$`Daily Ef`))))
    dev.off()
  }
}
}
}
