cadenita <- function(pro1, pro2,pro3,pro4,monas){
  #Laminas máximas
  N<-638
  
  probDesanimarse<-function(i){
    if(0<=i & i<162){
      prob<-pro1
    }
    if(162<=i & i<324){
      prob <- pro2
    }
    if(324<=i & i<486){
      prob <- pro3
    }
    if(486<=i & i<=638){
      prob <- pro4
    }
    return(prob)
  }
  
  Sx <- c(5:N)
  Sy <- c("A","D")
  Sz <- expand.grid(Sx,Sy)
  
  estados <- paste(Sz$Var1,Sz$Var2,sep=",")
  # Matriz P
  matP <- matrix(0,nrow=length(estados),ncol=length(estados))
  dimnames(matP)<-list(estados,estados)
  # recorro para las filas 
  for(filas in estados){
    # extraemos la información de i, j 
    i = as.numeric(strsplit(filas, ",")[[1]][1])
    j = (strsplit(filas, ",")[[1]][2])
    
    # recorro para las columnas
    for(columnas in estados){
      
      # extraemos la información de i_, j_
      i_ = as.numeric(strsplit(columnas, ",")[[1]][1])
      j_= (strsplit(columnas, ",")[[1]][2])
      
      # Caso 1: que pueda pegar hasta 5 láminas (i_-i es máximo 5) y se desanime
      if(j_=="D" & i<=i_ & i_<=i+5 & i_<N & j=="A"){
        matP[filas,columnas]<-dbinom(i_-i,5,prob = (N-i)/N)*probDesanimarse(i)
      }
      # Caso 2: que pueda pegar hasta 4 láminas y se desanime 
      # --> que llene el album y se desanime
      if(i_==N & j_=="D" & N-4<=i & i<=N-1 & j=="A"){
        matP[filas,columnas]<-(1-pbinom(i_-i-1,5,prob = (N-i)/N))*probDesanimarse(i)
      }
      # Caso 3: que pueda pegar hasta 5 láminas (i_-i es máximo 5) y no se desanime
      if(j_=="A" & i<=i_ & i_<=i+5  & i_<N & j=="A"){
        matP[filas,columnas]<- dbinom(i_-i,5,prob = (N-i)/N)*(1-probDesanimarse(i))
      }
      # Caso 4: que pueda pegar hasta 4 láminas y no se desanime 
      # --> que llene el album y no se desanime
      if(i_==N & j_=="A" & N-4<=i & i<=N-1 &  j=="A"){
        matP[filas,columnas]<-(1-pbinom(i_-i-1,5,prob = (N-i)/N))*(1-probDesanimarse(i))
      }
      
      
      # Casos Absorbentes
      # Caso 5: que llene el album
      if(i_==i & j_==j & i==N){
        matP[filas,columnas]<-1
      }
      # Caso 6: que se desanime
      if(i_==i & j_==j & j=="D" & i<N){
        matP[filas,columnas]<-1
      }
    }
  }
  
  cadena <- new(Class="markovchain", states=estados, transitionMatrix=matP)
  
  transitorios <- transientStates(cadena)
  absorbentes <- absorbingStates(cadena)
  
  I <- diag(length(transitorios))
  Q <- matP[transitorios,transitorios]
  R <- matP[transitorios,absorbentes]
probsAbsorcion <- solve(I-Q)%*%R  

proba <- probsAbsorcion[paste(monas,"A",sep=","),]

rango1 <- 0; rango2 <- 0; rango3 <- 0; rango4 <- 0
for(filas in absorbentes){
  i = as.numeric(strsplit(filas, ",")[[1]][1])
  j = (strsplit(filas, ",")[[1]][2])
  
  if(j=="D"  & i>=5 & i<=161){
    rango1 <- rango1 +proba[filas]
  }
  if(j=="D"  & i>=162 & i<=323){
    rango2 <- rango2 +proba[filas]
  }
  if(j=="D" & i>=324 & i<=485){
    rango3 <- rango3 +proba[filas]
  }
  if(j=="D" & i>=486 & i<=638){
    rango4 <- rango4 +proba[filas]
  }
  
  
}
probabilidades <- round(c(rango1,rango2,rango3,rango4)*100,2)
lista <- list()
df <- data.frame(rangos=c("[0,161]","[162,323]","[324,485]","[486,638]"),
         probabilidad=probabilidades)
lista[[1]] <- df
tiemposAbs <- solve(I-Q)
numSobres <- round(sum(tiemposAbs[paste(monas,"A",sep=","),]))+1
costoTotal <- numSobres*3500+16000
lista[[2]] <- costoTotal
probTerminar <- proba["638,A"]+proba["638,D"]
lista[[3]] <- probTerminar

return(lista)
}

