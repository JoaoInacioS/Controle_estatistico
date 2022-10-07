g_controle<-function(vari,lsc,lic,lm,labx="Amostras",laby="Valor"){
  n<-1:length(vari)
  dados<-data.frame(vari,n,lsc,lic,lm)
  dados<-dados |> 
    mutate(fora=case_when(
      vari >= lsc | vari<= lic ~ vari
    ))
  ggplot(dados) +
    geom_line(aes(x = n, y = lsc),size = 0.6, colour = "black",alpha = 0.5) +
    geom_line(aes(x = n, y = lic),size = 0.6, colour = "black",alpha = 0.5) +
    geom_line(aes(x = n, y = lm),size = 0.4, colour = "black",alpha = 0.5) +
    geom_line(aes(x = n, y = vari),size = 0.5, colour = "black")+
    geom_point(aes(x = n, y = vari),shape = "circle", size = 1.5L,colour = "black") +
    geom_point(aes(x = n, y = fora),shape = "circle", size = 1.5L,colour = "red") +
    labs(x = labx, y = laby) +
    theme_minimal()
}
g_controle(vari,lsc,lic,lm)




g_controle_compl<-function(dados,cal=c("media","ampli","desvio"),met=c("ampli","desvio"),labx="Amostras",laby="Valor"){
  # "Dados" entra um data frame.
  dados1<-as.matrix(dados)
  x1<-t(dados1)
  x2<-colMeans(x1) # Media das colunas(amostra)
  x3<-mean(x2) # Media da media das colunas(amostra)
  n<-length(dados1[,1]) # Para os limites
  k<-length(dados1[,1]) # Para as amostras
  # # calculo para a amplitude em cada amostra: ------------------------------------
  # p<-0
  # for (i in 1:k) {
  #   p[i]<-diff(range(x1[,i]))  
  # }
  # p1<-mean(p) # Media das amplitudes de cada amostra
  
  if(met=="desvio"){
    v<-0
    for (i in 1:k){
      v[i]<-sqrt(var(x1[,i]))
    }
    v1<-mean(v) # Media dos desvios de cada amostra
    if(cal=="media"){
      c4<-(4*(n-1))/((4*n)-3)
      A_3<-3/(c4*sqrt(n))
      lsc<-x3+(A_3*v1)
      lic<-x3-(A_3*v1)
      lm<-x3
      vari<-x2
    }
    if(cal=="desvio"){
      d4<-(4*(n-1))/((4*n)-3)
      B_3<-round(1-(3/(d4*sqrt(2*(n-1)))),3)
      B_4<-1+(3/(d4*sqrt(2*(n-1))))
      lsc<-B_4*v1
      lic<-B_3*v1
      lm<-v1
      vari<-v
    }
  }  
  cat("LSC:",round(lsc,3),"|",
      "LIC:",round(lic,3),"|",
      "LM:",round(lm,3),"
")
  n1<-1:k
  graf<-data.frame(vari,n1,lsc,lic,lm)
  graf1<-graf |> 
    mutate(fora=case_when(
      vari >= lsc | vari<= lic ~ vari
    ))
  ggplot(graf1) +
    geom_line(aes(x = n1, y = lsc),size = 0.6, colour = "black",alpha = 0.5) +
    geom_line(aes(x = n1, y = lic),size = 0.6, colour = "black",alpha = 0.5) +
    geom_line(aes(x = n1, y = lm),size = 0.4, colour = "black",alpha = 0.5) +
    geom_line(aes(x = n1, y = vari),size = 0.5, colour = "black")+
    geom_point(aes(x = n1, y = vari),shape = "circle", size = 1.5L,colour = "black") +
    geom_point(aes(x = n1, y = fora),shape = "circle", size = 1.5L,colour = "red") +
    labs(x = labx, y = laby) +
    theme_minimal()
}
g_controle_compl(dados = dados,cal = "desvio",met = "desvio")
