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

