dados<- readxl::read_excel("GC - variáveis parte 1.xlsx")[,-1]
dados1<-as.matrix(dados)
x1<-t(dados1)

x2<-colMeans(x1) # Media das colunas(amostra)
x3<-mean(x2) # Media da media das colunas(amostra)

n<-length(dados1[,1])
# calculo para a amplitude em cada amostra: ------------------------------------
p<-0
for (i in 1:n) {
  p[i]<-diff(range(x1[,i]))  
}
p # Amplitude de cada amostra
p1<-mean(p) # Media das amplitudes de cada amostra


# Calculo para o desvio em cada amostra: ---------------------------------------
v<-0
for (i in 1:n) {
  v[i]<-sqrt(var(x1[,i]))
}
v # Desvio padrao de cada amostra
v1<-mean(v) # Media dos desvios de cada amostra

# Graficos para amplitude (P): -------------------------------------------------

# Calculos dos limites estão no caderno

## Media ----
A_2<-0.153
lsc<-x3+(A_2*p1)
lic<-x3-(A_2*p1)
plot(x2,ylim = c(lic-0.1,lsc+0.1)) # plot medias das amostras
a<-rep(lsc,n) # Limite superior (LSC)  rep(LSC,n)
b<-rep(lic,n) # Limite inferior (LIC)  rep(LIC,n)
lines(a,col=2) # plot da linha LSC (col=2 <=> cor vermelha)
lines(b,col=2) # plot da linha LIC (col=2 <=> cor vermelha)

## Desvio padrão ----
D4<-1.541
D3<-0.459
lsc<-D4*p1
lic<-D3*p1
plot(p,ylim = c(lic-0.1,lsc+0.1)) # plot amplitude das amostras
a1<-rep(lsc,n) # Limite superior (LSC)  rep(LSC,n)
b1<-rep(lic,n) # Limite inferior (LIC)  rep(LIC,n)
lines(a1,col=2) # plot da linha LSC (col=2 <=> cor vermelha)
lines(b1,col=2) # plot da linha LIC (col=2 <=> cor vermelha)

# Graficos dos limites para o desvio padrao (s): -------------------------------

## Media ----
A_3<-0.606
lsc<-x3+(A_3*v1)
lic<-x3-(A_3*v1)
plot(x2,ylim = c(lic-0.1,lsc+0.1)) # plot medias das amostras 
a2<-rep(lsc,n) # Limite superior (LSC)  rep(LSC,n)
b2<-rep(lic,n) # Limite inferior (LIC)  rep(LIC,n)
lines(a2,col=3) # plot da linha LSC (col=3 <=> cor verde) (limite p/ s)
lines(b2,col=3) # plot da linha LIC (col=3 <=> cor verde) (limite p/ s)
#====
lines(a,col=2) # plot da linha LSC (col=2 <=> cor vermelha) (limite p/ P)
lines(b,col=2) # plot da linha LIC (col=2 <=> cor vermelha) (limite p/ P)

## Desvio padrão ----

B_4<-1.435
B_3<-0.565
lsc<-B_4*v1
lic<-B_3*v1
plot(v,ylim = c(lic-0.1,lsc+0.1)) # plot desvio padrao das amostras
a3<-rep(lsc,n) # Limite superior (LSC)  rep(LSC,n)
b3<-rep(lic,n) # Limite inferior (LIC)  rep(LIC,n)
lines(a3,col=2) # plot da linha LSC (col=2 <=> cor vermelha)
lines(b3,col=2) # plot da linha LIC (col=2 <=> cor vermelha)

