rm(list=ls())
yperceptron<-function(xvec,w,par)

{
  if (par==1) 
    xvec<-cbind(-1,xvec) 
  u<-xvec %*% w
  y<-1.0*((u>=0))
  return ((as.matrix(y)))
}

trainperceptron<-function(xin,yd,eta,tol,maxepocas,par)
  
{
  dimxin<-dim(xin)
  N<-dimxin[1]
  n<-dimxin[2]
  if(par==1){
    wt<-as.matrix(runif(n+1)-0.5)
    xin<-cbind(-1,xin)
  }else wt<-as.matrix(runif(n)-0.5)
  nepocas<-0
  eepoca<-tol+1
  
  #vetor de erro
  evec<-matrix(nrow=1,ncol=maxepocas)
  while((nepocas < maxepocas) && (eepoca > tol))
  {
    ei2<-0
    xseq<-sample(N)
    for(i in 1:N)
    {
      irand<-xseq[i]
      yhati<-1.0*((xin[irand,] %*% wt)>=0)
      ei<-yd[irand]-yhati
      dw<-eta*ei*xin[irand,]
      wt<-wt+dw
      ei2<-ei2+ei*ei
    }
    nepocas<-nepocas+1
    evec[nepocas]<-ei2/N
    eepoca<-evec[nepocas]
  }
  retlist<-list(wt,evec[1:nepocas])
  return(retlist)
}


data(iris)
ntrain<-10

xc1<-iris[1:50,1:4] 
xc2<-iris[76:125,1:4] 

maxteste<-20

erros<-matrix(0,nrow=maxteste,ncol=100)


ytt<-matrix(0,nrow=100-(ntrain*2),ncol=maxteste)

wtt<-matrix(0,nrow=5,ncol=maxteste)

for(t in 1:maxteste){
 
  seqc1<-sample(50)

  xc1treina<-xc1[seqc1[1:ntrain],]
 
  yc1treina<-matrix(0,nrow=ntrain)
  
  seqc2<-sample(50)
  xc2treina<-xc2[seqc2[1:ntrain],]
  yc2treina<-matrix(1,nrow=ntrain)

  xc1teste<-xc1[seqc1[(ntrain+1):50],]
  yc1teste<-matrix(0,nrow=(50-ntrain))
 
  xc2teste<-xc2[seqc2[(ntrain+1):50],]
  yc2teste<-matrix(1,nrow=(50-ntrain))
  
  xin<-as.matrix(rbind(xc1treina,xc2treina))
  yd<-rbind(yc1treina,yc2treina)
  
  xinteste<-as.matrix(rbind(xc1teste,xc2teste))
  yteste<-rbind(yc1teste,yc2teste)
  
  retlist<-trainperceptron(xin,yd,0.1,0.01,100,1)
  
  wt<-as.matrix(unlist(retlist[1]))
  wtt[1:length(wt),t]<-wt[1:length(wt)]
  
  yt<-yperceptron(xinteste,wt,1)
  ytt[1:length(yt),t]<-yt[1:length(yt)]
  
  erroteste<-as.matrix(unlist(retlist[2]))
  
  a<-1
  for(a in 1:length(erroteste)){
    erros[t,a]<-erroteste[a]
  }
}


errosmedios<-matrix(0,nrow=100)

for(b in 1:100){
  errosmedios[b]<-mean(erros[1:maxteste,b])
}

plot(errosmedios,type='l',xlab='número total de épocas',ylab='média de erros',main='Média de Erros x Épocas')

boxplot(erros[,1:10],main='Boxplot Erros',xlab='Épocas',ylab='Erros')

yttmedios<-matrix(0,nrow=100-(2*ntrain))
for(b in 1:(100-(2*ntrain))){
  yttmedios[b]<-mean(ytt[b,1:maxteste])
}
plot(yttmedios, type='l',xlab='Amostras de Análise',ylab='', main=cbind('Classificação: ',maxteste,' Experimentos'))
par(new=T)
plot(yteste, type='p',xlab='Amostras de Análise',ylab='', main=cbind('Classificação: ',maxteste,' Experimentos'))


errosvar<-matrix(0,nrow=100)
for(b in 1:100){
  errosvar[b]<-var(erros[1:maxteste,b])
}

plot(errosvar[1:5],type='l',xlab='número total de Épocas',ylab='Variância de erros',main='Variância de Erros x épocas')

wttmedios<-matrix(0,nrow=5)

for(c in 1:5){
  wttmedios[c]<-mean(wtt[c,1:maxteste])
}
plot(wttmedios, type='b',xlab='w',ylab='Valor médio', main=cbind('Classificação: ',maxteste,' Experimentos - Valores Médios de w'))

wttvar<-matrix(0,nrow=5)

for(c in 1:5){wttvar[c]<-var(wtt[c,1:maxteste])}
plot(wttvar, type='b',xlab='w',ylab='variância', main=cbind('Classificação: ',maxteste,' Experimentos - Variância de w'))


