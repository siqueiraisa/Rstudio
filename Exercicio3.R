rm(list=ls())
sech2<-function(u) ((2/(exp(u)+exp(-u)))^2/(exp(u)+exp(-u)))
x<-matrix(data=c(0,0,0,1,1,0,1,1),nrow=4,ncol=2,byrow = TRUE) #entrada da XOR (A e B Tabela verdade)
y<-matrix(data=c(0,1,1,0,0,1,1,0),nrow=4,ncol=2,byrow = TRUE) #saida da XOR (A e B Tabela verdade)
i1<-1
i4<-1
i5<-1
i8<-1
w61<-runif(1)-0.5
w62<-runif(1)-0.5
w63<-runif(1)-0.5
w72<-runif(1)-0.5
w73<-runif(1)-0.5
w74<-runif(1)-0.5
w95<-runif(1)-0.5
w96<-runif(1)-0.5
w97<-runif(1)-0.5
w106<-runif(1)-0.5
w107<-runif(1)-0.5
w108<-runif(1)-0.5
nepocas<-0
tol<-0.01
eepoca<-tol+1
N<-4
eta<-0.05
maxepocas<-4000
matproj<-matrix(nrow=4,ncol=2)
evec<-matrix(nrow=1,ncol=maxepocas)

while ((nepocas < maxepocas) && (eepoca>tol))
{
  ei2<-0
  xseq<-sample(N)
  for (i in 1:N)
  {
    irand<-xseq[i]
    xi<-x[irand,]
    yi<-y[irand,]
    i3<-xi[2]
    i2<-xi[1]
    y9<-yi[2]
    y10<-yi[1]
    
    u6<-i1*w61+i2*w62+i3*w63
    i6<-tanh(u6)
    
    u7<-i2*w72+i3*w73+i4*w74
    i7<-tanh(u7)
    
    u9<-i5*w95+i6*w96+i7*w97
    i9<-tanh(u9)
    
    u10<-i6*w106+i7*w107+i8*w108
    i10<-tanh(u10)
    
    e9<-y9-i9
    e10<-y10-i10
    
    d9<-e9*sech2(u9)
    d10<-e10*sech2(u10)
    
    dw95<-eta*d9*i5
    dw96<-eta*d9*i6
    dw97<-eta*d9*i7
    
    dw106<-eta*d10*i6
    dw107<-eta*d10*i7
    dw108<-eta*d10*i8
    
    d6<-sech2(u6)*(d9*w96+d10*w106)
    dw61<-eta*d6*i1
    dw62<-eta*d6*i2
    dw63<-eta*d6*i3
    
    d7<-sech2(u7)*(d9*w97+d10*w107)
    dw72<-eta*d7*i2
    dw73<-eta*d7*i3
    dw74<-eta*d7*i4
    
    w95<-w95+dw95
    w96<-w96+dw96
    w97<-w97+dw97
    
    w106<-w106+dw106
    w107<-w107+dw107
    w108<-w108+dw108
    
    w61<-w61+dw61
    w62<-w62+dw62
    w63<-w63+dw63
    
    w72<-w72+dw72
    w73<-w73+dw73
    w74<-w74+dw74
    
    ei2<-ei2+(e9*e9+e10*e10)
    matproj[irand,1]<-i6
    matproj[irand,2]<-i7
  }
}
print(nepocas)
nepocas<-nepocas+1
evec[nepocas]<-e12/N
eepoca<evc[nepocas]


plot(1:length(evec),evec,type='1')
yhatmat<-matrix(nrow=4,ncol=2)
for (i in 1:N)
{
  xi<-x[1,]
  yi<-y[1,]
  i3<-xi[2]
  i2<-xi[1]
  y9<-yi[2]
  y10<-yi[1]
  
  u6<i1*w61+i2*w62+i3*w63
  i6<tanh(u6)
  
  u7<i2*w72+i3*w73+i4*w74
  i7<tanh(u7)
  

  u9<i5*w95+i6*w96+i7*w97
  i7<tanh(u9)
  
  
  u10<i6*w106+i7*w107+i8*w108
  i10<tanh(u10)
  yhatmat[i,1]<-i10
  yhatmat[i,2]<-i9
  
}

print(cbind(y,yhatmat))
print(yhatmat)


