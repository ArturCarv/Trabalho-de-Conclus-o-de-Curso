saresp <- read.table(file="c:\\Eduardo\\UnB\\Ensino\\Teoria da Resposta ao Item\\Aulas-Remotas-TRI\\saresp.txt")
gab2 <- read.table(file="c:\\Eduardo\\UnB\\Ensino\\Teoria da Resposta ao Item\\Aulas-Remotas-TRI\\saresp-gabarito.txt")

dados <- saresp

manha <- dados[dados[,4]=="m07",]
tarde <- dados[dados[,4]=="t07",]
noite <- dados[dados[,4]=="n07",]

gab3 <- matrix(9,nrow(gab2),ncol(gab2))

for (i in 1:nrow(gab3)) {
  for (j in 1:ncol(gab3)) {
    if (gab2[i,j]=="A") gab3[i,j] <- 1
    if (gab2[i,j]=="B") gab3[i,j] <- 2
    if (gab2[i,j]=="C") gab3[i,j] <- 3
    if (gab2[i,j]=="D") gab3[i,j] <- 4
  }
}

resp.manha <- manha[,5:34]
resp.manha <- as.matrix(resp.manha)
resp.m <- matrix(9,nrow(resp.manha),ncol(resp.manha))
resp.m[resp.manha=="A"] <- 1
resp.m[resp.manha=="B"] <- 2
resp.m[resp.manha=="C"] <- 3
resp.m[resp.manha=="D"] <- 4

for (i in 1:nrow(resp.m)) {
  for (j in 1:ncol(resp.m)) {
    if ((resp.m[i,j]!=gab3[1,j])&&(resp.m[i,j]!=9)) resp.m[i,j] <- 0 
    if (resp.m[i,j]==gab3[1,j]) resp.m[i,j] <- 1 
  }
}

resp.m[resp.m==9] <- 0
#resp.m[resp.m==9] <- NA

resp.tarde <- tarde[,5:34]
resp.tarde <- as.matrix(resp.tarde)
resp.t <- matrix(9,nrow(resp.tarde),ncol(resp.tarde))
resp.t[resp.tarde=="A"] <- 1
resp.t[resp.tarde=="B"] <- 2
resp.t[resp.tarde=="C"] <- 3
resp.t[resp.tarde=="D"] <- 4

for (i in 1:nrow(resp.t)) {
  for (j in 1:ncol(resp.t)) {
    if ((resp.t[i,j]!=gab3[2,j])&&(resp.t[i,j]!=9)) resp.t[i,j] <- 0 
    if (resp.t[i,j]==gab3[2,j]) resp.t[i,j] <- 1 
  }
}

resp.t[resp.t==9] <- NA


resp.noite <- noite[,5:34]
resp.noite <- as.matrix(resp.noite)
resp.n <- matrix(9,nrow(resp.noite),ncol(resp.noite))
resp.n[resp.noite=="A"] <- 1
resp.n[resp.noite=="B"] <- 2
resp.n[resp.noite=="C"] <- 3
resp.n[resp.noite=="D"] <- 4

for (i in 1:nrow(resp.n)) {
  for (j in 1:ncol(resp.n)) {
    if ((resp.n[i,j]!=gab3[3,j])&&(resp.n[i,j]!=9)) resp.n[i,j] <- 0 
    if (resp.n[i,j]==gab3[3,j]) resp.n[i,j] <- 1 
  }
}

resp.n[resp.n==9] <- NA

################ Cálculo das estatísticas para os respondentes do turno da manhã.

### Estatisticas nao parametricas

library(PerFit)

r.pbis.out <- r.pbis(resp.m)
plot(r.pbis.out)
r.pbis.cutoff <- cutoff(r.pbis.out, Blvl = .01)
# Respondentes detectados como tendo respostas incoerentes
r.pbis.detect <-flagged.resp(r.pbis.out, cutoff.obj = r.pbis.cutoff, ord = TRUE)
r.pbis.detect$Scores

C.Sato.out <- C.Sato(resp.m)
plot(C.Sato.out)
C.Sato.cutoff <- cutoff(C.Sato.out, Blvl = .01)
# Respondentes detectados como tendo respostas incoerentes
C.Sato.detect <-flagged.resp(C.Sato.out, cutoff.obj = C.Sato.cutoff, ord = TRUE)
C.Sato.detect$Scores

G.out <- G(resp.m)
plot(G.out)
G.cutoff <- cutoff(G.out, Blvl = .01)
# Respondentes detectados como tendo respostas incoerentes
G.detect <-flagged.resp(G.out, cutoff.obj = G.cutoff, ord = TRUE)
G.detect$Scores

Gnormed.out <- Gnormed(resp.m)
plot(Gnormed.out)
Gnormed.cutoff <- cutoff(Gnormed.out, Blvl = .01)
# Respondentes detectados como tendo respostas incoerentes
Gnormed.detect <-flagged.resp(Gnormed.out, cutoff.obj = Gnormed.cutoff, ord = TRUE)
Gnormed.detect$Scores

A.KB.out <- A.KB(resp.m)
plot(A.KB.out)
A.KB.cutoff <- cutoff(A.KB.out, Blvl = .01)
# Respondentes detectados como tendo respostas incoerentes
A.KB.detect <-flagged.resp(A.KB.out, cutoff.obj = A.KB.cutoff, ord = TRUE)
A.KB.detect$Scores

E.KB.out <- E.KB(resp.m)
plot(E.KB.out)
E.KB.cutoff <- cutoff(E.KB.out, Blvl = .01)
# Respondentes detectados como tendo respostas incoerentes
E.KB.detect <-flagged.resp(E.KB.out, cutoff.obj = E.KB.cutoff, ord = TRUE)
E.KB.detect$Scores

D.KB.out <- D.KB(resp.m)
plot(D.KB.out)
D.KB.cutoff <- cutoff(D.KB.out, Blvl = .01)
# Respondentes detectados como tendo respostas incoerentes
D.KB.detect <-flagged.resp(D.KB.out, cutoff.obj = D.KB.cutoff, ord = TRUE)
D.KB.detect$Scores

U3.out <- U3(resp.m)
plot(U3.out)
U3.cutoff <- cutoff(U3.out, Blvl = .01)
# Respondentes detectados como tendo respostas incoerentes
U3.detect <-flagged.resp(U3.out, cutoff.obj = U3.cutoff, ord = TRUE)
U3.detect$Scores

ZU3.out <- ZU3(resp.m)
plot(ZU3.out)
ZU3.cutoff <- cutoff(ZU3.out, Blvl = .01)
# Respondentes detectados como tendo respostas incoerentes
ZU3.detect <-flagged.resp(ZU3.out, cutoff.obj = ZU3.cutoff, ord = TRUE)
ZU3.detect$Scores

Cstar.out <- Cstar(resp.m)
plot(Cstar.out)
Cstar.cutoff <- cutoff(Cstar.out, Blvl = .01)
# Respondentes detectados como tendo respostas incoerentes
Cstar.detect <-flagged.resp(Cstar.out, cutoff.obj = Cstar.cutoff, ord = TRUE)
Cstar.detect$Scores

NCI.out <- NCI(resp.m)
plot(NCI.out)
NCI.cutoff <- cutoff(NCI.out, Blvl = .01)
# Respondentes detectados como tendo respostas incoerentes
NCI.detect <-flagged.resp(NCI.out, cutoff.obj = NCI.cutoff, ord = TRUE)
NCI.detect$Scores

Ht.out <- Ht(resp.m)
plot(Ht.out)
Ht.cutoff <- cutoff(Ht.out, Blvl = .01)
# Respondentes detectados como tendo respostas incoerentes
Ht.detect <-flagged.resp(Ht.out, cutoff.obj = Ht.cutoff, ord = TRUE)
Ht.detect$Scores

### Estatisticas parametricas

### AJUSTES UTILIZANDO TRI

########################################################################
## Baixar os pacotes "irtoys", "ltm" e "mirt"

require(irtoys)
require(ltm)
require(mirt)

########### Ajuste do modelo logistico de 3 parametros para itens dicotomicos

ml3 <- tpm(resp.m)
par.est.3p <- coef(ml3) # cc, bb, aa
prof.3p <- factor.scores(ml3)
prof.est.3p <- prof.3p$score.dat[,33]

#ajuste3  <- mirt(resp.m,1,itemtype=c('3PL'))
#ajuste3  <- mirt(rbind(c("i1","i2","i3","i4","i5","i6","i7","i8","i9","i10","i11","i12","i13","i14","i15","i16","i17","i18","i19","i20","i21","i22","i23","i24","i25","i26","i27","i28","i29","i30"),resp.m),1,itemtype=c('3PL'))
#prof.est.3 <- fscores(ajuste3, full.scores=TRUE)
#par.est.3 <- coef(ajuste3,IRTpars=TRUE)

### Tratando 9 como 0

resp.m[resp.m==9] <- 0
n.resp <- nrow(resp.m)
n.itens <- ncol(resp.m)

cc <- par.est.3p[,1]
bb <- par.est.3p[,2]
aa <- par.est.3p[,3]

# Matriz das probabilidades de resposta positiva de cada respondente (linha) para cada item (coluna)
mat.prob <- matrix(0,n.resp,n.itens)
for (i in 1:n.itens) 
  mat.prob[,i] <- cc[i]+(1-cc[i])/(1+exp(-aa[i]*(prof.est.3p-bb[i])))

# "prop" e' o vetor "p" nas estatisticas do Apendice B de Karabatsos (2003)
prop <- apply(resp.m,2,mean)

# g e' o vetor "G" nas estatisticas do Apendice B de Karabatsos (2003)
g <- apply(mat.prob,2,mean)

#############################################################
### Unweighted Mean Square (Media Quadratica nao Ponderada)
#############################################################
# valores altos indicam incoerencia

ums <- rep(0,n.resp)

for (j in 1:n.resp) 
  ums[j] <- sum(((resp.m[j,]-mat.prob[j,])^2)/(mat.prob[j,]*(1-mat.prob[j,])))/n.itens

plot(prof.est.3p,ums,xlab=c("Proficiência"),ylab=c("Índice"),main=c("Unweighted Mean Square"))

#######################################################
### Weighted Mean Square (Media Quadratica Ponderada) 
#######################################################

wms <- rep(0,n.resp)

for (j in 1:n.resp) 
  wms[j] <- sum((resp.m[j,]-mat.prob[j,])^2)/sum((mat.prob[j,]*(1-mat.prob[j,])))

plot(prof.est.3p,wms,xlab=c("Proficiência"),ylab=c("Índice"),main=c("Weighted Mean Square"))

##############################################################
### Extended Caution Index 1 (Indice de Cautela Estendido 1) 
##############################################################

eci1 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci1[j] <- 1-cov(resp.m[j,],prop)/cov(mat.prob[j,],prop)

plot(prof.est.3p,eci1,xlab=c("Proficiência"),ylab=c("Índice"),main=c("Extended Caution Index 1"))

##############################################################
### Extended Caution Index 2 (Indice de Cautela Estendido 2) 
##############################################################

eci2 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci2[j] <- 1-cov(resp.m[j,],g)/cov(mat.prob[j,],g)

plot(prof.est.3p,eci2,xlab=c("Proficiência"),ylab=c("Índice"),main=c("Extended Caution Index 2"))

##############################################################
### Extended Caution Index 3 (Indice de Cautela Estendido 3) 
##############################################################

eci3 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci3[j] <- 1-cor(resp.m[j,],g)/cor(mat.prob[j,],g)

plot(prof.est.3p,eci3,xlab=c("Proficiência"),ylab=c("Índice"),main=c("Extended Caution Index 3"))

##############################################################
### Extended Caution Index 4 (Indice de Cautela Estendido 4) 
##############################################################

eci4 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci4[j] <- 1-cov(resp.m[j,],mat.prob[j,])/cov(mat.prob[j,],g)

plot(prof.est.3p,eci4,xlab=c("Proficiência"),ylab=c("Índice"),main=c("Extended Caution Index 4"))

##############################################################
### Extended Caution Index 5 (Indice de Cautela Estendido 5) 
##############################################################

eci5 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci5[j] <- 1-cor(resp.m[j,],mat.prob[j,])/cor(mat.prob[j,],g)

plot(prof.est.3p,eci5,xlab=c("Proficiência"),ylab=c("Índice"),main=c("Extended Caution Index 5"))

##############################################################
### Extended Caution Index 6 (Indice de Cautela Estendido 6) 
##############################################################

eci6 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci6[j] <- 1-cor(resp.m[j,],mat.prob[j,])/var(mat.prob[j,])

plot(prof.est.3p,eci6,xlab=c("Proficiência"),ylab=c("Índice"),main=c("Extended Caution Index 6"))


###########################################
### Simulacao para calculo dos p-valores 
###########################################

na <- 100 # numero de amostras sob H0

conta.ums <- conta.wms <- conta.eci1 <- conta.eci2 <- conta.eci3 <- conta.eci4 <- conta.eci5 <- conta.eci6 <- rep(0,n.resp)

ums.boot <- wms.boot <- eci1.boot <- eci2.boot <- eci3.boot <- eci4.boot <- eci5.boot <- eci6.boot <- rep(0,n.resp)

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp) {
    ums.boot[j] <- sum(((resp.boot[j,]-mat.prob[j,])^2)/(mat.prob[j,]*(1-mat.prob[j,])))/n.itens
    conta.ums[j] <- conta.ums[j] + sign(sign(ums.boot[j]-ums[j])+1)
  }
  
}



#### PAREI AQUI

for (h in 1:na) {
  dados <- sign(sign(mat.prob-matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens))+1)
  for (j in 1:n.resp) 
    eci1[j] <- 1-cov(dados[j,],prop)/cov(mat.prob[j,],prop)
  
  conta.ums <- conta.ums + 
    
    
    