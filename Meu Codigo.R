# -----------------------------------------------
# --    Uso da TRI para a identificação de     --
# --       respondentes inconsistentes         --   
# -----------------------------------------------

# Pacotes utilizados
# require("pacman")
pacman::p_load(irtoys,ltm,mirt,PerFit, tidyverse, xtable, ggthemes, gridExtra)

# Semente 
set.seed(42)

# Importando dados e Manipulando dados ----

saresp <- read.table(file="Saresp - 2007.txt")
gab2 <- read.table(file="saresp-gabarito.txt")

dados <- saresp

# Dividindo dados entre as turmas da manha, tarde e noite
manha <- dados[dados[,4]=="m07",]
tarde <- dados[dados[,4]=="t07",]
noite <- dados[dados[,4]=="n07",]

# Transformando as respostas dos itens em algoritimo 
# binário (1- Certo, 0- errado, NA - em branco)

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

# Ajuste do modelo logistico de 3 parametros para itens dicotomicos ----

ml3 <- tpm(resp.m)
par.est.3p <- coef(ml3) # cc, bb, aa
prof.3p <- factor.scores(ml3)
prof.est.3p <- prof.3p$score.dat[,33]

n.resp <- nrow(resp.m)
n.itens <- ncol(resp.m)

cc <- par.est.3p[,1]
bb <- par.est.3p[,2]
aa <- par.est.3p[,3]

# Obter a matriz de covariância dos parâmetros
cov_matrix <- vcov(ml3)

# Obter os parâmetros estimados
params <- coef(ml3)

# Número de itens
num_itens <- ncol(resp.m)

# Número de parâmetros por item no modelo ML3P
# (Geralmente, ML3P tem 3 parâmetros por item: a, b e c)
num_params_por_item <- 3

# Criar uma matriz para armazenar os erros padrão de cada parâmetro de cada item
erros_padrao_por_item <- matrix(NA, nrow = num_itens, ncol = num_params_por_item)

# Calcular os erros padrão para cada parâmetro de cada item
for (i in 1:num_itens) {
  # Índices para os parâmetros do item atual
  idx_inicio <- (i - 1) * num_params_por_item + 1
  idx_fim <- i * num_params_por_item
  
  # Extrair a submatriz da matriz de covariância para o item atual
  cov_item <- cov_matrix[idx_inicio:idx_fim, idx_inicio:idx_fim]
  
  # Calcular os erros padrão para os parâmetros do item atual
  erros_padrao_item <- sqrt(diag(cov_item))
  
  # Armazenar os erros padrão na matriz correspondente ao item
  erros_padrao_por_item[i,] <- erros_padrao_item
}

# Mostrar os erros padrão de cada parâmetro de cada item
nomes_parametros <- c("a", "b", "c")  # Substitua com os nomes dos parâmetros do seu modelo, se necessário

for (i in 1:num_itens) {
  cat("Erros Padrão - Item", i, ":\n")
  for (j in 1:num_params_por_item) {
    cat("Parâmetro", nomes_parametros[j], ": ", round(erros_padrao_por_item[i, j],2), "\n")
  }
  cat("\n")
}


# Matriz das probabilidades de resposta positiva  
mat.prob <- matrix(0,n.resp,n.itens)
for (i in 1:n.itens) 
  mat.prob[,i] <- cc[i]+(1-cc[i])/(1+exp(-aa[i]*(prof.est.3p-bb[i])))

# "prop" e' o vetor "p" nas estatisticas do Apendice B de Karabatsos (2003)
prop <- apply(resp.m,2,mean)

# g e' o vetor "G" nas estatisticas do Apendice B de Karabatsos (2003)
g <- apply(mat.prob,2,mean)

# Gráfico do número de acertos
afa <- as.character(1:30)

resp.m.data <- data.frame(resp.m)
names(resp.m.data) <- factor(as.character(1:30))
resp.m.data <- resp.m.data %>% pivot_longer(everything(), names_to = "Item") %>% 
  mutate(resp = rep(1:nrow(resp.m),each=30)) %>% 
  rename(Acertos = value) %>% 
  filter(Acertos == 1) 


barr <- resp.m.data %>% count(Item) %>% mutate(n = n/nrow(resp.m))

ggplot(barr) +
  aes(
    x = factor(Item,levels = afa),
    y = 100*n,
    label = paste0(round(100*n,0),"%")
  ) +
  geom_bar(stat = "identity", fill = "#007875", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.2, hjust = .5,
    size = 3
  ) +
  labs(x = "Itens", y = "Percentual de Acertos") +
  theme_pander()
ggsave("percentual_acertos_item.pdf", width = 158, height = 93, units = "mm")

# Gráfico da proficiência 
ggplot() +
  aes(x = factor(""), y = prof.est.3p) +
  geom_boxplot(fill = c("#007875"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3,
               fill = "white") +
  labs(x = "", y = "Proficiência") +
  theme_pander()
ggsave("proficiencia.pdf", width = 158, height = 93, units = "mm")

# Variáveis para as simulações
na <- 1000 # numero de amostras sob H0

conta.ums <- conta.zu <- conta.wms <- conta.zw <- conta.eci1 <- rep(0,n.resp) 
conta.eci2 <- conta.eci3 <- conta.eci4 <- conta.eci5 <- conta.eci6 <- rep(0,n.resp)

ums.boot <- zu.boot <- wms.boot <- zw.boot <- eci1.boot <- rep(0,n.resp)
eci2.boot <- eci3.boot <- eci4.boot <- eci5.boot <- eci6.boot <- rep(0,n.resp)

# Cálculo das estatísticas para os respondentes do turno da manhã. ----

## Estatísticas de Ajuste individual Parametricas ----

### Unweighted Mean Square  ----
# valores altos indicam incoerencia

# Cálculo da estatística

ums <- rep(0,n.resp)

for (j in 1:n.resp) {
  ums[j] <- (1/n.itens)*sum(((resp.m[j,]-mat.prob[j,])^2)/(mat.prob[j,]*
                                                             (1-mat.prob[j,])))
}

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp) {
    ums.boot[j] <- (1/n.itens)*sum(((resp.boot[j,]-mat.prob[j,])^2)/
                                     (mat.prob[j,]*(1-mat.prob[j,])))
    conta.ums[j] <- conta.ums[j] + sign(sign(ums.boot[j]-ums[j])+1)
  }
  
}

ums_p <- conta.ums/na

### Standardized Unweighted Mean Square  ----

zu <- rep(0,n.resp)

for (j in 1:n.resp) 
  zu[j] <- (ums[j]^(1/3) - 1 + (2/9*n.itens))/sqrt(2/9*n.itens)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    ums.boot[j] <- (1/n.itens)*sum(((resp.boot[j,]-mat.prob[j,])^2)/
                                     (mat.prob[j,]*(1-mat.prob[j,])))
    zu.boot[j] <- (ums.boot[j]^(1/3) - 1 + (2/9*n.itens))/sqrt(2/9*n.itens)
    conta.zu[j] <- conta.zu[j] + sign(sign(zu.boot[j]-zu[j])+1)
    }
  
}

zu_p <- conta.zu/na

### Weighted Mean Square  ----

wms <- rep(0,n.resp)

for (j in 1:n.resp) 
  wms[j] <- sum((resp.m[j,]-mat.prob[j,])^2)/sum((mat.prob[j,]*(1-mat.prob[j,])))

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    wms.boot[j] <- sum((resp.boot[j,]-mat.prob[j,])^2)/sum((mat.prob[j,]
                                                            *(1-mat.prob[j,])))
    conta.wms[j] <- conta.wms[j] + sign(sign(wms.boot[j]-wms[j])+1)
  }
  
}

wms_p <- conta.wms/na

### Standardized Weighted Mean Square----

zw <- rep(0,n.resp)

for (j in 1:n.resp) 
  zw[j] <- (wms[j]^(1/3) - 1 + (2/9*n.itens))/sqrt(2/9*n.itens)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    wms.boot[j] <- sum((resp.boot[j,]-mat.prob[j,])^2)/sum((mat.prob[j,]
                                                            *(1-mat.prob[j,])))
    zw.boot[j] <- (wms.boot[j]^(1/3) - 1 + (2/9*n.itens))/sqrt(2/9*n.itens)
    conta.zw[j] <- conta.zw[j] + sign(sign(zw.boot[j]-zw[j])+1)
  }
  
}

zw_p <- conta.zw/na

### Extended Caution Index 1 ----

eci1 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci1[j] <- 1-cov(resp.m[j,],prop)/cov(mat.prob[j,],prop)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    eci1.boot[j] <- 1-cov(resp.boot[j,],prop)/cov(mat.prob[j,],prop)
    conta.eci1[j] <- conta.eci1[j] + sign(sign(eci1.boot[j]-eci1[j])+1)
  }
  
}

eci1_p <- conta.eci1/na

### Extended Caution Index 2 ----

eci2 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci2[j] <- 1-cov(resp.m[j,],g)/cov(mat.prob[j,],g)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    eci2.boot[j] <- 1-cov(resp.boot[j,],g)/cov(mat.prob[j,],g)
    conta.eci2[j] <- conta.eci2[j] + sign(sign(eci2.boot[j]-eci2[j])+1)
  }
  
}

eci2_p <- conta.eci2/na

### Extended Caution Index 3 ----

eci3 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci3[j] <- 1-cor(resp.m[j,],g)/cor(mat.prob[j,],g)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    eci3.boot[j] <- 1-cor(resp.boot[j,],g)/cor(mat.prob[j,],g)
    conta.eci3[j] <- conta.eci3[j] + sign(sign(eci3.boot[j]-eci3[j])+1)
  }
  
}

eci3_p <- conta.eci3/na

### Extended Caution Index 4 ----

eci4 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci4[j] <- 1-cov(resp.m[j,],mat.prob[j,])/cov(mat.prob[j,],g)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    eci4.boot[j] <- 1-cov(resp.boot[j,],mat.prob[j,])/cov(mat.prob[j,],g)
    conta.eci4[j] <- conta.eci4[j] + sign(sign(eci4.boot[j]-eci4[j])+1)
  }
  
}

eci4_p <- conta.eci4/na

### Extended Caution Index 5 ----

eci5 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci5[j] <- 1-cor(resp.m[j,],mat.prob[j,])/cor(mat.prob[j,],g)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    eci5.boot[j] <- 1-cor(resp.boot[j,],mat.prob[j,])/cor(mat.prob[j,],g)
    conta.eci5[j] <- conta.eci5[j] + sign(sign(eci5.boot[j]-eci5[j])+1)
  }
  
}

eci5_p <- conta.eci5/na

### Extended Caution Index 6 ----

eci6 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci6[j] <- 1-cor(resp.m[j,],mat.prob[j,])/var(mat.prob[j,])

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    eci6.boot[j] <- 1-cor(resp.boot[j,],mat.prob[j,])/var(mat.prob[j,])
    conta.eci6[j] <- conta.eci6[j] + sign(sign(eci6.boot[j]-eci6[j])+1)
  }
}

eci6_p <- conta.eci6/na

### Estudo das EAI ----

EAI <- data.frame(id = 1:1001,U = ums,ZU = zu,W = wms,ZW = zw,ECI1 = eci1,
                  ECI2 = eci2,ECI3 = eci3,ECI4 = eci4,ECI5 = eci5,
                  ECI6 = eci6)

prof <- data.frame(id = 1:1001, prof = prof.est.3p)

eai_longos <- pivot_longer(EAI,-id, names_to = "EAI") |> full_join(prof)

eai_longos$EAI <- factor(eai_longos$EAI, levels = names(EAI[,2:ncol(EAI)]))

# Gráfico das EAI

ggplot(eai_longos) +
  aes(x = `EAI`,y = value, fill = `EAI`) +
  geom_boxplot(fill = c("#007875"), width = 0.5) +
  guides(fill = FALSE) +
  facet_wrap(~ EAI, scales = "free") +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3,
               fill = "white") +
  labs(x = "", y = "Proficiência") +
  theme_pander()

xtable(data.frame(tapply(eai_longos$value, eai_longos$EAI, max)))

# Gráfico das EAI pela proficiência
ggplot(eai_longos, aes(x = prof,y = value, fill = EAI)) +
  geom_point(color = "#007875") +
  facet_wrap(~ EAI, scales = "free") +
  labs(x = "Proficiência", y = "Estatística") +
  guides(fill = FALSE) +
  theme(legend.position = "none") +
  theme_pander()

# P-valores simulação

EAI_p <- data.frame(id = 1:1001,U_p = ums_p,ZU_p = zu_p,W_p = wms_p,ZW_p = zw_p,
                    ECI1_p = eci1_p,ECI2_p = eci2_p,ECI3_p = eci3_p,
                    ECI4_p = eci4_p,ECI5_p = eci5_p,ECI6_p = eci6_p)

EAI2 <- full_join(EAI,EAI_p)  

# função gráfico das respostas:
fgr <- function(x){
  banco <- data.frame(dificuldade = bb,
                      respostas = resp.m[x,])
  ggplot(banco) +
    aes(x = dificuldade, y = respostas) +
    geom_point(colour = "#007875", size = 1.5) +
    labs(
      subtitle = paste0("Respondente ",x),
      x = "Dificuldade do item",
      y = "Respostas"
    ) +
    theme_pander()
}


# Respontes incoerentes
incoerentes <- EAI2 %>%
  filter_at(vars(names(EAI2[,12:21])), any_vars(. < 0.05))


# Número de indivíduos por estatistica
soma <- apply(EAI2[,12:21],2,function(ee){
  sum(ee < 0.05,na.rm = T)
})

percent <- soma/n.resp

xtable(data.frame(soma,percent))

tot <- apply(EAI2[,12:21],1,function(ee){
  sum(ee < 0.05, na.rm = T)
})
sum(tot == 10)
sum(tot == 10)/n.resp

# matriz:
matriz_incoerentes <- incoerentes[,12:21] |> mutate_all(any_vars(. < 0.05))

matriz_incoerentes <- incoerentes[,12:21] %>%
  mutate(across(everything(), ~ ifelse(. < 0.05, 1, 0), 
                .names = "{.col}_indicador"))

matriz_incoerentes[is.na(matriz_incoerentes)] <- 0

# Função para contar os 1s em comum entre duas colunas
contar_1s_em_comum <- function(col1, col2) {
  sum(col1 == 1 & col2 == 1)
}

# Criando a matriz de interseções
matriz_intersecoes <- outer(
  names(matriz_incoerentes),
  names(matriz_incoerentes),
  Vectorize(function(x, y) contar_1s_em_comum(matriz_incoerentes[[x]], 
                                              matriz_incoerentes[[y]]))
)


percentual_1s_em_comum <- function(col1, col2) {
  total <- sum(col1)
  if (total == 0) return(NA)
  
  paste0(100*round(sum(col1 == 1 & col2 == 1) / total,2),"%")
}

matriz_intersecoes_perc <- outer(
  names(matriz_incoerentes),
  names(matriz_incoerentes),
  Vectorize(function(x, y) percentual_1s_em_comum(matriz_incoerentes[[x]], 
                                                  matriz_incoerentes[[y]]))
)

xtable(matriz_intersecoes)
xtable(matriz_intersecoes_perc)

# Top incor.
top_inc <- incoerentes %>%
  filter_at(vars(names(EAI2[,12:21])), all_vars(. < 0.05))

xtable(top_inc[,1:11] |> filter(id %in% c(465,595,824,832)))


# incoerentes
grid.arrange(fgr(465), fgr(595),fgr(824),fgr(832),  ncol=2,nrow=2)


# coerentes.
coer <- EAI2 %>%
  filter_at(vars(names(EAI2[,12:21])), all_vars(. >= 0.05))

xtable(coer[,1:11] |> filter(id %in% c(15,396,501,605)))

# coerentes
grid.arrange(fgr(15), fgr(396), fgr(501),fgr(605), ncol=2,nrow=2)

# reestimar modelo ----
# Remover incoerentes
resp.m2 <- resp.m[-c(top_inc$id),]

ml3 <- tpm(resp.m2)
par.est.3p <- coef(ml3) # cc, bb, aa
prof.3p <- factor.scores(ml3)
prof.est.3p <- prof.3p$score.dat[,33]
n.resp <- nrow(resp.m2)
n.itens <- ncol(resp.m2)

cc <- par.est.3p[,1]
bb <- par.est.3p[,2]
aa <- par.est.3p[,3]

xtable(data.frame(aa,bb,cc))

# Obter a matriz de covariância dos parâmetros
cov_matrix <- vcov(ml3)

# Obter os parâmetros estimados
params <- coef(ml3)

# Número de itens
num_itens <- ncol(resp.m)

# Número de parâmetros por item no modelo ML3P
# (Geralmente, ML3P tem 3 parâmetros por item: a, b e c)
num_params_por_item <- 3

# Criar uma matriz para armazenar os erros padrão de cada parâmetro de cada item
erros_padrao_por_item <- matrix(NA, nrow = num_itens, ncol = num_params_por_item)

# Calcular os erros padrão para cada parâmetro de cada item
for (i in 1:num_itens) {
  # Índices para os parâmetros do item atual
  idx_inicio <- (i - 1) * num_params_por_item + 1
  idx_fim <- i * num_params_por_item
  
  # Extrair a submatriz da matriz de covariância para o item atual
  cov_item <- cov_matrix[idx_inicio:idx_fim, idx_inicio:idx_fim]
  
  # Calcular os erros padrão para os parâmetros do item atual
  erros_padrao_item <- sqrt(diag(cov_item))
  
  # Armazenar os erros padrão na matriz correspondente ao item
  erros_padrao_por_item[i,] <- erros_padrao_item
}

# Mostrar os erros padrão de cada parâmetro de cada item
nomes_parametros <- c("a", "b", "c")  # Substitua com os nomes dos parâmetros do seu modelo, se necessário

for (i in 1:num_itens) {
  cat("Erros Padrão - Item", i, ":\n")
  for (j in 1:num_params_por_item) {
    cat("Parâmetro", nomes_parametros[j], ": ", round(erros_padrao_por_item[i, j],2), "\n")
  }
  cat("\n")
}



# Fixando o parâmetro c ----

ml3 <- tpm(resp.m, const = cbind(1:30,1,0.25))

par.est.3p <- coef(ml3) # cc, bb, aa
prof.3p <- factor.scores(ml3)
prof.est.3p <- prof.3p$score.dat[,33]

n.resp <- nrow(resp.m)
n.itens <- ncol(resp.m)

cc <- par.est.3p[,1]
bb <- par.est.3p[,2]
aa <- par.est.3p[,3]

xtable(data.frame(aa,bb,cc))

# Obter a matriz de covariância dos parâmetros
cov_matrix <- vcov(ml3)

# Obter os parâmetros estimados
params <- coef(ml3)

# Número de itens
num_itens <- ncol(resp.m)

# Número de parâmetros por item no modelo ML3P
# (Geralmente, ML3P tem 2 parâmetros por item: a e b )
num_params_por_item <- 2

# Criar uma matriz para armazenar os erros padrão de cada parâmetro de cada item
erros_padrao_por_item <- matrix(NA, nrow = num_itens, ncol = num_params_por_item)

# Calcular os erros padrão para cada parâmetro de cada item
for (i in 1:num_itens) {
  # Índices para os parâmetros do item atual
  idx_inicio <- (i - 1) * num_params_por_item + 1
  idx_fim <- i * num_params_por_item
  
  # Extrair a submatriz da matriz de covariância para o item atual
  cov_item <- cov_matrix[idx_inicio:idx_fim, idx_inicio:idx_fim]
  
  # Calcular os erros padrão para os parâmetros do item atual
  erros_padrao_item <- sqrt(diag(cov_item))
  
  # Armazenar os erros padrão na matriz correspondente ao item
  erros_padrao_por_item[i,] <- erros_padrao_item
}

# Mostrar os erros padrão de cada parâmetro de cada item
nomes_parametros <- c("a", "b")  # Substitua com os nomes dos parâmetros do seu modelo, se necessário

for (i in 1:num_itens) {
  cat("Erros Padrão - Item", i, ":\n")
  for (j in 1:num_params_por_item) {
    cat("Parâmetro", nomes_parametros[j], ": ", round(erros_padrao_por_item[i, j],2), "\n")
  }
  cat("\n")
}



# Matriz das probabilidades de resposta positiva  
mat.prob <- matrix(0,n.resp,n.itens)
for (i in 1:n.itens) 
  mat.prob[,i] <- cc[i]+(1-cc[i])/(1+exp(-aa[i]*(prof.est.3p-bb[i])))

# "prop" e' o vetor "p" nas estatisticas do Apendice B de Karabatsos (2003)
prop <- apply(resp.m,2,mean)

# g e' o vetor "G" nas estatisticas do Apendice B de Karabatsos (2003)
g <- apply(mat.prob,2,mean)

# Gráfico da proficiência 
ggplot() +
  aes(x = factor(""), y = prof.est.3p) +
  geom_boxplot(fill = c("#007875"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3,
               fill = "white") +
  labs(x = "", y = "Proficiência") +
  theme_pander()
ggsave("proficiencia2.pdf", width = 158, height = 93, units = "mm")

# Variáveis para as simulações
na <- 1000 # numero de amostras sob H0

conta.ums <- conta.zu <- conta.wms <- conta.zw <- conta.eci1 <- rep(0,n.resp) 
conta.eci2 <- conta.eci3 <- conta.eci4 <- conta.eci5 <- conta.eci6 <- rep(0,n.resp)

ums.boot <- zu.boot <- wms.boot <- zw.boot <- eci1.boot <- rep(0,n.resp)
eci2.boot <- eci3.boot <- eci4.boot <- eci5.boot <- eci6.boot <- rep(0,n.resp)

# Cálculo das estatísticas para os respondentes do turno da manhã. ----

## Estatísticas de Ajuste individual Parametricas ----

### Unweighted Mean Square  ----
# valores altos indicam incoerencia

# Cálculo da estatística

ums_2 <- rep(0,n.resp)

for (j in 1:n.resp) {
  ums_2[j] <- (1/n.itens)*sum(((resp.m[j,]-mat.prob[j,])^2)/(mat.prob[j,]*
                                                             (1-mat.prob[j,])))
}

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp) {
    ums.boot[j] <- (1/n.itens)*sum(((resp.boot[j,]-mat.prob[j,])^2)/
                                     (mat.prob[j,]*(1-mat.prob[j,])))
    conta.ums[j] <- conta.ums[j] + sign(sign(ums.boot[j]-ums_2[j])+1)
  }
  
}

ums_p2 <- conta.ums/na

### Standardized Unweighted Mean Square  ----

zu_2 <- rep(0,n.resp)

for (j in 1:n.resp) 
  zu_2[j] <- (ums[j]^(1/3) - 1 + (2/9*n.itens))/sqrt(2/9*n.itens)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    ums.boot[j] <- (1/n.itens)*sum(((resp.boot[j,]-mat.prob[j,])^2)/
                                     (mat.prob[j,]*(1-mat.prob[j,])))
    zu.boot[j] <- (ums.boot[j]^(1/3) - 1 + (2/9*n.itens))/sqrt(2/9*n.itens)
    conta.zu[j] <- conta.zu[j] + sign(sign(zu.boot[j]-zu_2[j])+1)
  }
  
}

zu_p2 <- conta.zu/na

### Weighted Mean Square  ----

wms_2 <- rep(0,n.resp)

for (j in 1:n.resp) 
  wms_2[j] <- sum((resp.m[j,]-mat.prob[j,])^2)/sum((mat.prob[j,]*(1-mat.prob[j,])))

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    wms.boot[j] <- sum((resp.boot[j,]-mat.prob[j,])^2)/sum((mat.prob[j,]
                                                            *(1-mat.prob[j,])))
    conta.wms[j] <- conta.wms[j] + sign(sign(wms.boot[j]-wms_2[j])+1)
  }
  
}

wms_p2 <- conta.wms/na

### Standardized Weighted Mean Square----

zw_2 <- rep(0,n.resp)

for (j in 1:n.resp) 
  zw_2[j] <- (wms[j]^(1/3) - 1 + (2/9*n.itens))/sqrt(2/9*n.itens)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    wms.boot[j] <- sum((resp.boot[j,]-mat.prob[j,])^2)/sum((mat.prob[j,]
                                                            *(1-mat.prob[j,])))
    zw.boot[j] <- (wms.boot[j]^(1/3) - 1 + (2/9*n.itens))/sqrt(2/9*n.itens)
    conta.zw[j] <- conta.zw[j] + sign(sign(zw.boot[j]-zw_2[j])+1)
  }
  
}

zw_p2 <- conta.zw/na

### Extended Caution Index 1 ----

eci1_2 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci1_2[j] <- 1-cov(resp.m[j,],prop)/cov(mat.prob[j,],prop)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    eci1.boot[j] <- 1-cov(resp.boot[j,],prop)/cov(mat.prob[j,],prop)
    conta.eci1[j] <- conta.eci1[j] + sign(sign(eci1.boot[j]-eci1_2[j])+1)
  }
  
}

eci1_p2 <- conta.eci1/na

### Extended Caution Index 2 ----

eci2_2 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci2_2[j] <- 1-cov(resp.m[j,],g)/cov(mat.prob[j,],g)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    eci2.boot[j] <- 1-cov(resp.boot[j,],g)/cov(mat.prob[j,],g)
    conta.eci2[j] <- conta.eci2[j] + sign(sign(eci2.boot[j]-eci2_2[j])+1)
  }
  
}

eci2_p2 <- conta.eci2/na

### Extended Caution Index 3 ----

eci3_2 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci3_2[j] <- 1-cor(resp.m[j,],g)/cor(mat.prob[j,],g)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    eci3.boot[j] <- 1-cor(resp.boot[j,],g)/cor(mat.prob[j,],g)
    conta.eci3[j] <- conta.eci3[j] + sign(sign(eci3.boot[j]-eci3_2[j])+1)
  }
  
}

eci3_p2 <- conta.eci3/na

### Extended Caution Index 4 ----

eci4_2 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci4_2[j] <- 1-cov(resp.m[j,],mat.prob[j,])/cov(mat.prob[j,],g)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    eci4.boot[j] <- 1-cov(resp.boot[j,],mat.prob[j,])/cov(mat.prob[j,],g)
    conta.eci4[j] <- conta.eci4[j] + sign(sign(eci4.boot[j]-eci4_2[j])+1)
  }
  
}

eci4_p2 <- conta.eci4/na

### Extended Caution Index 5 ----

eci5_2 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci5_2[j] <- 1-cor(resp.m[j,],mat.prob[j,])/cor(mat.prob[j,],g)

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    eci5.boot[j] <- 1-cor(resp.boot[j,],mat.prob[j,])/cor(mat.prob[j,],g)
    conta.eci5[j] <- conta.eci5[j] + sign(sign(eci5.boot[j]-eci5_2[j])+1)
  }
  
}

eci5_p2 <- conta.eci5/na

### Extended Caution Index 6 ----

eci6_2 <- rep(0,n.resp)

for (j in 1:n.resp) 
  eci6_2[j] <- 1-cor(resp.m[j,],mat.prob[j,])/var(mat.prob[j,])

# Simulação

for (i in 1:na) {
  
  mat.boot <- matrix(runif(n.resp*n.itens,0,1),n.resp,n.itens)
  resp.boot <- sign(sign(mat.prob-mat.boot)+1)
  
  for (j in 1:n.resp){ 
    eci6.boot[j] <- 1-cor(resp.boot[j,],mat.prob[j,])/var(mat.prob[j,])
    conta.eci6[j] <- conta.eci6[j] + sign(sign(eci6.boot[j]-eci6_2[j])+1)
  }
}

eci6_p2 <- conta.eci6/na

### Estudo das EAI ----

EAI_2 <- data.frame(id = 1:1001,U = ums_2,ZU = zu_2,W = wms_2,ZW = zw_2,ECI1 = eci1_2,
                  ECI2 = eci2_2,ECI3 = eci3_2,ECI4 = eci4_2,ECI5 = eci5_2,
                  ECI6 = eci6_2)

prof <- data.frame(id = 1:1001, prof = prof.est.3p)

eai_longos_2 <- pivot_longer(EAI_2,-id, names_to = "EAI") |> full_join(prof)

eai_longos$EAI <- factor(eai_longos_2$EAI, levels = names(EAI_2[,2:ncol(EAI_2)]))

# Gráfico das eai 

ggplot(eai_longos_2) +
  aes(x = `EAI`,y = value, fill = `EAI`) +
  geom_boxplot(fill = c("#007875"), width = 0.5) +
  guides(fill = FALSE) +
  facet_wrap(~ EAI, scales = "free") +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3,
               fill = "white") +
  labs(x = "", y = "Proficiência") +
  theme_pander()

xtable(data.frame(tapply(eai_longos_2$value, eai_longos_2$EAI, max)))

# Gráfico das EAI pela proficiência
ggplot(eai_longos_2, aes(x = prof,y = value, fill = EAI)) +
  geom_point(color = "#007875") +
  facet_wrap(~ EAI, scales = "free") +
  labs(x = "Proficiência", y = "Estatística") +
  guides(fill = FALSE) +
  theme(legend.position = "none") +
  theme_pander()

# P-valores simulação

EAI_p2 <- data.frame(id = 1:1001,U_p = ums_p2,ZU_p = zu_p2,W_p = wms_p2,ZW_p = zw_p2,
                    ECI1_p = eci1_p2,ECI2_p = eci2_p2,ECI3_p = eci3_p2,
                    ECI4_p = eci4_p2,ECI5_p = eci5_p2,ECI6_p = eci6_p2)

EAI2_2 <- full_join(EAI_2,EAI_p2)  

# Respontes incoerentes
incoerentes_2 <- EAI2_2 %>%
  filter_at(vars(names(EAI2_2[,12:21])), any_vars(. < 0.05))


# Número de indivíduos por estatistica
soma <- apply(EAI2_2[,12:21],2,function(ee){
  sum(ee < 0.05,na.rm = T)
})

percent <- soma/n.resp

xtable(data.frame(soma,percent))

tot <- apply(EAI2_2[,12:21],1,function(ee){
  sum(ee < 0.05, na.rm = T)
})
sum(tot == 10)
sum(tot == 10)/n.resp

# matriz:
matriz_incoerentes_2 <- incoerentes_2[,12:21] |> mutate_all(any_vars(. < 0.05))

matriz_incoerentes_p2 <- incoerentes_2[,12:21] %>%
  mutate(across(everything(), ~ ifelse(. < 0.05, 1, 0), 
                .names = "{.col}_indicador"))

matriz_incoerentes_2[is.na(matriz_incoerentes_2)] <- 0

# Função para contar os 1s em comum entre duas colunas
contar_1s_em_comum <- function(col1, col2) {
  sum(col1 == 1 & col2 == 1)
}

# Criando a matriz de interseções
matriz_intersecoes_2 <- outer(
  names(matriz_incoerentes_2),
  names(matriz_incoerentes_2),
  Vectorize(function(x, y) contar_1s_em_comum(matriz_incoerentes_2[[x]], 
                                              matriz_incoerentes_2[[y]]))
)


percentual_1s_em_comum <- function(col1, col2) {
  total <- sum(col1)
  if (total == 0) return(NA)
  
  paste0(100*round(sum(col1 == 1 & col2 == 1) / total,2),"%")
}

matriz_intersecoes_perc_2 <- outer(
  names(matriz_incoerentes_2),
  names(matriz_incoerentes_2),
  Vectorize(function(x, y) percentual_1s_em_comum(matriz_incoerentes_2[[x]], 
                                                  matriz_incoerentes_2[[y]]))
)

xtable(matriz_intersecoes_2)
xtable(matriz_intersecoes_perc_2)

# Incoerentes em comum

inc_comum <- numeric(ncol(EAI_p)-1)

for (i in 2:ncol(EAI_p)) {
  j <- which(EAI_p[, i] < 0.05 & EAI_p2[, i] < 0.05)
  inc_comum[i-1] <- length(j)
}

inc_comum


# Top incor.
top_inc_2 <- incoerentes_2 %>%
  filter_at(vars(names(EAI2_2[,12:21])), all_vars(. < 0.05))


top_inc_comum <- top_inc_2 |> filter(id %in% top_inc$id) |> nrow()

# reestimar modelo ----
# Remover incoerentes
resp.m2 <- resp.m[-c(top_inc_2$id),]

ml3 <- tpm(resp.m2, const = cbind(1:30,1,0.25))

par.est.3p <- coef(ml3) # cc, bb, aa
prof.3p <- factor.scores(ml3)
prof.est.3p <- prof.3p$score.dat[,33]

n.resp <- nrow(resp.m2)
n.itens <- ncol(resp.m2)

cc <- par.est.3p[,1]
bb <- par.est.3p[,2]
aa <- par.est.3p[,3]

xtable(data.frame(aa,bb,cc))

# Obter a matriz de covariância dos parâmetros
cov_matrix <- vcov(ml3)

# Obter os parâmetros estimados
params <- coef(ml3)

# Número de itens
num_itens <- ncol(resp.m)

# Número de parâmetros por item no modelo ML3P
# (Geralmente, ML3P tem 2 parâmetros por item: a e b )
num_params_por_item <- 2

# Criar uma matriz para armazenar os erros padrão de cada parâmetro de cada item
erros_padrao_por_item <- matrix(NA, nrow = num_itens, ncol = num_params_por_item)

# Calcular os erros padrão para cada parâmetro de cada item
for (i in 1:num_itens) {
  # Índices para os parâmetros do item atual
  idx_inicio <- (i - 1) * num_params_por_item + 1
  idx_fim <- i * num_params_por_item
  
  # Extrair a submatriz da matriz de covariância para o item atual
  cov_item <- cov_matrix[idx_inicio:idx_fim, idx_inicio:idx_fim]
  
  # Calcular os erros padrão para os parâmetros do item atual
  erros_padrao_item <- sqrt(diag(cov_item))
  
  # Armazenar os erros padrão na matriz correspondente ao item
  erros_padrao_por_item[i,] <- erros_padrao_item
}

# Mostrar os erros padrão de cada parâmetro de cada item
nomes_parametros <- c("a", "b")  # Substitua com os nomes dos parâmetros do seu modelo, se necessário

for (i in 1:num_itens) {
  cat("Erros Padrão - Item", i, ":\n")
  for (j in 1:num_params_por_item) {
    cat("Parâmetro", nomes_parametros[j], ": ", round(erros_padrao_por_item[i, j],2), "\n")
  }
  cat("\n")
}


