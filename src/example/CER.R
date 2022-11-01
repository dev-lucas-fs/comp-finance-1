library(zoo)
library(quantmod)
library(tseries)
library(PerformanceAnalytics)
library(tidyverse)

############################################################
# DEMO 1
############################################################
# Estat�stica amostral

# Ita� Unibanco Holding SA
# Vale SA
# Bradesco
# Ambev
# B3
# Petrobr�s

# baixando dados do Yahoo...
START = "2014-09-01"
END = "2019-09-30"
codigos = c("ITUB4", "VALE3", "BBDC4", "ABEV3", "B3SA3", "PETR4")

close.zoo = zoo()
index(close.zoo) = as.yearmon(index(close.zoo)) 

for(codigo in codigos) {
  aux = get.hist.quote(
    instrument=paste(codigo, "SA", sep='.'), 
    start=START, end=END, 
    quote="AdjClose", 
    provider="yahoo", 
    compression = "m",
    retclass="zoo", 
    quiet=TRUE)
  index(aux) = as.yearmon(index(aux))
  close.zoo = merge(close.zoo, aux)
}

colnames(close.zoo) = codigos
close.df = data.frame(close.zoo)
head(close.df, 5)
tail(close.df, 5)

# usando dplyr
baixa_cotacoes = function(codigo) {
  aux = get.hist.quote(
    instrument=paste(codigo, "SA", sep='.'), 
    start=START, end=END, 
    quote="AdjClose", 
    provider="yahoo", 
    compression = "m",
    retclass="zoo", 
    quiet=TRUE) 
}

close.zoo = 
  codigos %>%
  map(~baixa_cotacoes(.)) %>% 
  purrr::reduce(merge) 

index(close.zoo) = as.yearmon(index(close.zoo))
names(close.zoo) = codigos
head(close.zoo)
# evolu��o dos pre�os 
plot(close.zoo$ITUB4, main = 'ITUB4', ylab = 'Fechamento Ajustado', lwd = 2)
quantmod::chartSeries(close.zoo$ITUB4, name = 'ITUB4')

plot(close.zoo$VALE3, main = 'VALE3', ylab = 'Fechamento Ajustado', lwd = 2)
plot(close.zoo$BBDC4, main = 'BBDC4', ylab = 'Fechamento Ajustado', lwd = 2)
plot(close.zoo$B3SA3, main = 'BBDC4', ylab = 'Fechamento Ajustado', lwd = 2)
# ou...
ts.plot(close.zoo, col=1:length(codigos), lwd=2)
graphics::legend(x = 2014.5, y = 35, legend = codigos, col=1:length(codigos), lty = 1, lwd = 3)

# transforma pre�os de fechamento em retornos mensais cc
ret.df = apply(X = log(close.zoo), MARGIN = 2, FUN = diff)
ret.df %>% head(1)
# ret.df � do tipo matrix. Convertendo para um data.frame:
ret.df = 
  ret.df %>% 
  data.frame() %>% 
  rownames_to_column('data') %>% 
  mutate(data = as.yearmon(data))
head(ret.df)

ret.xts = xts(x = ret.df[, -1], order.by = ret.df$data)
ret.xts %>% xts::first()
ret.df %>% xts::first()


# ser� que o modelo � bom?
# plota os retornos de ITUB4
plot(ret.df$ITUB4, type = 'l', ylab = 'retornos cc', main = 'ITUB4', lwd = 2)
quantmod::chartSeries(ret.xts$ITUB4, name = 'ITUB4')

# os dados parecem variar em torno de uma m�dia
abline(h = mean(ret.df$ITUB4), col = 'red')
# hip�tese: os retornos cc seguem uma distribui��o normal
hist(ret.df$ITUB4, breaks = 12, col = 'slateblue')
qqnorm(ret.df$ITUB4)
qqline(ret.df$ITUB4)

# como � gerada a qqnorm?
x = quantile(rnorm(1e4), probs = seq(0, 1, by=0.01))
plot(x, seq(0, 1, by=0.01), type='l')
y = quantile(rnorm(1e4), probs = seq(0, 1, by=0.01))
plot(y, seq(0, 1, by=0.01), type='l')
plot(x, y, type='l')

xseq = rnorm(n = 60, mean = mean(ret.df$ITUB4), sd = sd(ret.df$ITUB4))
qqnorm(xseq)
qqline(xseq)
hist(xseq, col='slateblue')

# como � a cara da qqnorm com kurtosis?
a = rnorm(5000, 0, 2)
b = rnorm(1000, -2, 4)
c = rnorm(1000, 2, 4)
babyGotKurtosis = c(a, b, c)
hist( babyGotKurtosis , freq=FALSE)
qqnorm(babyGotKurtosis)
qqline(babyGotKurtosis)

# estat�stica amostral: extraindo os par�metros do modelo
# a matriz de covariancias
sigma.mat = cov(ret.xts)
sigma.mat

# o valor esperado dos retornos
mu.vec = apply(ret.xts, 2, mean)
mu.vec

# a volatilidade dos ativos 
sd.vec = apply(ret.xts, 2, sd)
sd.vec
# tamb�m poderia ser obtida de sigma.mat
sqrt(diag(sigma.mat))

# matriz de correla��es
cor.mat = cor(ret.xts)
cor.mat

# visualizando as correla��es
pairs(ret.df, pch = 16)

library(corrplot)
corrplot(cor.mat)
corrplot.mixed(cor.mat)
# mais bonitinho...
corrplot.mixed(cor(ret.xts), tl.pos = 'lt', diag = 'u')

library(PerformanceAnalytics)
chart.Correlation(ret.xts)

# macete... extraindo os valores das correla��es da matriz de correlacoes
cor.vec = cor.mat[lower.tri(cor.mat)]
cor.vec

# como rapidamente dar nomes �s dimens�es?
pares = t(combn(codigos, 2))
pares
cor.df = data.frame(pares, cor.vec)
colnames(cor.df) = c('at1', 'at2', 'corr')
cor.df

# um gr�fico interessante...
plot(sd.vec, mu.vec, pch=16, 
     xlim = c(0, max(sd.vec)), ylim = c(0.004, max(mu.vec)),
     xlab = expression(sigma), ylab = expression(mu), 
     main = expression(paste(mu, ' X ', sigma)))
text(sd.vec, mu.vec, labels = codigos, pos = 4, cex = 0.7)

# qual ativo � melhor?
# essas retas ajudam?
# As retas de Sharpe
rf = 0.4638/100
rf
segments(0, rf, sd.vec, mu.vec, col = 1:6)

# hip�tese: o valor esperado do retorno e a volatilidade s�o constantes
# itub4
plot(ret.df$data, ret.df$ITUB4, 
     type = "l", lwd=2,
     main = 'ITUB4',
     ylab = expression(mu[ITUB4]),
     xlab = 'ano')
# petrobr�s
plot(ret.df$data, ret.df$PETR4, 
     type = "l", lwd=2,
     main = 'PETR4',
     ylab = expression(mu[PETR4]),
     xlab = 'ano')


############################################################
# DEMO 1a
############################################################
# Gerador de n�meros aleat�rios com distribui��o normal
aux = qnorm(runif(1e6))
hist(aux, col = 'lightblue', breaks = 100, freq = F)
# em Excel
# NORM.INV � o equivalente ao qnorm()
# NORM.DIST � o equivalente ao pnorm()
# =NORM.INV(RAND();0;1)


############################################################
# DEMO 2a
############################################################
# Simula��o de Monte Carlo
# Simulando retornos mensais para ABEV3 usando o modelo CER
# desconsiderando as covariancias...
ret.sim = data.frame(ABEV3 = rnorm(60, mean = mu.vec['ABEV3'], sd = sd.vec['ABEV3']))
# comparando as duas s�ries...
layout(1:2)
plot(ret.sim$ABEV3, type = 'l', lwd = 2, col = 'blue', main = 'ABEV3 Simula��o')
plot(ret.df$ABEV3, type = "l", lwd = 2, col = 'blue', main = 'ABEV3')
layout(1)
# ou...
ts.plot(ret.df$ABEV3, ts(ret.sim$ABEV3), 
        col=c('black', 'red'), lwd=2)

# ainda mais �til, olhar para a frente!
ret.sim = data.frame(ABEV3 = rnorm(24, mean = mu.vec['ABEV3'], sd = sd.vec['ABEV3']))
# a evolu��o dos pre�os
# P0 � o pre�o de ABEV ao final do per�odo de observa��o
P0 = as.numeric(tail(close.zoo$ABEV3, 1))
precos_futuros = P0*exp(cumsum(c(0, ret.sim$ABEV3)))
datas_futuras = seq(as.Date(END), by = "month", length.out = 25)
plot(datas_futuras, precos_futuros, type = 'l', ylim = c(0, 40))
abline(h = P0, lty = 2)

# o princ�pio da lista do COE...
conta1 = 0      # encerramento com o pagamento de 1 cupom
conta2 = 0      # encerramento com o pagamento de 2 cupons
conta3 = 0      # encerramento com o pagamento de 3 cupons
conta4 = 0      # encerramento com o pagamento de 4 cupons
conta0 = 0      # encerramento sem pagamento de cupons
# la�o de Monte Carlo
for (i in 1:1e5) {
  ret.sim = data.frame(ABEV3 = rnorm(60, mean = mu.vec['PETR4'], sd = sd.vec['ABEV3']))
  pf = P0*exp(cumsum(ret.sim$ABEV3))
  if (pf[6] > P0) {
    conta1 = conta1 + 1
  } else if (pf[12] > P0) {
    conta2 = conta2 + 1
  } else if (pf[18] > P0) {
    conta3 = conta3 + 1
  } else if (pf[24] > P0) {
    conta4 = conta4 + 1
  } else {
    conta0 = conta0 + 1
  }
}
conta1/1e5
conta2/1e5
conta3/1e5
conta4/1e5
conta0/1e5

############################################################
# DEMO 2b
############################################################
# Simula��o de Monte Carlo multivari�vel
# Simulando retornos mensais para ABEV3 e PETR4 usando o modelo CER
# mas dessa vez levando em conta as covariancias

# Como funciona a rmvnorm? ############################
# decompondo Sigma nas matrizes E e Lambda
aux = eigen(sigma.mat)
E = aux$vectors
Lambda = diag(aux$values)
# verificando
sigma.mat
E %*% Lambda %*% t(E)

# gerando a matriz V
m = 1e7
n = length(codigos)
V = matrix(rnorm(m*n), m, n)

# hip�tese a matriz V tem vetor de m�dias igual a 0
# e matriz de correla��o igual a identidade
apply(V, 2, mean)
cor(V)
# ou, mais elegante...
aux = cor(V)
ifelse(abs(aux) < 1e-3, 0, aux)
# ou...
aux[abs(aux) < 1e-3] = 0
aux

# como transformar a matriz V em uma matriz de s�ries que tem as propriedades estat�sticas da s�rie hist�rica?
X = t(E %*% sqrt(Lambda) %*% t(V))
head(X)
# calculando as propriedades estat�sticas da matriz X
sigma.mat
cov(X)
abs(sigma.mat - cov(X)) > 1e-3

# no entanto,
mu.vec
apply(X, 2, mean)

# para corrigir as m�dias...
# qual � o comportamento quando eu somo uma matriz e um vetor?
matrix(0, 3, 3) + c(1, 2, 3)
# cada elemento do vetor � somado a todos os elementos na linha correspondente.
# Logo...
X = t(t(X) + mu.vec)

# verificando mais uma vez...
mu.vec
apply(X, 2, mean)

sigma.mat
cov(X)

cor(ret.df)
cor(X)

# mas, � mais f�cil fazer com o rmvnorm...
#######################

library(mvtnorm)
ret.sim = rmvnorm(n = 60, mean = mu.vec, sigma = sigma.mat)
rownames(ret.sim) = rownames(ret.df)
ret.sim = data.frame(ret.sim)

# comparando as s�ries
# ABEV3
layout(1:2)
plot(ret.sim$ABEV3, type = 'l', lwd = 2, col = 'blue', main = 'ABEV3 Simula��o')
plot(ret.df$ABEV3, type = "l", lwd = 2, col = 'blue', main = 'ABEV3')
layout(1)
# ou...
ts.plot(ts(ret.df$ABEV3), ret.sim$ABEV3, col=c('black', 'red'), lwd=2)

# PETR4
layout(1:2)
plot(ret.sim$PETR4, type = 'l', lwd = 2, col = 'blue', main = 'PETR4 Simula��o')
plot(ret.df[, 'PETR4'], type = "l", lwd = 2, col = 'blue', main = 'PETR4')
layout(1)
# ou...
ts.plot(ts(ret.df$PETR4), ret.sim$PETR4, col=c('black', 'red'), lwd=2)


############################################################
# DEMO 3
############################################################
# ver o arquivo covmat.mw e o slide 23
eps.mat = t(t(ret.xts)-mu.vec)
t(eps.mat)%*%eps.mat/(nrow(eps.mat)-1)
sigma.mat

# constru�ndo a matriz de covari�ncias no Excel
# https://www.youtube.com/watch?v=EAf-3vO2A0I
# write.csv2(x = ret.df, "d:/temp/temp.csv")
xlsx::write.xlsx(
  x = ret.df, 
  file = "d:/temp/temp.xlsx", 
  row.names = F)

# abrir o arquivo com o Excel

# copiando a matriz de covariancias para o clipboard...
write.table(
  x=sigma.mat, 
  file = 'clipboard-16384', 
  sep = '\t', 
  dec = ',',
  row.names = F, 
  col.names = F)


############################################################
# DEMO 4
############################################################
# A evolu��o do log pre�o (slide 18)
# pre�o de ABEV3 no in�cio do per�odo
P0 = head(close.df$ABEV3, 1)
# par�metros amostrais
mu.ABEV = mu.vec['ABEV3']
sd.ABEV = sd.vec['ABEV3']
# tirando o retorno esperado, o que sobra s�o as not�cias
eps.ABEV = ret.df$ABEV3 - mu.ABEV

# o crescimento esperado do log pre�o
t = 0:60
plot(t, log(P0) + t * mu.ABEV, type = 'l', ylab = '')
text(10, 2.6, labels = expression(paste(ln, ' ', P[0])+t*mu))

# http://vis.supstat.com/2013/04/mathematical-annotation-in-r/
# acrescentando a acumula��o de not�cias
plot(t, log(P0) + t * mu.ABEV + c(0, cumsum(eps.ABEV)), type = 'l', ylab = '')
text(15, 3.1, expression(ln(P[t])==ln(P[0])+t*mu+sum(epsilon[s], s==1, t)), cex = 1.5)

# transformando em pre�os de fechamento (slide 19)
# primeiro sem as not�cias...
plot(t, P0*exp(t*mu.ABEV), type = 'l', ylab='')
text(10, 15, expression(P[t]==paste(P[0], e^paste(t, mu))), cex=1.5)
# acrescentando as not�cias
plot(t, P0*exp(t*mu.ABEV)*exp(c(0, cumsum(eps.ABEV))), type = 'l', ylab='')
text(10, 16, expression(P[t]==paste(P[0], e^paste(t, mu), e^sum(epsilon[s],s==1,t))), cex=1.5)

# para valida��o...
plot(close.df$ABEV3, type = 'l')
# ou
ts.plot(close.df$ABEV3)

# estudo de cen�rios
# como poderia ter sido a evolu��o dos pre�os de ABEV?
ts.plot(close.df$ABEV3, ylim=c(5, 40))
# la�o de Monte Carlo para 5 simula��es
for(i in 1:5) {
  ret.sim = rmvnorm(n = 60, mean = mu.vec, sigma = sigma.mat)
  ret.sim.ABEV3 = ret.sim[, 'ABEV3']
  points(t, P0 * exp(c(0, cumsum(ret.sim.ABEV3))), type = 'l', col = i)
}

plot(t, t, type = 'n', ylab='', ylim=c(5, 40))
# la�o de Monte Carlo para 500 simula��es
for(i in 1:500) {
  ret.sim = rmvnorm(n = 60, mean = mu.vec, sigma = sigma.mat)
  ret.sim.ABEV3 = ret.sim[, 'ABEV3']
  points(t, P0 * exp(c(0, cumsum(ret.sim.ABEV3))), type = 'l', col = i)
}
# o crescimento esperado de AMBEV (slide 19)
points(t, P0*exp(t*mu.ABEV), type = 'l', lwd=3)
# acrescentando o intervalo de confian�a (slide 15)
points(t, P0*exp(t*mu.ABEV+2*sqrt(t)*sd.ABEV), type='l', col='red', lwd=3)
points(t, P0*exp(t*mu.ABEV-2*sqrt(t)*sd.ABEV), type='l', col='red', lwd=3)

############################################################
# DEMO 5
############################################################
# incerteza na m�dia
# propriedades do estimador usando Monte Carlo
aux = NULL
for (i in 1:1e4) {
  aux[i] = mean(rnorm(n = 60, mean = 0.008, sd = 0.03))
}

# histograma de aux
hist(aux, col = 'lightblue', breaks = 30)

# o valor esperado da m�dia. Seria uma surpresa se ele n�o fosse muito pr�ximo de 0.008
mean(aux)
# o standard error da m�dia. Bem menor do que 0.03...
# n�o confunda o standard error com o standard deviation...
# o standard error eu posso reduzir se eu usar amostras grandes. O standard deviation � uma caracter�stica do fen�meno
sd(aux)
# poderia ter sido calculado analiticamente como:
0.03/sqrt(60)


############################################################
# DEMO 6
############################################################
# Estimando os standard errors (SE)
nobs = nrow(ret.df)
nobs

muhat.vec = apply(ret.xts, 2, mean)
muhat.vec

sdhat.vec = apply(ret.xts, 2, sd)
sdhat.vec

# matriz de correla��es
corhat.mat = cor(ret.xts)

# macete... extraindo os valores das correla��es da matriz de correla��es
rhohat.vec = corhat.mat[lower.tri(corhat.mat)]
# como rapidamente dar nomes �s dimens�es?
pares = t(combn(codigos, 2))
pares
rhohat.df = data.frame(pares, rhohat.vec)
rhohat.df
colnames(rhohat.df) = c('at1', 'at2', 'corr')


# SE do retorno esperado (slide 35)
se.muhat = sdhat.vec/sqrt(nobs)
rbind(muhat.vec, se.muhat)
se.muhat/muhat.vec

# SE do desvio padr�o
se.sdhat = sdhat.vec/sqrt(2*nobs)
rbind(sdhat.vec, se.sdhat)
se.sdhat/sdhat.vec # por que todos s�o iguais???

# um gr�fico interessante...
# uma fun��o encontrada na Internet...
ellipse <- function(a, b, xc, yc, ...) {
  # a is the length of the axis parallel to the x-axis
  # b is the length of the axis parallel to the y-axis
  # xc and yc are the coordinates of the center of the ellipse
  # ... are any arguments that can be passed to function lines
  t = seq(0, 2*pi, by=pi/400)
  xt = xc + a*cos(t)
  yt = yc + b*sin(t)
  points(xt, yt, ...)
}

plot(sdhat.vec, muhat.vec, pch=16,
     ylab=expression(hat(mu)),
     xlab=expression(hat(sigma)),
     xlim = c(0, 0.16),
     ylim = c(-0.03, 0.05))
text(sdhat.vec, muhat.vec, labels = codigos, pos = 4)
# tra�ando intervalos de confian�a
ellipse(a = 2*se.sdhat, b = 2*se.muhat, sdhat.vec, muhat.vec, col=1:6)

# as retas de Sharpe
# As retas de Sharpe
rf = 0.4638/100
mapply(abline, a=rf, b=(muhat.vec - rf)/sdhat.vec, 
       col=1:6, lty = 3)

# SE da vari�ncia (slide 35)
sigma2hat.vec = diag(sigma.mat)
se.sigma2hat = sigma2hat.vec/sqrt(nobs/2)
rbind(sigma2hat.vec, se.sigma2hat)

# SE da correla��o (slide 34)
rhohat.vec = cor(ret.xts)[lower.tri(cor(ret.xts))]
se.rhohat = (1 - rhohat.vec^2)/sqrt(nobs)
rbind(rhohat.vec, se.rhohat/rhohat.vec*100)


############################################################
# DEMO 7
############################################################
# Intervalos de confian�a de 95% para a m�dia
mu.lower = muhat.vec - 2 * se.muhat
mu.upper = muhat.vec + 2 * se.muhat
mu.width = mu.upper - mu.lower
cbind(mu.lower, mu.upper, mu.width)

# intervalos de confian�a de 95% para a volatilidade
sd.lower = sdhat.vec - 2 * se.sdhat
sd.upper = sdhat.vec + 2 * se.sdhat
sd.width = sd.upper - sd.lower
cbind(sd.lower, sd.upper, sd.width)

# intervalos de confian�a de 95% para a correla��o
rho.lower = rhohat.vec - 2 * se.rhohat
rho.upper = rhohat.vec + 2 * se.rhohat
rho.width = rho.upper - rho.lower
cbind(rho.lower, rho.upper, rho.width)

# intervalos de confian�a largos -> estimativa imprecisa

############################################################
# DEMO 8
############################################################
# um esquenta...
dentro = NULL
for (i in 1:1e5) {
  amostra = rnorm(n = 60, mean = 1e-2, sd = 5e-2)
  mu = mean(amostra)
  se = sd(amostra)/sqrt(60)
  dentro[i] = (mu-1.959964*se)<1e-2 & (mu+1.959964*se)>1e-2
}
sum(dentro)/1e5

# Simula��o de Monte Carlo
library(mvtnorm)
n.obs = nrow(ret.df)
n.sim = 1e4

#set.seed(123)

# vamos calcular os SEs da m�dia, vari�ncia e desvio padr�o por MC e compar�-los
# com os valores anal�ticos

# aloca as matrizes para os valores dos par�metros obtidos das simula��es
muhat.mat.MC = matrix(data = 0, nrow = n.sim, ncol = length(codigos))
sig2hat.mat.MC = matrix(data = 0, nrow = n.sim, ncol = length(codigos))
sighat.mat.MC = matrix(data = 0, nrow = n.sim, ncol = length(codigos))

# Simula��o de Monte Carlo
for (sim in 1:n.sim) {
  ret.sim = rmvnorm(n = n.obs, mean = muhat.vec, sigma = sigma.mat)
  muhat.mat.MC[sim, ] = apply(ret.sim, 2, mean)
  sig2hat.mat.MC[sim, ] = apply(ret.sim, 2, var)
  sighat.mat.MC[sim, ] = apply(ret.sim, 2, sd)
}

# calcula o SE de mu e compara com o valor te�rico
apply(muhat.mat.MC, 2, mean)
muhat.vec

se.muhat.MC  = apply(muhat.mat.MC, 2, sd)
result = rbind(se.muhat.MC, se.muhat)
rownames(result) = c('SE MC', 'SE TEORICO')
result

# calcula o SE da vari�ncia e compara com o valor te�rico
se.sig2hat.MC = apply(sig2hat.mat.MC, 2, sd)
result = rbind(se.sig2hat.MC, se.sigma2hat)
rownames(result) = c('SE MC', 'SE TEORICO')
result

# calcula o SE do desvio padr�o e compara com o valor te�rico
se.sighat.MC = apply(sighat.mat.MC, 2, sd)
result = rbind(se.sighat.MC, se.sdhat)
rownames(result) = c('SE MC', 'SE TEORICO')
result

# Histogramas de 10000 simula��es de Monte Carlo para ABEV3
layout(mat = matrix(1:4, 2, 2, byrow = T))
hist(muhat.mat.MC[, 1], breaks = 30, col = 'slateblue', main = 'sim.means')
hist(sig2hat.mat.MC[, 1], breaks = 30, col = 'slateblue', main = 'sim.vars')
hist(sighat.mat.MC[, 1], breaks = 30, col = 'slateblue', main = 'sim.sds')
layout(1)

# para estudar em casa...
# determinando o standard error da correla��o usando MC
# aloca espa�o para o resultado das simula��es
cor.sim = matrix(0, n.sim, choose(length(codigos), 2))
# como dar nomes �s dimens�es?
combina = function(pares) {
  paste(pares[1, ], pares[2, ], sep = '/')
}
colnames(cor.sim) = combina(combn(codigos, 2))
head(cor.sim)

# loop de Monte Carlo
for (i in 1:n.sim)  {
  ret.df.sim = rmvnorm(n = n.obs, mean = muhat.vec, sigma = sigma.mat)
  cor.aux = cor(ret.df.sim)
  cor.sim[i, ] = cor.aux[lower.tri(cor.aux)]
}

# tabula os resultados
# m�dia das correla��es; standard error amostral; standard error te�rico
result = rbind(
  cor.vec,                  # correla��es amostrais
  apply(cor.sim, 2, mean),  # m�dia das correla��es MC
  apply(cor.sim, 2, sd),    # standard error amostral
  (1 - cor.vec^2)/sqrt(n.obs)   # SE te�rico
)
rownames(result) = c('correla��es amostrais', 'correla��es MC', 'SE MC', 'SE teorico')
colnames(result) = colnames(cor.sim)
result

# histogramas
for (i in 1:choose(length(codigos), 2)) {
  hist(cor.sim[, i], col = 'slateblue1', main = colnames(cor.sim)[i])
}

############################################################
# DEMO 9
############################################################
# Estimando o VaR no modelo CER
# determina os quantis a 5%
q.05 = qnorm(p = 0.05, mean = muhat.vec, sd = sdhat.vec)
# calcula o VeR 5%
W0 = 100000
VeR.05 = (exp(q.05) - 1) * W0
VeR.05
# ou, por quantis...
q.05 = apply(ret.xts, 2, quantile, 0.05)
VeR.05 = (exp(q.05) - 1) * W0
VeR.05

# comparando com uma aloca��o burra
x.vec = rep(1/6, 6)
x.vec
mu.port = t(x.vec) %*% mu.vec
mu.port
sd.port = sqrt(t(x.vec)%*%sigma.mat%*%x.vec)
q.05 = mu.port + sd.port * qnorm(0.05)
# calcula o VeR 5%
W0 = 100000
VeR.05 = (exp(q.05) - 1) * W0
VeR.05


############################################################
# DEMO 10
############################################################
# determinando o SE no VeR.05 usando MonteCarlo
n.sim = 1e4
n.obs = nrow(ret.df)

# o VeR calculado a partir da �nica amostra
q.05 = qnorm(p = 0.05, mean = muhat.vec, sd = sdhat.vec)
# calcula o VeR 5%
W0 = 100000
VeR.05 = (exp(q.05) - 1) * W0
VeR.05

# aloca espa�o para os resultados das simula��es
VeR.mat.MC = matrix(data = 0, nrow = n.sim, ncol = length(codigos))
colnames(VeR.mat.MC) = codigos

# loop de Monte Carlo
for (i in 1:n.sim)  {
  ret.sim = rmvnorm(n = n.obs, mean = muhat.vec, sigma = sigma.mat)
  mu.vec.MC = apply(ret.sim, 2, mean)
  sd.vec.MC = apply(ret.sim, 2, sd)
  q.05.vec = qnorm(p = 0.05, mean = mu.vec.MC, sd = sd.vec.MC)
  VeR.mat.MC[i, ] = (exp(q.05.vec) - 1) * W0
}

# tabulando os resultados
result = rbind(
  VeR.05,                    # VeR amostral
  apply(VeR.mat.MC, 2, mean),   # VeR m�dio, Monte Carlo
  apply(VeR.mat.MC, 2, sd)      # SE VeR MC
)
rownames(result) = c('VeR.05 amostral', 'VeR.05 m�dio MC', 'SE.VeR.05 MC')
result

# intervalos de confian�a
result = rbind(
  apply(VeR.mat.MC, 2, mean) - 2 * apply(VeR.mat.MC, 2, sd),   # Limite inferior
  apply(VeR.mat.MC, 2, mean) + 2 * apply(VeR.mat.MC, 2, sd)    # Limite superior
)
rownames(result) = c('Liminf', 'Limsup')
result


############################################################
# DEMO 11
############################################################
# maxdrawdown
perdas = rep(0, length.out = length(codigos))
names(perdas) = codigos
# ITUB4
itub4 = close.df$ITUB4
mdd = tseries::maxdrawdown(itub4)
mdd
# percentual
perdas['ITUB4'] = -mdd$maxdrawdown/itub4[mdd$from]
perdas
# gr�fico
plot(itub4, type = 'l', main = 'ITUB4')
segments(mdd$from, itub4[mdd$from], mdd$to, itub4[mdd$from], col="red")
segments(mdd$from, itub4[mdd$to], mdd$to, itub4[mdd$to], col="red")
mid <- (mdd$from + mdd$to)/2
arrows(mid, itub4[mdd$from], mid, itub4[mdd$to], col="red", length = 0.16)

# gr�fico underwater
datas = seq(as.Date(START)+30, as.Date(END), by='month')
x = xts(exp(ret.df$ITUB4)-1, datas)
PerformanceAnalytics::charts.PerformanceSummary(x)

# VALE3
vale3 = close.df$VALE3
mdd = tseries::maxdrawdown(vale3)
mdd
# percentual
perdas['VALE3'] = -mdd$maxdrawdown/vale3[mdd$from]
perdas['VALE3']
# gr�fico
plot(vale3, type = 'l', main = 'VALE3')
segments(mdd$from, vale3[mdd$from], mdd$to, vale3[mdd$from], col="red")
segments(mdd$from, vale3[mdd$to], mdd$to, vale3[mdd$to], col="red")
mid <- (mdd$from + mdd$to)/2
arrows(mid, vale3[mdd$from], mid, vale3[mdd$to], col="red", length = 0.16)

# gr�fico underwater
x = xts(exp(ret.df$VALE3)-1, datas)
charts.PerformanceSummary(x)

# BBDC4
bbdc4 = close.df$BBDC4
mdd = tseries::maxdrawdown(bbdc4)
mdd
# percentual
perdas['BBDC4'] = -mdd$maxdrawdown/bbdc4[mdd$from]
perdas['BBDC4']
# gr�fico
plot(bbdc4, type = 'l', main = 'BBDC4')
segments(mdd$from, bbdc4[mdd$from], mdd$to, bbdc4[mdd$from], col="red")
segments(mdd$from, bbdc4[mdd$to], mdd$to, bbdc4[mdd$to], col="red")
mid <- (mdd$from + mdd$to)/2
arrows(mid, bbdc4[mdd$from], mid, bbdc4[mdd$to], col="red", length = 0.16)

# gr�fico underwater
x = xts(exp(ret.df$BBDC4)-1, datas)
charts.PerformanceSummary(x)

# ABEV3
abev3 = close.df$ABEV3
mdd = maxdrawdown(abev3)
mdd
# percentual
perdas['ABEV3'] = -mdd$maxdrawdown/abev3[mdd$from]
perdas['ABEV3']
# gr�fico
plot(abev3, type = 'l', main = 'ABEV3')
segments(mdd$from, abev3[mdd$from], mdd$to, abev3[mdd$from], col="red")
segments(mdd$from, abev3[mdd$to], mdd$to, abev3[mdd$to], col="red")
mid <- (mdd$from + mdd$to)/2
arrows(mid, abev3[mdd$from], mid, abev3[mdd$to], col="red", length = 0.16)

# gr�fico underwater
x = xts(exp(ret.df$ABEV3)-1, datas)
charts.PerformanceSummary(x)

# B3SA3
b3sa3 = close.df$B3SA3
mdd = tseries::maxdrawdown(b3sa3)
mdd
# percentual
perdas['B3SA3'] = -mdd$maxdrawdown/b3sa3[mdd$from]
perdas['B3SA3']
# gr�fico
plot(b3sa3, type = 'l', main = 'B3SA3')
segments(mdd$from, b3sa3[mdd$from], mdd$to, b3sa3[mdd$from], col="red")
segments(mdd$from, b3sa3[mdd$to], mdd$to, b3sa3[mdd$to], col="red")
mid <- (mdd$from + mdd$to)/2
arrows(mid, b3sa3[mdd$from], mid, b3sa3[mdd$to], col="red", length = 0.16)

# gr�fico underwater
x = xts(exp(ret.df$B3SA3)-1, datas)
PerformanceAnalytics::charts.PerformanceSummary(x)

# PETR4
petr4 = close.df$PETR4
mdd = tseries::maxdrawdown(petr4)
mdd
# percentual
perdas['PETR4'] = -mdd$maxdrawdown/petr4[mdd$from]
perdas['PETR4']
# gr�fico
plot(petr4, type = 'l', main = 'PETR4')
segments(mdd$from, petr4[mdd$from], mdd$to, petr4[mdd$from], col="red")
segments(mdd$from, petr4[mdd$to], mdd$to, petr4[mdd$to], col="red")
mid <- (mdd$from + mdd$to)/2
arrows(mid, petr4[mdd$from], mid, petr4[mdd$to], col="red", length = 0.16)

# gr�fico underwater
x = xts(exp(ret.df$PETR4)-1, datas)
charts.PerformanceSummary(x)

# vetor de perdas
perdas

library(PerformanceAnalytics)
# row.names(ret.df) = as.Date(as.yearmon(row.names(ret.df)))
ret.df = 
  ret.df %>% 
  mutate(data = lubridate::as_date(data)) %>% 
  column_to_rownames('data')
#maxDrawdown(ret.df[, 'VALE3'])
#sortDrawdowns(findDrawdowns(ret.df[, 'VALE3', drop=FALSE]))
PerformanceAnalytics::maxDrawdown(exp(ret.df)-1)


indices = c(1:6)
chart.Drawdown(
  subset(ret.xts, select=indices), 
  main=paste0(paste(names(ret.xts)[indices], collapse = ' x ')),
  legend.loc='bottomleft', cex.legend=1.3, lwd=2,
) 

# levar para o tableau
# USAR O DUAL AXIS!!!!
aux = 
  .Last.value$Env$xdata %>% 
  data.frame() %>% 
  rownames_to_column('data')
write.table(aux, file = 'clipboard-16384', sep = '\t', dec = ',', row.names = F)

# na forma de uma tabela...
ret.xts = xts(ret.xts, order.by = lubridate::as_date(index(ret.xts)))
table = table.Drawdowns(ret.xts$ITUB4, top = 10)
table = table %>%
  dplyr::select(inicio=From, queda=Depth, recuperacao=To, meses=Length) %>%
  mutate(queda = 100*queda, meses = meses-1)
textplot(table, show.rownames = F)
title('ITUB4')

PainIndex(ret.xts)