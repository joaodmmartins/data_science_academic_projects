################################################################################
## LCD - PL ## Introdução a Modelos Dinâmicos
# Grupo 13:
#   Catarina Castanheira, n.º 92478
#   João Martins, n.º 93259
#   Joel Paula, n.º 93392
################################################################################

# OBJECTIVO: Previsão da percentagem da despesa total alocada para alimentação
#   num agregado familiar de Espanha (por mês) - desenvolver um modelo de
#   regressão múltipla que permita estimar a respetiva percentagem com base nas
#   variáveis contidas na base de dados.

################################################################################

### DESCRIÇÃO DA BASE DE DADOS ###

# Base de dados: BudgetFood.csv ; subamostra contendo os registos de 15601 a 
#   16900

# Variáveis:
# wfood: percentagem da despesa total que uma família gasta para alimentação por
#   mês 
# totexp: despesas totais do agregado familiar (moeda em peseta Espanhola;
#   1000 pesetas = 6 euros) 
# age: idade da pessoa de referência do agregado familiar 
# size: número de elementos do agregado familiar 
# town: tamanho da cidade onde a família tem o domicilio (5 categórias: 1 - 
#   cidade pequena,...,5 - cidade grande) 
# sex: sexo da pessoa de referência do agregado familiar (man, woman)

################################################################################

# pacotes R necessários para o projecto:
install.packages("olsrr")
install.packages("lmtest")
install.packages("tseries")
install.packages("car")
install.packages("ggplot2")
install.packages("MASS")
install.packages("leaps")
install.packages("caTools")
install.packages("gvlma")
library(gvlma)
library(caTools)
library(olsrr)
library(lmtest)
library(tseries)
library(car)
library(ggplot2)
library(MASS)
library(leaps)


# importação de dados:
budget_food <- read.csv("BudgetFood.csv", header = TRUE)

# para este projecto iremos considerar somente as linhas 15601 a 16900:
amostra_grupo13 = budget_food[c(15601:16900),]

# converter despesa total (totexp) de peseta para euro:
amostra_grupo13$totexp = amostra_grupo13$totexp*0.006 # 1 ESP = 0.006 EUR

# analisar a estrutura dos dados:
head(amostra_grupo13)
str(amostra_grupo13)

# verificar estatísticas básicas:
summary(amostra_grupo13)


# Histogramas para ver tipo de valores nos campos
hist(amostra_grupo13$wfood, breaks="FD", plot = TRUE)
hist(amostra_grupo13$totexp, breaks="FD", plot = TRUE)
hist(amostra_grupo13$age, breaks="FD", plot = TRUE)
barplot(table(amostra_grupo13$size), main="amostra_grupo13$size")
barplot(table(amostra_grupo13$town), main="amostra_grupo13$town")
barplot(table(amostra_grupo13$sex), main="amostra_grupo13$sex")
# TODO descrição da estatística descritiva para todas as variáveis
# moda de wfood
y <- data.frame(table(round(amostra_grupo13$wfood,digits = 2)))
y[y$Freq == max(y$Freq),1]
# moda de totexp
y <- data.frame(table(round(amostra_grupo13$totexp,digits = 0)))
y[y$Freq == max(y$Freq),1]
# moda de Age
y <- data.frame(table(amostra_grupo13$age))
y[y$Freq == max(y$Freq),1]
# moda de size
y <- data.frame(table(amostra_grupo13$size))
y[y$Freq == max(y$Freq),1]
# moda de town
y <- data.frame(table(amostra_grupo13$town))
y[y$Freq == max(y$Freq),1]
# moda de sex
y <- data.frame(table(amostra_grupo13$sex))
y[y$Freq == max(y$Freq),1]
# proporção de homens/mulher
cbind(c("man", "woman"), round(y$Freq/1300*100,2))

# verificar NA:
sum(is.na(amostra_grupo13$x)) # zero
sum(is.na(amostra_grupo13$wfood)) # zero
sum(is.na(amostra_grupo13$totexp)) # zero
sum(is.na(amostra_grupo13$age)) # zero
sum(is.na(amostra_grupo13$size)) # zero
sum(is.na(amostra_grupo13$town)) # zero
sum(is.na(amostra_grupo13$sex)) # zero

# Verificar erros de input de dados 
# Verificar valores únicos nas variáveis
sort(unique(amostra_grupo13$age)) # não se observam valores estranhos
sort(unique(amostra_grupo13$size)) # não se observam valores estranhos
sort(unique(amostra_grupo13$town)) # não se observam valores estranhos
# TODO Não existem town de tamanho 5. Isso é normal?

# Criar variáveis dummy para variável sexo (man = 0, woman = 1)
amostra_grupo13$sex <- ifelse(amostra_grupo13$sex == 'man', 0, 1)
summary(amostra_grupo13)

# excluimos a variável "x" por ser um identificador
amostra_grupo13_n = amostra_grupo13[,-c(1)]
head(amostra_grupo13_n)

# diagramas de dispersão para os pares de variáveis:
# pairs(amostra_grupo13_n)
scatterplotMatrix(amostra_grupo13_n, spread=FALSE, smoother.args=list(lty=1))

# estudo das correlações entre as variáveis
cor(amostra_grupo13_n) # matriz de correlação de Pearson
# a variável totexp é a que parece ter uma maior correlação (negativa) com a 
#   wfood (teoricamente faz sentido, uma vez que wfood é calculada a partir de 
#   totexp, é uma proporção desta)

# a dimensão do agregado (size) tem uma correção positiva baixa (0.03)
# a idade (age) tem uma correlação positiva média? (0.25)
# a dimensão da cidade (town) tem uma correlação negativa média? (-0.18)
# TODO [desenvolver] são observadas correlações médias entre as variáveis: 
#   age e totexp
#   size e totexp
#   size e age
#   town e totexp
#   town e age


# TODO [interpretar] modelo de regressão com melhor ajuste, com base no p-value:
fit = lm(wfood ~., data = amostra_grupo13_n)
summary(fit)
# Temos R^2 ajustado de 0.34
AIC(fit)
# a coluna 'sex' não é significativa, mas as restantes são todas significativas
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit) 
# Temos nitidamente os resíduos em forma de funil
par(mfrow=c(1,1))
qqPlot(fit, id.method="identify", simulate=TRUE, main="Q-Q Plot")


# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
ols_step_both_p(fit)
# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
ols_step_both_aic(fit)
# Em ambos os casos, a coluna 'sex' não é relevante para o estudo da regressão
# mas as restantes são, pois todas afetam o R^2 ajustado e o AIC (respetivamente)

# verificação dos pressupostos dos resíduos:
mean(fit$residuals) # média é nula
bptest(fit) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedasticos, a variância não é constante
bgtest(fit)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit$residuals) 
#   os resíduos são normalmente distribuídos
#   H0 - resíduos seguem distribuição normal
#   H0 é rejeitado, os resíduos não seguem uma distribuição normal
#   o pressuposto da normalidade dos resíduos não é verificado

# verificamos se existe multicolinearidade entre as variáveis independentes (VIF >5)
vif(fit)

# não existem variáveis multicolineares
# a média é nula: verificado
# variância constante: não se verifique homocedasticidade ==> heterocedasticidade
# ausência de correlação: não verificado ==> dependência dos resíduos
# normalidade dos resíduos: não se verifica

# evidências gráficas de existência de outliers influenciadores (15961)

# Vamos verificar se a causa da não-normalidade são os outliers
# Outlier test - teste de Bonferroni, em que H0: não existem outliers
outliers <- outlierTest(fit)
# Rejeitar H0 ==> existem outliers
print(outliers)
# Ver outliers:
amostra_grupo13_n[which(rownames(amostra_grupo13_n) %in% names(outliers[[1]])),]
# Tem 0 de despesa com alimentos!!!
# Vamos ver se há mais:
amostra_grupo13_n[which(amostra_grupo13_n$wfood == 0),]
# Vamos retirar os que não têm despesas com alimentos:
amostra_grupo13_n2 <- amostra_grupo13_n[-which(amostra_grupo13_n$wfood == 0),]

### Estudo outliers por variável
# Tamanhos agregado:
par(mfrow=c(1,1))
boxplot(amostra_grupo13_n2$size, main="size", plot=TRUE)
outliers <- boxplot(amostra_grupo13_n2$size, plot=FALSE)$out
print(sort(outliers))
amostra_grupo13_n2[which(amostra_grupo13_n2$size >= 10),]
# outliers são todas as amostras com 10 ou mais pessoas no agregado

# total expenses
boxplot(amostra_grupo13_n2$totexp, main="Total expenses", plot=TRUE)
outliers <- boxplot(amostra_grupo13_n2$totexp, plot=FALSE)$out
print(sort(outliers))
# age
boxplot(amostra_grupo13_n2$age, main="Age", plot=TRUE)
outliers <- boxplot(amostra_grupo13_n2$age, plot=FALSE)$out
print(sort(outliers))
# wfoods
boxplot(amostra_grupo13_n2$wfood, main="weight food expenses", plot=TRUE)
outliers <- boxplot(amostra_grupo13_n2$wfood, plot=FALSE)$out
print(sort(outliers))
# Não parece existir razão para eliminarmos estes outliers, nestas categorias



# Vamos ver como fica agora depois de retirado os outliers (despesa com alimentos = 0):
fit2 = lm(wfood ~ totexp+age+size+town, data = amostra_grupo13_n2)
summary(fit2)
# O R^2 ajustado ficou melhor (de 0,34, para 0.35).
# comparar os dois modelos pelo AIC
AIC(fit, fit2)
# Existiu uma alteração para melhor no AIC
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit2) 
# continuamos com resíduos em forma de funil, em vez de "ruído branco"
par(mfrow=c(1,1))
qqPlot(fit2, id.method="identify", simulate=TRUE, main="Q-Q Plot")
#continuamos com os mesmos problemas

# verificação dos pressupostos dos resíduos:
mean(fit2$residuals) # média nula
bptest(fit2) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedásticos, a variância não é constante
bgtest(fit2)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit2$residuals) 
#   os resíduos são normalmente distribuídos
#   H0 - resíduos seguem distribuição normal
#   H0 é rejeitado, os resíduos não seguem uma distribuição normal
#   o pressuposto da normalidade dos resíduos não é verificado

# não existem variáveis multicolineares
# a média é nula: verificado
# variância constante: não se verifique homocedasticidade ==> heterocedasticidade
# ausência de correlação: não verificado ==> dependência dos resíduos
# normalidade dos resíduos: não se verifica

# ainda parece termos alguns outliers (que estão a enviesar os resultados)

#testar outliers
# Outlier test - teste de Bonferroni, em que H0: não existem outliers
outliers <- outlierTest(fit2)
print(outliers)
# Não Rejeitar H0 ==> não existem mais outliers

# Vamos agora verificar influenciadores com algoritmo kabacoff:
influenceIndexPlot(fit2, id=list(n=3))
# Parecem existir influenciadores

cutoff <- 4/(nrow(amostra_grupo13_n2)-length(fit2$coefficients)-2)
plot(fit2, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")
cooksd <- cooks.distance(fit2)
influential <- names(cooksd)[(cooksd > cutoff)]
amostra_grupo13_n2[which(rownames(amostra_grupo13_n2) %in% influential),]
# São 73! São demasiados!

# detetar se existem influenciadores com algoritmo da professora
# distância de Cook
cooksd <- cooks.distance(fit2)
influential <- names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))]
amostra_grupo13_n2[which(rownames(amostra_grupo13_n2) %in% influential),]
# São 42, ainda parecem demasiados
####################################

# vamos ver se existem elementos extremos influenciadores 
influenceIndexPlot (fit2, id=list(n=3))
influencePlot(fit2, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")
# verificar os high leverage values acima de 0.03
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit2)

# Ver influenciadores (high leverage + outliers) gráficamente:
amostra_grupo13_n2[which(rownames(amostra_grupo13_n2) %in% c("15961","15783","15836","15986","16645")),]
# Verificamos que os valores apresentados não levantam suspeitas sobre um hipotético erro de inserção:
outliers <- boxplot(amostra_grupo13_n2$totexp, plot=FALSE)$out
print(sort(outliers))
# Remove influenciador
amostra_grupo13_n3 <- amostra_grupo13_n2[-which(rownames(amostra_grupo13_n2)  %in% c("15961")),]
# Vamos ver como fica agora:
fit3 = lm(wfood ~ totexp+age+size+town, data = amostra_grupo13_n3)
summary(fit3)
# O R^2 ajustado ficou marginalmente melhor (de 0.35, para 0.36).
# Verificar modelos com o critério de AIC
AIC(fit, fit2, fit3)
# O fit3 é melhor . Temos vindo a melhorar.
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit3) 
par(mfrow=c(1,1))
qqPlot(fit3, id.method="identify", simulate=TRUE, main="Q-Q Plot")
#continuamos com os mesmos problemas

##########################
# verificação dos pressupostos dos resíduos:
mean(fit3$residuals) # média nula
bptest(fit3) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedasticos, a variância não é constante
bgtest(fit3)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit3$residuals) 
#   os resíduos são normalmente distribuídos
#   H0 - resíduos seguem distribuição normal
#   H0 é rejeitado, os resíduos não seguem uma distribuição normal
#   o pressuposto da normalidade dos resíduos não é verificado
######################################

# Vamos tentar resolver a heterocedasticidade logaritmizando a variável dependente:
fit4 = lm(log(wfood) ~ totexp+age+size+town, data = amostra_grupo13_n3)
summary(fit4)
# O R^2 ajustado ficou melhor (de 0.36, para 0.39).
# o critério de AIC não deve ser aplicado, já que alteramos a variavel dependente
AIC(fit, fit2, fit3, fit4)
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit4) 
# O funil desaparece dos resíduos mas não parece ruido branco
# Os resíduos deixam de seguir um padrão normal
# possíveis outliers com high leverage
outliers <- outlierTest(fit4)
print(outliers)

par(mfrow=c(1,1))
influenceIndexPlot (fit4, id=list(n=3))
influencePlot(fit4, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")
amostra_grupo13_n3[which(rownames(amostra_grupo13_n3) %in% c("16254","15836","15783","16645")),]
# ver outros casos de despesas em alimentação muito baixas parecidas com o 16254
amostra_grupo13_n3[which(amostra_grupo13_n3$wfood <= 0.05),]

# Remove outliers influenciadores
amostra_grupo13_n4 <- amostra_grupo13_n3[-which(rownames(amostra_grupo13_n3)  %in% c("16254","15836")),]
# rever agora
fit4 = lm(log(wfood) ~ totexp+age+size+town, data = amostra_grupo13_n4)
summary(fit4)
# O R^2 ajustado ficou melhor (de 0.39, para 0.41).
# o critério de AIC fica diferente, pq as premissas são outras ("nova" variável dependente)
AIC(fit, fit2, fit3, fit4)
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit4) 
# funil nos resíduos mais disperso, mas tb não é ruido branco
# resíduos não seguem forma normal
par(mfrow=c(1,1))
qqPlot(fit4, id.method="identify", simulate=TRUE, main="Q-Q Plot")
#continuamos com os mesmos problemas

##########################
# verificação dos pressupostos dos resíduos:
mean(fit4$residuals) # média nula
bptest(fit4) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedasticos, a variância não é constante
bgtest(fit4)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit4$residuals) 
#   os resíduos são normalmente distribuídos
#   H0 - resíduos seguem distribuição normal
#   H0 é rejeitado, os resíduos não seguem uma distribuição normal
#   o pressuposto da normalidade dos resíduos não é verificado
######################################

# Vamos ver se podemos melhorar a normalidade dos resíduos transformando variáveis:
# Ver Kabacoff pag. 199 e 200, tabela 8.5
boxTidwell(log(wfood)~totexp+age+size+town, data=amostra_grupo13_n4)
# sugere usarmos totexp^0.42 =+/- sqrt(totexp)
# sugere usar size^0.3 = +/- sqrt(size)
# Também sugere usarmos uma transformação aproximadamente 'town' = town^3

# Vamos experimentar aplicar de acordo com o verificado acima:
# Vamos ver como fica agora:
fit5 = lm(log(wfood) ~ sqrt(totexp)+age+sqrt(size)+I(town^3), data = amostra_grupo13_n4)
summary(fit5)
# O R^2 ajustado ficou melhor (de 0.41, para 0.44).
AIC(fit, fit2, fit3, fit4, fit5)
# ficou melhor que o anterior
ols_step_both_p(fit5)
ols_step_both_aic(fit5)
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit5) 
# Os resíduos já não têm funil mas não é ruído branco
par(mfrow=c(1,1))
qqPlot(fit5, id.method="identify", simulate=TRUE, main="Q-Q Plot")
# verificação dos pressupostos dos resíduos:
mean(fit5$residuals) # TODO média nula? SIM!
bptest(fit5) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedasticos, a variância não é constante
bgtest(fit5)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit5$residuals) 
#   os resíduos são normalmente distribuídos
#   H0 - resíduos seguem distribuição normal
#   H0 é rejeitado, os resíduos não seguem uma distribuição normal
#   o pressuposto da normalidade dos resíduos não é verificado

# vamnos ver se adicionando combinações de variáveis melhora
fit5b = lm(log(wfood) ~ sqrt(totexp)+age+sqrt(size)+I(town^3)+totexp*size+totexp*age+totexp*town+size*age+size*town, data = amostra_grupo13_n4)
summary(fit5b)
# O R^2 ajustado ficou melhor (de 0.44, para 0.45).
AIC(fit, fit2, fit3, fit4, fit5, fit5b)
# O AIC melhora ligeiramente
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit5b) 
par(mfrow=c(1,1))
# os resíduos voltam a mostrar o formato de funil, muito longe do ruído branco
qqPlot(fit5b, id.method="identify", simulate=TRUE, main="Q-Q Plot")
# pressupostos
mean(fit5b$residuals) # TODO média nula? SIM!
bptest(fit5b) 
bgtest(fit5b)
jarque.bera.test(fit5b$residuals) 
# nada de novo - não se verificam os 3 pressupostos habitsuais
# vamos experimentar dar pesos para resolver heterocedasticidade e auto-correlação de erros:
resi1 <- fit5b$residuals
varfunc.ols <- lm((resi1^2) ~ ., data = amostra_grupo13_n4)
varfunc1 <- (varfunc.ols$fitted.values)
# h <- sqrt(fit5b$residuals^2)
# yhat<- predict(fit5b,data=amostra_grupo13_n)
# w <- 1/yhat

# Vamos ver como fica agora:
fit5c = lm(log(wfood) ~ sqrt(totexp)+age+sqrt(size)+I(town^3)+totexp*size+totexp*age+totexp*town+size*age+size*town, data = amostra_grupo13_n4, weights = 1/sqrt(varfunc1))
summary(fit5c)
# O R qudrado não melhora
AIC(fit, fit2, fit3, fit4, fit5, fit5b, fit5c)
# o AIC melhora
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit5c) 
# mas os problemas mantêm-se
par(mfrow=c(1,1))
qqPlot(fit5c, id.method="identify", simulate=TRUE, main="Q-Q Plot")
# pressupostos
mean(fit5c$residuals) # TODO média nula? SIM!
bptest(fit5c) 
bgtest(fit5c)
jarque.bera.test(fit5c$residuals) 
#####################

# abandonamos o logaritmo da variável dependente
# Vamos tentar resolver acrescentando relações entre variáveis:
fit6 = lm(wfood ~ totexp+totexp*size+totexp*age+totexp*town+size*age+size*town+age*town+age+size+town, data = amostra_grupo13_n4)
summary(fit6)
# O R^2 ajustado ficou melhor (de 0.36 no fit 3, para 0.37).
AIC(fit, fit2, fit3, fit6)
# O AIC melhorou


# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
ols_step_both_p(fit6)
# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
ols_step_both_aic(fit6)
# Em ambos os casos, a coluna 'sex' não é relevante para o estudo da regressão
# totexp, totexp*size, totexp*town, age, town, size, size*age são relevantes
# totexp*age, size*town, age*town não são interessantes

# Vamos agora usar só as significativas:
fit7 = lm(wfood ~ totexp+totexp*size+totexp*town+size*age+age+size+town, data = amostra_grupo13_n4)
summary(fit7)
# O R^2 ajustado ficou igual (de 0.37).
# comparando o critério de AIC
AIC(fit, fit2, fit3, fit6, fit7)
# Ficou ligeiramente melhor

options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit7) 
par(mfrow=c(1,1))
qqPlot(fit7, id.method="identify", simulate=TRUE, main="Q-Q Plot")
#continuamos com os mesmos problemas

##########################
# verificação dos pressupostos dos resíduos:
mean(fit7$residuals) # média nula
bptest(fit7) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedasticos, a variância não é constante
bgtest(fit7)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit7$residuals) 
#   os resíduos são normalmente distribuídos
#   H0 - resíduos seguem distribuição normal
#   H0 é rejeitado, os resíduos não seguem uma distribuição normal
#   o pressuposto da normalidade dos resíduos não é verificado

# Vamos experimentar transformar variáveis independentes, sem logaritmizar a dependente
boxTidwell(wfood ~ totexp+age+size+town, data=amostra_grupo13_n4)
# sugere usarmos totexp^0.26 =+/- log(totexp) ou sqrt(totexp)
# sugere usar size^0.39 = +/- sqrt(size)
# Também sugere usarmos uma transformação aproximadamente 'town' = town^3

# Vamos ver como fica agora:
fit8 = lm(wfood ~ log(totexp)+totexp*size+totexp*town+size*age+age+sqrt(size)+I(town^3), data = amostra_grupo13_n4)
summary(fit8)
# O R^2 ajustado ficou melhor (de 0.37, para 0.41).
AIC(fit, fit2, fit3, fit6, fit7, fit8)
# O AIC ficou bastante melhor
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit8) 
par(mfrow=c(1,1))
qqPlot(fit8, id.method="identify", simulate=TRUE, main="Q-Q Plot")
# verificação dos pressupostos dos resíduos:
mean(fit8$residuals) # média nula? SIM!
bptest(fit8) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedasticos, a variância não é constante
bgtest(fit8)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit8$residuals) 
#   os resíduos são normalmente distribuídos
#   H0 - resíduos seguem distribuição normal
#   H0 é rejeitado, os resíduos não seguem uma distribuição normal
#   o pressuposto da normalidade dos resíduos não é verificado


# verificar se podemos eliminar variáveis e melhora
leaps <- regsubsets(wfood ~ log(totexp)+totexp*size+totexp*town+size*age+age+sqrt(size)+I(town^3), data=amostra_grupo13_n4, nbest=5)
plot(leaps, scale="adjr2")
subsets(leaps, statistic="cp",
        main="Cp Plot for All Subsets Regression")
abline(1,1,lty=2,col="red")

# Trying it out
fit9 = lm(wfood ~ log(totexp)+totexp*town+age+sqrt(size)+I(town^3), data = amostra_grupo13_n4)
summary(fit9)
# O R^2 ajustado ficou semelhante (de +/- 0.41).
AIC(fit, fit2, fit3, fit6, fit7,fit8,fit9)
# O AIC melhorou
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit9) 
par(mfrow=c(1,1))
qqPlot(fit9, id.method="identify", simulate=TRUE, main="Q-Q Plot")

# verificação dos pressupostos dos resíduos:
mean(fit9$residuals) # média nula? SIM!
bptest(fit9) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedasticos, a variância não é constante
bgtest(fit9)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit9$residuals) 
#   os resíduos são normalmente distribuídos
#   H0 - resíduos seguem distribuição normal
#   H0 é rejeitado, os resíduos não seguem uma distribuição normal
#   o pressuposto da normalidade dos resíduos não é verificado


# Detetar padrões...
scatterplotMatrix(amostra_grupo13_n4)
cor(amostra_grupo13_n4)
######

# vamos experimentar dar pesos para resolver heterocedasticidade e auto-corrrelação de erros:
N <- length(amostra_grupo13_n4$wfood) # nº total amostras

# Vamos ver como fica agora:
fit10 = lm(wfood ~ log(totexp)+totexp*town+age+sqrt(size)+I(town^3), data = amostra_grupo13_n4, weights = 1/(sqrt(1:N)))
summary(fit10)
# O R^2 ajustado ficou pior (de 0.41, para 0.39).
AIC(fit, fit2, fit3, fit6, fit7, fit8, fit9, fit10)
# O AIC tb ficou pior
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit10) 
# Graficamente, tb não venso melhorias
par(mfrow=c(1,1))
qqPlot(fit10, id.method="identify", simulate=TRUE, main="Q-Q Plot")

# verificação dos pressupostos dos resíduos:
mean(fit10$residuals) # média nula? SIM!
bptest(fit10) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedasticos, a variância não é constante
bgtest(fit10)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit10$residuals) 
#   os resíduos são normalmente distribuídos
#   H0 - resíduos seguem distribuição normal
#   H0 é rejeitado, os resíduos não seguem uma distribuição normal
#   o pressuposto da normalidade dos resíduos não é verificado

# Não resolveu nenhum dos problemas.

# vamos experimentar dar pesos baseados nos valores preditos ajustados:
yhat<- predict(fit9,data=amostra_grupo13_n4)
w <- ifelse(yhat<=0,1,1/sqrt(yhat))

# Vamos ver como fica agora:
fit11 = lm(wfood ~ log(totexp)+age+I(size^0.23)+I(town^3), data = amostra_grupo13_n4, weights = w)
summary(fit11)
# O R^2 ajustado ficou melhor (de 0.41, para 0.43).
AIC(fit, fit2, fit3, fit6, fit7, fit8, fit9, fit10, fit11)
# o AIC tb ficou melhor
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit11) 
# Graficamente continuamso a ver os mesmo problemas
par(mfrow=c(1,1))
qqPlot(fit11, id.method="identify", simulate=TRUE, main="Q-Q Plot")

# verificação dos pressupostos dos resíduos:
mean(fit11$residuals) # média nula? SIM!
bptest(fit11) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedasticos, a variância não é constante
bgtest(fit11)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit11$residuals) 
#   os resíduos são normalmente distribuídos
#   H0 - resíduos seguem distribuição normal
#   H0 é rejeitado, os resíduos não seguem uma distribuição normal
#   o pressuposto da normalidade dos resíduos não é verificado


# definir uma regressão robusta com a função "rlm" da biblioteca MASS (e com pesos)
fit12 = rlm(wfood ~ log(totexp)+age+I(size^0.23)+I(town^3), data = amostra_grupo13_n4, weights = w)
summary(fit12)
summary(fit12)$r.squared
summary(fit12)$adj.r.squared
# O R^2 / ajustado não está disponível.
AIC(fit, fit2, fit3, fit6, fit7, fit8, fit9, fit10, fit11, fit12)
# o AIC ficou ligeiramente pior
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit12) 
# graficamente parece ter os mesmos problemas

# verificação dos pressupostos dos resíduos:
mean(fit12$residuals) # média nula? SIM!
bptest(fit12) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedasticos, a variância não é constante
bgtest(fit12)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit12$residuals) 
#   os resíduos são normalmente distribuídos
#   H0 - resíduos seguem distribuição normal
#   H0 é rejeitado, os resíduos não seguem uma distribuição normal
#   o pressuposto da normalidade dos resíduos não é verificado

# os problemas mantêm-se

# Vamos tentar outro tipo de peso:
h <- sqrt(fit9$residuals^2)
fit13 = lm(wfood ~ log(totexp)+age+I(size^0.23)+I(town^3), data = amostra_grupo13_n4, weights = 1/h)
summary(fit13)
# O R^2 ajustado ficou muito melhor (de 0.43, para 0.84).
AIC(fit, fit2, fit3, fit4, fit5, fit6, fit7,fit8, fit9, fit10, fit11, fit12, fit13)
# o AIC tb ficou muito melhor
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit13) 
# problemas não parecem resolvidos
par(mfrow=c(1,1))
qqPlot(fit13, id.method="identify", simulate=TRUE, main="Q-Q Plot")

# verificação dos pressupostos dos resíduos:
mean(fit13$residuals) # média nula? SIM!
bptest(fit13) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedasticos, a variância não é constante
bgtest(fit13)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit13$residuals) 
#   os resíduos são normalmente distribuídos
#   H0 - resíduos seguem distribuição normal
#   H0 é rejeitado, os resíduos não seguem uma distribuição normal
#   o pressuposto da normalidade dos resíduos não é verificado

# Vamos tentar outro tipo de peso (WLS):
resi1 <- fit9$residuals
varfunc.ols <- lm((resi1^2) ~ ., data = amostra_grupo13_n4)
varfunc1 <- (varfunc.ols$fitted.values)
fit14 = lm(wfood ~ log(totexp)+age+I(size^0.23)+I(town^3), data = amostra_grupo13_n4, weights = 1/varfunc1^0.5)
summary(fit14)
# O R^2 ajustado ficou muito pior (de 0.83, para 0.42).
AIC(fit, fit2, fit3, fit4, fit5, fit6, fit7,fit8, fit9, fit10, fit11, fit12, fit13, fit14)
# o AIC tb ficou muito pior
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit14) 
# problemas não parecem resolvidos
par(mfrow=c(1,1))

# verificação dos pressupostos dos resíduos:
mean(fit14$residuals) # TODO média nula? SIM!
bptest(fit14) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedasticos, a variância não é constante
bgtest(fit14)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit14$residuals) 
#   os resíduos são normalmente distribuídos
#   H0 - resíduos seguem distribuição normal
#   H0 é rejeitado, os resíduos não seguem uma distribuição normal
#   o pressuposto da normalidade dos resíduos não é verificado

# Remover outliers influenciadores
outlierTest(fit14)
influencePlot(fit14)
amostra_grupo13_n4[which(rownames(amostra_grupo13_n4) %in% c("16191","16258", "16271")),]
amostra_grupo13_n5 <- amostra_grupo13_n4[-which(rownames(amostra_grupo13_n4) %in% c("16191","16258", "16271")),]

# recalcular fit 9
fit9_a = lm(wfood ~ log(totexp)+totexp*town+age+sqrt(size)+I(town^3), data = amostra_grupo13_n5)
resi1 <- fit9_a$residuals
varfunc.ols <- lm((resi1^2) ~ ., data = amostra_grupo13_n5)
varfunc1 <- (varfunc.ols$fitted.values)
fit15 = lm(wfood ~ log(totexp)+age+I(size^0.23)+I(town^3), data = amostra_grupo13_n5, weights = 1/varfunc1^0.5)
summary(fit15)
# O R^2 ajustado ficou muito pior (de 0.83, para 0.42).
AIC(fit, fit2, fit3, fit4, fit5, fit6, fit7,fit8, fit9, fit10, fit11, fit12, fit13, fit14, fit15)
# o AIC ficou melhor
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(fit15) 
# problemas não parecem resolvidos (regressou o funil)
par(mfrow=c(1,1))
mean(fit15$residuals) # média nula? Sim, próximo de zero.
bptest(fit15) # A não-rejeição da H0 (isto é p-value > 0.05) conduz a verificação 
#   do pressuposto (variância é constante); neste caso é rejeitado H0, logo este 
#   pressuposto não está a ser verificado
#   os resíduos são não-homocedasticos, a variância não é constante
bgtest(fit15)   
#   os resíduos são independentes - ausência de correlação
#   H0 - resíduos independentes
#   H0 é rejeitado, os resíduos são correlacionados
jarque.bera.test(fit15$residuals) 
# Nada ficou resolvido retirando os outliers influenciadores

####################################################################################
# Vamos comparar os vários modelos
# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
par(mfrow=c(1,1))
pr1 <-predict(fit,amostra_grupo13_n)
plot(pr1, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit1")

lines(amostra_grupo13_n$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual1 <- amostra_grupo13_n$wfood
prediction1 <- fit$fitted.values[which(actual1 != 0)]
actual1 <- actual1[which(actual1 != 0)]
n <- length(amostra_grupo13_n$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE1 <- (1/n) * sum(abs((actual1 - prediction1)/actual1))
MAPE1

#
par(mfrow=c(1,1))
pr2 <-predict(fit2,amostra_grupo13_n2)
plot(pr2, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit2")

lines(amostra_grupo13_n2$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual2 <- amostra_grupo13_n2$wfood
prediction2 <- fit2$fitted.values
n <- length(amostra_grupo13_n2$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE2 <- (1/n) * sum(abs((actual2 - prediction2)/actual2))
MAPE2

#
par(mfrow=c(1,1))
pr3 <-predict(fit3,amostra_grupo13_n3)
plot(pr3, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit3")

lines(amostra_grupo13_n3$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual3 <- amostra_grupo13_n3$wfood
prediction3 <- fit3$fitted.values
n <- length(amostra_grupo13_n3$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE3 <- (1/n) * sum(abs((actual3 - prediction3)/actual3))
MAPE3

#
par(mfrow=c(1,1))
pr4 <-predict(fit4,amostra_grupo13_n4)
plot(exp(pr4), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit4")

lines(amostra_grupo13_n4$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual4 <- amostra_grupo13_n4$wfood
prediction4 <- exp(fit4$fitted.values)
n <- length(amostra_grupo13_n4$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE4 <- (1/n) * sum(abs((actual4 - prediction4)/actual4))
MAPE4

#
par(mfrow=c(1,1))
pr5 <-predict(fit5,amostra_grupo13_n4)
plot(exp(pr5), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit5")

lines(amostra_grupo13_n4$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual5 <- amostra_grupo13_n4$wfood
prediction5 <- exp(fit5$fitted.values)
n <- length(amostra_grupo13_n4$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE5 <- (1/n) * sum(abs((actual5 - prediction5)/actual5))
MAPE5

#
par(mfrow=c(1,1))
pr6 <-predict(fit6,amostra_grupo13_n4)
plot(pr6, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit6")

lines(amostra_grupo13_n4$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual6 <- amostra_grupo13_n4$wfood
prediction6 <- fit6$fitted.values
n <- length(amostra_grupo13_n4$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE6 <- (1/n) * sum(abs((actual6 - prediction6)/actual6))
MAPE6

#
par(mfrow=c(1,1))
pr7 <-predict(fit7,amostra_grupo13_n4)
plot(pr7, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit7")

lines(amostra_grupo13_n4$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual7 <- amostra_grupo13_n4$wfood
prediction7 <- fit7$fitted.values
n <- length(amostra_grupo13_n4$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE7 <- (1/n) * sum(abs((actual7 - prediction7)/actual7))
MAPE7

#
par(mfrow=c(1,1))
pr8 <-predict(fit8,amostra_grupo13_n4)
plot(pr8, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit8")

lines(amostra_grupo13_n4$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual8 <- amostra_grupo13_n4$wfood
prediction8 <- fit8$fitted.values
n <- length(amostra_grupo13_n4$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE8 <- (1/n) * sum(abs((actual8 - prediction8)/actual8))
MAPE8

#
par(mfrow=c(1,1))
pr9 <-predict(fit9,amostra_grupo13_n4)
plot(pr9, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit9")

lines(amostra_grupo13_n4$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual9 <- amostra_grupo13_n4$wfood
prediction9 <- fit9$fitted.values
n <- length(amostra_grupo13_n4$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE9 <- (1/n) * sum(abs((actual9 - prediction9)/actual9))
MAPE9

#
par(mfrow=c(1,1))
pr10 <-predict(fit10,amostra_grupo13_n4)
plot(pr10, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit10")

lines(amostra_grupo13_n4$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual10 <- amostra_grupo13_n4$wfood
prediction10 <- fit10$fitted.values
n <- length(amostra_grupo13_n4$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE10 <- (1/n) * sum(abs((actual10 - prediction10)/actual10))
MAPE10

#
par(mfrow=c(1,1))
pr11 <-predict(fit11,amostra_grupo13_n4)
plot(pr11, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit11")

lines(amostra_grupo13_n4$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual11 <- amostra_grupo13_n4$wfood
prediction11 <- fit11$fitted.values
n <- length(amostra_grupo13_n4$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE11 <- (1/n) * sum(abs((actual11 - prediction11)/actual11))
MAPE11

#
par(mfrow=c(1,1))
pr12 <-predict(fit12,amostra_grupo13_n4)
plot(pr12, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit12")

lines(amostra_grupo13_n4$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual12 <- amostra_grupo13_n4$wfood
prediction12 <- fit12$fitted.values
n <- length(amostra_grupo13_n4$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE12 <- (1/n) * sum(abs((actual12 - prediction12)/actual12))
MAPE12

#
par(mfrow=c(1,1))
pr13 <-predict(fit13,amostra_grupo13_n4)
plot(pr13, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit13")

lines(amostra_grupo13_n4$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual13 <- amostra_grupo13_n4$wfood
prediction13 <- fit13$fitted.values
n <- length(amostra_grupo13_n4$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE13 <- (1/n) * sum(abs((actual13 - prediction13)/actual13))
MAPE13

#
par(mfrow=c(1,1))
pr14 <-predict(fit14,amostra_grupo13_n4)
plot(pr14, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit14")

lines(amostra_grupo13_n4$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual14 <- amostra_grupo13_n4$wfood
prediction14 <- fit14$fitted.values
n <- length(amostra_grupo13_n4$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE14 <- (1/n) * sum(abs((actual14 - prediction14)/actual14))
MAPE14

#
par(mfrow=c(1,1))
pr15 <-predict(fit15,amostra_grupo13_n5)
plot(pr15, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim = c(-0.5,1), xlim=c(0,1400), main="modelo fit15")

lines(amostra_grupo13_n5$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual15 <- amostra_grupo13_n5$wfood
prediction15 <- fit15$fitted.values
n <- length(amostra_grupo13_n5$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE15 <- (1/n) * sum(abs((actual15 - prediction15)/actual15))
MAPE15

erros <-c(MAPE1,MAPE2,MAPE3,MAPE4,MAPE5,MAPE6,MAPE7,MAPE8,MAPE9,MAPE10,MAPE11,MAPE12,MAPE13,MAPE14,MAPE15)
AICs <-c(AIC(fit),AIC(fit2),AIC(fit3),AIC(fit4),AIC(fit5),AIC(fit6),AIC(fit7),AIC(fit8),AIC(fit9),AIC(fit10), AIC(fit11),AIC(fit12),AIC(fit13),AIC(fit14),AIC(fit15))
R_quadrado <-c(summary(fit)$r.squared, summary(fit2)$r.squared, summary(fit3)$r.squared, summary(fit4)$r.squared, summary(fit5)$r.squared, summary(fit6)$r.squared, summary(fit7)$r.squared, summary(fit8)$r.squared, summary(fit9)$r.squared, summary(fit10)$r.squared, summary(fit11)$r.squared, summary(fit12)$r.squared, summary(fit13)$r.squared, summary(fit14)$r.squared, summary(fit15)$r.squared)
R_quadrado_ajustado <-c(summary(fit)$adj.r.squared, summary(fit2)$adj.r.squared, summary(fit3)$adj.r.squared, summary(fit4)$adj.r.squared, summary(fit5)$adj.r.squared, summary(fit6)$adj.r.squared, summary(fit7)$adj.r.squared, summary(fit8)$adj.r.squared, summary(fit9)$adj.r.squared, summary(fit10)$adj.r.squared, summary(fit11)$adj.r.squared, summary(fit12)$adj.r.squared, summary(fit13)$adj.r.squared, summary(fit14)$adj.r.squared, summary(fit15)$adj.r.squared)
cbind(erros,AICs,R_quadrado, R_quadrado_ajustado)

################## Testar modelos ##############
set.seed(19090)
separar = sample.split(amostra_grupo13, SplitRatio = 0.70)
train = amostra_grupo13[separar,]
train = train[-which(train$wfood == 0),]
test = amostra_grupo13[!(separar),]
test = test[-which(test$wfood == 0),]
# verificar o número de observações de cada conjunto
nrow(train)
nrow(test)

# correr o modelo fit5
fit5_n = lm(log(wfood) ~ sqrt(totexp)+age+sqrt(size)+I(town^3), data = train)
yhat<- predict(fit5_n,data=train)


# previsão usando o modelo fit5_n sobre o conjunto de teste
pr5_n <-predict(fit5_n,test)
plot(exp(pr5_n), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,1), xlim=c(0,600), main="modelo 5_n")

lines(test$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual5_n<-test$wfood
prediction5_n <- exp(pr5_n)
n<-length(test$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE5_n <- (1/n) * sum(abs((actual5_n - prediction5_n)/actual5_n))
MAPE5_n

# previsão usando o modelo fit11_n sobre o conjunto de teste
fit9_n = lm(wfood ~ log(totexp)+totexp*town+age+sqrt(size)+I(town^3), data = train)
yhat<- predict(fit9_n,data=train)
w <- ifelse(yhat<=0,1,1/sqrt(yhat))

fit11_n = lm(wfood ~ log(totexp)+age+I(size^0.23)+I(town^3), data = train, weights = w)
yhat <- predict(fit11_n, data=train)
pr11_n <-predict(fit11_n, test)
plot(pr11_n, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,1), xlim=c(0,600), main="modelo 11_n")

lines(test$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão
actual11_n<-test$wfood
prediction11_n <- pr11_n
n <- length(test$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE11_n <- (1/n) * sum(abs((actual11_n - prediction11_n)/actual11_n))
MAPE11_n


# previsão usando o modelo fit12_n sobre o conjunto de teste
# fit9_n = lm(wfood ~ log(totexp)+totexp*town+age+sqrt(size)+I(town^3), data = train)
yhat<- predict(fit9_n,data=train)
w <- ifelse(yhat<=0,1,1/sqrt(yhat))
fit12_n = rlm(wfood ~ log(totexp)+age+I(size^0.23)+I(town^3), data = train, weights = w)
yhat <- predict(fit12_n, data=train)
pr12_n <-predict(fit12_n, test)
plot(pr12_n, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,1), xlim=c(0,600), main="modelo 12_n")

lines(test$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual12_n<-test$wfood
prediction12_n <- pr12_n
n <- length(test$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE12_n <- (1/n) * sum(abs((actual12_n - prediction12_n)/actual12_n))
MAPE12_n

# previsão usando o modelo fit13_n sobre o conjunto de teste
# fit9_n = lm(wfood ~ log(totexp)+totexp*town+age+sqrt(size)+I(town^3), data = train)
h <- sqrt(fit9_n$residuals^2)
fit13_n = lm(wfood ~ log(totexp)+age+I(size^0.23)+I(town^3), data = train, weights = 1/h)
pr13_n <-predict(fit13_n, test)
plot(pr13_n, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,1), xlim=c(0,600), main="modelo 13_n")

lines(test$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual13_n <- test$wfood
prediction13_n <- pr13_n
n <- length(test$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE13_n <- (1/n) * sum(abs((actual13_n - prediction13_n)/actual13_n))
MAPE13_n

# previsão usando o modelo fit14_n sobre o conjunto de teste
# fit9_n = lm(wfood ~ log(totexp)+totexp*town+age+sqrt(size)+I(town^3), data = train)
resi1 <- fit9_n$residuals
varfunc.ols <- lm((resi1^2) ~ ., data = train)
varfunc1 <- (varfunc.ols$fitted.values)
fit14_n = lm(wfood ~ log(totexp)+age+I(size^0.23)+I(town^3), data = train, weights = 1/varfunc1^0.5)

yhat <- predict(fit14_n, data=train)
pr14_n <-predict(fit14_n, test)
plot(pr14_n, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,1), xlim=c(0,600), main="modelo 14_n")

lines(test$wfood, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual14_n <- test$wfood
prediction14_n <- pr14_n
n <- length(test$wfood)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE14_n <- (1/n) * sum(abs((actual14_n - prediction14_n)/actual14_n))
MAPE14_n


#####
cbind(MAPE5_n,MAPE11_n,MAPE12_n,MAPE13_n,MAPE14_n)
