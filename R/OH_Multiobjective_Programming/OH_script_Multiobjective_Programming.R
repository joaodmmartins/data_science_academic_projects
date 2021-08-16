##################################################################

# OPTIMIZAÇÃO HEURÍSTICA
# Trabalho Individual 1
# João Diogo Mendes Martins, n.º 93259

##################################################################


# Variáveis de decisão do problema:
# x1 = número de kits do tipo básico
# x2 = número de kits do tipo avançado
# x3 = número de kits do tipo premium

# Metas / Restrições Soft / Níveis de Aspiração:

# Meta 1: ajudar, pelo menos, 20% dos 11 milhões de habitantes do país (20% 
# corresponde a 2200000 habitantes)
# meta1 = 0.00003x1 + 0.000035x2 + 0.000054x3 + d1m >= 2.2, com d1m >=0
# Meta 2: enviar, pelo menos, 3000 kits premium
# meta2 = x3 + d2m >= 3000, com d2m >= 0
# Meta 3: custo da ajuda humanitária não deve exceder os 20 milhões de Euros
# meta3 = 0.0003x1 + 0.00035x2 + 0.00072x3 - d3p <= 20, com d3p >= 0

# contudo, os desvios d são de grandezas diferentes (milhões, milhares, e 
# dezenas de milhões, respectivamente); será necessária a introdução dos 
# seguintes desvios percentuais na função objectivo:
t1 <- 2.2 # número de milhões de habitantes mínimo que se pretende ajudar
t2 <- 3000 # número mínimo de kits premium que se pretende enviar
t3 <- 20 # valor máximo em milhões de euros que se pretende gastar

# além disto, como a organização atribui diferentes importâncias a cada meta, é
# também necessário definir pesos que reflictam  essa importância; de acordo com 
# a informação no enunciado, os pesos deverão ser:
p1 <- 0.007 # número de pontos de penalização por cada desvio percentual abaixo do nível 
            # de aspiração definido na meta 1
p2 <- 0.1 # número de pontos de penalização por cada desvio percentual abaixo do nível 
          # de aspiração definido na meta 2
p3 <- 0.0001 # número de pontos de penalização por cada desvio percentual abaixo do nível 
             # de aspiração definido na meta 3

# Restrições Hard:
# restrições de transporte:
# x1 + x2 + x3 <= 40000 # máximo de kits que podem ser enviados
# 120x1 + 180x2 + 220x3 <= 6000000 # máximo de 6000000kg de kits que poderão ser enviados

# condição para envio de kits premium: por cada 100 kits premium enviados, é 
# necessário enviar 1 médico, com um custo associado de 33000€ (0.033 milhões de euros)
# Por este motivo, a Meta 3 toma outra forma, no sentido de considerar estes 
# meta3 = 0.0003x1 + 0.00035x2 + 0.00072x3 + x3/100*0.033 - d3p <= 20, com d3p >= 0 
# <=> meta3 = 0.0003x1 +0.00035x2 + 0.00105x3 - d3p <= 20, com d3p >=0

# Função Objectivo que traduz o problema:
# O objectivo do problema é determinar o número de kits de cada tipo (x1, x2, 
# x3) que serão enviados para ajuda humanitária, atendendo às metas e 
# condicionantes definidas
# MinZ = p1*d1m/t1 + p2*d2m/t2 + p3*d3p/t3


### a) Quantos kits básicos, avançados e premium deve a organização enviar para 
      # o país?

# Resumindo a informação sobre o problema, temos o seguinte:

# minZ = p1*d1m/t1 + p2*d2m/t2 + p3*d3p/t3
# sa:
# 0.00003x1 + 0.000035x2 + 0.000054x3 + d1m >= 2200000
# x3 + d2m >= 3000
# 0.0003x1 +0.00035x2 + 0.00105x3 - d3p <= 20
# x1 + x2 + x3 <= 40000
# 120x1 + 180x2 + 220x3 <= 6000000
# x1, x2, x3, d1m, d2m, d3p >= 0

library(lpSolveAPI)

# Definição das colunas das restrições:
x1_col <- c(0.00003, 0, 0.0003, 1, 120)
x2_col <- c(0.000035, 0, 0.00035, 1, 180)
x3_col <- c(0.000054, 1, 0.00105, 1, 220)
d1m_col <- c(1, 0, 0, 0, 0)
d2m_col <- c(0, 1, 0, 0, 0)
d3p_col <- c(0, 0, -1, 0, 0)

## Modelo de Programação Linear por Metas, minimização da soma dos desvios

modelo_1 <- make.lp(5,6) # restrições, variáveis
set.column(modelo_1, 1, x1_col) 
set.column(modelo_1, 2, x2_col) 
set.column(modelo_1, 3, x3_col) 
set.column(modelo_1, 4, d1m_col)
set.column(modelo_1, 5, d2m_col)
set.column(modelo_1, 6, d3p_col)

# x1, x2, x3 são inteiros:
set.type(modelo_1, 1, "integer")
set.type(modelo_1, 2, "integer")
set.type(modelo_1, 3, "integer") 

# minZ = p1*d1m/t1 + p2*d2m/t2 + p3*d3p/t3
set.objfn(modelo_1, c(0,0,0,p1/t1,p2/t2,p3/t3)) # definição da função objectivo

set.constr.type(modelo_1, c(">=",">=","<=","<=","<=")) # sinal das restrições, pela ordem da matriz dos coeficientes
set.rhs(modelo_1, c(2.2,3000,20,40000,6000000)) # construção do right hand side das restrições

# Renomeamos as variáveis e as restrições:
RowNames <- c('Meta 1', 'Meta 2', 'Meta 3', 'Restrição Hard 1', 'Restrição Hard 2')
ColNames <- c('x1', 'x2', 'x3', 'd1-', 'd2-', 'd3+')
dimnames(modelo_1) <- list(RowNames, ColNames)

lp.control(modelo_1, sense=c("min"))
name.lp(modelo_1, 'Modelo a)')
solve.lpExtPtr(modelo_1)
(modelo_1_val <- get.objective(modelo_1)) # valor óptimo
(modelo_1_sol <- get.variables(modelo_1)) # solução óptima

# Determinar número de habitantes ajudados:
modelo_1_sol[1]*0.00003+ modelo_1_sol[2]*0.000035 + modelo_1_sol[3]*0.000054
# Kits premium enviados:
modelo_1_sol[3]
# Total de euros gastos:
modelo_1_sol[1]*0.0003 + modelo_1_sol[2]*0.00035 + modelo_1_sol[3]*0.00105
# Peso total dos kits:
modelo_1_sol[1]*120+ modelo_1_sol[2]*180 + modelo_1_sol[3]*220
# Número total de kits enviados:
modelo_1_sol[1]+ modelo_1_sol[2] + modelo_1_sol[3]



### b) A organização reavaliou os níveis de importância atribuídos a cada uma das três metas.
# Em consequência, foi decidido dar mais importância à Meta 3: 15 pontos por cada €1 milhão acima do nível de aspiração (€20 milhões).
# Em relação às restantes metas, manteve as mesmas penalidades. Sob este cenário, quantos kits básicos, avançados e premium deve a organização enviar para o país?

# De acordo com a reformulação do problema é necessário reajustar a imporância da meta 3
p3b <- 0.0015 # peso associado ao desvio da meta 3

# Mantendo-se tudo o resto igual, é somente necessário redefinir a função objectivo e calculamos de novo os resultados da implementação do modelo

modelo_2 <- make.lp(5,6) # restrições, variáveis
set.column(modelo_2, 1, x1_col) 
set.column(modelo_2, 2, x2_col) 
set.column(modelo_2, 3, x3_col) 
set.column(modelo_2, 4, d1m_col)
set.column(modelo_2, 5, d2m_col)
set.column(modelo_2, 6, d3p_col)

set.type(modelo_2, 1, "integer")
set.type(modelo_2, 2, "integer")
set.type(modelo_2, 3, "integer") 

# minZ = p1*d1m/t1 + p2*d2m/t2 + p3b*d3p/t3
set.objfn(modelo_2, c(0,0,0,p1/t1,p2/t2,p3b/t3)) # definição da função objectivo

set.constr.type(modelo_2, c(">=",">=","<=","<=","<=")) # sinal das restrições, pela ordem da matriz dos coeficientes
set.rhs(modelo_2, c(2.2,3000,20,40000,6000000)) # construção do right hand side das restrições

# Renomeamos as variáveis e as restrições:
RowNames <- c('Meta 1', 'Meta 2', 'Meta 3', 'Restrição Hard 1', 'Restrição Hard 2')
ColNames <- c('X1', 'X2', 'X3', 'd1-', 'd2-', 'd3+')
dimnames(modelo_2) <- list(RowNames, ColNames)

lp.control(modelo_2, sense=c("min"))
name.lp(modelo_2, 'Modelo b)')

solve.lpExtPtr(modelo_2)
(modelo_2_val <- get.objective(modelo_2)) # valor óptimo
(modelo_2_sol <- get.variables(modelo_2)) # solução óptima

# Determinar número de habitantes ajudados:
modelo_2_sol[1]*0.00003+ modelo_2_sol[2]*0.000035 + modelo_2_sol[3]*0.000054
# Kits premium enviados:
modelo_2_sol[3]
# Total de euros gastos:
modelo_2_sol[1]*0.0003 + modelo_2_sol[2]*0.00035 + modelo_2_sol[3]*0.00105
# Peso total dos kits:
modelo_2_sol[1]*120+ modelo_2_sol[2]*180 + modelo_2_sol[3]*220
# Número total de kits enviados:
modelo_2_sol[1]+ modelo_2_sol[2] + modelo_2_sol[3]



### c) A organização constatou que seria importante aumentar o rácio de médicos 
    # para os kits premium: enviar um médico por cada 75 kits.
    # Considerando as penalizações para as metas indicadas em b), quantos kits 
    # básicos, avançados e premium deve a organização enviar para o país?

# Aumentar o rácio de médicos para os kits premium implica uma alteração na meta 3, relativa ao custo da ajuda humanitária:
# onde antes tínhamos meta3 = 0.0003x1 + 0.00035x2 + 0.00072x3 + x3/100*0.033 - d3p <= 20, com d3p >= 0, passaremos a ter:
# meta 3 = 0.0003x1 + 0.00035x2 + 0.00072x3 + x3/75*0.033 - d3p <= 20, com d3p >= 0 <=>
# <=> meta 3 = 0.0003x1 + 0.00035x2 + 0.00116x3 - d3p <= 20, com d3p >= 0

# logo, a reformulação do problema consistirá em:
# minZ = p1*d1m/t1 + p2*d2m/t2 + p3b*d3p/t3
# sa:
# 0.00003x1 + 0.000035x2 + 0.000054x3 + d1m >= 2.2
# x3 + d2m >= 3000
# 0.0003x1 +0.00035x2 + 0.00116x3 - d3p <= 20
# x1 + x2 + x3 <= 40000
# 120x1 + 180x2 + 220x3 <= 6000000
# x1, x2, x3, d1m, d2m, d3p >= 0

# os pesos mantêm-se

# Definição das colunas das restrições:
x1_col <- c(0.00003, 0, 0.0003, 1, 120)
x2_col <- c(0.000035, 0, 0.00035, 1, 180)
x3_col <- c(0.000054, 1, 0.00116, 1, 220) # coluna redefinida
d1m_col <- c(1, 0, 0, 0, 0)
d2m_col <- c(0, 1, 0, 0, 0)
d3p_col <- c(0, 0, -1, 0, 0)

modelo_3 <- make.lp(5,6) # restrições, variáveis
set.column(modelo_3, 1, x1_col) 
set.column(modelo_3, 2, x2_col) 
set.column(modelo_3, 3, x3_col) 
set.column(modelo_3, 4, d1m_col)
set.column(modelo_3, 5, d2m_col)
set.column(modelo_3, 6, d3p_col)

set.type(modelo_3, 1, "integer")
set.type(modelo_3, 2, "integer")
set.type(modelo_3, 3, "integer") 

# minZ = p1*d1m/t1 + p2*d2m/t2 + p3b*d3p/t3
set.objfn(modelo_3, c(0,0,0,p1/t1,p2/t2,p3b/t3)) # definição da função objectivo

set.constr.type(modelo_3, c(">=",">=","<=","<=","<=")) # sinal das restrições, pela ordem da matriz dos coeficientes
set.rhs(modelo_3, c(2.2,3000,20,40000,6000000)) # construção do right hand side das restrições

# Renomeamos as variáveis e as restrições:
RowNames <- c('Meta 1', 'Meta 2', 'Meta 3', 'Restrição Hard 1', 'Restrição Hard 2')
ColNames <- c('X1', 'X2', 'X3', 'd1-', 'd2-', 'd3+')
dimnames(modelo_3) <- list(RowNames, ColNames)

lp.control(modelo_3, sense=c("min"))
name.lp(modelo_3, 'Modelo c)')

solve.lpExtPtr(modelo_3)
(modelo_3_val <- get.objective(modelo_3)) # valor óptimo
(modelo_3_sol <- get.variables(modelo_3)) # solução óptima

# Determinar número de habitantes ajudados:
modelo_3_sol[1]*0.00003+ modelo_3_sol[2]*0.000035 + modelo_3_sol[3]*0.000054
# Kits premium enviados:
modelo_3_sol[3]
# Total de euros gastos:
modelo_3_sol[1]*0.0003 + modelo_3_sol[2]*0.00035 + modelo_3_sol[3]*0.00116
# Peso total dos kits:
modelo_3_sol[1]*120+ modelo_3_sol[2]*180 + modelo_3_sol[3]*220
# Número total de kits enviados:
modelo_3_sol[1]+ modelo_3_sol[2] + modelo_3_sol[3]



### d) O orçamento que a organização tem disponível para a ajuda humanitária 
    # sofreu uma redução. Por essa razão, a organização não poderá mesmo 
    # ultrapassar os 20 milhões de euros em ajuda a este país. Devido a este 
    # corte orçamental, a organização decidiu manter a sua política original 
    # de enviar um médico por cada 100 kits premium. Quantos kits básicos, 
    # avançados e premium deve a organização enviar para o país, assumindo 
    # que as penalizações por não cumprimento das restantes duas metas continuam 
    # a ser as mesmas que em a)?

# Aquela que era uma meta/aspiração a atingir, é transformada numa restrição hard, não admitindo qualquer desvio;
# Por este motivo temos de proceder à reformulação do conjunto de metas e restrições:

# Mantêm-se as duas primeiras metas:
# Meta 1: ajudar, pelo menos, 20% dos 11 milhões de habitantes do país (20% 
# corresponde a 2200000 habitantes)
# meta1 = 0.00003x1 + 0.000035x2 + 0.000054x3 + d1m >= 2.2, com d1m >=0
# Meta 2: enviar, pelo menos, 3000 kits premium
# meta2 = x3 + d2m >= 3000, com d2m >= 0

# mantêm-se t1 3 t2; excluímos t3
# mantêm-se p1 e p2; excluímos p3

# Restrições Hard:
# restrições de transporte:
# x1 + x2 + x3 <= 40000 # máximo de kits que podem ser enviados
# 120x1 + 180x2 + 220x3 <= 6000000 # máximo de 6000000kg de kits que poderão ser enviados
# Restrição de custos: 
# 0.0003x1 + 0.00035x2 + 0.00105x3 <= 20 # custo da ajuda humanitária não poderá exceder os 20 milhões de Euros; considera-se aqui também a inclusão de um médico a 33000€ por cada 100 kits premium

# Nova Função Objectivo que traduz o problema:
# MinZ = p1*d1m/t1 + p2*d2m/t2

# Resumindo a informação sobre o problema, temos o seguinte:

# minZ = p1*d1m/t1 + p2*d2m/t2
# sa:
# 0.00003x1 + 0.000035x2 + 0.000054x3 + d1m >= 2.2
# x3 + d2m >= 3000
# 0.0003x1 +0.00035x2 + 0.00105x3 <= 20
# x1 + x2 + x3 <= 40000
# 120x1 + 180x2 + 220x3 <= 6000000
# x1, x2, x3, d1m, d2m >= 0

# Definição das colunas das restrições:

x1_col <- c(0.00003, 0, 1, 120, 0.0003)
x2_col <- c(0.000035, 0, 1, 180, 0.00035)
x3_col <- c(0.000054, 1, 1, 220, 0.00105)
d1m_col <- c(1, 0, 0, 0, 0)
d2m_col <- c(0, 1, 0, 0, 0)

## Modelo de Programação Linear por Metas, minimização da soma dos desvios

modelo_4 <- make.lp(5,5) # restrições, variáveis
set.column(modelo_4, 1, x1_col) 
set.column(modelo_4, 2, x2_col) 
set.column(modelo_4, 3, x3_col) 
set.column(modelo_4, 4, d1m_col)
set.column(modelo_4, 5, d2m_col)

set.type(modelo_4, 1, "integer")
set.type(modelo_4, 2, "integer")
set.type(modelo_4, 3, "integer") 

# minZ = p1*d1m/t1 + p2*d2m/t2
set.objfn(modelo_4, c(0,0,0,p1/t1,p2/t2)) # definição da função objectivo

set.constr.type(modelo_4, c(">=",">=","<=","<=","<=")) # sinal das restrições, pela ordem da matriz dos coeficientes
set.rhs(modelo_4, c(2.2,3000,40000,6000000, 20)) # construção do right hand side das restrições

# Renomeamos as variáveis e as restrições:
RowNames <- c('Meta 1', 'Meta 2', 'Restrição Hard 1','Restrição Hard 2', 'Restrição Hard 3')
ColNames <- c('X1', 'X2', 'X3', 'd1-', 'd2-')
dimnames(modelo_4) <- list(RowNames, ColNames)

lp.control(modelo_4, sense=c("min"))
name.lp(modelo_4, 'Modelo d)')

solve.lpExtPtr(modelo_4)
(modelo_4_val <- get.objective(modelo_4)) # valor óptimo
(modelo_4_sol <- get.variables(modelo_4)) # solução óptima

# Determinar número de habitantes ajudados:
modelo_4_sol[1]*0.00003+ modelo_4_sol[2]*0.000035 + modelo_4_sol[3]*0.000054
# Kits premium enviados:
modelo_4_sol[3]
# Total de euros gastos:
modelo_4_sol[1]*0.0003 + modelo_4_sol[2]*0.00035 + modelo_4_sol[3]*0.00105
# Peso total dos kits:
modelo_4_sol[1]*120+ modelo_4_sol[2]*180 + modelo_4_sol[3]*220
# Número total de kits enviados:
modelo_4_sol[1]+ modelo_4_sol[2] + modelo_4_sol[3]