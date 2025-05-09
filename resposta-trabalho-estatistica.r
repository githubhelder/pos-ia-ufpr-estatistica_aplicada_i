
# configurar o caminho do banco de dados
setwd("/pasta)

# Carregar os dados
load("salarios.RData")

# 1. Gráficos e tabelas
# a) Gráficos box-plot, histograma e resultados

# Boxplot para idades
par(mfrow=c(1,2))
boxplot(salarios$age, main="Idade da Esposa", ylab="Anos", col="lightblue")
boxplot(salarios$husage, main="Idade do Marido", ylab="Anos", col="lightgreen")

## Análise comparativa dos resultados
cat("\nANÁLISE COMPARATIVA:\n")
cat("----------------------------------------\n")
cat("1. COMPARAÇÃO DOS BOXPLOTS:\n")
cat("- Amplitude total (diferença entre máximo e mínimo):\n")
cat("  Esposas:", diff(range(salarios$age)), "anos\n")
cat("  Maridos:", diff(range(salarios$husage)), "anos\n\n")

cat("- Mediana (linha central no boxplot):\n")
cat("  Esposas:", median(salarios$age), "anos\n")
cat("  Maridos:", median(salarios$husage), "anos\n\n")

cat("- Intervalo interquartil (IQR, altura da caixa):\n")
cat("  Esposas:", IQR(salarios$age), "anos\n")
cat("  Maridos:", IQR(salarios$husage), "anos\n\n")


# Histogramas
# Resetar configuração de plotagem
par(mfrow=c(1,2))

hist(salarios$age, main="Distribuição Idade Esposa", xlab="Anos", col="lightblue", breaks=15)
hist(salarios$husage, main="Distribuição Idade Marido", xlab="Anos", col="lightgreen", breaks=15)

#COMPARAÇÃO DOS HISTOGRAMAS:
cat("2. COMPARAÇÃO DOS HISTOGRAMAS:\n")
cat("- Forma da distribuição:\n")
age_skew <- ifelse(mean(salarios$age) > median(salarios$age), "positiva", "negativa")
husage_skew <- ifelse(mean(salarios$husage) > median(salarios$husage), "positiva", "negativa")
cat("  Esposas: Assimetria", age_skew, "\n")
cat("  Maridos: Assimetria", husage_skew, "\n\n")

cat("- Faixa etária mais frequente:\n")
cat("  Esposas:", names(which.max(table(round(salarios$age)))), "anos\n")
cat("  Maridos:", names(which.max(table(round(salarios$husage)))), "anos\n")



# b) (15 pontos) Elaborar a tabela de frequencias das variáveis “age” (idade da esposa) e 
# “husage” (idade do marido) e comparar os resultados
# Tabela de frequência para idade da esposa

freq_age <- table(cut(salarios$age, breaks=seq(15, 75, by=5)))
freq_husage <- table(cut(salarios$husage, breaks=seq(15, 75, by=5)))
# Exibir tabelas
print("Frequência Idade Esposa:")
print(freq_age)

print("Frequência Idade Marido:")
print(freq_husage)

# Principais Diferenças

#Faixa Etária Dominante:
#  Esposas: 35-40 anos (976)
#  Maridos: 35-40 anos (913) e 40-45 anos (892) quase equivalentes
#Distribuição em Idades Jovens:
#  Esposas têm mais casos nas faixas 20-25 (359) e 25-30 (803)
#  Maridos têm menos casos jovens: 20-25 (221) e 25-30 (630)
#Distribuição em Idades Avançadas:
#  Esposas: Nenhum caso acima de 60 anos
#  Maridos: 354 casos acima de 60 anos (256+67+31)
#Formato da Distribuição:
#  Esposas: Curva mais assimétrica à esquerda
#  Maridos: Distribuição mais uniforme nas idades maduras


#-------------------------------------------------------------------------------------------------------
#2 Medidas de posição e dispersão
#a) (15 pontos) Calcular a média, mediana e moda das variáveis “age” (idade da esposa) 
#e “husage” (idade do marido) e comparar os resultados
# Para idade da esposa
media_age <- mean(salarios$age)
mediana_age <- median(salarios$age)
moda_age <- as.numeric(names(sort(table(salarios$age), decreasing=TRUE)[1]))

# Para idade do marido
media_husage <- mean(salarios$husage)
mediana_husage <- median(salarios$husage)
moda_husage <- as.numeric(names(sort(table(salarios$husage), decreasing=TRUE)[1]))

# Resultados
cat("Idade da Esposa:\nMédia:", media_age, "\nMediana:", mediana_age, "\nModa:", moda_age, "\n")
cat("\nIdade do Marido:\nMédia:", media_husage, "\nMediana:", mediana_husage, "\nModa:", moda_husage, "\n")

# b)(15 pontos) Calcular a variância, desvio padrão e coeficiente de variação 
# das variáveis “age” (idade da esposa) e “husage” (idade do marido) e comparar os resultados

# Para idade da esposa
var_age <- var(salarios$age)
sd_age <- sd(salarios$age)
cv_age <- (sd_age/media_age)*100

# Para idade do marido
var_husage <- var(salarios$husage)
sd_husage <- sd(salarios$husage)
cv_husage <- (sd_husage/media_husage)*100

# Resultados
cat("Idade da Esposa:\nVariância:", var_age, "\nDesvio Padrão:", sd_age, "\nCoef. Variação:", cv_age, "%\n")
cat("\nIdade do Marido:\nVariância:", var_husage, "\nDesvio Padrão:", sd_husage, "\nCoef. Variação:", cv_husage, "%\n")


# PENDÊNCIAS - FALTA COMPRAR OS VALORES.






# 3. Testes paramétricos ou não paramétricos
# a) Teste de comparação

#install.packages("nortest")
#library(nortest)


# ----------------------------------------------------------
# ANÁLISE DE DADOS PAREADOS: IDADE DE ESPOSAS vs MARIDOS
# ----------------------------------------------------------

# Passo 1: Carregar pacotes necessários
if (!require("nortest")) install.packages("nortest")  # Para lillie.test()
if (!require("ggplot2")) install.packages("ggplot2")
library(nortest)
library(ggplot2)

# 1. Calcular diferenças entre idades
salarios$diff_idades <- salarios$husage - salarios$age

# 2. Teste de normalidade de Lilliefors
lillie_test <- lillie.test(salarios$diff_idades)
print(lillie_test)

# 3. Visualização gráfica
ggplot(salarios, aes(x = diff_idades)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  ggtitle("Distribuição das Diferenças de Idade") +
  xlab("Diferença (Marido - Esposa)") +
  ylab("Densidade")

# Q-Q plot
ggplot(salarios, aes(sample = diff_idades)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  ggtitle("Q-Q Plot das Diferenças de Idade")

# ----------------------------------------------------------
# Aplicação do teste apropriado
# ----------------------------------------------------------

if(lillie_test$p.value > 0.05) {
  # Caso paramétrico
  teste <- t.test(salarios$husage, salarios$age, paired = TRUE)
  cat("\nUSANDO TESTE T PARAMÉTRICO (dados normais)\n")
  intervalo_confianca <- salarios$conf.int
  estatistica <- "Média das diferenças"
  valor_central <- mean(salarios$diff_idades)
} else {
  # Caso não-paramétrico
  teste <- wilcox.test(salarios$husage, salarios$age, paired = TRUE, conf.int = TRUE)
  cat("\nUSANDO TESTE DE WILCOXON PAREADO (dados não-normais)\n")
  intervalo_confianca <- teste$conf.int
  estatistica <- "Mediana das diferenças"
  valor_central <- median(salarios$diff_idades)
}

# Resultados do teste
print(teste)

# Intervalo de confiança
cat("\nINTERVALO DE CONFIANÇA 95%:\n")
print(intervalo_confianca)

# ----------------------------------------------------------
# ANÁLISE COMPLEMENTAR
# ----------------------------------------------------------

# Estatísticas descritivas
cat("\nESTATÍSTICAS DESCRITIVAS:\n")
cat("Esposas (age):\n")
print(summary(salarios$age))
cat("\nMaridos (husage):\n")
print(summary(salarios$husage))
cat("\nDiferenças (husage - age):\n")
print(summary(salarios$diff_idades))

# Boxplot comparativo
ggplot(salarios) +
  geom_boxplot(aes(x = "Esposas", y = age), fill = "pink") +
  geom_boxplot(aes(x = "Maridos", y = husage), fill = "lightblue") +
  labs(title = "Comparação de Idades", y = "Idade", x = "Grupo")

# Justificativa
cat("\nJUSTIFICATIVA:\n")
if(lillie_test$p.value > 0.05) {
  cat("O teste de Lilliefors (p =", round(lillie_test$p.value, 4), 
      ") não rejeitou a normalidade.\nPortanto, usamos o teste t pareado.\n")
} else {
  cat("O teste de Lilliefors (p =", round(lillie_test$p.value, 4), 
      ") rejeitou a normalidade.\nPortanto, usamos o teste de Wilcoxon pareado.\n")
}

# Conclusao
cat("\nCONCLUSÃO:\n")
cat(estatistica, ":", round(valor_central, 2), "anos\n")
cat("Intervalo de confiança 95%: [", round(intervalo_confianca[1], 2), ",", round(intervalo_confianca[2], 2), "]\n")

if(teste$p.value > 0.05) {
  cat("Não há evidências de diferença significativa (p =", round(teste$p.value, 4), ")\n")
} else {
  cat("Há diferença significativa (p =", round(teste$p.value, 4), ")\n")
}

