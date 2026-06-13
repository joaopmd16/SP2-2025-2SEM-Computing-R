# ============================================================================
# CHALLENGE SPRINT 4 - ANÁLISE BIVARIADA E MULTIVARIADA
# ============================================================================
# Nome: [INSIRA NOMES COMPLETOS DOS INTEGRANTES]
# RM: [INSIRA RMs DOS INTEGRANTES]
# Data: Outubro 2025
# ============================================================================

# Instalação e carregamento de pacotes necessários
if(!require(readxl)) install.packages("readxl")
if(!require(corrplot)) install.packages("corrplot")

# Atualizar ggplot2 se necessário
if(!require(ggplot2)) {
  install.packages("ggplot2")
} else {
  if(packageVersion("ggplot2") < "4.0.0") {
    message("Atualizando ggplot2...")
    install.packages("ggplot2")
  }
}

library(readxl)
library(ggplot2)
library(corrplot)

# GGally é opcional - tentar carregar, mas o código funciona sem ele
usar_ggally <- FALSE
tryCatch({
  if(!require(GGally)) install.packages("GGally")
  library(GGally)
  usar_ggally <- TRUE
  cat("GGally carregado com sucesso!\n")
}, error = function(e) {
  cat("GGally não disponível. Usando métodos alternativos.\n")
})

# Configuração para melhor visualização
options(scipen = 999)

# ============================================================================
# CARREGAMENTO DOS DADOS
# ============================================================================
# IMPORTANTE: Ajuste o caminho do arquivo para sua base de dados

# Tentar carregar o arquivo CSV
tryCatch({
  dados <- read.csv("dados_reais.csv", header = TRUE, sep = ",", dec = ".")
  cat("Arquivo 'dados_reais.csv' carregado com sucesso!\n")
}, error = function(e) {
  # Se o CSV não existir, tentar Excel
  tryCatch({
    dados <- read_excel("dados_reais.xlsx")
    cat("Arquivo 'dados_reais.xlsx' carregado com sucesso!\n")
  }, error = function(e2) {
    # Se nenhum arquivo existir, usar dados exemplo
    cat("Nenhum arquivo encontrado. Usando dados exemplo.\n")
    cat("IMPORTANTE: Coloque seu arquivo 'dados_reais.csv' ou 'dados_reais.xlsx' na pasta do projeto!\n\n")
    
    set.seed(123)
    n <- 50
    dados <<- data.frame(
      investimento_marketing = round(runif(n, 1000, 10000), 2),
      vendas = round(runif(n, 5000, 50000) + runif(n, 1000, 10000) * 2, 2),
      satisfacao_cliente = round(runif(n, 6, 10), 1),
      tempo_entrega = round(runif(n, 1, 5), 1)
    )
  })
})

# Visualizar primeiras linhas
cat("\n=== PRIMEIRAS LINHAS DA BASE DE DADOS ===\n")
head(dados)

cat("\n=== ESTATÍSTICAS DESCRITIVAS ===\n")
summary(dados)


# ============================================================================
# QUESTÃO 01 - ANÁLISE BIVARIADA
# ============================================================================
cat("\n\n")
cat("============================================================================\n")
cat("QUESTÃO 01 - ANÁLISE BIVARIADA\n")
cat("============================================================================\n")

# Definindo variáveis para análise bivariada
x <- dados$investimento_marketing
y <- dados$vendas

# ---------------------------------------------------------------------------
# a) GRÁFICO DE DISPERSÃO
# ---------------------------------------------------------------------------
cat("\na) GRÁFICO DE DISPERSÃO\n")

png("01a_grafico_dispersao_bivariada.png", width = 800, height = 600)
plot(x, y, 
     main = "Relação entre Investimento em Marketing e Vendas",
     xlab = "Investimento em Marketing (R$)",
     ylab = "Vendas (R$)",
     pch = 19,
     col = "steelblue",
     cex = 1.2)
grid()
dev.off()

# Também criar com ggplot2 para melhor qualidade
p1 <- ggplot(dados, aes(x = investimento_marketing, y = vendas)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  labs(title = "Relação entre Investimento em Marketing e Vendas",
       x = "Investimento em Marketing (R$)",
       y = "Vendas (R$)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

ggsave("01a_grafico_dispersao_bivariada_ggplot.png", p1, width = 10, height = 6)

# ---------------------------------------------------------------------------
# b) COVARIÂNCIA E CORRELAÇÃO LINEAR DE PEARSON
# ---------------------------------------------------------------------------
cat("\nb) COVARIÂNCIA E CORRELAÇÃO LINEAR DE PEARSON\n")

covariancia <- cov(x, y)
correlacao <- cor(x, y)

cat(sprintf("\nCovariância: %.2f\n", covariancia))
cat(sprintf("Correlação de Pearson (r): %.4f\n", correlacao))

# Interpretação da correlação
cat("\n--- INTERPRETAÇÃO ---\n")
if(abs(correlacao) >= 0.9) {
  interpretacao <- "muito forte"
} else if(abs(correlacao) >= 0.7) {
  interpretacao <- "forte"
} else if(abs(correlacao) >= 0.5) {
  interpretacao <- "moderada"
} else if(abs(correlacao) >= 0.3) {
  interpretacao <- "fraca"
} else {
  interpretacao <- "muito fraca ou inexistente"
}

direcao <- ifelse(correlacao > 0, "positiva", "negativa")

cat(sprintf("A correlação de %.4f indica uma relação %s %s.\n", 
            correlacao, interpretacao, direcao))
cat("Isso significa que, quando o investimento em marketing aumenta,\n")
cat(sprintf("as vendas tendem a %s.\n", 
            ifelse(correlacao > 0, "aumentar proporcionalmente", "diminuir")))

# ---------------------------------------------------------------------------
# c) GRÁFICO DE CORRELAÇÃO LINEAR DE PEARSON
# ---------------------------------------------------------------------------
cat("\nc) GRÁFICO DE CORRELAÇÃO LINEAR DE PEARSON\n")

# Matriz de correlação (mesmo com 2 variáveis, para demonstração)
dados_bivariada <- data.frame(
  "Investimento\nMarketing" = x,
  "Vendas" = y
)

matriz_cor_biv <- cor(dados_bivariada)

png("01c_grafico_correlacao_bivariada.png", width = 600, height = 600)
corrplot(matriz_cor_biv, 
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "Correlação Linear de Pearson",
         mar = c(0,0,2,0),
         number.cex = 1.2)
dev.off()

# ---------------------------------------------------------------------------
# d) RETA DE REGRESSÃO LINEAR SIMPLES E EQUAÇÃO
# ---------------------------------------------------------------------------
cat("\nd) REGRESSÃO LINEAR SIMPLES\n")

modelo_simples <- lm(y ~ x)
resumo <- summary(modelo_simples)

cat("\n--- EQUAÇÃO DA REGRESSÃO LINEAR SIMPLES ---\n")
beta0 <- coef(modelo_simples)[1]
beta1 <- coef(modelo_simples)[2]

cat(sprintf("Y = %.4f + %.4f * X\n", beta0, beta1))
cat(sprintf("Vendas = %.2f + %.4f * Investimento_Marketing\n\n", beta0, beta1))

cat("Interpretação:\n")
cat(sprintf("- Intercepto (β0): %.2f - Valor esperado de vendas quando investimento = 0\n", beta0))
cat(sprintf("- Coeficiente Angular (β1): %.4f - A cada R$ 1 investido em marketing,\n", beta1))
cat(sprintf("  espera-se um aumento de R$ %.4f nas vendas\n", beta1))

# Gráfico de dispersão com reta de regressão
png("01d_dispersao_com_reta_regressao.png", width = 800, height = 600)
plot(x, y,
     main = "Regressão Linear Simples: Vendas x Investimento",
     xlab = "Investimento em Marketing (R$)",
     ylab = "Vendas (R$)",
     pch = 19,
     col = "steelblue",
     cex = 1.2)
abline(modelo_simples, col = "red", lwd = 2)
grid()
legend("topleft", 
       legend = sprintf("Y = %.2f + %.4f*X", beta0, beta1),
       col = "red", lwd = 2, bty = "n")
dev.off()

# Versão ggplot2
p2 <- ggplot(dados, aes(x = investimento_marketing, y = vendas)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE, fill = "pink", alpha = 0.3) +
  labs(title = "Regressão Linear Simples: Vendas x Investimento em Marketing",
       subtitle = sprintf("Y = %.2f + %.4f*X", beta0, beta1),
       x = "Investimento em Marketing (R$)",
       y = "Vendas (R$)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "red"))

ggsave("01d_dispersao_com_reta_regressao_ggplot.png", p2, width = 10, height = 6)

# ---------------------------------------------------------------------------
# e) COEFICIENTE DE DETERMINAÇÃO (R²) E TESTE F
# ---------------------------------------------------------------------------
cat("\ne) COEFICIENTE DE DETERMINAÇÃO (R²) E TESTE F\n")

r2 <- resumo$r.squared
r2_ajustado <- resumo$adj.r.squared
f_estatistica <- resumo$fstatistic[1]
p_valor_f <- pf(f_estatistica, 
                resumo$fstatistic[2], 
                resumo$fstatistic[3], 
                lower.tail = FALSE)

cat(sprintf("\nR² (Coeficiente de Determinação): %.4f (%.2f%%)\n", r2, r2*100))
cat(sprintf("R² Ajustado: %.4f (%.2f%%)\n", r2_ajustado, r2_ajustado*100))
cat(sprintf("\nEstatística F: %.4f\n", f_estatistica))
cat(sprintf("p-valor: %.6f\n", p_valor_f))

cat("\n--- INTERPRETAÇÃO DO R² ---\n")
cat(sprintf("O R² de %.2f%% indica que %.2f%% da variabilidade nas vendas\n", 
            r2*100, r2*100))
cat("é explicada pelo investimento em marketing.\n")
cat(sprintf("Os %.2f%% restantes são explicados por outros fatores não incluídos no modelo.\n",
            (1-r2)*100))

cat("\n--- INTERPRETAÇÃO DO TESTE F ---\n")
if(p_valor_f < 0.05) {
  cat(sprintf("p-valor (%.6f) < 0.05: Rejeitamos H0\n", p_valor_f))
  cat("O modelo de regressão é estatisticamente significativo.\n")
  cat("Existe uma relação linear significativa entre as variáveis.\n")
} else {
  cat(sprintf("p-valor (%.6f) >= 0.05: Não rejeitamos H0\n", p_valor_f))
  cat("O modelo de regressão NÃO é estatisticamente significativo.\n")
}

# ---------------------------------------------------------------------------
# f) PREVISÃO
# ---------------------------------------------------------------------------
cat("\nf) PREVISÃO\n")

# Fazer previsões para diferentes valores de investimento
novos_valores <- data.frame(x = c(3000, 5000, 8000))
previsoes <- predict(modelo_simples, novos_valores, interval = "confidence", level = 0.95)

cat("\n--- PREVISÕES DE VENDAS ---\n")
for(i in 1:nrow(novos_valores)) {
  cat(sprintf("\nInvestimento: R$ %.2f\n", novos_valores$x[i]))
  cat(sprintf("  Vendas Previstas: R$ %.2f\n", previsoes[i, "fit"]))
  cat(sprintf("  Intervalo de Confiança 95%%: [R$ %.2f - R$ %.2f]\n", 
              previsoes[i, "lwr"], previsoes[i, "upr"]))
}


# ============================================================================
# QUESTÃO 02 - ANÁLISE MULTIVARIADA
# ============================================================================
cat("\n\n")
cat("============================================================================\n")
cat("QUESTÃO 02 - ANÁLISE MULTIVARIADA\n")
cat("============================================================================\n")

# Usando múltiplas variáveis explicativas
# Y = Vendas
# X1 = Investimento em Marketing
# X2 = Satisfação do Cliente
# X3 = Tempo de Entrega

# ---------------------------------------------------------------------------
# a) GRÁFICO DE DISPERSÃO (MATRIZ DE DISPERSÃO)
# ---------------------------------------------------------------------------
cat("\na) GRÁFICO DE DISPERSÃO (MATRIZ DE DISPERSÃO)\n")

png("02a_matriz_dispersao_multivariada.png", width = 1000, height = 1000)
pairs(dados,
      main = "Matriz de Dispersão - Análise Multivariada",
      pch = 19,
      col = "steelblue",
      cex = 0.8)
dev.off()

# Versão mais elaborada com ggpairs (se disponível)
if(usar_ggally) {
  p3 <- ggpairs(dados,
                title = "Matriz de Dispersão - Análise Multivariada",
                columnLabels = c("Invest.\nMarketing", "Vendas", 
                                 "Satisf.\nCliente", "Tempo\nEntrega"),
                lower = list(continuous = wrap("smooth", alpha = 0.3, color = "blue")),
                upper = list(continuous = wrap("cor", size = 5))) +
    theme_minimal()
  
  ggsave("02a_matriz_dispersao_multivariada_ggpairs.png", p3, width = 12, height = 12)
  cat("Gráfico ggpairs criado com sucesso!\n")
} else {
  cat("Usando apenas pairs() para matriz de dispersão.\n")
}

# ---------------------------------------------------------------------------
# b) MATRIZ DE COVARIÂNCIA E MATRIZ DE CORRELAÇÃO
# ---------------------------------------------------------------------------
cat("\nb) MATRIZ DE COVARIÂNCIA E MATRIZ DE CORRELAÇÃO\n")

matriz_cov <- cov(dados)
matriz_cor <- cor(dados)

cat("\n--- MATRIZ DE COVARIÂNCIA ---\n")
print(round(matriz_cov, 2))

cat("\n--- MATRIZ DE CORRELAÇÃO DE PEARSON ---\n")
print(round(matriz_cor, 4))

cat("\n--- INTERPRETAÇÃO DAS CORRELAÇÕES ---\n")
var_names <- colnames(dados)
for(i in 1:(ncol(dados)-1)) {
  for(j in (i+1):ncol(dados)) {
    cor_val <- matriz_cor[i, j]
    cat(sprintf("\n%s x %s: %.4f - ", var_names[i], var_names[j], cor_val))
    
    if(abs(cor_val) >= 0.9) {
      cat("Correlação MUITO FORTE")
    } else if(abs(cor_val) >= 0.7) {
      cat("Correlação FORTE")
    } else if(abs(cor_val) >= 0.5) {
      cat("Correlação MODERADA")
    } else if(abs(cor_val) >= 0.3) {
      cat("Correlação FRACA")
    } else {
      cat("Correlação MUITO FRACA")
    }
    
    cat(ifelse(cor_val > 0, " (positiva)", " (negativa)"))
  }
}

# ---------------------------------------------------------------------------
# c) GRÁFICO DE CORRELAÇÃO LINEAR DE PEARSON
# ---------------------------------------------------------------------------
cat("\n\nc) GRÁFICO DE CORRELAÇÃO LINEAR DE PEARSON\n")

png("02c_grafico_correlacao_multivariada.png", width = 800, height = 800)
corrplot(matriz_cor,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "Matriz de Correlação de Pearson - Análise Multivariada",
         mar = c(0,0,2,0),
         number.cex = 0.9,
         col = colorRampPalette(c("red", "white", "blue"))(200))
dev.off()

# Versão alternativa
png("02c_grafico_correlacao_multivariada_v2.png", width = 800, height = 800)
corrplot(matriz_cor,
         method = "circle",
         type = "lower",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "Matriz de Correlação - Método Circle",
         mar = c(0,0,2,0),
         number.cex = 0.8)
dev.off()

# ---------------------------------------------------------------------------
# d) EQUAÇÃO DA REGRESSÃO LINEAR MÚLTIPLA
# ---------------------------------------------------------------------------
cat("\nd) EQUAÇÃO DA REGRESSÃO LINEAR MÚLTIPLA\n")

# Modelo com todas as variáveis explicativas
modelo_multiplo <- lm(vendas ~ investimento_marketing + satisfacao_cliente + tempo_entrega,
                      data = dados)

resumo_multiplo <- summary(modelo_multiplo)

cat("\n--- RESUMO DO MODELO ---\n")
print(resumo_multiplo)

cat("\n--- EQUAÇÃO DA REGRESSÃO LINEAR MÚLTIPLA ---\n")
coefs <- coef(modelo_multiplo)
cat(sprintf("Y = %.4f + %.4f*X1 + %.4f*X2 + %.4f*X3\n",
            coefs[1], coefs[2], coefs[3], coefs[4]))

cat("\nOu seja:\n")
cat(sprintf("Vendas = %.2f + %.4f*Investimento_Marketing + %.4f*Satisfacao_Cliente + %.4f*Tempo_Entrega\n",
            coefs[1], coefs[2], coefs[3], coefs[4]))

cat("\n--- INTERPRETAÇÃO DOS COEFICIENTES ---\n")
cat(sprintf("β0 (Intercepto): %.2f - Valor base estimado de vendas\n", coefs[1]))
cat(sprintf("β1 (Invest. Marketing): %.4f - Mantendo outras variáveis constantes,\n", coefs[2]))
cat(sprintf("    cada R$ 1 de investimento aumenta as vendas em R$ %.4f\n", coefs[2]))
cat(sprintf("β2 (Satisf. Cliente): %.4f - Mantendo outras variáveis constantes,\n", coefs[3]))
cat(sprintf("    cada ponto de satisfação aumenta as vendas em R$ %.4f\n", coefs[3]))
cat(sprintf("β3 (Tempo Entrega): %.4f - Mantendo outras variáveis constantes,\n", coefs[4]))
cat(sprintf("    cada dia adicional de entrega %s as vendas em R$ %.4f\n", 
            ifelse(coefs[4] < 0, "reduz", "aumenta"), abs(coefs[4])))

# ---------------------------------------------------------------------------
# e) COEFICIENTE DE DETERMINAÇÃO MÚLTIPLO (R²) E TESTE F
# ---------------------------------------------------------------------------
cat("\ne) COEFICIENTE DE DETERMINAÇÃO MÚLTIPLO (R²) E TESTE F\n")

r2_mult <- resumo_multiplo$r.squared
r2_ajust_mult <- resumo_multiplo$adj.r.squared
f_estat_mult <- resumo_multiplo$fstatistic[1]
p_valor_f_mult <- pf(f_estat_mult,
                     resumo_multiplo$fstatistic[2],
                     resumo_multiplo$fstatistic[3],
                     lower.tail = FALSE)

cat(sprintf("\nR² Múltiplo: %.4f (%.2f%%)\n", r2_mult, r2_mult*100))
cat(sprintf("R² Ajustado: %.4f (%.2f%%)\n", r2_ajust_mult, r2_ajust_mult*100))
cat(sprintf("\nEstatística F: %.4f\n", f_estat_mult))
cat(sprintf("p-valor: %.6f\n", p_valor_f_mult))

cat("\n--- INTERPRETAÇÃO DO R² MÚLTIPLO ---\n")
cat(sprintf("O R² de %.2f%% indica que %.2f%% da variabilidade nas vendas\n",
            r2_mult*100, r2_mult*100))
cat("é explicada conjuntamente pelas variáveis:\n")
cat("  - Investimento em Marketing\n")
cat("  - Satisfação do Cliente\n")
cat("  - Tempo de Entrega\n")

cat("\n--- COMPARAÇÃO ENTRE MODELOS ---\n")
cat(sprintf("Modelo Simples - R²: %.2f%%\n", r2*100))
cat(sprintf("Modelo Múltiplo - R²: %.2f%%\n", r2_mult*100))
cat(sprintf("Melhoria no poder explicativo: %.2f pontos percentuais\n", 
            (r2_mult - r2)*100))

cat("\n--- INTERPRETAÇÃO DO TESTE F ---\n")
if(p_valor_f_mult < 0.05) {
  cat(sprintf("p-valor (%.6f) < 0.05: Rejeitamos H0\n", p_valor_f_mult))
  cat("O modelo de regressão múltipla é estatisticamente significativo.\n")
  cat("Pelo menos uma das variáveis explicativas tem relação significativa com vendas.\n")
} else {
  cat(sprintf("p-valor (%.6f) >= 0.05: Não rejeitamos H0\n", p_valor_f_mult))
  cat("O modelo de regressão múltipla NÃO é estatisticamente significativo.\n")
}

# ---------------------------------------------------------------------------
# f) PREVISÃO COM MÚLTIPLAS VARIÁVEIS
# ---------------------------------------------------------------------------
cat("\nf) PREVISÃO COM MÚLTIPLAS VARIÁVEIS\n")

# Criar cenários de previsão
cenarios <- data.frame(
  investimento_marketing = c(4000, 6000, 7500),
  satisfacao_cliente = c(7.5, 8.5, 9.0),
  tempo_entrega = c(3.0, 2.5, 2.0)
)

previsoes_mult <- predict(modelo_multiplo, cenarios, 
                          interval = "confidence", level = 0.95)

cat("\n--- PREVISÕES DE VENDAS (MODELO MÚLTIPLO) ---\n")
for(i in 1:nrow(cenarios)) {
  cat(sprintf("\nCENÁRIO %d:\n", i))
  cat(sprintf("  Investimento em Marketing: R$ %.2f\n", cenarios$investimento_marketing[i]))
  cat(sprintf("  Satisfação do Cliente: %.1f\n", cenarios$satisfacao_cliente[i]))
  cat(sprintf("  Tempo de Entrega: %.1f dias\n", cenarios$tempo_entrega[i]))
  cat(sprintf("  → Vendas Previstas: R$ %.2f\n", previsoes_mult[i, "fit"]))
  cat(sprintf("  → IC 95%%: [R$ %.2f - R$ %.2f]\n",
              previsoes_mult[i, "lwr"], previsoes_mult[i, "upr"]))
}

# Comparação com modelo simples
previsoes_simples_comp <- predict(modelo_simples, 
                                  data.frame(x = cenarios$investimento_marketing))

cat("\n--- COMPARAÇÃO: MODELO SIMPLES vs MÚLTIPLO ---\n")
for(i in 1:nrow(cenarios)) {
  cat(sprintf("\nCENÁRIO %d (Invest.: R$ %.2f):\n", i, cenarios$investimento_marketing[i]))
  cat(sprintf("  Modelo Simples: R$ %.2f\n", previsoes_simples_comp[i]))
  cat(sprintf("  Modelo Múltiplo: R$ %.2f\n", previsoes_mult[i, "fit"]))
  cat(sprintf("  Diferença: R$ %.2f\n", 
              previsoes_mult[i, "fit"] - previsoes_simples_comp[i]))
}


# ============================================================================
# EXPORTAR RESULTADOS
# ============================================================================
cat("\n\n")
cat("============================================================================\n")
cat("ANÁLISE CONCLUÍDA COM SUCESSO!\n")
cat("============================================================================\n")
cat("\nArquivos gerados:\n")
cat("- 01a_grafico_dispersao_bivariada.png\n")
cat("- 01c_grafico_correlacao_bivariada.png\n")
cat("- 01d_dispersao_com_reta_regressao.png\n")
cat("- 02a_matriz_dispersao_multivariada.png\n")
cat("- 02c_grafico_correlacao_multivariada.png\n")
cat("\nLembre-se de:\n")
cat("1. Substituir a base de dados exemplo pela sua base real\n")
cat("2. Adicionar os nomes e RMs dos integrantes no início do código\n")
cat("3. Exportar sua base em formato .xlsx\n")
cat("4. Salvar este script em formato .r\n")
cat("============================================================================\n")