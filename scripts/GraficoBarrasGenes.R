library(ggplot2)
library(readxl)
library(tidyr)

df_geral = read_excel("../tabelas/tabelaGeral.xlsx")

total_colunas <- ncol(df_geral)

df_porcentagem <- df_geral[,-total_colunas]

for(i in 2:(total_colunas-1)){
  coluna = names(df_geral)[i]
  coluna_porcentagem <- df_geral[[coluna]] / sum(df_geral[[coluna]], na.rm = TRUE) * 100
  df_porcentagem[[coluna]] <- coluna_porcentagem
}

i <- 1

if(i == 1){
  adrVar <- 'adr_16'
  novVar <- 'nov_1'
  dezVar <- 'dez_1'
  dadosVar <- df_porcentagem$dez_1
} else if (i == 2){
  adrVar <- 'adr_10'
  novVar <- 'nov_2'
  dezVar <- 'dez_2'
  dadosVar <- df_porcentagem$dez_2
} else {
  adrVar <- 'adr_13'
  novVar <- 'nov_3'
  dezVar <- 'dez_3'
  dadosVar <- df_porcentagem$dez_3
}

dados_filtrados <- df_porcentagem[dadosVar > 0.3, ]
posicao_excluir <- which(dados_filtrados$v_call == "IGHV5-51")
dados_filtrados <- dados_filtrados[-posicao_excluir, ]

dados_long <- gather(dados_filtrados, key = "variavel", value = "porcentagem", adrVar, novVar, dezVar)
dados_long <- dados_long[!is.na(dados_long$v_call),]

# Crie o gráfico utilizando ggplot2
p <- ggplot(dados_long, aes(x = v_call, y = porcentagem, fill = variavel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "", fill = "Corrida") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))


ggsave("barcode1.png", plot=p, width = 8, height = 6, units = "in")
