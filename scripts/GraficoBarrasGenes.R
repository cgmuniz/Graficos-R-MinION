df_csv = read.table("/data8/AKT/20221219/20221219_pcr2_260bp/no_sample/20221219_1517_MC-110697_FAV13136_b6d54ef8/analise/barcode03/filtered_20221219_barcode03_igblast_db-pass_parse-select.tsv", header = TRUE, sep = "\t")

# contagem <- table(df_csv$v_call)
# contagem_filt <- subset(contagem, contagem > 1000)
# contagem_filt <- data.frame(contagem[contagem > 100])

contagemDupla <- table(df_csv$junction_aa,df_csv$v_call)
# contagemDupla <- subset(contagemDupla, subset = contagemDupla > 0)
tabelaCont <- data.frame(contagemDupla)
tabelaCont <- tabelaCont[tabelaCont$Freq > 0, ]
# Recontando o número das posições das linhas
rownames(tabelaCont) <- NULL

hist(tabelaCont$Freq[tabelaCont$Freq>1000])




df_adrb1 = read.table("/home/administrator/adri/barcode16_trim_filt13_igblast_db-pass_parse-select.tsv", header = TRUE, sep = "\t")
df_adrb2 = read.table("/home/administrator/adri/barcode10_trim_filt13_igblast_db-pass_parse-select.tsv", header = TRUE, sep = "\t")
df_adrb3 = read.table("/home/administrator/adri/barcode13_trim_filt13_igblast_db-pass_parse-select.tsv", header = TRUE, sep = "\t")

df_novb1 = read.table("/data8/AKT/20221129_pcr2coronavac/no_sample/20221129_2102_MC-110697_FAU75433_22d65ffd/analise/barcode01/filtered_20221129_barcode01_igblast_db-pass_parse-select.tsv", header = TRUE, sep = "\t")
df_novb2 = read.table("/data8/AKT/20221129_pcr2coronavac/no_sample/20221129_2102_MC-110697_FAU75433_22d65ffd/analise/barcode02/filtered_20221129_barcode02_igblast_db-pass_parse-select.tsv", header = TRUE, sep = "\t")
df_novb3 = read.table("/data8/AKT/20221129_pcr2coronavac/no_sample/20221129_2102_MC-110697_FAU75433_22d65ffd/analise/barcode03/filtered_20221129_barcode03_igblast_db-pass_parse-select.tsv", header = TRUE, sep = "\t")

df_dezb1 = read.table("/data8/AKT/20221219/20221219_pcr2_260bp/no_sample/20221219_1517_MC-110697_FAV13136_b6d54ef8/analise/barcode01/filtered_20221219_barcode01_igblast_db-pass_parse-select.tsv", header = TRUE, sep = "\t")
df_dezb2 = read.table("/data8/AKT/20221219/20221219_pcr2_260bp/no_sample/20221219_1517_MC-110697_FAV13136_b6d54ef8/analise/barcode02/filtered_20221219_barcode02_igblast_db-pass_parse-select.tsv", header = TRUE, sep = "\t")
df_dezb3 = read.table("/data8/AKT/20221219/20221219_pcr2_260bp/no_sample/20221219_1517_MC-110697_FAV13136_b6d54ef8/analise/barcode03/filtered_20221219_barcode03_igblast_db-pass_parse-select.tsv", header = TRUE, sep = "\t")



dfGroupAdrB1 <- df_adrb1
dfGroupAdrB1$v_call <- as.data.frame(sub("\\*.*", "", dfGroupAdrB1$v_call))
# Contando as ocorrências de v_call
dfGroupAdrB1 <- as.data.frame(table(dfGroupAdrB1$v_call))
names(dfGroupAdrB1)[1] <- "v_call"
# dfGroup <- dfGroup[dfGroup$Freq > 3000, ]
dfGroupAdrB1 <- dfGroupAdrB1[grep("IGHV", dfGroupAdrB1$v_call), ]

dfGroupNovB1 <- df_novb1
dfGroupNovB1$v_call <- as.data.frame(sub("\\*.*", "", dfGroupNovB1$v_call))
# Contando as ocorrências de v_call
dfGroupNovB1 <- as.data.frame(table(dfGroupNovB1$v_call))
names(dfGroupNovB1)[1] <- "v_call"
# dfGroup <- dfGroup[dfGroup$Freq > 3000, ]
dfGroupNovB1 <- dfGroupNovB1[grep("IGHV", dfGroupNovB1$v_call), ]

dfGroupDezB1 <- df_dezb1
dfGroupDezB1$v_call <- as.data.frame(sub("\\*.*", "", dfGroupDezB1$v_call))
# Contando as ocorrências de v_call
dfGroupDezB1 <- as.data.frame(table(dfGroupDezB1$v_call))
names(dfGroupDezB1)[1] <- "v_call"
# dfGroup <- dfGroup[dfGroup$Freq > 3000, ]
dfGroupDezB1 <- dfGroupDezB1[grep("IGHV", dfGroupDezB1$v_call), ]

df <- rbind(dfGroupAdrB1, dfGroupNovB1, dfGroupDezB1)

cores <- rep(c("red","blue","green"), each = nrow(dfGroupAdrB1))

barplot(height = df$Freq,
        names.arg = df$v_call,
        col = cores,
        ylab = "Frequência"
)


total <- sum(dfGroupDezB1$Freq)
porc <- (dfGroupDezB1$Freq/total)*100

posicao_excluir <- which(dfGroup$v_call == "IGHV5-51")
dados_filtrados <- dfGroup[-posicao_excluir, ]
porc551 <- porc[-posicao_excluir]
dados_filtrados <- dados_filtrados[porc551 > 0.3, ]
porc551 <- porc551[porc551 > 0.3]

y_axis <- seq(0, max(porc551) + 1L, by = 2)

par(mar = c(10,5,4,2))

# Plotando um gráfico de barras
barplot(porc551,
        names.arg = dados_filtrados$v_call,
        ylab = "Frequência",
        cex.axis = 0.8,
        cex.names = 0.8,
        main = "Frequência de v_call",
        las = 3,
        yaxt = "n",
        ylim = c(0, max(porc551) + 1L)
        )

axis(2, at = y_axis, labels = paste0(y_axis, "%"))




library(ggplot2)
library(readxl)
library(tidyr)

df_geral = read_excel("/home/administrator/Rtestes/tabelaGeral.xlsx")

total_colunas <- ncol(df_geral)

df_porcentagem <- df_geral[,-total_colunas]

for(i in 2:(total_colunas-1)){
  coluna = names(df_geral)[i]
  coluna_porcentagem <- df_geral[[coluna]] / sum(df_geral[[coluna]], na.rm = TRUE) * 100
  df_porcentagem[[coluna]] <- coluna_porcentagem
}

i <- 3

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
ggplot(dados_long, aes(x = v_call, y = porcentagem, fill = variavel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "", fill = "Corrida") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))
