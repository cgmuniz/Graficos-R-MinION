library(ggplot2)
library(dplyr)

dadosH <- read.csv("./tabelas/MINCOV_comp_Heavy.csv", header = TRUE, sep = "\t")

# Somando os valores das três colunas para criar as colunas dos individuos
dadosH$individuo1 <- rowSums(dadosH[, c("barcode16_trim_filt13_tsvfilter", "filtered_20221129_barcode01", "filtered_20221219_barcode01")], na.rm = TRUE)
dadosH$individuo2 <- rowSums(dadosH[, c("barcode10_trim_filt13_tsvfilter", "filtered_20221129_barcode02", "filtered_20221219_barcode02")], na.rm = TRUE)
dadosH$individuo3 <- rowSums(dadosH[, c("barcode13_trim_filt13_tsvfilter", "filtered_20221129_barcode03", "filtered_20221219_barcode03")], na.rm = TRUE)

# Filtrar os dados para manter apenas os valores com similarity_percentage > 81.25
dadosH_filtrados <- subset(dadosH, similarity_percentage > 81.25)

dadosH_filtrados$individuo1_log <- log(dadosH_filtrados$individuo1)
dadosH_filtrados$individuo2_log <- log(dadosH_filtrados$individuo2)
dadosH_filtrados$individuo3_log <- log(dadosH_filtrados$individuo3)

# Criar o heatmap dotplot pesadas
ggplot(dadosH_filtrados, aes(x = covabdab_sequence, y = junction_aa, size = individuo1, fill = similarity_percentage)) +
#ggplot(dados_filtrados, aes(x = covabdab_sequence, y = junction_aa, size = individuo3_log, fill = similarity_percentage)) + # log
  geom_point(shape = 21, aes(fill = ifelse(individuo1 != 0, similarity_percentage, NA))) + # Ponto com borda
  scale_size_continuous(range = c(3, 15), limits = c(0, 953)) + # Definir a faixa de tamanhos
  #scale_size_continuous(range = c(1, 10), limits = c(0, 7)) + # log
  scale_fill_gradient(low = "blue", high = "red", limits = c(81.82, 89.47), na.value = "transparent") + # Gradiente de cores
  theme_minimal() + # Estilo mínimo
  labs(x = "Sequências CoV-AbDab", y = "Sequências MinION", size = "Ocorrências", fill = "Similaridade", caption = "*Sem cor tem 0 ocorrências") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + # Girar os rótulos em 90 graus 
  ggtitle("Individuo 1 (H)")


# ---------------Leves---------------


dadosL <- read.csv("./tabelas/MINCOV_comp_Light.csv", header = TRUE, sep = "\t")

# Somando os valores das três colunas para criar as colunas dos individuos
dadosL$individuo1 <- rowSums(dadosL[, c("barcode16_trim_filt13_tsvfilter", "filtered_20221129_barcode01", "filtered_20221219_barcode01")], na.rm = TRUE)
dadosL$individuo2 <- rowSums(dadosL[, c("barcode10_trim_filt13_tsvfilter", "filtered_20221129_barcode02", "filtered_20221219_barcode02")], na.rm = TRUE)
dadosL$individuo3 <- rowSums(dadosL[, c("barcode13_trim_filt13_tsvfilter", "filtered_20221129_barcode03", "filtered_20221219_barcode03")], na.rm = TRUE)

# Filtrar os dados para manter apenas os valores com similarity_percentage > 81.25
dadosL_filtrados <- subset(dadosL, similarity_percentage > 81.25)
#dadosL_filtrados <- subset(dadosL, similarity_percentage > 83.33)

dadosL_filtrados$individuo1_log <- log(dadosL_filtrados$individuo1)
dadosL_filtrados$individuo2_log <- log(dadosL_filtrados$individuo2)
dadosL_filtrados$individuo3_log <- log(dadosL_filtrados$individuo3)

# Criar o heatmap dotplot pesadas
ggplot(dadosL_filtrados, aes(x = covabdab_sequence, y = junction_aa, size = individuo3, fill = similarity_percentage)) +
  #ggplot(dados_filtrados, aes(x = covabdab_sequence, y = junction_aa, size = individuo3_log, fill = similarity_percentage)) + # log
  geom_point(shape = 21, aes(fill = ifelse(individuo3 != 0, similarity_percentage, NA))) + # Ponto com borda
  scale_size_continuous(range = c(3, 15), limits = c(0, 23542)) + # Definir a faixa de tamanhos
  #scale_size_continuous(range = c(3, 15), limits = c(0, 4781)) + # Para similaridade > 83.33
  #scale_size_continuous(range = c(1, 10), limits = c(0, 7)) + # log
  scale_fill_gradient(low = "blue", high = "red", limits = c(81.82, 92.31), na.value = "transparent") + # Gradiente de cores
  theme_minimal() + # Estilo mínimo
  labs(x = "Sequências CoV-AbDab", y = "Sequências MinION", size = "Ocorrências", fill = "Similaridade", caption = "*Sem cor tem 0 ocorrências") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5)) + # Girar os rótulos em 90 graus 
  ggtitle("Individuo 3 (L)")


dev.copy2pdf(file="Individuo1H.pdf", width=16,height=9)
dev.off()
