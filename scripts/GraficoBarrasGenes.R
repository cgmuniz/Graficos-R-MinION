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




df_csv = read.table("/data8/AKT/20221219/20221219_pcr2_260bp/no_sample/20221219_1517_MC-110697_FAV13136_b6d54ef8/analise/barcode03/filtered_20221219_barcode03_igblast_db-pass_parse-select.tsv", header = TRUE, sep = "\t")

dfGroup <- df_csv
dfGroup$v_call <- as.data.frame(sub("\\*.*", "", dfGroup$v_call))
# Contando as ocorrências de v_call
dfGroup <- as.data.frame(table(dfGroup$v_call))
names(dfGroup)[1] <- "v_call"
# dfGroup <- dfGroup[dfGroup$Freq > 3000, ]
dfGroup <- dfGroup[grep("IGHV", dfGroup$v_call), ]

total <- sum(dfGroup$Freq)
porc <- (dfGroup$Freq/total)*100

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

