# Gráficos-R-MinION
Scripts em R para a criaçao de gráficos usando dados coletados no projeto

- O arquivo `dotplot.R` utiliza tabelas com sequências cdr3 (leves ou pesadas) coletadas que foram comparadas com o banco de dados CoV-AbDab, cada tabela contendo 3 indivíduos. Gera um gráfico dotplot com a caracterizaçao de tamanho se referindo a quantas sequências foram encontradas para o individuo e a cor para quão semelhante é a sequência com uma sequência do CoV-AbDab. (Tabelas: MINCOV_comp_Heavy.csv e MINCOV_comp_Light.csv)
- O arquivo `GraficoBarrasGenes.R` utiliza a tabelaGeral.xlsx que contém 3 corridas com 3 indivíduos, calcula em cada coluna a porcentagem em relação ao total da coluna e exibe em um gráfico de barras, excluindo o *IGHV5-51*, os genes com porcentagem maior que 0.3 e a comparação entre cada corrida
