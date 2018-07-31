### Agregação de Preferências Eleitorais via Escalagem de Thurstone
### Script em R para analise dos dados
### Autor: Lucas Loureiro Lino da Costa
### Data: Julho de 2018

# Instalação e carregamento dos pacotes necessários
packages = c("ggplot2", "readr", "tidyverse")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

library(readr)
library(tidyverse)

# Carregando os dados da pesquisa no sistema
dados_pibic = read_csv(file.choose())
View(dados_pibic)

candidatos = c("Jair_Bolsonaro", "Marina_Silva", "Ciro_Gomes", "Geraldo_Alckmin", "Guilherme_Boulos")

# Crianção de um dataframe vazio para receber os dados por pares
quadro_parwise = data.frame(Jair_Bolsonaro = numeric(),
                        Marina_Silva = numeric(),
                        Ciro_Gomes = numeric(),
                        Geraldo_Alckmin = numeric(),
                        Guilherme_Boulos = numeric())


# Alimentando o quadro de escolhas por pares
for (i in 1:5){
  for (j in 1: 5){
    quadro_parwise[i,j] = length(which(dados_pibic[, i] > dados_pibic[,j]))
  }
}

row.names(quadro_parwise) = c("Jair_Bolsonaro", "Marina_Silva",
                          "Ciro_Gomes", "Geraldo_Alckmin",
                          "Guilherme_Boulos")

quadro_parwise[quadro_parwise == 0] = NA

# Quadro de escolhas por pares, dados em frequência relativa
quadro_parwise_freq = quadro_parwise/nrow(dados_pibic)

# Quadro para os valores críticos para uma distribuição normal

quadro_parwise_critical = quadro_parwise_freq %>% mutate_all(funs(qnorm))

# Valores de escala de cada candidato

escala = rowSums(quadro_parwise_critical, na.rm = TRUE)*(1/5)
escala_candidatos = data.frame(candidatos, escala)
#escala_candidatos[order(escala_candidatos$escala, decreasing = TRUE),]


