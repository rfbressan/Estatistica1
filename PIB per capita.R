## Trabalho de Estatística I
## PIB per capita do Brasil, Regiões, Estados do Sul 
## e Microrregiões de SC
library(reshape) # Para rearranjar os dados
library(xtable) # Imprimir tabela para arquivo

# Defina o diretorio onde esta seu trabalho e arquivos 
setwd("C:\\Users\\Rafael\\Documents\\UDESC\\2 - Estatistica I\\Trabalho PIB per Capta")

# Leitura dos dados no arquivo csv para um Data Frame

dadosmicro <- na.omit(read.csv2("Microrregioes-final.csv"))
dadosreg <- na.omit(read.csv2("PIB per capita-final.csv"))
dadosdf <- read.csv2("PIB per capita regioes.csv")

# Conjuntos de dados
regioes <- c("NORTE", "NORDESTE", "CENTRO-OESTE", "SUDESTE", "SUL")
estados <- c("PR", "SC", "RS")
microrregioes <- levels(dadosmicro$Microrregião)

# Como os nomes sao muito grandes para ficarem todos no eixo X, criei uma correspondencia
# com siglas. Imprimam esta tabela no trabalho para relacionar as microrregioes as siglas
# que aparecem no grafico.
# Para imprimir a tabela, aprender a usar o comando print.xtable() e xtable(), do pacote
# xtable
microrregioes <- cbind(microrregioes, c("ARA", "BLU", "CLS", "CAS", "CHO", "COA", "CRA",
                                        "CUS", "FLS", "ITI", "ITA", "JOA", "JOE", "RIL",
                                        "SAL", "SAE", "TAO", "TIS", "TUO", "XAE"))
colnames(microrregioes) <- c("Microrregião", "Sigla")

# Tabela com as correspondências de microrregiões e siglas
# Como está, só serve para quem for fazer o relatorio em LaTEX
print.xtable(xtable(microrregioes, caption = "Lista das microrregiões e suas siglas",
                    label = "tab:tabsiglas"),
             file = "tabsiglas.tex", caption.placement = "top")

# Boxplot das regioes
boxplot(dadosreg[,"X2010"] ~ dadosreg[,"Região"], cex.axis=0.8)

# Dos estados
dadosest <- subset(dadosreg, subset = (Sigla %in% estados))
dadosest$Sigla <- droplevels(dadosest$Sigla)
boxplot(dadosest[,"X2010"] ~ dadosest[,"Sigla"])

# Das microrregioes
## de 1 a 10
dadosmicro1 <- subset(dadosmicro, subset = (Microrregião %in% microrregioes[1:10,1]))
dadosmicro1$Microrregião <- droplevels(dadosmicro1$Microrregião)
boxplot(dadosmicro1[,"X2010"] ~ dadosmicro1[,"Microrregião"], cex.axis=0.8,
        names=microrregioes[1:10,2])

## de 11 a 20
dadosmicro2 <- subset(dadosmicro, subset = (Microrregião %in% microrregioes[11:20,1]))
dadosmicro2$Microrregião <- droplevels(dadosmicro2$Microrregião)
boxplot(dadosmicro2[,"X2010"] ~ dadosmicro2[,"Microrregião"], cex.axis=0.8,
        names=microrregioes[11:20,2])

#########################################################################################
## A evolucao temporal de cada microrregiao deixo para ser descoberto.
## O pacote reshape pode ser util para este fim