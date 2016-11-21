## Trabalho de Estatística I
## PIB per capita do Brasil, Regiões, Estados do Sul 
## e Microrregiões de SC
library(reshape) # Para rearranjar os dados
library(ggplot2)
library(xtable)

setwd("C:\\Users\\Rafael\\Documents\\UDESC\\2 - Estatistica I\\Trabalho PIB per Capta")

# Leitura dos dados no arquivo csv para um Data Frame

dadosmicro <- na.omit(read.csv2("Microrregioes-final.csv"))
dadosreg <- na.omit(read.csv2("PIB per capita-final.csv"))


microrregioes <- levels(dadosmicro$Microrregião)
microrregioes <- cbind(microrregioes, c("ARA", "BLU", "CLS", "CAS", "CHO", "COA", "CRA",
                                        "CUS", "FLS", "ITI", "ITA", "JOA", "JOE", "RIL",
                                        "SAL", "SAE", "TAO", "TIS", "TUO", "XAE"))
colnames(microrregioes) <- c("Microrregiões", "Sigla")
# Tabela com as correspondências de microrregiões e siglas
print.xtable(xtable(microrregioes, caption = "Lista das microrregiões e suas siglas",
                    label = "tab:tabmicro"),
             file = "tabmicro.tex", caption.placement = "top")

## Evita imprmir numeros em notação científica
options(scipen = 7)

# Estados
plot <- ggplot(subset(dadosreg, subset = (Sigla %in% c("PR", "SC", "RS"))),
               aes(x=Sigla, y=X2010, group=Sigla))
plot <- plot+geom_boxplot()+labs(x="", y="Preços de 2000 - R$", title="PIB per capita 2010")
plot

# Regiões
plot <- ggplot(dadosreg[,15:16], aes(x=Região, y=X2010, group=Região))
plot <- plot+geom_boxplot()+labs(x="", y="Preços de 2000 - R$", title="PIB per capita 2010")
plot

# Microrregioes 1 a 10
plot <- ggplot(subset(dadosmicro, subset = (Microrregião %in% microrregioes[1:10,1])),
               aes(x=Microrregião, y=X2010, group=Microrregião))
plot <- plot+geom_boxplot()+labs(x="", y="Preços de 2000 - R$", title="PIB per capita 2010")+
  scale_x_discrete(breaks = microrregioes[1:10,1],
                   labels = microrregioes[1:10,2])
plot

# Microrregioes 11 a 20
plot <- ggplot(subset(dadosmicro, subset = (Microrregião %in% microrregioes[11:20,1])),
               aes(x=Microrregião, y=X2010))
plot <- plot+geom_boxplot()+labs(x="", y="Preços de 2000 - R$", title="PIB per capita 2010")+
  scale_x_discrete(breaks = microrregioes[11:20,1],
                   labels = microrregioes[11:20,2])
plot

# Evolução temporal das microrregiões
colnames(dadosmicro)[4:15] <- c("1999","2000","2001","2002","2003", "2004","2005",
                                "2006","2007","2008","2009","2010")
dadostemp <- melt(dadosmicro, id=c("Município", "Microrregião"),
                  measure.vars = c("1999","2000","2001","2002","2003", "2004","2005",
                                   "2006","2007","2008","2009","2010"),
                  variable_name = "Ano")

for(i in 1:20){
  plot <- ggplot(subset(dadostemp, subset = (Microrregião %in% microrregioes[i,1])),
                aes(x=factor(Ano), y=value, group = Ano))
  plot <- plot+geom_boxplot()+labs(x="", y="Preços de 2000 - R$",
                                 title=paste("Microrregião ",microrregioes[i,1]))
  # Plota para um arquivo
  jpeg(paste(microrregioes[i,2],".jpg"), width = 593, height = 434)
  print(plot)
  dev.off() # Fecha o arquivo grafico
  rm(plot)
}

## Calcula a mediana de cada microrregião para cada ano
micromediana <- aggregate(dadosmicro[,4:15], list(Microrregião=dadosmicro$Microrregião), median)
# Calcula a taxa média geométrica do crescimento da mediana do pib per capta
cagr <- apply(micromediana[-1], 1, function(x) (((x[12]/x[1])^(1/12))-1)*100)
micromediana <- cbind(micromediana, cagr)
micromediana <- micromediana[order(micromediana[,"cagr"], decreasing = TRUE),]
colnames(micromediana)[length(colnames(micromediana))] <- "CAGR (%a.a.)"
# Tabela com os crescimentos das medianas de cada microrregião
print.xtable(xtable(micromediana[,c(1, 14)], caption = "Crescimento médio anual da mediana de cada microrregião. Em pontos percentuais.",
                    label = "tab:cagrmicro", digits = 2),
             file = "tabcagr.tex", caption.placement = "top", include.rownames = FALSE)

#######################################################################################
## Analise dos outliers da cada regiao e estado do sul

# Regiões
maxreg <- tapply(dadosreg$X2010, dadosreg$Região, max)
muni <- vector(mode = "character", length = 5)
for(i in seq_along(maxreg)){
muni[i] <- as.character(dadosreg[which(dadosreg$X2010==maxreg[i] &
                   dadosreg$Região == dimnames(maxreg)[[1]][i]), "Município"])
}
maxreg <- data.frame(muni, maxreg)
colnames(maxreg) <- c("Município", "PIB per capita")
print.xtable(xtable(maxreg, caption = "Municípios com os maiores PIB per capita em 2010, por regiões.",
                    label = "tab:maxreg", digits=0),
             file = "tabmaxreg.tex", caption.placement = "top")

# Estados do Sul
maxest <- tapply(dadosreg$X2010, dadosreg$Sigla, max)[c("PR", "SC", "RS")]
muni <- vector(mode = "character", length = 3)
for(i in seq_along(maxest)){
  muni[i] <- as.character(dadosreg[which(dadosreg$X2010==maxest[i] &
                                           dadosreg$Sigla == dimnames(maxest)[[1]][i]),
                                   "Município"])
}
maxest <- data.frame(muni, maxest)
colnames(maxest) <- c("Município", "PIB per capita")
print.xtable(xtable(maxest, digits=0, caption = "Municípios com os maiores PIB per capita em 2010, por estados do Sul.",
                    label = "tab:maxest"),
             file = "tabmaxest.tex", caption.placement = "top")

########################################################################################
## Histogramas
regioes <- c("NORTE", "NORDESTE", "CENTRO-OESTE", "SUDESTE", "SUL")
estados <- c("PR", "SC", "RS")

# Regiões
for(i in seq_along(regioes)){
  subreg <- subset(dadosreg, subset = (Região==regioes[i]), select = X2010)
  
  histo <- ggplot(subreg, aes(x=X2010, y=..count..))+
    geom_histogram(colour="black", fill="White",binwidth = 1000)+
    labs(x="PIB per capita (R$)", y="Contagem", title=paste("Região ",regioes[i]))
  #histo <- histo+geom_density(aes(y=..count..))
  # Plota para um arquivo
  jpeg(paste("Hist_",regioes[i],".jpg"), width = 593, height = 434)
  print(histo)
  dev.off() # Fecha o arquivo grafico
}

# Estados do Sul
for(i in seq_along(estados)){
  subest <- subset(dadosreg, subset = (Sigla==estados[i]), select = X2010)
  
  histo <- ggplot(subest, aes(x=X2010, y=..count..))+
    geom_histogram(colour="black", fill="White",binwidth = 1000)+
    labs(x="PIB per capita (R$)", y="Contagem", title=paste(estados[i]))
  #histo <- histo+geom_density(aes(y=..count..))
  # Plota para um arquivo
  jpeg(paste("Hist_",estados[i],".jpg"), width = 593, height = 434)
  print(histo)
  dev.off() # Fecha o arquivo grafico
}

# Microrregiões
for(i in seq_along(microrregioes[,1])){
  submicro <- subset(dadosmicro, subset = (Microrregião==microrregioes[i,1]),
                     select = as.character(2010))
  
  histo <- ggplot(submicro, aes(x=submicro[,1]))+
    geom_histogram(colour="black", fill="White",binwidth = 1000)+
    labs(x="PIB per capita (R$)", y="Contagem", title=paste("Microrregião de ",
                                                            microrregioes[i]))
  #histo <- histo+geom_density(aes(y=..count..))
  # Plota para um arquivo
  jpeg(paste("Hist_",microrregioes[i,2],".jpg"), width = 593, height = 434)
  print(histo)
  dev.off() # Fecha o arquivo grafico
}





options(scipen = 0) # Retorna ao valor padrao