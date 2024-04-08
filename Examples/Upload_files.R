#Upload a file

# Carregando os dados:
library(readxl)
dados_exp <- read_xls("C:/Users/Pedro Henrique/Desktop/Data science/Iniciação Científica/Codigos_Vitoria/Dados_MFS.xls")

# Definindo dados como fatores e Renomeando as colunas:
library(dplyr)
dados_exp <- dados_exp %>% 
  mutate(GENOTIPO = as.factor(GENOTIPO),
         LOCAL = as.factor(LOCAL),
         CORTE = as.factor(CORTE),
         MSF = as.double(MSF),
         REP = as.factor(REP)) %>%
  rename(Genotipo = GENOTIPO,
         Local = LOCAL,
         Corte = CORTE,
         Bloco = REP,
         Peso = MSF)

#### Filtragem ####
# Removendo outlier
dados_fil <- droplevels(dados_exp[-which(dados_exp$Local == "MS" & dados_exp$Corte == 4 
                                         & dados_exp$Genotipo == "PM35" & dados_exp$Bloco == 2),])

# Removendo cortes perdidos
dados_fil <- droplevels(dados_fil[-which(dados_fil$Local == "DF" & dados_fil$Corte == 5),])
dados_fil <- droplevels(dados_fil[-which(dados_fil$Local == "RJ" & dados_fil$Corte == 1),])
dados_fil <- droplevels(dados_fil[-which(dados_fil$Local == "RJ" & dados_fil$Corte == 2),])

write.csv(dados_fil, "dados_fil.csv", row.names = TRUE)


#------------------------------------------------------------------------------#
#Dados - Parciais - MS
library(readr)
library(sommer)
library(tidyverse)
library(dplyr)

data_split_MS <- dados_fil %>% 
  filter(Local %in% "MS") %>%
  filter(Corte %in% c("2","3","4","5")) %>% droplevels()
str(data_split_MS)

# Modelo 14 - Com inserção do efeito fixo e da interação 'block'
ans15r <- mmer(Peso ~ Corte + Bloco,
               random= ~ Corte:Bloco + Genotipo + Genotipo:Bloco + Genotipo:Corte,
               rcov= ~ units,
               data=data_split_MS)
summary(ans15r)$varcomp
summary(ans15r)

ans15.1r <- mmer(Peso ~ Corte + Bloco,
                 random= ~ Corte:Bloco + Genotipo + Genotipo:Bloco + vsr(dsr(Corte),Genotipo),
                 rcov= ~ vsr(dsr(Corte),units),
                 data=data_split_MS)
summary(ans15.1r)$varcomp
summary(ans15.1r)

ans15.2r <- mmer(Peso ~ Corte + Bloco,
                 random= ~ Corte:Bloco + Genotipo + Genotipo:Bloco + vsr(usr(Corte),Genotipo),
                 rcov= ~ vsr(dsr(Corte),units),
                 data=data_split_MS)
summary(ans15.2r)$varcomp
summary(ans15.2r)

AIC <- c(ans15r$AIC, ans15.1r$AIC, ans15.2r$AIC)
BIC <- c(ans15r$BIC, ans15.1r$BIC, ans15.2r$BIC)

AB <- cbind(AIC, BIC);AB

random <- ans15.2r$U

g5 <- vector()
for (i in 1:23) {
  print(i)
  g5[i] <- random[["5:Genotipo"]][["Peso"]][[i]]
} 

interaction_ghi <- cbind(g2, g3, g4, g5)
row.names(interaction_ghi) <- names(random[["5:Genotipo"]][["Peso"]])
colnames(interaction_ghi) <- c("C2", "C3", "C4", "C5" )
View(interaction_ghi)

## Heatmap

  library(fields)
  
  # Define the color gradient using the specified hex colors
  colors <- colorRampPalette(c("#003350", "#cc662f"))(100)
  
  # Invert the columns' positions
  interaction_ghi_inverted <- interaction_ghi[, ncol(interaction_ghi):1]
  
  # Create the heatmap
  heatmap(interaction_ghi_inverted,
          Colv = NA,
          Rowv = NA,
          scale = "column",
          col = colors,
          # labCol = NA,  # To remove column labels if not needed
          main = "Genotype by Harvest Interaction")  # Set the title
  
  # Add a color legend
  image.plot(zlim = range(interaction_ghi), col = colors,
             legend.lab = "Color Scale",
             legend.mar = 10, legend.only = TRUE)
  
 



