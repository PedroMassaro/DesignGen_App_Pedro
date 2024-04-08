#------------------------------------------------------------------------------#
#Dados - Parciais - MS
library(readr)
library(sommer)
library(tidyverse)
library(dplyr)

data_split_MS_full <- dados_fil %>% 
  filter(Local %in% "MS") %>%
  droplevels()
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

ans15.2r_f <- mmer(Peso ~ Corte + Bloco,
                 random= ~ Corte:Bloco + Genotipo + Genotipo:Bloco + vsr(usr(Corte),Genotipo),
                 rcov= ~ vsr(dsr(Corte),units),
                 data=data_split_MS_full)
summary(ans15.2r_f)$varcomp
summary(ans15.2r_f)

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

