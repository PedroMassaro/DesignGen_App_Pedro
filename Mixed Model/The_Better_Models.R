#Upload a file

# Carregando os dados:
library(readxl)
dados_exp <- read_xls("~/Desktop/Data Science/Iniciação Científica/Dados/Dados_MFS.xls")

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

# data_split_MS <- dados_fil %>% 
#   filter(Local %in% "MS") %>%
#   filter(Corte %in% c("2","3","4","5")) %>% droplevels()
# str(data_split_MS)

data_split_MS <- dados_fil %>% 
  filter(Local %in% "MS") %>%
  droplevels()
str(data_split_MS)



# Modelo 1 - Homogeniedade de variância
ans1r <- mmer(Peso ~ Corte + Corte:Bloco ,
               random= ~ Corte:Genotipo,
               rcov= ~ units,
               data=data_split_MS)
summary(ans1r)$varcomp
summary(ans1r)
ans1r$AIC
View(ans1r$U$`Corte:Genotipo`$Peso)

# Modelo 2 - Heterogeniedade de variância

# ans2r <- mmer(Peso ~ Corte + Corte:Bloco,
#                  random= ~ vsr(dsr(Corte),Genotipo),
#                  rcov= ~ vsr(dsr(Corte),units),
#                  data=data_split_MS)
# summary(ans2r)$varcomp
# summary(ans2r)
# View(ans2r$U$`1:Genotipo`$Peso)

data_split_MS.01 <- na.omit(data_split_MS)

ans2.1r <- mmer(Peso ~ Corte + Corte:Bloco,
              random= ~ vsr(dsr(Corte),Genotipo),
              rcov= ~ units,
              data=data_split_MS.01)
summary(ans2.1r)$varcomp
summary(ans2.1r)
View(ans2.1r$U$`Corte:Genotipo`$Peso)

#Remover NA
data_split_MS.01 <- na.omit(data_split_MS)

ans2.2r <- mmer(Peso ~ Corte + Corte:Bloco,
                random= ~ vsr(dsr(Corte),Genotipo),
                rcov= ~ units,
                data=data_split_MS.01)
summary(ans2.2r)$varcomp
summary(ans2.2r)
View(ans2.1r$U$`Corte:Genotipo`$Peso)

ggplot(data_split_MS.01, aes(x = Corte, y = Peso, color = Corte)) +
  geom_point() +
  labs(x = "Corte", y = "Peso") +
  ggtitle("Peso vs. Corte") +
  theme_minimal()

#rodar para o Acre
data_split_AC <- dados_fil %>% 
  filter(Local %in% "AC") %>%
  droplevels()
str(data_split_AC)

ans2.2r <- mmer(Peso ~ Corte + Corte:Bloco,
                random= ~ vsr(dsr(Corte),Genotipo),
                rcov= ~ units,
                data=data_split_AC)
summary(ans2.2r)$varcomp
summary(ans2.2r)
View(ans2.1r$U$`Corte:Genotipo`$Peso)

library(ggplot2)

# Assuming data_split_MS is your tibble containing the data
# Convert "Corte" to a factor to ensure correct plotting order
data_split_AC$Corte <- factor(data_split_AC$Corte)

# Plot using ggplot
ggplot(data_split_AC, aes(x = Corte, y = Peso, color = Corte)) +
  geom_point() +
  labs(x = "Corte", y = "Peso") +
  ggtitle("Peso vs. Corte") +
  theme_minimal()


AIC <- c(ans1r$AIC, ans2r$AIC)
#dds
BIC <- c(ans1r$BIC, ans2r$BIC)

AB <- cbind(AIC, BIC);AB

library(ggplot2)

# Assuming data_split_MS is your tibble containing the data
# Convert "Corte" to a factor to ensure correct plotting order
data_split_MS$Corte <- factor(data_split_MS$Corte)

# Plot using ggplot
ggplot(data_split_MS, aes(x = Corte, y = Peso, color = Corte)) +
  geom_point() +
  labs(x = "Corte", y = "Peso") +
  ggtitle("Peso vs. Corte") +
  theme_minimal()

#------------------------------------------------------------------------------#
#Dados - Parciais - MS e AC - Teste
library(readr)
library(sommer)
library(tidyverse)
library(dplyr)

data_parcial_MS <- dados_fil %>%
  filter(Local %in% "AC") %>%
  filter(Corte %in% c("2","3","4","5")) %>% droplevels()
str(data_parcial_MS)


# Modelo 1 - Homogeniedade de variância
ans11r <- mmer(Peso ~ Corte + Corte:Bloco ,
              random= ~ Corte:Genotipo,
              rcov= ~ units,
              data=data_parcial_MS)
summary(ans11r)$varcomp
summary(ans11r)
View(ans11r$U$`Corte:Genotipo`$Peso)


ans22r <- mmer(Peso ~ Corte + Corte:Bloco,
                random= ~ vsr(dsr(Corte),Genotipo),
                rcov= ~ units,
                data= data_parcial_MS)
summary(ans22r)$varcomp
summary(ans22r)
View(ans22r$U$`Corte:Genotipo`$Peso)

AIC <- c(ans1r$AIC, ans2r$AIC)
BIC <- c(ans1r$BIC, ans2r$BIC)

AB <- cbind(AIC, BIC);AB

library(ggplot2)

# Assuming data_split_MS is your tibble containing the data
# Convert "Corte" to a factor to ensure correct plotting order
data_parcial_MS <- factor(data_parcial_MS$Corte)

# Plot using ggplot
ggplot(data_parcial_MS, aes(x = Corte, y = Peso, color = Corte)) +
  geom_point() +
  labs(x = "Corte", y = "Peso") +
  ggtitle("Peso vs. Corte") +
  theme_minimal()





