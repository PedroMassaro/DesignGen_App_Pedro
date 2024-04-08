#-------------------------------------------------------------#
#                      Análise de Variâncias                  #
#                                                             #
#                Análise de cada local com modelo             # 
#                 de parcela subdividida no tempo             #
#                                                             #
#                     By Vitória Bizão Murakami               #
#                                                             #
# Modelo:                                                     #
# Y_ijk= μ + B_i + G_j + (BG)_ij + C_k + (BC)_ik + (GC)jk + ϵ #
#-------------------------------------------------------------#

# Limpando o ambiente
rm(list=ls())

# Carregando vários pacotes a serem usados na sessão simultaneamente
pkg = c("gridExtra", "lme4", "tidyverse", "ggplot2", 'dplyr', "asreml", "Matrix")
sapply(pkg, require, character.only=TRUE)

# Carregando os dados
dados_aov <- read_csv('dados_fil')
dados_aov <- dados_fil

# Definindo dados como fatores e Renomeando as colunas
dados_aov <- dados_aov %>% mutate(Genotipo = as.factor(Genotipo),
                                  Local = as.factor(Local),
                                  Corte = as.factor(Corte),
                                  Peso = as.double(Peso),
                                  Bloco = as.factor(Bloco))

str(dados_aov)

with(dados_aov, table(Peso, Genotipo))    # Visualizando os dados em tabela

#### Manualmente ####
# Loop
n <- levels(dados_aov$Local)
anova <- list()

for (i in n) {
  temp <- droplevels(subset(dados_aov, Local == i))
  anova[[i]] <- list(
    # Total recebe a soma das parcelas em Local = i
    Total <- with(temp, sum(Peso, na.rm = T)),
    
    # Fator de correção 
    Corr <- (Total^2)/(nlevels(temp$Genotipo)*nlevels(temp$Corte)*nlevels(temp$Bloco)),
    
    # Soma de quadrados TOTAIS corrigido
    SQtotal <- sum(temp$Peso^2, na.rm = T) - Corr,
    
    # Soma de quadrados dos GENÓTIPOS
    SQg <- sum(with(temp, tapply(Peso, list(Genotipo), sum, na.rm = T))^2),
    
    # SQ dos GENÓTIPOS corrigida
    SQgenotipos <- (SQg/(nlevels(temp$Bloco)*nlevels(temp$Corte))) - Corr,
    
    # Soma de Quadrados dos BLOCOS
    SQb <- sum(with(temp, tapply(Peso, Bloco, sum, na.rm = T))^2),
    
    # SQ das BLOCOS corrigida
    SQblocos <- (SQb/(nlevels(temp$Genotipo)*nlevels(temp$Corte))) - Corr,
    
    # Soma de Quadrados CONJUNTA (G,B)
    SQgb <- sum(with(temp, tapply(Peso, list(Bloco, Genotipo), sum, na.rm = T))^2),
    
    # SQ CONJUNTA (G,B) corrigida
    SQGenBlo <- (SQgb/nlevels(temp$Corte)) - Corr,
    
    # Soma de Quadrados ERRO A (Interação GxB)
    SQres.a <- SQGenBlo - SQgenotipos - SQblocos,
    
    # Soma de quadrados dos CORTES
    SQc <- sum(with(temp, tapply(Peso, Corte, sum, na.rm = T))^2),
    
    # SQ dos CORTES corrigidos
    SQcortes <- (SQc/(nlevels(temp$Genotipo)*nlevels(temp$Bloco))) - Corr,
    
    # Soma de Quadrados CONJUNTA (G,C)
    SQgc <- sum(with(temp, tapply(Peso, list(Corte, Genotipo), sum, na.rm = T))^2),
    
    # SQ CONJUNTA (G,C) corrigida
    SQGenBlo <- (SQgc/nlevels(temp$Bloco)) - Corr,
    
    # Soma de Quadrados INTERAÇÂO (GxC)
    SQGxC <- SQGenBlo - SQgenotipos - SQcortes,
    
    # Soma de Quadrados CONJUNTA (C,B)
    SQcb <- sum(with(temp, tapply(Peso, list(Corte, Bloco), sum, na.rm = T))^2),
    
    # SQ CONJUNTA (C,B) corrigida
    SQCorBlo <- (SQcb/nlevels(temp$Genotipo)) - Corr,
    
    # Soma de Quadrados ERRO B (Interação CxB)
    SQres.b <- SQCorBlo - SQblocos - SQcortes,
    
    # Soma de Quadrados ERRO C
    SQres.c <- SQtotal - SQblocos - SQgenotipos - SQres.a - SQcortes - SQres.b - SQGxC,
    
    # Fator de Variação
    FV <- c("Bloco", "Genótipo", "Erro a", "Corte", "Erro b", "Interação (GxC)", "Erro c", "Total"),
    
    # Graus de Liberdade
    gl <- c(nlevels(temp$Bloco)-1,
            nlevels(temp$Genotipo)-1,
            (nlevels(temp$Bloco)-1)*(nlevels(temp$Genotipo)-1),
            nlevels(temp$Corte)-1,
            (nlevels(temp$Corte)-1)*(nlevels(temp$Bloco)-1),
            (nlevels(temp$Genotipo)-1)*(nlevels(temp$Corte)-1),
            (nlevels(temp$Genotipo)-1)*(nlevels(temp$Corte)-1)*(nlevels(temp$Bloco)-1),
            nlevels(temp$Genotipo)*nlevels(temp$Corte)*nlevels(temp$Bloco)-1),
    
    # Soma de Quadrados
    Soma <- c(SQblocos, SQgenotipos, SQres.a, SQcortes, 
              SQres.b, SQGxC, SQres.c, SQtotal),
    
    # Quadrado Medio
    Quad <- c((Soma)/(gl)),
    
    # F Calculado
    Fcal<- c((Quad[1])/(Quad[3]), (Quad[2])/(Quad[3]), NA,
             (Quad[4])/(Quad[5]), NA, (Quad[6])/(Quad[7]), NA, NA),
    
    Ftab <- c((qf(.95,gl[1],gl[3])), (qf(.95,gl[2],gl[3])), NA, 
              (qf(.95,gl[4],gl[5])), NA, (qf(.95,gl[6],gl[7])), NA, NA),
    
    # Tabela ANOVA 
    anova.t <- data.frame("C.V" = FV, "G.L" = gl,
                          "S.Q" = Soma, "Q.M" = Quad,
                          "F calculado" = Fcal, "F tabelado" = Ftab)
    
  )
  
  print(i)
}


anova$AC[[26]]
anova$DF[[26]]
anova$MS[[26]]
anova$RJ[[26]]
anova$RO[[26]]


#### Com aov(), lmer() e asreml() ####
# Loop
n <- levels(dados_aov$Local)
anova_aov <- vector()
anova_lmer <- vector()
# wald_asreml <- vector()

for (i in n) {
  temp <- droplevels(subset(dados_aov, Local == i))
  model_aov <- aov(Peso ~ (Bloco + Genotipo*Corte) + Error(Bloco:Genotipo + Bloco:Corte), data = temp)
  anova_aov[[i]] <- list(model_aov)
  model_lmer <- lmer(Peso ~ (Bloco + Genotipo*Corte) + (1 | Bloco:Genotipo) + (1 | Bloco:Corte), temp)
  anova_lmer[[i]] <- list(model_lmer)
  # model_asrml <- asreml(fixed = Peso ~ Bloco + Genotipo*Corte, random = ~Bloco:Genotipo + Bloco:Corte, data = temp)
  # wald_asreml[[i]] <- list(model_asrml)
}

#Aqui ela ainda está trabalhando com modelos fixos,no qual genótipo é o fator primário e corte é o fator secundário. 

# Tabela ANOVA
# aov:
summary(anova_aov$AC[[1]]) # Acre
summary(anova_aov$DF[[1]]) # Distrito Federal
summary(anova_aov$MS[[1]]) # Mato Grosso do Sul
summary(anova_aov$RO[[1]]) # Rondonia
summary(anova_aov$RJ[[1]]) # Rio de Janeiro

# lmer():
anova(anova_lmer$AC[[1]]) # Acre
anova(anova_lmer$DF[[1]]) # Distrito Federal
anova(anova_lmer$MS[[1]]) # Mato Grosso do Sul
anova(anova_lmer$RO[[1]]) # Rondonia
anova(anova_lmer$RJ[[1]]) # Rio de Janeiro

# asreml():
wald.asreml(wald_asreml$AC[[1]]) # Acre
wald.asreml(wald_asreml$DF[[1]]) # Distrito Federal
wald.asreml(wald_asreml$MS[[1]]) # Mato Grosso do Sul
wald.asreml(wald_asreml$RO[[1]]) # Rondonia
wald.asreml(wald_asreml$RJ[[1]]) # Rio de Janeiro

#### Graficos ####
# Acre
p_aov.AC <- ggplot(as.data.frame(anova_aov$AC[[1]]$Within$residuals),
                   aes(x = anova_aov$AC[[1]]$Within$residuals)) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - aov()",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

p_lmer.AC <- ggplot(as.data.frame(residuals(anova_lmer$AC[[1]])),
                    aes(x = residuals(anova_lmer$AC[[1]]))) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - lmer()",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

p_asreml.AC <- ggplot(as.data.frame(residuals(wald_asreml$AC[[1]])),
                      aes(x = residuals(wald_asreml$AC[[1]]))) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - asreml()",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

grid.arrange(p_aov.AC, p_lmer.AC, p_asreml.AC, ncol = 2, 
             top = "Local: Acre")

# Distrito Federal
p_aov.DF <- ggplot(as.data.frame(anova_aov$DF[[1]]$Within$residuals),
                   aes(x = anova_aov$DF[[1]]$Within$residuals)) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - aov()",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

p_lmer.DF <- ggplot(as.data.frame(residuals(anova_lmer$DF[[1]])),
                    aes(x = residuals(anova_lmer$DF[[1]]))) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - lmer()",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

p_asreml.DF <- ggplot(as.data.frame(residuals(wald_asreml$DF[[1]])),
                      aes(x = residuals(wald_asreml$DF[[1]]))) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - asreml()",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

grid.arrange(p_aov.DF, p_lmer.DF, p_asreml.DF, ncol = 2, 
             top = "Local: Distrito Federal")

# Mato Grosso do Sul
p_aov.MS <- ggplot(as.data.frame(anova_aov$MS[[1]]$Within$residuals),
                   aes(x = anova_aov$MS[[1]]$Within$residuals)) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - aov()",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

p_lmer.MS <- ggplot(as.data.frame(residuals(anova_lmer$MS[[1]])),
                    aes(x = residuals(anova_lmer$MS[[1]]))) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - lmer()",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

p_asreml.MS <- ggplot(as.data.frame(residuals(wald_asreml$MS[[1]])),
                      aes(x = residuals(wald_asreml$MS[[1]]))) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - asreml()",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

grid.arrange(p_aov.MS, p_lmer.MS, p_asreml.MS, ncol = 2, 
             top = "Mato Grosso do Sul")

# Rondonia
p_aov.RO <- ggplot(as.data.frame(anova_aov$RO[[1]]$Within$residuals),
                   aes(x = anova_aov$RO[[1]]$Within$residuals)) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - aov()",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

p_lmer.RO <- ggplot(as.data.frame(residuals(anova_lmer$RO[[1]])),
                    aes(x = residuals(anova_lmer$RO[[1]]))) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - lmer()",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

p_asreml.RO <- ggplot(as.data.frame(residuals(wald_asreml$RO[[1]])),
                      aes(x = residuals(wald_asreml$RO[[1]]))) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - asreml()",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

grid.arrange(p_aov.RO, p_lmer.RO, p_asreml.RO, ncol = 2, 
             top = "Local: Rondônia")

# Rio de Janeiro
p_aov.RJ <- ggplot(as.data.frame(anova_aov$RJ[[1]]$Within$residuals),
                   aes(x = anova_aov$RJ[[1]]$Within$residuals)) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - aov()/Local: RJ",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

p_lmer.RJ <- ggplot(as.data.frame(residuals(anova_lmer$RJ[[1]])),
                    aes(x = residuals(anova_lmer$RJ[[1]]))) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - lmer()/Local: RJ",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

p_asreml.RJ <- ggplot(as.data.frame(residuals(wald_asreml$RJ[[1]])),
                      aes(x = residuals(wald_asreml$RJ[[1]]))) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=after_stat(density)),
                 alpha = 0.75) +
  labs(title = "Histograma dos resíduos - asreml()",
       x = "Resíduos",
       y = "Frequência") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5))

grid.arrange(p_aov.RJ, p_lmer.RJ, p_asreml.RJ, ncol = 2, 
             top = "Local: Rio de Janeiro")


save.image("2.Analise_de_Variancias.RData")