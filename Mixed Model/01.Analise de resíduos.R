#---------------------------------#
#       Analise de resíduos       #
#     By Vitória Bizão Murakami   #
#---------------------------------#

# Limpando o ambiente
rm(list=ls())

# Carregando os pacotes
pkg = c("ggfortify", "gridExtra", "ggplot2", "tidyverse", "readxl")
sapply(pkg, require, character.only=TRUE)

# Carregando os dados
setwd("~/Documents/Dissertacao")
dados_res <- read_csv('dados_fil')
dados_res <- dados_fil
str(dados_res)

# Definindo dados como fatores e Renomeando as colunas
dados_res <- dados_res %>% 
  mutate(Genotipo = as.factor(Genotipo),
         Local = as.factor(Local),
         Corte = as.factor(Corte),
         Peso = as.double(Peso),
         Bloco = as.factor(Bloco)) 

#--- Para o ambiente AC ---#
# Selecionando apenas os dados no ambiente AC
dados_AC <- dados_res %>% 
  filter(Local == 'AC') %>% 
  droplevels() 

# Loop - AC
nc_AC <- levels(dados_AC$Corte)   # Cria variável com os n Cortes de AC
mod_AC <- list() # Cria uma lista vazia
p_AC <- list()

#------------------- Descrição Loop ------------------#
# 1) Para cada i em n Cortes em AC
# 2) Seleciona os dados em que Corte = i
# 3) Aplique o modelo lm() para esse subconjunto de dados
# 4) Guarde os resultados em mod_AC
# 5) Print o i (corte)
# 6) Plota os resíduos do modelo i e guarda em p_AC 
# 7) Repete até i = ultimo corte
#-----------------------------------------------------#

for (i in nc_AC) {
  edata = droplevels(subset(dados_AC, Corte == i))
  mod <- lm(Peso ~ Bloco + Genotipo, data = edata)
  mod_AC[[i]] <- mod
  print(i)
  p_AC[[i]] <- local({
    i <- i
    p1 <- ggplot(mod_AC[[i]], aes(x = mod_AC[[i]]$residuals)) +
      geom_histogram(bins = 11,
                     colour = "black",
                     fill = "#CC662f",
                     aes(y=..density..),
                     alpha = 0.75) +
      labs(title = paste("Local: AC - Corte", i, sep = " "),
           x = "Residuals",
           y = "Frequency") + 
      geom_density(color = "#003350") +
      theme_classic() +
      theme(axis.text.x = element_text(size = 7),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 8.5),
            plot.subtitle = element_text(size = 7.5))
  })
}

# Histogramas dos resíduos AC:
plots_AC <- grid.arrange(p_AC[[1]], p_AC[[2]], p_AC[[3]], p_AC[[4]],
                         p_AC[[5]], p_AC[[6]], p_AC[[7]], p_AC[[8]], 
                         p_AC[[9]], p_AC[[10]], p_AC[[11]], p_AC[[12]],
                         p_AC[[13]], p_AC[[14]], p_AC[[15]], p_AC[[16]],
                         p_AC[[17]], 
                         ncol = 5)

# ANOVA de cada corte AC:
ANOVA_AC <- print(list(anova(mod_AC[[1]]), anova(mod_AC[[2]]), anova(mod_AC[[3]]),
                       anova(mod_AC[[4]]), anova(mod_AC[[5]]), anova(mod_AC[[6]]),
                       anova(mod_AC[[7]]), anova(mod_AC[[8]]), anova(mod_AC[[9]]),
                       anova(mod_AC[[10]]), anova(mod_AC[[11]]), anova(mod_AC[[12]]),
                       anova(mod_AC[[13]]), anova(mod_AC[[14]]), anova(mod_AC[[15]]),
                       anova(mod_AC[[16]]), anova(mod_AC[[17]])))


#--- Para o ambiente DF ---#
# Selecionando apenas os dados no ambiente DF
dados_DF <- dados_res %>%
  filter(Local == 'DF') %>%
  droplevels()

# Loop DF
nc_DF <- levels(dados_DF$Corte) # Cria variável com os n Cortes de DF
mod_DF <- list() # Cria uma lista vazia
p_DF <- list()  # Cria uma lista vazia

for (i in nc_DF) {
  edata = droplevels(subset(dados_DF, Corte == i))
  mod <- lm(Peso ~ Bloco + Genotipo, data = edata)
  mod_DF[[i]] <- mod
  print(i)
  p_DF[[i]] <- local({
    i <- i
    p1 <- ggplot(mod_DF[[i]], aes(x = mod_DF[[i]]$residuals)) +
      geom_histogram(bins = 11,
                     colour = "black",
                     fill = "#CC662f",
                     aes(y=..density..),
                     alpha = 0.75) +
      labs(title = paste("Local: DF - Corte", i, sep = " "),
           x = "Residuals",
           y = "Frequency") + 
      geom_density(color = "#003350") +
      theme_classic() +
      theme(axis.text.x = element_text(size = 7),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 8.5),
            plot.subtitle = element_text(size = 7.5))
  })
}

# Histogramas resíduos DF:
plots_DF <- grid.arrange(p_DF[[1]], p_DF[[2]], p_DF[[3]], p_DF[[4]],
                         p_DF[[5]], p_DF[[6]], p_DF[[7]], p_DF[[8]],
                         p_DF[[9]], p_DF[[10]], p_DF[[11]],
                         ncol = 4)

# ANOVA de cada corte DF:
ANOVA_DF <- print(list(anova(mod_DF[[1]]), anova(mod_DF[[2]]), anova(mod_DF[[3]]),
                       anova(mod_DF[[4]]), anova(mod_DF[[5]]), anova(mod_DF[[6]]),
                       anova(mod_DF[[7]]), anova(mod_DF[[8]]), anova(mod_DF[[9]]),
                       anova(mod_DF[[10]]), anova(mod_DF[[11]])))

#--- Para o ambiente RJ ---#
# Selecionando apenas os dados no ambiente RJ
dados_RJ <- dados_res %>%
  filter(Local == 'RJ') %>%
  droplevels()

# Loop RJ:
nc_RJ <- levels(dados_RJ$Corte)
mod_RJ <- list() # Cria uma lista vazia
p_RJ <- list()  # Cria uma lista vazia

for (i in nc_RJ) {
  edata = droplevels(subset(dados_RJ, Corte == i))
  mod <- lm(Peso ~ Bloco + Genotipo, data = edata)
  mod_RJ[[i]] <- mod
  print(i)
  p_RJ[[i]] <- local({
    i <- i
    p1 <- ggplot(mod_RJ[[i]], aes(x = mod_RJ[[i]]$residuals)) +
      geom_histogram(bins = 11,
                     colour = "black",
                     fill = "#CC662f",
                     aes(y=..density..),
                     alpha = 0.75) +
      labs(title = paste("Local: RJ - Corte", i, sep = " "),
           x = "Residuals",
           y = "Frequency") + 
      geom_density(color = "#003350") +
      theme_classic() +
      theme(axis.text.x = element_text(size = 7),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 8.5),
            plot.subtitle = element_text(size = 7.5))
  })
}

# Histograma dos resíduos RJ:
plots_RJ <- grid.arrange(p_RJ[[1]], p_RJ[[2]], p_RJ[[3]], p_RJ[[4]],
                         p_RJ[[5]], p_RJ[[6]], p_RJ[[7]], p_RJ[[8]],
                         p_RJ[[9]], p_RJ[[10]], p_RJ[[11]], p_RJ[[12]],
                         p_RJ[[13]],
                         ncol = 5)

# ANOVA dos cortes RJ:
ANOVA_RJ <- print(list(anova(mod_RJ[[1]]), anova(mod_RJ[[2]]), anova(mod_RJ[[3]]),
                       anova(mod_RJ[[4]]), anova(mod_RJ[[5]]), anova(mod_RJ[[6]]),
                       anova(mod_RJ[[7]]), anova(mod_RJ[[8]]), anova(mod_RJ[[9]]),
                       anova(mod_RJ[[10]]), anova(mod_RJ[[11]]), anova(mod_RJ[[12]]),
                       anova(mod_RJ[[13]])))


#--- Para o ambiente MS ---#
# Selecionando apenas os dados no ambiente MS
dados_MS <- dados_res %>%
  filter(Local == 'MS') %>%
  droplevels()

# Loop MS:
nc_MS <- levels(dados_MS$Corte)
mod_MS <- list() # Cria uma lista vazia
p_MS <- list()  # Cria uma lista vazia

for (i in nc_MS) {
  edata = droplevels(subset(dados_MS, Corte == i))
  mod <- lm(Peso ~ Bloco + Genotipo, data = edata)
  mod_MS[[i]] <- mod
  print(i)
  p_MS[[i]] <- local({
    i <- i
    p1 <- ggplot(mod_MS[[i]], aes(x = mod_MS[[i]]$residuals)) +
      geom_histogram(bins = 11,
                     colour = "black",
                     fill = "#CC662f",
                     aes(y=..density..),
                     alpha = 0.75) +
      labs(title = paste("Local: MS - Corte", i, sep = " "),
           x = "Residuals",
           y = "Frequency") + 
      geom_density(color = "#003350") +
      theme_classic() +
      theme(axis.text.x = element_text(size = 7),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 8.5),
            plot.subtitle = element_text(size = 7.5))
  })
}

# Histograma dos resíduos MS:
plots_MS <- grid.arrange(p_MS[[1]], p_MS[[2]], p_MS[[3]], p_MS[[4]],
                         p_MS[[5]], p_MS[[6]], p_MS[[7]], p_MS[[8]],
                         p_MS[[9]], p_MS[[10]], p_MS[[11]], p_MS[[12]],
                         p_MS[[13]], p_MS[[14]],
                         ncol = 5)

# ANOVA Cortes MS:
ANOVA_MS <- print(list(anova(mod_MS[[1]]), anova(mod_MS[[2]]), anova(mod_MS[[3]]),
                       anova(mod_MS[[4]]), anova(mod_MS[[5]]), anova(mod_MS[[6]]),
                       anova(mod_MS[[7]]), anova(mod_MS[[8]]), anova(mod_MS[[9]]),
                       anova(mod_MS[[10]]), anova(mod_MS[[11]]), anova(mod_MS[[12]]),
                       anova(mod_MS[[13]]), anova(mod_MS[[14]])))


#--- Para o ambiente RO ---#
# Selecionando apenas os dados no ambiente RO
dados_RO <- dados_res %>%
  filter(Local == 'RO') %>%
  droplevels()

# Loop RO:
nc_RO <- levels(dados_RO$Corte)
mod_RO <- list() # Cria uma lista vazia
p_RO <- list()  # Cria uma lista vazia

for (i in nc_RO) {
  edata = droplevels(subset(dados_RO, Corte == i))
  mod <- lm(Peso ~ Bloco + Genotipo, data = edata)
  mod_RO[[i]] <- mod
  print(i)
  p_RO[[i]] <- local({
    i <- i
    p1 <- ggplot(mod_RO[[i]], aes(x = mod_RO[[i]]$residuals)) +
      geom_histogram(bins = 11,
                     colour = "black",
                     fill = "#CC662f",
                     aes(y=..density..),
                     alpha = 0.75) +
      labs(title = paste("Local: RO - Corte", i, sep = " "),
           x = "Residuals",
           y = "Frequency") + 
      geom_density(color = "#003350") +
      theme_classic() +
      theme(axis.text.x = element_text(size = 7),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 8.5),
            plot.subtitle = element_text(size = 7.5))
  })
}

# Histograma dos resíduos RO:
plots_RO <- grid.arrange(p_RO[[1]], p_RO[[2]], p_RO[[3]], p_RO[[4]],
                         p_RO[[5]], p_RO[[6]], p_RO[[7]], p_RO[[8]],
                         p_RO[[9]], p_RO[[10]], p_RO[[11]], p_RO[[12]],
                         p_RO[[13]], p_RO[[14]], p_RO[[15]], p_RO[[16]],
                         ncol = 5)

# ANOVA dos Cortes RO:
ANOVA_RO <- print(list(anova(mod_RO[[1]]), anova(mod_RO[[2]]), anova(mod_RO[[3]]),
                       anova(mod_RO[[4]]), anova(mod_RO[[5]]), anova(mod_RO[[6]]),
                       anova(mod_RO[[7]]), anova(mod_RO[[8]]), anova(mod_RO[[9]]),
                       anova(mod_RO[[10]]), anova(mod_RO[[11]]), anova(mod_RO[[12]]),
                       anova(mod_RO[[13]]), anova(mod_RO[[14]])))

#save.image('Analise_De_Residuos.RData')