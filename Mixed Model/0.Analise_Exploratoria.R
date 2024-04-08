#-------------------------------------#
#   Analise Exploratória e Filtragem  #
#      By Vitória Bizão Murakami      #
#-------------------------------------#



# Limpando o ambiente:
rm(list=ls())

# Carregando vários pacotes a serem usados na sessão simultaneamente:
pkg = c("tidyverse", "explore", "DataExplorer", "readxl", "gridExtra")
sapply(pkg, require, character.only=TRUE)

# Carregando os dados:
dados_exp <- read_xls('Dados_MFS.xls')

# Definindo dados como fatores e Renomeando as colunas:
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

#####--- Gráficos ---#####
#--- Geral ---#
# Boxplot do peso em função dos GENÓTIPOS - Geral:
p1_G <- ggplot(dados_exp, aes(x = Genotipo, y = Peso)) +
  geom_boxplot(color = "#003350",
               fill = "#CC662f",
               alpha = 0.75, 
               outlier.size = 1) +
  labs(x = "Genótipo",
       y = "Peso",
       title = 'Massa Seca Foliar ~ Genótipos') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
        plot.title = element_text(size = 12)); p1_G

# Boxplot do peso em função dos CORTES - Geral:
p2_G <- ggplot(dados_exp, aes(x = Corte, y = Peso)) +
  geom_boxplot(color = "#003350",
               fill = "#CC662f",
               alpha = 0.75, 
               outlier.size = 1) +
  theme_bw() +
  labs(x = "Corte",
       y = "Peso",
       title = 'Massa Seca Foliar ~ Corte') +
  theme(axis.text = element_text(size = 8),
        plot.title = element_text(size = 12)); p2_G

# Boxplot do peso em função dos Local - Geral:
p3_G <- ggplot(dados_exp, aes(x = Local, y = Peso)) +
  geom_boxplot(color = "#003350",
               fill = "#CC662f",
               alpha = 0.75, 
               outlier.size = 1,
               width = 0.45) +
  theme_bw() +
  labs(x = "Local",
       y = "Peso",
       title = 'Massa Seca Foliar ~ Local') +
  theme(axis.text = element_text(size = 8),
        plot.title = element_text(size = 12)); p3_G

# Histograma e densidade dos dados - Geral:
h_G <- dados_exp %>% ggplot(aes(x = Peso)) +
  geom_histogram(aes(y = ..density..),
                 fill = "#CC662f",
                 alpha = 0.75) +
  geom_density(color = "#003350") +
  labs(title = "Distribuição Massa Seca Foliar",
       x = "Massa Seca Foliar",
       y = "Frequência") +
  theme_bw() +
  theme(axis.text = element_text(size = 8),
        plot.title = element_text(size = 12)); h_G

# QQ-PLot - Geral:
qq_G <- ggplot(dados_exp, aes(sample = Peso)) + 
  stat_qq(color = "#003350") + 
  stat_qq_line(color = "#CC662f") +
  theme_bw() +
  labs(title = "QQ-Plot") +
  theme(axis.title = element_blank(),
        plot.title = element_text(size = 12)); qq_G

# Grid - Geral:
plots_G <- grid.arrange(p3_G, p2_G, p1_G, h_G, qq_G,
                        ncol = 3,
                        top = "Todos locais")


#--- Para cada local ---#
# Loop gráficos para cada local

#-------------- Descrição Loop ------------#
# 1) Para cada i em n Local em dados
# 2) Seleciona os dados em que Local = i
# 3) Print o i (Local)
# 4) Plota e guarda em p_ 
# 5) Repete até i = ultimo local
#------------------------------------------#

#--- Boxplots: Genótipos ---#
n <- levels(dados_exp$Local)   # Cria variável com os n Local
p_BoxG <- list() # Cria uma lista vazia

for (i in n) {
  edata = droplevels(subset(dados_exp, Local == i))
  print(i)
  p_BoxG[[i]] <- local({
    i <- i
    p_BoxG <- edata %>% ggplot(aes(x = Genotipo, y = Peso)) +
      geom_boxplot(color = "#003350",
                   fill = "#CC662f",
                   alpha = 0.75, 
                   outlier.size = 1) +
      labs(x = "Genótipo",
           y = "Peso",
           title = paste("Local:", i, sep = " ")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
            plot.title = element_text(size = 12))
  })
}

# Grid - Boxplots: Genótipos:
BoxG_all <- grid.arrange(p_BoxG[[1]], p_BoxG[[2]], p_BoxG[[3]], p_BoxG[[4]], p_BoxG[[5]], 
                         ncol = 3, 
                         top = "Massa Seca Foliar ~ Genótipo")

#--- Boxplots: Cortes ---#
n <- levels(dados_exp$Local) # Cria variável com os n Local
p_BoxC <- list() # Cria uma lista vazia

for (i in n) {
  edata = droplevels(subset(dados_exp, Local == i))
  print(i)
  p_BoxC[[i]] <- local({
    i <- i
    p_BoxC <- edata %>% ggplot(aes(x = Corte, y = Peso)) +
      geom_boxplot(color = "#003350",
                   fill = "#CC662f",
                   alpha = 0.75, 
                   outlier.size = 1) +
      theme_bw() +
      labs(x = "Corte",
           y = "Peso",
           title = paste("Local:", i, sep = " ")) +
      theme(axis.text = element_text(size = 8),
            plot.title = element_text(size = 12))
  })
}

# Grid - Boxplots: Cortes:
BoxC_all <- grid.arrange(p_BoxC[[1]], p_BoxC[[2]], p_BoxC[[3]], p_BoxC[[4]], p_BoxC[[5]], 
                         ncol = 3,
                         top = "Massa Seca Foliar ~ Corte")

#--- Histogramas ---#
n <- levels(dados_exp$Local)   # Cria variável com os n Locais
p_h <- list() # Cria uma lista vazia

for (i in n) {
  edata = droplevels(subset(dados_exp, Local == i))
  print(i)
  p_h[[i]] <- local({
    i <- i
    p_h <- edata %>% ggplot(aes(x = Peso)) +
      geom_histogram(aes(y = ..density..),
                     fill = "#CC662f",
                     alpha = 0.75) +
      geom_density(color = "#003350") +
      labs(title = paste("Local:", i, sep = " "),
           x = "Massa Seca Foliar",
           y = "Frequência") +
      theme_bw() +
      theme(axis.text = element_text(size = 8),
            plot.title = element_text(size = 12))
  })
}

# Grid - Histogramas e Densidade:
H_all <- grid.arrange(p_h[[1]], p_h[[2]], p_h[[3]], p_h[[4]], p_h[[5]],
                      ncol = 3,
                      top = "Histogramas")

#--- QQ-PLot ---#
n <- levels(dados_exp$Local)   # Cria variável com os n Cortes de AC
p_qq <- list() # Cria uma lista vazia

for (i in n) {
  edata = droplevels(subset(dados_exp, Local == i))
  print(i)
  p_qq[[i]] <- local({
    i <- i
    p_h <- edata %>% ggplot(aes(sample = Peso)) + 
      stat_qq(color = "#003350") + 
      stat_qq_line(color = "#CC662f") +
      theme_bw() +
      labs(title = paste("Local:", i, sep = " ")) +
      theme(axis.title = element_blank(),
            plot.title = element_text(size = 12))
  })
}

# Grid - QQ-Plot:
QQ_all<- grid.arrange(p_qq[[1]], p_qq[[2]], p_qq[[3]], p_qq[[4]], p_qq[[5]], 
                      ncol = 3,
                      top = "QQ-Plot")


#--- Grid de cada local---#
# Acre:
plots_AC <- grid.arrange(p_BoxG[[1]] + labs(title = "Peso ~ Genótipo"),
                         p_BoxC[[1]] + labs(title = "Peso ~ Corte"),
                         p_h[[1]] + labs(title = "Distribuição Peso"),
                         p_qq[[1]] + labs(title = "QQ-Plot"),
                         top = "Local: Acre",
                         ncol = 2)

# Distrito Federal:
plots_DF <- grid.arrange(p_BoxG[[2]] + labs(title = "Peso ~ Genótipo"),
                         p_BoxC[[2]] + labs(title = "Peso ~ Corte"),
                         p_h[[2]] + labs(title = "Distribuição Peso"),
                         p_qq[[2]] + labs(title = "QQ-Plot"),
                         top = "Local: Distrito Federal",
                         ncol = 2)

# Mato Grosso do Sul:
plots_MS <- grid.arrange(p_BoxG[[3]] + labs(title = "Peso ~ Genótipo"),
                         p_BoxC[[3]] + labs(title = "Peso ~ Corte"),
                         p_h[[3]] + labs(title = "Distribuição Peso"),
                         p_qq[[3]] + labs(title = "QQ-Plot"),
                         top = "Local: Mato Grosso do Sul",
                         ncol = 2)

# Rio de Janeiro:
plots_RJ <- grid.arrange(p_BoxG[[4]] + labs(title = "Peso ~ Genótipo"),
                         p_BoxC[[4]] + labs(title = "Peso ~ Corte"),
                         p_h[[4]] + labs(title = "Distribuição Peso"),
                         p_qq[[4]] + labs(title = "QQ-Plot"),
                         top = "Local: Rio de Janeiro",
                         ncol = 2)

# Rondônia:
plots_RO <- grid.arrange(p_BoxG[[5]] + labs(title = "Peso ~ Genótipo"),
                         p_BoxC[[5]] + labs(title = "Peso ~ Corte"),
                         p_h[[5]] + labs(title = "Distribuição Peso"),
                         p_qq[[5]] + labs(title = "QQ-Plot"),
                         top = "Local: Rondônia",
                         ncol = 2)

####--- Exploratória ---####

#--- Geral ---#
# explorer shiny
dados_exp %>% explore() # cria um shiny 

dados_exp %>% describe_all() # descrição dos dados

# DataExplorer
dados_exp %>% plot_intro() # gráfico com % de NAs

dados_exp %>% plot_correlation() # correlação

#--- Acre ---#
# explorer shiny
dados$AC %>% explore() # cria um shiny 

dados$AC %>% describe_all() # descrição dos dados

# DataExplorer
dados$AC %>% plot_intro() # gráfico com % de NAs

dados$AC %>% plot_correlation() # correlação

#--- Distrito Federal ---#
# explorer shiny
dados$DF %>% explore() # cria um shiny 

dados$DF %>% describe_all() # descrição dos dados

# DataExplorer
dados$DF %>% plot_intro() # gráfico com % de NAs

dados$DF %>% plot_correlation() # correlação

#--- Mato Grosso do Sul ---#
# explorer shiny
dados_exp$MS %>% explore() # cria um shiny 

dados$MS %>% describe_all() # descrição dos dados

# DataExplorer
dados$MS %>% plot_intro() # gráfico com % de NAs

dados$MS %>% plot_correlation() # correlação

#--- Rio de Janeiro ---#
# explorer shiny
dados$RJ %>% explore() # cria um shiny 

dados$RJ %>% describe_all() # descrição dos dados

# DataExplorer
dados$RJ %>% plot_intro() # gráfico com % de NAs

dados$RJ %>% plot_correlation() # correlação

#### Filtragem ####
# Removendo outlier
dados_fil <- droplevels(dados_exp[-which(dados_exp$Local == "MS" & dados_exp$Corte == 4 
                                         & dados_exp$Genotipo == "PM35" & dados_exp$Bloco == 2),])

# Removendo cortes perdidos
dados_fil <- droplevels(dados_fil[-which(dados_fil$Local == "DF" & dados_fil$Corte == 5),])
dados_fil <- droplevels(dados_fil[-which(dados_fil$Local == "RJ" & dados_fil$Corte == 1),])
dados_fil <- droplevels(dados_fil[-which(dados_fil$Local == "RJ" & dados_fil$Corte == 2),])

# library("writexl")
# write_xlsx(dados_fil,"path to store the Excel file\\dados_fil.xlsx")

#--- Gráficos filtrados ----
# Setting factors and changing columns names
df <- dados_fil %>% mutate(Genotipo = as.factor(Genotipo),
                           Local = as.factor(Local),
                           Corte = as.factor(Corte))
str(df)
sum(is.na(df))
df %>% mutate(Genotipo = forcats::fct_reorder(Genotipo, Peso, .desc = T))

# Boxplot do peso em função dos GENÓTIPOS - Geral:
p1_G.fil <- df %>% mutate(Genotipo = forcats::fct_reorder(Genotipo, Peso, .desc = T)) %>% 
  ggplot(aes(x = Genotipo, y = Peso)) +
  geom_boxplot(color = "#003350",
               fill = "#CC662f",
               alpha = 0.75, 
               outlier.size = 1) +
  labs(x = "Genotype",
       y = "LDM",
       title = 'Leaf Dry Matter ~ Genotypes') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 8),
        plot.title = element_text(size = 12))

# Boxplot do peso em função dos CORTES - Geral:
p2_G.fil <- ggplot(df, aes(x = Corte, y = Peso)) +
  geom_boxplot(color = "#003350",
               fill = "#CC662f",
               alpha = 0.75, 
               outlier.size = 1) +
  theme_bw() +
  labs(x = "Harvest",
       y = "LDM",
       title = 'Leaf Dry Matter ~ Harvest') +
  theme(axis.text = element_text(size = 8),
        plot.title = element_text(size = 12)); p2_G.fil

# Boxplot do peso em função dos Local - Geral:
p3_G.fil <- ggplot(df, aes(x = Local, y = Peso)) +
  geom_boxplot(color = "#003350",
               fill = "#CC662f",
               alpha = 0.75, 
               outlier.size = 1,
              width = 0.45) +
  theme_bw() +
  labs(x = "Site",
       y = "LDM",
       title = 'Leaf Dry Matter ~ Site') +
  theme(axis.text = element_text(size = 8),
        plot.title = element_text(size = 12))

# Histograma e densidade dos dados - Geral:
h_G.fil <- df %>% ggplot(aes(x = Peso)) +
  geom_histogram(aes(y = ..density..),
                 fill = "#CC662f",
                 alpha = 0.75) +
  geom_density(color = "#003350") +
  labs(title = "Distribuition",
       x = "Leaf Dry Matter",
       y = "Frequency") +
  theme_bw() +
  theme(axis.text = element_text(size = 8),
        plot.title = element_text(size = 12))

# QQ-PLot - Geral:
qq_G.fil <- ggplot(df, aes(sample = Peso)) + 
  stat_qq(color = "#003350") + 
  stat_qq_line(color = "#CC662f") +
  theme_bw() +
  labs(title = "QQ-Plot") +
  theme(axis.title = element_blank(),
        plot.title = element_text(size = 12))

# Grid - Geral:
plots_G.fil <- grid.arrange(p3_G.fil, p2_G.fil, p1_G.fil, h_G.fil, qq_G.fil,
                            ncol = 3,
                            top = "All sites")

# Para cada local - FILTRADO:
#--- Boxplots: Genótipos ---#
n <- levels(df$Local)   # Cria variável com os n Local
p_BoxG.fil <- list() # Cria uma lista vazia

for (i in n) {
  edata = droplevels(subset(df, Local == i))
  edata <- edata %>% mutate(Genotipo = forcats::fct_reorder(Genotipo, Peso, .desc = T))
  print(i)
  p_BoxG.fil[[i]] <- local({
    i <- i
    p_BoxG <- edata %>% ggplot(aes(x = Genotipo, y = Peso)) +
      geom_boxplot(color = "#003350",
                   fill = "#CC662f",
                   alpha = 0.75, 
                   outlier.size = 1) +
      labs(x = "Genotype",
           y = "LDM",
           title = paste("Site:", i, sep = " ")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
            plot.title = element_text(size = 12))
  })
}

# Grid - Boxplots: Genótipos:
grid.arrange(p_BoxG.fil[[1]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxG.fil[[2]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxG.fil[[3]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxG.fil[[4]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxG.fil[[5]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p1_G.fil + labs(title = "All sites")  + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             ncol = 3,
             top = grid::textGrob("Leaf Dry Matter ~ Genotype", gp = grid::gpar(fontsize = 14)),
             left = grid::textGrob("LDM", rot = 90, gp = grid::gpar(fontsize = 14)),
             bottom = grid::textGrob("Genotype", gp = grid::gpar(fontsize = 14)))

#--- Boxplots: Cortes ---#
n <- levels(df$Local) # Cria variável com os n Local
p_BoxC.fil <- list() # Cria uma lista vazia

for (i in n) {
  edata = droplevels(subset(df, Local == i))
  print(i)
  p_BoxC.fil[[i]] <- local({
    i <- i
    p_BoxC <- edata %>% ggplot(aes(x = Corte, y = Peso)) +
      geom_boxplot(color = "#003350",
                   fill = "#CC662f",
                   alpha = 0.75, 
                   outlier.size = 1) +
      theme_bw() +
      labs(x = "Harvest",
           y = "LDM",
           title = paste("Site:", i, sep = " ")) +
      theme(axis.text = element_text(size = 8),
            plot.title = element_text(size = 12))
  })
}

# Grid - Boxplots: Cortes:
grid.arrange(p_BoxC.fil[[1]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxC.fil[[2]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxC.fil[[3]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxC.fil[[4]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxC.fil[[5]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p2_G.fil + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + labs(title = "All sites"),
             ncol = 3,
             top = grid::textGrob("Leaf Dry Matter ~ Harvest", gp = grid::gpar(fontsize = 14)),
             left = grid::textGrob("LDM", rot = 90, gp = grid::gpar(fontsize = 14)),
             bottom = grid::textGrob("Harvest", gp = grid::gpar(fontsize = 14)))

#--- Histogramas ---#
n <- levels(df$Local)   # Cria variável com os n Locais
p_h.fil <- list() # Cria uma lista vazia

for (i in n) {
  edata = droplevels(subset(df, Local == i))
  print(i)
  p_h.fil[[i]] <- local({
    i <- i
    p_h <- edata %>% ggplot(aes(x = Peso)) +
      geom_histogram(aes(y = ..density..),
                     fill = "#CC662f",
                     alpha = 0.75) +
      geom_density(color = "#003350") +
      labs(title = paste("Site:", i, sep = " "),
           x = "Leaf Dry Matter",
           y = "Frequency") +
      theme_bw() +
      theme(axis.text = element_text(size = 8),
            plot.title = element_text(size = 12))
  })
}

# Grid - Histogramas e Densidade:
grid.arrange(p_h.fil[[1]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_h.fil[[2]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_h.fil[[3]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_h.fil[[4]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_h.fil[[5]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             h_G.fil + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + labs(title = "All sites"),
             top = grid::textGrob("Data density distribution", gp = grid::gpar(fontsize = 14)),
             left = grid::textGrob("Frequency", rot = 90, gp = grid::gpar(fontsize = 14)),
             bottom = grid::textGrob("Leaf dry matter", gp = grid::gpar(fontsize = 14)),
             ncol = 3)

#--- QQ-PLot ---#
n <- levels(df$Local)   # Cria variável com os n Cortes de AC
p_qq.fil <- list() # Cria uma lista vazia

for (i in n) {
  edata = droplevels(subset(df, Local == i))
  print(i)
  p_qq.fil[[i]] <- local({
    i <- i
    p_qq.fil <- edata %>% ggplot(aes(sample = Peso)) + 
      stat_qq(color = "#003350") + 
      stat_qq_line(color = "#CC662f") +
      theme_bw() +
      labs(title = paste("Site:", i, sep = " ")) +
      theme(axis.title = element_blank(),
            plot.title = element_text(size = 12))
  })
}

# Grid - QQ-Plot:
grid.arrange(p_qq.fil[[1]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_qq.fil[[2]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_qq.fil[[3]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_qq.fil[[4]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_qq.fil[[5]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             qq_G.fil + labs(title = "All sites"),
             ncol = 3,
             top = "QQ-Plot")

#### Descritiva ####
#---------- Descrição Loop ----------#
# Para cada i em n Locais dos dados
# Seleciona os dados em Local = i
# Guarde o subconjunto em dados
# Print o i (Local)
#------------------------------------#

n <- levels(dados_fil$Local)
dados_des <- list()

for (i in n) {
  temp <- droplevels(subset(dados_fil, Local == i))
  dados_des[[i]] <- list(Dados = temp,
                         Media_Genotipo = with(temp, tapply(Peso, Genotipo, mean, na.rm = T)),
                         Media_Corte = with(temp, tapply(Peso, Corte, mean, na.rm = T)),
                         Media_Bloco = with(temp, tapply(Peso, Bloco, mean, na.rm = T)),
                         Media_Genotipo_por_Corte = with(temp, tapply(Peso, list(Genotipo, Corte), mean, na.rm = T)),
                         Media_Genotipo_por_Bloco = with(temp, tapply(Peso, list(Genotipo, Bloco), mean, na.rm = T)),
                         Media_Corte_por_Bloco = with(temp, tapply(Peso, list(Corte, Bloco), mean, na.rm = T)),
                         Media_Genotipo_C_B = with(temp, tapply(Peso, list(Genotipo, Corte, Bloco), mean, na.rm = T))
  )
  print(i)
  
}

str(dados_des)

#write_csv(df, "df")
#save.image('0.Analise_Exploratória.RData')

#--- BLUPS ----
# Loading the data 
dados <- read.csv('blups_MSF.csv', h = T)

# Setting factors and changing columns names
df <- dados %>% mutate(Genotipo = as.factor(Genotipo),
                       Local = as.factor(Local),
                       Corte = as.factor(Corte))
str(df)
sum(is.na(df))
df %>% mutate(Genotipo = forcats::fct_reorder(Genotipo, Peso, .desc = T))

# Boxplot do peso em função dos GENÓTIPOS - Geral:
p1_G.blup <- df %>% mutate(Genotipo = forcats::fct_reorder(Genotipo, Peso, .desc = T)) %>% 
  ggplot(aes(x = Genotipo, y = Peso)) +
  geom_boxplot(color = "#003350",
               fill = "#CC662f",
               alpha = 0.75, 
               outlier.size = 1) +
  labs(x = "Genotype",
       y = "LDM",
       title = 'Leaf Dry Matter ~ Genotypes') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 8),
        plot.title = element_text(size = 12))

# Boxplot do peso em função dos CORTES - Geral:
p2_G.blup <- ggplot(df, aes(x = Corte, y = Peso)) +
  geom_boxplot(color = "#003350",
               fill = "#CC662f",
               alpha = 0.75, 
               outlier.size = 1) +
  theme_bw() +
  labs(x = "Harvest",
       y = "LDM",
       title = 'Leaf Dry Matter ~ Harvest') +
  theme(axis.text = element_text(size = 8),
        plot.title = element_text(size = 12))

# Boxplot do peso em função dos Local - Geral:
p3_G.blup <- ggplot(df, aes(x = Local, y = Peso)) +
  geom_boxplot(color = "#003350",
               fill = "#CC662f",
               alpha = 0.75, 
               outlier.size = 1,
               width = 0.45) +
  theme_bw() +
  labs(x = "Site",
       y = "LDM",
       title = 'Leaf Dry Matter ~ Site') +
  theme(axis.text = element_text(size = 8),
        plot.title = element_text(size = 12))

# Histograma e densidade dos dados - Geral:
h_G.blup <- df %>% ggplot(aes(x = Peso)) +
  geom_histogram(aes(y = ..density..),
                 fill = "#CC662f",
                 alpha = 0.75) +
  geom_density(color = "#003350") +
  labs(title = "Distribuition",
       x = "Leaf Dry Matter",
       y = "Frequency") +
  theme_bw() +
  theme(axis.text = element_text(size = 8),
        plot.title = element_text(size = 12))

# QQ-PLot - Geral:
qq_G.blup <- ggplot(df, aes(sample = Peso)) + 
  stat_qq(color = "#003350") + 
  stat_qq_line(color = "#CC662f") +
  theme_bw() +
  labs(title = "QQ-Plot") +
  theme(axis.title = element_blank(),
        plot.title = element_text(size = 12))

# Grid - Geral:
plots_G.blup <- grid.arrange(p3_G.blup, p2_G.blup, p1_G.blup, h_G.blup, qq_G.blup,
                             ncol = 3,
                             top = "All sites")

# Para cada local - FILTRADO:
#--- Boxplots: Genótipos ---#
n <- levels(df$Local)   # Cria variável com os n Local
p_BoxG.blup <- list() # Cria uma lista vazia

for (i in n) {
  edata = droplevels(subset(df, Local == i))
  edata <- edata %>% mutate(Genotipo = forcats::fct_reorder(Genotipo, Peso, .desc = T))
  print(i)
  p_BoxG.blup[[i]] <- local({
    i <- i
    p_BoxG <- edata %>% ggplot(aes(x = Genotipo, y = Peso)) +
      geom_boxplot(color = "#003350",
                   fill = "#CC662f",
                   alpha = 0.75, 
                   outlier.size = 1) +
      labs(x = "Genotype",
           y = "LDM",
           title = paste("Site:", i, sep = " ")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
            plot.title = element_text(size = 12))
  })
}

# Grid - Boxplots: Genótipos:
grid.arrange(p_BoxG.blup[[1]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxG.blup[[2]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxG.blup[[3]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxG.blup[[4]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxG.blup[[5]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p1_G.blup + labs(title = "All sites")  + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             ncol = 3,
             top = grid::textGrob("Leaf Dry Matter ~ Genotype", gp = grid::gpar(fontsize = 14)),
             left = grid::textGrob("LDM", rot = 90, gp = grid::gpar(fontsize = 14)),
             bottom = grid::textGrob("Genotype", gp = grid::gpar(fontsize = 14)))

#--- Boxplots: Cortes ---#
n <- levels(df$Local) # Cria variável com os n Local
p_BoxC.blup <- list() # Cria uma lista vazia

for (i in n) {
  edata = droplevels(subset(df, Local == i))
  print(i)
  p_BoxC.blup[[i]] <- local({
    i <- i
    p_BoxC <- edata %>% ggplot(aes(x = Corte, y = Peso)) +
      geom_boxplot(color = "#003350",
                   fill = "#CC662f",
                   alpha = 0.75, 
                   outlier.size = 1) +
      theme_bw() +
      labs(x = "Harvest",
           y = "LDM",
           title = paste("Site:", i, sep = " ")) +
      theme(axis.text = element_text(size = 8),
            plot.title = element_text(size = 12))
  })
}

# Grid - Boxplots: Cortes:
grid.arrange(p_BoxC.blup[[1]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxC.blup[[2]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxC.blup[[3]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxC.blup[[4]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_BoxC.blup[[5]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p2_G.blup + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + labs(title = "All sites"),
             ncol = 3,
             top = grid::textGrob("Leaf Dry Matter ~ Harvest", gp = grid::gpar(fontsize = 14)),
             left = grid::textGrob("LDM", rot = 90, gp = grid::gpar(fontsize = 14)),
             bottom = grid::textGrob("Harvest", gp = grid::gpar(fontsize = 14)))

#--- Histogramas ---#
n <- levels(df$Local)   # Cria variável com os n Locais
p_h.blup <- list() # Cria uma lista vazia

for (i in n) {
  edata = droplevels(subset(df, Local == i))
  print(i)
  p_h.blup[[i]] <- local({
    i <- i
    p_h <- edata %>% ggplot(aes(x = Peso)) +
      geom_histogram(aes(y = ..density..),
                     fill = "#CC662f",
                     alpha = 0.75) +
      geom_density(color = "#003350") +
      labs(title = paste("Site:", i, sep = " "),
           x = "Leaf Dry Matter",
           y = "Frequency") +
      theme_bw() +
      theme(axis.text = element_text(size = 8),
            plot.title = element_text(size = 12))
  })
}

# Grid - Histogramas e Densidade:
grid.arrange(p_h.blup[[1]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_h.blup[[2]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_h.blup[[3]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_h.blup[[4]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_h.blup[[5]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             h_G.blup + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + labs(title = "All sites"),
             top = grid::textGrob("Histograms", gp = grid::gpar(fontsize = 14)),
             left = grid::textGrob("Frequency", rot = 90, gp = grid::gpar(fontsize = 14)),
             bottom = grid::textGrob("Leaf dry matter", gp = grid::gpar(fontsize = 14)),
             ncol = 3)

#--- QQ-PLot ---#
n <- levels(df$Local)   # Cria variável com os n Cortes de AC
p_qq.blup <- list() # Cria uma lista vazia

for (i in n) {
  edata = droplevels(subset(df, Local == i))
  print(i)
  p_qq.blup[[i]] <- local({
    i <- i
    p_qq.blup <- edata %>% ggplot(aes(sample = Peso)) + 
      stat_qq(color = "#003350") + 
      stat_qq_line(color = "#CC662f") +
      theme_bw() +
      labs(title = paste("Site:", i, sep = " ")) +
      theme(axis.title = element_blank(),
            plot.title = element_text(size = 12))
  })
}

# Grid - QQ-Plot:
grid.arrange(p_qq.blup[[1]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_qq.blup[[2]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_qq.blup[[3]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_qq.blup[[4]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             p_qq.blup[[5]] + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
             qq_G.blup + labs("All sites"),
             ncol = 3,
             top = "QQ-Plot")

ggplot(df, aes(x = Corte, y = Peso, fill = Genotipo)) +
  geom_boxplot() + 
  facet_wrap(~ Local) +
  theme_bw()

ggplot(df, aes(x = Local, y = Peso, fill = Genotipo)) +
  geom_boxplot() +
  theme_bw()