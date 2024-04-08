####sommer####

library(sommer)
  
  vignette("v1.sommer.quick.start")
  vignette("v2.sommer.changes.and.faqs")
  vignette("v3.sommer.qg")
  vignette("v4.sommer.gxe")

#------------------------------------------------------------------------------#
####Quick start for the sommer package####
  
## 1 - Univariate homogeneous variance model
  
library(sommer)
  
  data(DT_example)
  DT <- DT_example; View(DT)
  ## solving for r records
  ans1r <- mmer(Yield~Env,
                random= ~ Name + Env:Name,
                rcov= ~ units,
                data=DT, verbose = FALSE)
  summary(ans1r)$varcomp
  summary(ans1r) 

  ans1r$U$Name
  
## 2 - Univariate heterogeneous variance model
  
  data(DT_example)
  DT <- DT_example
  ans2r <- mmer(Yield~Env,
                random= ~Name + vsr(dsr(Env),Name),
                rcov= ~ vsr(dsr(Env),units),
                data=DT, verbose = FALSE)
  summary(ans2r)$varcomp
  summary(ans2r)
  
  DT=DT[with(DT, order(Env)), ]
  ans2c <- mmec(Yield~Env,
                random= ~Name + vsc(dsc(Env),isc(Name)),
                rcov= ~ vsc(dsc(Env),isc(units)),
                data=DT, verbose = FALSE)
  summary(ans2c)$varcomp
  summary(ans2c)
  
## 3 - Unstructured variance models
  
  data(DT_example)
  DT <- DT_example
  ans3r <- mmer(Yield~Env,
                random=~ vsr(usr(Env),Name),
                rcov=~vsr(dsr(Env),units),
                data=DT, verbose = FALSE)
  summary(ans3r)$varcomp
  summary(ans3r)
  
  DT=DT[with(DT, order(Env)), ]
  ans3c <- mmec(Yield~Env,
                random=~ vsc(usc(Env),isc(Name)),
                rcov=~vsc(dsc(Env),isc(units)),
                data=DT, verbose = FALSE)
  summary(ans3c)$varcomp
  summary(ans3c)
  
## 4 - Multivariate homogeneous variance models
  
  data(DT_example)
  DT <- DT_example
  DT$EnvName <- paste(DT$Env,DT$Name)
  DT$Yield <- as.vector(scale(DT$Yield))
  DT$Weight <- as.vector(scale(DT$Weight))
  ans4r <- mmer(cbind(Yield, Weight) ~ Env,
                random= ~ vsr(Name, Gtc=unsm(2)),
                rcov= ~ vsr(units, Gtc=diag(2)),
                data=DT, verbose = FALSE)
  summary(ans4r)$varcomp
  summary(ans4r)
  
#------------------------------------------------------------------------------#
## Fitting genotype by enviroment models in sommer
  
## 1 - Single environment model
  library(sommer)
  data(DT_example)
  DT <- DT_example; str(DT)
  A <- A_example; str(A_example); View(A_example)
  ansSingle <- mmer(Yield~1,
                    random= ~ vsr(Name, Gu=A),
                    rcov= ~ units,
                    data=DT, verbose = FALSE)
  summary(ansSingle)

## 2 - MET_main effect model 
  #MET - Multi-environment trial
  ansMain <- mmer(Yield~Env,
                  random= ~ vsr(Name, Gu=A),
                  rcov= ~ units,
                  data=DT, verbose = FALSE)
  summary(ansMain)
  
## 3 - MET: diagonal model (DG)
  ansDG <- mmer(Yield~Env,
                random= ~ vsr(dsr(Env),Name, Gu=A),
                rcov= ~ units,
                data=DT, verbose = FALSE)
  summary(ansDG)
  
## 4 - MET: compund symmetry model (CS)
  E <- diag(length(unique(DT$Env)))
  rownames(E) <- colnames(E) <- unique(DT$Env)
  EA <- kronecker(E,A, make.dimnames = TRUE)
  ansCS <- mmer(Yield~Env,
                random= ~ vsr(Name, Gu=A) + vsr(Env:Name, Gu=EA),
                rcov= ~ units,
                data=DT, verbose = FALSE)
  summary(ansCS)
  
## 5 -  MET: unstructured model (US)
  ansUS <- mmer(Yield~Env,
                random= ~ vsr(usr(Env),Name, Gu=A),
                rcov= ~ units,
                data=DT, verbose = FALSE)
  summary(ansUS)
  # adjust variance BLUPs by adding covariances
  # ansUS$U[1:6] <- unsBLUP(ansUS$U[1:6])
#------------------------------------------------------------------------------#
  
## Randomized block design 
  
### Filtragem dos dados
  
  library(readr)
  library(sommer)
  
  dados_fil_embrapa <- read_csv("Examples/dados_fil-embrapa.csv")
  
  dados_fil_embrapa$block <- as.factor(dados_fil_embrapa$block)
  dados_fil_embrapa$gen <- as.factor(dados_fil_embrapa$gen)
  dados_fil_embrapa$local <- as.factor(dados_fil_embrapa$local)
  dados_fil_embrapa$corte <- as.factor(dados_fil_embrapa$corte)
  
  library(tidyverse)
  library(dplyr)
  
  data_fil_block <- dados_fil_embrapa %>% 
    filter(local %in% c("AC","MS")) %>%
    filter(corte %in% c("2","3","5")) %>% droplevels()
  str(data_fil_block)
  
  # mod <- mmer(fixed = as.formula(input$fixed), 
  #             random = as.formula(input$random), 
  #             rcov = as.formula(input$rcov),
  #             data = dat)
  
  # Modelo 01 - Sem do efeito fixo ou interação do 'block'
  ans01r <- mmer(peso ~ local,
                random= ~ gen + local:gen,
                rcov= ~ units,
                data= data_fil_block)
  summary(ans01r)$varcomp
  summary(ans01r)
  
  # Modelo 02 - Com inserção da interação 'local:block'
  ans02r <- mmer(peso ~ local,
                 random= ~ gen + local:gen + local:block,
                 rcov= ~ units,
                 data= data_fil_block)
  summary(ans02r)$varcomp
  summary(ans02r)
  
  # Modelo 03 - Com inserção do efeito fixo 'block'
  ans03r <- mmer(peso ~ local + block,
                 random= ~ gen + local:gen,
                 rcov= ~ units,
                 data= data_fil_block)
  summary(ans03r)$varcomp
  summary(ans03r)
  
  # Modelo 04 - Com inserção do efeito fixo e interação do 'block'
  ans04r <- mmer(peso ~ local + block,
                 random= ~ gen + local:gen + local:block,
                 rcov= ~ units,
                 data= data_fil_block)
  summary(ans04r)$varcomp
  summary(ans04r)
  
  
  ans01r$Beta
  ans02r$Beta
  ans03r$Beta
  ans04r$Beta
  
  summary(ans01r)$varcomp
  summary(ans02r)$varcomp
  summary(ans03r)$varcomp
  summary(ans04r)$varcomp
  
  var01 <- summary(ans01r)$varcomp
  var02 <- summary(ans02r)$varcomp
  var03 <- summary(ans03r)$varcomp
  var04 <- summary(ans04r)$varcomp
  
  sum(var01[,1])
  sum(var02[,1])
  sum(var03[,1])
  sum(var04[,1])
  
  AIC <- c(ans01r$AIC, ans02r$AIC, ans03r$AIC, ans04r$AIC)
  BIC <- c(ans01r$BIC, ans02r$BIC, ans03r$BIC, ans04r$AIC)

  AB.01 <- cbind(AIC, BIC);AB.01  

#------------------------------------------------------------------------------#
## Split plot design 
  
  #Dados - Parciais - MS
  data_split_MS <- dados_fil_embrapa %>% 
    filter(local %in% "MS") %>%
    filter(corte %in% c("2","3","4","5")) %>% droplevels()
  str(data_split_MS)
  
  # Modelo 11 - Sem do efeito fixo ou interação do 'block'
  ans11r <- mmer(Peso ~ Corte,
                random= ~ Genotipo + Genotipo:Corte,
                rcov= ~ units,
                data= data_split_MS)
  summary(ans11r)$varcomp
  summary(ans11r)
  
  # Modelo 12 - Com inserção da interação 'corte:block'
  ans12r <- mmer(Peso ~ Corte,
                 random= ~ Genotipo + Genotipo:Corte + Corte:Bloco,
                 rcov= ~ units,
                 data=data_split_MS)
  summary(ans12r)$varcomp
  summary(ans12r)
  
  # Modelo 13 - Com inserção do efeito fixo 'block'
  ans13r <- mmer(Peso ~ Corte + Bloco,
                 random= ~ Genotipo + Genotipo:Corte,
                 rcov= ~ units,
                 data=data_split_MS)
  summary(ans13r)$varcomp
  summary(ans13r)
  
  #Dados - Parciais - MS
  data_split_MS <- dados_fil %>% 
    filter(Local %in% "MS") %>%
    filter(Corte %in% c("2","3","4","5")) %>% droplevels()
  str(data_split_MS)
  
  # Modelo 14 - Com inserção do efeito fixo e da interação 'block'
  ans14r <- mmer(Peso ~ Corte + Bloco,
                 random= ~ Genotipo + Genotipo:Corte + Corte:Bloco,
                 rcov= ~ units,
                 data=data_split_MS)
  summary(ans14r)$varcomp
  summary(ans14r)
  
  ans14.1r <- mmer(Peso ~ Corte + Bloco,
                   random= ~ Genotipo + vsr(dsr(Corte),Genotipo) + Corte:Bloco,
                   rcov= ~ vsr(dsr(Corte),units),
                   data=data_split_MS)
  summary(ans14.1r)$varcomp
  summary(ans14.1r)
  
  ans14.2r <- mmer(Peso ~ Corte + Bloco,
                   random= ~ Genotipo + vsr(usr(Corte),Genotipo) + Corte:Bloco,
                   rcov= ~ vsr(dsr(Corte),units),
                   data=data_split_MS)
  summary(ans14.2r)$varcomp
  summary(ans14.2r)
  
  ans14.2r$U$Genotipo
  
  AIC <- c(ans14r$AIC, ans14.1r$AIC, ans14.2r$AIC)
  BIC <- c(ans14r$BIC, ans14.1r$BIC, ans14.2r$BIC)
  
  AB <- cbind(AIC, BIC);AB
  
  # Modelo 15 - Com inserção do efeito fixo e da interação 'block'
  ans15r <- mmer(Peso ~ Corte + Bloco,
                 random= ~ Corte:Bloco + Genotipo + Genotipo:Bloco + Genotipo:Corte,
                 rcov= ~ units,
                 data=data_split_MS)
  summary(ans15r)$varcomp
  summary(ans15r)
  
  # Modelo 16 - Com inserção da interação 'corte:block'
  ans16r <- mmer(Peso ~ Corte,
                 random= ~ Corte:Bloco + Genotipo + Genotipo:Bloco + Genotipo:Corte,
                 rcov= ~ units,
                 data=data_split_MS)
  summary(ans16r)$varcomp
  summary(ans16r)
  
  ans11r$Beta
  ans12r$Beta
  ans13r$Beta
  ans14r$Beta
  
  summary(ans11r)$varcomp
  summary(ans12r)$varcomp
  summary(ans13r)$varcomp
  summary(ans14r)$varcomp
  
  var11 <- summary(ans11r)$varcomp
  var12 <- summary(ans12r)$varcomp
  var13 <- summary(ans13r)$varcomp
  var14 <- summary(ans14r)$varcomp
  
  sum(var11[,1])
  sum(var12[,1])
  sum(var13[,1])
  sum(var14[,1])
  
  AIC <- c(ans11r$AIC, ans12r$AIC, ans13r$AIC, ans14r$AIC, ans15r$AIC, ans16r$AIC)
  BIC <- c(ans11r$BIC, ans12r$BIC, ans13r$BIC, ans14r$AIC, ans15r$BIC, ans16r$BIC)
  
  AB <- cbind(AIC, BIC);AB
  
  #Dados - completos - MS
  data_split_MS_full <- dados_fil_embrapa %>% 
    filter(local %in% "MS") %>%
    droplevels()
  str(data_split_MS_full)
  
  # Modelo 21 - Sem do efeito fixo ou interação do 'block'
  ans21r <- mmer(peso ~ corte,
                 random= ~ gen + gen:corte,
                 rcov= ~ units,
                 data=data_split_MS_full)
  summary(ans21r)$varcomp
  summary(ans21r)
  
  # Modelo 22 - Com inserção da interação 'corte:block'
  ans22r <- mmer(peso ~ corte,
                 random= ~ gen + gen:corte + corte:block,
                 rcov= ~ units,
                 data=data_split_MS_full)
  summary(ans22r)$varcomp
  summary(ans22r)
  
  # Modelo 23 - Com inserção do efeito fixo 'block'
  ans23r <- mmer(peso ~ corte + block,
                 random= ~ gen + gen:corte,
                 rcov= ~ units,
                 data=data_split_MS_full)
  summary(ans23r)$varcomp
  summary(ans23r)
  
  # Modelo 24 - Com inserção do efeito fixo e da interação 'block'
  ans24r <- mmer(peso ~ corte + block,
                 random= ~ gen + gen:corte + corte:block,
                 rcov= ~ units,
                 data=data_split_MS_full)
  summary(ans24r)$varcomp
  summary(ans24r)
  
  ans24.1r <- mmer(peso ~ corte + block,
                 random= ~ gen + vsr(dsr(corte),gen) + corte:block,
                 rcov= ~ vsr(dsr(corte),units),
                 data=data_split_MS_full)
  summary(ans24.1r)$varcomp
  summary(ans24.1r)
  
  ans24.2r <- mmer(peso ~ corte + block,
                   random= ~ gen + vsr(usr(corte),gen) + corte:block,
                   rcov= ~ vsr(dsr(corte),units),
                   data=data_split_MS_full)
  summary(ans24.1r)$varcomp
  summary(ans24.1r)
  
  ans21r$Beta
  ans22r$Beta
  ans23r$Beta
  ans24r$Beta
  
  summary(ans21r)$varcomp
  summary(ans22r)$varcomp
  summary(ans23r)$varcomp
  summary(ans24r)$varcomp
  
  var21 <- summary(ans21r)$varcomp
  var22 <- summary(ans22r)$varcomp
  var23 <- summary(ans23r)$varcomp
  var24 <- summary(ans24r)$varcomp
  
  sum(var21[,1])
  sum(var22[,1])
  sum(var23[,1])
  sum(var24[,1])
  
  AIC <- c(ans21r$AIC, ans22r$AIC, ans23r$AIC, ans24r$AIC)
  BIC <- c(ans21r$BIC, ans22r$BIC, ans23r$BIC, ans24r$AIC)
  
  AB <- cbind(AIC, BIC);AB
  
  #Dados - Parciais - AC
  data_split_AC <- dados_fil_embrapa %>% 
    filter(local %in% "AC") %>%
    filter(corte %in% c("2","3","4","5")) %>% droplevels()
  str(data_split_MS)
  
  # Modelo 31 - Sem do efeito fixo ou interação do 'block'
  ans31r <- mmer(peso ~ corte,
                 random= ~ gen + gen:corte,
                 rcov= ~ units,
                 data=data_split_AC)
  summary(ans31r)$varcomp
  summary(ans31r)
  
  # Modelo 32 - Com inserção da interação 'corte:block'
  ans32r <- mmer(peso ~ corte,
                 random= ~ gen + gen:corte + corte:block,
                 rcov= ~ units,
                 data=data_split_AC)
  summary(ans32r)$varcomp
  summary(ans32r)
  
  # Modelo 33 - Com inserção do efeito fixo 'block'
  ans33r <- mmer(peso ~ corte + block,
                 random= ~ gen + gen:corte,
                 rcov= ~ units,
                 data=data_split_AC)
  summary(ans33r)$varcomp
  summary(ans33r)
  
  # Modelo 34 - Com inserção do efeito fixo e da interação 'block'
  ans34r <- mmer(peso ~ corte + block,
                 random= ~ gen + gen:corte + corte:block,
                 rcov= ~ units,
                 data=data_split_AC)
  summary(ans34r)$varcomp
  summary(ans34r)
  
  ans31r$Beta
  ans32r$Beta
  ans33r$Beta
  ans34r$Beta
  
  summary(ans31r)$varcomp
  summary(ans32r)$varcomp
  summary(ans33r)$varcomp
  summary(ans34r)$varcomp
  
  var31 <- summary(ans31r)$varcomp
  var32 <- summary(ans32r)$varcomp
  var33 <- summary(ans33r)$varcomp
  var34 <- summary(ans34r)$varcomp
  
  sum(var31[,1])
  sum(var32[,1])
  sum(var33[,1])
  sum(var34[,1])
  
  AIC <- c(ans31r$AIC, ans32r$AIC, ans33r$AIC, ans34r$AIC)
  BIC <- c(ans31r$BIC, ans32r$BIC, ans33r$BIC, ans34r$AIC)
  
  AB <- cbind(AIC, BIC);AB
  
  #Dados - Parciais - AC
  data_split_AC <- dados_fil %>% 
    filter(Local %in% "AC") %>%
    filter(Corte %in% c("2","3","4","5")) %>% droplevels()
  str(data_split_AC)
  
  # Modelo 14 - Com inserção do efeito fixo e da interação 'block'
  ans34r <- mmer(Peso ~ Corte + Bloco,
                 random= ~ Genotipo + Genotipo:Corte + Corte:Bloco,
                 rcov= ~ units,
                 data=data_split_AC)
  summary(ans34r)$varcomp
  summary(ans34r)
  
  ans34.1r <- mmer(Peso ~ Corte + Bloco,
                   random= ~ Genotipo + vsr(dsr(Corte),Genotipo) + Corte:Bloco,
                   rcov= ~ vsr(dsr(Corte),units),
                   data=data_split_AC)
  summary(ans34.1r)$varcomp
  summary(ans34.1r)
  
  ans34.2r <- mmer(Peso ~ Corte + Bloco,
                   random= ~ Genotipo + vsr(usr(Corte),Genotipo) + Corte:Bloco,
                   rcov= ~ vsr(dsr(Corte),units),
                   data=data_split_AC)
  summary(ans34.2r)$varcomp
  summary(ans34.2r)
  
  ans34.2r$U$Genotipo
  
  AIC <- c(ans34r$AIC, ans34.1r$AIC, ans34.2r$AIC)
  BIC <- c(ans34r$BIC, ans34.1r$BIC, ans34.2r$BIC)
  
  AB <- cbind(AIC, BIC);AB
  

  
  
  