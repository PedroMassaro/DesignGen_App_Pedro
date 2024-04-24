####AGBT congress analysis####

library(sommer)

vignette("v1.sommer.quick.start")
vignette("v2.sommer.changes.and.faqs")
vignette("v3.sommer.qg")
vignette("v4.sommer.gxe")

#------------------------------------------------------------------------------#  
####Teste com os dados - Visualização de saídas ####

library(readr)

# dados_fil1 <- read_csv("dados_fil.csv")

dados_fil1 <- read_csv("dados_fil.csv", col_types = cols(peso = col_character()))
dados_fil1$block <- as.factor(dados_fil1$block)
dados_fil1$gen <- as.factor(dados_fil1$gen)
dados_fil1$local <- as.factor(dados_fil1$local)
# dados_fil1$peso <- as.numeric(gsub("\\.", "", dados_fil1$peso))

write.csv(dados_fil1, file = "dados1.csv", sep = ",", row.names = FALSE)

library(tidyverse)
library(dplyr)

# Apenas o melhor corte escolhido (6) para todos os ambientes
data_fil <- dados_fil1 %>%
  filter(corte %in% "6" & local %in% c("AC","MS","RO","RJ","DF")) %>%
  droplevels()

str(data_fil)
write.table(data_fil, file = "dados.txt", sep = ",", row.names = FALSE)
write.csv(data_fil, file = "dados.csv", sep = ",", row.names = FALSE)

# FIltrando o gen PM38 e TANZ, pois alteravam os comp. de var
data_fil_f <- data_fil %>%
  filter(!(gen %in% c("PM38", "TANZ") & local == "RO")) %>%
  droplevels()

data_fil_f$block <- as.factor(data_fil_f$block)
data_fil_f$gen <- as.factor(data_fil_f$gen)
data_fil_f$local <- as.factor(data_fil_f$local)   
data_fil_f$peso <- as.numeric(data_fil_f$peso)

str(data_fil_f)
write.table(data_fil_f, file = "dados_f.txt", sep = ",", row.names = FALSE)
write.csv(data_fil_f, file = "dados_f.csv", sep = ",", row.names = FALSE)

#------------------------------------------------------------------------------#  
####Teste do melhor modelo - Visualização de saídas ####

## 1 - Univariate homogeneous variance model
ans1r <- mmer(peso ~ local + local:block,
              random= ~ local:gen,
              rcov= ~ units,
              data=data_fil_f)
summary(ans1r)$varcomp
summary(ans1r)
ans1r$U$`local:gen`$peso

## 2 - Univariate heterogeneous variance model

ans2r <- mmer(peso ~ local + local:block,
              random= ~ vsr(usr(local),gen),
              rcov= ~ units,
              data=data_fil_f)
summary(ans2r)$varcomp
summary(ans2r)
ans2r$U$`AC:gen`$peso
ans2r$U$`DF:gen`$peso
ans2r$U$`MS:gen`$peso
ans2r$U$`RJ:gen`$peso
ans2r$U$`RO:gen`$peso
## AIC & BIC ##
AIC <- c(ans1r$AIC, ans2r$AIC)
BIC <- c(ans1r$BIC, ans2r$BIC)

AB <- cbind(AIC, BIC); AB 

# Interaction plots ----
# GEI

# Load required packages
library(ggplot2)
library(gghighlight)

# Grouping and summarizing
GEI <- data_fil_f %>%
  group_by(local, gen) %>%
  summarise(peso = mean(peso, na.rm = TRUE), .groups = "drop")

# Ordering
index <- order(GEI$peso, decreasing = FALSE)
GEI <- GEI[index,]

# Plot using ggplot2
p_E <- GEI %>%
  ggplot(aes(local, peso)) +
  geom_line(linewidth = 1.2, aes(group = gen, color = gen, alpha = ifelse(gen %in% c('MASS', 'PM34', 'PM38', 'PM45'), 1, 0.7))) +
  labs(title = "Interaction between genotype and environment",
       x = "Location",
       y = "Leaf dry matter") +
  guides(alpha = "none") +
  theme_bw() +
  scale_color_manual(values = c("#003350", "#ff6441", "#cc662f", "#2F95CC"),
                     limits = c('PM40', 'PM34', 'PM35', 'PM45'),
                     breaks = c('PM40', 'PM34', 'PM35', "PM45"))

# Plot using gghighlight
GEI %>%
  ggplot(aes(local, peso)) +
  geom_line(linewidth = 1.0, aes(group = gen, color = gen)) +
  gghighlight(gen %in% c('PM40', 'PM34', 'PM35', 'PM45'), use_direct_label = FALSE) +
  labs(x = "Location",
       y = "Leaf dry matter") +
  guides(color = guide_legend(title = "Genotype", ncol = 1)) +
  theme_bw() +
  scale_color_manual(values = c("#003350", "#ff6441", "#cc662f", "#2F95CC"),
                     limits = c('PM40', 'PM34', 'PM35', 'PM45'),
                     breaks = c('PM40', 'PM34', 'PM35', "PM45"))


## 3 - Unstructured variance models

# # 3.1) vsr(gen)
# ans3r <- mmer(peso ~ local + block,
#               random=~ vsr(gen) + local:block + vsr(usr(local),gen),
#               rcov=~ units,
#               data=data_fil_f)
# summary(ans3r)$varcomp
# summary(ans3r)
# ans3r$U$gen
# 
# # 3.2) just gen
# ans3.1r <- mmer(peso ~ local + block,
#                 random=~ gen+ local:block + vsr(usr(local),gen),
#                 rcov=~ units,
#                 data=data_fil_f)
# summary(ans3.1r)$varcomp
# summary(ans3.1r)
# ans3.1r$U$gen