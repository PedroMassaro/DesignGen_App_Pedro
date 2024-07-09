ans1r$U$`Corte:Genotipo`$Peso[["Corte1:GenotipoARUA"]]
ansr1$U$`Corte:Genotipo`$Peso[[1]]
ans1r$U$`Corte:Genotipo`$Peso

genotipo.01 <- as.vector(ans1r$data$Genotipo[1:23])
genotipo <- rep(genotipo.01, each = 14)
genotipo <- as.factor(genotipo)
genotipo 

corte <- rep(1:14, times = 23)
corte <- as.factor(corte)
corte

blup <- vector()
for (i in 1:322) {
  blup[i] <- ans1r$U$`Corte:Genotipo`$Peso[[i]]
}
blup <- as.numeric(blup)
blup

matriz <- cbind(genotipo, corte, blup)

# Converta a matriz em um data frame
dados_blup <- as.data.frame(matriz)
View(dados_blup)

dados_blup$genotipo <- as.factor(dados_blup$genotipo)
dados_blup$corte <- as.factor(dados_blup$corte)
str(dados_blup)

# Load necessary library
library(ggplot2)


# Plot the data
ggplot(dados_blup, aes(x = corte, y = blup, group = genotipo, color = genotipo)) +
  geom_line() +
  geom_point() +
  labs(title = "Peso by Corte and Genotipo",
       x = "Corte",
       y = "Peso") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(dplyr)

# Selecionando linhas onde o nível do fator genotipo é "G1"

genotipo.01 <- as.vector(ans1r$data$Genotipo[1:23])

library(dplyr)


plot <- list()
for (i in 1:23) {
  plot[[i]] <- ggplot(dados_blup %>% filter(genotipo == i),
                    aes(x = corte, y = blup, group = genotipo)) +
    geom_line(color = "#cc662f") +
    geom_point(color = "#cc662f") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", se = FALSE, color = "#003350", linewidth = 0.5) +
    labs(title = print(genotipo.01[i]),
         x = "Corte",
         y = "BLUP") +
    ylim(-800, 800) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


library(gridExtra)
grid.arrange(plot[[1]], plot[[2]], plot[[3]], plot[[4]], plot[[5]],
             plot[[6]], plot[[7]], plot[[8]], plot[[9]], plot[[10]], 
             plot[[11]], plot[[12]], plot[[13]], plot[[14]], plot[[15]], 
             plot[[16]], plot[[17]], plot[[18]], plot[[19]], plot[[20]],
             plot[[21]], plot[[22]], plot[[23]], nrow= 5)

#-------------------------------------------------------------------------------

str(dados_blup)

df <- list()
res <- list()
for (j in 1:23) {
  df[[j]] <- dados_blup %>% filter(genotipo == j)
  amp <- max(df[[j]]$blup) - min(df[[j]]$blup)
  med <- mean(df[[j]]$blup)
  n <- vector()
  for (i in 1:14) {
    n[i] <- ((df[[j]]$blup[i] - med)/amp)
  }
  res[[j]] <- n
  print(j)
}

r_blup <- cbind(c(res[[1]], res[[2]], res[[3]], res[[4]], res[[5]], res[[6]], res[[7]],
                  res[[8]], res[[9]], res[[10]], res[[11]], res[[12]], res[[13]], res[[14]],
                  res[[15]], res[[16]], res[[17]], res[[18]], res[[19]], res[[20]], res[[21]],
                  res[[22]], res[[23]]))

dados_blup$r_blup <- r_blup

plot <- list()
for (i in 1:23) {
  plot[[i]] <- ggplot(dados_blup %>% filter(genotipo == i),
                      aes(x = corte, y = r_blup, group = genotipo)) +
    geom_line(color = "#cc662f") +
    geom_point(color = "#cc662f") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", se = FALSE, color = "#003350", linewidth = 0.5) +
    labs(title = print(genotipo.01[i]),
         x = "Corte",
         y = "BLUP") +
    ylim(-1, 1) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


library(gridExtra)
grid.arrange(plot[[1]], plot[[2]], plot[[3]], plot[[4]], plot[[5]],
             plot[[6]], plot[[7]], plot[[8]], plot[[9]], plot[[10]], 
             plot[[11]], plot[[12]], plot[[13]], plot[[14]], plot[[15]], 
             plot[[16]], plot[[17]], plot[[18]], plot[[19]], plot[[20]],
             plot[[21]], plot[[22]], plot[[23]], nrow= 5)


library(dplyr)
library(ggplot2)

plot <- list()
for (i in 1:23) {
  plot[[i]] <- ggplot(dados_blup %>% 
                        filter(genotipo == i, corte != 1, corte != 8),
                      aes(x = corte, y = blup, group = genotipo)) +
    geom_line(color = "#cc662f") +
    geom_point(color = "#cc662f") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", se = FALSE, color = "#003350", linewidth = 0.5) +
    labs(title = print(genotipo.01[i]),
         x = "Corte",
         y = "BLUP") +
    ylim(-800, 800) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


library(gridExtra)
grid.arrange(plot[[1]], plot[[2]], plot[[3]], plot[[4]], plot[[5]],
             plot[[6]], plot[[7]], plot[[8]], plot[[9]], plot[[10]], 
             plot[[11]], plot[[12]], plot[[13]], plot[[14]], plot[[15]], 
             plot[[16]], plot[[17]], plot[[18]], plot[[19]], plot[[20]],
             plot[[21]], plot[[22]], plot[[23]], nrow= 5)



