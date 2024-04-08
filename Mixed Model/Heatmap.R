
random <- ans15.2r_f$U

g14 <- vector()
for (i in 1:23) {
  print(i)
  g14[i] <- random[["14:Genotipo"]][["Peso"]][[i]]
} 

interaction_ghi <- cbind(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14)
row.names(interaction_ghi) <- names(random[["1:Genotipo"]][["Peso"]])
colnames(interaction_ghi) <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14" )
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
           legend.mar = 5, legend.only = TRUE)


random <- ans15.2r_f$U

gen <- vector()
for (i in 1:23) {
  print(i)
  gen[i] <- random[["Genotipo"]][["Peso"]][[i]]
} 

var_cov <- gen%*%t(gen)
row.names(var_cov) <- names(random[["1:Genotipo"]][["Peso"]])
colnames(var_cov) <- names(random[["1:Genotipo"]][["Peso"]])
View(var_cov)
log_var_cov <- log10(var_cov)
View(log_var_cov)

## Heatmap

library(fields)

# Define the color gradient using the specified hex colors
colors <- colorRampPalette(c("#003350", "#cc662f"))(100)

# Invert the columns' positions
var_cov_inverted <- var_cov[, ncol(var_cov):1]

# Create the heatmap
heatmap(var_cov_inverted,
        Colv = NA,
        Rowv = NA,
        scale = "column",
        col = colors,
        # labCol = NA,  # To remove column labels if not needed
        main = "Covariance Matrix")  # Set the title

# Add a color legend
image.plot(zlim = range(interaction_ghi), col = colors,
           legend.lab = "Color Scale",
           legend.mar = 5, legend.only = TRUE)

# Criar uma matriz de exemplo
matriz_exemplo <- matrix(1:9, nrow = 3, ncol = 3)

# Aplicar o logaritmo a todas as células da matriz
matriz_log <- apply(matriz_exemplo, c(1, 2), log)

# Exibir a matriz original e a matriz com logaritmo aplicado
print("Matriz Original:")
print(matriz_exemplo)
print("Matriz com Logaritmo Aplicado:")
print(matriz_log)


# Criar uma matriz de exemplo
matriz_exemplo <- matrix(1:9, nrow = 3, ncol = 3)

# Obter o número de linhas e colunas da matriz
n_linhas <- nrow(matriz_exemplo)
n_colunas <- ncol(matriz_exemplo)

# Criar uma matriz vazia para armazenar o triângulo inferior
triangulo_inferior <- matrix(NA, nrow = 23, ncol = 23)

# Preencher o triângulo inferior com os valores da matriz original
for (i in 1:23) {
  for (j in 1:23) {
    if (i <= j) {
      triangulo_inferior[i, j] <- var_cov[i, j]
    }
  }
}

# Exibir a matriz original e o triângulo inferior
print("Matriz Original:")
print(matriz_exemplo)
print("Triângulo Inferior:")
print(triangulo_inferior)
view(triangulo_inferior)
row.names(triangulo_inferior) <- names(random[["1:Genotipo"]][["Peso"]])
colnames(triangulo_inferior) <- names(random[["1:Genotipo"]][["Peso"]])

# Define the color gradient using the specified hex colors
colors <- colorRampPalette(c("#003350", "#cc662f"))(100)

# Invert the columns' positions
triangulo_inferior_inverted <- triangulo_inferior[, ncol(var_cov):1]
View(log_var_cov_inverted)

# Create the heatmap
heatmap(triangulo_inferior_inverted,
        Colv = NA,
        Rowv = NA,
        scale = "row",
        col = colors,
        # labCol = NA,  # To remove column labels if not needed
        main = "Covariance Matrix")  # Set the title

# Add a color legend
image.plot(zlim = range(var_cov), col = colors,
           legend.lab = "Color Scale",
           legend.mar = 5, legend.only = TRUE)



log_var_cov <- log10(abs(triangulo_inferior))
View(log_var_cov)

log_var_cov.1 <- log10(abs(var_cov))
View(log_var_cov)

## Heatmap

library(fields)

# Define the color gradient using the specified hex colors
colors <- colorRampPalette(c("#003350", "#cc662f"))(100)

# Invert the columns' positions
log_var_cov_inverted <- log_var_cov[, ncol(var_cov):1]
View(log_var_cov_inverted)

# Create the heatmap
heatmap(log_var_cov_inverted,
        Colv = NA,
        Rowv = NA,
        scale = "row",
        col = colors,
        # labCol = NA,  # To remove column labels if not needed
        main = "Covariance Matrix")  # Set the title

# Add a color legend
image.plot(zlim = range(log_var_cov.1), col = colors,
           legend.lab = "Color Scale",
           legend.mar = 5, legend.only = TRUE)
