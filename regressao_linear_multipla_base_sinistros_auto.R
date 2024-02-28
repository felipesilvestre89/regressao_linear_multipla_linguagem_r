# Instale e carregue a biblioteca necessária
install.packages("data.table")
install.packages("corrgram")
library(data.table)
library(corrgram)

# Defina as variáveis
veiculos <- c("Gol", "Onix", "HB20", "Prisma", "Ka", "Fox", "Creta", "Corolla", "Civic", "Jeep Renegade")
marcas <- c("Volkswagen", "Chevrolet", "Hyundai", "Ford", "Toyota", "Honda", "Jeep")
categorias <- c("Hatch", "Sedan", "SUV", "Picape", "Crossover", "Perua", "Minivan", "Esportivo", "Furgão")
sexos <- c("Masculino", "Feminino")
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

# Gere o dataset
set.seed(123) # Defina a semente para reproduzibilidade
num_rows <- 10000

dataset <- data.table(
  veiculo = sample(veiculos, num_rows, replace = TRUE),
  ano = sample(2000:2024, num_rows, replace = TRUE),
  marca = sample(marcas, num_rows, replace = TRUE),
  categoria = sample(categorias, num_rows, replace = TRUE),
  sexo_condutor = sample(sexos, num_rows, replace = TRUE),
  idade_condutor = sample(18:65, num_rows, replace = TRUE),
  estado = sample(estados, num_rows, replace = TRUE)
)

# Defina a função para calcular o valor do sinistro com base na idade
calcular_valor_sinistro <- function(idade) {
  if (idade <= 25) {
    return(runif(1, min = 300, max = 300000))
  } else if (idade <= 40) {
    return(runif(1, min = 300, max = 200000))
  } else if (idade <= 50) {
    return(runif(1, min = 300, max = 100000))
  } else {
    return(runif(1, min = 300, max = 50000))
  }
}

# Aplique a função para calcular o valor do sinistro
dataset[, valor_sinistro := sapply(idade_condutor, calcular_valor_sinistro)]

# Salve o dataset em um arquivo CSV
write.csv(dataset, "dataset_seguros_auto.csv", row.names = FALSE)

dados <- read.csv("C:/Users/Felipe/OneDrive/Área de Trabalho/dataset_seguros_auto.csv")

# Matriz de correlação 
cor(dados[, c(2, 6, 8)])

# Gráficos para analisar a matriz de correlação:
corrgram(dados[, c(2, 6, 8)], order = TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, mais ="Carros")

