#-------------------------------------------------------------------
# Tratamentos iniciais dos dados

dados <- read.table(file = "pinguim.txt", header = TRUE, sep = ",")

corrompido <- which(dados$sex == ".")
dados <- dados[-corrompido, ]

faltantes <- which(is.na(dados$sex))
dados <- dados[-faltantes, ]

dados$sex <- as.factor(dados$sex)
dados$species <- as.factor(dados$species)
dados$island <- as.factor(dados$island)

summary(dados)

#-------------------------------------------------------------------
# Separando treinamento e testes
amostra <- sample(x = 1:nrow(dados), size = nrow(dados), replace = FALSE)
dados <- dados[amostra, ]

tamanho <- round(nrow(dados) * 0.75) 
treinamento <- dados[1:tamanho, ]
teste <- dados[(tamanho+1):nrow(dados), ]

par(mfrow = c(2,2))

barplot(table(dados$species))
barplot(table(treinamento$species))
barplot(table(teste$species))

#-------------------------------------------------------------------
# Verificando relações dos dados

ggplot(data = treinamento, mapping = aes(x = island, fill = species)) + geom_bar()
#Com isso vemos que apenas o Adelie está em torgersen

ggplot(data = treinamento, mapping = aes(x = culmen_length_mm, y = flipper_length_mm, color = species)) + geom_point()
#Posso usar flipper length > 205 = gentoo
ggplot(data = treinamento, mapping = aes(x = culmen_length_mm, y = culmen_depth_mm, color = species)) + geom_point()
#Posso usar culmen depth > 45 = chinstrap

#-------------------------------------------------------------------
#Árvore de decisão
respostas <- c()


teste[1, 1:6]

for (j in 1:nrow(teste)) {
  if(teste$island[j] == "Torgersen"){
    respostas[j] <- "Adelie" 
  } else if(teste$island[j] == "Dream"){
    if(teste$culmen_length_mm[j] > 45){
      respostas[j] <- "Chinstrap"
    } else{
      respostas[j] <- "Adelie"
    }
  } else {
    if(teste$flipper_length_mm[j] > 205){
      respostas[j] <- "Gentoo"
    } else{
      respostas[j] <- "Adelie"
    }
  }
}

mean(respostas == teste$species)

#-------------------------------------------------------------------

