# ----- Importanto data frame baixado -----------------------------------------

# session -> set working directory -> choose Directory (Escolher pasta onde está o dataframe)

#Primeira linha DESTE arquivo se trata de titulos de cada coluna, HEADER = true
dataBase <- read.table(file = "pinguim.txt", header = TRUE, sep = ",")
#Colunas separadas por "," SEP = ","

str(dataBase) #verifcar como os dados estão no data frame

# -----------------------------------------------------------------------------


# ----- Analise inicial dos dados do data frame -------------------------------

#Contar quantos pinguins de cada especie tem
tabela <- table(dataBase$species)
barplot(tabela)

#Verificando especie está apenas como string, não como uma categoria de fator
summary(dataBase)
# -----------------------------------------------------------------------------


# ----- Trabalhando a coluna dos sexos ----------------------------------------

#Verificando quantidades de masculino e feminino
barplot(table(dataBase$sex)) #Achamos um "."

corrupts <- which(dataBase$sex == ".") #Retorna qual o pinguim está com o dado corrompido
dataBase <- dataBase[-corrupts, ] #Retira os dados corrpompidos [linha, coluna]
dataBase$sex <- as.factor(dataBase$sex) #Tornando sexo como fator

faltantes <- which(is.na(dataBase$sex)) #Verificando onde está com dados faltando
dataBase <- dataBase[-faltantes, ]

#Após os tratamentos da coluna de sexo, verificamos que agora o conjunto não possui mais nenhum conjuto faltante
# -----------------------------------------------------------------------------


# ----- Transformando especies e ilhas para categorias ------------------------
dataBase$species <- as.factor(dataBase$species)
dataBase$island <- as.factor(dataBase$island)

# ----- Pacote para plotar gráficos mais bonitos ------------------------------
#packet -> install -> ggplot2
#Depois de instalar selecionar para poder usar, ou comando library(ggplot2)

#Grafico de barras
ggplot(data = dataBase, mapping = aes(x = island, fill = species)) + geom_bar() + theme_minimal()

#Grafico de pontos
ggplot(data = dataBase, mapping = aes(x = culmen_length_mm, y = flipper_length_mm, color = species )) + geom_jitter()

#Blox pot
ggplot(data = dataBase, mapping = aes(y = body_mass_g, x = species)) + geom_boxplot()


