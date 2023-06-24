###### vetores #######

#numeric
preco <- c(16.92, 24.03, 7.61, 15.49, 11.77)

mean(preco)

#numeric
custo <- c(8.37, 12.93, 26.2, 12.2, 10.12)

#character
produto <- c('A', 'A','B', 'C', 'C')

table(produto)

#factor
produto <- factor(produto)

#Boolean
Obteve_lucro <- c(TRUE, TRUE, FALSE, TRUE, TRUE)


#Date

datas <- as.Date(c('2019-07-01','2019-07-02', '2019-07-03', '2019-07-04','2019-07-005'))

lucro <- preco - custo
#comparando  dois vetores
preco > custo

####### Matrizes #######

matrix <- cbind(preco, custo)

matriz2 <- cbind(preco, produto)

###### Data Frame #######

data_frame <- data.frame(Coluna1 = preco, Coluna2 = produto)

data_frame2 <- data.frame(preco_praticado = preco, preco_custo = custo, produto_vendido = produto, receita_positiva = Obteve_lucro)

View(data_frame2)

summary(data_frame2$preco_custo)

##### Lista #######
lista <- list(Objeto1 = preco,
              Objeto2 = data_frame2)
lista$Objeto1
lista$Objeto2
lista$Objeto2$preco_custo


#recapitulando
#Os principais tipos de variáveis que o R trabalha são: Numeric, character, Boolean, Date, Factor
# Os principais tipos de objetos que o R trabalha são: Vetor, Matriz,  Data Frame,  Lista
# para importar documentos clicar na tabela lado direito superior
#e cola o código no arquivo
library(readxl)
dataset <- read_excel(NULL)
View(dataset)
head("nome da tabela")#primeiras cinco linhas
