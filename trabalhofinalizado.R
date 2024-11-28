
setwd("C:/Users/migue/Documents/trabalho estatistica 2 etapa")

dados <- read.table("dados2.txt", header = TRUE, sep = ",", dec = ".")
str(dados)

#Funções
pearson=function(x){
  me=mean(x)
  m=median(x)
  sd=sd(x)
  pearson=3*(me-m)/sd
  names(pearson)="pearson"
  pearson
}

yule=function(x){
  q1 = quantile(x,0.25, type=5)
  q2 = quantile(x,0.5 , type=5)
  q3 = quantile(x,0.75 , type=5)
  yule = (q3+q1-2 *q2)/(q3-q1)
  names(yule)="yule"
  yule
}

kelley=function(x){
  q1 = quantile(x,0.1, type=5)
  q2 = quantile(x,0.5 , type=5)
  q3 = quantile(x,0.9 , type=5)
  kelley = (q3+q1-2*q2)/(q3-q1)
  names(kelley)="kelley"
  kelley
}

curt=function(x){
  q10 = quantile(x,0.1, type=5)
  q25 = quantile(x,0.25 , type=5)
  q75 = quantile(x,0.75 , type=5)
  q90 = quantile(x,0.9 , type=5)
  curt = (q75-q25)/(2*(q90-q10))
  names(curt)="curt"
  curt
}

minimo_liberdade = min(dados$liberdade_monetaria, na.rm = TRUE)
minimo_inflacao = min(dados$inflacao, na.rm = TRUE)

max_liberdade = max(dados$liberdade_monetaria, na.rm = TRUE)
max_inflacao = max(dados$inflacao, na.rm = TRUE)

#medianas 
med_liberdade = median(dados$liberdade_monetaria, na.rm = TRUE)
med_inflacao = median(dados$inflacao, na.rm = TRUE)
View(med_liberdade)
View(med_inflacao)

#media artmetica
mean_liberdade =  mean(dados$liberdade_monetaria, na.rm = TRUE)
mean_inflacao =  mean(dados$inflacao, na.rm = TRUE)
View(mean_liberdade)
View(mean_inflacao)

#desvio padrao
desviopadrao_liberdade = sd(dados$liberdade_monetaria, na.rm = TRUE)
desviopadrao_inflacao = sd(dados$inflacao, na.rm = TRUE)
View(desviopadrao_liberdade)
View(desviopadrao_inflacao)

# calculando os quartil
quartil1_liberdade = quantile(dados$liberdade_monetaria, probs = 0.25, na.rm = TRUE)
quartil1_inflacao = quantile(dados$inflacao, probs = 0.25, na.rm = TRUE)

quartil3_liberdade = quantile(dados$liberdade_monetaria, probs = 0.75, na.rm = TRUE)
quartil3_inflacao = quantile(dados$inflacao, probs = 0.75, na.rm = TRUE)

desvio_interquartil_liberdade = quartil3_liberdade - quartil1_liberdade
desvio_interquartil_inflacao = quartil3_inflacao - quartil1_inflacao

assimetriapearson_liberdade = pearson(dados$liberdade_monetaria)
assimetriapearson_inflacao = pearson(dados$inflacao)
View(assimetriapearson_liberdade)
View(assimetriapearson_inflacao)

assimetriayule_liberdade = yule(dados$liberdade_monetaria)
assimetriayule_inflacao = yule(dados$inflacao)

assimetriakelley_liberdade = kelley(dados$liberdade_monetaria)
assimetriakelley_inflacao = kelley(dados$inflacao)

curtose_liberdade = curt(dados$liberdade_monetaria)
curtose_inflacao = curt(dados$inflacao)

# escores Z
escoresz_liberdade = (dados$liberdade_monetaria - mean_liberdade) / desviopadrao_liberdade
escoresz_inflacao = (dados$liberdade_monetaria - mean_inflacao) / desviopadrao_inflacao

# Gera histogramas
histograma_liberdade = hist(dados$liberdade_monetaria, breaks = 7, 
                            main = "Histograma Liberdade Monetária", 
                            xlab = "Liberdade Monetária", 
                            ylab = "Frequência", 
                            col = "cyan", border = "black")

histograma_inflacao = hist(dados$inflacao, breaks = 16, 
                           main = "Histograma Índice Inflacionário", 
                           xlab = "Índice Inflacionário", 
                           ylab = "Frequência", 
                           col = "red", border = "black")

# Gera boxplots 
boxplot_liberdade_monetaria = boxplot(dados$liberdade_monetaria, 
                                      main = "Boxplot da Liberdade Monetária", 
                                      ylab = "Liberdade Monetária", 
                                      col = "cyan")

boxplot_inflacao = boxplot(dados$inflacao, 
                           main = "Boxplot da Inflação", 
                           ylab = "Índice de Inflação", 
                           col = "cyan")




# Criando o data.frame com as variáveis calculadas
resultados <- data.frame(
  Variável = c("Liberdade Monetária", "Inflação"),
  Mínimo = c(minimo_liberdade, minimo_inflacao),
  Máximo = c(max_liberdade, max_inflacao),
  Mediana = c(med_liberdade, med_inflacao),
  Média = c(mean_liberdade, mean_inflacao),
  Desvio_Padrão = c(desviopadrao_liberdade, desviopadrao_inflacao),
  Quartil_1 = c(quartil1_liberdade, quartil1_inflacao),
  Quartil_3 = c(quartil3_liberdade, quartil3_inflacao),
  Desvio_Interquartil = c(desvio_interquartil_liberdade, desvio_interquartil_inflacao),
  Assimetria_Pearson = c(assimetriapearson_liberdade, assimetriapearson_inflacao),
  Assimetria_Yule = c(assimetriayule_liberdade, assimetriayule_inflacao),
  Assimetria_Kelley = c(assimetriakelley_liberdade, assimetriakelley_inflacao),
  Curtose = c(curtose_liberdade, curtose_inflacao)
)

# Exibindo o data.frame
View(resultados)
print(resultados)

calcula_escores_z <- function(dados) {
  
  colunas_numericas <- sapply(dados, is.numeric)
  dados_numericos <- dados[, colunas_numericas]
  
  # Calcula os escores Z
  escores_z <- as.data.frame(scale(dados_numericos, center = TRUE, scale = TRUE))
  
  
  if ("pais" %in% names(dados)) {
    escores_z$pais <- dados$pais
    row.names(escores_z) <- escores_z$pais 
    escores_z$pais <- NULL                 
  }
  
  return(escores_z)
}


escores_z_todos <- calcula_escores_z(dados)


write.csv(escores_z_todos, "escores_z_todos.csv", row.names = TRUE)

# Visualizando os resultados
View(escores_z_todos)
print(escores_z_todos)

