library(jsonlite)
library(dplyr)
library(ggplot2)
library(tibble)

path_pub_artes <- "~/Downloads/Filosofia/Filosofia.publication.json"
path_orient_fil <- "~/Downloads/Filosofia/Filosofia.advise.json"
path_artes <- "~/Downloads/ArtesCen/ArtesCen.profile.json"

pub_artes <- fromJSON(path_pub_artes)
orient_fil <- fromJSON(path_orient_fil)
perfis_cenica <- fromJSON(path_artes)


# Publicações Artes Cênicas

# Estrutura dos dados
summary(pub_artes)
summary(pub_artes$PERIODICO)

# Contagem de publicações/ano
## Juntando todo tipo de publicação em um dataframe
total_periodico <- data.frame()
for (i in 1:length(pub_artes$PERIODICO)){
  total_periodico <- rbind(total_periodico, as.data.frame(pub_artes$PERIODICO[[i]]))
}

total_livro <- data.frame()
for (i in 1:length(pub_artes$LIVRO)){
  total_livro<- rbind(total_livro, as.data.frame(pub_artes$LIVRO[[i]]))
}

total_cap <- data.frame()
for (i in 1:length(pub_artes$CAPITULO_DE_LIVRO)){
  total_cap <- rbind(total_cap, as.data.frame(pub_artes$CAPITULO_DE_LIVRO[[i]]))
}

total_text <- data.frame()
for (i in 1:length(pub_artes$TEXTO_EM_JORNAIS)){
  total_text <- rbind(total_text, as.data.frame(pub_artes$TEXTO_EM_JORNAIS[[i]]))
}

total_event <- data.frame()
for (i in 1:length(pub_artes$EVENTO)){
  total_event <- rbind(total_event, as.data.frame(pub_artes$EVENTO[[i]]))
}

total_art <- data.frame()
for (i in 1:length(pub_artes$ARTIGO_ACEITO)){
  total_art <- rbind(total_art, as.data.frame(pub_artes$ARTIGO_ACEITO[[i]]))
}

total_demais <- data.frame()
for (i in 1:length(pub_artes$DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA)){
  total_demais <- rbind(total_demais, as.data.frame(pub_artes$DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA[[i]]))
}

total_pub <- data.frame(nrow(total_art), nrow(total_cap), nrow(total_demais), nrow(total_event), 
                        nrow(total_livro), nrow(total_periodico), nrow(total_text))
colnames(total_pub) <- c("Artigo","Capítulos", "Demais", "Eventos", "Livro", "Periódico", "Texto")
total_pub <- data.frame(t(total_pub))
colnames(total_pub) <- "Total"
total_pub <- rownames_to_column(total_pub, "Tipo")
total_pub$Total <- total_pub$Total/sum(total_pub$Total)

# Pie tipo/quantidade de publicações
pie_type_pubs <- ggplot(total_pub, aes(x="", y=Total, fill=Tipo)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_y_continuous(labels = scales::percent) +
  labs(y="Tipos de publicações")

pais_eventos <- select(total_event, pais_do_evento)
num_pais_eventos <- as.data.frame(table(pais_eventos))

pais_livros <- select(total_livro, pais_de_publicacao)
num_pais_livros <- as.data.frame(table(pais_livros))

natureza_text <- select(total_text, natureza)
num_natureza_text <- as.data.frame(table(natureza_text))
num_natureza_text$Freq <- num_natureza_text$Freq/sum(num_natureza_text$Freq)

natureza_demais <- select(total_demais, natureza)
num_natureza_demais <- as.data.frame(table(natureza_demais))
num_natureza_demais$Freq <- num_natureza_demais$Freq/sum(num_natureza_demais$Freq)

pie_natureza_text <- ggplot(num_natureza_text, aes(x="", y=Freq, fill=natureza_text)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_y_continuous(labels = scales::percent) +
  labs(y="Natureza das publicações de texto")

pie_natureza_demais <- ggplot(num_natureza_demais, aes(x="", y=Freq, fill=natureza_demais)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_y_continuous(labels = scales::percent) +
  labs(y="Natureza das demais publicações")

periodicos <- select(total_periodico, periodico, ano)
periodicos_2010 <- filter(periodicos, ano == 2010)
periodicos_2011 <- filter(periodicos, ano == 2011)
periodicos_2012 <- filter(periodicos, ano == 2012)
periodicos_2013 <- filter(periodicos, ano == 2013)
periodicos_2014 <- filter(periodicos, ano == 2014)
periodicos_2015 <- filter(periodicos, ano == 2015)
periodicos_2016 <- filter(periodicos, ano == 2016)
periodicos_2017 <- filter(periodicos, ano == 2017)

periodicos_2010 <- as.data.frame(table(select(periodicos_2010, periodico)))
periodicos_2011 <- as.data.frame(table(select(periodicos_2011, periodico)))
periodicos_2012 <- as.data.frame(table(select(periodicos_2012, periodico)))
periodicos_2013 <- as.data.frame(table(select(periodicos_2013, periodico)))
periodicos_2014 <- as.data.frame(table(select(periodicos_2014, periodico)))
periodicos_2015 <- as.data.frame(table(select(periodicos_2015, periodico)))
periodicos_2016 <- as.data.frame(table(select(periodicos_2016, periodico)))
periodicos_2017 <- as.data.frame(table(select(periodicos_2017, periodico)))

total_2010 <- nrow(pub_artes[[1]][[1]]) + nrow(pub_artes[[2]][[1]]) + nrow(pub_artes[[3]][[1]]) + 
  nrow(pub_artes[[5]][[1]]) + nrow(pub_artes[[6]][[1]]) + 
  nrow(pub_artes[[7]][[1]])

total_2011 <- nrow(pub_artes[[1]][[2]]) + nrow(pub_artes[[2]][[2]]) + nrow(pub_artes[[3]][[2]]) + 
  nrow(pub_artes[[4]][[2]]) + nrow(pub_artes[[5]][[2]]) + 0 + 
  nrow(pub_artes[[7]][[2]])

total_2012 <- nrow(pub_artes[[1]][[3]]) + nrow(pub_artes[[2]][[3]]) + nrow(pub_artes[[3]][[3]]) + 
  nrow(pub_artes[[4]][[3]]) + nrow(pub_artes[[5]][[3]]) + 0 + 
  nrow(pub_artes[[7]][[3]])

total_2013 <- nrow(pub_artes[[1]][[4]]) + nrow(pub_artes[[2]][[4]]) + nrow(pub_artes[[3]][[4]]) + 
  nrow(pub_artes[[4]][[4]]) + nrow(pub_artes[[5]][[4]]) + nrow(pub_artes[[6]][[4]]) + 
  nrow(pub_artes[[7]][[4]])

total_2014 <- nrow(pub_artes[[1]][[5]]) + nrow(pub_artes[[2]][[5]]) + nrow(pub_artes[[3]][[5]]) + 
  nrow(pub_artes[[4]][[5]]) + nrow(pub_artes[[5]][[5]]) + nrow(pub_artes[[6]][[5]]) + 
  nrow(pub_artes[[7]][[5]])

total_2015 <- nrow(pub_artes[[1]][[6]]) + nrow(pub_artes[[2]][[6]]) + nrow(pub_artes[[3]][[6]]) + 
  nrow(pub_artes[[4]][[6]]) + nrow(pub_artes[[5]][[6]]) + nrow(pub_artes[[6]][[6]]) + 
  0

total_2016 <- nrow(pub_artes[[1]][[7]]) + nrow(pub_artes[[2]][[7]]) + nrow(pub_artes[[3]][[7]]) + 
  nrow(pub_artes[[4]][[7]]) + nrow(pub_artes[[5]][[7]]) + 0

total_2017 <- nrow(pub_artes[[1]][[8]]) + nrow(pub_artes[[2]][[8]]) + nrow(pub_artes[[3]][[8]]) + 
  0 + nrow(pub_artes[[5]][[8]]) + nrow(pub_artes[[6]][[8]]) + 
  0

total_pub_year<- data.frame(total_2010,total_2011,total_2012,total_2013,total_2014,total_2015,total_2016,total_2017)
colnames(total_pub_year)<- c("2010","2011","2012","2013","2014","2015","2016","2017")

total_pub_year <- data.frame(t(total_pub_year))
colnames(total_pub_year) <- "Total"
total_pub_year <- rownames_to_column(total_pub_year, "Ano")

bar_pub_year <- ggplot(total_pub_year, aes(x=Ano, y=Total)) + 
  geom_bar(stat = "identity", fill = "#388E8E") +
  labs(title = "Frequência de publicações por ano", x = "Ano de publicação", y = "Número de publicações")

unique_periodicos <- as.data.frame(unique(periodicos$periodico))
