library(jsonlite)
library(dplyr)
library(ggplot2)

# Files paths

path_bdtd <- "~/Downloads/unb.BDTD.json"
path_pub <- "~/Downloads/unb.Pub.json"
path_orientacoes <- "~/Downloads/unb.Orientacao.json"
path_perfis <- "~/Downloads/unb.Perfis.json"

# Read Json

bdtd_dados <- fromJSON(path_bdtd)
pub_dados <- fromJSON(path_pub)
orientacao_dados <- fromJSON(path_orientacoes)
perfis_dados <- fromJSON(path_perfis)

# Turn into data frames

## BDTD
df_bdtd <- as.data.frame(bdtd_dados$response$docs)

## Orientações
orientacoes_andamento <- data.frame()
for(i in 1:5)
  for(j in 1:length(orientacao_dados[[i]]))
    orientacoes_andamento <- rbind(orientacoes_andamento, as.data.frame(orientacao_dados[[i]][[j]]))

orientacoes_concluidas <- data.frame()
for(i in 6:8)
  for(j in 1:length(orientacao_dados[[i]]))
    orientacoes_concluidas <- rbind(orientacoes_concluidas, as.data.frame(orientacao_dados[[i]][[j]]))

## Get publications by year of bdtd

pub_year <- select(df_bdtd, publishDate) 
pub_year <- as.data.frame(lapply(pub_year, unlist), stringsAsFactors = FALSE)

pub_year <- as.data.frame(table(pub_year), stringsAsFactors = FALSE)
colnames(pub_year) <- c("year", "freq")
pub_year$year <- as.numeric(pub_year$year)
filtered_pub <- filter(pub_year, year > 2000)

plot_year_pub <- ggplot(filtered_pub, aes(x = year, y = freq)) +
                  geom_bar(stat = "identity", fill = "#388E8E") +
                  scale_x_continuous(breaks = filtered_pub$year) +
                  labs(title = "Frequência de publicações BDTD", x = "Ano de publicação", y = "Número de publicações")
                  

## Get type of theses on bdtd dataset

format_bdtd <- select(df_bdtd, format)
format_bdtd <- as.data.frame(lapply(format_bdtd, unlist))
format_bdtd <- as.data.frame(table(format_bdtd))
colnames(format_bdtd) <- c("Tipo", "Freq")
format_bdtd$Freq <- format_bdtd$Freq/sum(format_bdtd$Freq)

pie_format <- ggplot(format_bdtd, aes(x="", y=Freq, fill=Tipo)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_y_continuous(labels = scales::percent) +
  labs(y="Tipos de teses")


# Orientações dataset

## Number of orientations by year

### Em andamento
years_andamento <- select(orientacoes_andamento, ano)
years_andamento <- as.data.frame(lapply(years_andamento, unlist))
years_andamento <- as.data.frame(table(years_andamento), stringsAsFactors = FALSE)
colnames(years_andamento) <- c("Ano", "Frequencia")
years_andamento$Ano <- as.numeric(years_andamento$Ano)

bar_orient_andamento <- ggplot(years_andamento, aes(x = Ano, y = Frequencia)) + 
                          geom_bar(stat="identity", fill = "#3F88C5") +
                          scale_x_continuous(breaks = years_andamento$Ano) +
                          labs(title = "Frequência de orientações em andamento", 
                               x = "Ano de orientação", y = "Número de orientações")

### Concluída

years_concluida <- select(orientacoes_concluidas, ano)
years_concluida <- as.data.frame(lapply(years_concluida, unlist))
years_concluida <- as.data.frame(table(years_concluida), stringsAsFactors = FALSE)
colnames(years_concluida) <- c("Ano", "Frequencia")
years_concluida$Ano <- as.numeric(years_concluida$Ano)

bar_orient_concluida <- ggplot(years_concluida, aes(x = Ano, y = Frequencia)) + 
                          geom_bar(stat="identity", fill = "#44BBA4") +
                          scale_x_continuous(breaks = years_andamento$Ano) +
                          labs(title = "Frequência de orientações em andamento", 
                               x = "Ano de orientação", y = "Número de orientações")

### Total
years_orientacoes <- left_join(years_andamento, years_concluida, by = "Ano")
colnames(years_orientacoes) <- c("Ano", "Em.andamento", "Concluida")
years_orientacoes <- mutate(years_orientacoes, total = Em.andamento + Concluida)

## Pie dos tipos do estado das orientações

Tipos <- c("Em.andamento", "Concluida")
soma <- c(sum(years_orientacoes$Em.andamento), sum(years_orientacoes$Concluida))
orientacoes_tipo_year <- data.frame(Tipos, soma)
orientacoes_tipo_year$soma <- orientacoes_tipo_year$soma / sum(orientacoes_tipo_year$soma)

pie_orientacoes <- ggplot(orientacoes_tipo_year, aes(x="", y=soma, fill=Tipos)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x="",y="Estado das orientações")

## Pie bolsas das orientações

bolsas_orient <- select(orientacoes_andamento, bolsa)
bolsas_orient <- rbind(bolsas_orient, select(orientacoes_concluidas, bolsa))
bolsas_orient <- as.data.frame(table(bolsas_orient))
colnames(bolsas_orient) <- c("Disponibilizam", "Freq")
bolsas_orient$Freq <- bolsas_orient$Freq/sum(bolsas_orient$Freq)

pie_bolsas_orient <- ggplot(bolsas_orient, aes(x="", y=Freq, fill=Disponibilizam)) + 
                      geom_bar(width = 1, stat = "identity") + 
                      coord_polar("y", start=0) + 
                      scale_y_continuous(labels = scales::percent) +
                      labs(x="",y="Orientações que disponibilizam bolsas")