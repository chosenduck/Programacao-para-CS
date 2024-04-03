#Limpando todos os objetos atualmente ativos no Ambiente (Environment) =========
rm(list=ls(all=T))

#Pacotes========================================================================
if(!require(skimr)){install.packages("skimr")};library(skimr)
if(!require(rio)){install.packages("rio")};library(rio)
if(!require(tidyverse)){install.packages("tidyverse")};library(tidyverse)
if(!require(janitor)){install.packages("janitor")};library(janitor)

#Abrindo base ==================================================================
# abrindo arquivo
votacao_mg_18 <- read.csv("votacao_mg_2018_edit.csv") 

# descobrindo nomes das variaveis
names(votacao_mg_18)

# descobrindo tipos das variaveis
glimpse(votacao_mg_18)

# selecionando variáveis necessárias para a avaliação
mg18 <- votacao_mg_18 %>%
  select(partido = SG_PARTIDO, candidato = NM_URNA_CANDIDATO,
         votos = QT_VOTOS_NOMINAIS, cargos = DS_CARGO, municipio = NM_MUNICIPIO,
         codigo_municipio = CD_MUNICIPIO, resultado = DS_SIT_TOT_TURNO)

# 1) descobrir os 5 candidatos a deputado federal mais votados, e eleitos, =====
# nos 10 municípios mais populosos de MG nas eleições de 2018.

## 10 municípios mais populosos de MG
# Belo Horizonte: 2.315.560; Uberlândia: 713.232; Contagem: 621.865; Juiz de Fora: 540.756;
# Montes Claros: 414.240; Betim: 411.859; Uberaba:  337.846; Ribeirão das Neves: 329.794;
# Governador Valadares: 257.172; Divinópolis:  231.091

# procurando municipios
lista_municipios <- mg18 %>% 
  select(municipio, codigo_municipio) %>% 
  distinct(municipio, .keep_all = TRUE) %>% 
  arrange(municipio)

# filtrando base: 10 municipios mais populosos
top_municipios <- mg18 %>% 
  filter(codigo_municipio %in% c("41238","41335", "54038", "43710","47333", "48658",
                                 "54011","50911", "45535","44458"))

# filtrando base: apenas deputados federais
dep_fed <- top_municipios %>% 
  filter(cargos == "Deputado Federal")

# filtrando base: apenas eleitos
unique(dep_fed$resultado) #verificando condições de resultado para recorte atual

eleitos <- dep_fed %>% 
  filter(resultado %in% c("ELEITO POR QP","ELEITO POR M\xc9DIA"))

# obtendo o total de votos por município
top_eleitos <- eleitos %>%
  group_by(municipio) %>%
  mutate(total_votos = sum(votos)) 

# obtendo o total de votos por candidato
top_eleitos <- top_eleitos %>%
  group_by(candidato, municipio) %>%
  mutate(total_candidato = sum(votos))

#obtendo a porcentagem de votos para cada candidato por municipio
top_eleitos <- top_eleitos %>% 
  group_by(candidato, municipio) %>% 
  summarise(percent = total_candidato/total_votos) %>% 
  distinct(candidato, municipio, percent) %>% 
  print(n = 200)

# organizando o resultado para obter os 5 deputados eleitos mais votados nos 
# 10 municipais mais populosos de MG
top_eleitos %>% 
  arrange(municipio, -percent) %>%
  group_by(municipio) %>% 
  slice(1:5) %>% # top 5
  print(n=200) 

# 2) descobrir os 5 candidatos a deputado federal mais votados, ================
# e não eleitos, nos 10 municípios mais populosos de MG nas eleições de 2018.

# filtrando base: apenas nao eleitos
unique(dep_fed$resultado) #verificando condições de resultado para recorte atual

nao_eleitos <- dep_fed %>% 
  filter(resultado %in% c("SUPLENTE","N\xc3O ELEITO"))

# obtendo o total de votos por município
top_nao_eleitos <- nao_eleitos %>%
  group_by(municipio) %>%
  mutate(total_votos = sum(votos)) 

# obtendo o total de votos por candidato
top_nao_eleitos <- top_nao_eleitos %>%
  group_by(candidato, municipio) %>%
  mutate(total_candidato = sum(votos))

#obtendo a porcentagem de votos para cada candidato por municipio
top_nao_eleitos <- top_nao_eleitos %>% 
  group_by(candidato, municipio) %>% 
  summarise(percent = total_candidato/total_votos) %>% 
  distinct(candidato, municipio, percent) %>% 
  print(n = 200)

# organizando o resultado para obter os 5 deputados nao eleitos mais votados nos 
# 10 municipais mais populosos de MG
top_nao_eleitos %>% 
  arrange(municipio, -percent) %>%
  group_by(municipio) %>% 
  slice(1:5) %>% # top 5
  print(n=200)
