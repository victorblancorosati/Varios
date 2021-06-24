# https://www.rpubs.com/jboscomendoza/analisis_sentimientos_lexico_afinn


library(tidyverse)
library(tidytext)
#install.packages('tm')
library(tm)
library(lubridate)
library(zoo)
library(scales)

tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/tuits_candidatos.csv",
              "tuits_candidatos.csv")

# COnvierte en tabla
tuits <- read.csv("tuits_candidatos.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  as_tibble()
#tuits

# diccionario lexico afinn
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  as_tibble()
afinn


tuits <- 
  tuits %>%
  separate(created_at, into = c("Fecha", "Hora"), sep = " ") %>% # separa la primera columna en 2 columnas
  separate(Fecha, into = c("Dia", "Mes", "Periodo"), sep = "/",  
           remove = FALSE) %>%                                   # separa la columna fecha en 3 columnas
  mutate(Fecha = dmy(Fecha),                    # COnvierte la fecha   
         Semana = week(Fecha) %>% as.factor(),  # Contabiliza la semana
         text = tolower(text)) %>%              # Minúsculas
  filter(Periodo == 2018)                       # seleccio solo el 2018
tuits


tuits_afinn <- 
  tuits %>%
  unnest_tokens(input = "text", output = "Palabra") %>%    # divide el texto en palabras
  inner_join(afinn, ., by = "Palabra") %>%                 # cruza con el diccionario de palabras 
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>%     # Asigna la valoración
  rename("Candidato" = screen_name)                                     # renombra una columna


tuits <-
  tuits_afinn %>%
  group_by(status_id) %>%                            # Agrupa por las frases
  summarise(Puntuacion_tuit = mean(Puntuacion)) %>%  # promedia las puntuaciones  
  left_join(tuits, ., by = "status_id") %>%          # une tuits originales con la base de tuits_afinn
  mutate(Puntuacion_tuit = ifelse(is.na(Puntuacion_tuit), 0, Puntuacion_tuit)) %>%   # asigna 0  cuando no tiene puntuación
  rename("Candidato" = screen_name)                  # renombra por candidatos

# Estadisticas

# Total n_tuits por candidatos
tuits_afinn %>%     
  count(Candidato)

# Palabras por candidatos
tuits_afinn %>%         
  group_by(Candidato) %>% 
  distinct(Palabra) %>% 
  count()

# QUitando la palara no
tuits_afinn <-
  tuits_afinn %>%
  filter(Palabra != "no") 

# Calculo de fechas
tuits_afinn_fecha <-
  tuits_afinn %>%                           # archivo sin NO
  group_by(status_id) %>%                   # agrupado por frase
  mutate(Suma = mean(Puntuacion)) %>%       # promedio de puntuación
  group_by(Candidato, Fecha) %>%            # agrupado por candidato y fecha
  summarise(Media = mean(Puntuacion))       # promedio de puntuación
