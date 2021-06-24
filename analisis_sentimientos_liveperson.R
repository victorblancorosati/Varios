#############################################################################
#############################################################################
#
#                      ANALISIS DE SENTIMIENTOS LIVEPERSON
#
#############################################################################
# Version: V1
# Fecha: 13/10/2020
# Autor: Victor Blanco
#############################################################################
#############################################################################

rm(list=ls(all=TRUE))

#install.packages('tm')

library(tidyverse)
library(tidytext)
library(tm)
library(lubridate)
library(zoo)
library(scales)
library(readr)


# Obtención del archivo
# se debe desacrgar el archivo al incio de cada mes desde liveperson.
# descomprimir en la carpeta conversaciones


setwd("C:/1 Satisfacción del Cliente/analisis_liveperson/conversaciones")
mes<- month(Sys.Date())
archivo_actual<- list.files()
k =length(archivo_actual)


for (i in 2:k) {
  conversaciones <- read_csv(archivo_actual[[1]], locale = locale())%>%as_tibble()
  conversacionesi <- read_csv(archivo_actual[[i]], locale = locale())%>%as_tibble()
  conversaciones <-rbind(conversaciones, conversacionesi)
}

prueba <-conversaciones%>%
   group_by(timeDate)%>%
   summarise(n())
# 
# prueba <-conversaciones%>%
# #  group_by(sentBy)%>%
#   group_by(timeDate)%>%
#   summarise(n())


abandonos <-conversaciones%>%
  filter( str_detect(text, pattern = "No hemos sabido nada de usted por un tiempo"))%>%
  summarise(n())

# 

setwd("C:/1 Satisfacción del Cliente/analisis_liveperson")

# diccionario lexico afinn
# download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
#               "lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  as_tibble()
afinn


aconversaciones <- 
  conversaciones %>%
  filter(sentBy == 'Consumer', !is.na(text))%>%
  select(conversationId, timeDate, timeYear, timeMonth,text)%>%
  mutate(Fecha = dmy(timeDate),                    # COnvierte la fecha   
         text = tolower(stringi::stri_trans_general(text,"latin-ascii")))                 # Minúsculas
aconversaciones

# prueba= aconversaciones%>%
#   group_by(Fecha)%>%
#   summarise(n())


conversaciones_afinn <- 
  aconversaciones %>%
  unnest_tokens(input = "text", output = "Palabra") %>%    # divide el texto en palabras
  inner_join(afinn, ., by = "Palabra") %>%                 # cruza con el diccionario de palabras 
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa"))     # Asigna la valoración
conversaciones_afinn

conversaciones1 <-
  conversaciones_afinn %>%
  group_by(conversationId) %>%                            # Agrupa por las frases
  summarise(Puntuacion_chat = mean(Puntuacion),
            Tipo = ifelse(Puntuacion_chat > 0, "Positiva", "Negativa"),
            Palabra = first(Palabra),
            Word = first(Word)) %>%  # promedia las puntuaciones  
  left_join(aconversaciones, ., by = "conversationId") %>%          # une tuits originales con la base de tuits_afinn
  mutate(Puntuacion_chat = ifelse(is.na(Puntuacion_chat), 0, Puntuacion_chat))   # asigna 0  cuando no tiene puntuación


# prueba= conversaciones1%>%
#   group_by(Fecha)%>%
#   summarise(n())

conversaciones2 <-
  conversaciones1%>%
  #select(conversationId, text, Puntuacion_chat, Tipo)%>%
  group_by(conversationId) %>%
  summarise(
    texto = paste(text, collapse = " "),
    puntuacion = mean(Puntuacion_chat),
    Tipo = ifelse(puntuacion > 0, "Positiva", "Negativa"),
    Palabra = first(Palabra),
    Word = first(Word),
    fecha = first(Fecha))
conversaciones2


conversaciones3<-conversaciones2%>%
     mutate(producto1 =
           case_when(
             str_detect(texto,"banca en linea") == TRUE ~ "banca en linea",
             str_detect(texto,"tarjeta de credito") == TRUE ~ "tarjeta",
             str_detect(texto,"tarjeta de cr") == TRUE ~ "tarjeta",
             str_detect(texto,"asistencia finan") == TRUE ~ "asistencia financiera",
             str_detect(texto,"cajero") == TRUE ~ "cajero",
             str_detect(texto,"compra de saldo") == TRUE ~ "compra de saldo",
             str_detect(texto,"cuenta de ahorro") == TRUE ~ "cuenta de ahorro",
             str_detect(texto,"estado de cuenta") == TRUE ~ "estado de cuenta",
             str_detect(texto,"financiamento") == TRUE ~ "financiamiento",
             str_detect(texto,"pagina") == TRUE ~ "pagina",
             str_detect(texto,"prestamo") == TRUE ~ "prestamo",
             str_detect(texto,"sucursal") == TRUE ~ "sucursal",
             str_detect(texto,"tasa") == TRUE ~ "tasa",
             str_detect(texto,"transacciones") == TRUE ~ "transacciones",
             str_detect(texto,"covi") == TRUE ~ "covid_19",
             str_detect(texto,"token") == TRUE ~ "token",
             str_detect(texto,"seguro de ") == TRUE ~ "seguro de",
             str_detect(texto,"cobrando") == TRUE ~ "cobro",
             str_detect(texto,"anualidad") == TRUE ~ "anualidad",
             str_detect(texto,"transferencia") == TRUE ~ "transferencia",
             str_detect(texto,"pago de") == TRUE ~ "pago",
             str_detect(texto,"saldo de") == TRUE ~ "saldo",
             str_detect(texto,"usuario") == TRUE ~ "usuario",
             str_detect(texto,"no he recibido") == TRUE ~ "no he recibido",
             TRUE ~ "no aplica"))

# prueba= conversaciones3%>%
#     group_by(fecha)%>%
#     summarise(n())

conversaciones4<- conversaciones3%>%
  select(fecha, conversationId, texto, puntuacion, producto1)

prueba= conversaciones4%>%
       group_by(producto1)%>%
       summarise(n())
  
# GUARDAR EL ARCHIVO 

write.csv(conversaciones4, "liveperson_mes.csv")
write.csv(conversaciones4, "B:/BI/BI/SATISFACCION DE CLIENTE/liveperson_13102020/liveperson_mes.csv")

#############################################################################
## INVESTIGACION DE PALABRAS CLAVES
#############################################################################
# #
# bigramas <- conversaciones2 %>%
#    unnest_tokens(bigram, text2, token = "ngrams", n = 3)
# 
# palabras_claves<-bigramas%>%
#    count(bigram, sort = TRUE)%>%
#   mutate(producto1 = 
#            case_when(
#       str_detect(bigram,"banca en linea") == TRUE ~ "banca en linea",
#       str_detect(bigram,"tarjeta de credito") == TRUE ~ "tarjeta",
#       str_detect(bigram,"asistencia finan") == TRUE ~ "asistencia financiera",
#       str_detect(bigram,"cajero") == TRUE ~ "cajero",
#       str_detect(bigram,"compra de saldo") == TRUE ~ "compra de saldo",
#       str_detect(bigram,"cuenta de ahorro") == TRUE ~ "cuenta de ahorro",
#       str_detect(bigram,"estado de cuenta") == TRUE ~ "estado de cuenta",
#       str_detect(bigram,"financiamento") == TRUE ~ "financiamiento",
#       str_detect(bigram,"pagina") == TRUE ~ "pagina",
#       str_detect(bigram,"prestamo") == TRUE ~ "prestamo",
#       str_detect(bigram,"sucursal") == TRUE ~ "sucursal",
#       str_detect(bigram,"tasa") == TRUE ~ "tasa",
#       str_detect(bigram,"transacciones") == TRUE ~ "transacciones",
#       str_detect(bigram,"covi") == TRUE ~ "covid_19",
#       str_detect(bigram,"token") == TRUE ~ "token",
#       TRUE ~ "no aplica"))
# palabras_claves
# 
# bigramas<-bigramas%>%
#   inner_join(palabras_claves, by = "bigram")
# 
# bigramas_resumen<- bigramas%>%
#   group_by(conversationId)%>%
#   summarise(producto1 = first(producto1))
#   
# 
# conversaciones3<-conversaciones2%>%
#   inner_join(bigramas_resumen, by = "conversationId")
# 
# 
# names(bigramas_resumen)  
# names(conversaciones2)
#

# ############################################################
# ############################################################
# 
# # # Historico para el cambio de estructura de la tabla
# 
# ############################################################
# ############################################################
# 
# 
# library(readr)
# datahistorica <- read_delim("C:/1 Satisfacción del Cliente/analisis_liveperson/liveperson_afinn.csv", 
#                                "~", escape_double = FALSE, trim_ws = TRUE)
# 
# names(datahistorica)
# 
# datahistorica1<- datahistorica%>%
#   select(fecha, cliente3, cliente, Puntuacion)
# 
# colnames(datahistorica1) <- c("fecha","conversationId", "texto", "puntuacion")
# 
# 
# datahistorica2<-datahistorica1%>%
#   mutate(producto1 =
#            case_when(
#              str_detect(texto,"banca en linea") == TRUE ~ "banca en linea",
#              str_detect(texto,"tarjeta de credito") == TRUE ~ "tarjeta",
#              str_detect(texto,"tarjeta de cr") == TRUE ~ "tarjeta",
#              str_detect(texto,"asistencia finan") == TRUE ~ "asistencia financiera",
#              str_detect(texto,"cajero") == TRUE ~ "cajero",
#              str_detect(texto,"compra de saldo") == TRUE ~ "compra de saldo",
#              str_detect(texto,"cuenta de ahorro") == TRUE ~ "cuenta de ahorro",
#              str_detect(texto,"estado de cuenta") == TRUE ~ "estado de cuenta",
#              str_detect(texto,"financiamento") == TRUE ~ "financiamiento",
#              str_detect(texto,"pagina") == TRUE ~ "pagina",
#              str_detect(texto,"prestamo") == TRUE ~ "prestamo",
#              str_detect(texto,"sucursal") == TRUE ~ "sucursal",
#              str_detect(texto,"tasa") == TRUE ~ "tasa",
#              str_detect(texto,"transacciones") == TRUE ~ "transacciones",
#              str_detect(texto,"covi") == TRUE ~ "covid_19",
#              str_detect(texto,"token") == TRUE ~ "token",
#              str_detect(texto,"seguro de ") == TRUE ~ "seguro de",
#              str_detect(texto,"cobrando") == TRUE ~ "cobro",
#              str_detect(texto,"anualidad") == TRUE ~ "anualidad",
#              str_detect(texto,"transferencia") == TRUE ~ "transferencia",
#              str_detect(texto,"pago de") == TRUE ~ "pago",
#              str_detect(texto,"saldo de") == TRUE ~ "saldo",
#              str_detect(texto,"usuario") == TRUE ~ "usuario",
#              str_detect(texto,"no he recibido") == TRUE ~ "no he recibido",
#              TRUE ~ "no aplica"))
# 
#  # datahistorica2%>%
#  #   group_by(producto1)%>%
#  #   summarise(n())
# 
# write.csv(datahistorica2, "livepersonhistoria.csv")
