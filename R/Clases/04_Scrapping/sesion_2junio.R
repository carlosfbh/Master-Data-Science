## Scrapping

# install.packages(c("rvest", "tidytext"), dep = TRUE)

# Podemos rascar prácticamente todo de la web

require(tidyverse)
require(rvest)
amanece <- read_html("http://www.imdb.com/title/tt0094641/")

amanece %>% html_nodes("title")
h <- amanece %>% html_nodes("p")
amanece %>% html_nodes("table")

amanece %>% html_nodes("title") %>% html_text(trim = TRUE)
amanece %>% html_nodes("p") %>% .[[20]] %>% html_text(trim = TRUE)


discurso <- read_html("http://www.casareal.es/ES/Actividades/Paginas/actividades_discursos_detalle.aspx?data=6035")
frases <- discurso %>% html_nodes("p") %>% html_text() %>% paste(collapse = " ")

amanece %>% html_nodes("table") %>% .[[1]]

html_node(amanece, "table.cast_list") %>% 
  html_table(header=TRUE) %>% 
  .[[2]] # .[[2]] para segunda columna

a <- amanece %>% html_node("table.cast_list") %>% html_table(header = TRUE)

## Ejercicio
# Descargar las cotizaciones del IBEX 35 en tiempo real desde la siguiente pagin
ibex35 <- "http://www.bolsamadrid.es/esp/aspx/Mercados/Precios.aspx?indice=ESI100000000"

tablas <- read_html(ibex35) %>% 
  html_nodes("table") %>% 
  html_table(header = TRUE, fill = TRUE)

lapply(tablas, dim)
lapply(tablas, colnames)

tablas <- read_html(ibex35) %>% 
  html_nodes("table") %>% 
  .[[5]] %>% 
  html_table(header = TRUE, fill = TRUE)


tablas <- read_html(ibex35) %>% 
  html_nodes("table") %>% 
  .[[5]] %>% 
  html_table(header = TRUE, fill = TRUE, dec = ",")

tablas

ggplot(ibex35, aes(x = reorder(Nombre, Últ.), y = Últ.) + geom_bar(stat = "identity") + coord_flip())


# Vínculos
amanece %>% html_nodes("table a") %>% .[[1]] %>% html_attr("href")

enlaces <- html_nodes(amanece, "table a")
actores <- enlaces %>% html_attr("href")
raiz = "http://www.imdb.com"
browseURL(paste(raiz,actores[1],sep="/")) #pagina de José Sazatornil

enlaces2 <- amanece %>% html_nodes("table.cast_list a")
actores2 <- enlaces2 %>% html_attr("href")       
browseURL(paste(raiz, actores2[2], sep="/"))


# Navegación
sesion <- html_session("http://www.imdb.com")

sesion %>% jump_to("boxoffice") %>% session_history()
sesion %>% follow_link(5) %>% html_node("title") %>% html_text()
sesion %>% follow_link(css = "p a")

# Con esto vemos los formularios. En este caso de la web IMDB
html_form(sesion)
busqueda <- html_form(sesion)[[1]] %>% set_values(`q` = "amanece", s = "Titles") 
# con q le estamos diciendo que haga una búsqueda de la palabra amanece
# con s = Titles le estamos diciendo que el criterio es en el títutlo
busqueda %>% submit_form(session=sesion) %>% html_nodes("td.result_text") %>% html_text()


busqueda <- html_form(sesion)[[1]] %>% set_values(`q` = "amanece", s = "Titles") %>% 
  submit_form(session = sesion)


## Analizar texto 

# Empezamos con una función básica de R.
# gsub es capaz de hacer reemplazos en el texto
gsub("^h", "H", c("hola", "búho")) # Cambia las primeras h en H
gsub("o$", "os", c("hola", "búho")) # Cambia las últimas o en os
gsub("\\.", "", c(3.715, 5.698)) # Cambia los puntos de los números y los quita. El punto hay que escaparlo con \\
gsub(".", "", c(56.26, 48.48), fixed = TRUE) # Esto es porque el punto es expresión regular.

# Función grep
# Busca el número de coincidencias de un texto
grep("^h", c("hola", "búho")) # Cuantas palabras hay que empiezan por h

## Ejercicio
# colors() es una función que devuelve el nombre de más de 600 colores en R. Usándolo,
# Encontrar * Aquellos cuyo nombre contenga un número (posiblemente tengas que investigar
# cómo se expresa cualquier número como expresión regular) * Aquellos que comiencen 
# con yellow * Aquellos que contengan blue.


colors()[grep("blue", colors())] # colores que contengan blue
grep("blue", colors(), value = TRUE) # con poner value = TRUE es lo mismo

grep("^yellow", colors(), value = TRUE) # colores que empiezan por yellow

grep("\\d", colors(), value = TRUE) # colores que contienen un número en su nombre
grep("[1234567890]", colors(), value = TRUE) # colores que contienen un número en su nombre
grep("[012]", colors(), value = TRUE) # colores que contienen 0, 1 o 2

# Función paste
# con sep o collapse

paste("A", 1:6, sep = ",") # Pega A a los números y los separa con ,
paste("Hoy es ", date(), " y tengo clase de R", sep = "")

paste("A", 1:6, collapse = ",") # Genera una única cadena y separa los elementos por ,

paste("A", 1:6, sep = "_", collapse = ",")

# Función strsplit.
# Separa cadena de textos
strsplit(c("hoy es martes", "mañana es miércoles"), split = " ")
# Devuelve una lista de cadena de textos



## Ejercicio

# Crea una función que tome los nombres de ficheros
# y genere una tabla con una fila por fichero y tres columnas: el nombre del fichero, 
# la fecha y y la provincia. Nota: puedes crear una función que procese solo un nombre
# de fichero y aplicársela convenientemente al vector de nombres.

ficheros <- c("ventas_20160522_zaragoza.csv", "pedidos_firmes_20160422_soria.csv")
strsplit(ficheros, split="_")


# Funciones stringr o tidyr
require(rvest)
mfield<-read_html("https://es.wikipedia.org/w/index.php?title=Medalla_Fields&oldid=103644843")
mfield %>% html_nodes("table") 

tabla <- mfield %>% html_nodes("table") %>% .[[2]] %>% html_table(header=TRUE)
knitr:::kable(tabla %>% head(10))

require(tidyverse)
tmp <- tabla$Medallistas %>% str_extract("\\([^()]+\\)") #extrae contenido entre parentesis 
tmp <- substring(tmp,2,nchar(tmp)-1)
paises<- tmp %>% str_split_fixed(" y ", 2) %>% str_trim() %>% c()

freq=c(table(paises))[-1] #el -1 es para quitar la frecuencia de ""
qplot(freq,reorder(names(freq),freq),ylab="paises")


## Tokenización

texto<-c("Eso es insultar al lector, es llamarle torpe","Es decirle: ¡fíjate, hombre, fíjate, que aquí hay intención!","Y por eso le recomendaba yo a un señor que escribiese sus artículos todo en bastardilla","Para que el público se diese cuenta de que eran intencionadísimos desde la primera palabra a la última.")
texto

require(tidytext)
require(tidyverse)
texto_df <- data_frame(fila = 1:4, texto = texto)
texto_df

a <- texto_df %>% unnest_tokens(palabra, texto)

b <- texto_df %>% unnest_tokens(palabra, texto, token="ngrams", n=2) # bigramm


require(janeaustenr)
libros <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [[:digit:]ivxlc]", ignore_case=TRUE)))) %>%
  ungroup()

tokens <- libros %>% unnest_tokens(word, text) # word es la nueva variable que creamos
# text, es la variable que estamos tokenizando
str(tokens)

# Con los tokens podemos hacer estadística del texto

tokens <- tokens %>% anti_join(stop_words) # Con esto nos cargamos palabras de poca 
# utilidad. Por ejemplo conjunciones, determinantes, artículos, etc...
# es una primera estrategia para analizar un texto
# stop_words tiene dos variables: "words" y "linenumber"
# stop_words y tokens tienen la columna "words" en común, por lo tanto por defecto 
# anti_join hace el trabajo para estas columnas en común.
# En caso de que no fuera así, habría que cambiar nombre
tokens

freq <- tokens %>% count(word, sort = TRUE) 
# Aquí vemos las palabras que más se repiten

require(ggplot2)
freq %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
# Graficamos por columna


require(wordcloud)
wordcloud(words = freq$word, freq = freq$n, min.freq = 300,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
# Aquí graficamos por nube de palabras

# Ahora vamos a dar peso a las palabras en función de su uso en todas las novelas
# Si una palabra se repite mucho en todas las novelas no tienen importancia

book_words <- austen_books() %>% unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()
# Contamos las palabras más repetidas por novela

freq_rel <- book_words %>% bind_tf_idf(word, book, n)
freq_rel
# Aquí va dar valor a cada palabra según la frecuencia en cada novela
# Nos da tres columnas nuevas
# tf: frecuencia de la palabra en el seno de su novela
# idf: si es igual a cero es que sale en todas las novelas
# tf_idf: es el producto de tf * idf

freq_rel %>% arrange(desc(tf_idf))
# ordenamos y vemos

freq_rel %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

# Vemos que las palabras que más se repiten con este criterio son nombres propios
# Vamos ahora a quitar aquellas palabras que empiecen con mayúscula
# y hacer el mismo análisis

book_words2 <- austen_books() %>% unnest_tokens(word, text, to_lower = FALSE) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

# Quitamos las palabras que empiezan con mayúscula

book_words2 <- book_words2 %>% 
  filter(!str_detect(word, "^[:upper:]"))

freq_rel2 <- book_words2 %>% bind_tf_idf(word, book, n)
freq_rel2 %>% arrange(desc(tf_idf))

freq_rel2 %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()


ggsave("Jane.png", width = 40, height = 20, units = "cm")
