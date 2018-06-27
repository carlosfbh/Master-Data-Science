##### EJERCICIOS SCRAPING rvest ######

require(tidyverse)
require(rvest)
require(ggpubr)

##########################################################################
# 1. Ejercicio: 
# Descargar el ultimo discurso del rey de España desde la siguiente dirección:
# http://www.casareal.es/ES/Actividades/Paginas/actividades_discursos_detalle.aspx?data=5738
##########################################################################

discurso <- read_html("http://www.casareal.es/ES/Actividades/Paginas/actividades_discursos_detalle.aspx?data=5738")

frases <- discurso %>% html_nodes("p") %>% 
  html_text(trim = TRUE) %>% 
  paste(collapse = " ")


##########################################################################
# 2. Ejercicio: 
# Descargar las cotizaciones del IBEX 35 en tiempo real desde la siguiente página:
# http://www.bolsamadrid.es/esp/aspx/Mercados/Precios.aspx?indice=ESI100000000
##########################################################################

Ibex35 <- read_html("http://www.bolsamadrid.es/esp/aspx/Mercados/Precios.aspx?indice=ESI100000000")

tablas <- Ibex35 %>% html_nodes("table") %>% html_table(header = TRUE, fill = TRUE)
lapply(tablas, colnames)
lapply(tablas, dim) # Con esto veo que el contenido que quiero está en el elemento 5 de la lista

tablas <- Ibex35 %>% 
  html_nodes("table") %>% 
  .[[5]] %>% 
  html_table(header = TRUE, fill = TRUE, dec = ",") # Los decimales están puestos con ,

ggplot(tablas) + 
  aes(x = reorder(Nombre, Últ.), y = Últ.) + 
  geom_bar(stat = "identity") + 
  coord_flip()
  

##########################################################################
# 3. Ejercicio: 
# Crea una función que tome como data la url de un artista de la web Filmaffinity
# "https://www.filmaffinity.com"
# La función tiene que devolver un Data Frame con los siguientes datos de las películas
# del artista:
# Director, Título, Año, Duración, Reparto, Valoración y Nº de Votos
# Además la función tendrá que devolver 3 gráficos:
# 1. Relación ordenada de películas según la valoración.
# 2. Relación ordenada de películas según el Nº de Votos.
# 3. Relación ordenada de películas según criterio de valoración conjunta
# entre Nº de votos y valoración
##########################################################################

filmaffinity <- function(enlace, raiz = "https://www.filmaffinity.com") {
  
  artista_url <- read_html(enlace)
  # Con esto obtengo las href de las películas relacionadas con el artista
  pelis_href <- artista_url %>% html_nodes("div.mc-title a") %>% html_attr("href")
  
  # Inicializo el Data Frame
  peliculas <- structure(list(Director = character(), 
                              Titulo = character(),
                              Year = character(),
                              Duracion = character(),
                              Reparto = character(),
                              Rating = character(),
                              Votos = character()), 
                         class = "data.frame")
  
  # En este bucle recorro las url de todas las películas para sacar la información con los nodos
  for (i in 1:length(pelis_href)) {
    
    enlace_pelicula <- paste(raiz, pelis_href[i], sep = "")
    pelicula_url <- read_html(enlace_pelicula)
    
    # Obtengo las variables.
    # En el caso de director y reparto lo que se obtiene es una lista de varios elementos
    # Transformo esa lista para que sea de un único elemento. De ahí el paste y collapse
    
    titulo <- pelicula_url %>% html_nodes("title") %>% html_text(trim = TRUE)
    year <- pelicula_url %>% html_nodes("[itemprop=datePublished]") %>% html_text(trim = TRUE)
    duracion <- pelicula_url %>% html_nodes("[itemprop=duration]") %>% html_text(trim = TRUE)
    director <- pelicula_url %>% html_nodes("[itemprop=director]") %>% html_text(trim = TRUE)
    director <- paste(director, sep = " ", collapse = " ") 
    actores <- pelicula_url %>% html_nodes("dd span.cast a") %>% html_text(trim = TRUE)
    actores <- paste(actores, sep = " ", collapse = ",")
    rating <- pelicula_url %>% html_nodes("[itemprop=ratingValue]") %>% html_text(trim = TRUE)
    votos <- pelicula_url %>% html_nodes("[itemprop=ratingCount]") %>% html_text(trim = TRUE)
    
    
    # En el caso de que una de las variables no obtenga información de la web me crea una
    # variable character(0). Me da problemas para crear el DF, así que en ese caso esa
    # variable la transformo en un NA. No sé como hacerlo diferente.
    
    if (length(titulo) == 0) {
      titulo = NA
    }
    if (length(year) == 0) {
      year = NA
    }
    if (length(duracion) == 0) {
      duracion = NA
    }
    if (length(director) == 0) {
      director = NA
    }
    if (length(actores) == 0) {
      actores = NA
    }
    if (length(rating) == 0) {
      rating = NA
    }
    if (length(votos) == 0) {
      votos = NA
    }
    
    # Completo el DF con los datos obtenidos
    pelicula <- data.frame(Director = director, 
                           Titulo = titulo, 
                           Year = year, 
                           Duracion = duracion, 
                           Reparto = actores,
                           Rating = rating,
                           Votos = votos)
    peliculas <- rbind(peliculas, pelicula)
  }
  
  # Ahora realizo los gráficos. Para ello tengo que transformar las variables Rating y Nº de Votos
  # en variables numéricas.
  # Además voy a crear una nueva columna que sea el criterio para realizar el 3 gráfico
  peliculas <- peliculas %>% 
    mutate(Rating = as.numeric(sub(",", ".", Rating, fixed = TRUE)), 
           Votos = as.numeric(sub(".", "", Votos, fixed = TRUE)),
           Valoracion = (Rating * Votos) / sum(Votos, na.rm = TRUE))
    
  # Gráfico 1. Relación Pelis - Rating
  grafico_uno <- ggplot(peliculas %>% filter(!is.na(Rating))) +
    aes(x = reorder(Titulo, Rating), y = Rating) +
    geom_bar(stat = "identity", fill = "steelblue4", width = 0.5) +
    scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
    ggtitle("Valoración de los usuarios") +
    labs(y = "Rating", x = "Películas") +
    coord_flip() +
    theme_bw() +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10, face = "bold"),
          plot.title = element_text(size = 12, face = "bold"))
  
  # Gráfico 2. Relación Pelis - Nº Votos
  grafico_dos <- ggplot(peliculas %>% filter(!is.na(Votos))) +
    aes(x = reorder(Titulo, Votos), y = Votos) +
    geom_bar(stat = "identity", fill = "steelblue4", width = 0.5) +
    scale_y_continuous(breaks = seq(0, 300000, 5000), labels = ) +
    ggtitle("Nº de Votos") +
    labs(y = "Votos", x = "Películas") +
    coord_flip() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10, face = "bold"),
          plot.title = element_text(size = 12, face = "bold"))
  
  # Gráfico 3. Relación Pelis - Valoración
  grafico_tres <- ggplot(peliculas %>% filter(!is.na(Valoracion))) +
    aes(x = reorder(Titulo, Valoracion), y = Valoracion) +
    geom_bar(stat = "identity", fill = "steelblue4", width = 0.5) +
    scale_y_continuous(breaks = seq(0, 5, 0.2)) +
    ggtitle("Relación Rating con Nº de Votos") +
    labs(y = "Score", x = "Películas") +
    coord_flip() +
    theme_bw() +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10, face = "bold"),
          plot.title = element_text(size = 12, face = "bold"))
  
  result <- list(peliculas, grafico_uno, grafico_dos, grafico_tres)
  return(result)
}

