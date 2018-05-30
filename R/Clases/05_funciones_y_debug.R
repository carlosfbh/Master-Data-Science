# Ejemplo de funciones y debuguear

# NOTA: En este fichero hay un error, y hay que buscarlo

# Para ello debuguea las funciones
# La forma más fácil es añadir un breakpoint (punto de ruptura)
# en la línea en la que te quieras detener

# Para añadir este breakpoint tienes que pulsar a la izquierda
# del número de fila (en la zona gris izquierda donde tienes)
# el código fuente.
# Cuando ya salga un punto rojo. Tienes que hacer un source
# del fichero y ese punto rojo se rellenará
# Ahora ya puedes ejecutar la función con invertir2(...)
# y ver que pasa línea a línea

invertir <- function (a,b) {
  sum(a[a>b])
}

invertir2 <- function (appl,googl) {
  importe <- 0
  # Se ha cambiado la longitud del for para que recorra la longitud del vector y no 100.
  # Se le ha añadido llaves al bucle for ya que no los tenía y no entraba al if
  # Se ha cambiado el nombre de appl dentro del if ya que tenía otro y no coincidía
  # con el puesto en la entrada de la función.
  
  for (i in 1:length(appl)) {
    if (appl[i] < googl[i])
      importe <- importe + googl[i]
  }
  importe
}

# Ejemplos de vectores
a <- c(10, 8, 7)
b <- c(10, 9, 6)

invertir2(a, b)
invertir(a, b)

