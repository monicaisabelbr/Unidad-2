generar_poblacion <- function(n, p) {
  rbinom(n, 1, p)
}

#x = generar_poblacion(123,1000,0.3)

tomar_muestra <- function(poblacion, n_muestra) {
  sample(poblacion, n_muestra, replace=FALSE)
}

#y = tomar_muestra(x, 100)

calcular_proporcion <- function(muestra) {
  sum(muestra) / length(muestra)
}

#calcular_proporcion(y)
 #table(x)
#prop.table(table(x))

rbinom(1000,1,0.5)