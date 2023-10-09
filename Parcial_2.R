
#Parcial Eduard Patiño


#//Punto 1//

P_defectuosa <- 0.2
P_buen_estado <- 0.8
P_defectuosa_inspeccion <- 0.8
P_buen_estado_inspeccion <- 0.3

p_Inspección_completa <- P_defectuosa_inspeccion * P_defectuosa + P_buen_estado_inspeccion * P_buen_estado

prob_punto_1 <- (P_defectuosa_inspeccion * P_defectuosa) / p_Inspección_completa
prob_punto_1


#//Punto 2//

n <- 20
p <- 0.3

prob_punto_2 <- 1-pbinom(7, size=n, prob= p, lower.tail = TRUE)
prob_punto_2


#//Punto 3//

λ <- 6
prob_3 <- ppois(2,λ)
prob_punto_3 <- 1 - prob_3
prob_punto_3


#//Punto 4//

ingenieros <- 20
ingenieros_selec <- 10

prob_punto_4 <- (choose(3, 3) * choose(17, 7)) / choose(ingenieros, ingenieros_selec)
prob_punto_4


#//Punto 5//

Años <- c(3:13)
px <- c(0.03, 0.05, 0.07, 0.1, 0.14, 0.2, 0.18, 0.12, 0.07, 0.03, 0.01)

Media_5 <- sum(Años * px)

Varianza_5 <- sum((Años - Media_5)^2 * px)

Desviacion_5 <- sqrt(Varianza_5)

limite_superior <- Media_5 + 2 * Desviacion_5
limite_inferior <- Media_5 - 2 * Desviacion_5

#Gráfico de barras
barplot(px, names.arg = Años, xlab = "Años", ylab = "Probabilidad", main = "Distribución de probabilidad")
#En el gráfico se aprecia que la prob_punto_5 sería 0.96


#//Punto 6//

prob_punto_6 <- 1-exp(-(200)^2)-(1-exp(-100^2))
prob_punto_6


#//Punto 7//

f <- function(x) {
  return(x*(3/8)*(7-x)^2)}

prob_punto_7 <- integrate(f, lower = 5, upper = 7)$value
prob_punto_7


#//Punto 8//

Media_8 <- 78
Desviacion_8 <- 6
Percentil_8 <- 0.9

prob_punto_8 <- qnorm(Percentil_8, Media_8, Desviacion_8)
prob_punto_8


#//Punto 9//

f_2 <- function(r) {
  r^2 * (1 - (10 - r)^2)
}
result <- (3 * pi / 4) * integrate(f_2, 9, 11)$value
prob_punto_9 <- result/2  #Se divide en 2 ya que debe terminarse usando el valor esperado para el área del semicírculo que define la función de densidad.
prob_punto_9


#//Punto 10//

sintoma_A <- 0.1
sintoma_B <- 0.3 + sintoma_A

prob_punto_10 <- sintoma_A/sintoma_B
prob_punto_10
