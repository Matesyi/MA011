#	Priklad 1

data <- read.csv (file = "znamky.csv", header = TRUE, sep = ",", dec = ".")
str (data)

#	pocet skupin
r <- nrow (data)

#	absolutni cetnosti
n.j <- data$cetnosti

#	pocet pozorovani
n <- sum (n.j)

#	relativni cetnosti
p.j <- n.j / n
sum (p.j)

# kumulativni absolutni cetnosti
N.j <- cumsum (n.j)

# kumulativni relativni cetnosti
F.j <- N.j / n

#	tabulka 
tabulka <- data.frame (znamka = data$znamka, n.j, p.j, N.j, F.j)
tabulka
# sloupce tabulky jsou pojmenovane, lze k nim pristupovat take pomoci jejich nazvu

# sloupkovy diagram
barplot (tabulka$n.j, names.arg = tabulka$znamka, xlab = "znamka", ylab = "pocet pozorovani", main = "sloupkovy diagram")

#	graf cetnostni funkce
plot (tabulka$znamka, tabulka$n.j, type = "p", pch = 20, cex = 2, col = "red", xlab = "znamka", ylab = "hodnoty cetnostni funkce", main = "cetnostni funkce")

# polygon cetnosti
plot (tabulka$znamka, tabulka$n.j, type="b", pch = 20, xlab = "znamka", ylab = "pocet pozorovani", main = "polygon cetnosti", col = "red", cex = 2, lty = 2)

# graf empiricke distribucni funkce
plot (c(0,tabulka$znamka,5), c(0,tabulka$F.j,1), type="s", xlab = "znamka", ylab = "ECDF", main="empiricka distribucni funkce", col = "red", lwd = 2)

X <- rep (tabulka$znamka, tabulka$n.j)
plot (ecdf (X), col = "red", lwd = 2, xlab = "znamka", ylab = "ECDF", main="empiricka distribucni funkce")

#	histogram 
hist (X, breaks = seq (0.5, 4.5, by = 1), freq = FALSE, col = "yellow", xlab = "znamka", ylab = "cetnostni hustota", main = "histogram", xlim = c (0, 5))

# prumer
prumer <- sum (X) / n
prumer
# v R je pro prumer funkce "mean"
mean (X)

# rozpyl a smerodatna odchylka
rozptyl <- mean (X^2) - prumer^2
rozptyl
smerodatna_odchylka <- sqrt (rozptyl)
smerodatna_odchylka
# v R je pro vyberovy rozptyl funkce "var", ale je potreba jej zkorigovat na rozptyl z prednasky
var (X) * (n-1) / n

# krabicovy diagram (boxplot)
boxplot (X, horizontal = TRUE, ylim = range (X), main = "krabicovy diagram (boxplot)")
# pro nazornost muzeme prikreslit i jednotliva pozorovani
stripchart (X, vertical = FALSE, method = "jitter", pch = 21, col = "red", bg = "yellow", cex = 1.5, add = TRUE)

#	kvantily a krabicovy diagram
# vektor poradi a serazeny vzorek
R <- rank (X)
R
X.sorted <- sort (X)
X.sorted

# median, kvartily, kvartilova odchylka
c.25 <- 0.25 * n
c.50 <- 0.50 * n
c.75 <- 0.75 * n
c (c.25, c.50, c.75)

# kvantily
x.25 <- (X.sorted[c.25] + X.sorted[c.25 + 1]) / 2
x.50 <- (X.sorted[c.50] + X.sorted[c.50 + 1]) / 2
x.75 <- (X.sorted[c.75] + X.sorted[c.75 + 1]) / 2

# spocitejte navic x.33
c.33 <- 0.33 * n
x.33 <- X.sorted[ceiling(c.33)] 

c (x.25, x.50, x.75, x.33)

# kvartilova odchylka (IQR)
q <- x.75 - x.25
q

# hradby boxplotu
c (x.25, x.75) + c (-1, 1) * 1.5 * q
c (x.25, x.75) + c (-1, 1) * 3 * q

# porovnejte s nasledujicim
median (X)
quantile (X, c (0.25, 0.5, 0.75, 0.33))
# 33% kvantil se lisi, R totiz pri vypoctu kvantilu vzdy pouziva prumerovani dvou okolnich hodnot

# boxplot pomoci stejnojmenne funkce
boxplot (X, horizontal = TRUE, ylim = range (X), xlab = "znamka", main = "krabicovy diagram (boxplot)")
stripchart (X, vertical = FALSE, method = "jitter", pch = 21, col = "red", bg = "yellow", cex = 1.5, add = TRUE)
# "fousy" v R oznacuji nejmensi a nejvetsi pozorovani, ktera jeste jsou uvnitr vnitrni hradby 



#	=====================================================================================================================================
# Priklad 2

# ... podobne jako v Prikladu 1

# dopocitejte si stredy a delky jednotlivych intervalu
d.j <- data$mez.h - data$mez.d
stredy <- (data$mez.d + data$mez.h) / 2
# a pridejte je do tabulky 

# ... podobne jako v Prikladu 1



#	=====================================================================================================================================
# Priklad 3 

puvodni_prumer <- 110
puvodni_rozptyl <- 800
n <- 20

opraveny_prumer <- puvodni_prumer - (85 + 120) / n + (95 + 150) / n
opraveny_prumer

opraveny_rozptyl <- puvodni_rozptyl - (85^2 + 120^2) / n + puvodni_prumer^2 + (95^2 + 150^2) / n - opraveny_prumer^2
opraveny_rozptyl



#	=====================================================================================================================================
