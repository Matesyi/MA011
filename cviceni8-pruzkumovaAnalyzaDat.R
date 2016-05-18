#Cviceni 8 - Pruzkumova analyza dat
setwd("D:/tomas/Skola/FIMU/Jaro 2016/MV011/cviceni/Moje vypracovani")

#Priklad 1----------------------------------------------------------
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
sum (p.j) # musi dat 1

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

# graf empiricke distribucni funkce bez vertikalnich propojeni
X <- rep (tabulka$znamka, tabulka$n.j) # (co replikovat, kolikrat) - provede se na vektoru
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

# vypocet kvantilu, prvky je nutne seradit
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

#Priklad 2----------------------------------------------------------
data <- read.csv (file = "ocel.csv", header = TRUE, sep = ",", dec = ".")

#	pocet skupin
r <- nrow (data)

# stredy a delky jednotlivych intervalu
d.j <- data$mez.h - data$mez.d
stredy <- (data$mez.d + data$mez.h) / 2

n.j <- data$pocet

#	pocet pozorovani
n <- sum(n.j)

#	relativni cetnosti
p.j <- n.j / n
sum (p.j) # musi dat 1

# kumulativni absolutni cetnosti
N.j <- cumsum (n.j)

# kumulativni relativni cetnosti
F.j <- N.j / n

# cetnostni hustota (pouze u intervalovych mereni)
f.j <- p.j / d.j

#	tabulka (... , d.j - delky intervalu, n.j - pocet, p.j - relativni cetnost, N.j - kumulativni cetnost, F.j relativni kumulativni cetnost)
tabulka <- data.frame (data$mez.d, data$mez.h, stredy, d.j, n.j, p.j, N.j, F.j, f.j)

# diagramy

# sloupkovy diagram
barplot (tabulka$n.j, names.arg = tabulka$mez, xlab = "mez", ylab = "pocet pozorovani", main = "sloupkovy diagram")

#	graf cetnostni funkce
plot (tabulka$stredy, tabulka$n.j, type = "p", pch = 20, cex = 2, col = "red", xlab = "znamka", ylab = "hodnoty cetnostni funkce", main = "cetnostni funkce")

# polygon cetnosti
plot (tabulka$stredy, tabulka$n.j, type="b", pch = 20, xlab = "mez", ylab = "pocet pozorovani", main = "polygon cetnosti", col = "red", cex = 2, lty = 2)

# graf empiricke distribucni funkce
plot (c(0,tabulka$stredy,200), c(0,tabulka$F.j,1), type="s", xlab = "mez", ylab = "ECDF", main="empiricka distribucni funkce", col = "red", lwd = 2)

# graf empiricke distribucni funkce bez vertikalnich propojeni
X <- rep (tabulka$stredy, tabulka$n.j) # (co replikovat, kolikrat) - provede se na vektoru
plot (ecdf (X), col = "red", lwd = 2, xlab = "meze", ylab = "ECDF", main="empiricka distribucni funkce")

#	histogram 
hist (X, breaks = seq (40, 160, by = 20), freq = FALSE, col = "yellow", xlab = "meze", ylab = "cetnostni hustota", main = "histogram", xlim = c (20, 180))

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

# vypocet kvantilu, prvky je nutne seradit
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
# vypocet dolni vnitrni hradby a horni vnitrni hradby (napevno dane koeficienty -1.5 a 1.5)
c (x.25, x.75) + c (-1, 1) * 1.5 * q
# vypocet dolni vnejsi hradby a horni vnejsi hradby (napevno dane koeficienty -3 a 3)
c (x.25, x.75) + c (-1, 1) * 3 * q

# porovnejte s nasledujicim
median (X)
quantile (X, c (0.25, 0.5, 0.75, 0.33))
# 33% kvantil se lisi, R totiz pri vypoctu kvantilu vzdy pouziva prumerovani dvou okolnich hodnot

# boxplot pomoci stejnojmenne funkce
boxplot (X, horizontal = TRUE, ylim = range (X), xlab = "znamka", main = "krabicovy diagram (boxplot)")
stripchart (X, vertical = FALSE, method = "jitter", pch = 21, col = "red", bg = "yellow", cex = 1.5, add = TRUE)
# "fousy" v R oznacuji nejmensi a nejvetsi pozorovani, ktera jeste jsou uvnitr vnitrni hradby 

#Priklad 3----------------------------------------------------------
puvodni_prumer <- 110
puvodni_rozptyl <- 800
n <- 20

opraveny_prumer <- puvodni_prumer - (85 + 120) / n + (95 + 150) / n
opraveny_prumer

opraveny_rozptyl <- puvodni_rozptyl - (85^2 + 120^2) / n + puvodni_prumer^2 + (95^2 + 150^2) / n - opraveny_prumer^2
opraveny_rozptyl

#Priklad 4----------------------------------------------------------
data <- read.csv (file = "kola.csv", header = TRUE, sep = ",", dec = ".")
names (data)
str (data)

# vyber sloupce dat z datove tabulky
X <- data$cnt

n <- length (X)
r <- round (sqrt (n))
r

range (X)
diff (range (X)) / r
#	volime tedy 27 intervalu o delkach 322 

dolni <- seq (from = 21.5, to = 8714, by = 322)
horni <- seq (from = 21.5 + 322, to = 8720, by = 322)
stredy <- (dolni + horni) / 2

tabulka <- data.frame (dolni, horni, stredy)

n.j <- apply (tabulka, 1, function (w) {
  sum (X >= w[1] & X < w[2])
})
sum (n.j)

# cetnosti, relativni, absolutni, kumulativni, cetnostni hustota...
d.j <- horni - dolni
#	relativni cetnosti
p.j <- n.j / n
sum (p.j) # musi dat 1

# kumulativni absolutni cetnosti
N.j <- cumsum (n.j)

# kumulativni relativni cetnosti
F.j <- N.j / n

# cetnostni hustota (pouze u intervalovych mereni)
f.j <- p.j / d.j

tabulka <- cbind (tabulka, data.frame (n.j, p.j, N.j, F.j, d.j, f.j))
tabulka

# sloupkovy diagram cetnosti
# podobne jako v prikladu 2

# sloupkovy diagram relativnich cetnosti
# podobne jako v prikladu 2

# sloupkovy diagram cetnostni hustoty
# podobne jako v prikladu 2

#	defaultni histogram v R, zkousejte menit parametr "breaks"
# podobne jako v prikladu 2

# graf empiricke distribucni funkce
# podobne jako v prikladu 2

# sloupkovy diagram
barplot (tabulka$n.j, names.arg = tabulka$mez, xlab = "mez", ylab = "pocet pozorovani", main = "sloupkovy diagram")

# graf empiricke distribucni funkce bez vertikalnich propojeni
X <- rep (tabulka$stredy, tabulka$n.j) # (co replikovat, kolikrat) - provede se na vektoru
plot (ecdf (X), col = "red", lwd = 2, xlab = "meze", ylab = "ECDF", main="empiricka distribucni funkce")

#	histogram 
hist (X, breaks = seq (0, 9000, by = 500), freq = FALSE, col = "yellow", xlab = "meze", ylab = "cetnostni hustota", main = "histogram", xlim = c (0, 9000))

# prumer
prumer <- sum (X) / n
prumer

#rozptyl
rozptyl <- mean (X^2) - prumer^2
rozptyl

#smerodatna odchylka
smerodatna_odchylka <- sqrt (rozptyl)
smerodatna_odchylka

# vazeny prumer 
# podobne jako v prikladu 2

# vazeny rozptyl 
# podobne jako v prikladu 2

X <- rep (data$cnt, times=n.j) # (co replikovat, kolikrat) - provede se na vektoru

n <- nrow(data)

# vektor poradi a serazeny vzorek
R <- rank (X)
X.sorted <- sort (X)

# median, kvartily, kvartilova odchylka
c.25 <- 0.25 * n
c.50 <- 0.50 * n
c.75 <- 0.75 * n
c (c.25, c.50, c.75)

# kvantily
x.25 <- (X.sorted[c.25] + X.sorted[c.25 + 1]) / 2
x.50 <- (X.sorted[c.50] + X.sorted[c.50 + 1]) / 2
x.75 <- (X.sorted[c.75] + X.sorted[c.75 + 1]) / 2


# kvartilova odchylka (IQR)
q <- x.75 - x.25
q

# hradby boxplotu
c (x.25, x.75) + c (-1, 1) * 1.5 * q
c (x.25, x.75) + c (-1, 1) * 3 * q

c (x.25, x.50, x.75)
q <- x.75 - x.25
q

# hradby boxplotu
#...

# boxplot pomoci stejnojmenne funkce
boxplot (X, horizontal = TRUE, xlab = "pocty zapujcek", ylim = range (X), main = "krabicovy diagram (boxplot)")
stripchart (X, vertical = FALSE, method = "jitter", pch = 21, col = "red", bg = "yellow", cex = 0.8, add = TRUE)



# Q-Q plot
# tip: viz prednaska 7 a prislusny skript

# prevedeme poradi na faktor, abychom odfiltrovali duplicity
RR <- factor (R)
# zjistime poradi
j <- as.numeric (levels (RR))
# porovnejte
length (RR)
length (j)

# vzorecek pro alpha.j
alpha.j <- (j-.375)/(n+.25)
  
# prislusne kvantily standardizovaneho normalniho rozdeleni
u.j <- qnorm(alpha.j)
  
  # analogicky odfiltrujeme duplicity ve vzorku
  XX <- factor (X)
x <- as.numeric (levels (XX))
# porovnejte
length (XX)
length (j)

# vykreslime body grafu
plot (u.j, x, pch = 20, xlab = "teoreticky kvantil", ylab = "pozorovany kvantil", main = "Q-Q plot")

# prolozime primku
model <- lm (x ~ u.j)
lines (u.j, model$fitted.values, col = 2, lwd = 1.5)



# N-P plot
# tip: viz prednaska 7

alpha.j <- (j-.375)/(n+.25)
  u.j <- qnorm(alpha.j)
  
  # vykreslime body grafu
  plot (x, u.j, pch = 20, xlab = "pozorovana hodnota", ylab = "ocekavana normalni hodnota", main = "N-P plot")

# prolozime primku
model <- lm (u.j ~ x)
lines (x, model$fitted.values, col = 2, lwd = 1.5)



# P-P plot
# tip: viz prednaska 7

z <- (x - prumer)/smerodatna_odchylka #...
  F <- j/n #...
  Phi <- pnorm(z) #...
  
  # vykreslime body grafu
  plot (Phi, F, pch = 20, xlab = "teoreticka distribucni funkce", ylab = "empiricka distribucni funkce", main = "P-P plot")

# prikreslime primku y = x
abline (a = 0, b = 1, col = 2, lwd = 1.5)