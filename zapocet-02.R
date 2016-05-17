#nastaveni cesty pracovniho adresare
# setwd ("C:/Users/Matesyi/Desktop/Programy/MA011")
#nactu data ze souboru
data <- read.csv (file = "kola.csv", header = TRUE, sep = ",", dec = ".")
#jmena sloupcu
names (data)
#jmena sloupcu s datovym typem a prvnimi n radky
str (data)

# vyber sloupce dat z datove tabulky
X <- data$cnt

# ========================================================================================

#pocet radku
n <- length (X)
r <- round (sqrt (n))
r
range (X)
diff (range (X)) / r
#	volime tedy 27 intervalu o delkach 322 

dolni <- seq (from = 21.5, to = 8714, by = 322)
horni <- seq (from = 21.5 + 322, to = 8720, by = 322)
stredy <- (dolni + horni) / 2
#delky jednotlivych intervalu
d.j <- horni - dolni

tabulka <- data.frame (dolni, horni, stredy)

#	absolutni cetnosti (nebo pocet)
n.j <- apply (tabulka, 1, function (w) {
  sum (X >= w[1] & X < w[2])
})
sum (n.j)

# cetnosti, relativni, absolutni, kumulativni, cetnostni hustota...
#	pocet pozorovani
n <- sum (n.j)

#	relativni cetnosti
p.j <- n.j / n
sum (p.j)

# kumulativni absolutni cetnosti
N.j <- cumsum (n.j)

# kumulativni relativni cetnosti
F.j <- N.j / n

f.j <- p.j / d.j

tabulka <- cbind (tabulka, data.frame (n.j, p.j, N.j, F.j, d.j, f.j))
tabulka

# sloupkovy diagram cetnosti
barplot (tabulka$n.j, names.arg = tabulka$stredy, xlab = "stredy", ylab = "pocet pozorovani", main = "barchart cetnosti")

# sloupkovy diagram relativnich cetnosti
barplot (tabulka$p.j, names.arg = tabulka$stredy, xlab = "stredy", ylab = "pocet pozorovani", main = "barchart relativnich cetnosti")

# sloupkovy diagram cetnostni hustoty
barplot (tabulka$N.j, names.arg = tabulka$stredy, xlab = "stredy", ylab = "pocet pozorovani", main = "barchart cetnosti hustoty")

# sloupkovy diagram relativni cetnostni hustoty
barplot (tabulka$F.j, names.arg = tabulka$stredy, xlab = "stredy", ylab = "pocet pozorovani", main = "barchart relativni cetnosti hustoty")

#	defaultni histogram v R, zkousejte menit parametr "breaks"
hist (X, breaks = c (tabulka$dolni[1], tabulka$horni), freq = FALSE, col = "blue", xlab = "pozorovani", ylab = "cetnostni hustota", main = "histogram", xlim = c (0, 10000))

# graf empiricke distribucni funkce
plot (c(0,tabulka$stredy,10000), c(0,tabulka$F.j,1), type="s", xlab = "stred", ylab = "ECDF", main="empiricka distribucni funkce", col = "red", lwd = 2)

# prumer
prumer <- sum (X) / n
prumer
# v R je pro prumer funkce "mean"
mean (X)

# rozpyl
rozptyl <- mean (X^2) - prumer^2
rozptyl

#smerodatna odchylka
smerodatna_odchylka <- sqrt (rozptyl)
smerodatna_odchylka

# v R je pro vyberovy rozptyl funkce "var", ale je potreba jej zkorigovat na rozptyl z prednasky
var (X) * (n-1) / n

# vazeny prumer 
#...

# vazeny rozptyl 
#...



# vektor poradi a serazeny vzorek
R <- rank (X)
X.sorted <- sort (X)

# median, kvartily
c.25 <- 0.25 * n
c.50 <- 0.50 * n
c.75 <- 0.75 * n
c (c.25, c.50, c.75)

#kvartily
x.25 <- (X.sorted[c.25] + X.sorted[c.25 + 1]) / 2
x.50 <- (X.sorted[c.50] + X.sorted[c.50 + 1]) / 2
x.75 <- (X.sorted[c.75] + X.sorted[c.75 + 1]) / 2
c (x.25, x.50, x.75)

#kvartilova odchylka
q <- x.75 - x.25
q

# hradby boxplotu
c (x.25, x.75) + c (-1, 1) * 1.5 * q
c (x.25, x.75) + c (-1, 1) * 3 * q

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
alpha.j <- #...
  
  # prislusne kvantily standardizovaneho normalniho rozdeleni
  u.j <- #...
  
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

alpha.j <- #...
  u.j <- #...
  
  # vykreslime body grafu
  plot (x, u.j, pch = 20, xlab = "pozorovana hodnota", ylab = "ocekavana normalni hodnota", main = "N-P plot")

# prolozime primku
model <- lm (u.j ~ x)
lines (x, model$fitted.values, col = 2, lwd = 1.5)



# P-P plot
# tip: viz prednaska 7

z <- #...
  F <- #...
  Phi <- #...
  
  # vykreslime body grafu
  plot (Phi, F, pch = 20, xlab = "teoreticka distribucni funkce", ylab = "empiricka distribucni funkce", main = "P-P plot")

# prikreslime primku y = x
abline (a = 0, b = 1, col = 2, lwd = 1.5)