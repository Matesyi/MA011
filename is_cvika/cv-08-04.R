data <- read.csv (file = "kola.csv", header = TRUE, sep = ",", dec = ".")
names (data)
str (data)

# vyber sloupce dat z datove tabulky
X <- data$cnt

# ========================================================================================

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


tabulka <- cbind (tabulka, data.frame (n.j, p.j, N.j, F.j, d.j, f.j))
tabulka

# sloupkovy diagram cetnosti
#...

# sloupkovy diagram relativnich cetnosti
#...

# sloupkovy diagram cetnostni hustoty
#...

#	defaultni histogram v R, zkousejte menit parametr "breaks"
#...

# graf empiricke distribucni funkce
#...

# prumer
#...

# rozptyl 
#...

# smerodatna odchylka
#...

# vazeny prumer 
#...

# vazeny rozptyl 
#...



# vektor poradi a serazeny vzorek
R <- rank (X)
X.sorted <- sort (X)

# median, kvartily, kvartilova odchylka
c.25 <- #...
c.50 <- #...
c.75 <- #...
c (c.25, c.50, c.75)

x.25 <- #...
x.50 <- #...
x.75 <- #...

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
