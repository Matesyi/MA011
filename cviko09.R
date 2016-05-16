# Priklad 1
X <- c (0.31, 0.30, 0.29, 0.32)
n <- length (X)
prumer <- mean(X)
odchylka <- sd(X)
alpha <- 0.05
kvantil <- qchisq (1 - alpha, n - 1)
D <- sqrt (n-1) * odchylka / sqrt (kvantil)
D


# Priklad 2 
data <- read.csv (file = "intervaly.csv", header = TRUE, sep = ",", dec = ".")
stredy <- (data$dolni + data$horni) / 2
X <- rep (stredy, data$cetnost)
n <- sum (data$cetnost)
prumer <- mean (X)
odchylka <- sd (X)
rozptyl <- var (X)
# a)
alpha <- 0.05
kvantil <- qt (1 - alpha/2, n - 1)
D <- prumer - kvantil * odchylka / sqrt(n)
H <- prumer + kvantil * odchylka / sqrt(n)
c (D, H)
# b)
alpha <- 0.05
kvantil1 <- qchisq (1 - alpha/2, n - 1)
kvantil2 <- qchisq (alpha/2, n - 1)
D <- (n - 1) * rozptyl / kvantil1
H <- (n - 1) * rozptyl / kvantil2
c (D, H)

# Priklad 3
N <- 8000
n <- 160
x <- 48
alpha <- 0.05
prumer  = x /n
odchylka <- sqrt (prumer * (1 - prumer))
kvantil <- qnor (1 - alpha /2)

# pro podil
#a) prumer
prumer
#b) interval
D <- prumer - kvantil *odchylka /sqrt(n)
H <- prumer + kvantil *odchylka /sqrt(n)
c (D, H)

# pro pocet zamestnancu
#a) prumer * n
prumer * N
# b)  interval
round (c (D, H) * N)

#Priklad 4
X <- c (0.140, 0.138, 0.143, 0.142, 0.144, 0.137)
Y <- c (0.135, 0.140, 0.142, 0.136, 0.138)
alpha <- 0.05
n_X <- length (X)
n_Y <- length (Y)
prumer_X <- mean(X)
prumer_Y <- mean(Y)
rozptyl_X <- 4*10^-6 #4e-6
rozptyl_Y <- 9*10^-6 #9e-6
S <- sqrt (rozptyl_X /n_X + rozptyl_Y /n_Y)
kvantil <- qnorm (1 - alpha)
D <- prumer_X - prumer_Y - kvantil *S
D


# Priklad 5
data <- read.csv (file = "selata.csv", header = TRUE, sep = ",", dec = ".")
X <- data$prir1 - data$prir2
n <- length(X)
alpha <- 0.05
prumer <- mean (X)
odchylka <- sd (X)
kvantil <- qt (1 - alpha / 2, n-1)
D <- prumer - kvantil * odchylka /sqrt(n)
D
H <- prumer + kvantil * odchylka /sqrt(n)
H
c (D, H)


# Priklad 6
# slajd 36
X <- c(3.26, 3.26, 3.27, 3.27)
Y <- c(3.23, 3.27, 3.29, 3.29)
alpha <- 0.05
n_X <- length(X)
n_Y <- length(Y)
kvantil <- qf (alpha, n_X - 1, n_Y - 1)
H <- var(X) / var(Y) * 1/kvantil
sqrt(H)
# kvantil <- qf (1 - alpha, n_Y - 1, n_X - 1)
# H <- sd (x) /sd(Y) *sqrt(kvantil) #sd(X) je odchylka_X


# Priklad 7
data <- read.csv (file = "spotreba.csv", header = TRUE, sep = ";", dec = ",")
X <- data$spotreba
n <- length(X)
alpha <- 0.05
prumer <- mean(X)
rozptyl <- var(X)
#...

# Priklad 10
data <- read.csv (file = "SiO2.csv", header = TRUE, sep = ";", dec = ",")
str(data)
#sloupec metoda je tvz. faktor, podle nehoz merene obsahy SiO2 rozdelime do dvou
X <- subset (data, metoda == "A")$obsah
Y <- subset (data, metoda == "B")$obsah
n_X <- length(X)
n_Y <- length(Y)
prumer_X <- mean (X)
prumer_Y <- mean (Y)
odchylka_X <- sd (X)
odchylka_Y <- sd (Y)
rozptyl_X <- var (X)
rozptyl_Y <- var (Y)

alpha <- 0.05
kvantil1 <- qf (1 - alpha /2, n_X - 1, n_Y - 1)
kvantil2 <- qf (1 - alpha /2, n_Y - 1, n_X - 1)
D <- rozptyl_X / rozptyl_Y / kvantil1
H <- rozptyl_X / rozptyl_Y * kvantil2
c (D, H)

alpha <- 0.05
S12 <- sqrt (((n_X - 1) * rozptyl_X + (n_Y - 1) * rozptyl_Y) / (n_X + n_Y -2))
kvantil <- qt (1 - alpha /2, n_X + n_Y - 2)
D <- prumer_Y - prumer_X - kvantil * S12 * sqrt ((n_X + n_Y) / (n_X * n_Y))
H <- prumer_Y - prumer_X + kvantil * S12 * sqrt ((n_X + n_Y) / (n_X * n_Y))
c (D, H)


# Priklad 11
data <- read.csv (file = "zakaznici.csv", header = TRUE, sep = ",", dec = ".")
str (data)
#.. jako predchozi 6,7..


# Priklad 12 
data <- read.csv (file = "kola.csv", header = TRUE, sep = ",", dec = ".")

# cyklus pomoci funkce "sapply", kde se promenna M (= cislo mesice) postupne meni od 1 do 12
# v kazdem cyklu vratime 4 cisla: cislo mesice, dolni odhad stredni hodnoty, prumer, a horni odhad stredni hodnoty 
matice <- sapply (seq (1, 12), function (M) {
	X <- subset (data, mnth == M)$cnt
	n <- length (X)
	prumer <- mean (X)
	odchylka <- sd (X)
	alpha <- 0.05
	kvantil <- qnorm (1 - alpha / 2)
	D <- prumer - kvantil * odchylka / sqrt (n) 
	H <- prumer + kvantil * odchylka / sqrt (n) 
	# jako posledni prikaz tela funkce v cyklu se uvede vektor cisel, ktera se maji ulozit do matice vysledku
	return (c (mesic = M, D = D, prumer = prumer, H = H))
})

matice 
# prehodime jeste sloupce a radky funkci "t" (= transpozice matice) a vytvorime tabulku 

tabulka <- data.frame (t (matice))
tabulka

plot (tabulka$mesic, tabulka$prumer, type = "b", lwd = 2, col = "red", ylim = c (1500, 6500), xlab = "mesic", ylab = "prumerny pocet zapujcek za den s 95% IS")
lines (tabulka$mesic, tabulka$D, type = "b", lty = 2, col = "green")
lines (tabulka$mesic, tabulka$H, type = "b", lty = 2, col = "blue")
