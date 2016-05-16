# Priklad 2 

data <- read.csv (file = "intervaly.csv", header = TRUE, sep = ",", dec = ".")
stredy <- (data$dolni + data$horni) / 2
X <- rep (stredy, data$cetnost)
n <- sum (data$cetnost)
prumer <- mean (X)
odchylka <- sd (X)
rozptyl <- var (X)

alpha <- 0.05
kvantil <- qt (1 - alpha/2, n - 1)
D <- prumer - kvantil * odchylka / sqrt(n)
H <- prumer + kvantil * odchylka / sqrt(n)
c (D, H)

alpha <- 0.05
kvantil1 <- qchisq (1 - alpha/2, n - 1)
kvantil2 <- qchisq (alpha/2, n - 1)
D <- (n - 1) * rozptyl / kvantil1
H <- (n - 1) * rozptyl / kvantil2
c (D, H)



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
