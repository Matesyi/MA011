# Potrebne knihovny - pokud je dosud nemate, je treba je nejdrive nainstalovat, napr. funkci "install.packages"
library (agricolae)	#	pro Scheffeho test
library (car)	#	pro Levenuv test

#	Nacteme data
tabulka <- read.csv2 (file = "brambory.csv")
# Podivame se na strukturu
summary (tabulka)
str (tabulka)
# Vsimneme si, ze obe promenne se nacetly jako numericke
# Promenna "odruda" je vsak kalitativni, cisla maji povahu kodoveho oznaceni kategorii
# V R se takove promenne rika "factor" a vyrobime ji takto 
tabulka$odruda
tabulka$odruda <- factor (tabulka$odruda)
tabulka$odruda
# Podivame se na upravenou tabulku, promenna "odruda" uz je nyni kvalitativni 
summary (tabulka)
str (tabulka)
# Interne jsou kategorie kodovany prirozenymi cisly, muzeme s nimi takto i pracovat
as.numeric(tabulka$odruda)
levels (tabulka$odruda)

# Podivame se na cetnosti, prumery a mediany
table (tabulka$odruda)
table (tabulka)
tapply (tabulka$hmotnost, tabulka$odruda, mean)
tapply (tabulka$hmotnost, tabulka$odruda, median)

#	Grafy 
par (mfrow = c (1, 2))
plot.design (hmotnost ~ odruda, data = tabulka, fun = mean)
plot.design (hmotnost ~ odruda, data = tabulka, fun = median)
par (mfrow = c (1, 1))
barvy <- c ("red", "green", "cyan", "orange")

# Boxplot a overeni homogenity rozptylu
boxplot (hmotnost ~ odruda, data = tabulka, col = barvy, xlab = "odruda", ylab = "hmotnost")
points (tabulka$odruda, tabulka$hmotnost, pch = 4)
# Bartlettuv test
bartlett.test (hmotnost ~ odruda, data = tabulka)
# Bartlettuv test
leveneTest (hmotnost ~ odruda, data = tabulka)

# Rozdeleni datove tabulky podle jednotlivych hodnot faktoru, napr. takto: 
skupiny <- lapply (levels (tabulka$odruda), function (L) {
	return (subset (tabulka, odruda == L))
	})
# Vysledek je tzv. "list"
str (skupiny)
# Porovnejte nasledujici
skupiny[[1]]
skupiny[1]

# Overeni normality - histogramy a QQ-ploty
par (mfrow = c (2, 2))
sapply (skupiny, function (x) {
	hist (x$hmotnost, freq = FALSE, main = x$odruda[1])
	})
sapply (skupiny, function (x) {
	qqnorm (x$hmotnost, main = x$odruda[1])
	qqline (x$hmotnost)
	})
par (mfrow = c (1, 1))
# Overeni normality - Shapiruv-Wilkeuv test
lapply (skupiny, function (x) {
	shapiro.test (x$hmotnost)
	})

#	linearni regresni model pomoc ifunkce "lm"
model <- lm (hmotnost ~ odruda, data = tabulka)
# Praci s vysledky LRM jiz zname z minuleho semestru
model
model$coefficients
# Matice planu - vsimneme si designu, ktery R automaticky zvolilo
model.matrix (model)
# Podrobnejsi informace
summary (model)

# Odhady strednich hodnot pro jednotlive skupiny
predict (model, data.frame (odruda = levels (tabulka$odruda)))



# ANOVA na LRM s pomoci funkce "anova"
anova.model <- anova (model)
anova.model

# Vyzkousejte nasledujici prikazu. O jaky LRM se jedna? Jak vysvetlite vyslednou tabulku ANOVy porovnani 2 LRM? 
model0 <- lm (hmotnost ~ 1, data = tabulka)
model0
model$coefficients
model.matrix (model)
summary (model)
anova (model0, model)



# ANOVA pomoci funkce "aov"
aov.model <- aov (hmotnost ~ odruda, data = tabulka)
aov.model
# Podivejme se do vysledneho objektu
names (aov.model)
# Zjistime design 
model.matrix (aov.model)
# Tabulky ANOVy ziskame takto
summary (aov.model)
# Odhady efektu, kontrastu a strednich hodnot
aov.model$coefficients
model.tables (aov.model, type = "effects")
model.tables (aov.model, type = "means")

# Nulovou hypotezu ANOVy jsme na hladine vyznamnosti 95 % zamitli



# Mnohonasobne porovnavani - Scheffeho test
ScheffeTest <- scheffe.test (aov.model, "odruda")
ScheffeTest
# pro nas podstatne je v promenne 
ScheffeTest$groups
# zajimavy ukol pro programatory: naprogramovat rozumnou funkci, ktera z tohoto vypise vsechny dvojice, ktere se signifikantne odlisuji
#	=> lisi se dvojice 1-3

# Scheffeho test rucne
# mean squae error
s2 <- anova.model["Residuals", "Mean Sq"]
# anebo (pozor, tabulka ANOVy z funkce "aov" je az v 1. polozce listu summary!)
s2 <- summary(aov.model)[[1]]["Residuals", "Mean Sq"]
# pocet skupin a mereni
a <- length (skupiny)
n <- nrow (tabulka)
c (n, a)
# Pocitame napr. pro dvojici A-B - absolutni hondota rozdilu prumeru
Leva <- abs (mean (skupiny[[1]]$hmotnost) - mean (skupiny[[2]]$hmotnost))
# prava strana vzorce pro Seffeho test
Prava <- sqrt (s2 * (a - 1) * (1 / length (skupiny[[1]]$hmotnost) + 1 / length (skupiny[[2]]$hmotnost)) * qf (0.95, a - 1, n - a))
Leva; Prava
Leva >= Prava



# Mnohonasobne porovnavani - Scheffeho test
# TukeyHSD (HSD = Honest Significant Difference) je verze Tukeyova testu pouzitelna i pro nevyvazene trideni 
TukeyTest <- TukeyHSD (aov.model)
# Ktere dvojice jsou signifikantne odlisne? 
TukeyTest
# Gragicke vyjadreni - jak nyni pozname, ktere dvojice jsou signifikantne odlisne? 
plot (TukeyTest, las = 1)
#	=> lisi se dvojice 1-2, 1-3



# Neparametrika varianta ANOVy - Kruskalluv-Wallisuv test
KWTest <- kruskal (tabulka$hmotnost, tabulka$odruda)
KWTest
#	=> lisi se dvojice A-B, A-C, A-D, C-D

