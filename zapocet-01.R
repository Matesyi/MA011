library(prob)
#-------------------------------------------------------------------------------
#-MINCE-------------------------------------------------------------------------
#-------------------------------------------------------------------------------
n <- 4

#hozeni kostkou
mince <- tosscoin(n)
mince

dim(mince)
nrow(mince)
ncol(mince)

names (mince) <- c ("first", "second", "third")

mince$first
mince$second

str(mince)

as.numeric (mince$third)

s <- probspace(mince)
s
str (s)
names (s)

nrow(s)

A <- subset (s, isin (s, rep ("H", n)))
A

nrow (A)
nrow (A) / nrow (s)

Prob (A)

B0 <- subset (s, isin (s, rep ("T", 4)))
B1 <- subset (s, isin (s, c ("H", "T", "T", "T")))
B2 <- subset (s, isin (s, c ("H", "H", "T", "T")))
B3 <- subset (s, isin (s, c ("H", "H", "H", "T")))
B4 <- A

Prob(B0)
Prob(B3)

psti <- c (Prob(B0), Prob(B1), Prob(B2), Prob(B3), Prob(B4))

sum (psti)

names (psti) <- seq (0, 4, by = 1)
#bar chart
barplot (psti, xlab = "head count", ylab = "prob")

#-------------------------------------------------------------------------------
#-KOSTKY------------------------------------------------------------------------
#-------------------------------------------------------------------------------
kostky <- rolldie(2)
s <- probspace(kostky)

s
dim(s)

apply(kostky, 1, sum)
apply(kostky, 2, sum)

s1 <- s
s1$soucet <- apply (kostky, 1, sum)

s1
str(s1)
names(s1)

tab <- table (s1$soucet)
tab

psti <- tab /nrow (s1)
psti
sum (psti)
#bar chart
barplot(psti)

#-------------------------------------------------------------------------------
#-KOSTKY 2 - PODMINENA ---------------------------------------------------------
#-------------------------------------------------------------------------------
#hodime kostkou a priradime pravedepodobnosti
kostky <- rolldie (2)
S <- probspace (kostky)

# A = 2 petky
# B = soucet delitelny peti
A <- subset (S, X1 == 5 & X2 == 5)
B <- subset (S, (X1 + X2) %% 5 == 0)
#prunik obou pravdepodobnosti
AB <- intersect (A, B)
#vzpsani pravdepodobnosti na konzolu
Prob (AB) / Prob (B)

#-------------------------------------------------------------------------------
#-TRI KOSTKY--------------------------------------------------------------------
#-------------------------------------------------------------------------------
kostky2 <- rolldie(3)

s <- probspace (kostky2)

s1 <- s
s1$soucet <- apply(kostky2, 1, sum)
#S1$soucin <- apply(kostky2, 1, prod)

tab.soucet <- table(s1$soucet)

psti.soucet <- tab.soucet / nrow(s1)
psti.soucet
barplot(psti.soucet)

#-------------------------------------------------------------------------------
#-GEOMETRICKA PRAVDEPODOBNOST---------------------------------------------------
#-------------------------------------------------------------------------------
# Priklad 10

n <- 1000

# nastaveni grafickych parametru, vice viz "?par"
par (mfrow = c(1,1), mar = c (4,4,4,1))
# prikazem "plot" s parametrem type="n" prichystame prazdny souradnicovy system 
plot (c(18,19), c(18,19), type = "n", xlab = "Eva", ylab = "Honza")

# funkce "runif" generuje vektor (delky n) nahodnych cisel nezavisle generovanych v zadanem intervalu 
E <- runif (n, 18, 19)
H <- runif (n, 18, 19)

# funkce "cbind" vytvari matici z vektoru (matic) tak, ze je umisti jako sloupce vedle sebe
# funkce "data.frame" vytvari datovou tabulku
# v datove tabulce sloupce odpovidaji jednotlivym promennych, radky odpovidaji jednotlivym merenim (opakovanim pokusu)
V <- data.frame (cbind (E, H))
# logicka formule
V$ok <- (V$E - 1/6 <= V$H) & (V$H <= V$E + 1/3)
V$color <- ifelse (V$ok, "#00cc00", "#ff0000")
V$symbol <- ifelse (V$ok, 4, 4)

# prikaz "points" vykresluje body, jejich souradnice jsou zadany vektory jako prvni dva argumenty
# parametr col nastavuje barvu, parametr pch kreslici symbol, lwd tloustku car
points (V$E, V$H, col = V$color, pch = V$symbol, lwd = 2)
# spocitame a do obrazku vypiseme relativni cetnost
nA <- sum (V$ok)
fA <- nA / n
title (main = paste (nA, "/", n, "=", fA))

#-------------------------------------------------------------------------------
#-KUNHUTA - PODMINENA PRAVDEPODOBNOST-------------------------------------------
#-------------------------------------------------------------------------------
#	Priklad 3

#	Kunhuta
# "DK" = divka Kunhuta, "Do" = divka jineho jmena, "H" = hoch

u <- c ("DK", "Do", "H")
# Ceska republika: 1 osoba z 3566848 ma jmeno Kunhuta
p1 <- 1 / 3566848
p <- c (p1, 0.5 - p1, 0.5)
S <- iidspace (u, 2, probs = p)
S
#	V jedne rodine nemuze mit vice deti stejne jmeno
S[2,]$probs <- S[2,]$probs + S[1,]$probs / 2
S[4,]$probs <- S[4,]$probs + S[1,]$probs / 2
S[1,]$probs <- 0
S

dev.new ()
barplot (as.matrix (S$probs), col = rainbow (nrow (S)), horiz = TRUE, xlim = c (0, 1.4), main = "Kunhuta") 
legend ("right", legend = paste (S$X1, S$X2, round (S$probs, 2), sep = " - ") , fill = rainbow (nrow (S)))

B <- subset (S, isin (S, "DK"))
A <- subset (S, ! isin (S, "H"))
AB <- intersect (A, B)
Prob (AB) / Prob (B)


#-------------------------------------------------------------------------------
#-MARIE (KUNHUTA) - PODMINENA PRAVDEPODOBNOST-----------------------------------
#-------------------------------------------------------------------------------

u <- c ("DM", "Do", "H")
# Ceska republika: 1 osoba z 40 ma jmeno Marie
p1 <- 1 / 40
p <- c (p1, 0.5 - p1, 0.5)
S <- iidspace (u, 2, probs = p)
S
#	V jedne rodine nemuze mit vice deti stejne jmeno
S[2,]$probs <- S[2,]$probs + S[1,]$probs / 2
S[4,]$probs <- S[4,]$probs + S[1,]$probs / 2
S[1,]$probs <- 0
S

dev.new ()
barplot (as.matrix (S$probs), col = rainbow (nrow (S)), horiz = TRUE, xlim = c (0, 1.4), main = "Marie") 
legend ("right", legend = paste (S$X1, S$X2, round (S$probs, 2), sep = " - ") , fill = rainbow (nrow (S)))

B <- subset (S, isin (S, "DK"))
A <- subset (S, ! isin (S, "H"))
AB <- intersect (A, B)
Prob (AB) / Prob (B)

#-------------------------------------------------------------------------------
#-NEHODOVOST - PODMINENA PRAVDEPODOBNOST----------------------------------------
#-------------------------------------------------------------------------------

# Pravdepodobnostn prostor vytvorime primym zapisem
kategorie <- rep (c ("A", "B", "C"), each = 2)
nehoda <- rep (c ("ano", "ne"), 3)
# Zkombinujeme do datove tabulky
el.jevy <- data.frame (kategorie, nehoda)
# Zadame pravdepodobnosti 
p <- c (0.7 * 0.03, 0.7 * (1-0.03), 0.2 * 0.06, 0.2 * (1-0.06), 0.1 * 0.1, 0.1 * (1-0.1))
S <- probspace (el.jevy, probs = p)

S
sum (S$probs)

barplot (as.matrix (S$probs), col = rainbow (nrow (S)), horiz = TRUE, xlim = c (0, 1.4), main = "kategorie - nehoda") 
legend ("right", legend = paste (S$kategorie, S$nehoda, round (S$probs, 2), sep = " - ") , fill = rainbow (nrow (S)))

N <- subset (S, nehoda == "ano")
Prob (N)

A <- subset (S, kategorie == "A")
AN <- intersect (A, N)
Prob (AN) / Prob (N)
Prob (AN)