#mince
n <- 4

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

#kostky
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

#tri kostky

kostky2 <- rolldie(3)

s <- probspace (kostky2)

s1 <- s
s1$soucet <- apply(kostky2, 1, sum)
#S1$soucin <- apply(kostky2, 1, prod)

tab.soucet <- table(s1$soucet)

psti.soucet <- tab.soucet / nrow(s1)
psti.soucet
barplot(psti.soucet)

#geometricka prob
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

##########################################################

n <- 100

d <- 10

par (mfrow = c(1,1), mar = c(4,4,4,1))
plot(c(0,d), c(0,d), type = "n", xlab = "bob x", ylab = "bod y")

x <- runif (n, 0, d)
y <- runif (n, 0, d)

v <- data.frame (cbind(x,y))
v$ok <- (( v$X <= v$Y) & (v$Y > d/2) & (v$X <d/2) & (v$Y < v$X + d/2) | (v$X > v$Y)) #doplnit a opravit
v$color <- ifelse(v$ok, "#00cc00", "#ff0000")
v$symbol <- ifelse (v$ok, 1,4)

points (v$X, v$Y, col = c$color, pch = v$symbol, lwd = 2)
nA <- sum (v$ok)
fA <- nA / n
title (main = paste(nA, "/", n, "=", fA))


