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