library (prob)

#	Priklad 1

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

##################################################################
#	library (prob)

#	Priklad 2

# "D" = divka, "H" = hoch, "pes" = pes
u <- c ("D", "H", "pes")
p <- c (0.5, 0.3, 0.2)
#rozdeleni na vsechny moznosti (vytvoreni space)
iidspace (u, 1, probs = p)
S <- iidspace (u, 2, probs = p)
S

#vykresleni bar chartu s legendou
barplot (as.matrix (S$probs), col = rainbow (nrow (S)), horiz = TRUE, xlim = c (0, 1.4)) 
legend ("right", legend = paste (S$X1, S$X2, round (S$probs, 2), sep = " - ") , fill = rainbow (nrow (S)))

# A = obe deti jsou dcery
# B = jedno dite je dcera
# P (A | B) = ?
B <- subset (S, isin (S, "D"))
A <- subset (S, isin (S, c ("D", "D")))
AB <- intersect (A, B)
Prob (AB) / Prob (B)

#######################################################################################

#	library (prob)

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



# ================================================================================
#	Marie

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



# ================================================================================
#	Fikce: kdyby petinu populace v CR tvorily zeny se stejnym jmenem

u <- c ("DJ", "Do", "H")
# Fikce: 1 osoba z 5 ma jedno stejne zenske jmeno
p1 <- 1 / 5
p <- c (p1, 0.5 - p1, 0.5)
S <- iidspace (u, 2, probs = p)
S
#	V jedne rodine nemuze mit vice deti stejne jmeno
S[2,]$probs <- S[2,]$probs + S[1,]$probs / 2
S[4,]$probs <- S[4,]$probs + S[1,]$probs / 2
S[1,]$probs <- 0
S

dev.new ()
barplot (as.matrix (S$probs), col = rainbow (nrow (S)), horiz = TRUE, xlim = c (0, 1.4), main = "velmi zastoupene jmeno") 
legend ("right", legend = paste (S$X1, S$X2, round (S$probs, 2), sep = " - ") , fill = rainbow (nrow (S)))

B <- subset (S, isin (S, "DK"))
A <- subset (S, ! isin (S, "H"))
AB <- intersect (A, B)
Prob (AB) / Prob (B)

##########################################################################################

#	Priklad 4

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

# Analogicky zkontrolujte ostatni podminene pravdepodobnosti a pravdepodobnosti pruniku

