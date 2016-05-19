#PR01 - Dankovcikova, Karolyi
#hodime kostkou a priradime pravedepodobnosti
kostky <- rolldie (2)
S <- probspace (kostky)

# A = sucet 7
# B = nepadla dvojka
A <- subset (S, X1 + X2 == 7)
A
Prob(A)

B <- subset (S, (X1 != 2 & X2 != 2))
B
Prob(B)

#prunik obou pravdepodobnosti
#nepadla dvojka a sucet bol sedem
AB <- intersect (A, B)
AB
Prob(AB)
#0.111111

#pravdepodobnost ze sucet bol sedem, za podmienky ze padla dvojka
Prob (AB) / Prob (B)
#0.16

#otazka? su nezavisle
Prob(A) * Prob(B)
#0.115747
#P(A prienik B) nerovna sa P(A)*P(B) to znamena javy nie su nezavisle