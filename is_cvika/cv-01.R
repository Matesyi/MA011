#	Vypocty Prikladu z kombinatriky 

#	1. (a)
a <- factorial (5)
a

#	1. (b)
a / 2

# 1. (c)
factorial (4)

# 2. (a)
6 * 6

# 2. (b)
6 * 7 / 2

# 3. 
7 * 6 * 5
# nebo
choose (7, 3) * factorial (3)
# nebo
factorial (7) / factorial (4)

# 5. 
choose (3, 1) * choose (8, 1) * choose (4, 1) * choose (5, 1)
# jednoduseji:
3 * 8 * 4 * 5

# 6. (a)
b <- choose (5, 2)
b

# 6. (b)
b - 1

# 7. (a) 
choose (7 - 1 + 5, 6)
# nebo
choose (7 - 1 + 5, 5)

# 7. (b)
choose (7, 5)

# 8. (a)
2^8
# umocnovani lze psat take pomoci dvou hvezdicek (umocnovani ve FORTRANu) 
2**8

# 8. (b) 
2^5 + 2^5

# 9. (a)
factorial (4)

# 9. (b)
factorial (5) / factorial (2) 

# 9. (c)
# pozor na poradi operatoru 
a <- factorial (11) / factorial (4) / factorial (4) / factorial (2)
a

# 9. (d)
b <- factorial (8) / factorial (4) / factorial (2)
b
a - b

# 10. (a) 
4 * 4 * 3 * 2
# nebo 
factorial (5) - factorial (4)

# 10. (b) 
3 * 3 * 2 + 4 * 3 * 2

# 11. (a) 
choose (4 - 1 + 10, 3) * choose (4 - 1 + 15, 3) * choose (4 - 1 + 8, 3)

# 11. (b) 
choose (4 - 1 + 6, 3) * choose (4 - 1 + 11, 3) * choose (4 - 1 + 4, 3)



###############################################################################################################################
# Ruzne ukazky, co vse (nejen) s R lze provadet za vypocty 

# vse za znakem # az do konce radku je komentar
# Vytvorime promennou "a" s hodnotou 5,5
a <- 5.5	#	pouziva se desetinna tecka

# hodnota promenne je vypsana, napiseme-li jen nazev promenne
a

# jde to prip. i smerem doprava, ale neni to moc obvykle
25 -> b
b

# vytvorime vektor o 4 slozkach funkci "c"
v <- c (1.25, 10, a, b)
v

# podivame se na jeho delku
length (v)

# vybereme 3. slozku vektoru pomoci hranatych zavorek
v[3]

# 2. a 4. 
v[c(2,4)]

# zaporny index naopak vynechava
v[-2]

# nektere caste vektory maji svoje funkce, napr. 
# posloupnost 
seq (0, 5, by = 0.5)

# anebo 
seq (0, 5, len = 6)

# opakovani stejne hodnoty, treba 5 krat cislo 10
rep (10, 5)

# matice se vytvari trochu komplikovaneji. 
# Nejdriv se vytvori vektor, v nemz jsou pod sebou vsechny sloupce matice, a az z nej se funkci "matrix" tvorti matice
M <- matrix (v, 2, 2)	# 2 a 2 jsou pocty radku a sloupcu 

# matice se opravdu necekane zadava po sloupcich, ve statistice je totiz obvykle, ze v datovych tabulkach ma kazda velicina svuj sloupec 
# treba matici rozmeru 4 x 2 cisel od 1 do 8 
matrix (seq (1, 8, by = 1), 4, 2)

# pokud bychom chteli matici zadavat po radcich (jak obvykle cteme), pouzijeme nepovinny parametr byrow = TRUE
Matice <- matrix (seq (1, 8, by = 1), 4, 2, byrow = TRUE)
Matice

# transponovana matice
t (Matice)

# indexovani u matice je analogicke vektorum, ale v hranatych zavorkach je nejdriv index radku a po carce index sloupce
Matice[1,2]
Matice[1,]
Matice[,2]
Matice[,c(2,4)]
Matice[-1,]

# nejake klasicke matematicke operace se skalary, vektory, maticemi. 
# R u vektoru a matic automaticky operace aplikuje po slozkach

a + b
2*v
v^3
2 * M^2

# poradi operatoru je obvykle, kulate zavorky maji prednost
40 / 4 / 5
40 / (4 * 5)

# kdyz se pokusime o matematicky nespravne operace, R ji v mnoha pripadech ji intiutivne provede doplnenim skalaru na vektor ci vektoru na matici, ale je lepsi se tomu vyhnout
M + v
M + 3
3 * M - a

# textove hodnoty se zapisuji do uvozovek ci apostrofu
text <- "muj text"

# logicke hodnoty TRUE/FALSE se zapisuji velkymi pismeny
d <- TRUE
d
!d

# zkusime nejakou logickou formuli, klasicke znaceni: & je AND, | je OR, ! je NOT
l <- (!d & (a >= 100)) | (b > 0)
l

# pri aritmetickych operacich ma klasicky TRUE hodnotu 1, FALSE hodnotu 0
1 * l + 0.5

# Vypocet faktorialu: 
factorial (5) 

# Vypocet kombinacnihpo cisla: 5 nad 2 je? 
choose (5, 2)



###############################################################################################################################
# Do pristiho cviceni si doinstalujte knihovnu "prob". 
# Instalce knihovny "prob", pokud jeste neni nainstalovana. Lze provest take pomoci Instal na zalozce Packages v RStudiu. 
install.packages (c("prob"))

# Nacteni knihovny "prob". Lze provest take zaskrtnutim prislusneho zaskrtavatka v seznamu na zalozce Packages v RStudiu. 
library (prob)

# Pro kontrolu uspesneho nacteni knihovny "prob" se podivejme napr. do helpu prikazu "Prob".
?Prob
