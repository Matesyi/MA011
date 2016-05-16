#	Priklad 10 

# Moivreova-Laplaceova veta: 
# Graficke porovnani binomickeho a normalniho rozdeleni

n <- 5
theta <- 0.5

EX <- n * theta
DX <- n * theta * (1 - theta)

x <- seq (0 - 5, n + 5, by = 0.01)
F <- pbinom (x, size = n, prob = theta)
F.norm <- pnorm (x, mean = EX, sd = sqrt (DX))

plot (x, F, type = "l", col = "red", lwd = 2, ylab = "distribucni funkce")
lines (x, F.norm, col = "blue", lwd = 2)
legend ("bottomright", c("binomicke", "normalni"), col = c ("red", "blue"), lwd = 2)
