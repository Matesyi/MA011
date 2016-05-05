# =============================================================================================================================
# Priklad 3 

tabulka <- read.csv (file = "CO2.csv", header = TRUE, sep = ",", dec = ".")
names (tabulka)
str (tabulka)
summary (tabulka)

# primka 

model1 <- lm (CO2 ~ Year, data = tabulka) 
model1
#provedeme pro celkovy vypis informaci
prehled1 <- summary (model1)
#Intercept je "beta 0"
prehled1
#beta0 = -1.3, beta1 = 0.85
# Yi = -1.3 + 0.85 + xi + "epsilon i"

model1$coefficients
coef (model1)
model1$residuals
residuals (model1)
model1$fitted.values
fitted.values (model1)
model.matrix (model1)

prehled1$sigma
prehled1$df
prehled1$r.squared
prehled1$adj.r.squared
prehled1$fstatistic

# kubicka parabola

model2 <- lm (CO2 ~ Year + I(Year^2) + I(Year^3), data = tabulka) 
model2
prehled2 <- summary (model2)
#p hodnoty jsou vysoke takze je to spatny(pro slovaky zly) model
prehled2

model2$coefficients
coef (model2)
model2$residual2
residuals (model2)
model2$fitted.values
fitted.values (model2)
model.matrix (model2)

prehled2$sigma
prehled2$df
prehled2$r.squared
prehled2$adj.r.squared
prehled2$fstatistic

# grafika

summary (tabulka)
plot (c (1760, 2000), c (250, 370), type = "n", xlab = "rok", ylab = "CO2")

points (tabulka$Year, tabulka$CO2, col = "blue", pch = 24, lwd = 1.5, cex = 1.0)

# pruchystame si sit na x-ove ose
x <- seq (1760, 2000, by = 0.1)
# pomoci funkce "predict" nechame dopocitat odpovidajici Y-ove hodnoty na regresni funkci
Y1 <- predict (model1, data.frame (Year = x))
Y2 <- predict (model2, data.frame (Year = x))

#primka
lines (x, Y1, col = "red", lwd = 2)
#parabola
lines (x, Y2, col = "#00cc00", lwd = 2)

# 95% intervaly spolehlivosti pro koeficienty modelu
confint (model1, level = 0.95)
# porovnejte je s testy vyznamnosti jednotlivych parametru
prehled1

confint (model2, level = 0.95)
prehled2

# muzeme prikreslit 95% pasy spolehlivosti pro stredni hodnotu, tj. kolem regresni funkce
CI1 <- predict (model1, data.frame (Year = x), interval = "confidence", level = 0.95)
CI2 <- predict (model2, data.frame (Year = x), interval = "confidence", level = 0.95)
lines (x, CI1[,2], col = "red", lty = 2)
lines (x, CI1[,3], col = "red", lty = 2)
lines (x, CI2[,2], col = "#00cc00", lty = 2)
lines (x, CI2[,3], col = "#00cc00", lty = 2)
#IMPORTANT - ulozeni grafu do PDF }budeme potrebovat na zapoctu)
dev.copy2pdf (file = "obrazek5.pdf", width = 5, height = 4)

# boxploty rezidui

boxplot (model1$residuals, model2$residuals, ylab = "rezidua", names = c(1,2), border = c ("red", "#00cc00"))
dev.copy2pdf (file = "obrazek6.pdf", width = 3.5, height = 4)

# porovnani modelu

#tady jsou reziuda lepsi
sum (model1$residuals^2) / model1$df.residual
prehled1$r.squared
prehled1$adj.r.squared

sum (model2$residuals^2) / model2$df.residual
prehled2$r.squared
prehled2$adj.r.squared

anova (model1, model2) 

#parabola vysla mnohem lepe nez primka - vyberu parabolu

