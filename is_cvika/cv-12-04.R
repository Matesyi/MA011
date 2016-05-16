#	Nacteme data
tabulka <- read.csv2 (file = "rada1.csv")

# Podivame se na strukturu
str (tabulka)
# Vsimneme si, ze obe promenne jsou brany jako kvantitativni

#	Bodovy graf
range (tabulka$x)
range (tabulka$Y)
plot (tabulka$x, tabulka$Y, type = "p", pch = 20, xlab = "x", ylab = "Y", xlim = c (0, 11), ylim = c (0, 200))

# Linearni modely
model3 <- lm (Y ~ 1 + x + I(x^2) + I(x^3), data = tabulka)
model2 <- lm (Y ~ 1 + x + I(x^2), data = tabulka)
model1 <- lm (Y ~ 1 + x, data = tabulka)
model0 <- lm (Y ~ 0 + x, data = tabulka)
summary (model3)
summary (model2)
summary (model1)
summary (model0)

# Porovnani modely pomoci ANOVy
anova (model3, model2)
# => prechodem od modelu 3 k modelu 2 se linearni model statisticky vyznamne nezhorsi 
anova (model2, model1)
# => prechodem od modelu 2 k modelu 1 se linearni model statisticky vyznamne zhorsi 
# => z teto trojice bychom meli zvolit model 2
anova (model1, model0)

# Vykreslime vsechny zavislosti do grafu 
xx <- seq (0, 11, by = 0.1)
Y3 <- predict (model3, data.frame (x = xx))
Y2 <- predict (model2, data.frame (x = xx))
Y1 <- predict (model1, data.frame (x = xx))
Y0 <- predict (model0, data.frame (x = xx))
lines (xx, Y3, lwd = 2, col = "red")
lines (xx, Y2, lwd = 2, col = "green")
lines (xx, Y1, lwd = 2, col = "blue")
lines (xx, Y0, lwd = 2, col = "cyan")
legend ("top", legend = c ("m3", "m2", "m1", "m0"), col = c ("red", "green", "blue", "cyan"), lty = c (1, 1, 1, 1), lwd = 2, horiz = TRUE)

# Podivame se na QQ-ploty rezidui
boxplot (model3$residuals, model2$residuals, model1$residuals, model0$residuals, names = c ("m3", "m2", "m1", "m0"), col = c ("red", "green", "blue", "cyan"), ylab = "Rezidua")
par (mfrow = c (1, 4))
qqnorm (model3$residuals, col = "red")
qqline (model3$residuals, col = "red")
qqnorm (model2$residuals, col = "green")
qqline (model2$residuals, col = "green")
qqnorm (model1$residuals, col = "blue")
qqline (model1$residuals, col = "blue")
qqnorm (model0$residuals, col = "cyan")
qqline (model0$residuals, col = "cyan")
par (mfrow = c (1, 1))

