########### Wczytanie bibliotek ##########################
library(DMRnet)
library(smurf)
library(MASS)
library(grpreg)
library(caret)
library(RColorBrewer)
#setwd("C:/Users/Aga/Documents/Sages/StacjaIT")

####### dane miete  #######################################
### maly przyklad
data(miete)
miete_small = data.frame(rent = miete$rent, rooms = miete$rooms, year = miete$year)
m1 = lm(rent~., data = miete_small)
length(m1$coef)
summary(m1)
model.matrix(m1)
### Sposob pierwszy: usunąć albo zostawić cały faktor

### Sposób drugi: usunąć kolumnę z macierzy planu
# y_1 = b_0 + b_1
# y_3 = b_0 + b_2
# y_28 = b_0
# Usuwajac kolumne z macierzy planu, np. rooms2 (b_2 = 0): 
# y_3 = b_0 (bo b_2 = 0), tyle samo co y_28 - łączenie rooms2 z interceptem
# Ale! Nie możemy połączyć np. romms2 z rooms3 (b_2 = b_3)

### Sposób trzeci: selekcja czynnikowa - dowolne grupowanie poziomów w faktorze:
m2 = DMR(miete_small[,c("rooms", "year")], miete_small$rent)
m2$beta[,3] 
m2$df[3]
###########################################################
?miete
summary(miete)
miete[, c("year", "size")] = scale(miete[, c("year", "size")])

### Zbiory treningowy 50%, walidacyjny 25% i testowy 25%
set.seed(17)

tr_va_id = createDataPartition(miete[, 1], p = 0.75)$Resample1

miete_tr_va = miete[tr_va_id, ]
miete_te = miete[-tr_va_id, ]

tr_id = createDataPartition(miete_tr_va[, 1], p = 0.67)$Resample1

miete_tr = miete_tr_va[tr_id, ]
miete_va = miete_tr_va[-tr_id, ]

dim(miete_tr)
dim(miete_va)
dim(miete_te)

X_tr = miete_tr[,-1]
y_tr = miete_tr[,1]
X_va = miete_va[,-1]
y_va = miete_va[,1]
X_te = miete_te[,-1]
y_te = miete_te[,1]

### Model regresji liniowej lm
model_lm = lm(rent ~ ., data = miete_tr)
summary(model_lm) 
model.matrix(model_lm)
md_lm = length(model_lm$coefficients)
pred_lm = predict(model_lm, X_va)
bl_lm = mean((pred_lm - y_va)^2)

### all subsets pakiet leaps nie działa na faktorach, na model.matrix nie ma sensu

### step AIC
model_aic = step(model_lm, k = 2)  
summary(model_aic) 
md_aic = length(model_aic$coefficients)
pred_aic = predict(model_aic, X_va)
bl_aic = mean((pred_aic - y_va)^2)

### step BIC
model_bic = step(model_lm, k = log(length(y_tr))) 
summary(model_bic) 
md_bic = length(model_bic$coefficients)
pred_bic = predict(model_bic, X_va)
bl_bic = mean((pred_bic - y_va)^2)

plot(md_lm, bl_lm, col = 2, pch = 8, xlab = "wielkosc modelu", ylab = "blad", type = "o", xlim = c(0, 39))
points(md_aic, bl_aic, col = 3, pch = 17, type = "o")
points(md_bic, bl_bic, col = 4, pch = 18, type = "o")
legend("topright", c("lm" , "stepAIC", "stepBIC"), col = 2:4, pch = c(8, 17, 18))

### group mcp
?grpreg
Z_tr = model.matrix(rent~., data = miete_tr)
Z_va = model.matrix(rent~., data = miete_va)
fl <- sapply(1:ncol(X_tr), function(i) length(levels(X_tr[,i])))
fl[fl == 0] = 2
model_mcp <- grpreg(Z_tr[,-1], y_tr, group=rep(1:length(fl), fl-1), penalty = "grMCP", family ="gaussian")
md_mcp <- model_mcp$df
pr = predict(model_mcp, Z_va[,-1])
pr = pr-y_va
bl_mcp = apply(pr, 2, function(x) mean(x^2))

### grouplasso
model_grl <- grpreg(Z_tr[,-1], y_tr, group=rep(1:length(fl), fl-1), penalty = "grLasso", family ="gaussian")
md_grl <- model_grl$df
pr = predict(model_grl, Z_va[,-1])
pr = pr-y_va
bl_grl = apply(pr, 2, function(x) mean(x^2))

plot(md_lm, bl_lm, col = 2, pch = 8, xlab = "wielkosc modelu", ylab = "blad", type = "o", ylim = c(20000, 30000), xlim = c(0,39))
lines(md_mcp, bl_mcp, col = 5, pch = 16, type = "o")
lines(md_grl, bl_grl, col = 6, pch = 8, type = "o")
points(md_aic, bl_aic, col = 3, pch = 17, type = "o", cex = 1.5)
points(md_bic, bl_bic, col = 4, pch = 18, type = "o", cex = 1.5)
legend("topright", c("lm" , "stepAIC", "stepBIC", "groupMCP", "groupLasso"), col = 2:6, pch = c(8, 17, 18, 16, 8))


### DMR
model_dmr= DMR(X_tr, y_tr)
md_dmr = model_dmr$df
pr = predict(model_dmr, X_va)
pr = pr-y_va
bl_dmr = apply(pr, 2, function(x) mean(x^2))

plot(md_dmr, bl_dmr, pch = 16, col = 1, xlab = "wielkosc modelu", ylab = "blad", type = "o", ylim = c(20000, 30000))
points(md_bic, bl_bic, col = 4, pch = 18, type = "o")
lines(md_mcp, bl_mcp, col = 5, pch = 16, type = "o")
lines(md_grl, bl_grl, col = 6, pch = 8, type = "o")
points(md_lm, bl_lm, col = 2, pch = 8, type = "o", cex = 1.5)
points(md_aic, bl_aic, col = 3, pch = 17, type = "o", cex = 1.5)
legend("topright", c("dmr", "lm" , "stepAIC", "stepBIC", "groupMCP", "groupLasso"), col = 1:6, pch = c(16, 8, 17, 18, 16, 8))

md_dmr[which.min(bl_dmr)]
model_dmr$beta[,which.min(bl_dmr)]
plot(1:39, model_dmr$beta[,which.min(bl_dmr)], pch = 16, col = 2)


### smurf
?smurf
form1 = rent ~ p(bathextra, pen = "flasso") + p(tiles, pen = "flasso")+p(area, pen = "gflasso")+p(kitchen, pen = "flasso")+p(rooms, pen = "gflasso")+p(best, pen = "flasso")+p(good, pen = "flasso")+p(warm, pen = "flasso")+p(central, pen = "flasso") +p(year, pen = "lasso")+p(size, pen = "lasso")
model_smurf = glmsmurf(formula = form1, family = gaussian(), data = miete_tr, lambda = "is.bic")
b_smurf <- model_smurf$coefficients
md_smurf = model_smurf$rank
pr = predict(model_smurf, X_va)
bl_smurf = mean((pr - y_va)^2)

plot(md_dmr, bl_dmr, pch = 16, col = 1, xlab = "wielkosc modelu", ylab = "blad", type = "o", ylim = c(20000, 30000))
points(md_lm, bl_lm, col = 2, pch = 8, type = "o")
points(md_aic, bl_aic, col = 3, pch = 17, type = "o")
points(md_bic, bl_bic, col = 4, pch = 18, type = "o")
lines(md_mcp, bl_mcp, col = 5, pch = 16, type = "o")
lines(md_grl, bl_grl, col = 6, pch = 8, type = "o")
points(md_smurf, bl_smurf, col = 7, pch = 17, type = "o")
legend("topright", c("dmr", "lm" , "stepAIC", "stepBIC", "groupMCP", "groupLasso", "smurf"), col = 1:7, pch = c(16, 8, 17, 18, 16, 8, 17))

# #### model fin
# model_fin = DMR(rbind(X_tr, X_va), c(y_tr, y_va))
# md_fin = md_dmr[which.min(bl_dmr)]
# pr = predict(model_fin, X_te)[,which(model_fin$df == md_fin)]
# bl_fin = mean((pr - y_te)^2)
# model_fin$beta[,which(model_fin$df == md_fin) ]
