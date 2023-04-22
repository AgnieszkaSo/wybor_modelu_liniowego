########### Wczytanie bibliotek ##########################
library(DMRnet)
library(glmnet)
library(MASS)
library(sparsenet)
library(caret)
library(bbreg)
library(leaps)
library(RColorBrewer)
#setwd("C:/Users/Aga/Documents/Sages/StacjaIT")

############ Dane bodyfat ################################
data(BF)
?BF
summary(BF)

### skalowanie danych
BF[, -1] = scale(BF[, -1])

### Zbiory treningowy 50%, walidacyjny 25% i testowy 25%
set.seed(13)
# set.seed(7)

tr_va_id = createDataPartition(BF[, 1], p = 0.75)$Resample1

BF_tr_va = BF[tr_va_id, ]
BF_te = BF[-tr_va_id, ]

tr_id = createDataPartition(BF_tr_va[, 1], p = 0.67)$Resample1

BF_tr = BF_tr_va[tr_id, ]
BF_va = BF_tr_va[-tr_id, ]

dim(BF_tr)
dim(BF_va)
dim(BF_te)

X_tr = BF_tr[,-1]
y_tr = BF_tr[,1]
X_va = BF_va[,-1]
y_va = BF_va[,1]
X_te = BF_te[,-1]
y_te = BF_te[,1]

### Model regresji liniowej lm
model_lm = lm(bodyfat ~ ., data = BF_tr)
summary(model_lm) 
md_lm = length(model_lm$coefficients)
pred_lm = predict(model_lm, X_va)
bl_lm = mean((pred_lm - y_va)^2)
  
### all subsets
?leaps
model_allsubset = leaps(x = X_tr, y = y_tr, method = "r2", nbest = 1)
md_allsubset = model_allsubset$size
bl_allsubset = c()
Varsy = list()
for (i in 1:ncol(X_tr)){
  varsy = colnames(X_tr)[model_allsubset$which[i,]]
  Varsy[[i]] = varsy
  form = as.formula(paste("y~", paste("+", varsy, collapse = "")))
  m_i = lm(form, data = data.frame(y = y_tr, X_tr))
  bl_allsubset[i] = mean((predict(m_i, X_va) - y_va)^2)
}
Varsy

plot(md_allsubset, bl_allsubset, pch = 16, col = 1, xlab = "wielkosc modelu", ylab = "blad", type = "o", xlim = c(1,14))
points(md_lm, bl_lm, col = 2, pch = 8, type = "o")
legend("topright", c("all subset", "lm"), col = 1:2, pch = c(16, 8))

Varsy[[which.min(bl_allsubset)]]

### step AIC
?step
model_aic = step(model_lm, k = 2)  
summary(model_aic) 
md_aic = length(model_aic$coefficients)
pred_aic = predict(model_aic, X_va)
bl_aic = mean((pred_aic - y_va)^2)

summary(model_aic)

### step BIC 
model_bic = step(model_lm, k = log(length(y_tr))) 
summary(model_bic) 
md_bic = length(model_bic$coefficients)
pred_bic = predict(model_bic, X_va)
bl_bic = mean((pred_bic - y_va)^2)

summary(model_bic)

plot(md_allsubset, bl_allsubset, pch = 16, col = 1, xlab = "wielkosc modelu", ylab = "blad", type = "o", xlim = c(1,14))
points(md_lm, bl_lm, col = 2, pch = 8, type = "o")
points(md_aic, bl_aic, col = 3, pch = 17, type = "o")
points(md_bic, bl_bic, col = 4, pch = 18, type = "o")
legend("topright", c("all subset", "lm",  "stepAIC", "stepBIC"), col = 1:4, pch = c(16, 8, 17, 18))



### DMR
?DMR
model_dmr= DMR(X_tr, y_tr)
md_dmr = model_dmr$df
pr = predict(model_dmr, X_va)
pr = pr-y_va # odejmuje wektor od kazdej kolumny!!
bl_dmr = apply(pr, 2, function(x) mean(x^2))

plot(md_allsubset, bl_allsubset, pch = 16, col = 1, xlab = "wielkosc modelu", ylab = "blad", type = "o", xlim = c(1,14), ylim = c(0.0015,0.003))
lines(md_dmr, bl_dmr, col = 5, pch = 8, type = "o")
points(md_lm, bl_lm, col = 2, pch = 8, type = "o")
points(md_aic, bl_aic, col = 3, pch = 17, type = "o")
points(md_bic, bl_bic, col = 4, pch = 18, type = "o")
legend("topright", c("all subset", "lm" , "stepAIC", "stepBIC", "DMR"), col = 1:5, pch = c(16, 8, 17, 18, 8))

model_dmr$beta[, which.min(bl_dmr)]
Varsy[[which.min(bl_allsubset)]]

### Lasso
?glmnet
model_lasso = glmnet(X_tr, y_tr, alpha = 1)  
model_lasso$lambda # sam wylicza wielkosc regularyzacji, moc sciagania bet
plot(model_lasso)
md_lasso = model_lasso$df
pr = predict(model_lasso, as.matrix(X_va))
pr = pr-y_va
bl_lasso = apply(pr, 2, function(x) mean(x^2))

plot(md_allsubset, bl_allsubset, pch = 16, col = 1, xlab = "wielkosc modelu", ylab = "blad", type = "o", xlim = c(1,14), ylim = c(0.0015, .005))
lines(md_dmr, bl_dmr, col = 5, pch = 8, type = "o")
lines(md_lasso, bl_lasso, col = 6, pch = 16, type = "o")
points(md_lm, bl_lm, col = 2, pch = 8, type = "o")
points(md_aic, bl_aic, col = 3, pch = 17, type = "o")
points(md_bic, bl_bic, col = 4, pch = 18, type = "o")
legend("topright", c("all subset", "lm" , "stepAIC", "stepBIC", "DMR", "lasso"), col = 1:6, pch = c(16, 8, 17, 18, 8, 16))

md_lasso[which.min(bl_lasso)]
# min(bl_lasso)
# md_lasso[which.min(bl_lasso)]
# min(bl_dmr)
# md_dmr[which.min(bl_dmr)]

### sparsenet
?sparsenet
model_sparsenet = sparsenet(x = as.matrix(X_tr), y = y_tr)
md_sparsenet <- bl_sparsenet <- matrix(NA, nrow = length(model_sparsenet$gamma), ncol = length(model_sparsenet$lambda))
for (i in 1: length(model_sparsenet$gamma)){
  md_sparsenet[i,] = model_sparsenet$coeff[[i]]$df
  pr = predict(model_sparsenet, as.matrix(X_va), which.gamma = i)
  pr = pr-y_va
  bl_sparsenet[i,] = apply(pr, 2, function(x) mean(x^2))
}

cols <- brewer.pal(9, "OrRd")

plot(md_sparsenet[1,], bl_sparsenet[1,], pch = 16, col = cols[1], xlab = "wielkosc modelu", ylab = "blad", type = "o", xlim = c(1,14), ylim = c(0.0015, .005))
for (i in 2: nrow(md_sparsenet)){
  lines(md_sparsenet[i,], bl_sparsenet[i,], col = cols[i], pch = 16, type = "o")  
}
lines(md_lasso, bl_lasso, col = 6, pch = 16, type = "o")
lines(md_allsubset, bl_allsubset, pch = 16, col = 1, type = "o")


plot(md_allsubset, bl_allsubset, pch = 16, col = 1, xlab = "wielkosc modelu", ylab = "blad", type = "o", xlim = c(1,14), ylim = c(0.0015, .005))
lines(md_dmr, bl_dmr, col = 5, pch = 8, type = "o")
lines(md_lasso, bl_lasso, col = 6, pch = 16, type = "o")
points(md_lm, bl_lm, col = 2, pch = 8, type = "o")
points(md_aic, bl_aic, col = 3, pch = 17, type = "o")
points(md_bic, bl_bic, col = 4, pch = 18, type = "o")
lines(md_sparsenet[9,], bl_sparsenet[9,], pch = 16, col = cols[9], type = "o")
legend("topright", c("all subset" ,"lm", "stepAIC", "stepBIC", "DMR", "lasso", "sparsenet"), col = c(1:6, cols[9]), pch = c(16, 8, 17, 18, 8, 16, 16))

min(bl_dmr)
min(bl_sparsenet[9,])
### zmienne w sparsenet
model_sparsenet$coeff[[9]]$beta[, which.min(bl_sparsenet[9,])]

# i_lam = which.min(bl_sparsenet[9,]) # 15
# 
# ###


Varsy[[which.min(bl_allsubset)]]
model_fin = lm(bodyfat ~ weight + abdomen + wrist, data = BF_tr_va)
pred_fin = predict(model_fin, BF_te)
mean((y_te - pred_fin)^2)

## puscic calosc z innym seedem 

## KROSWALIDACJA!!!
blocks = createFolds(y_train, k = 10)
