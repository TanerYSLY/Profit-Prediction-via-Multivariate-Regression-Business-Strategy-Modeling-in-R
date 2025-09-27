setwd("C:/Users/taner/OneDrive/Masa\u00fcst\u00fc/projeler/regresyon/odev")
data <- read.table("data.txt", header = TRUE)

print(paste("Are there any missing values?", any(is.na(data))))

max_values <- apply(data[, !names(data) %in% "x4"], 2, max, na.rm = TRUE)
min_values <- apply(data[, !names(data) %in% "x4"], 2, min, na.rm = TRUE)

min_max_values <- rbind(Minimum = min_values, Maximum = max_values)


#2.Descriptive statistics
summary(data[, !names(data) %in% "x4"]) #x4 is categorical variable


counts <- table(data$x4)
labels <- paste0(names(counts), "\n(", counts, ")")
pie(counts,
    labels = labels,
    main = "Dist. of x4",
    col = c("lightblue", "lightgreen", "lightcoral"),
    cex = 0.8)

#percentage
print(round(prop.table(table(data$x4)) * 100, 2))


#normality assumption

#Check with qqplot
qqnorm(data$y)
qqline(data$y) 

#Lilliefors-corrected Kolmogorov???Smirnov test
library(nortest) #Install required libraries.
lillie_test_result <- lillie.test(data$y)
print(lillie_test_result) #The null hypothesis cannot be rejected; the data appears to follow a normal distribution

# Distribution of Dependent variable
hist(data$y, 
     main = "Distribution of Dependent variable",
     xlab = "y", 
     ylab = "Frekans",
     col = "skyblue",
     border = "white",
     breaks = 30)

# To detect extreme values, we applied the IQR technique, a robust method suitable for non-normally distributed data.
Q1 <- quantile(data$y, 0.25) 
Q3 <- quantile(data$y, 0.75) 
IQR_value <- Q3 - Q1      
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# New data without outliers.
data <- subset(data, y >= lower_bound & y <= upper_bound)
rownames(data) <- NULL

#normality assumption

qqnorm(data$y)
qqline(data$y) 

#Lilliefors-corrected Kolmogorov???Smirnov test
lillie_test_result <- lillie.test(data$y)
print(lillie_test_result) #The null hypothesis cannot be rejected; the data appears to follow a normal distribution

# Distribution of Dependent variable
hist(data$y, 
     main = "# Distribution of Dependent variable",
     xlab = "y", 
     ylab = "Frekans",
     col = "darkgreen",
     border = "white",
     breaks = 30)


#Linearity between countinious variables
pairs(data[, !names(data) %in% "x4"])
cor(data[, !names(data) %in% "x4"])


#Multivariate Regression model.

data$x4 <- as.factor(data$x4)

model <- lm(y ~ x1 + x2 + x3 + x4,data=data)

#Function 
aykiri_degerler_tespit <- function(model) {
  # Gerekli fonksiyonlar genellikle 'stats' paketinden gelir ve R ile otomatik y??klenir.
  # stats:: ??n ekini kullanmak, fonksiyonlar??n do??ru paketten geldi??inden emin olmay?? sa??lar.
  
  # n: G??zlem say??s??
  n_obs <- stats::nobs(model)
  if (is.null(n_obs) || n_obs == 0) {
    stop("Modelden ge??erli bir g??zlem say??s?? (n) al??namad?? veya g??zlem yok.")
  }
  
  # k: Ba????ms??z de??i??ken say??s?? (sabit hari??)
  if (inherits(model, "lm")) {
    k_predictors <- length(attr(stats::terms(model), "term.labels"))
  } else {
    k_predictors <- length(stats::coef(model)) - 1
    warning("Model 'lm' tipinde de??il. k_predictors, katsay?? say??s??ndan (intercept d??????lerek) tahmin edildi. L??tfen do??rulu??unu kontrol edin.")
  }
  if (k_predictors < 0) k_predictors <- 0 
  
  # Model tan??lamalar?? i??in temel hesaplamalar
  inf <- stats::ls.diag(model) 
  
  # 1. Standartla??t??r??lm???? art??klar ile Ayk??r?? De??erler
  standartlastirilmis_aykiri <- which(inf$std.res > 2 | inf$std.res < -2)
  names(standartlastirilmis_aykiri) <- NULL 
  
  # 2. Student T??r?? art??klar ile Ayk??r?? De??erler
  studentized_residuals <- stats::rstudent(model)
  student_turu_aykiri <- which(studentized_residuals > 3 | studentized_residuals < -3)
  names(student_turu_aykiri) <- NULL
  
  # 3. G??zlem Uzakl?????? (Leverage/Hat values) -> U?? G??zlemler
  hat_values <- inf$hat 
  leverage_threshold <- 2 * (k_predictors + 1) / n_obs
  
  plot(hat_values, pch = "*", cex = 0.7, main = "Leverage Value by Hat value", 
       ylab = "Hat De??eri", xlab = "G??zlem ??ndeksi")
  abline(h = leverage_threshold, col = "red")
  
  labels_hat <- ifelse(hat_values > leverage_threshold, 
                       if(!is.null(names(hat_values))) names(hat_values) else 1:length(hat_values), 
                       "")
  text(x = 1:length(hat_values), y = hat_values, labels = labels_hat, col = "red", cex = 0.8, pos = 4)
  
  ucgozlemler <- which(hat_values > leverage_threshold)
  names(ucgozlemler) <- NULL
  
  # 4. Cook's distance -> Etkili G??zlemler
  cooksd <- stats::cooks.distance(model)
  df_residual <- n_obs - k_predictors - 1 
  cooks_threshold <- if (n_obs > 50) {
    4 / n_obs
  } else {
    if (df_residual > 0) 4 / df_residual else 1 
  }
  
  plot(cooksd, pch = "*", cex = 1, main = "Influential Obs by Cooks distance",
       ylab = "Cook's Distance", xlab = "G??zlem ??ndeksi")
  abline(h = cooks_threshold, col = "red")
  
  labels_cooks <- ifelse(cooksd > cooks_threshold, 
                         if(!is.null(names(cooksd))) names(cooksd) else 1:length(cooksd), 
                         "")
  text(x = 1:length(cooksd), y = cooksd, labels = labels_cooks, col = "red", cex = 0.8, pos = 4)
  
  etkili_degerler <- which(cooksd > cooks_threshold)
  names(etkili_degerler) <- NULL
  
  # T??m ????pheli g??zlem indekslerinin birle??imi (benzersiz ve s??ral??)
  vektor_listesi_supheli <- list(
    standartlastirilmis_aykiri, 
    student_turu_aykiri,      
    ucgozlemler,
    etkili_degerler
  )
  
  vektor_listesi_dolu <- Filter(function(x) length(x) > 0, vektor_listesi_supheli)
  
  cikarilacak_indexler_birlesim <- if (length(vektor_listesi_dolu) > 0) {
    sort(unique(Reduce(union, vektor_listesi_dolu)))
  } else {
    integer(0)
  }
  
  # ????kt??y?? liste olarak d??nd??r
  return(list(
    standartlastirilmis_aykiri = standartlastirilmis_aykiri,
    student_turu_aykiri = student_turu_aykiri,
    uc_gozlemler = ucgozlemler,
    etkili_degerler = etkili_degerler,
    tum_supheli_gozlemler_birlesim = cikarilacak_indexler_birlesim
  ))
}

data_artik_incelemesi <- aykiri_degerler_tespit(model)
print(data_artik_incelemesi)

#t??m ayk??r?? de??erlerin indexlerini alal??m
tum_supheli_gozlemler_birlesim <- data_artik_incelemesi$tum_supheli_gozlemler_birlesim
print(tum_supheli_gozlemler_birlesim)

#ayk??r??lar?? medyanla de??i??tirelim
data_yeni <- data 
# 2. Ayk??r?? indekslerin ge??erlili??ini kontrol et (pozitif ve data_yeni s??n??rlar?? i??inde)
if (exists("data_yeni") && exists("tum_supheli_gozlemler_birlesim")) {
  
  gecerli_aykiri_indeksler <- tum_supheli_gozlemler_birlesim[
    tum_supheli_gozlemler_birlesim > 0 & 
      tum_supheli_gozlemler_birlesim <= nrow(data_yeni)
  ]
  
  # 3. E??er ge??erli ayk??r?? indeks varsa i??leme devam et
  if (length(gecerli_aykiri_indeksler) > 0) {
    
    # 4. Veri ??er??evesindeki her bir s??tun i??in d??ng??
    for (sutun_idx in 1:ncol(data_yeni)) {
      
      # Mevcut s??tunun ad??n?? al
      sutun_adi_mevcut <- names(data_yeni)[sutun_idx]
      
      # 5. S??tunun say??sal olup olmad??????n?? VE "x4" OLMADI??INI kontrol et
      if (is.numeric(data_yeni[[sutun_idx]]) && sutun_adi_mevcut != "x4") {
        
        # 6. S??tunun medyan??n?? hesapla (NA de??erlerini g??z ard?? ederek)
        #    Medyan, o anki s??tun de??erleri ??zerinden hesaplan??r.
        sutun_medyani <- median(data_yeni[[sutun_idx]], na.rm = TRUE)
        
        # 7. Belirtilen ge??erli ayk??r?? sat??rlardaki bu s??tunun de??erini medyan ile de??i??tir
        data_yeni[gecerli_aykiri_indeksler, sutun_idx] <- sutun_medyani
        
        # print(paste("'", sutun_adi_mevcut, "' s??tunundaki ayk??r?? de??erler medyan (", round(sutun_medyani, 2), ") ile g??ncellendi.", sep=""))
      }
    }
    
    print(paste(length(gecerli_aykiri_indeksler), "sat??rdaki uygun say??sal s??tunlar??n (x4 s??tunu hari??) de??erleri, kendi s??tun medyanlar?? ile g??ncellendi."))
    
  } else {
    print("Sa??lanan ayk??r?? indeks listesi bo?? veya `data_yeni` i??in ge??erli indeks i??ermiyor. Veri ??er??evesinde de??i??iklik yap??lmad??.")
  }
  
} else {
  print("`data_yeni` veya `tum_supheli_gozlemler_birlesim` R ortam??nda bulunamad??. L??tfen tan??ml?? olduklar??ndan emin olun.")
}

#5. modelin kestirim denklemini yaz??n??z (std. hatalar?? ile), model anlaml??l??????n?? test ediniz. 
sonuc <- lm(y ~ x1 + x2 + x3 + x4, data = data_yeni)
summary(sonuc) 
#Regresyon katsay??lar?? i??in %99 g??ven aral??klar??n?? bulunuz ve yorumlay??n??z
confint(sonuc, level = 0.99)

inf = ls.diag(sonuc)
#De??i??en varyansl??l??k kontrol??

#grafiksel y??ntem
plot(predict (sonuc), inf$stud.res, ylab="Studentized Residuals", xlab="Predicted Value")
#rasgele da????l??yor de??i??en varyansl??l??k yok

summary(lm(abs(residuals(sonuc)) ~ fitted(sonuc)))  
# H0 model anlaml?? de??ildir.   p-de??eri > 0.05  H0 red edilemez model anlaml?? de??ildir.
# o y??zden de??i??en varyansl??l??k yok

#Test y??ntemiyle
library(lmtest)
bptest(sonuc) # Breusch-Pagan test
#H0 varyanslar homojendir p-de??eri > 0.05 H0 red edilemez varyanslar homojen
#de??i??en varyansl??l??k yoktur.


# ??z ili??ki sorununu inceleyiniz
plot(data_yeni$y,inf$stud.res, xlab="G??zlem ??ndeksi", ylab="Studentized Residuals")
plot(inf$stud.res[1:length(inf$stud.res)-1],inf$stud.res[2:length(inf$stud.res)],xlab = "i-1. student art??k",ylab="i. student art??k")
#yukar??daki grafik ile hatalar??n ili??kili oldu??u g??z??k??yor.


dwtest(sonuc) # Durbin-Watson testi
# DW = 0.25   H0 = ??zili??ki yok(p = 0)  p-de??eri < 0.05 H0 red pozitif ??zili??ki var

# epsilon(t) = p*epsilon(t-1) + u(t) do??rusal denklemi vard??r hatalar aras??nda
# dwtest sonucu p>0 ????kt??  -> p otokorelasyon parametresi

#11. ??oklu ba??lant?? sorununu incelemesi

library(car)
vif(sonuc) 
#VIF de??erleri hepsi 10 dan k??????k ??oklu ba??lant?? sorunu yok


#12 uyum kestirimi
secilen_gozlem <- data_yeni[10, ]
secilen_gozlem
# Tahmin edilen kar miktar??n?? bulma
uyum_degeri <- predict(sonuc, newdata = secilen_gozlem)
uyum_degeri

#13 ??n kestirim
library(dplyr)

# Yeni g??zlemi olu??tururken veri k??mesindeki x4 ile ayn?? d??zeyleri kullan:
yeni_gozlem_df <- data.frame(
  x1 = 6.0,
  x2 = 3.5,
  x3 = 4.0,
  x4 = factor("2", levels = levels(data_yeni$x4))  # Ayn?? d??zeyler!
)

# ??imdi filtreleme g??venli ??ekilde yap??labilir:
eslesen_satirlar <- data_yeni %>%
  filter(
    x1 == yeni_gozlem_df$x1,
    x2 == yeni_gozlem_df$x2,
    x3 == yeni_gozlem_df$x3,
    x4 == yeni_gozlem_df$x4
  )

# Sonu?? kontrol??
if (nrow(eslesen_satirlar) > 0) {
  print("Bu g??zlem veri k??mesinde ZATEN VAR.")
} else {
  print("Bu g??zlem veri k??mesinde YOK, yeni bir g??zlemdir.")
}
on_kestirim_degeri <- predict(sonuc, newdata = yeni_gozlem_df)
on_kestirim_degeri

#uyum kestirimi g??ven aral??????
uyum_kestirimi_guven <- predict(sonuc, newdata = secilen_gozlem, interval = "confidence",level= 0.95)
uyum_kestirimi_guven
on_kestirim_guven <- predict(sonuc, newdata = yeni_gozlem_df, interval = "prediction",level= 0.95)
on_kestirim_guven

## ??leriye Do??ru Se??im (Forward selection)
library(stats)
lm.null <- lm(y ~ 1 , data = data_yeni)
forward <- step(lm.null,y ~ x1 + x2 + x3 + x4, data = data_yeni,  direction = "forward")
forward
summary(forward)

## Geriye Do??ru Se??im (Backward elimination)
backward<-step(sonuc,direction="backward")
summary(backward)

## Ad??msal Se??im Y??ntemi (Stepwise elimination) 
library(MASS)
step.model <- stepAIC(sonuc, direction = "both", trace = FALSE)
summary(step.model)


#Ridge Regresyon 
library(MASS)
ridge <- lm.ridge(sonuc ,lambda = seq(0,1,0.05))
# Matplotlib ile grafi??i ??izecek kod
matplot(
  ridge$lambda,          # X ekseni: Lambda de??erleri
  t(ridge$coef),         # Y ekseni: Katsay??lar (beta)
  type = "l",            # ??izgi tipi: Lineer
  xlab = expression(lambda),  # X eksen etiketi: Lambda
  ylab = expression(hat(beta)), # Y eksen etiketi: Beta katsay??lar??
  col = rainbow(ncol(ridge$coef))  # Renkler: Farkl?? ba????ms??z de??i??kenler i??in farkl?? renkler
)
#x eksenine ridge deki lambdalar?? al. y i??in transpoz(t) ridge$coef al.
# Legend eklemek i??in:
legend(
  "topright",           # Konum: Grafik sa?? ??st k????esi
  legend = names(coef(sonuc)),  # Legend etiketleri: Ba????ms??z de??i??ken isimleri
  lty = 1,              # ??izgi tipleri: T??m?? d??z ??izgi
  col = rainbow(ncol(ridge$coef)),   # Renkler: Ayn?? matplot'taki renkler
  bty = "n"             # S??n??rlay??c?? kutu: Yok
)

abline(h=0,lwd=2)
ridge$coef


select(ridge)
#hangi lambda de??erini kullan??ca????m??za dair tahminler veriyor


ridge$coef[,ridge$lam == 0.05]





