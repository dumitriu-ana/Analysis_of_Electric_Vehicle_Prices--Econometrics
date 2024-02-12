
rm(list = ls()) 
directory <- "C:/Users/anadu/Desktop/a3/Econometrie/p/PROIECT/"

install.packages('mltools', dependencies = TRUE, repos='http://cran.rstudio.com/')
install.packages('MLmetrics', dependencies = TRUE, repos='http://cran.rstudio.com/')
install.packages('strucchange', dependencies = TRUE, repos='http://cran.rstudio.com/')
# Incarcarea librariilor
library(tidyverse)
library(caret)
library(mltools)
library(MLmetrics)
library(strucchange)

PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", 
                  "olsrr", "moments","whitestrap","ggplot2", "tseries", "caret", "DataCombine","car", "glmnet")      # Instalare si activare pachete
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}
if(!require(tseries)){install.packages('tseries')}
if(!require('caret')) {
  install.packages('caret')
  library('caret')
}
if(!require('Boruta')) {
  install.packages('Boruta')
  library('Boruta')
}

data(economics)           # Incarcam setul de date economics

    # Model REGRESIE SIMPLA
    # var dependenta = pretul    #var independente = toate celelalte var din model care pot explica variatia pretului
    # testam modelul de regresie simpla cu fiecare variabila dependenata si continuam cu variabila dependenta semnificativa
    # care explica cu un procent cat mai mare variabilitatea variabilei dependente - pretul (Bonutatea, R-patrat)
    # Afisam setul de date cu privire la pretul masinilor

price1 <- read.csv(paste0(directory, "Date_Masini.csv"))           #price1 contine setul de date

price1 %>% 
  select(PriceEuro, Brand, Model, Accel, TopSpeed, Range, Efficiency, FastCharge, PowerTrain, PlugType, BodyStyle, Segment, Seats,
         DummyFC, DummyFWD, DummyPlugType, DummyBodyStyle, DummySegmentB, DummySeats5) %>% 
  head(10)       #afisam primele 10 observatii din model

price1 %>% 
  select(PriceEuro, Brand, Model, Accel, TopSpeed, Range, Efficiency, FastCharge, PowerTrain, PlugType, BodyStyle, Segment, Seats,
         DummyFC, DummyFWD, DummyPlugType, DummyBodyStyle, DummySegmentB, DummySeats5) %>% 
  str       # afisam primele 10 observatii din model pentru fiecare variabila in parte
            # Exemplu: $ Brand         : chr  "Tesla " "Volkswagen " "Polestar " "BMW " ...

price1 %>% 
  select(PriceEuro, Brand, Model, Accel, TopSpeed, Range, Efficiency, FastCharge, PowerTrain, PlugType, BodyStyle, Segment, Seats,
         DummyFC, DummyFWD, DummyPlugType, DummyBodyStyle, DummySegmentB, DummySeats5) %>% 
  stargazer(type = "text")   # afisam pentru fiecare variabila: numarul de observatii, media, abaterea standard
                             # minumul si maximul. EX: PriceEuro      103 55,811.560 34,134.670 20,129 215,000
                                                     # TopSpeed       103  179.194     43.573    123     410 

    # ------------------Regresia simpla: 
    # calculam regresia simpla folosind fiecare variabila independenta si continuam modelul 
    # cu regresia cu bonitatea maxima dintre cele care au variabila independenta cu coeficient semnificativ

rs_brand <- lm(PriceEuro ~ Brand, price1)    # Pretul (var. dependenta) in functie de Brand (var. independenta)
summary(rs_brand)                            # R patrat: 57% (Bonitatea modelului), Brand este var. calitativa

rs_mod <- lm(PriceEuro ~ Model, price1)      # Pretul in functie de modelul masinii, R-sq = 100%
summary(rs_mod)                              # Modelul este variabila calitativa, diferita pentru fiecare masina
                                             # Explica in proportie de 100% variabilitatea pretului

rs_acc <- lm(PriceEuro ~ Accel, price1)      # Pretul in functie de Acceleratie (in cate sec. ajunge masina la 100 km/h)
summary(rs_acc)                              # Acceleratia este semnificawtiva la 99%
                                             # Bonitatea modelului: 38% (variabilitatea pretului e explicata in proportie de 38% de acceleratie)
                                             # La o crestere cu o secunda a acceleratiei, pretul scade cu 7094.9 euro

 #-----------------------------TopSpeed-------------
rs_topspeed <- lm(PriceEuro ~ TopSpeed, price1)    #Pretul in functie de viteza maxima
summary(rs_topspeed)
   #coeficient semnificativ la 99%. La o crestere cu 1 unit a vitezei maxime (1 km/h), pretul creste cu 649.47 euro
   #=> continuam modelul folosind ca var indep TopSpeedww
   #R patrat: 68.7% (bonitatea modelului). Varaibila viteza maxima explica 68.7 din variabilitatea pretului.
 #-----------------------------------------------------------------------

rs_range <- lm(PriceEuro ~ Range, price1)   # Pretul in functie de autonomie; autonomia este semnificativa la 99%
summary(rs_range)                           # Bonitatea modelului: 45.5%.
                                            # La o crestere a autonomiei cu un km, pretul creste cu 182.8 euro

rs_eff <- lm(PriceEuro ~ Efficiency, price1)  # Pretul in functie de eficienta. Eficienta este semnificativa la 99%
summary(rs_eff)                               # Bonitatea modelului: 15.7%. 
                                              # La o crestere a eficientei cu o unitate, pewtul creste cu 458 euro

rs_fastch <- lm(PriceEuro ~ FastCharge, price1)  # Pretul in functie de tipul de incarcare rapida. 
summary(rs_fastch)                               # Bonitatea modelului: 85.23%

rs_plugt <- lm(PriceEuro ~ PlugType, price1)     # Pretul in functie de tipul de incarcator, nu este semnificativ 
summary(rs_plugt)                                # Bonitatea modelului: 1.2%

rs_body <- lm(PriceEuro ~ BodyStyle, price1)     # Pretul in functie de tipul de caroserie (variabila calitativa).
summary(rs_body)                                 # Bonitatea modelului: 42.73

rs_segment <- lm(PriceEuro ~ Segment, price1)    # Pretul in functie de tipul de segment (variabila calitativa).
summary(rs_segment)                              # Bonitatea modelului: 84.4%
                         #Pentru incadrarea in segmentul B pretul creste cu 12106 euro, pentru incadrarea in S creste cu 192306 euro

rs_seats <- lm(PriceEuro ~ Seats, price1)       # Pretul in functie de numarul de locuri
summary(rs_seats)                               # Coeficientul nu este semnificativ, R-patrat este 0.04%



#-----------------------------REGRESIE SIMPLA: Pretul in functie de viteza maxima---------------------
   # Variabila dependenta este Pretul, variabila independenta este viteza maxima

rs_topspeed <- lm(PriceEuro ~ TopSpeed, price1)   # Bonitatea modelului: 68.73%. Coeficientul este semnificativ la 99%

summary(rs_topspeed)                              # Viteza maxima explica 68.73% din variabilitatea pretului.
                                                  # La o crestere cu un km/h a vitezei maxime, pretul creste cu 649.47 euro

 
    # Ipoteza - Nr de observatii > nr variabile independente
nobs(rs_topspeed) > (rs_topspeed$rank - 1)       
        # Numarul de obs. este 103 (masinile din csv), numarul de variabile independente este 1 (TopSpeed)

    # Ipoteza - Variabilitatea in x este pozitiva
var(price1$TopSpeed)              # 1898.6 > 0

    # Ipoteza - Media reziduurilor este 0
mean(rs_topspeed$residuals)       # 6.531544e-13, foarte aproape de 0


#--------------------Testare ipoteze pe reziduuri
  # HOMOSCEDASTICITATEA este prezenta atunci cand varianta erorilor este constanta pentru fiecare x
  # Ipoteza - Reziduurile sunt homoscedastice
  # Ipoteze:  H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
            # H1: erorile sunt heteroscedastice 
bptest(rs_topspeed)      #  p-value =  0.16 > 0.1  => reziduuri homoscedastice
white_test(rs_topspeed)  #  p-value =  0.34 > 0.1  => reziduuri homoscedastice

  # Valorile previzionate si reziduurile 
price1 %<>% mutate(pricehat = fitted(rs_topspeed),
                  uhat = residuals(rs_topspeed))
coeftest(rs_topspeed, vcov. = vcovHC(rs_topspeed, type = "HC1"))# homo, nu corectam
ggplot(data = price1, mapping = aes(x = TopSpeed)) +
  theme_bw() +
  geom_point(mapping = aes(y = uhat)) +
  geom_hline(yintercept = 0, col = 'red') + # adaugarea dreptei orizontale
  ylab(label = "Residuals") # nume axa y

  # Ipoteza - Reziduurile nu sunt corelate cu variabilele independente
cor.test(price1$TopSpeed, rs_topspeed$residuals) # p-value > 0.1 => rezsiduurile nu sunt corelate cu variabila independenta

   # AUTOCORELATIE
    # Inspectarea autocorelarii cu ajutorul graficului ACF (autocorelare)
acf(rs_topspeed$residuals)
 # fiecare dreapta din grafic reprezinta laguri, deoarece lagurile nu depasesc intervalul punctat, nu avem autocorelare
 # Explicatie laguri: lag0 = valoarea prezenta, lag1 = valoarea de ieri, lag2 = valoarea de acum 2 zile


   # Testul Durbin-Watson (verifica autocorelarea pentru ordinul 1)
     # H0: reziduurile nu sunt autocorelate
     # H1: reziduurile sunt autocorelate
dwtest(rs_topspeed) # p-value= 0.59 > 0.1 => reziduurile nu sunt autocorelate

  # Testul Breusch-Godfrey (verifica autocorelarea pentru ordin superior)
     # H0: reziduurile nu sunt autocorelate
     # H1: reziduurile sunt autocorelate
bgtest(rs_topspeed)                    # p-value = 0.7 > 0.1 => reziduurile nu sunt corelate
bgtest(rs_topspeed, order = 2)         # p-value = 0.93 >0.1 => reziduurile nu sunt corelate pentru ordinul 2
bgtest(rs_topspeed, order = 3)         # p-value = 0.97 >0.1 => reziduurile nu sunt corelate pentru ordinul 3 

  # Ipoteza -  Reziduurile sunt normal distribuite
    # H0: distributie normala
    # Ha: distributie nenormala
model_speed<- lm(PriceEuro ~ TopSpeed, price1)
jarque.bera.test(model_speed$residuals)   # p-value = 2.2e-16 < 0.1 => reziduurile nu sunt normale distribuite 
ols_test_normality(model_speed)           # toate cele 4 teste au p-value aproape de 0 => rez nu sunt normal distribuite
   # Corectie: eliminam valorile extreme
ols_plot_cooksd_bar(model_speed)
ols_plot_cooksd_chart(model_speed)        # afiseaza punctele outlier (valori extreme) ce trebuie apoi eliminate

hprice_cook <- price1[-c(1,9,17,25,49,52,62,73,80,85), ]  # Eliminam punctele indentificate anterior cu distanta Cook
model_2 <- lm(PriceEuro ~  TopSpeed, hprice_cook)    # reestimam modelul folosinf setul noi de date (fara puncte extreme)
summary(model_2)                                     # Bonitatea modelului: 84.4%. 
ols_plot_cooksd_bar(model_2)                         # Afisam punctele extreme pentru noul model, dar nu corectam

hprice_cook %<>% mutate(uhat_cook = resid(model_2)) # extragem reziduurile din model 
# Testam cu Jarque-Bera normalitatea 
jarque.bera.test(hprice_cook$uhat) # p-value 0.01183 < 0.1 => reziduurile nu sunt normal distribuite, dar p-value este mai mare decat era inainte de corectie

#GRAFICE 
# Pas 1 - Graficul 'Residuals vs Fitted'
ols_plot_resid_fit(model_2)
# Pas 2 - Graficul Q-Q plot
ols_plot_resid_qq(model_2)
# Pas 3 - Histograma reziduurilor
ols_plot_resid_hist(model_2)
# Pas 4 - Boxplotul reziduurilor
ols_plot_resid_box(model_2)
# Pas 5 - Testarea cu ajutorul diferitelor test de normalitate
ols_test_normality(model_2) 




#-----------------------S7: PROGNOZE pentru regresia simpla-----------
    # Pentru a testa acuratetea prognozelor modelelor de regresie, de obicei se imparte setul de date intr-un 
    # set de antrenare (80%) si intr-un set de testare (20%). 
    # Se implementeaza regresia pe setul de antrenare si se testeaza acuratetea pe setul de testare.
set.seed(123)     #permite crearea variabilelor pe baza unor valori random (valorile random sunt aceleasi la fiecare rulare)

training.samples <- hprice_cook$PriceEuro %>%    
  createDataPartition(p = 0.8, list = FALSE)      # pentru setul de antrenare luam 80% din numarul de observatii
train.data  <- hprice_cook[training.samples, ]
test.data <- hprice_cook[-training.samples, ]     # pentru setul de testare se iau restul de obs. care nu au fost adaugate in setul de antrenare


model_2 <- lm(PriceEuro ~  TopSpeed, hprice_cook)   # Modelul anterior
summary(model_2)   

  # Predictia modelului pe setul de testare
y_pred <- predict(model_2, newdata = test.data)     
y_pred                                 #afiseaza valorile de pret prognozate pentru masinile din setul de testare

    # RMSE - Root Mean Squared Error arata cat de departe se incadreaza predictiile fata de valorile reale masurate
            # folosind distanta euclidiana. 
RMSE(y_pred, test.data$PriceEuro)   # 9248.291
MAE(y_pred, test.data$PriceEuro)    # 7491.364
mse(y_pred, test.data$PriceEuro)    # 85530879
MAPE(y_pred, test.data$PriceEuro)   # 0.1483784 < 1 predictia este buna


   # Cream un set de masini, completand viteza maxima
out_of_sample <- data.frame(TopSpeed = c(200,250,270))    # masina 1 are viteza maxima de 200 km/h

  # Realizam prognoza pentru cele 3 masini 
y_pred_outsample <- predict(model_2, newdata = out_of_sample)
y_pred_outsample              # 67076.92  98795.46 111482.88 -> preturile prognozate pentru cele 3 masini in functie de viteza maxima

  # Interval de incredere
new.speeds <- data.frame(TopSpeed = c(200, 250, 270, 280, 300, 320))   #cream un set de 6 valori care specifica vitezele pentru 6 masini
predict(model_2, newdata = new.speeds, se.fit=TRUE,interval="confidence", level=0.90)
  # pentru un interval de incredere de de 90% putem estima urmatoarele valori: 1  67076.92  65042.81  69111.03
  # pentru prima masini, avem limita inferioara de 65 042 euro, limita superioara de 69 111 euro si media de pret de 67 076 euro



#--------------------------------REGRESIE MULTIPLA-----------------------------------

price1 %>% 
  select(PriceEuro, Brand, Model, Accel, TopSpeed, Range, Efficiency, FastCharge, PowerTrain, PlugType, BodyStyle, Segment, Seats,
         DummyFC, DummyFWD, DummyPlugType, DummyBodyStyle, DummySegmentB, DummySeats5) %>% 
  stargazer(type = "text")     # Afisam datele 

#-----------Model de regresie multipla pentru pretul masinilor in functie de numarul de locrui (1- daca vem 5 locuri,0 altfel)
              # viteza maxima (km/h) si autonomia (km):     

model_0 <- lm(PriceEuro ~  DummySeats5+ 
                TopSpeed+ Range, price1)

summary(model_0)             #Bonitatea modelului 70.9%. Variabilele independente explica 70.9% din variabilitatea pretului.

   # PriceEuro = -49734.44 + (-9186.03) * DummySeats5 + 538.22 * TopSpeed + 45.55 * Range
   # Daca masina are fix 5 locuri (dummy = 1), atunci pretul masinii scade cu 9186.03 euro
   # La o crestere cu 1 km/h a vitezei maxime, pretul masinii creste cu 538.22 euro
   # La o crestere cu 1 km a autonomiei, pretul creste cu 45.55 euro

hprice1<-price1
hprice1 %<>% mutate(uhat = resid(model_0))      # extragem reziduurile din model

   # Ipoteza - Modelul este liniar in parametri, deoarece poate fi scris ca o functie liniara  
   # PriceEuro = -49734.44 + (-9186.03) * DummySeats5 + 538.22 * TopSpeed + 45.55 * Range

   # Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(model_0) > (model_0$rank - 1)  # nr obs. = 103, nr. var. independente = 3 (dummy, TopSpeed, Range) => true

   # Ipoteza - Variabilitatea in x este pozitiva
var(hprice1$DummySeats5)         # 0.2162574
var(hprice1$TopSpeed)            # 1898.609
var(hprice1$Range)               # 15879.64     => toate valorile > 0 => variabilitatea in x este pozitiva

   # Ipoteza - Media reziduurilor este 0
mean(model_0$residuals)          # medie = -8.227281e-13 (aproape de 0) => ipoteza acceptata

   # Ipoteza - Testare multicoliniaritate
     # verificam daca exista doua sau mai multe variabiled corelate => valori intre 1 si 5 (coliniaritate redusa),
     # intre 5 si 10 (coliniaritate moderata) si peste 10 avem coloniatiritate severa
vif(model_0)                     # 1.072013    2.416504    2.380761 => avem coliniaritate redusa (valori<5)

  # Ipoteza - Reziduurile nu sunt corelate cu variabilele independente
cor.test(hprice1$DummySeats5, model_0$residuals)    # p-value = 1 > 0.1 => nu sunt corelate
cor.test(hprice1$TopSpeed, model_0$residuals)       # p-value = 1 > 0.1 => nu sunt corelate
cor.test(hprice1$Range, model_0$residuals)          # p-value = 1 > 0.1 => nu sunt corelate

# Pas 1 - Graficul 'Residuals vs Fitted' 
plot(model_0)             # scriu 1 in consola ca sa afisez graficul, linie rosie=trend, 
                          # apoape de 0 -> distributie normala, puncte in afara liniei -> valori extreme

# Pasul 2 - Graficul 'Q-Q plot' - o diagrama Q-Q (sau diagrama cuantile-cuantile) 
plot(model_0)
ols_plot_resid_qq(model_0)   

# Pasul 3 - Histograma reziduurilor 
ols_plot_resid_hist(model_0)    #clopot in jurul lui 0, pare ok

#histograma rez 2
hprice1<-price1
hprice1 %<>% mutate(uhat = resid(model_0)) # extragem reziduurile din model
ggplot(data = hprice1) +
  theme_bw() +
  geom_histogram(mapping = aes(x = uhat), col = 'grey')+
  xlab('Reziduuri') + 
  ylab('Count') +
  ggtitle('Histograma reziduurilor') + 
  theme(plot.title = element_text(hjust = 0.5))

    # Asimetria (skewness)
skewness(hprice1$uhat)            #2.29 => valoare pozitiva => distributia e centrata la dreapta
    # Boltirea (kurtosis)
kurtosis(hprice1$uhat)            #12.99 >3 => distributie platicurtica
  # In urma testarii coeficintului de asimetrie si a boltirii putem trage concluzia ca distributia reziduurilor 
  # este asimetrica la dreapta si turtita => reziduurile nu sunt normal distribuite

  # Pasul 4 - Graficele de tip Boxplot
boxplot(model_0$residuals, main="Box Plot reziduuri")
ols_plot_resid_box(model_0)


   # Teste de heteroscedasticitate ---------------------------------
     # homoscedasticitatea este prezenta atunci cand varianta erorilor este constanta pentru fiecare x
   # Testul Breusch-Pagan si Testul White 
     # Ipoteze: H0: erorile sunt homoscedastice (reziduurile distribuite cu varianta egala)
              # H1: erorile sunt heteroscedastice
              
bptest(model_0)          # p-value = 0.079 > 0.05 => reziduurile homoschedastice la 95%
white_test(model_0)      # p-value = 0.059 > 0.05 => reziduurile homoschedastice la 95%

  # Testarea ipotezei de homoscedasticitate
     # Graficul reziduurilor fata de variabilele independente
ggplot(data = hprice_cook, mapping = aes(x = TopSpeed + Range + DummySeats5, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'TopSpeed Range DummySeats5')
 
   #Autocorelatie ------------------------
acf(model_0$residuals)        # nu sunt autocorelate, am o singura dreapta care iese din interval 
    # Testul Durbin-Watson (ordinul 1)
        # H0: reziduurile nu sunt autocorelate
        # H1: reziduurile sunt autocorelate
dwtest(model_0)              # p-value= 0.83 > 0.1 => reziduurile nu sunt autocorelate

   # Testul Breusch-Godfrey (ordin superior)
      # H0: reziduurile nu sunt autocorelate
      # H1: reziduurile sunt autocorelate
bgtest(model_0)                           # p-value = 0.28 >0.1 => reziduurile nu sunt corelate)
bgtest(model_0, order = 2)                # p-value = 0.56 >0.1 => reziduurile nu sunt corelate pentru ordinul 2)
bgtest(model_0, order = 3)                # p-value = 0.70 >0.1 => reziduurile nu sunt corelate pentru ordinul 3)

   # Testarea NORMALITATII----------------
      # Testul Shapiro Wilk pentru normalitate
         # H0: distributie normala
         # Ha: distributie nenormala
shapiro.test(hprice1$uhat)            # p-value = 1.554e-10 < 0.05 => reziduurile nu sunt normal distribuite
 
      # Testul Jarque-Bera pentru normalitate
         # H0: distributie normala
         # Ha: distributie nenormala
jarque.bera.test(hprice1$uhat)        # p-value = 2.2e-16< 0.05 => reziduurile nu sunt normal distribuite
         # Cele 4 teste
ols_test_normality(model_0)           # p-value aproape 0 < 0.05 => reziduurile nu sunt norm distribuite

   # Distanta Cook este folosita pentru a identifica punctele de date influente. 
ols_plot_cooksd_chart(model_0)      
 
hprice_cook <- hprice1[-c(9,17,25,34,49,62,73,80,85), ]    # Eliminam punctele deterinate, valori extreme
model_1 <- lm(PriceEuro ~  DummySeats5+ TopSpeed+ Range, hprice_cook)  #reestimam modelul cu valorile ramase
summary(model_1)                 # Bonitatea modelului = 88%
ols_plot_cooksd_bar(model_1)     # Retestam distanta Cook, obervam ca inca avem valori extreme, dar nu le eliminam

# Testam cu Jarque-Bera
jarque.bera.test(hprice_cook$uhat) # p-value = 0.04476 < 0.1 => reziduurile nu sunt normal distrib,
                                       # dar p-value a crescut in urma eliminarii unui set de valori extreme
      # GRAFICE
# Pas 1 - Graficul 'Residuals vs Fitted'
ols_plot_resid_fit(model_1)
# Pas 2 - Graficul Q-Q plot
ols_plot_resid_qq(model_1)
# Pas 3 - Histograma reziduurilor
ols_plot_resid_hist(model_1)
# Pas 4 - Boxplotul reziduurilor
ols_plot_resid_box(model_1)
# Pas 5 - Testarea cu ajutorul diferitelor test de normalitate
ols_test_normality(model_1) 


   # Regresie cu forma functionala LOG - LOG
lPriceEuro<-log(hprice_cook$PriceEuro)    # cream variabile logaritmand variabilele independente
lTopSpeed <- log(hprice_cook$TopSpeed)    # variabilele dummy, varsta, ani, etc nu se logaritmeaza
lRange <- log(hprice_cook$Range)

hprice1<-hprice_cook   #hprice1 va avea valoarea lui hp_cook (dupa eliminare, de la 103 obs ramanem cu 94)
model_log <- lm(lPriceEuro ~  DummySeats5+ lTopSpeed+ lRange, hprice1)    #r=85
summary(model_log)
  # Bonitatea modelului este 85%. Variabila dummy nu este semnificativa. Modificarile sunt procentuale.
  # La o crestere cu 1% a vitezei maxime, pretul creste cu 1.87%, la o crestere cu 1% a autonomiei, pretul creste cu 0.1%.

hprice1 %<>% mutate(uhat1 = resid(model_log))

  # Graficul reziduurilor fata de variabiabilele independente logaritmate, nu avem forma de palnie,  
ggplot(hprice1) + 
  theme_bw() + 
  geom_point(aes(x = DummySeats5+ TopSpeed+ Range, y = uhat1)) +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'TopSpeed Range DummySeats5')        # reziduurile sunt homoscedastice
   
  # Testul Breusch-Pagan ----
bptest(model_log)          # p-value 0,07 >0.05 => reziduuri homoscedastice

  # Testul White ----
white_test(model_log)      # p-value = 0.059 => reziduuri homoscedastice 

#-----------------------S7: PROGNOZE-----------
  # Pentru a testa acuratetea prognozelor modelelor de regresie, se imparte setul de date intr-un set de 
    # antrenare (80%) si un set de testare (20%). Se implementeaza regresia pe setulde antrenare si 
    # se testeaza acuratetea pe setul de testare.

set.seed(123)  #permite crearea variabilelor pe baza unor valori random (valorile random sunt aceleasi la fiecare rulare)

training.samples <- hprice1$PriceEuro %>%
  createDataPartition(p = 0.8, list = FALSE)   # pentru setul de antrenare luam 80% din numarul de observatii
train.data  <- hprice1[training.samples, ]
test.data <- hprice1[-training.samples, ]      # pentru setul de testare se iau restul de obs. care nu au fost adaugate in setul de antrenare

   # Model de regresie de tip lin-lin pentru setul de antrenare
model_a <- lm(PriceEuro ~ DummySeats5+ TopSpeed+ Range, data = train.data) 
summary(model_a)                               # Modelul anterior
  
   # Predictia modelului pe setul de testare
y_pred <- predict(model_a, newdata = test.data)
y_pred      #afiseaza valorile de pret prognozate pentru masinile din setul de testare

   # RMSE - Root Mean Squared Error => acest indicator de acuratete arata cat de departe se incadreaza
    # predictiile fata de valorile reale masurate folosind distanta euclidiana.  
RMSE(y_pred, test.data$PriceEuro)    # 11621.37
MAE(y_pred, test.data$PriceEuro)     # 8503.803
mse(y_pred, test.data$PriceEuro)     # 135056150
MAPE(y_pred, test.data$PriceEuro)    # 0.1563608 < 1, predictie buna

  # Cream un set de masini, completand dummy pentru numarul de locuri, viteza maxima si autonomia pentru 3 masini
out_of_sample <- data.frame(DummySeats5 = c(1,1,1),TopSpeed = c(200,250,270), Range = c(250,300,400))
  # Realizam prognoza pentru cele 3 masini 
y_pred_outsample <- predict(model_a, newdata = out_of_sample)
y_pred_outsample

  # Interval de incredere
new.specificatii <- data.frame(DummySeats5 = c(1,1,1,0,1,0),TopSpeed = c(200, 250, 270, 280, 300, 320), 
                         Range = c(250,300,400,500, 600, 450))
predict(model_a, newdata = new.specificatii, se.fit=TRUE,interval="confidence", level=0.90)
  # pentru un interval de incredere de de 90% putem estima urmatoarele valori (masina 2): 2  97244.11  89627.94 104860.28
  # pentru a doua masina, avem limita inferioara de 60 029 euro, limita superioara de 70 091 euro si media de pret de 65 060 euro



#------------------------- REGRESIA NELINIARA -------------------
model1 <- lm(PriceEuro ~ TopSpeed+ Range +DummySeats5, hprice1) #mod initial
summary(model1)
hprice1 %<>% mutate(yhat = fitted(model1), # reziduuri
                    yhatsq = yhat^2, # patratul reziduurilor
                    yhatcube = yhat^3) # cubul reziduurilor

   # Cream modelul reset in care adaugam si valorile pentru patratil si cubul reziduurilor
     # testam semnificatia comuna a coeficientilor yhatsq si yhatcube
model1_RESET <- update(model1, ~ . + yhatsq + yhatcube) # adaugam in modelul initial
summary(model1_RESET)                                   # Coeficientii nu sunt semnificativi, Bonitatea este 88.7%
 
 # Verificam daca modelul este bine specificat
      # H0: modelul este bine specificat 
      # H1: modelul nu este bine specificat
linearHypothesis(model1_RESET, c("yhatsq = 0", "yhatcube = 0"))
   # p-value = 0.3 > 0.1 => modelul este bine specificat

   # Model de regresie log-lin   (logaritmam pretul, variabilele independente raman liniare)
model2 <- lm(lPriceEuro ~ TopSpeed+ Range +DummySeats5, hprice1) 
summary(model2)
hprice1 %<>% mutate(lyhat = fitted(model2),
                    lyhatsq = lyhat^2,
                    lyhatcube = lyhat^3)
model2_RESET <- update(model2, ~ . + lyhatsq + lyhatcube, hprice1)
summary(model2_RESET)
linearHypothesis(model2_RESET, c("lyhatsq = 0", "lyhatcube = 0")) # p-value < 0.1=>
  # modelul nu este bine specificat 

  # Generam patratul variabilelor
hprice1 %<>% mutate(TopSpeed_sq = TopSpeed^2, Range_sq = Range^2, DummySeats5_sq = DummySeats5^2)

   # Modelul liniar de regresia al salariului care include si patratul variabilelor
model3 <- update(model1, ~ . + TopSpeed_sq + Range_sq + DummySeats5)
summary(model3)
hprice1 %<>% mutate(yhat1 = fitted(model3),
                    yhat1sq = yhat1^2,
                    yhat1cube = yhat1^3)
model3_RESET <- update(model3, ~ . + yhat1sq + yhat1cube) 
summary(model3_RESET)
linearHypothesis(model3_RESET, c("yhat1sq = 0", "yhat1cube = 0")) # p-value < 0.1=>
   # modelul nu este bine specificat

  # Testul Chow pentru variabila dependenta -  pret
library(strucchange)
    #nu avem evenimente in timp care sa determine modificarea pretului
sctest(hprice1$PriceEuro ~ hprice1$TopSpeed, type = "Chow", point = 10) # p-value = 0.22 > 0.1 
           #=> nu exista rupturi pentru variabila TopSpeed, nu trebuie sa avem doua modele de regresie
sctest(hprice1$PriceEuro ~ hprice1$Range, type = "Chow", point = 10) 
           # p-value = 0.89 > 0.1 => nu exista rupturi pentru variabila autonomie Range
sctest(hprice1$PriceEuro ~ hprice1$DummySeats5, type = "Chow", point = 10) 
           # p-value = 0.57 > 0.1 => nu exista rupturi


#------------------------9. REGRESIE CU VARIABILE indicatorii------------
   # Afisam numarul de obs.,    media,      abaterea standard, min   si max pentru PriceEuro
     #  PriceEuro     94      51,078.700     28,248.130        20,129    215,000
lm(PriceEuro ~ 1, hprice1) %>% summary
hprice1 %>% select(PriceEuro) %>% stargazer(type = "text")

   # Regresia pretului tinand cont de  FWD (dummy tractiune fata = 1, restul 0)
lm(PriceEuro ~ DummyFWD, hprice1) %>% summary     # Coeficient semnificativ la 99%, Bonitatea modelului = 21.83%
     # Daca masina are tractiune fata (dummy = 1), pretul este cu 27 004 euro mai mic

  # Graficul pretului si FWD
ggplot(hprice1, aes(x = DummyFWD, y = PriceEuro)) +
  theme_bw() +
  geom_point() +
  geom_smooth(aes(col = 'fitted line'), method = "lm", se = F)

  # Statistici descriptive pe GRUPURI
hprice1 %>% 
  select(PriceEuro) %>% # pastram 'priceEuro'
  split(hprice1$DummyFWD) %>%  # facem split de data.frame bazat pe var 'DummyFWD'
  walk(~ stargazer(., type = "text")) # summary stats pentru  fiecare grup
  # grup 1 (dummy 0): PriceEuro  58   61,420.830   31,486.420   21,387   215,000
  # grup 2 (dummy 1): PriceEuro  36   34,416.390    6,551.062   20,129    50,000

  # Testul T pentru diferenta pe medii a pretului dintre masinile cu tractiune fata si celelalte
t.test(formula = PriceEuro ~ DummyFWD, data = hprice1)    # p-value = 2.8e-08 < 0.05
  # Media pretului intre masinile cu tractiune fata si celelalte masini este semnificativ diferit la 99% deoarece p-value<0.01

   # Generam variabila indicator pentru masinile care nu au tractiune fata 
hprice1 %<>% mutate(notFWD = 1 - DummyFWD)

   # Diferenta dintre regresia cu masini fara FWD si masinile cu FWD
lm(PriceEuro ~ DummyFWD, hprice1) %>% summary      # coeficient semnificativ, Bonitate = 21.8%
lm(PriceEuro ~ notFWD, hprice1) %>% summary        # coeficient semnificativ, Bonitate = 21.8%
  # Coeficientul are aceeasi magnitudine si avem aceeasi p-value 
    # dar semn opus fata de regresia cu DummyFWD. Daca dummy-ul este 0, pretul este cu 27 004 euro mai ridicat,
    # daca dummy este 1, pretul este cu 27 004 mai mic

   # Regresia care cuprinde ca variabile independente masinile cu tractiune fata si pe cele care nu sunt 
     # tractiune fata nu poate fi realizata deoarece exista coliniaritate perfecta intre cele doua variabila 
     #(cand una este 0, stim sigur ca celalata este 1)
lm(PriceEuro ~ 0+ DummyFWD + notFWD, hprice1) %>% summary    #cu 0+ excludem constanta


#-----INTERACTIUNE CU DUMMY--------

   # consideram 1 daca masina este hatchback (HB)/compacta, 0 altfel  x1
   # consideram 1 daca masina are tractiune fata (FWD), 0 altfel      x2
hprice1 %<>% mutate(notBodyStyle = 1 - DummyBodyStyle)  

   # Categorii:HB*FWD, notHB*FWD, HB*notFWD (11, 01, 10)

hprice1 %<>% mutate(HB_FWD = DummyBodyStyle * DummyFWD, 
                    notHB_FWD = notBodyStyle * DummyFWD, 
                    HB_notFWD = DummyBodyStyle * notFWD)

    # Afisam lista variabilelor create pentru primele 10 observatii
hprice1 %>% 
  select(DummyBodyStyle, notBodyStyle, DummyFWD, notFWD, HB_FWD, notHB_FWD, HB_notFWD) %>%
  head(10)

# Regresia cu cele 3 categorii
lm(PriceEuro ~ HB_FWD + notHB_FWD + HB_notFWD, hprice1) %>% summary    
        # Coeficientii sunt semnificativi la 99%;  Bonitatea modelului = 38%
  
  # Regresia HB_FWD categorie de referinta
model_HB_FWD <- lm(PriceEuro ~  notHB_FWD + HB_notFWD, hprice1)
summary(model_HB_FWD)             # Bonitatea modelului = 14.66%
  # Daca masina nu este HB (0) si are tractiune fata (1), pretul este cu 22 972 mai mic
  # Daca masina este HB (1) si nu are tractiune fata (0), pretul este cu 21 254 mai mic

  # Var de interactiune Hb si FWD
hprice1 %<>% mutate(DummyBodyStyleXDummyFWD = DummyBodyStyle * DummyFWD)  
  # cream o variabila care reprezinta indeplinirea simultana a celor doua categorii (interactiune)
     #(1 - este simultan HB si are tractiune fata, 0 altfel)

  # Regresia cu termen de interactiune

model_interactiune_dummy <- lm(PriceEuro ~ DummyBodyStyle +
                  DummyFWD + DummyBodyStyleXDummyFWD, hprice1)

summary(model_interactiune_dummy)

  # Interpretare:  Efectul lui x1 (HB) in pret este 70232 (intercept) + 26681*x2. 
    # Daca x2 (tractiune fata) este 0, efectul lui x1 (HB) in pret este 70232 (intercept).
       # Pentru doua masini cu x2=0 (nu sunt tractiune fata), o masina Hatchback are pretul mai mic cu 
       # 31940 euro decat o masina cu alta caroserie.
    # Daca x2 = 1, efectul lui x1 este -31940 + 26681 = -5259 euro => 
  # Pentru doua masini cu tractiune fata, o masina HB are pretul cu 5259 euro mai mic decat o masina care nu e HB.

  # Regresia pretului cu TopSpeed si Body Style (dummy -  HB)
model_speed_HB <- lm(PriceEuro ~ TopSpeed + DummyBodyStyle, hprice1)
summary(model_speed_HB)
  # Coeficientul variabilei Dummy nu este semnificativ (0.29)   Bonitatea modelului = 88.4%



# ------ TESTUL F pt diferente intre grupuri ---------------------------------

  # F-test pentru semnificatia coeficientului comun este utilizat pentru a testa
  # mai multi coeficienti simultan sa fie semnificativi diferiti de zero 

  # Generam termeni de interactiune
hprice1 %<>% mutate(TopSpeedXHB = DummyBodyStyle * TopSpeed,
                    RangeXHB = DummyBodyStyle * Range,
                    AccelXHB = DummyBodyStyle * Accel)

# Model nerestrictionat: PriceEuro= alpha0 + alpha1*TopSpeed + alpha2*Range + alpha3*Accel + e
model_9 <- lm(PriceEuro ~ TopSpeed + Range + Accel, hprice1)
ssr_r <- sum(resid(model_9)^2)             # suma patratelor reziduurilor
summary(model_9)       # Coeficientii pentru Range si Accel nu sunt semnificativi
                       # Bonitatea modelului = 88.45%

model_10 <- lm(PriceEuro ~ TopSpeed + Range
               + Accel + TopSpeedXHB + 
                 RangeXHB + AccelXHB, hprice1)        # adaugam termenii de interctiune in model
summary(model_10)                                     # coeficientii nu sunt semnificativi
ssr_ur <- sum(resid(model_10)^2)      # suma patratelor reziduurilor
ssr_ur            # ssr pentru modelul 10 (cu termeni de interctiune):  8213467165
ssr_r             # ssr pentru modelul 9 (fara termeni de interctiune): 8573712728
q <- 4
   # Calculam F_stat 
     # df_resid sunt gradele de liberatate pentru reziduurile modelului nerestrictionat (n-k-1)
df_resid <- model_10$df.residual        # 87 grade de libertate
(F_stat <- ((ssr_r - ssr_ur)/q) / (ssr_ur/df_resid))         # F_stat = 0.9539627

  # F-critic
qf(0.95, q, df_resid)                                        # F-critic =  2.476494
   # Daca F-stat > F-critical respingem H0 si coeficientii sunt semnificativi simultan
      # Daca F-stat =0.95 < F-critic(2.47) => nu respingem ipoteza nula H0,
                                              # coeficientii nu sunt semnificativi simultan
  # p-value pt F-test
(F_pvalue <- pf(F_stat, q, df_resid, lower.tail = F))        # F-p-value = 0.4370166
   # Daca F p-value<0.1 respingem H0, coefficientii sunt semnificativi simultan
       # F-p-value (0.43) > 0.1, nu respingem H0, coeficientii nu sunt semnificativi simultan


   # Testul Chow pentru diferente intre grupuri ------------------------------
model_11 <- model_9
summary(model_11)
k <- 3
n <- nobs(model_11)
ssr_r <- sum(resid(model_11)^2)

  # Model nerestrictionat pt BodyStyle (HB): 
model_12 <- update(model_11, subset = (DummyBodyStyle == 1))  # modelul pentru subsetul de date cu masini HB
ssr1 <- sum(resid(model_12)^2)         # suma patrratelor reziduurilor =  482341710
ssr1
  # Model nerestrictionat pt notBodyStyle: 
model_13 <- update(model_11, subset = (DummyBodyStyle == 0)) # modelul-  subsetul cu masini care nu sunt HB
ssr2 <- sum(resid(model_13)^2)         # suma patrratelor reziduurilor =  7516531866
ssr2

  # Calculam Chow F-statistic
(F_stat <- ((ssr_r-(ssr1+ssr2))/(k+1)) / ((ssr1+ssr2)/(n-2*(k+1))))   # 1.545098

# Valoarea critica F
qf(0.99, k+1, n-2*(k+1))                                              # 3.545424
# Daca F-stat > F-critical (fals) nu respingem nula, coeficientii nu sunt semnificativi simultan

# p-value pt F-test
(F_pvalue <- pf(F_stat, k+1, n-2*(k+1), lower.tail = F))              # p-value = 0.1964012
# Daca F-p-value< 0.1 (fals) nu respingem nula, coeficientii nu sunt semnificativi simultan


#------------------10. Modele de regularizare. Alg de selectie a var

# Regresia Ridge - este un model care se foloseste cu precadere atunci cand  exista multicoliniaritate in date  care minimizeaza SSR
# SSR = sum((y-yfit)^2)    Regresia ridge incearca sa minimizeze SSR + lambda*sum(beta^2)
# lambda*sum(beta^2) se mai numeste si shrinkage penalty 

model_reg <- lm(PriceEuro ~  DummySeats5+ TopSpeed+ Range, hprice1)
summary(model_reg) #Bonitatea = 88.46%, coeficientii pentru numerul de locuri si range nu sunt semmnificativi, ceilalti sunt semnificativi la 99%
  # completam datele variabilelor independente pentru o masina 

prognoza <- data.frame(DummySeats5 = c(1),
                       TopSpeed = c(200), Range = c(300))

y_pred_scenariu <- predict(model_log, newdata = prognoza)
y_pred_scenariu           
y <- hprice1$PriceEuro    # definim variabila raspuns 

   # Definim predictorii
x <- data.matrix(hprice1[, c('DummySeats5', 'TopSpeed', 'Range')])   #preluam variabilele independente pe care le folosim
   # Estimam modelul ridge (alpha = 0)
model <- glmnet(x, y, alpha = 0)
summary(model)
   # Identificam valoarea lui lambda pt care avem MSE minimizat utilizand validarea incrucisata (cross validation)
cv_model <- cv.glmnet(x, y, alpha = 0)
summary(cv_model)
best_lambda <- cv_model$lambda.min
best_lambda                     # 2640.67

   # testarea valorii lamda 
plot(cv_model) 
   # Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 
   # Interpretare: Daca masina are fix 5 locuri, pretul ei este cu 3859 euro mai mic
                 # La cresterea cu 1 km/h a vitezei maxime, pretul creste cu 506 euro
                 # La cresterea cu 1 km a autonimiei, pretul creste cu 35.65 euro

    # Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au modificat ca urmare a 
       # cresterii valorii lui lambda
plot(model, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)
    # Interpretare: Ca urmare a crersterii valorii lui lambda, coeficientului lui dummy a crescut, 
       # coeficientul variabilei TopSpeed a scazut si coeficientul variabilei Range s-a mentinut

# Prognoze 

y_predicted <- predict(model, 
              s = best_lambda, newx = x)

# Progoza pentru masina cu 5 locuri, viteza maxima 210, autonomie 400 (3 coloane completate)
new <- matrix(c(1, 210,400 ), nrow=1, ncol=3)    

predict(best_model, s = best_lambda,
        newx = new)  

# Pretul previzionat este 70 519 euro

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq                         # Bonitatea modelului: 87.3%


# ----------Regresia LASSO - functioneaza similar cu regresie Ridge doar ca incearca sa minimizeze
# --------------SSR + lambda*sum(|beta|)
model <- glmnet(x, y, alpha = 1)
# Din punct de vedere tehnic, vom seta valoarea alpha = 1 pentru regresia LASSO. 
cv_model <- cv.glmnet(x, y, alpha = 1)

   # Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda                    # 131.4271

   # testarea valorii lamda
plot(cv_model) 

   # Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)                     # coeficientii variabilelor  
    # daca unul din coeficienti este 0 inseamna ca acea variabila nu este importanta si modelul nu o estimeaza
       # Coeficientul variabilei Range nu este calculat pentru ca variabila nu este importanta

  # Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au modificat ca urmare a
     # cresterii valorii lui lambda
plot(model, xvar = "lambda",label=T)
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)
  # Interpretare: Ca urmare a cresterii valorii lui lambda, coefientul variabilei dummy a crescut, 
       # coeficientul variabilei TopSpeed a scazut si coeficientul variabilei Range este constant, 0.

  # Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)

  # Progoza pentru masina cu 5 locuri, viteza maxima 210, autonomie 400 (3 coloane completate)
new <- matrix(c(1, 210, 400), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new)  # Pretul estimat este de 72 343 euro

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq                    # Bonitatea modelului = 88.45%


# ---Elastic net regression - functioneaza similar cu Ridge si LASSO doar ca  adauga ambele penalitati
# -----SSR + lambda*sum(beta^2) + lambda*sum(|beta|)
model <- cv.glmnet(x, y, alpha = 0.5)
cv_model <- cv.glmnet(x, y, alpha = 0.5)
# Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda # 198.83

  # testarea valorii lamda
plot(cv_model) 

  # Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model)        # coeficientii variabilelor sunt aproape 0

  # Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)

  # Progoza pentru masina cu 5 locuri, viteza maxima 210, autonomie 400 (3 coloane completate)
new <- matrix(c(1, 210, 400), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new)   # Pretul estimat este 72 741.27 euro

  # calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq                              # Bonitatea modelului =  88.45%

# Vom compara valorile lui rsq si in functie de acestea vom alege modelul cu cea
# mai mare bonitate drept modelul optim 

# Algoritmul BORUTA 

# Vom converti variabilele categoricale in variabile factor 
#convert <- c(5:10)
#hprice1[,convert] <- data.frame(apply(hprice1[convert], 2, as.factor))

library(Boruta)
set.seed(111)

boruta.bank_train <- Boruta(PriceEuro~.,
                        data = hprice1, doTrace = 2)

print(boruta.bank_train)


# Vom selecta atributele importante 

getSelectedAttributes(boruta.bank_train, withTentative = T)

#[1] "Brand"       "Accel"       "TopSpeed"    "Range"       "Efficiency"  "PowerTrain"  "Segment"    
#[8] "Seats"       "pricehat"    "uhat"        "uhat1"       "yhat"        "yhatsq"      "yhatcube"   
#[15] "lyhat"       "lyhatsq"     "lyhatcube"   "TopSpeed_sq" "Range_sq"    "yhat1"       "yhat1sq"    
#[22] "yhat1cube"   "lyhat1"      "lyhat1sq"    "lyhat1cube"  "notHB_FWD"   "AccelXHB"

  # Modelul Boruta in functie de toate variabilele calitative si cantitative preluate din csv
      #Brandul, PowerTrain, Segment sunt variabile calitative

model_Boruta1 <- lm(lPriceEuro ~  Brand + Accel+ TopSpeed + Range+ Efficiency 
                    + PowerTrain + Segment + Seats, hprice1)    # R-squared = 98.82%

summary(model_Boruta1)     

   # Modelul Boruta in functie de variabilele numerice preluate din csv

model_Boruta2 <- lm(PriceEuro ~ Accel + TopSpeed + 
                      Efficiency + Range, hprice1) #R-squared=90.76%. 

summary(model_Boruta2)  

   #Modelul este semnificativ la 99% (p-value: < 2.2e-16)
   # Coeficientul pentru acceleratie este semnificativ la 90%, coeficientul pentru autonomie este semnificativ la 99%
   # Ceilalti coeficienti sunt semnificativi la 99%.
   # Forma functionala: PriceEuro = -102000 + 8480 * Accel + 642.7* TopSpeed + 182.5 * Efficiency - 0.7152 * Range
      # La o crestere cu 1 km/h a vitezei maxime, pretul creste cu 642.7 euro
      # La o crestere cu 1 km a autonomiei, pretul scade cu 0.7 euro
  
   # Modelul Boruta in functie de variabilele numerice si variabilele Dummy create pe baza variabilelor calitative importante 

model_Boruta3 <- lm(PriceEuro ~ Accel + TopSpeed + Efficiency + Range + 
                 DummySegmentB + DummySeats5, hprice1) # Bonitatea modelului = 91.17%.

summary(model_Boruta3)    

    # Modelul este semnificativ la 99% (p-value: < 2.2e-16)
 
    # Forma functionala: PriceEuro = -105700 + 8786* Accel + 630.7* TopSpeed + 207.1 * Efficiency - 6.17 * Range + 
                            #   + 3352 * DummySegmentB -2922 *DummySeats5
         # La o crestere cu 1 km/h a vitezei maxime, pretul creste cu 630.7 euro
         # La o crestere cu 1 km a autonomiei, pretul scade cu 6.17 euro
         # Daca masina face parte din segmentul B, pretul sau este cu 3353 euro mai ridicat
         # Daca masina are fix 5 locuri, pretul ei este cu 2922 mai mic


#---------------------------- Model Panel -----------------------------------------------------------

#PROIECT PANEL
    # Sa se testeze alegerea tipului de model RE sau FE cu ajutorul testului Hausman.
    # Să se estimeze parametrii modelului cu metoda oferita de rezultatele testului Hausman.
    # Să se interpreteze rezultatele obţinute din punct de vedere economic si econometric si sa se verifice ipotezele modelului.
    # Sa se simuleze impactul asupra variabilei de interes prin scenarii de prognoza pentru modelul analizat.

  # Fisier utilizat: date_excel1.xlsx
  # Coloane: pib, consum

  #Instalarea pachetelor:
PackageNames <- c("tidyverse","gplots","plm","readxl","foreign","lmtest")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

data <- read_excel("C:/Users/anadu/Desktop/a3/Econometrie/PROIECT2/excel_date1.xlsx")    # citirea datelor din fisierul excel
summary(data)

pd.df <- pdata.frame(data, index = c("country","year"), drop.index = TRUE)      # Declararea setului de date de tip panel 

coplot(gdp ~ year|country, type="l", data=data)                  # Corelatia dintre GDP, tara si an

    # Heterogeneitatea presupune ca exista diferente intre unitatile studiate.
    # Explorarea heterogeneitatii in sectiunea transversala.
    # Graficul traseaza un interval de incredere de 95% in jurul mediilor.
    # Tari cu rata foarte mare si tari cu rata foarte mica => avem heterogeneitate transversala
    
plotmeans(gdp ~ country, main = 'Heterogeneitate in randul tarilor', data = data)

    # Explorarea heterogeneitatii in sectiunea temporala 
    # Ani cu rata aproximativ egala => nu avem heterogeneitate temporala
plotmeans(gdp ~ year, main = 'Heterogeneitate in timp', data = data)

    # Model OLS - model clasic de regresie liniara. Nu ia in calcul heterogeneitatea intre spatiu si timp
    # Modelul care explica variabilitatea variabilei dependente GDP in functie de consumul de energie nucleara,
            # din combustibil bio, carbune, fosil, gaz, hidroenergie, petrol, resurse regenerabile, energie solara
            # si energie eoliana
    # Variabilitatea GBP este explicata in proportie de 99% de variabilitatea variabilelor independenta 
    # Modelul este semnificativ la 95%
ols <- lm(gdp ~ nuclear_consumption + biofuel_consumption + coal_consumption +
            fossil_fuel_consumption+ gas_consumption+ hydro_consumption+
            +oil_consumption + renewables_consumption + solar_consumption
          +wind_consumption, data)    # R-squared = 99%, p-value < 0.05 

    # Modelul care explica variabilitatea variabilei dependente GDP in functie de consumul de energie nucleara,
        #  hidroenergie, resurse regenerabile, energie solara si energie eoliana
    # Variabilitatea GBP este explicata in proportie de 98% de variabilitatea variabilelor independenta 
    # Modelul este semnificativ la 95%

ols <- lm(gdp ~ nuclear_consumption +  hydro_consumption+
             + renewables_consumption + solar_consumption
          +wind_consumption, data)     # R-squared = 98%, p-value < 0.05 

    # Modelul care explica variabilitatea variabilei dependente GDP in functie de consumul de energie nucleara,
        #  hidroenergie si resurse regenerabile
    # Variabilitatea GBP este explicata in proportie de 97% de variabilitatea variabilelor independenta 
    # Modelul este semnificativ la 95%

ols <- lm(gdp ~ nuclear_consumption + hydro_consumption
          + renewables_consumption,data)     

  # R-squared = 83.27%, p-value < 0.05, toate variabilele independente sunt semnificative la 99%
summary(ols) 
yhat <- ols$fitted                # valori estimate

    # GDP in functie de cosumul de energie nuclara, hidroenergie si energie din resurse regenerabile
ggplot(data, aes(x= nuclear_consumption+hydro_consumption + renewables_consumption, y=gdp))+geom_point()+
  geom_smooth(method='lm', se=FALSE)+theme_bw()

    # Model FE (cu efecte fixe) 

fe <- plm(gdp ~ nuclear_consumption + hydro_consumption
 + renewables_consumption, data, index = c('country','year'), model = 'within')     # R-squared = 41.9%, p-value<0.05


summary(fe)
        #  nuclear_consumption, hydro_consumption si renewables_consumption sunt semnificative


  # Alegerea celei mai adecvate variante de model prin testarea intre regresie 
    # H0: FE;      H1: OLS
pFtest(fe, ols)                        # p-value < 0.05 => se recomanda model de panel data FE (efecte fixe)
phtest(fe,ols)                         # p-value < 0.05 => se recomanda utilizarea modelului cu efecte fixe 
  
   # Model cu efecte aleatorii RE (random effects)
re <- plm(gdp ~  nuclear_consumption + hydro_consumption+ renewables_consumption, data, index = c('country','year'), model = 'between')
summary(re)           # R-squared = 85%;    p-value <0.05  => modelul este semnificativ  
       # Toate variabilele sunt semnificative

  # Testul Hausmann il utilizam pentru a decide intre FE si RE
     # H0: model cu efecte random       # H1: model cu efecte fixe
phtest(fe,re)                # p-value <0.05 => se recomanda utilizarea modelului cu efecte fixe 




#---------------------- Testare ipoteze model -------------------------
# In urma testului Hausmann, am decis sa utilizam modelul FE
fe <- plm(gdp ~ nuclear_consumption+  hydro_consumption + renewables_consumption,
          data, index = c('country','year'), model = 'within')    
summary(fe)

# Testarea efectelor fixe in timp
fixed.time <- plm(gdp ~   nuclear_consumption +hydro_consumption
                  + renewables_consumption + factor(year), 
                  data=data, index=c("country","year"), model="within")
 
 # H0:  nu sunt necesare efectele fixe in timp
  # H1:  sunt necesare efectele fixe in timp
pFtest(fixed.time, fe)               # p-value = 0.27 < 0.05 (fals) => se recomanda folosirea fe 
plmtest(fe, c('time'), type = 'bp')   # p-value = 0.25

  # Testarea efectelor aleatorii cu Breusch-Pagan Lagrange Multiplier
  # Testul ne ajuta sa decidem intre RE si OLS 

pool <- plm(gdp ~ nuclear_consumption +  hydro_consumption 
            + renewables_consumption, data=data,
            index=c("country", "year"), model="pooling")

summary(pool)            # R = 83%.27,    nuclear_consumption,  hydro_consumption si renewables_consumption semnificsative
       # p-value <0.05 => modelul este semnificativ

    # H0: variatiile in timp sunt 0      H1: variatiile in timp sunt diferite de 0
plmtest(pool, type=c("bp"))       # p-value < 0.05 => respingem ipoteza nula
                            # variatiile in timp sunt diferite de 0 => efectele aleatorii sunt adecvate a.i.
                            # exista diferente semnificative intre tari


  # Testarea dependentei transversale folosind testul Breusch-Pagan LM si testul Parasan CD
        # Potrivit lui Baltagi, dependenta transversala este o problema intalnita
        # in randul seturilor de date panel cu serii de timp lungi. Aceasta problema
        # este rar intalnita in cazul panel data cu serii de timp scurte
  # Ipoteze teste
       # H0: reziduurile intre entitati nu sunt corelate    H1: reziduurile intre entitati sunt corelate

pcdtest(fe, test = 'lm') # p-value  =  1.242e-07 <0.05 => dependenta transversala
pcdtest(fe, test = 'cd') # p-value  =  0.42   <0.05 (fals) => nu avem dependenta in timp


  # Testarea autocorelarii - Breusch-Godfrey/Wooldridge test 
  # Testul se aplica doar cand seriile de timp sunt lungi. In acest caz nu pune probleme setul de date deoarece avem date pe 12 ani
        # H0: Nu exista autocorelate         # H1: autocorelarea este prezenta
pbgtest(fe) # p-value  <0.05  =>  avem autocorelare

  # Testarea heteroschedasticitatii cu testul Breusch-Pa  gan
    # H0: homoschedasticitate         # H1: heteroschedasticitate
bptest(gdp ~   hydro_consumption + renewables_consumption + factor(country), data = data, studentize=F)
         # deoarece p-value  <0.05 => avem heteroschedasticitate
#--------------------------------------------------------------------
  # Testarea efectelor random 
pFtest(re, ols) # p-value = 0.7 > 0.05 => nu se recomanda efecte random.
plmtest(re, c('time'), type = 'bp') # p-value = 0.25 > 0.05 => nu se recomanda efecte random
  # Testul Hausmann recomanda efectele random, aplicam testele:
pbgtest(re) # p-value > 0.05 => nu exista autocorelare
bptest(gdp ~   hydro_consumption + renewables_consumption+ factor(year), data = data, studentize=F) # p-value < 0.05 => exista hetero

