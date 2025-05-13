library(moments)
packages <- c("ggplot2", "readxl", "dplyr", "stats","tidyr", "GGally", "corrplot", "lmtest", "sandwich", "whitestrap")
invisible(lapply(packages, library, character.only = TRUE))


df <- read.csv("preturi-toyota-sp500-general_motors.csv", sep = ";", dec = ".")
View(df) #am preturile pt toyota si sp500 direct in csv

statistici_descriptive <- summary(df)

df <- df[order(as.Date(df$data, format="%d.%m.%Y"), decreasing = FALSE),]
View(df) #am sortat crescator dupa data tranzactionarii
attach(df)



par(mfrow = c(3,1))
plot(pret_toyota, type = "l" , col = "red",ylab = "Pret Actiune", xlab = "timp",
     ,lwd = 3, main = "Evolutie pret Toyota 01.04.2024 - 02.04.2025")
plot(pret_general_motors, type = "l", col = "blue1",ylab = "Pret Actiune", xlab = "timp",
     main = "Evolutie Pret General Motors 01.04.2024 - 02.04.2025")
plot(pret_sp500, type = "l" , col = "green", ylab = "Pret Actiune", xlab = "timp",
     main = "Evolutie Pret SP500 01.04.2024 - 02.04.2025")


par(mfrow = c(2,1))

hist(pret_toyota, main = "Distributie Pret Toyota")
hist(pret_sp500, main = "Distributie Pret SP500")


# Calculează rentabilitatea pentru Toyota
ret_toyota <- c(diff(pret_toyota) / head(pret_toyota, -1))

# Calculează rentabilitatea pentru S&P 500
ret_sp500 <- c(diff(pret_sp500) / head(pret_sp500, -1))

ret_gm <- c(diff(pret_general_motors) / head(pret_general_motors, -1))

mat_rentab <- data.frame(df$data[-1], ret_toyota, ret_sp500, ret_gm)

par(mfrow = c(3,1))
hist(mat_rentab$ret_toyota, main = "Evolutie  Rentabilitate Toyota ",
     col = "red", xlab = "Pret Standardizat",ylab = "Frecventa")
hist(mat_rentab$ret_sp500, main = "Evolutie Rentabilitate GM",
     col = "blue",xlab = "Pret Standardizat",ylab = "Frecventa")
hist(mat_rentab$ret_gm, main = "Evolutie Rentabilitate SP500", 
     col = "green",xlab = "Pret Standardizat",ylab = "Frecventa")

View(mat_rentab)

#analizez evolutia pt Toyota
amplit_toyota <- max(pret_toyota) - min(pret_toyota)
sd_toyota <- sd(pret_toyota)
asim_toyota <- skewness(pret_toyota)
bolt_toyota <- kurtosis(pret_toyota)
#general_motors
amplit_gm <- max(pret_general_motors) - min(pret_general_motors)
sd_gm <- sd(pret_general_motors)
asim_gm <- skewness(pret_general_motors)
bolt_gm <- kurtosis(pret_general_motors)

#sp500
amplit_sp500 <- max(pret_sp500) - min(pret_sp500)
sd_sp500 <- sd(pret_sp500)
asim_sp500 <- skewness(pret_sp500)
bolt_sp500 <- kurtosis(pret_sp500)
                      
mat_cor <- matrix(c(amplit_toyota,sd_toyota, asim_toyota, bolt_toyota,
                    amplit_gm, sd_gm, asim_gm, bolt_gm,
                    amplit_sp500, sd_sp500, asim_sp500,bolt_sp500),
                  nrow = 3, ncol = 4, byrow = TRUE)
colnames(mat_cor) <- c("amplitudine", "abaterea std", "asimetrie", "boltire")
rownames(mat_cor) <- c("Toyota", "General_Motors", "SP500")

View(mat_cor) 

#matricea de corelatie intre valorile act. celor 3 companii
lowerFn <- function(data=df[,2:4], mapping, method = "lm", ...) {
  fig <- ggplot(data = df, mapping = mapping) +
    geom_point(colour = "blue3") +
    geom_smooth(method = method, color = "red", ...)
  fig
}
plot<-ggpairs(
  df[,2:4],lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", bins=12,colour = "blue")),
  upper = list(continuous = wrap("cor", size = 9))
); plot
#rentabilitati
lowerFn <- function(data=mat_rentab[,2:4], mapping, method = "lm", ...) {
  fig <- ggplot(data = mat_rentab, mapping = mapping) +
    geom_point(colour = "blue3") +
    geom_smooth(method = method, color = "red", ...)
  fig
}
plot<-ggpairs(
  mat_rentab[,2:4],lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", bins=12,colour = "blue")),
  upper = list(continuous = wrap("cor", size = 9))
); plot
#standardizarea datelor
std_complex <- function(x) {
  (x - mean(x)) / sd(x)
}

date_std_complex <- apply(df[-1], MARGIN =2, FUN = std_complex)
View(date_std_complex)

date_std_complex <- data.frame(df[1], date_std_complex)

par(mfrow = c(3,1))
hist(date_std_complex$pret_toyota, main = "Evolutie Pret Toyota Std",
     col = "red", xlab = "Pret Standardizat",ylab = "Frecventa")
hist(date_std_complex$pret_general_motors, main = "Evolutie Pret GM Std",
     col = "blue",xlab = "Pret Standardizat",ylab = "Frecventa")
hist(date_std_complex$pret_sp500, main = "Evolutie Pret SP500 Std", 
     col = "green",xlab = "Pret Standardizat",ylab = "Frecventa")


#3 grafice pe aceeasi axa
ts.plot(date_std_complex[,2:4], col = c("red", "green", "blue"),
        main = "Evolutie Pret Actiuni Standardizate in perioada 01.04.2024 - 02.04.2025")

legend("bottomright", legend = c("Toyota", "SP500", "General Motors"), 
       col = c("red", "green", "blue"), lty = 1, cex = 0.8, bty = "n")


ts.plot(mat_rentab[,2:4], col = c("red", "green", "blue"),
        main = "Evolutie rentabilitati actiuni")

legend("topleft", legend = c("Toyota", "SP500", "General Motors"), 
       col = c("red", "green", "blue"), lty = 1, cex = 0.8, bty = "n")

#3 grafice intr-unul pt fiecare companie
plot.ts(date_std_complex[,2:4], col = "purple", main = "Evolutie Pret Actiuni", lwd = 3)

par(mfrow=c(1,3))
boxplot(pret_toyota, main = "Boxplot pret actiune Toyota", col="red",
        ylab = "Pret($)")
boxplot(pret_general_motors,main = "Boxplot pret actiune GM", col = "blue",
        ylab = "Pret($)")
boxplot(pret_sp500,main = "Boxplot pret actiune SP500",col = "green",
        ylab = "Pret($)")


boxplot(mat_rentab$ret_toyota, mat_rentab$ret_gm, mat_rentab$ret_sp500)
#varianta mai scurta pt a gasi valori de tip outlier
#returneaza valorile outlier
outlier_toyota <- boxplot.stats(pret_toyota)$out
outlier_sp500 <- boxplot.stats(pret_sp500)$out
outlier_gm <- boxplot.stats(pret_general_motors)$out

#caut in ce zile s-au gasit outlier pt toyota(2 val) si pt general motors (1 val)
zile_outlier_toyota <- df$data[pret_toyota %in% outlier_toyota]
zile_outlier_sp500 <- df$data[pret_sp500 %in% outlier_sp500]
zile_outlier_gm <- df$data[pret_general_motors %in% outlier_gm]


#pas5
#Pentru mine Toyota ramane un brand consacrat din punct de vedere al fiabilitatii. Imaginea creata la nivel global, dar si la nivel national de acest brand este una a eficientei si a sigurantei.Toyota a castigat increderea consumatorilor prin garantia pe care o ofera la achizitionarea unui vehicul nou, dar si prin fiabilitatea modelelor vechi,care au inregistrat sute de mii de km fara a avea probleme majore.
#Ca orice brand auto s-a confruntat cu probleme in ceea ce priveste lantul de productie,dar Toyota a ajuns la o maiestrie (specific japoneza) de a eficientiza fiecare proces de productie.Spre deosebire de alte marci din industrie care nu sunt originare din SUA, Toyota a reusit foare bine sa se impuna pe segmentul ei de piata in SUA, facand investitii masive pentru productie, cercetare si dezvoltare, dar si marketing.
#Daca analizam dpdv al indicatorilor financiari, Toyota a performat mult mai bine in ultimii 14 ani fata de competitorul analizat, General Motors, oferind o incredere sporita actionarilor sai. Datorita eficentizarii procesului de productie , Toyota a reusit sa inregistreze un profit semnificativ mai mare fata de G.M.
#Daca as alege sa cumpar actiunii, cu siguranta as alege sa investesc in compania Toyota, mai degraba decat in General Motors, deoarece ofera incredere si perspectiva de viitor.Date fiind directiile de Uniunea Europeana si de SUA in ceea ce priveste reducerea emisiilor si transportul cu emisii reduse, Toyota pare sa fi castigat deja trend-ul prin investiile masive in R&D pe acest segment.
#Nu doar ca cifrele inregistrate de Toyota sunt mai optimiste, dar si perspectiva de viitor suna mai bine in privinta ei.Chiar daca G.M. a anuntat public investitii masive in aceasta tehnologie, Toyota a ajuns deja la o maturitate si pare sa fie printre liderii de piata care ofera directii celorlalti competitori.
#Indicatorii ROA si ROE sunt de asemenea in favoarea companiei Toyota, inregistrand valori apropiate de 10% in ultimii ani, in vreme ce G.M. s-a confruntat cu valori f. mici , apropiate de 0 oferind nesiguranta investitorilor. Chiar daca vanzarile au fost pe un trend ascendent pentru G.M. deciziile de investitii ale companiei au scazut profitul marginal.
#Daca ar fi sa ma rezum strict din punctul de vedere al unui investitor, eu sunt riscofob.As alege sa investesc in SP500 sau Toyota,
#deoarece sunt deja consacrate pe piata bursiera.Au un randament relativ mic (sub 10%), dar este garantat.
#In ciuda taxelor vamale impuse in 2025, Toyota a inregistrat vanzari record si daca e sa privim pe un termen mai lung,Toyota pare sa aiba
#o viziune bine conturata in ceea ce priveste viitorul auto, atat al celor eletrice,cat si al celor hibride, dar si a condusului autonom.
#Astfel,Toyota poate deveni un lider pe piata auto globala.

