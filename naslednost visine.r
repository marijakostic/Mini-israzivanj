install.packages("mosaicData") #instaliramo paket 
library(mosaicData)  #ucitavamo paket

data(Galton) #ucitavamo bazu
attach(Galton) #ukljucujemo bazu 
help("Galton") 
str(Galton) #ispitujemo sta sadrzi baza

summary(Galton) #prikaz uopstenih podataka baze

HSmax<-tapply(height,sex,max)
HSmax # najveca visina za svaki pol deteta
HSmin<-tapply(height,sex,min)
HSmin # najmanja visina za svaki pol deteta
HSmeans<-tapply(height,sex,mean)
HSmeans  # srednja vrednost visine za svaki pol deteta

ListaVisina<-list(Najveca_visina_za_svaki_pol_deteta=HSmax,
                  Najmanja_visina_za_svaki_pol_deteta=HSmin,
                  Srednja_vrednost_visine_za_svaki_pol_deteta=HSmeans)
#lista elemenata koja sacrdi najvecu,najmanju i srednju visinu za svaki pol deteta

#barplotovi visina oceva,majki,dece i broja dece u porodici redom
barplot(table(father),main="Father's height",xlab = "Height in inches",
        ylab = "Count of fathers with the appropriate height",col="royalblue1")
barplot(table(mother),main="Mother's height",xlab = "Height in inches",
        ylab = "Count of mothers with the appropriate height",col="firebrick2")
barplot(table(height),main="Child's height",xlab = "Height in inches",
        ylab = "Count of children with the appropriate height",col="darkseagreen2")

barplot(table(nkids),main="Number of children in family",xlab = "Number of children",
        ylab = "Family",col="rosybrown1")

barplot(table(sex),main="Sex of children",xlab = "sex",
        names.arg = c("Female","Male"),col = "salmon",density=5)

barplotHS <- table(Galton$sex,Galton$nkids)
barplot((barplotHS), main="Number of children for each sex",xlab="Number of children",
        col=c("firebrick1","royalblue2"),legend = rownames(barplotHS),beside = TRUE)


barplot(prop.table(table(nkids,sex),2),beside = T,col=rainbow(15))
legend(locator(n=1),legend = rownames(table(nkids,sex)),fill = rainbow(15))

library(aplpack)
bagplot(height,sex, xlab="Child's height", ylab="Sex",main="Bagplot 2D")

NumberM<-length(subset(Galton,sex=="M")$sex)
NumberF<-length(subset(Galton,sex=="F")$sex)

slices<-c(NumberM,NumberF)
lbls<-c("Male","Female")
pie(slices,labels=lbls,main="Pie:sex of children",col = c("slateblue4","thistle"))

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # dodavanje procenta 
lbls <- paste(lbls,"%",sep="") # dodavanje % 
pie(slices,labels = lbls, col = c("slateblue4","thistle"),main="Pie sex of children")

slices2<-c()
for(i in 1:15)
  slices2[i]<-sum(Galton$nkids==i)
lbls<-c(1:15)
pie(slices2,lbls,col=rainbow(15),main ="Number od children in family")

install.packages("plotrix")
library(plotrix)

slices3<-slices2[slices2!=0]
lbls3 <- c(1,2,3,4,5,6,7,8,9,10,11,15)
pct2 <- round(slices3/sum(slices3)*100)
lbls2 <- paste(lbls3, pct2)
lbls2<- paste(lbls2, "%", sep = "")
pie3D(slices3, labels = lbls2, explode = 0.1,main ="Number od children in family", col = rainbow(12))

#histoframi visina oceva,majki,dece i histogram broja dece u porodici redom
hist(Galton$father, main="Father's height",xlab = "Height in inches",xlim=c(60,80),col="royalblue1")
hist(Galton$mother, main="Mother's height",xlab = "Height in inches",xlim=c(55,75),col="firebrick2")
hist(Galton$height, main="Child's height",xlab = "Height in inches",xlim=c(55,80),col="darkseagreen2")
hist(Galton$nkids, main="Number of children in family",xlab = "Number",xlim=c(0,15),col="rosybrown1")

#histogram visine dece sa normalnom krivom
x <- Galton$height
h <- hist(x,xlim=c(55,80),breaks = 40, col = "mediumorchid1",
          xlab = "Height in inches",
          main = "Galton: Child's height with normal curve")
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col = "darkorchid4", lwd =2)

# Kernel Density plot
d1 <- density(Galton$nkids) # vraca gustinu podataka
plot(d1,main="Kernel Density of number od kids") # crta rezultat

# Popunjen Density Plot
d2 <- density(Galton$nkids)
plot(d2, main="Filled Kernel Density of number of kids")
polygon(d2, col="paleturquoise2", border="pink1")

install.packages("sm")
library(sm)
# uvodimo oznake za pol
sex.f <- factor(sex, levels= c(0,1),labels = c("Male", "Female") )
# grafici gusine
sm.density.compare(height,sex, xlab="Child's height")
title(main="Child's height per sex")
# dodavanje legende klikom misa na mesto gde zelimo da se pojavi legenda
colfill<-c(2:(2+length(levels(sex)))) 
legend(locator(1), levels(sex), fill=colfill)


boxplot(father,horizontal=TRUE,main="Father's height",xlab = "Height in inches",col="royalblue1")
boxplot(mother,horizontal=TRUE,main="Mother's height",xlab = "Height in inches",col="firebrick2")
boxplot(height,horizontal=TRUE,main="Child's height",xlab = "Height in inches",col="darkseagreen2")
boxplot(nkids,horizontal=TRUE,main="Number of children in family",xlab = "Number of children",col="rosybrown1")
boxplot(height~sex,col=c("darkseagreen2","brown3"),main="Boxplot of height by sex",notch=TRUE)


Hmean<-((Galton$father+Galton$mother)/2)
plot(Hmean, height, main="Scatterplot of height", 
     xlab="Mean od parent's height ", ylab="Child's height ", pch=19)

abline(lm(height~Hmean), col="red") # regresiona linija

stripchart(height~sex,
           main="Different height of children for each sex",
           xlab="Sex",
           ylab="Chil's height",
           col="brown3",
           group.names=c("Female","Male"),
           vertical=T,
           pch=16
)

install.packages("scatterplot3d")
library(scatterplot3d)

scatterplot3d(height,Hmean,nkids,pch=20, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")

install.packages("rgl")
library(rgl)
plot3d(height,Hmean,nkids, col="red", size=3)


fivenum(mother)
fivenum(father)
fivenum(height)
#donji kvantil, medijana, gornji kvantil,maksimum 

stem(father)
shapiro.test(father)

stem(mother)
shapiro.test(mother)

stem(height)
shapiro.test(height)

stem(nkids) #stablo lisce dijagram
shapiro.test(nkids) #testiramo da li ima standardnu normalnu raspodelu
shapiro.test(rnorm(length(nkids),mean = mean(nkids), sd=sd(nkids))) #testiramo da li ima normalnu raspodelu
mean(nkids)
sd(nkids)

install.packages("stepfun")
library(stepfun)
#grafik empirijske funkcije raspodele
plot(ecdf(height), do.points=FALSE, verticals=TRUE)
#Ova raspodela je ocigledno razlicita od bilo koje standardne raspodjele
x <- seq(56, 80, 2)
lines(x, pnorm(x, mean=mean(height), sd=sqrt(var(height))), lty=3,col="red") 
#Prilagodimo normalnoj raspodeli i prekrijemo sa prilagodjenom funkcijom raspodele

qqnorm(height)
qqline(height)

# Kolmogorov-Smirnov test
ks.test(height, "pnorm", mean=mean(height), sd=sqrt(var(height)))

qqplot(height[sex=="M"],height[sex=="F"], col="blue", main="Nezavisnost visina decaka i devojcica",
       xlab = "Visina decaka", ylab = "visina devojcica")

#t-test

t.test(father,mother)
t.test(height[sex=="M"],height[sex=="F"])

wilcox.test(height~sex) 
kruskal.test(height~sex) 

TK<-table(family,sex)
chisq.test(TK)



cor.test(height,nkids)
cor.test( height,nkids, method="spearman")






















