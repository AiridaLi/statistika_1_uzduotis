###############################################################################
####                    Klausimų pavyzdžiai spalio 8 d.                    ####
###############################################################################
# Įvykdykite komandą
B<-2:7 #skaiciu seka
# Klausimai:
# Koks objekto B tipas?
class(B) #tipas-integar
str(B)
mode(B)
# Kokie objekto matavimai (elementų skaičius)?
length(B) #elementu skc 6 (kadangi vektorius tai vienintele dimensija ilgis)
# Parašykite komandų (kurias įvykdžius gaunami atsakymai į klausimus)
# tekstą ir atsakymus.
#
#Įvykdykite komandą
library(car)
install.packages("car")
#      Klausimai apie duomenų rinkinį UN
#
#
###########################################################
#
#   Klausimai:
#
# 1) Kiek eilučių ir stulpelių turi duomenų aibė
# (parašykit atitinkamą komandą ir atsakymą)?
data(UN) #Susiradau rinkini
ncol(UN) #Stulpeli7 skc -7
nrow(UN) #eiluciu skc -213
# 2) Parašykit komandą, kurią įvykdžius, kintamojo “ppgdp” 
# pavadinimas pasikeis į “ppbvp”.
colnames(UN)[colnames(UN)=="ppgdp"] <- "ppbvp"
# 3) Parašykite komandas, kurias įvykdžius, iš duomenų aibės bus išrinktos:
#    a) eilutės (šalių duomenys), kuriose tikėtina moterų gyvenimo trukmė  didesnė nei 45 metai ir
#        ne didesne nei 70,
library(dplyr)
filter(UN, UN$lifeExpF>45 & UN$lifeExpF<=70)
#    b) eilutės su gimstamumo rodikliu (fertility) didesniu nei 2 arba kūdikių
#        mirtingumu (infantMortality) mažesniu nei 50,
filter(UN, UN$fertility>2 | UN$infantMortality<50)
#    c) eilutes, kuriose kintamasis pctUrban neturi praleistų reikšmių.
filter(UN, pctUrban!="NA")
filter(UN, !is.na(pctUrban)) #kitas variantas
# 4) Parašykit komandą (ir jos atsakymą), kurias įvydžius, apskaičiuosime 
# vidutinį kūdikių mirštamumo rodiklį šalims, kurių ppbvp>1000. 
r<-filter(UN, UN$ppbvp>1000)
mean(r$ppbvp, na.rm=T) #15953.35
#####################################################
#
#   Įvykdykite žemiau parašytas komandas
#
####################################################
library(ggplot2)
UNN<-UN
UNN$country<-rownames(UN)
UNN<-na.omit(sample_n(UNN,10))
ggplot(UNN,aes(x=ppbvp,y=infantMortality,color=country))+geom_point()+
        theme(axis.text.x=element_text(angle=90, hjust=1,size = "12"))+
        xlab("PPbvp") + ylab("kudikiu mirtingumas")+
        ggtitle("Kūdikių mirtingumo priklausomybė nuo PPbvp")+
        geom_hline(aes(yintercept=20))+
        scale_color_discrete(name = "Salis")
##########################################################################
#
# 1) Papildykite ggplot komandą taip, kad x ašies pavadinimas būtų “PPbvp”,
#    o y ašies “kūdikių mirtingumas”.
#xlab("PPbvp") + ylab("kudikiu mirtingumas")
# 2) Papildykite komandą, kad legendos pavadinimas būtų "šalis".
#scale_color_discrete(name = "Salis")
# 3) Papildykite  komandą, kad būtų pateiktas grafiko pavadinimas:
# “Kūdikių mirtingumo priklausomybė nuo PPbvp”.
#ggtitle("Kūdikių mirtingumo priklausomybė nuo PPbvp")
# 4) Perrašykite komandą taip, kad x ašies žymelės būtų pateiktos vertikaliai.
#theme(axis.text.x=element_text(angle=90, hjust=1,size = "12"))
# 5) Papildykite komandą taip, kad būtų nubrėžta horizontali atskaitos linija y=20.
#geom_hline(aes(yintercept=20))