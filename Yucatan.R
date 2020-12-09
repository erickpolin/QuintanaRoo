########## Creación base 2010 ############

#ENIGH 2010
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2010/ENIGH2010")
Conc<-read.dbf("Conc2010.dbf",as.is = T)

ConcYucatan<-Conc%>% 
  filter(entidad=="31")

remove(Conc)

#apparently this is a "flag", IDK what is this shit yet
ConcYucatan$Nhog <- 1

#DECILES 

#Attaching the data frame
attach(ConcYucatan) #this is for not writing the name of the data frame and the $ ever time

#Sort Conc according to INGCOR, FOLIOVIV, FOLIOHOG
ConcYucatan<- orderBy (~+ing_cor+folioviv+foliohog, data=ConcYucatan) #this give us the households sorted by total income

#Adding the values of the expansion FACTOR. The sum is 34 million, which is the number of households in the country.
tot_hogares<-sum(ConcYucatan$factor,to.data.frame=TRUE)

#Dividing the number of households into 10 without decimals
tam_dec<-trunc(tot_hogares/10) #the result is 3.5 million housholds per decile

#Adding a variable to the data frame with this value
ConcYucatan$tam_dec<-tam_dec

# Creating Deciles of Income 

ConcYucatan$MAXT<-ConcYucatan$ing_cor #vamos a crear en esta base la variable MAXT que es una copia de la columna de ingresos

ConcYucatan<-ConcYucatan[with(ConcYucatan, order(rank(MAXT))),]  #lo que hicimos aqu? fue reordenar la base con respecto a MAXT. Cosa que en realidad, ya estaba.

ConcYucatan$ACUMULA<-cumsum(ConcYucatan$factor) #aqu? creamos una variable de suma acumulada del FACTOR de viviendas.



# Ahora viene la creaci?n de los deciles###


#no se que es esto de a1 y b1. Pero s? e simportante. Los resultados cmabian por poquito si no lo haces 
for(i in 1:9)
{
  a1<-ConcYucatan[dim(ConcYucatan[ConcYucatan$ACUMULA<tam_dec*i,])[1]+1,]$factor
  ConcYucatan<-rbind(ConcYucatan[1:(dim(ConcYucatan[ConcYucatan$ACUMULA<tam_dec*i,])[1]+1),],
                      ConcYucatan[(dim(ConcYucatan[ConcYucatan$ACUMULA<tam_dec*i,])[1]+1):dim(ConcYucatan[1])[1],])
  
  b1<-tam_dec*i-ConcYucatan[dim(ConcYucatan[ConcYucatan$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  ConcYucatan[(dim(ConcYucatan[ConcYucatan$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  ConcYucatan[(dim(ConcYucatan[ConcYucatan$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}

#aqu? estamos creando otra variable de suma acumulada del n?mero de hogares
ConcYucatan$ACUMULA2<-cumsum(ConcYucatan$factor)

#aqu? estamos creando una variable que se llama decil que solo tiene ceros
ConcYucatan$DECIL<-0

#recordemos que el tama?o de cada decil es de 3,474,481. 
#loq ue hicimos aqu? es pedirle que ponga un uno a los primeros hogares menores al tama?o de decil. 
#es decir, que el primer decil tiene ya UNOS
ConcYucatan[(ConcYucatan$ACUMULA2<=tam_dec),]$DECIL<-1

#para una sucesi?n del 1 al 9, cuando la variable acumulado2 sea mayor que el tama?o de decil multiplicado por
#1, 2, 3... pone en la variable decil el n?mero i+1
for(i in 1:9)
{
  ConcYucatan[((ConcYucatan$ACUMULA2>tam_dec*i)&(ConcYucatan$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}

# a lo que le qued? cero (que es la ?ltima observaci?n), ponle el decil 10
ConcYucatan[ConcYucatan$DECIL%in%"0",]$DECIL<-10

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
write.dbf(ConcYucatan,file="ConcYucatan2010.dbf")

rm(list=ls())

########## Creación base 2018 ############

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018")
Conc<-read.dbf("Conc2018.dbf",as.is = T)


ConcYucatan<-Conc%>% 
  filter(entidad=="31")



remove(Conc)

#apparently this is a "flag", IDK what is this shit yet
ConcYucatan$Nhog <- 1

# DECILES 


#Attaching the data frame
attach(ConcYucatan) #this is for not writing the name of the data frame and the $ ever time

#Sort Conc according to ing_cor, folioviv, foliohog
ConcYucatan<- orderBy (~+ing_cor+folioviv+foliohog, data=ConcYucatan) #this give us the households sorted by total income

#Adding the values of the expansion factor. The sum is 34 million, which is the number of households in the country.
tot_hogares<-sum(ConcYucatan$factor,to.data.frame=TRUE)

#Dividing the number of households into 10 without decimals
tam_dec<-trunc(tot_hogares/10) #the result is 3.5 million housholds per decile

#Adding a variable to the data frame with this value
ConcYucatan$tam_dec<-tam_dec

# Creating Deciles of Income 

ConcYucatan$MAXT<-ConcYucatan$ing_cor #vamos a crear en esta base la variable MAXT que es una copia de la columna de ingresos

ConcYucatan<-ConcYucatan[with(ConcYucatan, order(rank(MAXT))),]  #lo que hicimos aqu? fue reordenar la base con respecto a MAXT. Cosa que en realidad, ya estaba.

ConcYucatan$ACUMULA<-cumsum(ConcYucatan$factor) #aqu? creamos una variable de suma acumulada del factor de viviendas.



# Ahora viene la creaci?n de los deciles 


#no se que es esto de a1 y b1. Pero s? e simportante. Los resultados cmabian por poquito si no lo haces 
for(i in 1:9)
{
  a1<-ConcYucatan[dim(ConcYucatan[ConcYucatan$ACUMULA<tam_dec*i,])[1]+1,]$factor
  ConcYucatan<-rbind(ConcYucatan[1:(dim(ConcYucatan[ConcYucatan$ACUMULA<tam_dec*i,])[1]+1),],
                      ConcYucatan[(dim(ConcYucatan[ConcYucatan$ACUMULA<tam_dec*i,])[1]+1):dim(ConcYucatan[1])[1],])
  b1<-tam_dec*i-ConcYucatan[dim(ConcYucatan[ConcYucatan$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  ConcYucatan[(dim(ConcYucatan[ConcYucatan$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  ConcYucatan[(dim(ConcYucatan[ConcYucatan$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}

#aqu? estamos creando otra variable de suma acumulada del n?mero de hogares
ConcYucatan$ACUMULA2<-cumsum(ConcYucatan$factor)

#aqu? estamos creando una variable que se llama decil que solo tiene ceros
ConcYucatan$DECIL<-0

#recordemos que el tama?o de cada decil es de 3,474,481. 
#loq ue hicimos aqu? es pedirle que ponga un uno a los primeros hogares menores al tama?o de decil. 
#es decir, que el primer decil tiene ya UNOS
ConcYucatan[(ConcYucatan$ACUMULA2<=tam_dec),]$DECIL<-1

#para una sucesi?n del 1 al 9, cuando la variable acumulado2 sea mayor que el tama?o de decil multiplicado por
#1, 2, 3... pone en la variable decil el n?mero i+1
for(i in 1:9)
{
  ConcYucatan[((ConcYucatan$ACUMULA2>tam_dec*i)&(ConcYucatan$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}

# a lo que le qued? cero (que es la ?ltima observaci?n), ponle el decil 10
ConcYucatan[ConcYucatan$DECIL%in%"0",]$DECIL<-10

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
write.dbf(ConcYucatan,file="ConcYucatan2018.dbf")

rm(list=ls())



########## Creación tablas ingresos total 2010 #######

library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
Conc2010<-read.dbf("ConcYucatan2010.dbf",as.is = T)

names(Conc2010)<-c("ENTIDAD","FOLIOVIV","FOLIOHOG","TOT_INTEG","INGCOR","INGTRAB",
                   "TRABAJO","NEGOCIO","OTROS_TRAB","RENTAS","UTILIDAD","ARRENDA",
                   "TRANSFER","JUBILA","BECA","DONATIVO","REMESA","BENE_GOB",
                   "ESP_HOG","ESP_INST","ESTI","OTROS","FACTOR","UPM",
                   "EST_DIS","tam_loc","Small","HOGARINDIG","NOMBRE_ENT","DEFLACTORES","Nhog","TAM_DEC",
                   "MAXT","ACUMULA","ACUMULA2","DECIL")

mydesign <- svydesign(id=~UPM,strata=~EST_DIS,data=Conc2010,weights=~FACTOR)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~INGCOR,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por decil
#aqu? cmabia la funci?n a svyby, en by va el decil que creamos.
#y al final va la funci?n que queremos
Ming_corDECIL <- svyby(~INGCOR,denominator=~Nhog,by=~DECIL,mydesign,svyratio)


#     Trabajo
#
#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
MingtrabTot <- svyratio(~INGTRAB,denominator=~Nhog,mydesign) # Total promedio
MingtrabDECIL <- svyby(~INGTRAB,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~TRABAJO,denominator=~Nhog,mydesign) # Total promedio
MtrabajoDECIL <- svyby(~TRABAJO,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo independiente
MnegocioTot <- svyratio(~NEGOCIO,denominator=~Nhog,mydesign) # Total promedio
MnegocioDECIL <- svyby(~NEGOCIO,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso de otros trabajos
Motros_trabTot <- svyratio(~OTROS_TRAB,denominator=~Nhog,mydesign) # Total promedio
Motros_trabDECIL<- svyby(~OTROS_TRAB,denominator=~Nhog,by=~DECIL,mydesign,svyratio) # por decil


###################################        Rentas de la propiedad 

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
MrentasTot <- svyratio(~RENTAS,denominator=~Nhog,mydesign) # Total promedio
MrentasDECIL <- svyby(~RENTAS,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #Por decil
###### ingresos de sociedades
MutilidadTot <- svyratio(~UTILIDAD,denominator=~Nhog,mydesign) # Total promedio
MutilidadDECIL <- svyby(~UTILIDAD,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### arrendamiento
MarrendaTot <- svyratio(~ARRENDA,denominator=~Nhog,mydesign) # Total promedio
MarrendaDECIL <- svyby(~ARRENDA,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Por decil


###################################        Transferencias   

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

MtransferTot <- svyratio(~TRANSFER,denominator=~Nhog,mydesign) # Total promedio
MtransferDECIL <- svyby(~TRANSFER,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

MjubilacionTot <- svyratio(~JUBILA,denominator=~Nhog,mydesign) # Total promedio
MjubilacionDECIL <- svyby(~JUBILA,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### becas que pueden ser, de nuevo, p?blicas privadas. 
MbecasTot <- svyratio(~BECA,denominator=~Nhog,mydesign) # Total promedio
MbecasDECIL <- svyby(~BECA,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### donativos que tambi?n pueden ser p?blicos o privados.
MdonativosTot <- svyratio(~DONATIVO,denominator=~Nhog,mydesign) # Total promedio
MdonativosDECIL <- svyby(~DONATIVO,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
MremesasTot <- svyratio(~REMESA,denominator=~Nhog,mydesign) # Total promedio
MremesasDECIL <- svyby(~REMESA,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~BENE_GOB,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobDECIL <- svyby(~BENE_GOB,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~ESP_HOG,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogDECIL <- svyby(~ESP_HOG,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #decil

###### trans_inst: puede venir de institucione sp?blicas o privadas.
Mtrans_instTot <- svyratio(~ESP_INST,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instDECIL <- svyby(~ESP_INST,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL


### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~ESTI,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquDECIL <- svyby(~ESTI,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil


### otros_ing ### es literalmente ?algo m?s?
Motros_ingTot <- svyratio(~OTROS,denominator=~Nhog,mydesign) # Total promedio
Motros_ingDECIL <- svyby(~OTROS,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Decil


######################################### Estimaciones 

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corDECIL <- Ming_corDECIL[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

ES_MingtrabTot <- MingtrabTot[[1]]
ES_MingtrabDECIL <- MingtrabDECIL[[2]]

ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_MtrabajoDECIL <- MtrabajoDECIL[[2]]

ES_MnegocioTot <- MnegocioTot[[1]]
ES_MnegocioDECIL <- MnegocioDECIL[[2]]

ES_Motros_trabTot <- Motros_trabTot [[1]]
ES_Motros_trabDECIL <- Motros_trabDECIL [[2]]

ES_MrentasTot <- MrentasTot [[1]]
ES_MrentasDECIL <- MrentasDECIL [[2]]

ES_MutilidadTot <- MutilidadTot [[1]]
ES_MutilidadDECIL <- MutilidadDECIL [[2]]

ES_MarrendaTot <- MarrendaTot [[1]]
ES_MarrendaDECIL <- MarrendaDECIL [[2]]

ES_MtransferTot <- MtransferTot[[1]]
ES_MtransferDECIL <- MtransferDECIL[[2]]

ES_MjubilacionTot <- MjubilacionTot [[1]]
ES_MjubilacionDECIL <- MjubilacionDECIL [[2]]

ES_MbecasTot <- MbecasTot [[1]]
ES_MbecasDECIL <- MbecasDECIL [[2]]

ES_MdonativosTot <- MdonativosTot[[1]]
ES_MdonativosDECIL <- MdonativosDECIL[[2]]

ES_MremesasTot <- MremesasTot[[1]]
ES_MremesasDECIL <- MremesasDECIL[[2]]

ES_Mbene_gobTot <- Mbene_gobTot [[1]]
ES_Mbene_gobDECIL <- Mbene_gobDECIL [[2]]

ES_Mtransf_hogTot <- Mtransf_hogTot [[1]]
ES_Mtransf_hogDECIL <- Mtransf_hogDECIL [[2]]

ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instDECIL <- Mtrans_instDECIL[[2]]

ES_Mestim_alquTot <- Mestim_alquTot [[1]]
ES_Mestim_alquDECIL <- Mestim_alquDECIL [[2]]

ES_Motros_ingTot <- Motros_ingTot [[1]]
ES_Motros_ingDECIL <- Motros_ingDECIL [[2]]

########## Error Est?ndar 
SE_Ming_corTot <- SE (Ming_corTot)
SE_Ming_corDECIL <- SE (Ming_corDECIL)

SE_MingtrabTot <- SE (MingtrabTot)
SE_MingtrabDECIL <- SE (MingtrabDECIL)

SE_MtrabajoTot <- SE (MtrabajoTot)
SE_MtrabajoDECIL <- SE (MtrabajoDECIL)

SE_MnegocioTot <- SE (MnegocioTot)
SE_MnegocioDECIL <- SE (MnegocioDECIL)

SE_Motros_trabTot <- SE (Motros_trabTot)
SE_Motros_trabDECIL <- SE (Motros_trabDECIL)

SE_MrentasTot <- SE (MrentasTot)
SE_MrentasDECIL <- SE (MrentasDECIL)

SE_MutilidadTot <- SE (MutilidadTot)
SE_MutilidadDECIL <- SE (MutilidadDECIL)

SE_MarrendaTot <- SE (MarrendaTot)
SE_MarrendaDECIL <- SE (MarrendaDECIL)

SE_MtransferTot <- SE (MtransferTot)
SE_MtransferDECIL <- SE (MtransferDECIL)

SE_MjubilacionTot <- SE (MjubilacionTot)
SE_MjubilacionDECIL <- SE (MjubilacionDECIL)

SE_MbecasTot <- SE (MbecasTot)
SE_MbecasDECIL <- SE (MbecasDECIL)

SE_MdonativosTot <- SE (MdonativosTot)
SE_MdonativosDECIL <- SE (MdonativosDECIL)

SE_MremesasTot <- SE (MremesasTot)
SE_MremesasDECIL <- SE (MremesasDECIL)

SE_Mbene_gobTot <- SE (Mbene_gobTot)
SE_Mbene_gobDECIL <- SE (Mbene_gobDECIL)

SE_Mtransf_hogTot <- SE (Mtransf_hogTot)
SE_Mtransf_hogDECIL <- SE (Mtransf_hogDECIL)

SE_Mtrans_instTot <- SE (Mtrans_instTot)
SE_Mtrans_instDECIL <- SE (Mtrans_instDECIL)

SE_Mestim_alquTot <- SE (Mestim_alquTot)
SE_Mestim_alquDECIL <- SE (Mestim_alquDECIL)

SE_Motros_ingTot <- SE (Motros_ingTot)
SE_Motros_ingDECIL <- SE (Motros_ingDECIL)

########## Coeficiente de variaci?n 
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corDECIL <- cv(Ming_corDECIL)

CV_MingtrabTot <- cv(MingtrabTot)
CV_MingtrabDECIL <- cv(MingtrabDECIL)

CV_MtrabajoTot <- cv(MtrabajoTot)
CV_MtrabajoDECIL <- cv(MtrabajoDECIL)

CV_MnegocioTot <- cv(MnegocioTot)
CV_MnegocioDECIL <- cv(MnegocioDECIL)

CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabDECIL <- cv(Motros_trabDECIL)

CV_MrentasTot <- cv(MrentasTot)
CV_MrentasDECIL <- cv(MrentasDECIL)

CV_MutilidadTot <- cv(MutilidadTot)
CV_MutilidadDECIL <- cv(MutilidadDECIL)

CV_MarrendaTot <- cv(MarrendaTot)
CV_MarrendaDECIL <- cv(MarrendaDECIL)

CV_MtransferTot <- cv(MtransferTot)
CV_MtransferDECIL <- cv(MtransferDECIL)

CV_MjubilacionTot <- cv(MjubilacionTot)
CV_MjubilacionDECIL <- cv(MjubilacionDECIL)

CV_MbecasTot <- cv(MbecasTot)
CV_MbecasDECIL <- cv(MbecasDECIL)

CV_MdonativosTot <- cv(MdonativosTot)
CV_MdonativosDECIL <- cv(MdonativosDECIL)

CV_MremesasTot <- cv(MremesasTot)
CV_MremesasDECIL <- cv(MremesasDECIL)

CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobDECIL <- cv(Mbene_gobDECIL)

CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogDECIL <- cv(Mtransf_hogDECIL)

CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instDECIL <- cv(Mtrans_instDECIL)

CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquDECIL <- cv(Mestim_alquDECIL)

CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingDECIL <- cv(Motros_ingDECIL)

########## Limite inferior 
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,1]

LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,1]

LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,1]

LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,1]

LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,1]

LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,1]

LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,1]

LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,1]

LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,1]

LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1]
LI_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,1]

LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,1]

LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,1]

LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,1]

LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,1]

LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,1]

LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,1]

LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,1
]
LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingDECIL <- confint(Motros_ingDECIL ,level=0.90)[,1]

########## Limite superior 
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,2]

LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,2]

LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,2]

LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,2]

LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,2]

LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,2]

LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,2]

LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,2]

LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,2]

LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,2]

LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,2]

LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,2]

LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,2]

LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,2]

LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,2]

LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,2]

LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,2]

LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingDECIL <- confint(Motros_ingDECIL,level=0.90)[,2]

#############################      Cuadros   
#este cuadro, lo ?nico que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_DECIL_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corDECIL),c(ES_MingtrabTot,ES_MingtrabDECIL),c(ES_MtrabajoTot,ES_MtrabajoDECIL),c(ES_MnegocioTot,ES_MnegocioDECIL)
             ,c(ES_Motros_trabTot,ES_Motros_trabDECIL),c(ES_MrentasTot,ES_MrentasDECIL),c(ES_MutilidadTot,ES_MutilidadDECIL)
             ,c(ES_MarrendaTot,ES_MarrendaDECIL),c(ES_MtransferTot,ES_MtransferDECIL),c(ES_MjubilacionTot,ES_MjubilacionDECIL),c(ES_MbecasTot,ES_MbecasDECIL),
             c(ES_MdonativosTot,ES_MdonativosDECIL),c(ES_MremesasTot,ES_MremesasDECIL),c(ES_Mbene_gobTot,ES_Mbene_gobDECIL),c(ES_Mtransf_hogTot,ES_Mtransf_hogDECIL)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instDECIL),c(ES_Mestim_alquTot,ES_Mestim_alquDECIL),c(ES_Motros_ingTot,ES_Motros_ingDECIL))
##### ERROR ESTANDAR
c_DECIL_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corDECIL),c(SE_MingtrabTot,SE_MingtrabDECIL),c(SE_MtrabajoTot,SE_MtrabajoDECIL),c(SE_MnegocioTot,SE_MnegocioDECIL)
             ,c(SE_Motros_trabTot,SE_Motros_trabDECIL),c(SE_MrentasTot,SE_MrentasDECIL),c(SE_MutilidadTot,SE_MutilidadDECIL)
             ,c(SE_MarrendaTot,SE_MarrendaDECIL),c(SE_MtransferTot,SE_MtransferDECIL),c(SE_MjubilacionTot,SE_MjubilacionDECIL),c(SE_MbecasTot,SE_MbecasDECIL),
             c(SE_MdonativosTot,SE_MdonativosDECIL),c(SE_MremesasTot,SE_MremesasDECIL),c(SE_Mbene_gobTot,SE_Mbene_gobDECIL),c(SE_Mtransf_hogTot,SE_Mtransf_hogDECIL),c(SE_Mtrans_instTot,SE_Mtrans_instDECIL)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquDECIL),c(SE_Motros_ingTot,SE_Motros_ingDECIL))

##### COEFICIENTE DE VARIACION
c_DECIL_CV <-
  data.frame(c(CV_Ming_corTot,CV_Ming_corDECIL),c(CV_MingtrabTot,CV_MingtrabDECIL),c(CV_MtrabajoTot,CV_MtrabajoDECIL),c(CV_MnegocioTot,CV_MnegocioDECIL)
             ,c(CV_Motros_trabTot,CV_Motros_trabDECIL),c(CV_MrentasTot,CV_MrentasDECIL),c(CV_MutilidadTot,CV_MutilidadDECIL),
             c(CV_MarrendaTot,CV_MarrendaDECIL),c(CV_MtransferTot,CV_MtransferDECIL),c(CV_MjubilacionTot,CV_MjubilacionDECIL),c(CV_MbecasTot,CV_MbecasDECIL)
             ,c(CV_MdonativosTot,CV_MdonativosDECIL),c(CV_MremesasTot,CV_MremesasDECIL),c(CV_Mbene_gobTot,CV_Mbene_gobDECIL),c(CV_Mtransf_hogTot,CV_Mtransf_hogDECIL),c(CV_Mtrans_instTot,CV_Mtrans_instDECIL)
             ,c(CV_Mestim_alquTot,CV_Mestim_alquDECIL),c(CV_Motros_ingTot,CV_Motros_ingDECIL))

##### LIMITE INFERIOR AL 90%
c_DECIL_LI <-
  data.frame(c(LI_Ming_corTot,LI_Ming_corDECIL),c(LI_MingtrabTot,LI_MingtrabDECIL),c(LI_MtrabajoTot,LI_MtrabajoDECIL),
             c(LI_MnegocioTot,LI_MnegocioDECIL),c(LI_Motros_trabTot,LI_Motros_trabDECIL),c(LI_MrentasTot,LI_MrentasDECIL),c(LI_MutilidadTot,LI_MutilidadDECIL),c(LI_MarrendaTot,LI_MarrendaDECIL)
             ,c(LI_MtransferTot,LI_MtransferDECIL),c(LI_MjubilacionTot,LI_MjubilacionDECIL),c(LI_MbecasTot,LI_MbecasDECIL),c(LI_MdonativosTot,LI_MdonativosDECIL)
             ,c(LI_MremesasTot,LI_MremesasDECIL),c(LI_Mbene_gobTot,LI_Mbene_gobDECIL),c(LI_Mtransf_hogTot,LI_Mtransf_hogDECIL),c(LI_Mtrans_instTot,LI_Mtrans_instDECIL)
             ,c(LI_Mestim_alquTot,LI_Mestim_alquDECIL),c(LI_Motros_ingTot,LI_Motros_ingDECIL))

### LIMITE SUPERIOR AL 90%
c_DECIL_LS <-
  data.frame(c(LS_Ming_corTot,LS_Ming_corDECIL),c(LS_MingtrabTot,LS_MingtrabDECIL),c(LS_MtrabajoTot,LS_MtrabajoDECIL),c(LS_MnegocioTot,LS_MnegocioDECIL)
             ,c(LS_Motros_trabTot,LS_Motros_trabDECIL),c(LS_MrentasTot,LS_MrentasDECIL),c(LS_MutilidadTot,LS_MutilidadDECIL),
             c(LS_MarrendaTot,LS_MarrendaDECIL),c(LS_MtransferTot,LS_MtransferDECIL),c(LS_MjubilacionTot,LS_MjubilacionDECIL),c(LS_MbecasTot,LS_MbecasDECIL),
             c(LS_MdonativosTot,LS_MdonativosDECIL),c(LS_MremesasTot,LS_MremesasDECIL),c(LS_Mbene_gobTot,LS_Mbene_gobDECIL),c(LS_Mtransf_hogTot,LS_Mtransf_hogDECIL),c(LS_Mtrans_instTot,LS_Mtrans_instDECIL)
             ,c(LS_Mestim_alquTot,LS_Mestim_alquDECIL),c(LS_Motros_ingTot,LS_Motros_ingDECIL))

# se agregan los nombres de las entidades a las filas
#esta cadena est? bien loca, no?
DECILES<-c("PROMEDIO", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
row.names(c_DECIL_ES)<-row.names(c_DECIL_SE)<-row.names(c_DECIL_CV)<-row.names(c_DECIL_LI)<-row.names(c_DECIL_LS)<-DECILES

#ahora vamos a ponerle nombre a las columnas
names(c_DECIL_ES)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

names(c_DECIL_SE)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

names(c_DECIL_CV)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

names(c_DECIL_LI)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

names(c_DECIL_LS)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla
round(c_DECIL_ES)
round(c_DECIL_SE)
round(c_DECIL_CV,4)*100
round(c_DECIL_LI)
round(c_DECIL_LS)

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")

write.dbf(c_DECIL_ES,file = "Yucatan por fuente por DECIL estimaciones 2010.dbf")
write.dbf(c_DECIL_SE,file = "Yucatan por fuente por DECIL errores standard 2010.dbf")
write.dbf(c_DECIL_CV,file = "Yucatan por fuente por DECIL CV 2010.dbf")
write.dbf(c_DECIL_LI,file = "Yucatan por fuente por DECIL LI 2010.dbf")
write.dbf(c_DECIL_ES,file = "Yucatan por fuente por DECIL LS 2010.dbf")

rm(list = ls())

########## Ingresos por fuente por decil INDIGENA 2010 ############

library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
Conc2010<-read.dbf("ConcYucatan2010.dbf",as.is = T)

names(Conc2010)<-c("ENTIDAD","FOLIOVIV","FOLIOHOG","TOT_INTEG","INGCOR","INGTRAB",
                   "TRABAJO","NEGOCIO","OTROS_TRAB","RENTAS","UTILIDAD","ARRENDA",
                   "TRANSFER","JUBILA","BECA","DONATIVO","REMESA","BENE_GOB",
                   "ESP_HOG","ESP_INST","ESTI","OTROS","FACTOR","UPM",
                   "EST_DIS","tam_loc","Small","HOGARINDIG","NOMBRE_ENT","DEFLACTORES","Nhog","TAM_DEC",
                   "MAXT","ACUMULA","ACUMULA2","DECIL")

Conc2010<-Conc2010 %>%
  filter(HOGARINDIG==1)

mydesign <- svydesign(id=~UPM,strata=~EST_DIS,data=Conc2010,weights=~FACTOR)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~INGCOR,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por decil
#aqu? cmabia la funci?n a svyby, en by va el decil que creamos.
#y al final va la funci?n que queremos
Ming_corDECIL <- svyby(~INGCOR,denominator=~Nhog,by=~DECIL,mydesign,svyratio)


#     Trabajo
#
#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
MingtrabTot <- svyratio(~INGTRAB,denominator=~Nhog,mydesign) # Total promedio
MingtrabDECIL <- svyby(~INGTRAB,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~TRABAJO,denominator=~Nhog,mydesign) # Total promedio
MtrabajoDECIL <- svyby(~TRABAJO,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo independiente
MnegocioTot <- svyratio(~NEGOCIO,denominator=~Nhog,mydesign) # Total promedio
MnegocioDECIL <- svyby(~NEGOCIO,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso de otros trabajos
Motros_trabTot <- svyratio(~OTROS_TRAB,denominator=~Nhog,mydesign) # Total promedio
Motros_trabDECIL<- svyby(~OTROS_TRAB,denominator=~Nhog,by=~DECIL,mydesign,svyratio) # por decil


###################################        Rentas de la propiedad 

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
MrentasTot <- svyratio(~RENTAS,denominator=~Nhog,mydesign) # Total promedio
MrentasDECIL <- svyby(~RENTAS,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #Por decil
###### ingresos de sociedades
MutilidadTot <- svyratio(~UTILIDAD,denominator=~Nhog,mydesign) # Total promedio
MutilidadDECIL <- svyby(~UTILIDAD,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### arrendamiento
MarrendaTot <- svyratio(~ARRENDA,denominator=~Nhog,mydesign) # Total promedio
MarrendaDECIL <- svyby(~ARRENDA,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Por decil


###################################        Transferencias   

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

MtransferTot <- svyratio(~TRANSFER,denominator=~Nhog,mydesign) # Total promedio
MtransferDECIL <- svyby(~TRANSFER,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

MjubilacionTot <- svyratio(~JUBILA,denominator=~Nhog,mydesign) # Total promedio
MjubilacionDECIL <- svyby(~JUBILA,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### becas que pueden ser, de nuevo, p?blicas privadas. 
MbecasTot <- svyratio(~BECA,denominator=~Nhog,mydesign) # Total promedio
MbecasDECIL <- svyby(~BECA,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### donativos que tambi?n pueden ser p?blicos o privados.
MdonativosTot <- svyratio(~DONATIVO,denominator=~Nhog,mydesign) # Total promedio
MdonativosDECIL <- svyby(~DONATIVO,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
MremesasTot <- svyratio(~REMESA,denominator=~Nhog,mydesign) # Total promedio
MremesasDECIL <- svyby(~REMESA,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~BENE_GOB,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobDECIL <- svyby(~BENE_GOB,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~ESP_HOG,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogDECIL <- svyby(~ESP_HOG,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #decil

###### trans_inst: puede venir de institucione sp?blicas o privadas.
Mtrans_instTot <- svyratio(~ESP_INST,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instDECIL <- svyby(~ESP_INST,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL


### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~ESTI,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquDECIL <- svyby(~ESTI,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil


### otros_ing ### es literalmente ?algo m?s?
Motros_ingTot <- svyratio(~OTROS,denominator=~Nhog,mydesign) # Total promedio
Motros_ingDECIL <- svyby(~OTROS,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Decil


######################################### Estimaciones 

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corDECIL <- Ming_corDECIL[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

ES_MingtrabTot <- MingtrabTot[[1]]
ES_MingtrabDECIL <- MingtrabDECIL[[2]]

ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_MtrabajoDECIL <- MtrabajoDECIL[[2]]

ES_MnegocioTot <- MnegocioTot[[1]]
ES_MnegocioDECIL <- MnegocioDECIL[[2]]

ES_Motros_trabTot <- Motros_trabTot [[1]]
ES_Motros_trabDECIL <- Motros_trabDECIL [[2]]

ES_MrentasTot <- MrentasTot [[1]]
ES_MrentasDECIL <- MrentasDECIL [[2]]

ES_MutilidadTot <- MutilidadTot [[1]]
ES_MutilidadDECIL <- MutilidadDECIL [[2]]

ES_MarrendaTot <- MarrendaTot [[1]]
ES_MarrendaDECIL <- MarrendaDECIL [[2]]

ES_MtransferTot <- MtransferTot[[1]]
ES_MtransferDECIL <- MtransferDECIL[[2]]

ES_MjubilacionTot <- MjubilacionTot [[1]]
ES_MjubilacionDECIL <- MjubilacionDECIL [[2]]

ES_MbecasTot <- MbecasTot [[1]]
ES_MbecasDECIL <- MbecasDECIL [[2]]

ES_MdonativosTot <- MdonativosTot[[1]]
ES_MdonativosDECIL <- MdonativosDECIL[[2]]

ES_MremesasTot <- MremesasTot[[1]]
ES_MremesasDECIL <- MremesasDECIL[[2]]

ES_Mbene_gobTot <- Mbene_gobTot [[1]]
ES_Mbene_gobDECIL <- Mbene_gobDECIL [[2]]

ES_Mtransf_hogTot <- Mtransf_hogTot [[1]]
ES_Mtransf_hogDECIL <- Mtransf_hogDECIL [[2]]

ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instDECIL <- Mtrans_instDECIL[[2]]

ES_Mestim_alquTot <- Mestim_alquTot [[1]]
ES_Mestim_alquDECIL <- Mestim_alquDECIL [[2]]

ES_Motros_ingTot <- Motros_ingTot [[1]]
ES_Motros_ingDECIL <- Motros_ingDECIL [[2]]

########## Error Est?ndar 
SE_Ming_corTot <- SE (Ming_corTot)
SE_Ming_corDECIL <- SE (Ming_corDECIL)

SE_MingtrabTot <- SE (MingtrabTot)
SE_MingtrabDECIL <- SE (MingtrabDECIL)

SE_MtrabajoTot <- SE (MtrabajoTot)
SE_MtrabajoDECIL <- SE (MtrabajoDECIL)

SE_MnegocioTot <- SE (MnegocioTot)
SE_MnegocioDECIL <- SE (MnegocioDECIL)

SE_Motros_trabTot <- SE (Motros_trabTot)
SE_Motros_trabDECIL <- SE (Motros_trabDECIL)

SE_MrentasTot <- SE (MrentasTot)
SE_MrentasDECIL <- SE (MrentasDECIL)

SE_MutilidadTot <- SE (MutilidadTot)
SE_MutilidadDECIL <- SE (MutilidadDECIL)

SE_MarrendaTot <- SE (MarrendaTot)
SE_MarrendaDECIL <- SE (MarrendaDECIL)

SE_MtransferTot <- SE (MtransferTot)
SE_MtransferDECIL <- SE (MtransferDECIL)

SE_MjubilacionTot <- SE (MjubilacionTot)
SE_MjubilacionDECIL <- SE (MjubilacionDECIL)

SE_MbecasTot <- SE (MbecasTot)
SE_MbecasDECIL <- SE (MbecasDECIL)

SE_MdonativosTot <- SE (MdonativosTot)
SE_MdonativosDECIL <- SE (MdonativosDECIL)

SE_MremesasTot <- SE (MremesasTot)
SE_MremesasDECIL <- SE (MremesasDECIL)

SE_Mbene_gobTot <- SE (Mbene_gobTot)
SE_Mbene_gobDECIL <- SE (Mbene_gobDECIL)

SE_Mtransf_hogTot <- SE (Mtransf_hogTot)
SE_Mtransf_hogDECIL <- SE (Mtransf_hogDECIL)

SE_Mtrans_instTot <- SE (Mtrans_instTot)
SE_Mtrans_instDECIL <- SE (Mtrans_instDECIL)

SE_Mestim_alquTot <- SE (Mestim_alquTot)
SE_Mestim_alquDECIL <- SE (Mestim_alquDECIL)

SE_Motros_ingTot <- SE (Motros_ingTot)
SE_Motros_ingDECIL <- SE (Motros_ingDECIL)

########## Coeficiente de variaci?n 
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corDECIL <- cv(Ming_corDECIL)

CV_MingtrabTot <- cv(MingtrabTot)
CV_MingtrabDECIL <- cv(MingtrabDECIL)

CV_MtrabajoTot <- cv(MtrabajoTot)
CV_MtrabajoDECIL <- cv(MtrabajoDECIL)

CV_MnegocioTot <- cv(MnegocioTot)
CV_MnegocioDECIL <- cv(MnegocioDECIL)

CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabDECIL <- cv(Motros_trabDECIL)

CV_MrentasTot <- cv(MrentasTot)
CV_MrentasDECIL <- cv(MrentasDECIL)

CV_MutilidadTot <- cv(MutilidadTot)
CV_MutilidadDECIL <- cv(MutilidadDECIL)

CV_MarrendaTot <- cv(MarrendaTot)
CV_MarrendaDECIL <- cv(MarrendaDECIL)

CV_MtransferTot <- cv(MtransferTot)
CV_MtransferDECIL <- cv(MtransferDECIL)

CV_MjubilacionTot <- cv(MjubilacionTot)
CV_MjubilacionDECIL <- cv(MjubilacionDECIL)

CV_MbecasTot <- cv(MbecasTot)
CV_MbecasDECIL <- cv(MbecasDECIL)

CV_MdonativosTot <- cv(MdonativosTot)
CV_MdonativosDECIL <- cv(MdonativosDECIL)

CV_MremesasTot <- cv(MremesasTot)
CV_MremesasDECIL <- cv(MremesasDECIL)

CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobDECIL <- cv(Mbene_gobDECIL)

CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogDECIL <- cv(Mtransf_hogDECIL)

CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instDECIL <- cv(Mtrans_instDECIL)

CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquDECIL <- cv(Mestim_alquDECIL)

CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingDECIL <- cv(Motros_ingDECIL)

########## Limite inferior 
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,1]

LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,1]

LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,1]

LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,1]

LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,1]

LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,1]

LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,1]

LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,1]

LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,1]

LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1]
LI_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,1]

LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,1]

LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,1]

LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,1]

LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,1]

LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,1]

LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,1]

LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,1
]
LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingDECIL <- confint(Motros_ingDECIL ,level=0.90)[,1]

########## Limite superior 
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,2]

LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,2]

LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,2]

LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,2]

LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,2]

LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,2]

LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,2]

LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,2]

LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,2]

LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,2]

LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,2]

LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,2]

LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,2]

LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,2]

LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,2]

LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,2]

LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,2]

LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingDECIL <- confint(Motros_ingDECIL,level=0.90)[,2]

#############################      Cuadros   
#este cuadro, lo ?nico que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_DECIL_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corDECIL),c(ES_MingtrabTot,ES_MingtrabDECIL),c(ES_MtrabajoTot,ES_MtrabajoDECIL),c(ES_MnegocioTot,ES_MnegocioDECIL)
             ,c(ES_Motros_trabTot,ES_Motros_trabDECIL),c(ES_MrentasTot,ES_MrentasDECIL),c(ES_MutilidadTot,ES_MutilidadDECIL)
             ,c(ES_MarrendaTot,ES_MarrendaDECIL),c(ES_MtransferTot,ES_MtransferDECIL),c(ES_MjubilacionTot,ES_MjubilacionDECIL),c(ES_MbecasTot,ES_MbecasDECIL),
             c(ES_MdonativosTot,ES_MdonativosDECIL),c(ES_MremesasTot,ES_MremesasDECIL),c(ES_Mbene_gobTot,ES_Mbene_gobDECIL),c(ES_Mtransf_hogTot,ES_Mtransf_hogDECIL)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instDECIL),c(ES_Mestim_alquTot,ES_Mestim_alquDECIL),c(ES_Motros_ingTot,ES_Motros_ingDECIL))
##### ERROR ESTANDAR
c_DECIL_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corDECIL),c(SE_MingtrabTot,SE_MingtrabDECIL),c(SE_MtrabajoTot,SE_MtrabajoDECIL),c(SE_MnegocioTot,SE_MnegocioDECIL)
             ,c(SE_Motros_trabTot,SE_Motros_trabDECIL),c(SE_MrentasTot,SE_MrentasDECIL),c(SE_MutilidadTot,SE_MutilidadDECIL)
             ,c(SE_MarrendaTot,SE_MarrendaDECIL),c(SE_MtransferTot,SE_MtransferDECIL),c(SE_MjubilacionTot,SE_MjubilacionDECIL),c(SE_MbecasTot,SE_MbecasDECIL),
             c(SE_MdonativosTot,SE_MdonativosDECIL),c(SE_MremesasTot,SE_MremesasDECIL),c(SE_Mbene_gobTot,SE_Mbene_gobDECIL),c(SE_Mtransf_hogTot,SE_Mtransf_hogDECIL),c(SE_Mtrans_instTot,SE_Mtrans_instDECIL)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquDECIL),c(SE_Motros_ingTot,SE_Motros_ingDECIL))

##### COEFICIENTE DE VARIACION
c_DECIL_CV <-
  data.frame(c(CV_Ming_corTot,CV_Ming_corDECIL),c(CV_MingtrabTot,CV_MingtrabDECIL),c(CV_MtrabajoTot,CV_MtrabajoDECIL),c(CV_MnegocioTot,CV_MnegocioDECIL)
             ,c(CV_Motros_trabTot,CV_Motros_trabDECIL),c(CV_MrentasTot,CV_MrentasDECIL),c(CV_MutilidadTot,CV_MutilidadDECIL),
             c(CV_MarrendaTot,CV_MarrendaDECIL),c(CV_MtransferTot,CV_MtransferDECIL),c(CV_MjubilacionTot,CV_MjubilacionDECIL),c(CV_MbecasTot,CV_MbecasDECIL)
             ,c(CV_MdonativosTot,CV_MdonativosDECIL),c(CV_MremesasTot,CV_MremesasDECIL),c(CV_Mbene_gobTot,CV_Mbene_gobDECIL),c(CV_Mtransf_hogTot,CV_Mtransf_hogDECIL),c(CV_Mtrans_instTot,CV_Mtrans_instDECIL)
             ,c(CV_Mestim_alquTot,CV_Mestim_alquDECIL),c(CV_Motros_ingTot,CV_Motros_ingDECIL))

##### LIMITE INFERIOR AL 90%
c_DECIL_LI <-
  data.frame(c(LI_Ming_corTot,LI_Ming_corDECIL),c(LI_MingtrabTot,LI_MingtrabDECIL),c(LI_MtrabajoTot,LI_MtrabajoDECIL),
             c(LI_MnegocioTot,LI_MnegocioDECIL),c(LI_Motros_trabTot,LI_Motros_trabDECIL),c(LI_MrentasTot,LI_MrentasDECIL),c(LI_MutilidadTot,LI_MutilidadDECIL),c(LI_MarrendaTot,LI_MarrendaDECIL)
             ,c(LI_MtransferTot,LI_MtransferDECIL),c(LI_MjubilacionTot,LI_MjubilacionDECIL),c(LI_MbecasTot,LI_MbecasDECIL),c(LI_MdonativosTot,LI_MdonativosDECIL)
             ,c(LI_MremesasTot,LI_MremesasDECIL),c(LI_Mbene_gobTot,LI_Mbene_gobDECIL),c(LI_Mtransf_hogTot,LI_Mtransf_hogDECIL),c(LI_Mtrans_instTot,LI_Mtrans_instDECIL)
             ,c(LI_Mestim_alquTot,LI_Mestim_alquDECIL),c(LI_Motros_ingTot,LI_Motros_ingDECIL))

### LIMITE SUPERIOR AL 90%
c_DECIL_LS <-
  data.frame(c(LS_Ming_corTot,LS_Ming_corDECIL),c(LS_MingtrabTot,LS_MingtrabDECIL),c(LS_MtrabajoTot,LS_MtrabajoDECIL),c(LS_MnegocioTot,LS_MnegocioDECIL)
             ,c(LS_Motros_trabTot,LS_Motros_trabDECIL),c(LS_MrentasTot,LS_MrentasDECIL),c(LS_MutilidadTot,LS_MutilidadDECIL),
             c(LS_MarrendaTot,LS_MarrendaDECIL),c(LS_MtransferTot,LS_MtransferDECIL),c(LS_MjubilacionTot,LS_MjubilacionDECIL),c(LS_MbecasTot,LS_MbecasDECIL),
             c(LS_MdonativosTot,LS_MdonativosDECIL),c(LS_MremesasTot,LS_MremesasDECIL),c(LS_Mbene_gobTot,LS_Mbene_gobDECIL),c(LS_Mtransf_hogTot,LS_Mtransf_hogDECIL),c(LS_Mtrans_instTot,LS_Mtrans_instDECIL)
             ,c(LS_Mestim_alquTot,LS_Mestim_alquDECIL),c(LS_Motros_ingTot,LS_Motros_ingDECIL))

# se agregan los nombres de las entidades a las filas
#esta cadena est? bien loca, no?
DECILES<-c("PROMEDIO", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
row.names(c_DECIL_ES)<-row.names(c_DECIL_SE)<-row.names(c_DECIL_CV)<-row.names(c_DECIL_LI)<-row.names(c_DECIL_LS)<-DECILES

#ahora vamos a ponerle nombre a las columnas
names(c_DECIL_ES)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

names(c_DECIL_SE)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

names(c_DECIL_CV)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

names(c_DECIL_LI)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

names(c_DECIL_LS)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla
round(c_DECIL_ES)
round(c_DECIL_SE)
round(c_DECIL_CV,4)*100
round(c_DECIL_LI)
round(c_DECIL_LS)

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")

write.dbf(c_DECIL_ES,file = "Yucatan INDIGENA por fuente por DECIL estimaciones 2010.dbf")
write.dbf(c_DECIL_SE,file = "Yucatan INDIGENA por fuente por DECIL errores standard 2010.dbf")
write.dbf(c_DECIL_CV,file = "Yucatan INDIGENA por fuente por DECIL CV 2010.dbf")
write.dbf(c_DECIL_LI,file = "Yucatan INDIGENA por fuente por DECIL LI 2010.dbf")
write.dbf(c_DECIL_ES,file = "Yucatan INDIGENA por fuente por DECIL LS 2010.dbf")

rm(list = ls())

########## Ingresos por fuente por decil NO 2010 ############

library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
Conc2010<-read.dbf("ConcYucatan2010.dbf",as.is = T)

names(Conc2010)<-c("ENTIDAD","FOLIOVIV","FOLIOHOG","TOT_INTEG","INGCOR","INGTRAB",
                   "TRABAJO","NEGOCIO","OTROS_TRAB","RENTAS","UTILIDAD","ARRENDA",
                   "TRANSFER","JUBILA","BECA","DONATIVO","REMESA","BENE_GOB",
                   "ESP_HOG","ESP_INST","ESTI","OTROS","FACTOR","UPM",
                   "EST_DIS","tam_loc","Small","HOGARINDIG","NOMBRE_ENT","DEFLACTORES","Nhog","TAM_DEC",
                   "MAXT","ACUMULA","ACUMULA2","DECIL")


Conc2010<-Conc2010 %>%
  filter(HOGARINDIG==0)


mydesign <- svydesign(id=~UPM,strata=~EST_DIS,data=Conc2010,weights=~FACTOR)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~INGCOR,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por decil
#aqu? cmabia la funci?n a svyby, en by va el decil que creamos.
#y al final va la funci?n que queremos
Ming_corDECIL <- svyby(~INGCOR,denominator=~Nhog,by=~DECIL,mydesign,svyratio)


#     Trabajo
#
#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
MingtrabTot <- svyratio(~INGTRAB,denominator=~Nhog,mydesign) # Total promedio
MingtrabDECIL <- svyby(~INGTRAB,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~TRABAJO,denominator=~Nhog,mydesign) # Total promedio
MtrabajoDECIL <- svyby(~TRABAJO,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo independiente
MnegocioTot <- svyratio(~NEGOCIO,denominator=~Nhog,mydesign) # Total promedio
MnegocioDECIL <- svyby(~NEGOCIO,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso de otros trabajos
Motros_trabTot <- svyratio(~OTROS_TRAB,denominator=~Nhog,mydesign) # Total promedio
Motros_trabDECIL<- svyby(~OTROS_TRAB,denominator=~Nhog,by=~DECIL,mydesign,svyratio) # por decil


###################################        Rentas de la propiedad 

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
MrentasTot <- svyratio(~RENTAS,denominator=~Nhog,mydesign) # Total promedio
MrentasDECIL <- svyby(~RENTAS,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #Por decil
###### ingresos de sociedades
MutilidadTot <- svyratio(~UTILIDAD,denominator=~Nhog,mydesign) # Total promedio
MutilidadDECIL <- svyby(~UTILIDAD,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### arrendamiento
MarrendaTot <- svyratio(~ARRENDA,denominator=~Nhog,mydesign) # Total promedio
MarrendaDECIL <- svyby(~ARRENDA,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Por decil


###################################        Transferencias   

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

MtransferTot <- svyratio(~TRANSFER,denominator=~Nhog,mydesign) # Total promedio
MtransferDECIL <- svyby(~TRANSFER,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

MjubilacionTot <- svyratio(~JUBILA,denominator=~Nhog,mydesign) # Total promedio
MjubilacionDECIL <- svyby(~JUBILA,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### becas que pueden ser, de nuevo, p?blicas privadas. 
MbecasTot <- svyratio(~BECA,denominator=~Nhog,mydesign) # Total promedio
MbecasDECIL <- svyby(~BECA,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### donativos que tambi?n pueden ser p?blicos o privados.
MdonativosTot <- svyratio(~DONATIVO,denominator=~Nhog,mydesign) # Total promedio
MdonativosDECIL <- svyby(~DONATIVO,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
MremesasTot <- svyratio(~REMESA,denominator=~Nhog,mydesign) # Total promedio
MremesasDECIL <- svyby(~REMESA,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~BENE_GOB,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobDECIL <- svyby(~BENE_GOB,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~ESP_HOG,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogDECIL <- svyby(~ESP_HOG,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #decil

###### trans_inst: puede venir de institucione sp?blicas o privadas.
Mtrans_instTot <- svyratio(~ESP_INST,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instDECIL <- svyby(~ESP_INST,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL


### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~ESTI,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquDECIL <- svyby(~ESTI,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil


### otros_ing ### es literalmente ?algo m?s?
Motros_ingTot <- svyratio(~OTROS,denominator=~Nhog,mydesign) # Total promedio
Motros_ingDECIL <- svyby(~OTROS,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Decil


######################################### Estimaciones 

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corDECIL <- Ming_corDECIL[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

ES_MingtrabTot <- MingtrabTot[[1]]
ES_MingtrabDECIL <- MingtrabDECIL[[2]]

ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_MtrabajoDECIL <- MtrabajoDECIL[[2]]

ES_MnegocioTot <- MnegocioTot[[1]]
ES_MnegocioDECIL <- MnegocioDECIL[[2]]

ES_Motros_trabTot <- Motros_trabTot [[1]]
ES_Motros_trabDECIL <- Motros_trabDECIL [[2]]

ES_MrentasTot <- MrentasTot [[1]]
ES_MrentasDECIL <- MrentasDECIL [[2]]

ES_MutilidadTot <- MutilidadTot [[1]]
ES_MutilidadDECIL <- MutilidadDECIL [[2]]

ES_MarrendaTot <- MarrendaTot [[1]]
ES_MarrendaDECIL <- MarrendaDECIL [[2]]

ES_MtransferTot <- MtransferTot[[1]]
ES_MtransferDECIL <- MtransferDECIL[[2]]

ES_MjubilacionTot <- MjubilacionTot [[1]]
ES_MjubilacionDECIL <- MjubilacionDECIL [[2]]

ES_MbecasTot <- MbecasTot [[1]]
ES_MbecasDECIL <- MbecasDECIL [[2]]

ES_MdonativosTot <- MdonativosTot[[1]]
ES_MdonativosDECIL <- MdonativosDECIL[[2]]

ES_MremesasTot <- MremesasTot[[1]]
ES_MremesasDECIL <- MremesasDECIL[[2]]

ES_Mbene_gobTot <- Mbene_gobTot [[1]]
ES_Mbene_gobDECIL <- Mbene_gobDECIL [[2]]

ES_Mtransf_hogTot <- Mtransf_hogTot [[1]]
ES_Mtransf_hogDECIL <- Mtransf_hogDECIL [[2]]

ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instDECIL <- Mtrans_instDECIL[[2]]

ES_Mestim_alquTot <- Mestim_alquTot [[1]]
ES_Mestim_alquDECIL <- Mestim_alquDECIL [[2]]

ES_Motros_ingTot <- Motros_ingTot [[1]]
ES_Motros_ingDECIL <- Motros_ingDECIL [[2]]

########## Error Est?ndar 
SE_Ming_corTot <- SE (Ming_corTot)
SE_Ming_corDECIL <- SE (Ming_corDECIL)

SE_MingtrabTot <- SE (MingtrabTot)
SE_MingtrabDECIL <- SE (MingtrabDECIL)

SE_MtrabajoTot <- SE (MtrabajoTot)
SE_MtrabajoDECIL <- SE (MtrabajoDECIL)

SE_MnegocioTot <- SE (MnegocioTot)
SE_MnegocioDECIL <- SE (MnegocioDECIL)

SE_Motros_trabTot <- SE (Motros_trabTot)
SE_Motros_trabDECIL <- SE (Motros_trabDECIL)

SE_MrentasTot <- SE (MrentasTot)
SE_MrentasDECIL <- SE (MrentasDECIL)

SE_MutilidadTot <- SE (MutilidadTot)
SE_MutilidadDECIL <- SE (MutilidadDECIL)

SE_MarrendaTot <- SE (MarrendaTot)
SE_MarrendaDECIL <- SE (MarrendaDECIL)

SE_MtransferTot <- SE (MtransferTot)
SE_MtransferDECIL <- SE (MtransferDECIL)

SE_MjubilacionTot <- SE (MjubilacionTot)
SE_MjubilacionDECIL <- SE (MjubilacionDECIL)

SE_MbecasTot <- SE (MbecasTot)
SE_MbecasDECIL <- SE (MbecasDECIL)

SE_MdonativosTot <- SE (MdonativosTot)
SE_MdonativosDECIL <- SE (MdonativosDECIL)

SE_MremesasTot <- SE (MremesasTot)
SE_MremesasDECIL <- SE (MremesasDECIL)

SE_Mbene_gobTot <- SE (Mbene_gobTot)
SE_Mbene_gobDECIL <- SE (Mbene_gobDECIL)

SE_Mtransf_hogTot <- SE (Mtransf_hogTot)
SE_Mtransf_hogDECIL <- SE (Mtransf_hogDECIL)

SE_Mtrans_instTot <- SE (Mtrans_instTot)
SE_Mtrans_instDECIL <- SE (Mtrans_instDECIL)

SE_Mestim_alquTot <- SE (Mestim_alquTot)
SE_Mestim_alquDECIL <- SE (Mestim_alquDECIL)

SE_Motros_ingTot <- SE (Motros_ingTot)
SE_Motros_ingDECIL <- SE (Motros_ingDECIL)

########## Coeficiente de variaci?n 
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corDECIL <- cv(Ming_corDECIL)

CV_MingtrabTot <- cv(MingtrabTot)
CV_MingtrabDECIL <- cv(MingtrabDECIL)

CV_MtrabajoTot <- cv(MtrabajoTot)
CV_MtrabajoDECIL <- cv(MtrabajoDECIL)

CV_MnegocioTot <- cv(MnegocioTot)
CV_MnegocioDECIL <- cv(MnegocioDECIL)

CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabDECIL <- cv(Motros_trabDECIL)

CV_MrentasTot <- cv(MrentasTot)
CV_MrentasDECIL <- cv(MrentasDECIL)

CV_MutilidadTot <- cv(MutilidadTot)
CV_MutilidadDECIL <- cv(MutilidadDECIL)

CV_MarrendaTot <- cv(MarrendaTot)
CV_MarrendaDECIL <- cv(MarrendaDECIL)

CV_MtransferTot <- cv(MtransferTot)
CV_MtransferDECIL <- cv(MtransferDECIL)

CV_MjubilacionTot <- cv(MjubilacionTot)
CV_MjubilacionDECIL <- cv(MjubilacionDECIL)

CV_MbecasTot <- cv(MbecasTot)
CV_MbecasDECIL <- cv(MbecasDECIL)

CV_MdonativosTot <- cv(MdonativosTot)
CV_MdonativosDECIL <- cv(MdonativosDECIL)

CV_MremesasTot <- cv(MremesasTot)
CV_MremesasDECIL <- cv(MremesasDECIL)

CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobDECIL <- cv(Mbene_gobDECIL)

CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogDECIL <- cv(Mtransf_hogDECIL)

CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instDECIL <- cv(Mtrans_instDECIL)

CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquDECIL <- cv(Mestim_alquDECIL)

CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingDECIL <- cv(Motros_ingDECIL)

########## Limite inferior 
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,1]

LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,1]

LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,1]

LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,1]

LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,1]

LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,1]

LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,1]

LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,1]

LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,1]

LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1]
LI_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,1]

LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,1]

LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,1]

LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,1]

LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,1]

LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,1]

LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,1]

LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,1
]
LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingDECIL <- confint(Motros_ingDECIL ,level=0.90)[,1]

########## Limite superior 
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,2]

LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,2]

LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,2]

LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,2]

LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,2]

LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,2]

LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,2]

LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,2]

LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,2]

LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,2]

LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,2]

LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,2]

LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,2]

LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,2]

LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,2]

LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,2]

LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,2]

LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingDECIL <- confint(Motros_ingDECIL,level=0.90)[,2]

#############################      Cuadros   
#este cuadro, lo ?nico que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_DECIL_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corDECIL),c(ES_MingtrabTot,ES_MingtrabDECIL),c(ES_MtrabajoTot,ES_MtrabajoDECIL),c(ES_MnegocioTot,ES_MnegocioDECIL)
             ,c(ES_Motros_trabTot,ES_Motros_trabDECIL),c(ES_MrentasTot,ES_MrentasDECIL),c(ES_MutilidadTot,ES_MutilidadDECIL)
             ,c(ES_MarrendaTot,ES_MarrendaDECIL),c(ES_MtransferTot,ES_MtransferDECIL),c(ES_MjubilacionTot,ES_MjubilacionDECIL),c(ES_MbecasTot,ES_MbecasDECIL),
             c(ES_MdonativosTot,ES_MdonativosDECIL),c(ES_MremesasTot,ES_MremesasDECIL),c(ES_Mbene_gobTot,ES_Mbene_gobDECIL),c(ES_Mtransf_hogTot,ES_Mtransf_hogDECIL)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instDECIL),c(ES_Mestim_alquTot,ES_Mestim_alquDECIL),c(ES_Motros_ingTot,ES_Motros_ingDECIL))
##### ERROR ESTANDAR
c_DECIL_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corDECIL),c(SE_MingtrabTot,SE_MingtrabDECIL),c(SE_MtrabajoTot,SE_MtrabajoDECIL),c(SE_MnegocioTot,SE_MnegocioDECIL)
             ,c(SE_Motros_trabTot,SE_Motros_trabDECIL),c(SE_MrentasTot,SE_MrentasDECIL),c(SE_MutilidadTot,SE_MutilidadDECIL)
             ,c(SE_MarrendaTot,SE_MarrendaDECIL),c(SE_MtransferTot,SE_MtransferDECIL),c(SE_MjubilacionTot,SE_MjubilacionDECIL),c(SE_MbecasTot,SE_MbecasDECIL),
             c(SE_MdonativosTot,SE_MdonativosDECIL),c(SE_MremesasTot,SE_MremesasDECIL),c(SE_Mbene_gobTot,SE_Mbene_gobDECIL),c(SE_Mtransf_hogTot,SE_Mtransf_hogDECIL),c(SE_Mtrans_instTot,SE_Mtrans_instDECIL)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquDECIL),c(SE_Motros_ingTot,SE_Motros_ingDECIL))

##### COEFICIENTE DE VARIACION
c_DECIL_CV <-
  data.frame(c(CV_Ming_corTot,CV_Ming_corDECIL),c(CV_MingtrabTot,CV_MingtrabDECIL),c(CV_MtrabajoTot,CV_MtrabajoDECIL),c(CV_MnegocioTot,CV_MnegocioDECIL)
             ,c(CV_Motros_trabTot,CV_Motros_trabDECIL),c(CV_MrentasTot,CV_MrentasDECIL),c(CV_MutilidadTot,CV_MutilidadDECIL),
             c(CV_MarrendaTot,CV_MarrendaDECIL),c(CV_MtransferTot,CV_MtransferDECIL),c(CV_MjubilacionTot,CV_MjubilacionDECIL),c(CV_MbecasTot,CV_MbecasDECIL)
             ,c(CV_MdonativosTot,CV_MdonativosDECIL),c(CV_MremesasTot,CV_MremesasDECIL),c(CV_Mbene_gobTot,CV_Mbene_gobDECIL),c(CV_Mtransf_hogTot,CV_Mtransf_hogDECIL),c(CV_Mtrans_instTot,CV_Mtrans_instDECIL)
             ,c(CV_Mestim_alquTot,CV_Mestim_alquDECIL),c(CV_Motros_ingTot,CV_Motros_ingDECIL))

##### LIMITE INFERIOR AL 90%
c_DECIL_LI <-
  data.frame(c(LI_Ming_corTot,LI_Ming_corDECIL),c(LI_MingtrabTot,LI_MingtrabDECIL),c(LI_MtrabajoTot,LI_MtrabajoDECIL),
             c(LI_MnegocioTot,LI_MnegocioDECIL),c(LI_Motros_trabTot,LI_Motros_trabDECIL),c(LI_MrentasTot,LI_MrentasDECIL),c(LI_MutilidadTot,LI_MutilidadDECIL),c(LI_MarrendaTot,LI_MarrendaDECIL)
             ,c(LI_MtransferTot,LI_MtransferDECIL),c(LI_MjubilacionTot,LI_MjubilacionDECIL),c(LI_MbecasTot,LI_MbecasDECIL),c(LI_MdonativosTot,LI_MdonativosDECIL)
             ,c(LI_MremesasTot,LI_MremesasDECIL),c(LI_Mbene_gobTot,LI_Mbene_gobDECIL),c(LI_Mtransf_hogTot,LI_Mtransf_hogDECIL),c(LI_Mtrans_instTot,LI_Mtrans_instDECIL)
             ,c(LI_Mestim_alquTot,LI_Mestim_alquDECIL),c(LI_Motros_ingTot,LI_Motros_ingDECIL))

### LIMITE SUPERIOR AL 90%
c_DECIL_LS <-
  data.frame(c(LS_Ming_corTot,LS_Ming_corDECIL),c(LS_MingtrabTot,LS_MingtrabDECIL),c(LS_MtrabajoTot,LS_MtrabajoDECIL),c(LS_MnegocioTot,LS_MnegocioDECIL)
             ,c(LS_Motros_trabTot,LS_Motros_trabDECIL),c(LS_MrentasTot,LS_MrentasDECIL),c(LS_MutilidadTot,LS_MutilidadDECIL),
             c(LS_MarrendaTot,LS_MarrendaDECIL),c(LS_MtransferTot,LS_MtransferDECIL),c(LS_MjubilacionTot,LS_MjubilacionDECIL),c(LS_MbecasTot,LS_MbecasDECIL),
             c(LS_MdonativosTot,LS_MdonativosDECIL),c(LS_MremesasTot,LS_MremesasDECIL),c(LS_Mbene_gobTot,LS_Mbene_gobDECIL),c(LS_Mtransf_hogTot,LS_Mtransf_hogDECIL),c(LS_Mtrans_instTot,LS_Mtrans_instDECIL)
             ,c(LS_Mestim_alquTot,LS_Mestim_alquDECIL),c(LS_Motros_ingTot,LS_Motros_ingDECIL))

# se agregan los nombres de las entidades a las filas
#esta cadena est? bien loca, no?
DECILES<-c("PROMEDIO", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
row.names(c_DECIL_ES)<-row.names(c_DECIL_SE)<-row.names(c_DECIL_CV)<-row.names(c_DECIL_LI)<-row.names(c_DECIL_LS)<-DECILES

#ahora vamos a ponerle nombre a las columnas
names(c_DECIL_ES)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

names(c_DECIL_SE)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

names(c_DECIL_CV)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

names(c_DECIL_LI)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

names(c_DECIL_LS)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla
round(c_DECIL_ES)
round(c_DECIL_SE)
round(c_DECIL_CV,4)*100
round(c_DECIL_LI)
round(c_DECIL_LS)

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")

write.dbf(c_DECIL_ES,file = "Yucatan NO por fuente por DECIL estimaciones 2010.dbf")
write.dbf(c_DECIL_SE,file = "Yucatan NO por fuente por DECIL errores standard 2010.dbf")
write.dbf(c_DECIL_CV,file = "Yucatan NO por fuente por DECIL CV 2010.dbf")
write.dbf(c_DECIL_LI,file = "Yucatan NO por fuente por DECIL LI 2010.dbf")
write.dbf(c_DECIL_ES,file = "Yucatan NO por fuente por DECIL LS 2010.dbf")

rm(list = ls())

########## Creación tablas ingresos total 2018 ########

library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
Conc2018<-read.dbf("ConcYucatan2018.dbf",as.is = T)

mydesign <- svydesign(id=~upm,strata=~est_dis,data=Conc2018,weights=~factor)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~ing_cor,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por decil
#aqu? cmabia la funci?n a svyby, en by va el decil que creamos.
#y al final va la funci?n que queremos
Ming_corDECIL <- svyby(~ing_cor,denominator=~Nhog,by=~DECIL,mydesign,svyratio)


#      Trabajo

#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
MingtrabTot <- svyratio(~ingtrab,denominator=~Nhog,mydesign) # Total promedio
MingtrabDECIL <- svyby(~ingtrab,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~trabajo,denominator=~Nhog,mydesign) # Total promedio
MtrabajoDECIL <- svyby(~trabajo,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo independiente
MnegocioTot <- svyratio(~negocio,denominator=~Nhog,mydesign) # Total promedio
MnegocioDECIL <- svyby(~negocio,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso de otros trabajos
Motros_trabTot <- svyratio(~otros_trab,denominator=~Nhog,mydesign) # Total promedio
Motros_trabDECIL<- svyby(~otros_trab,denominator=~Nhog,by=~DECIL,mydesign,svyratio) # por decil


# Rentas de la propiedad

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
MrentasTot <- svyratio(~rentas,denominator=~Nhog,mydesign) # Total promedio
MrentasDECIL <- svyby(~rentas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #Por decil
###### ingresos de sociedades
MutilidadTot <- svyratio(~utilidad,denominator=~Nhog,mydesign) # Total promedio
MutilidadDECIL <- svyby(~utilidad,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### arrendamiento
MarrendaTot <- svyratio(~arrenda,denominator=~Nhog,mydesign) # Total promedio
MarrendaDECIL <- svyby(~arrenda,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Por decil


#       Transferencias 

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

MtransferTot <- svyratio(~transfer,denominator=~Nhog,mydesign) # Total promedio
MtransferDECIL <- svyby(~transfer,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

MjubilacionTot <- svyratio(~jubilacion,denominator=~Nhog,mydesign) # Total promedio
MjubilacionDECIL <- svyby(~jubilacion,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### becas que pueden ser, de nuevo, p?blicas privadas. 
MbecasTot <- svyratio(~becas,denominator=~Nhog,mydesign) # Total promedio
MbecasDECIL <- svyby(~becas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### donativos que tambi?n pueden ser p?blicos o privados.
MdonativosTot <- svyratio(~donativos,denominator=~Nhog,mydesign) # Total promedio
MdonativosDECIL <- svyby(~donativos,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
MremesasTot <- svyratio(~remesas,denominator=~Nhog,mydesign) # Total promedio
MremesasDECIL <- svyby(~remesas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~bene_gob,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobDECIL <- svyby(~bene_gob,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~transf_hog,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogDECIL <- svyby(~transf_hog,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #decil

###### trans_inst: puede venir de institucione sp?blicas o privadas.
Mtrans_instTot <- svyratio(~trans_inst,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instDECIL <- svyby(~trans_inst,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~estim_alqu,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquDECIL <- svyby(~estim_alqu,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

### otros_ing ### es literalmente ?algo m?s?
Motros_ingTot <- svyratio(~otros_ing,denominator=~Nhog,mydesign) # Total promedio
Motros_ingDECIL <- svyby(~otros_ing,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Decil



# Estimaciones 

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corDECIL <- Ming_corDECIL[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

ES_MingtrabTot <- MingtrabTot[[1]]
ES_MingtrabDECIL <- MingtrabDECIL[[2]]

ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_MtrabajoDECIL <- MtrabajoDECIL[[2]]

ES_MnegocioTot <- MnegocioTot[[1]]
ES_MnegocioDECIL <- MnegocioDECIL[[2]]

ES_Motros_trabTot <- Motros_trabTot [[1]]
ES_Motros_trabDECIL <- Motros_trabDECIL [[2]]

ES_MrentasTot <- MrentasTot [[1]]
ES_MrentasDECIL <- MrentasDECIL [[2]]

ES_MutilidadTot <- MutilidadTot [[1]]
ES_MutilidadDECIL <- MutilidadDECIL [[2]]

ES_MarrendaTot <- MarrendaTot [[1]]
ES_MarrendaDECIL <- MarrendaDECIL [[2]]

ES_MtransferTot <- MtransferTot[[1]]
ES_MtransferDECIL <- MtransferDECIL[[2]]

ES_MjubilacionTot <- MjubilacionTot [[1]]
ES_MjubilacionDECIL <- MjubilacionDECIL [[2]]

ES_MbecasTot <- MbecasTot [[1]]
ES_MbecasDECIL <- MbecasDECIL [[2]]

ES_MdonativosTot <- MdonativosTot[[1]]
ES_MdonativosDECIL <- MdonativosDECIL[[2]]


ES_MremesasTot <- MremesasTot[[1]]
ES_MremesasDECIL <- MremesasDECIL[[2]]

ES_Mbene_gobTot <- Mbene_gobTot [[1]]
ES_Mbene_gobDECIL <- Mbene_gobDECIL [[2]]

ES_Mtransf_hogTot <- Mtransf_hogTot [[1]]
ES_Mtransf_hogDECIL <- Mtransf_hogDECIL [[2]]

ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instDECIL <- Mtrans_instDECIL[[2]]

ES_Mestim_alquTot <- Mestim_alquTot [[1]]
ES_Mestim_alquDECIL <- Mestim_alquDECIL [[2]]

ES_Motros_ingTot <- Motros_ingTot [[1]]
ES_Motros_ingDECIL <- Motros_ingDECIL [[2]]

########## Error Est?ndar 
SE_Ming_corTot <- SE (Ming_corTot)
SE_Ming_corDECIL <- SE (Ming_corDECIL)

SE_MingtrabTot <- SE (MingtrabTot)
SE_MingtrabDECIL <- SE (MingtrabDECIL)

SE_MtrabajoTot <- SE (MtrabajoTot)
SE_MtrabajoDECIL <- SE (MtrabajoDECIL)

SE_MnegocioTot <- SE (MnegocioTot)
SE_MnegocioDECIL <- SE (MnegocioDECIL)

SE_Motros_trabTot <- SE (Motros_trabTot)
SE_Motros_trabDECIL <- SE (Motros_trabDECIL)

SE_MrentasTot <- SE (MrentasTot)
SE_MrentasDECIL <- SE (MrentasDECIL)

SE_MutilidadTot <- SE (MutilidadTot)
SE_MutilidadDECIL <- SE (MutilidadDECIL)

SE_MarrendaTot <- SE (MarrendaTot)
SE_MarrendaDECIL <- SE (MarrendaDECIL)

SE_MtransferTot <- SE (MtransferTot)
SE_MtransferDECIL <- SE (MtransferDECIL)

SE_MjubilacionTot <- SE (MjubilacionTot)
SE_MjubilacionDECIL <- SE (MjubilacionDECIL)

SE_MbecasTot <- SE (MbecasTot)
SE_MbecasDECIL <- SE (MbecasDECIL)

SE_MdonativosTot <- SE (MdonativosTot)
SE_MdonativosDECIL <- SE (MdonativosDECIL)

SE_MremesasTot <- SE (MremesasTot)
SE_MremesasDECIL <- SE (MremesasDECIL)

SE_Mbene_gobTot <- SE (Mbene_gobTot)
SE_Mbene_gobDECIL <- SE (Mbene_gobDECIL)

SE_Mtransf_hogTot <- SE (Mtransf_hogTot)
SE_Mtransf_hogDECIL <- SE (Mtransf_hogDECIL)

SE_Mtrans_instTot <- SE (Mtrans_instTot)
SE_Mtrans_instDECIL <- SE (Mtrans_instDECIL)

SE_Mestim_alquTot <- SE (Mestim_alquTot)
SE_Mestim_alquDECIL <- SE (Mestim_alquDECIL)

SE_Motros_ingTot <- SE (Motros_ingTot)
SE_Motros_ingDECIL <- SE (Motros_ingDECIL)

########## Coeficiente de variaci?n 
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corDECIL <- cv(Ming_corDECIL)

CV_MingtrabTot <- cv(MingtrabTot)
CV_MingtrabDECIL <- cv(MingtrabDECIL)

CV_MtrabajoTot <- cv(MtrabajoTot)
CV_MtrabajoDECIL <- cv(MtrabajoDECIL)

CV_MnegocioTot <- cv(MnegocioTot)
CV_MnegocioDECIL <- cv(MnegocioDECIL)

CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabDECIL <- cv(Motros_trabDECIL)

CV_MrentasTot <- cv(MrentasTot)
CV_MrentasDECIL <- cv(MrentasDECIL)

CV_MutilidadTot <- cv(MutilidadTot)
CV_MutilidadDECIL <- cv(MutilidadDECIL)

CV_MarrendaTot <- cv(MarrendaTot)
CV_MarrendaDECIL <- cv(MarrendaDECIL)

CV_MtransferTot <- cv(MtransferTot)
CV_MtransferDECIL <- cv(MtransferDECIL)

CV_MjubilacionTot <- cv(MjubilacionTot)
CV_MjubilacionDECIL <- cv(MjubilacionDECIL)

CV_MbecasTot <- cv(MbecasTot)
CV_MbecasDECIL <- cv(MbecasDECIL)

CV_MdonativosTot <- cv(MdonativosTot)
CV_MdonativosDECIL <- cv(MdonativosDECIL)

CV_MremesasTot <- cv(MremesasTot)
CV_MremesasDECIL <- cv(MremesasDECIL)

CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobDECIL <- cv(Mbene_gobDECIL)

CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogDECIL <- cv(Mtransf_hogDECIL)

CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instDECIL <- cv(Mtrans_instDECIL)

CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquDECIL <- cv(Mestim_alquDECIL)

CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingDECIL <- cv(Motros_ingDECIL)
########## Limite inferior 
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,1]

LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,1]

LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,1]

LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,1]

LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,1]

LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,1]

LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,1]

LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,1]

LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,1]

LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1]
LI_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,1]

LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,1]

LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,1]

LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,1]

LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,1]

LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,1]

LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,1]

LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,1
]
LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingDECIL <- confint(Motros_ingDECIL ,level=0.90)[,1]

########## Limite superior 
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,2]

LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,2]

LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,2]

LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,2]

LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,2]

LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,2]

LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,2]

LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,2]

LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,2]

LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,2]

LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,2]

LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,2]

LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,2]

LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,2]

LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,2]

LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,2]

LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,2]

LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingDECIL <- confint(Motros_ingDECIL,level=0.90)[,2]



#    Cuadros

#este cuadro, lo ?nico que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_DECIL_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corDECIL),c(ES_MingtrabTot,ES_MingtrabDECIL),c(ES_MtrabajoTot,ES_MtrabajoDECIL),c(ES_MnegocioTot,ES_MnegocioDECIL)
             ,c(ES_Motros_trabTot,ES_Motros_trabDECIL),c(ES_MrentasTot,ES_MrentasDECIL),c(ES_MutilidadTot,ES_MutilidadDECIL)
             ,c(ES_MarrendaTot,ES_MarrendaDECIL),c(ES_MtransferTot,ES_MtransferDECIL),c(ES_MjubilacionTot,ES_MjubilacionDECIL),c(ES_MbecasTot,ES_MbecasDECIL),
             c(ES_MdonativosTot,ES_MdonativosDECIL),c(ES_MremesasTot,ES_MremesasDECIL),c(ES_Mbene_gobTot,ES_Mbene_gobDECIL),c(ES_Mtransf_hogTot,ES_Mtransf_hogDECIL)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instDECIL),c(ES_Mestim_alquTot,ES_Mestim_alquDECIL),c(ES_Motros_ingTot,ES_Motros_ingDECIL))
##### ERROR ESTANDAR
c_DECIL_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corDECIL),c(SE_MingtrabTot,SE_MingtrabDECIL),c(SE_MtrabajoTot,SE_MtrabajoDECIL),c(SE_MnegocioTot,SE_MnegocioDECIL)
             ,c(SE_Motros_trabTot,SE_Motros_trabDECIL),c(SE_MrentasTot,SE_MrentasDECIL),c(SE_MutilidadTot,SE_MutilidadDECIL)
             ,c(SE_MarrendaTot,SE_MarrendaDECIL),c(SE_MtransferTot,SE_MtransferDECIL),c(SE_MjubilacionTot,SE_MjubilacionDECIL),c(SE_MbecasTot,SE_MbecasDECIL),
             c(SE_MdonativosTot,SE_MdonativosDECIL),c(SE_MremesasTot,SE_MremesasDECIL),c(SE_Mbene_gobTot,SE_Mbene_gobDECIL),c(SE_Mtransf_hogTot,SE_Mtransf_hogDECIL),c(SE_Mtrans_instTot,SE_Mtrans_instDECIL)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquDECIL),c(SE_Motros_ingTot,SE_Motros_ingDECIL))

##### COEFICIENTE DE VARIACION
c_DECIL_CV <-
  data.frame(c(CV_Ming_corTot,CV_Ming_corDECIL),c(CV_MingtrabTot,CV_MingtrabDECIL),c(CV_MtrabajoTot,CV_MtrabajoDECIL),c(CV_MnegocioTot,CV_MnegocioDECIL)
             ,c(CV_Motros_trabTot,CV_Motros_trabDECIL),c(CV_MrentasTot,CV_MrentasDECIL),c(CV_MutilidadTot,CV_MutilidadDECIL),
             c(CV_MarrendaTot,CV_MarrendaDECIL),c(CV_MtransferTot,CV_MtransferDECIL),c(CV_MjubilacionTot,CV_MjubilacionDECIL),c(CV_MbecasTot,CV_MbecasDECIL)
             ,c(CV_MdonativosTot,CV_MdonativosDECIL),c(CV_MremesasTot,CV_MremesasDECIL),c(CV_Mbene_gobTot,CV_Mbene_gobDECIL),c(CV_Mtransf_hogTot,CV_Mtransf_hogDECIL),c(CV_Mtrans_instTot,CV_Mtrans_instDECIL)
             ,c(CV_Mestim_alquTot,CV_Mestim_alquDECIL),c(CV_Motros_ingTot,CV_Motros_ingDECIL))

##### LIMITE INFERIOR AL 90%
c_DECIL_LI <-
  data.frame(c(LI_Ming_corTot,LI_Ming_corDECIL),c(LI_MingtrabTot,LI_MingtrabDECIL),c(LI_MtrabajoTot,LI_MtrabajoDECIL),
             c(LI_MnegocioTot,LI_MnegocioDECIL),c(LI_Motros_trabTot,LI_Motros_trabDECIL),c(LI_MrentasTot,LI_MrentasDECIL),c(LI_MutilidadTot,LI_MutilidadDECIL),c(LI_MarrendaTot,LI_MarrendaDECIL)
             ,c(LI_MtransferTot,LI_MtransferDECIL),c(LI_MjubilacionTot,LI_MjubilacionDECIL),c(LI_MbecasTot,LI_MbecasDECIL),c(LI_MdonativosTot,LI_MdonativosDECIL)
             ,c(LI_MremesasTot,LI_MremesasDECIL),c(LI_Mbene_gobTot,LI_Mbene_gobDECIL),c(LI_Mtransf_hogTot,LI_Mtransf_hogDECIL),c(LI_Mtrans_instTot,LI_Mtrans_instDECIL)
             ,c(LI_Mestim_alquTot,LI_Mestim_alquDECIL),c(LI_Motros_ingTot,LI_Motros_ingDECIL))

### LIMITE SUPERIOR AL 90%
c_DECIL_LS <-
  data.frame(c(LS_Ming_corTot,LS_Ming_corDECIL),c(LS_MingtrabTot,LS_MingtrabDECIL),c(LS_MtrabajoTot,LS_MtrabajoDECIL),c(LS_MnegocioTot,LS_MnegocioDECIL)
             ,c(LS_Motros_trabTot,LS_Motros_trabDECIL),c(LS_MrentasTot,LS_MrentasDECIL),c(LS_MutilidadTot,LS_MutilidadDECIL),
             c(LS_MarrendaTot,LS_MarrendaDECIL),c(LS_MtransferTot,LS_MtransferDECIL),c(LS_MjubilacionTot,LS_MjubilacionDECIL),c(LS_MbecasTot,LS_MbecasDECIL),
             c(LS_MdonativosTot,LS_MdonativosDECIL),c(LS_MremesasTot,LS_MremesasDECIL),c(LS_Mbene_gobTot,LS_Mbene_gobDECIL),c(LS_Mtransf_hogTot,LS_Mtransf_hogDECIL),c(LS_Mtrans_instTot,LS_Mtrans_instDECIL)
             ,c(LS_Mestim_alquTot,LS_Mestim_alquDECIL),c(LS_Motros_ingTot,LS_Motros_ingDECIL))

# se agregan los nombres de las entidades a las filas
#esta cadena est? bien loca, no?
DECILES<-c("PROMEDIO", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
row.names(c_DECIL_ES)<-row.names(c_DECIL_SE)<-row.names(c_DECIL_CV)<-row.names(c_DECIL_LI)<-row.names(c_DECIL_LS)<-DECILES

#ahora vamos a ponerle nombre a las columnas
names(c_DECIL_ES)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_SE)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_CV)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_LI)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_LS)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla 
round(c_DECIL_ES)
round(c_DECIL_SE)
round(c_DECIL_CV,4)*100
round(c_DECIL_LI)
round(c_DECIL_LS)

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")

write.dbf(c_DECIL_ES,file = "Yucatan Ingresos por fuente por DECIL estimaciones 2018.dbf")
write.dbf(c_DECIL_SE,file = "Yucatan Ingresos por fuente por DECIL errores standard 2018.dbf")
write.dbf(c_DECIL_CV,file = "Yucatan Ingresos por fuente por DECIL CV 2018.dbf")
write.dbf(c_DECIL_LI,file = "Yucatan Ingresos por fuente por DECIL LI 2018.dbf")
write.dbf(c_DECIL_ES,file = "Yucatan Ingresos por fuente por DECIL LS 2018.dbf")

rm(list=ls())





########## Ingresos por fuente por decil INDIGENA 2018 ##############

library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
Conc2018<-read.dbf("ConcYucatan2018.dbf",as.is = T)

Conc2018<-Conc2018 %>%
  filter(HogarIndig==1)


mydesign <- svydesign(id=~upm,strata=~est_dis,data=Conc2018,weights=~factor)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~ing_cor,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por decil
#aqu? cmabia la funci?n a svyby, en by va el decil que creamos.
#y al final va la funci?n que queremos
Ming_corDECIL <- svyby(~ing_cor,denominator=~Nhog,by=~DECIL,mydesign,svyratio)


#      Trabajo

#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
MingtrabTot <- svyratio(~ingtrab,denominator=~Nhog,mydesign) # Total promedio
MingtrabDECIL <- svyby(~ingtrab,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~trabajo,denominator=~Nhog,mydesign) # Total promedio
MtrabajoDECIL <- svyby(~trabajo,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo independiente
MnegocioTot <- svyratio(~negocio,denominator=~Nhog,mydesign) # Total promedio
MnegocioDECIL <- svyby(~negocio,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso de otros trabajos
Motros_trabTot <- svyratio(~otros_trab,denominator=~Nhog,mydesign) # Total promedio
Motros_trabDECIL<- svyby(~otros_trab,denominator=~Nhog,by=~DECIL,mydesign,svyratio) # por decil


# Rentas de la propiedad

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
MrentasTot <- svyratio(~rentas,denominator=~Nhog,mydesign) # Total promedio
MrentasDECIL <- svyby(~rentas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #Por decil
###### ingresos de sociedades
MutilidadTot <- svyratio(~utilidad,denominator=~Nhog,mydesign) # Total promedio
MutilidadDECIL <- svyby(~utilidad,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### arrendamiento
MarrendaTot <- svyratio(~arrenda,denominator=~Nhog,mydesign) # Total promedio
MarrendaDECIL <- svyby(~arrenda,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Por decil


#       Transferencias 

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

MtransferTot <- svyratio(~transfer,denominator=~Nhog,mydesign) # Total promedio
MtransferDECIL <- svyby(~transfer,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

MjubilacionTot <- svyratio(~jubilacion,denominator=~Nhog,mydesign) # Total promedio
MjubilacionDECIL <- svyby(~jubilacion,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### becas que pueden ser, de nuevo, p?blicas privadas. 
MbecasTot <- svyratio(~becas,denominator=~Nhog,mydesign) # Total promedio
MbecasDECIL <- svyby(~becas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### donativos que tambi?n pueden ser p?blicos o privados.
MdonativosTot <- svyratio(~donativos,denominator=~Nhog,mydesign) # Total promedio
MdonativosDECIL <- svyby(~donativos,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
MremesasTot <- svyratio(~remesas,denominator=~Nhog,mydesign) # Total promedio
MremesasDECIL <- svyby(~remesas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~bene_gob,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobDECIL <- svyby(~bene_gob,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~transf_hog,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogDECIL <- svyby(~transf_hog,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #decil

###### trans_inst: puede venir de institucione sp?blicas o privadas.
Mtrans_instTot <- svyratio(~trans_inst,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instDECIL <- svyby(~trans_inst,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~estim_alqu,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquDECIL <- svyby(~estim_alqu,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

### otros_ing ### es literalmente ?algo m?s?
Motros_ingTot <- svyratio(~otros_ing,denominator=~Nhog,mydesign) # Total promedio
Motros_ingDECIL <- svyby(~otros_ing,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Decil



# Estimaciones 

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corDECIL <- Ming_corDECIL[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

ES_MingtrabTot <- MingtrabTot[[1]]
ES_MingtrabDECIL <- MingtrabDECIL[[2]]

ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_MtrabajoDECIL <- MtrabajoDECIL[[2]]

ES_MnegocioTot <- MnegocioTot[[1]]
ES_MnegocioDECIL <- MnegocioDECIL[[2]]

ES_Motros_trabTot <- Motros_trabTot [[1]]
ES_Motros_trabDECIL <- Motros_trabDECIL [[2]]

ES_MrentasTot <- MrentasTot [[1]]
ES_MrentasDECIL <- MrentasDECIL [[2]]

ES_MutilidadTot <- MutilidadTot [[1]]
ES_MutilidadDECIL <- MutilidadDECIL [[2]]

ES_MarrendaTot <- MarrendaTot [[1]]
ES_MarrendaDECIL <- MarrendaDECIL [[2]]

ES_MtransferTot <- MtransferTot[[1]]
ES_MtransferDECIL <- MtransferDECIL[[2]]

ES_MjubilacionTot <- MjubilacionTot [[1]]
ES_MjubilacionDECIL <- MjubilacionDECIL [[2]]

ES_MbecasTot <- MbecasTot [[1]]
ES_MbecasDECIL <- MbecasDECIL [[2]]

ES_MdonativosTot <- MdonativosTot[[1]]
ES_MdonativosDECIL <- MdonativosDECIL[[2]]


ES_MremesasTot <- MremesasTot[[1]]
ES_MremesasDECIL <- MremesasDECIL[[2]]

ES_Mbene_gobTot <- Mbene_gobTot [[1]]
ES_Mbene_gobDECIL <- Mbene_gobDECIL [[2]]

ES_Mtransf_hogTot <- Mtransf_hogTot [[1]]
ES_Mtransf_hogDECIL <- Mtransf_hogDECIL [[2]]

ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instDECIL <- Mtrans_instDECIL[[2]]

ES_Mestim_alquTot <- Mestim_alquTot [[1]]
ES_Mestim_alquDECIL <- Mestim_alquDECIL [[2]]

ES_Motros_ingTot <- Motros_ingTot [[1]]
ES_Motros_ingDECIL <- Motros_ingDECIL [[2]]

########## Error Est?ndar 
SE_Ming_corTot <- SE (Ming_corTot)
SE_Ming_corDECIL <- SE (Ming_corDECIL)

SE_MingtrabTot <- SE (MingtrabTot)
SE_MingtrabDECIL <- SE (MingtrabDECIL)

SE_MtrabajoTot <- SE (MtrabajoTot)
SE_MtrabajoDECIL <- SE (MtrabajoDECIL)

SE_MnegocioTot <- SE (MnegocioTot)
SE_MnegocioDECIL <- SE (MnegocioDECIL)

SE_Motros_trabTot <- SE (Motros_trabTot)
SE_Motros_trabDECIL <- SE (Motros_trabDECIL)

SE_MrentasTot <- SE (MrentasTot)
SE_MrentasDECIL <- SE (MrentasDECIL)

SE_MutilidadTot <- SE (MutilidadTot)
SE_MutilidadDECIL <- SE (MutilidadDECIL)

SE_MarrendaTot <- SE (MarrendaTot)
SE_MarrendaDECIL <- SE (MarrendaDECIL)

SE_MtransferTot <- SE (MtransferTot)
SE_MtransferDECIL <- SE (MtransferDECIL)

SE_MjubilacionTot <- SE (MjubilacionTot)
SE_MjubilacionDECIL <- SE (MjubilacionDECIL)

SE_MbecasTot <- SE (MbecasTot)
SE_MbecasDECIL <- SE (MbecasDECIL)

SE_MdonativosTot <- SE (MdonativosTot)
SE_MdonativosDECIL <- SE (MdonativosDECIL)

SE_MremesasTot <- SE (MremesasTot)
SE_MremesasDECIL <- SE (MremesasDECIL)

SE_Mbene_gobTot <- SE (Mbene_gobTot)
SE_Mbene_gobDECIL <- SE (Mbene_gobDECIL)

SE_Mtransf_hogTot <- SE (Mtransf_hogTot)
SE_Mtransf_hogDECIL <- SE (Mtransf_hogDECIL)

SE_Mtrans_instTot <- SE (Mtrans_instTot)
SE_Mtrans_instDECIL <- SE (Mtrans_instDECIL)

SE_Mestim_alquTot <- SE (Mestim_alquTot)
SE_Mestim_alquDECIL <- SE (Mestim_alquDECIL)

SE_Motros_ingTot <- SE (Motros_ingTot)
SE_Motros_ingDECIL <- SE (Motros_ingDECIL)

########## Coeficiente de variaci?n 
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corDECIL <- cv(Ming_corDECIL)

CV_MingtrabTot <- cv(MingtrabTot)
CV_MingtrabDECIL <- cv(MingtrabDECIL)

CV_MtrabajoTot <- cv(MtrabajoTot)
CV_MtrabajoDECIL <- cv(MtrabajoDECIL)

CV_MnegocioTot <- cv(MnegocioTot)
CV_MnegocioDECIL <- cv(MnegocioDECIL)

CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabDECIL <- cv(Motros_trabDECIL)

CV_MrentasTot <- cv(MrentasTot)
CV_MrentasDECIL <- cv(MrentasDECIL)

CV_MutilidadTot <- cv(MutilidadTot)
CV_MutilidadDECIL <- cv(MutilidadDECIL)

CV_MarrendaTot <- cv(MarrendaTot)
CV_MarrendaDECIL <- cv(MarrendaDECIL)

CV_MtransferTot <- cv(MtransferTot)
CV_MtransferDECIL <- cv(MtransferDECIL)

CV_MjubilacionTot <- cv(MjubilacionTot)
CV_MjubilacionDECIL <- cv(MjubilacionDECIL)

CV_MbecasTot <- cv(MbecasTot)
CV_MbecasDECIL <- cv(MbecasDECIL)

CV_MdonativosTot <- cv(MdonativosTot)
CV_MdonativosDECIL <- cv(MdonativosDECIL)

CV_MremesasTot <- cv(MremesasTot)
CV_MremesasDECIL <- cv(MremesasDECIL)

CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobDECIL <- cv(Mbene_gobDECIL)

CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogDECIL <- cv(Mtransf_hogDECIL)

CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instDECIL <- cv(Mtrans_instDECIL)

CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquDECIL <- cv(Mestim_alquDECIL)

CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingDECIL <- cv(Motros_ingDECIL)
########## Limite inferior 
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,1]

LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,1]

LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,1]

LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,1]

LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,1]

LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,1]

LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,1]

LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,1]

LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,1]

LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1]
LI_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,1]

LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,1]

LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,1]

LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,1]

LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,1]

LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,1]

LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,1]

LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,1
]
LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingDECIL <- confint(Motros_ingDECIL ,level=0.90)[,1]

########## Limite superior 
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,2]

LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,2]

LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,2]

LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,2]

LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,2]

LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,2]

LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,2]

LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,2]

LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,2]

LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,2]

LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,2]

LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,2]

LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,2]

LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,2]

LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,2]

LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,2]

LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,2]

LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingDECIL <- confint(Motros_ingDECIL,level=0.90)[,2]



#    Cuadros

#este cuadro, lo ?nico que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_DECIL_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corDECIL),c(ES_MingtrabTot,ES_MingtrabDECIL),c(ES_MtrabajoTot,ES_MtrabajoDECIL),c(ES_MnegocioTot,ES_MnegocioDECIL)
             ,c(ES_Motros_trabTot,ES_Motros_trabDECIL),c(ES_MrentasTot,ES_MrentasDECIL),c(ES_MutilidadTot,ES_MutilidadDECIL)
             ,c(ES_MarrendaTot,ES_MarrendaDECIL),c(ES_MtransferTot,ES_MtransferDECIL),c(ES_MjubilacionTot,ES_MjubilacionDECIL),c(ES_MbecasTot,ES_MbecasDECIL),
             c(ES_MdonativosTot,ES_MdonativosDECIL),c(ES_MremesasTot,ES_MremesasDECIL),c(ES_Mbene_gobTot,ES_Mbene_gobDECIL),c(ES_Mtransf_hogTot,ES_Mtransf_hogDECIL)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instDECIL),c(ES_Mestim_alquTot,ES_Mestim_alquDECIL),c(ES_Motros_ingTot,ES_Motros_ingDECIL))
##### ERROR ESTANDAR
c_DECIL_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corDECIL),c(SE_MingtrabTot,SE_MingtrabDECIL),c(SE_MtrabajoTot,SE_MtrabajoDECIL),c(SE_MnegocioTot,SE_MnegocioDECIL)
             ,c(SE_Motros_trabTot,SE_Motros_trabDECIL),c(SE_MrentasTot,SE_MrentasDECIL),c(SE_MutilidadTot,SE_MutilidadDECIL)
             ,c(SE_MarrendaTot,SE_MarrendaDECIL),c(SE_MtransferTot,SE_MtransferDECIL),c(SE_MjubilacionTot,SE_MjubilacionDECIL),c(SE_MbecasTot,SE_MbecasDECIL),
             c(SE_MdonativosTot,SE_MdonativosDECIL),c(SE_MremesasTot,SE_MremesasDECIL),c(SE_Mbene_gobTot,SE_Mbene_gobDECIL),c(SE_Mtransf_hogTot,SE_Mtransf_hogDECIL),c(SE_Mtrans_instTot,SE_Mtrans_instDECIL)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquDECIL),c(SE_Motros_ingTot,SE_Motros_ingDECIL))

##### COEFICIENTE DE VARIACION
c_DECIL_CV <-
  data.frame(c(CV_Ming_corTot,CV_Ming_corDECIL),c(CV_MingtrabTot,CV_MingtrabDECIL),c(CV_MtrabajoTot,CV_MtrabajoDECIL),c(CV_MnegocioTot,CV_MnegocioDECIL)
             ,c(CV_Motros_trabTot,CV_Motros_trabDECIL),c(CV_MrentasTot,CV_MrentasDECIL),c(CV_MutilidadTot,CV_MutilidadDECIL),
             c(CV_MarrendaTot,CV_MarrendaDECIL),c(CV_MtransferTot,CV_MtransferDECIL),c(CV_MjubilacionTot,CV_MjubilacionDECIL),c(CV_MbecasTot,CV_MbecasDECIL)
             ,c(CV_MdonativosTot,CV_MdonativosDECIL),c(CV_MremesasTot,CV_MremesasDECIL),c(CV_Mbene_gobTot,CV_Mbene_gobDECIL),c(CV_Mtransf_hogTot,CV_Mtransf_hogDECIL),c(CV_Mtrans_instTot,CV_Mtrans_instDECIL)
             ,c(CV_Mestim_alquTot,CV_Mestim_alquDECIL),c(CV_Motros_ingTot,CV_Motros_ingDECIL))

##### LIMITE INFERIOR AL 90%
c_DECIL_LI <-
  data.frame(c(LI_Ming_corTot,LI_Ming_corDECIL),c(LI_MingtrabTot,LI_MingtrabDECIL),c(LI_MtrabajoTot,LI_MtrabajoDECIL),
             c(LI_MnegocioTot,LI_MnegocioDECIL),c(LI_Motros_trabTot,LI_Motros_trabDECIL),c(LI_MrentasTot,LI_MrentasDECIL),c(LI_MutilidadTot,LI_MutilidadDECIL),c(LI_MarrendaTot,LI_MarrendaDECIL)
             ,c(LI_MtransferTot,LI_MtransferDECIL),c(LI_MjubilacionTot,LI_MjubilacionDECIL),c(LI_MbecasTot,LI_MbecasDECIL),c(LI_MdonativosTot,LI_MdonativosDECIL)
             ,c(LI_MremesasTot,LI_MremesasDECIL),c(LI_Mbene_gobTot,LI_Mbene_gobDECIL),c(LI_Mtransf_hogTot,LI_Mtransf_hogDECIL),c(LI_Mtrans_instTot,LI_Mtrans_instDECIL)
             ,c(LI_Mestim_alquTot,LI_Mestim_alquDECIL),c(LI_Motros_ingTot,LI_Motros_ingDECIL))

### LIMITE SUPERIOR AL 90%
c_DECIL_LS <-
  data.frame(c(LS_Ming_corTot,LS_Ming_corDECIL),c(LS_MingtrabTot,LS_MingtrabDECIL),c(LS_MtrabajoTot,LS_MtrabajoDECIL),c(LS_MnegocioTot,LS_MnegocioDECIL)
             ,c(LS_Motros_trabTot,LS_Motros_trabDECIL),c(LS_MrentasTot,LS_MrentasDECIL),c(LS_MutilidadTot,LS_MutilidadDECIL),
             c(LS_MarrendaTot,LS_MarrendaDECIL),c(LS_MtransferTot,LS_MtransferDECIL),c(LS_MjubilacionTot,LS_MjubilacionDECIL),c(LS_MbecasTot,LS_MbecasDECIL),
             c(LS_MdonativosTot,LS_MdonativosDECIL),c(LS_MremesasTot,LS_MremesasDECIL),c(LS_Mbene_gobTot,LS_Mbene_gobDECIL),c(LS_Mtransf_hogTot,LS_Mtransf_hogDECIL),c(LS_Mtrans_instTot,LS_Mtrans_instDECIL)
             ,c(LS_Mestim_alquTot,LS_Mestim_alquDECIL),c(LS_Motros_ingTot,LS_Motros_ingDECIL))

# se agregan los nombres de las entidades a las filas
#esta cadena est? bien loca, no?
DECILES<-c("PROMEDIO", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
row.names(c_DECIL_ES)<-row.names(c_DECIL_SE)<-row.names(c_DECIL_CV)<-row.names(c_DECIL_LI)<-row.names(c_DECIL_LS)<-DECILES

#ahora vamos a ponerle nombre a las columnas
names(c_DECIL_ES)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_SE)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_CV)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_LI)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_LS)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla 
round(c_DECIL_ES)
round(c_DECIL_SE)
round(c_DECIL_CV,4)*100
round(c_DECIL_LI)
round(c_DECIL_LS)

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")

write.dbf(c_DECIL_ES,file = "Yucatan INDIGENA Ingresos por fuente por DECIL estimaciones 2018.dbf")
write.dbf(c_DECIL_SE,file = "Yucatan INDIGENA Ingresos por fuente por DECIL errores standard 2018.dbf")
write.dbf(c_DECIL_CV,file = "Yucatan INDIGENA Ingresos por fuente por DECIL CV 2018.dbf")
write.dbf(c_DECIL_LI,file = "Yucatan INDIGENA Ingresos por fuente por DECIL LI 2018.dbf")
write.dbf(c_DECIL_ES,file = "Yucatan INDIGENA Ingresos por fuente por DECIL LS 2018.dbf")

rm(list=ls())


########## Ingresos por fuente por decil NO 2018 ##############

library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
Conc2018<-read.dbf("ConcYucatan2018.dbf",as.is = T)

Conc2018<-Conc2018 %>%
  filter(HogarIndig==0)


mydesign <- svydesign(id=~upm,strata=~est_dis,data=Conc2018,weights=~factor)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~ing_cor,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por decil
#aqu? cmabia la funci?n a svyby, en by va el decil que creamos.
#y al final va la funci?n que queremos
Ming_corDECIL <- svyby(~ing_cor,denominator=~Nhog,by=~DECIL,mydesign,svyratio)


#      Trabajo

#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
MingtrabTot <- svyratio(~ingtrab,denominator=~Nhog,mydesign) # Total promedio
MingtrabDECIL <- svyby(~ingtrab,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~trabajo,denominator=~Nhog,mydesign) # Total promedio
MtrabajoDECIL <- svyby(~trabajo,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso del trabajo independiente
MnegocioTot <- svyratio(~negocio,denominator=~Nhog,mydesign) # Total promedio
MnegocioDECIL <- svyby(~negocio,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### ingreso de otros trabajos
Motros_trabTot <- svyratio(~otros_trab,denominator=~Nhog,mydesign) # Total promedio
Motros_trabDECIL<- svyby(~otros_trab,denominator=~Nhog,by=~DECIL,mydesign,svyratio) # por decil


# Rentas de la propiedad

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
MrentasTot <- svyratio(~rentas,denominator=~Nhog,mydesign) # Total promedio
MrentasDECIL <- svyby(~rentas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #Por decil
###### ingresos de sociedades
MutilidadTot <- svyratio(~utilidad,denominator=~Nhog,mydesign) # Total promedio
MutilidadDECIL <- svyby(~utilidad,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # por decil
###### arrendamiento
MarrendaTot <- svyratio(~arrenda,denominator=~Nhog,mydesign) # Total promedio
MarrendaDECIL <- svyby(~arrenda,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Por decil


#       Transferencias 

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

MtransferTot <- svyratio(~transfer,denominator=~Nhog,mydesign) # Total promedio
MtransferDECIL <- svyby(~transfer,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

MjubilacionTot <- svyratio(~jubilacion,denominator=~Nhog,mydesign) # Total promedio
MjubilacionDECIL <- svyby(~jubilacion,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### becas que pueden ser, de nuevo, p?blicas privadas. 
MbecasTot <- svyratio(~becas,denominator=~Nhog,mydesign) # Total promedio
MbecasDECIL <- svyby(~becas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### donativos que tambi?n pueden ser p?blicos o privados.
MdonativosTot <- svyratio(~donativos,denominator=~Nhog,mydesign) # Total promedio
MdonativosDECIL <- svyby(~donativos,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
MremesasTot <- svyratio(~remesas,denominator=~Nhog,mydesign) # Total promedio
MremesasDECIL <- svyby(~remesas,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~bene_gob,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobDECIL <- svyby(~bene_gob,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~transf_hog,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogDECIL <- svyby(~transf_hog,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) #decil

###### trans_inst: puede venir de institucione sp?blicas o privadas.
Mtrans_instTot <- svyratio(~trans_inst,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instDECIL <- svyby(~trans_inst,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # DECIL

### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~estim_alqu,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquDECIL <- svyby(~estim_alqu,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # decil

### otros_ing ### es literalmente ?algo m?s?
Motros_ingTot <- svyratio(~otros_ing,denominator=~Nhog,mydesign) # Total promedio
Motros_ingDECIL <- svyby(~otros_ing,denominator=~Nhog,by=~DECIL ,mydesign,svyratio) # Decil



# Estimaciones 

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corDECIL <- Ming_corDECIL[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

ES_MingtrabTot <- MingtrabTot[[1]]
ES_MingtrabDECIL <- MingtrabDECIL[[2]]

ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_MtrabajoDECIL <- MtrabajoDECIL[[2]]

ES_MnegocioTot <- MnegocioTot[[1]]
ES_MnegocioDECIL <- MnegocioDECIL[[2]]

ES_Motros_trabTot <- Motros_trabTot [[1]]
ES_Motros_trabDECIL <- Motros_trabDECIL [[2]]

ES_MrentasTot <- MrentasTot [[1]]
ES_MrentasDECIL <- MrentasDECIL [[2]]

ES_MutilidadTot <- MutilidadTot [[1]]
ES_MutilidadDECIL <- MutilidadDECIL [[2]]

ES_MarrendaTot <- MarrendaTot [[1]]
ES_MarrendaDECIL <- MarrendaDECIL [[2]]

ES_MtransferTot <- MtransferTot[[1]]
ES_MtransferDECIL <- MtransferDECIL[[2]]

ES_MjubilacionTot <- MjubilacionTot [[1]]
ES_MjubilacionDECIL <- MjubilacionDECIL [[2]]

ES_MbecasTot <- MbecasTot [[1]]
ES_MbecasDECIL <- MbecasDECIL [[2]]

ES_MdonativosTot <- MdonativosTot[[1]]
ES_MdonativosDECIL <- MdonativosDECIL[[2]]


ES_MremesasTot <- MremesasTot[[1]]
ES_MremesasDECIL <- MremesasDECIL[[2]]

ES_Mbene_gobTot <- Mbene_gobTot [[1]]
ES_Mbene_gobDECIL <- Mbene_gobDECIL [[2]]

ES_Mtransf_hogTot <- Mtransf_hogTot [[1]]
ES_Mtransf_hogDECIL <- Mtransf_hogDECIL [[2]]

ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instDECIL <- Mtrans_instDECIL[[2]]

ES_Mestim_alquTot <- Mestim_alquTot [[1]]
ES_Mestim_alquDECIL <- Mestim_alquDECIL [[2]]

ES_Motros_ingTot <- Motros_ingTot [[1]]
ES_Motros_ingDECIL <- Motros_ingDECIL [[2]]

########## Error Est?ndar 
SE_Ming_corTot <- SE (Ming_corTot)
SE_Ming_corDECIL <- SE (Ming_corDECIL)

SE_MingtrabTot <- SE (MingtrabTot)
SE_MingtrabDECIL <- SE (MingtrabDECIL)

SE_MtrabajoTot <- SE (MtrabajoTot)
SE_MtrabajoDECIL <- SE (MtrabajoDECIL)

SE_MnegocioTot <- SE (MnegocioTot)
SE_MnegocioDECIL <- SE (MnegocioDECIL)

SE_Motros_trabTot <- SE (Motros_trabTot)
SE_Motros_trabDECIL <- SE (Motros_trabDECIL)

SE_MrentasTot <- SE (MrentasTot)
SE_MrentasDECIL <- SE (MrentasDECIL)

SE_MutilidadTot <- SE (MutilidadTot)
SE_MutilidadDECIL <- SE (MutilidadDECIL)

SE_MarrendaTot <- SE (MarrendaTot)
SE_MarrendaDECIL <- SE (MarrendaDECIL)

SE_MtransferTot <- SE (MtransferTot)
SE_MtransferDECIL <- SE (MtransferDECIL)

SE_MjubilacionTot <- SE (MjubilacionTot)
SE_MjubilacionDECIL <- SE (MjubilacionDECIL)

SE_MbecasTot <- SE (MbecasTot)
SE_MbecasDECIL <- SE (MbecasDECIL)

SE_MdonativosTot <- SE (MdonativosTot)
SE_MdonativosDECIL <- SE (MdonativosDECIL)

SE_MremesasTot <- SE (MremesasTot)
SE_MremesasDECIL <- SE (MremesasDECIL)

SE_Mbene_gobTot <- SE (Mbene_gobTot)
SE_Mbene_gobDECIL <- SE (Mbene_gobDECIL)

SE_Mtransf_hogTot <- SE (Mtransf_hogTot)
SE_Mtransf_hogDECIL <- SE (Mtransf_hogDECIL)

SE_Mtrans_instTot <- SE (Mtrans_instTot)
SE_Mtrans_instDECIL <- SE (Mtrans_instDECIL)

SE_Mestim_alquTot <- SE (Mestim_alquTot)
SE_Mestim_alquDECIL <- SE (Mestim_alquDECIL)

SE_Motros_ingTot <- SE (Motros_ingTot)
SE_Motros_ingDECIL <- SE (Motros_ingDECIL)

########## Coeficiente de variaci?n 
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corDECIL <- cv(Ming_corDECIL)

CV_MingtrabTot <- cv(MingtrabTot)
CV_MingtrabDECIL <- cv(MingtrabDECIL)

CV_MtrabajoTot <- cv(MtrabajoTot)
CV_MtrabajoDECIL <- cv(MtrabajoDECIL)

CV_MnegocioTot <- cv(MnegocioTot)
CV_MnegocioDECIL <- cv(MnegocioDECIL)

CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabDECIL <- cv(Motros_trabDECIL)

CV_MrentasTot <- cv(MrentasTot)
CV_MrentasDECIL <- cv(MrentasDECIL)

CV_MutilidadTot <- cv(MutilidadTot)
CV_MutilidadDECIL <- cv(MutilidadDECIL)

CV_MarrendaTot <- cv(MarrendaTot)
CV_MarrendaDECIL <- cv(MarrendaDECIL)

CV_MtransferTot <- cv(MtransferTot)
CV_MtransferDECIL <- cv(MtransferDECIL)

CV_MjubilacionTot <- cv(MjubilacionTot)
CV_MjubilacionDECIL <- cv(MjubilacionDECIL)

CV_MbecasTot <- cv(MbecasTot)
CV_MbecasDECIL <- cv(MbecasDECIL)

CV_MdonativosTot <- cv(MdonativosTot)
CV_MdonativosDECIL <- cv(MdonativosDECIL)

CV_MremesasTot <- cv(MremesasTot)
CV_MremesasDECIL <- cv(MremesasDECIL)

CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobDECIL <- cv(Mbene_gobDECIL)

CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogDECIL <- cv(Mtransf_hogDECIL)

CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instDECIL <- cv(Mtrans_instDECIL)

CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquDECIL <- cv(Mestim_alquDECIL)

CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingDECIL <- cv(Motros_ingDECIL)
########## Limite inferior 
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,1]

LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,1]

LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,1]

LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,1]

LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,1]

LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,1]

LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,1]

LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,1]

LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,1]

LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1]
LI_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,1]

LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,1]

LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,1]

LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,1]

LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,1]

LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,1]

LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,1]

LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,1
]
LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingDECIL <- confint(Motros_ingDECIL ,level=0.90)[,1]

########## Limite superior 
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corDECIL <- confint(Ming_corDECIL,level=0.90)[,2]

LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_MingtrabDECIL <- confint(MingtrabDECIL,level=0.90)[,2]

LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_MtrabajoDECIL <- confint(MtrabajoDECIL,level=0.90)[,2]

LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_MnegocioDECIL <- confint(MnegocioDECIL,level=0.90)[,2]

LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabDECIL <- confint(Motros_trabDECIL,level=0.90)[,2]

LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_MrentasDECIL <- confint(MrentasDECIL,level=0.90)[,2]

LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_MutilidadDECIL <- confint(MutilidadDECIL,level=0.90)[,2]

LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_MarrendaDECIL <- confint(MarrendaDECIL,level=0.90)[,2]

LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_MtransferDECIL <- confint(MtransferDECIL,level=0.90)[,2]

LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_MjubilacionDECIL <- confint(MjubilacionDECIL,level=0.90)[,2]

LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_MbecasDECIL <- confint(MbecasDECIL,level=0.90)[,2]

LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_MdonativosDECIL <- confint(MdonativosDECIL,level=0.90)[,2]

LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_MremesasDECIL <- confint(MremesasDECIL,level=0.90)[,2]

LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobDECIL <- confint(Mbene_gobDECIL,level=0.90)[,2]

LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogDECIL <- confint(Mtransf_hogDECIL,level=0.90)[,2]

LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instDECIL <- confint(Mtrans_instDECIL,level=0.90)[,2]

LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquDECIL <- confint(Mestim_alquDECIL,level=0.90)[,2]

LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingDECIL <- confint(Motros_ingDECIL,level=0.90)[,2]



#    Cuadros

#este cuadro, lo ?nico que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_DECIL_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corDECIL),c(ES_MingtrabTot,ES_MingtrabDECIL),c(ES_MtrabajoTot,ES_MtrabajoDECIL),c(ES_MnegocioTot,ES_MnegocioDECIL)
             ,c(ES_Motros_trabTot,ES_Motros_trabDECIL),c(ES_MrentasTot,ES_MrentasDECIL),c(ES_MutilidadTot,ES_MutilidadDECIL)
             ,c(ES_MarrendaTot,ES_MarrendaDECIL),c(ES_MtransferTot,ES_MtransferDECIL),c(ES_MjubilacionTot,ES_MjubilacionDECIL),c(ES_MbecasTot,ES_MbecasDECIL),
             c(ES_MdonativosTot,ES_MdonativosDECIL),c(ES_MremesasTot,ES_MremesasDECIL),c(ES_Mbene_gobTot,ES_Mbene_gobDECIL),c(ES_Mtransf_hogTot,ES_Mtransf_hogDECIL)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instDECIL),c(ES_Mestim_alquTot,ES_Mestim_alquDECIL),c(ES_Motros_ingTot,ES_Motros_ingDECIL))
##### ERROR ESTANDAR
c_DECIL_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corDECIL),c(SE_MingtrabTot,SE_MingtrabDECIL),c(SE_MtrabajoTot,SE_MtrabajoDECIL),c(SE_MnegocioTot,SE_MnegocioDECIL)
             ,c(SE_Motros_trabTot,SE_Motros_trabDECIL),c(SE_MrentasTot,SE_MrentasDECIL),c(SE_MutilidadTot,SE_MutilidadDECIL)
             ,c(SE_MarrendaTot,SE_MarrendaDECIL),c(SE_MtransferTot,SE_MtransferDECIL),c(SE_MjubilacionTot,SE_MjubilacionDECIL),c(SE_MbecasTot,SE_MbecasDECIL),
             c(SE_MdonativosTot,SE_MdonativosDECIL),c(SE_MremesasTot,SE_MremesasDECIL),c(SE_Mbene_gobTot,SE_Mbene_gobDECIL),c(SE_Mtransf_hogTot,SE_Mtransf_hogDECIL),c(SE_Mtrans_instTot,SE_Mtrans_instDECIL)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquDECIL),c(SE_Motros_ingTot,SE_Motros_ingDECIL))

##### COEFICIENTE DE VARIACION
c_DECIL_CV <-
  data.frame(c(CV_Ming_corTot,CV_Ming_corDECIL),c(CV_MingtrabTot,CV_MingtrabDECIL),c(CV_MtrabajoTot,CV_MtrabajoDECIL),c(CV_MnegocioTot,CV_MnegocioDECIL)
             ,c(CV_Motros_trabTot,CV_Motros_trabDECIL),c(CV_MrentasTot,CV_MrentasDECIL),c(CV_MutilidadTot,CV_MutilidadDECIL),
             c(CV_MarrendaTot,CV_MarrendaDECIL),c(CV_MtransferTot,CV_MtransferDECIL),c(CV_MjubilacionTot,CV_MjubilacionDECIL),c(CV_MbecasTot,CV_MbecasDECIL)
             ,c(CV_MdonativosTot,CV_MdonativosDECIL),c(CV_MremesasTot,CV_MremesasDECIL),c(CV_Mbene_gobTot,CV_Mbene_gobDECIL),c(CV_Mtransf_hogTot,CV_Mtransf_hogDECIL),c(CV_Mtrans_instTot,CV_Mtrans_instDECIL)
             ,c(CV_Mestim_alquTot,CV_Mestim_alquDECIL),c(CV_Motros_ingTot,CV_Motros_ingDECIL))

##### LIMITE INFERIOR AL 90%
c_DECIL_LI <-
  data.frame(c(LI_Ming_corTot,LI_Ming_corDECIL),c(LI_MingtrabTot,LI_MingtrabDECIL),c(LI_MtrabajoTot,LI_MtrabajoDECIL),
             c(LI_MnegocioTot,LI_MnegocioDECIL),c(LI_Motros_trabTot,LI_Motros_trabDECIL),c(LI_MrentasTot,LI_MrentasDECIL),c(LI_MutilidadTot,LI_MutilidadDECIL),c(LI_MarrendaTot,LI_MarrendaDECIL)
             ,c(LI_MtransferTot,LI_MtransferDECIL),c(LI_MjubilacionTot,LI_MjubilacionDECIL),c(LI_MbecasTot,LI_MbecasDECIL),c(LI_MdonativosTot,LI_MdonativosDECIL)
             ,c(LI_MremesasTot,LI_MremesasDECIL),c(LI_Mbene_gobTot,LI_Mbene_gobDECIL),c(LI_Mtransf_hogTot,LI_Mtransf_hogDECIL),c(LI_Mtrans_instTot,LI_Mtrans_instDECIL)
             ,c(LI_Mestim_alquTot,LI_Mestim_alquDECIL),c(LI_Motros_ingTot,LI_Motros_ingDECIL))

### LIMITE SUPERIOR AL 90%
c_DECIL_LS <-
  data.frame(c(LS_Ming_corTot,LS_Ming_corDECIL),c(LS_MingtrabTot,LS_MingtrabDECIL),c(LS_MtrabajoTot,LS_MtrabajoDECIL),c(LS_MnegocioTot,LS_MnegocioDECIL)
             ,c(LS_Motros_trabTot,LS_Motros_trabDECIL),c(LS_MrentasTot,LS_MrentasDECIL),c(LS_MutilidadTot,LS_MutilidadDECIL),
             c(LS_MarrendaTot,LS_MarrendaDECIL),c(LS_MtransferTot,LS_MtransferDECIL),c(LS_MjubilacionTot,LS_MjubilacionDECIL),c(LS_MbecasTot,LS_MbecasDECIL),
             c(LS_MdonativosTot,LS_MdonativosDECIL),c(LS_MremesasTot,LS_MremesasDECIL),c(LS_Mbene_gobTot,LS_Mbene_gobDECIL),c(LS_Mtransf_hogTot,LS_Mtransf_hogDECIL),c(LS_Mtrans_instTot,LS_Mtrans_instDECIL)
             ,c(LS_Mestim_alquTot,LS_Mestim_alquDECIL),c(LS_Motros_ingTot,LS_Motros_ingDECIL))

# se agregan los nombres de las entidades a las filas
#esta cadena est? bien loca, no?
DECILES<-c("PROMEDIO", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
row.names(c_DECIL_ES)<-row.names(c_DECIL_SE)<-row.names(c_DECIL_CV)<-row.names(c_DECIL_LI)<-row.names(c_DECIL_LS)<-DECILES

#ahora vamos a ponerle nombre a las columnas
names(c_DECIL_ES)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_SE)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_CV)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_LI)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

names(c_DECIL_LS)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla 
round(c_DECIL_ES)
round(c_DECIL_SE)
round(c_DECIL_CV,4)*100
round(c_DECIL_LI)
round(c_DECIL_LS)

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")

write.dbf(c_DECIL_ES,file = "Yucatan NO Ingresos por fuente por DECIL estimaciones 2018.dbf")
write.dbf(c_DECIL_SE,file = "Yucatan NO Ingresos por fuente por DECIL errores standard 2018.dbf")
write.dbf(c_DECIL_CV,file = "Yucatan NO Ingresos por fuente por DECIL CV 2018.dbf")
write.dbf(c_DECIL_LI,file = "Yucatan NO Ingresos por fuente por DECIL LI 2018.dbf")
write.dbf(c_DECIL_ES,file = "Yucatan NO Ingresos por fuente por DECIL LS 2018.dbf")

rm(list=ls())

###### Shared prosperity total #######
library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(reshape2)

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")

Deciles2010<-read.dbf("Yucatan por fuente por DECIL estimaciones 2010.dbf")
names(Deciles2010)[1]<-c("ingcor2010")

Deciles2018<-read.dbf("Yucatan Ingresos por fuente por DECIL estimaciones 2018.dbf")
names(Deciles2018)[1]<-c("ingcor2018")

GICTotal<-data.frame(Deciles2010,Deciles2018)


GICTotal<-GICTotal%>%
  mutate(Rate=((ingcor2018-ingcor2010)/ingcor2010)*100,Deciles=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"),
         orden=1:11)

growth_total<-round(GICTotal$Rate[1],2)

bottom_40<-round(mean(GICTotal$Rate[2:5]),2)

rm(list = ls())

###### shared prosperity non-idigenous #######
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")

deciles2010No<-read.dbf("Yucatan NO por fuente por DECIL estimaciones 2010.dbf")
names(deciles2010No)[1]<-c("No2010")

deciles2018No<-read.dbf("Yucatan NO Ingresos por fuente por DECIL estimaciones 2018.dbf")
names(deciles2018No)[1]<-c("No2018")

GICTotal<-data.frame(deciles2010No,deciles2018No)


GICTotal<-GICTotal%>%
  mutate(Rate=((No2018-No2010)/No2010)*100,Deciles=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"),
         orden=1:11)

growth_total<-round(GICTotal$Rate[1],2)

bottom_40<-round(mean(GICTotal$Rate[2:5]),2)

rm(list = ls())

###### shared prosperity idigenous #######
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")

deciles2010indigena<-read.dbf("Yucatan INDIGENA por fuente por DECIL estimaciones 2010.dbf")
names(deciles2010indigena)[1]<-c("Indigena2010")

deciles2018indigena<-read.dbf("Yucatan INDIGENA Ingresos por fuente por DECIL estimaciones 2018.dbf")
names(deciles2018indigena)[1]<-c("Indigena2018")

GICTotal<-data.frame(deciles2010indigena,deciles2018indigena)


GICTotal<-GICTotal%>%
  mutate(Rate=((Indigena2018-Indigena2010)/Indigena2010)*100,Deciles=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"),
         orden=1:11)

growth_total<-round(GICTotal$Rate[1],2)

bottom_40<-round(mean(GICTotal$Rate[2:5]),2)

rm(list = ls())

########## GIC total ##############################
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(foreign)

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan"))

Deciles2010<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan/Yucatan por fuente por DECIL estimaciones 2010.dbf")
names(Deciles2010)[1]<-c("ingcor2010")

Deciles2018<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan/Yucatan Ingresos por fuente por DECIL estimaciones 2018.dbf")
names(Deciles2018)[1]<-c("ingcor2018")


GICTotal<-data.frame(Deciles2010,Deciles2018)

GICTotal<-GICTotal%>%
  mutate(Rate=((ingcor2018-ingcor2010)/ingcor2010)*100,Deciles=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"),
         orden=1:11)

max<-round(max(GICTotal$Rate),0)
min<-round(min(GICTotal$Rate),0)

GIC_Yucatan<-GICTotal%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(Deciles,Rate))+
  geom_col()+
  labs(title = "Growth Incidence Curve Yucatan 2010-2018",
       y="Growth rate (total)",
       x="Decile")+
  scale_y_continuous(breaks=seq(min,max,1))+
  theme_minimal()

GIC_Yucatan

GIC_Yucatan<-ggplotly(GIC_Yucatan)

GIC_Yucatan

saveWidget(GIC_Yucatan,fil="GIC_Yucatan_total.html")

rm(list=ls())

########## GIC por fuente ########
library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(reshape2)

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan"))

Deciles_por_fuente_2010<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan/Yucatan por fuente por DECIL estimaciones 2010.dbf")

names(Deciles_por_fuente_2010)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

Deciles_por_fuente_2010<-Deciles_por_fuente_2010%>%
  mutate(prueba=Deciles_por_fuente_2010$TRABAJO2010+Deciles_por_fuente_2010$RENTAS2010+
           Deciles_por_fuente_2010$JUBILACION2010+Deciles_por_fuente_2010$BECAS2010+
           Deciles_por_fuente_2010$DONATIVOS2010+Deciles_por_fuente_2010$REMESAS2010+
           Deciles_por_fuente_2010$BENEGOBIERNO2010+Deciles_por_fuente_2010$`TRANS HOG2010`+
           Deciles_por_fuente_2010$`TRANS INST2010`+Deciles_por_fuente_2010$`ESTIM ALQU2010`+
           Deciles_por_fuente_2010$`OTROS INGRESOS2010`)

all.equal(Deciles_por_fuente_2010$`ING COR2010`,Deciles_por_fuente_2010$prueba)

Deciles_por_fuente_2018<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan/Yucatan Ingresos por fuente por DECIL estimaciones 2018.dbf")

names(Deciles_por_fuente_2018)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

Deciles_por_fuente_2018<-Deciles_por_fuente_2018%>%
  mutate(prueba=Deciles_por_fuente_2018$TRABAJO2018+Deciles_por_fuente_2018$RENTAS2018+
           Deciles_por_fuente_2018$JUBILACION2018+Deciles_por_fuente_2018$BECAS2018+
           Deciles_por_fuente_2018$DONATIVOS2018+Deciles_por_fuente_2018$REMESAS2018+
           Deciles_por_fuente_2018$BENEGOBIERNO2018+Deciles_por_fuente_2018$`TRANS HOG2018`+
           Deciles_por_fuente_2018$`TRANS INST2018`+Deciles_por_fuente_2018$`ESTIM ALQU2018`+
           Deciles_por_fuente_2018$`OTROS INGRESOS2018`)

all.equal(Deciles_por_fuente_2018$`ING COR2018`,Deciles_por_fuente_2018$prueba)


Tasa_total<-((Deciles_por_fuente_2018$`ING COR2018`- Deciles_por_fuente_2010$`ING COR2010`)/Deciles_por_fuente_2010$`ING COR2010`)*100


########################## Trabajo 

trabajo<-data.frame(trabajo2010=Deciles_por_fuente_2010$TRABAJO2010,
                    trabajo2018=Deciles_por_fuente_2018$TRABAJO2018,
                    ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                    ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                    Tasa_total)

trabajo<-trabajo%>%
  mutate(trabajo_aporte=((trabajo2018-trabajo2010)/((ing_cor2018-ing_cor2010)))*Tasa_total)

################################### Rentas 
rentas<-data.frame(rentas2010=Deciles_por_fuente_2010$RENTAS2010,rentas2018=Deciles_por_fuente_2018$RENTAS2018,
                   ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                   ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                   Tasa_total)
rentas<-rentas%>%
  mutate(rentas_aporte=((rentas2018-rentas2010)/((ing_cor2018-ing_cor2010)))*Tasa_total)

################################### Jubilaciones 

jubilaciones<-data.frame(jubilaciones2010=Deciles_por_fuente_2010$JUBILACION2010,jubilaciones2018=Deciles_por_fuente_2018$JUBILACION2018,
                         ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                         ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                         Tasa_total)
jubilaciones<-jubilaciones%>%
  mutate(jubilaciones_aporte=((jubilaciones2018-jubilaciones2010)/((ing_cor2018-ing_cor2010)))*Tasa_total)

################################### Becas 
becas<-data.frame(becas2010=Deciles_por_fuente_2010$BECAS2010,becas2018=Deciles_por_fuente_2018$BECAS2018,
                  ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                  ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                  Tasa_total)
becas<-becas%>%
  mutate(becas_aporte=((becas2018-becas2010)/((ing_cor2018-ing_cor2010)))*Tasa_total)

################################### Donativos 

donativos<-data.frame(donativos2010=Deciles_por_fuente_2010$DONATIVOS2010,donativos2018=Deciles_por_fuente_2018$DONATIVOS2018,
                      ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                      ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                      Tasa_total)

donativos<-donativos%>%
  mutate(donativos_aporte=((donativos2018-donativos2010)/((ing_cor2018-ing_cor2010)))*Tasa_total)

################################### Remesas 

remesas<-data.frame(remesas2010=Deciles_por_fuente_2010$REMESAS2010,remesas2018=Deciles_por_fuente_2018$REMESAS2018,
                    ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                    ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                    Tasa_total)

remesas<-remesas%>%
  mutate(remesas_aporte=((remesas2018-remesas2010)/((ing_cor2018-ing_cor2010)))*Tasa_total)

################################### Benegobierno 

benegobierno<-data.frame(benegob2010=Deciles_por_fuente_2010$BENEGOBIERNO2010,
                         benegob2018=Deciles_por_fuente_2018$BENEGOBIERNO2018,
                         ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                         ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                         Tasa_total)

benegobierno<-benegobierno%>%
  mutate(benegob_aporte=((benegob2018-benegob2010)/((ing_cor2018-ing_cor2010)))*Tasa_total)

################################### Transdehogares 

transdehogares<-data.frame(transdehogares2010=Deciles_por_fuente_2010$`TRANS HOG2010`, 
                           transdehogares2018=Deciles_por_fuente_2018$`TRANS HOG2018`,
                           ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                           ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                           Tasa_total)

transdehogares<-transdehogares%>%
  mutate(transdehogares_aporte=((transdehogares2018-transdehogares2010)/((ing_cor2018-ing_cor2010)))*Tasa_total)

################################### instituciones 

instituciones<-data.frame(instituciones2010=Deciles_por_fuente_2010$`TRANS INST2010`, 
                          instituciones2018=Deciles_por_fuente_2018$`TRANS INST2018`,
                          ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                          ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                          Tasa_total)

instituciones<-instituciones%>%
  mutate(instituciones_aporte=((instituciones2018-instituciones2010)/((ing_cor2018-ing_cor2010)))*Tasa_total)

################################### alquiler 

alquiler<-data.frame(alquiler2010=Deciles_por_fuente_2010$`ESTIM ALQU2010`,alquiler2018=Deciles_por_fuente_2018$`ESTIM ALQU2018`,
                     ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                     ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                     Tasa_total)
alquiler<-alquiler%>%
  mutate(alquiler_aporte=((alquiler2018-alquiler2010)/((ing_cor2018-ing_cor2010)))*Tasa_total)

################################### otros 

otros<-data.frame(otros2010=Deciles_por_fuente_2010$`OTROS INGRESOS2010`,
                  otros2018=Deciles_por_fuente_2018$`OTROS INGRESOS2018`,
                  ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                  ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                  Tasa_total)
otros<-otros%>%
  mutate(otros_aporte=((otros2018-otros2010)/((ing_cor2018-ing_cor2010)))*Tasa_total)

################################### Cuadro final 

cuadro_final<-data.frame(
  Labor=trabajo$trabajo_aporte,
  Capital=rentas$rentas_aporte,
  Pensions=jubilaciones$jubilaciones_aporte,
  Scholarships=becas$becas_aporte,
  Donations=donativos$donativos_aporte,
  Remittances=remesas$remesas_aporte,
  "Government transfers"=benegobierno$benegob_aporte,
  "Household transfers"=transdehogares$transdehogares_aporte,
  "Instituion transfers"=instituciones$instituciones_aporte,
  "Rent estimate"=alquiler$alquiler_aporte,
  "Others"=otros$otros_aporte)

Prueba<-cuadro_final%>%
  mutate(Prueba=Labor+Capital+Pensions+Scholarships+Donations+Remittances+Government.transfers+Household.transfers+
           Instituion.transfers+Rent.estimate+Others)

all.equal(Prueba$Prueba,Tasa_total)


names(cuadro_final)<-c("Labor","Capital","Pensions","Scholarships","Donations","Remittances","Government transfers",
                       "Household transfers","Instituion transfers","Imputed rent","Others")


cuadro_final<-cuadro_final%>%
  mutate(Deciles= c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

row.names(cuadro_final)<-c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X")

cuadro_final<-cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

cuadro_final<-melt(cuadro_final)

labels<-cuadro_final%>%
  mutate(labels=ifelse(value<0,value,0))%>%
  group_by(Deciles)%>%
  summarize(sum(labels)-1.5)

labels<-labels$`sum(labels) - 1.5`

max<-cuadro_final%>%
  mutate(value=ifelse(value>0,value,0))%>%
  group_by(Deciles)%>%
  summarize(sum(value))

max<-round(max(max$`sum(value)`)+2)

min<-cuadro_final%>%
  mutate(value=ifelse(value<0,value,0))%>%
  group_by(Deciles)%>%
  summarize(sum(value))

min<-round(min(min$`sum(value)`)-2)


GIC<-cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(x=Deciles, y=value , fill= variable),position= "dodge")+
  geom_col()+
  labs(title = "Growth Incidence Curve Yucatan 2010-2018",
       y="Growth rate (total)",
       x="Decile",
       fill="Source of
  income")+
  geom_hline(yintercept = 0)+
  annotate("text", x= "Mean", y= labels[1], label=round(Tasa_total[1],2))+
  annotate("text", x= "I", y= labels[2], label=round(Tasa_total[2],2))+
  annotate("text", x= "II", y= labels[3], label=round(Tasa_total[3],2))+
  annotate("text", x= "III", y= labels[4], label=round(Tasa_total[4],2))+
  annotate("text", x= "IV", y= labels[5], label=round(Tasa_total[5],2))+
  annotate("text", x= "V", y= labels[6], label=round(Tasa_total[6],2))+
  annotate("text", x= "VI", y= labels[7], label=round(Tasa_total[7],2))+
  annotate("text", x= "VII", y= labels[8], label=round(Tasa_total[8],2))+
  annotate("text", x= "VIII", y= labels[9], label=round(Tasa_total[9],2))+
  annotate("text", x= "IX", y= labels[10], label=round(Tasa_total[10],2))+
  annotate("text", x= "X", y= labels[11], label=round(Tasa_total[11],2))+
  scale_y_continuous(breaks=seq(min,max,1))+
  theme_minimal()

GIC<-ggplotly(GIC)

GIC

saveWidget(GIC,fil="GIC_by_source.html")

rm(list=ls())




########## GIC por raza ########
library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(reshape2)
library(ggrepel)

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan"))

deciles2010indigena<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan/Yucatan INDIGENA por fuente por DECIL estimaciones 2010.dbf")
names(deciles2010indigena)[1]<-c("Indigena2010")

deciles2018indigena<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan/Yucatan INDIGENA Ingresos por fuente por DECIL estimaciones 2018.dbf")
names(deciles2018indigena)[1]<-c("Indigena2018")

deciles2010No<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan/Yucatan NO por fuente por DECIL estimaciones 2010.dbf")
names(deciles2010No)[1]<-c("No2010")

deciles2018No<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan/Yucatan NO Ingresos por fuente por DECIL estimaciones 2018.dbf")
names(deciles2018No)[1]<-c("No2018")

GIC_por_raza<-data.frame(deciles2010indigena,deciles2018indigena,deciles2010No,deciles2018No)


GIC_por_raza<-GIC_por_raza%>%
  mutate("Indigenous"=((Indigena2018-Indigena2010)/Indigena2010)*100,
         "Non-indigenous"=((No2018-No2010)/No2010)*100,
         "Deciles"=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

GIC_reducida<-GIC_por_raza%>%
  select(Indigenous,`Non-indigenous`,Deciles)

GIC_por_raza_derretida<-GIC_reducida%>%
  melt(id.vars="Deciles",variable.name="Rates")

GIC_por_raza_derretida$Rates<-as.factor(as.character(GIC_por_raza_derretida$Rates))


max<-round(max(GIC_por_raza_derretida$value)+2)

min<-round(min(GIC_por_raza_derretida$value)+2)


GIC_por_raza<-GIC_por_raza_derretida%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(x=Deciles,y=value,fill=Rates))+
  geom_col(position=position_dodge())+
  labs(title = "Growth Incidence Curve Yucatan by ethnic group 2010-2018",
       fill="Ethnic group:",
       y="Growth rate (total)",
       x="Decile")+
  scale_y_continuous(breaks=seq(min,max,ifelse(abs(max-min)>20,4,1)))+
  theme_minimal()

GIC_por_raza<-ggplotly(GIC_por_raza)

GIC_por_raza

saveWidget(GIC_por_raza,fil="GIC_Yucatan_by_ethnic_group.html")

rm(list=ls())

########## GIC por fuente por raza ###########
library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(reshape2)


setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan"))

Deciles_por_fuente_2010_indigena<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan/Yucatan INDIGENA por fuente por DECIL estimaciones 2010.dbf")

names(Deciles_por_fuente_2010_indigena)=c("ING.COR","TRABAJO","SUBORDINADO", "NEGOCIOS","OTROS.TRAB",
                                          "RENTAS","UTILIDAD","ARRENDA","TRANSFER","JUBILACION","BECAS",
                                          "DONATIVOS","REMESAS","BENEGOBIER","TRANS.HOG","TRANS.INST",
                                          "ESTIM.ALQU","OTROS.INGR")

Deciles_por_fuente_2010_indigena<-Deciles_por_fuente_2010_indigena%>%
  mutate(prueba=TRABAJO+RENTAS+JUBILACION+BECAS+DONATIVOS+REMESAS+BENEGOBIER+TRANS.HOG+TRANS.INST+ESTIM.ALQU+OTROS.INGR)

all.equal(Deciles_por_fuente_2010_indigena$ING.COR,Deciles_por_fuente_2010_indigena$prueba)

Deciles_por_fuente_2018_indigena<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan/Yucatan INDIGENA Ingresos por fuente por DECIL estimaciones 2018.dbf")

names(Deciles_por_fuente_2018_indigena)=c("ING.COR","TRABAJO","SUBORDINADO", "NEGOCIOS","OTROS.TRAB","RENTAS","UTILIDAD","ARRENDA",
                                          "TRANSFER","JUBILACION","BECAS","DONATIVOS","REMESAS","BENEGOBIER","TRANS.HOG","TRANS.INST","ESTIM.ALQU","OTROS.INGR")

Deciles_por_fuente_2018_indigena<-Deciles_por_fuente_2018_indigena%>%
  mutate(prueba=TRABAJO+RENTAS+JUBILACION+BECAS+DONATIVOS+REMESAS+BENEGOBIER+TRANS.HOG+TRANS.INST+ESTIM.ALQU+OTROS.INGR)

all.equal(Deciles_por_fuente_2018_indigena$ING.COR,Deciles_por_fuente_2018_indigena$prueba)

################################## Indigenous 
Tasa_total_indigena<-((Deciles_por_fuente_2018_indigena$ING.COR-Deciles_por_fuente_2010_indigena$ING.COR)/
                        Deciles_por_fuente_2010_indigena$ING.COR)*100

#### trabajo

Trabajo_indigena<-data.frame(trabajo2010=Deciles_por_fuente_2010_indigena$TRABAJO,
                             trabajo2018=Deciles_por_fuente_2018_indigena$TRABAJO,
                             ing_cor2010=Deciles_por_fuente_2010_indigena$ING.COR,
                             ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                             Tasa_total_indigena)

Trabajo_indigena<-Trabajo_indigena%>%
  mutate(trabajo_aporte=((trabajo2018-trabajo2010)/((ing_cor2018-ing_cor2010)))*Tasa_total_indigena)

#### renta

Renta_indigena<-data.frame(renta2010=Deciles_por_fuente_2010_indigena$RENTAS,
                           renta2018=Deciles_por_fuente_2018_indigena$RENTAS,
                           ing_cor2010=Deciles_por_fuente_2010_indigena$ING.COR,
                           ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                           Tasa_total_indigena)

Renta_indigena<-Renta_indigena%>%
  mutate(rentas_aporte=((renta2018-renta2010)/((ing_cor2018-ing_cor2010)))*Tasa_total_indigena)

#### Jubilaciones

Jubilaciones_indigena<-data.frame(jubilaciones2010=Deciles_por_fuente_2010_indigena$JUBILACION,
                                  jubilaciones2018=Deciles_por_fuente_2018_indigena$JUBILACION,
                                  ing_cor2010=Deciles_por_fuente_2010_indigena$ING.COR,
                                  ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                                  Tasa_total_indigena)

Jubilaciones_indigena<-Jubilaciones_indigena%>%
  mutate(jubilaciones_aporte=((jubilaciones2018-jubilaciones2010)/((ing_cor2018-ing_cor2010)))*Tasa_total_indigena)

## Becas

Becas_indigena<-data.frame(becas2010=Deciles_por_fuente_2010_indigena$BECAS,
                           becas2018=Deciles_por_fuente_2018_indigena$BECAS,
                           ing_cor2010=Deciles_por_fuente_2010_indigena$ING.COR,
                           ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                           Tasa_total_indigena)

Becas_indigena<-Becas_indigena%>%
  mutate(becas_aporte=((becas2018-becas2010)/((ing_cor2018-ing_cor2010)))*Tasa_total_indigena)

## Donativos

Donativos_indigena<-data.frame(donativos2010=Deciles_por_fuente_2010_indigena$DONATIVOS,
                               donativos2018=Deciles_por_fuente_2018_indigena$DONATIVOS,
                               ing_cor2010=Deciles_por_fuente_2010_indigena$ING.COR,
                               ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                               Tasa_total_indigena)

Donativos_indigena<-Donativos_indigena%>%
  mutate(donativos_aporte=((donativos2018-donativos2010)/((ing_cor2018-ing_cor2010)))*Tasa_total_indigena)

### Remesas

Remesas_indigena<-data.frame(remesas2010=Deciles_por_fuente_2010_indigena$REMESAS,
                             remesas2018=Deciles_por_fuente_2018_indigena$REMESAS,
                             ing_cor2010=Deciles_por_fuente_2010_indigena$ING.COR,
                             ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                             Tasa_total_indigena)

Remesas_indigena<-Remesas_indigena%>%
  mutate(remesas_aporte=((remesas2018-remesas2010)/((ing_cor2018-ing_cor2010)))*Tasa_total_indigena)

### Bene Gob

Benegob_indigena<-data.frame(benegob2010=Deciles_por_fuente_2010_indigena$BENEGOBIER,
                             benegob2018=Deciles_por_fuente_2018_indigena$BENEGOBIER,
                             ing_cor2010=Deciles_por_fuente_2010_indigena$ING.COR,
                             ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                             Tasa_total_indigena)

Benegob_indigena<-Benegob_indigena%>%
  mutate(benegob_aporte=((benegob2018-benegob2010)/((ing_cor2018-ing_cor2010)))*Tasa_total_indigena)

### Trans de hogares

Trans_hogares_indigena<-data.frame(transdehogares2010=Deciles_por_fuente_2010_indigena$TRANS.HOG,
                                   transdehogares2018=Deciles_por_fuente_2018_indigena$TRANS.HOG,
                                   ing_cor2010=Deciles_por_fuente_2010_indigena$ING.COR,
                                   ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                                   Tasa_total_indigena)

Trans_hogares_indigena<-Trans_hogares_indigena%>%
  mutate(transdehogares_aporte=((transdehogares2018-transdehogares2010)/((ing_cor2018-ing_cor2010)))*Tasa_total_indigena)

### Trans de Instituciones

Instituciones_indigena<-data.frame(instituciones2010=Deciles_por_fuente_2010_indigena$TRANS.INST,
                                   instituciones2018=Deciles_por_fuente_2018_indigena$TRANS.INST,
                                   ing_cor2010=Deciles_por_fuente_2010_indigena$ING.COR,
                                   ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                                   Tasa_total_indigena)

Instituciones_indigena<-Instituciones_indigena%>%
  mutate(instituciones_aporte=((instituciones2018-instituciones2010)/((ing_cor2018-ing_cor2010)))*Tasa_total_indigena)

### Alquiler

Alquiler_indigena<-data.frame(alquiler2010=Deciles_por_fuente_2010_indigena$ESTIM.ALQU,
                              alquiler2018=Deciles_por_fuente_2018_indigena$ESTIM.ALQU,
                              ing_cor2010=Deciles_por_fuente_2010_indigena$ING.COR,
                              ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                              Tasa_total_indigena)

Alquiler_indigena<-Alquiler_indigena%>%
  mutate(alquiler_aporte=((alquiler2018-alquiler2010)/((ing_cor2018-ing_cor2010)))*Tasa_total_indigena)

### otros

Otros_indigena<-data.frame(otros2010=Deciles_por_fuente_2010_indigena$OTROS.INGR,
                           otros2018=Deciles_por_fuente_2018_indigena$OTROS.INGR,
                           ing_cor2010=Deciles_por_fuente_2010_indigena$ING.COR,
                           ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                           Tasa_total_indigena)

Otros_indigena<-Otros_indigena%>%
  mutate(otros_aporte=((otros2018-otros2010)/((ing_cor2018-ing_cor2010)))*Tasa_total_indigena)

Cuadro_indigena<-data.frame(Trabajo_indigena$trabajo_aporte,
                            Renta_indigena$rentas_aporte,
                            Jubilaciones_indigena$jubilaciones_aporte,
                            Becas_indigena$becas_aporte,
                            Donativos_indigena$donativos_aporte,
                            Remesas_indigena$remesas_aporte,
                            Benegob_indigena$benegob_aporte,
                            Trans_hogares_indigena$transdehogares_aporte,
                            Instituciones_indigena$instituciones_aporte,
                            Alquiler_indigena$alquiler_aporte,
                            Otros_indigena$otros_aporte)

Prueba<-(Trabajo_indigena$trabajo_aporte+
           Renta_indigena$rentas_aporte+
           Jubilaciones_indigena$jubilaciones_aporte+
           Becas_indigena$becas_aporte+
           Donativos_indigena$donativos_aporte+
           Remesas_indigena$remesas_aporte+
           Benegob_indigena$benegob_aporte+
           Trans_hogares_indigena$transdehogares_aporte+
           Instituciones_indigena$instituciones_aporte+
           Alquiler_indigena$alquiler_aporte+
           Otros_indigena$otros_aporte)

all.equal(Prueba,Tasa_total_indigena)


names(Cuadro_indigena)<-c("Labor","Capital","Pensions","Scholarships","Donations","Remittances","Government transfers",
                          "Household transfers","Instituion transfers","Rent estimate","Others")                             

#No indigena                         

Deciles_por_fuente_2010_NO<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan/Yucatan NO por fuente por DECIL estimaciones 2010.dbf")
names(Deciles_por_fuente_2010_NO)=c("ING.COR","TRABAJO","SUBORDINADO", "NEGOCIOS","OTROS.TRAB","RENTAS","UTILIDAD","ARRENDA",
                                    "TRANSFER","JUBILACION","BECAS","DONATIVOS","REMESAS","BENEGOBIER","TRANS.HOG","TRANS.INST","ESTIM.ALQU","OTROS.INGR")

Deciles_por_fuente_2010_NO<-Deciles_por_fuente_2010_NO%>%
  mutate(prueba=TRABAJO+RENTAS+JUBILACION+BECAS+DONATIVOS+REMESAS+BENEGOBIER+TRANS.HOG+TRANS.INST+ESTIM.ALQU+OTROS.INGR)

all.equal(Deciles_por_fuente_2010_NO$ING.COR,Deciles_por_fuente_2010_NO$prueba)

###


Deciles_por_fuente_2018_NO<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/Yucatan/Yucatan NO Ingresos por fuente por DECIL estimaciones 2018.dbf")

names(Deciles_por_fuente_2018_NO)=c("ING.COR","TRABAJO","SUBORDINADO", "NEGOCIOS","OTROS.TRAB","RENTAS","UTILIDAD","ARRENDA",
                                    "TRANSFER","JUBILACION","BECAS","DONATIVOS","REMESAS","BENEGOBIER","TRANS.HOG","TRANS.INST","ESTIM.ALQU","OTROS.INGR")

Deciles_por_fuente_2018_NO<-Deciles_por_fuente_2018_NO%>%
  mutate(prueba=TRABAJO+RENTAS+JUBILACION+BECAS+DONATIVOS+REMESAS+BENEGOBIER+TRANS.HOG+TRANS.INST+ESTIM.ALQU+OTROS.INGR)

all.equal(Deciles_por_fuente_2018_NO$ING.COR,Deciles_por_fuente_2018_NO$prueba)

Tasa_total_NO<-((Deciles_por_fuente_2018_NO$ING.COR-Deciles_por_fuente_2010_NO$ING.COR)/Deciles_por_fuente_2010_NO$ING.COR)*100


# Trabajo

Trabajo_NO<-data.frame(trabajo2010=Deciles_por_fuente_2010_NO$TRABAJO,
                       trabajo2018=Deciles_por_fuente_2018_NO$TRABAJO,
                       ingcor2010=Deciles_por_fuente_2010_NO$ING.COR,
                       ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                       Tasa_total_NO)

Trabajo_NO<-Trabajo_NO%>%
  mutate(Trabajo_aporte=((trabajo2018-trabajo2010)/((ingcor2018-ingcor2010)))*Tasa_total_NO)


# Rentas

Rentas_NO<-data.frame(rentas2010=Deciles_por_fuente_2010_NO$RENTAS,
                      rentas2018=Deciles_por_fuente_2018_NO$RENTAS,
                      ingcor2010=Deciles_por_fuente_2010_NO$ING.COR,
                      ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                      Tasa_total_NO)

Rentas_NO<-Rentas_NO%>%
  mutate(rentas_aporte=((rentas2018-rentas2010)/(ingcor2018-ingcor2010))*Tasa_total_NO)

#Jubilacion

Jubilacion_NO<-data.frame(jubilacion2010=Deciles_por_fuente_2010_NO$JUBILACION,
                          jubilacion2018=Deciles_por_fuente_2018_NO$JUBILACION,
                          ingcor2010=Deciles_por_fuente_2010_NO$ING.COR,
                          ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                          Tasa_total_NO)

Jubilacion_NO<-Jubilacion_NO%>%
  mutate(jubilacion_aporte=((jubilacion2018-jubilacion2010)/(ingcor2018-ingcor2010))*Tasa_total_NO)

#Becas

Becas_NO<-data.frame(becas2010=Deciles_por_fuente_2010_NO$BECAS,
                     becas2018=Deciles_por_fuente_2018_NO$BECAS,
                     ingcor2010=Deciles_por_fuente_2010_NO$ING.COR,
                     ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                     Tasa_total_NO)

Becas_NO<-Becas_NO%>%
  mutate(becas_aporte=((becas2018-becas2010)/(ingcor2018-ingcor2010))*Tasa_total_NO)

#Remesas

Remesas_NO<-data.frame(remesas2010=Deciles_por_fuente_2010_NO$REMESAS,
                       remesas2018=Deciles_por_fuente_2018_NO$REMESAS,
                       ingcor2010=Deciles_por_fuente_2010_NO$ING.COR,
                       ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                       Tasa_total_NO)

Remesas_NO<-Remesas_NO%>%
  mutate(becas_aporte=((remesas2018-remesas2010)/(ingcor2018-ingcor2010))*Tasa_total_NO)

#Donativos

Donativos_NO<-data.frame(donativos2010=Deciles_por_fuente_2010_NO$DONATIVOS,
                         donativos2018=Deciles_por_fuente_2018_NO$DONATIVOS,
                         ingcor2010=Deciles_por_fuente_2010_NO$ING.COR,
                         ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                         Tasa_total_NO)

Donativos_NO<-Donativos_NO%>%
  mutate(donativos_aporte=((donativos2018-donativos2010)/(ingcor2018-ingcor2010))*Tasa_total_NO)

#BENEGOBIER

Benegob_NO<-data.frame(bene2010=Deciles_por_fuente_2010_NO$BENEGOBIER,
                       bene2018=Deciles_por_fuente_2018_NO$BENEGOBIER,
                       ingcor2010=Deciles_por_fuente_2010_NO$ING.COR,
                       ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                       Tasa_total_NO)

Benegob_NO<-Benegob_NO%>%
  mutate(bene_aporte=((bene2018-bene2010)/(ingcor2018-ingcor2010))*Tasa_total_NO)

#TRANS.HOG

trans_hog_NO<-data.frame(transhog2010=Deciles_por_fuente_2010_NO$TRANS.HOG,
                         transhog2018=Deciles_por_fuente_2018_NO$TRANS.HOG,
                         ingcor2010=Deciles_por_fuente_2010_NO$ING.COR,
                         ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                         Tasa_total_NO)

trans_hog_NO<-trans_hog_NO%>%
  mutate(trans_hog_aporte=((transhog2018-transhog2010)/(ingcor2018-ingcor2010))*Tasa_total_NO)


#TRANS.INST

trans_inst_NO<-data.frame(trans_inst2010=Deciles_por_fuente_2010_NO$TRANS.INST,
                          trans_inst2018=Deciles_por_fuente_2018_NO$TRANS.INST,
                          ingcor2010=Deciles_por_fuente_2010_NO$ING.COR,
                          ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                          Tasa_total_NO)

trans_inst_NO<-trans_inst_NO%>%
  mutate(trans_inst_aporte=((trans_inst2018-trans_inst2010)/(ingcor2018-ingcor2010))*Tasa_total_NO)


#ESTIM.ALQU

Estim_alqu_NO<-data.frame(estim2010=Deciles_por_fuente_2010_NO$ESTIM.ALQU,
                          estim2018=Deciles_por_fuente_2018_NO$ESTIM.ALQU,
                          ingcor2010=Deciles_por_fuente_2010_NO$ING.COR,
                          ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                          Tasa_total_NO)

Estim_alqu_NO<-Estim_alqu_NO%>%
  mutate(estim_aporte=((estim2018-estim2010)/(ingcor2018-ingcor2010))*Tasa_total_NO)


#OTROS.INGR   

Otros_NO<-data.frame(otros2010=Deciles_por_fuente_2010_NO$OTROS.INGR,
                     otros2018=Deciles_por_fuente_2018_NO$OTROS.INGR,
                     ingcor2010=Deciles_por_fuente_2010_NO$ING.COR,
                     ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                     Tasa_total_NO)

Otros_NO<-Otros_NO%>%
  mutate(otors_aporte=((otros2018-otros2010)/(ingcor2018-ingcor2010))*Tasa_total_NO)

Cuadro_NO<-data.frame(Trabajo_NO$Trabajo_aporte,
                      Rentas_NO$rentas_aporte,
                      Jubilacion_NO$jubilacion_aporte,
                      Becas_NO$becas_aporte,
                      Remesas_NO$becas_aporte,
                      Donativos_NO$donativos_aporte,
                      Benegob_NO$bene_aporte,
                      trans_hog_NO$trans_hog_aporte,
                      trans_inst_NO$trans_inst_aporte,
                      Estim_alqu_NO$estim_aporte,
                      Otros_NO$otors_aporte)  

prueba_NO<-(Trabajo_NO$Trabajo_aporte+
              Rentas_NO$rentas_aporte+
              Jubilacion_NO$jubilacion_aporte+
              Becas_NO$becas_aporte+
              Remesas_NO$becas_aporte+
              Donativos_NO$donativos_aporte+
              Benegob_NO$bene_aporte+
              trans_hog_NO$trans_hog_aporte+
              trans_inst_NO$trans_inst_aporte+
              Estim_alqu_NO$estim_aporte+
              Otros_NO$otors_aporte)

all.equal(prueba_NO,Tasa_total_NO)


names(Cuadro_NO)<-c("Labor","Capital","Pensions","Scholarships","Donations","Remittances","Government transfers",
                    "Household transfers","Instituion transfers","Rent estimate","Others")      

######### Construcción del cuadro final para el gráfico

Cuadro_indigena<-Cuadro_indigena %>%
  mutate("Ethnic group"="Indigenous", Deciles=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

indigena_melt<-melt(Cuadro_indigena)

indigena_melt<-indigena_melt%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

Cuadro_NO<-Cuadro_NO %>%
  mutate("Ethnic group"="Non-Indigenous", Deciles=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))


NO_melt<-melt(Cuadro_NO)

NO_melt<-NO_melt%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

Cuadro_final<-rbind(indigena_melt,NO_melt)

Cuadro_final<-Cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

####### Etiquetas para el gráfico

labels_indigena<-Cuadro_final%>%
  filter(`Ethnic group`=="Indigenous")%>%
  mutate(labels=ifelse(value>0,value,0))%>%
  group_by(Deciles)%>%
  summarize(sum(labels)+1.5)

labels_indigena<-labels_indigena$`sum(labels) + 1.5`


labels_NO<-Cuadro_final%>%
  filter(`Ethnic group`=="Non-Indigenous")%>%
  mutate(labels=ifelse(value>0,value,0))%>%
  group_by(Deciles)%>%
  summarize(sum(labels)+1.5)

labels_NO<-labels_NO$`sum(labels) + 1.5`

melt_para_max_y_min<-rbind(Cuadro_indigena,Cuadro_NO)

melt_para_max_y_min<-melt(melt_para_max_y_min)

max<-melt_para_max_y_min%>%
  group_by(Deciles)%>%
  filter(value>0)%>%
  summarize(sum(value))

max<-round(max(max$`sum(value)`))

min<-melt_para_max_y_min%>%
  group_by(Deciles)%>%
  filter(value<0)%>%
  summarize(sum(value))

min<-round(min(min$`sum(value)`))


###### Gráfico


GIC<-ggplot()+
  geom_col(data = indigena_melt, mapping=aes(x=as.numeric(Deciles)-0.15, y=value, fill= as.factor(variable), group=`Ethnic group`),
           width = 0.25)+
  geom_col(data = NO_melt, mapping=aes(x=as.numeric(Deciles)+0.15, y=value, fill= as.factor(variable), group=`Ethnic group`),
           width = 0.25)+
  labs(title = "Yucatan, Growth Incidence Curve by ethnic group, 2010-2018",
       y="Growth rate (total)",
       x="Decile",
       fill="Source of income")+
  scale_y_continuous(breaks = seq(min,max,ifelse(abs(max-min)>0,5,1)))+
  scale_x_continuous(breaks = seq(1,11,1),labels = c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X") )+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_blank())+
  annotate("text", label = round(Tasa_total_indigena[1],digits = 2),
           x = 0.85, y = labels_indigena[1], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_NO[1],digits = 2),
           x = 1.15, y = labels_NO[1], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_indigena[2],digits = 2),
           x = 1.85, y = labels_indigena[2], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_NO[2],digits = 2),
           x = 2.15, y = labels_NO[2], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_indigena[3],digits = 2),
           x = 2.85, y = labels_indigena[3], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_NO[3],digits = 2),
           x = 3.15, y = labels_NO[3], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_indigena[4],digits = 2),
           x = 3.85, y = labels_indigena[4], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_NO[4],digits = 2),
           x = 4.15, y = labels_NO[4], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_indigena[5],digits = 2),
           x = 4.85, y = labels_indigena[5], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_NO[5],digits = 2),
           x = 5.15, y = labels_NO[5], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_indigena[6],digits = 2),
           x = 5.85, y = labels_indigena[6], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_NO[6],digits = 2),
           x = 6.15, y = labels_NO[6], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_indigena[7],digits = 2),
           x = 6.85, y = labels_indigena[7], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_NO[7],digits = 2),
           x = 7.15, y = labels_NO[7], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_indigena[8],digits = 2),
           x = 7.85, y = labels_indigena[8], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_NO[8],digits = 2),
           x = 8.15, y = labels_NO[8], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_indigena[9],digits = 2),
           x = 8.85, y = labels_indigena[9], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_NO[9],digits = 2),
           x = 9.15, y = labels_NO[9], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_indigena[10],digits = 2),
           x = 9.85, y = labels_indigena[10], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_NO[10],digits = 2),
           x = 10.15, y = labels_NO[10], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_indigena[11],digits = 2),
           x = 10.85, y = labels_indigena[11], size = 3, colour = "black",angle=45)+
  annotate("text", label = round(Tasa_total_NO[11],digits = 2),
           x = 11.15, y = labels_NO[11], size = 3, colour = "black",angle=45)+
  theme_minimal()

GIC<-ggplotly(GIC)

GIC

saveWidget(GIC,fil="GIC_Yucatan_by_ethnic_group_and_source.html")

rm(list=ls())

########## Hogares debajo de la linea de pobreza rural y urbano ######

library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(reshape2)
library(survey)
options(survey.lonely.psu="adjust")

###2010 rurales

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan/")
Conc2010<-read.dbf("ConcYucatan2010.dbf",as.is = T)

Conc2010<-Conc2010%>%
  mutate(Canasta_rural_extrema=712.77*3,
         Canasta_rural_moderada=1378.05*3,
         Canasta_urbana_extrema=1012.12*3,
         Canasta_urbana_moderada=2185.79*3)

Conc2010<-Conc2010%>%
  mutate(linea_de_pobreza_rural_extrema=ifelse(Small==1,Canasta_rural_extrema*tot_integ,0),
         linea_de_pobreza_rural_moderada=ifelse(Small==1,Canasta_rural_moderada*tot_integ,0),
         linea_de_pobreza_urbana_extrema=ifelse(Small==0,Canasta_urbana_extrema*tot_integ,0),
         linea_de_pobreza_urbana_moderada=ifelse(Small==0,Canasta_urbana_moderada*tot_integ,0),
         pobreza_rural_extrema=ifelse(ing_cor<linea_de_pobreza_rural_extrema,1,0),
         pobreza_rural_moderada=ifelse(ing_cor<linea_de_pobreza_rural_moderada,1,0),
         pobreza_urbana_extrema=ifelse(ing_cor<linea_de_pobreza_urbana_extrema,1,0),
         pobreza_urbana_moderada=ifelse(ing_cor<linea_de_pobreza_urbana_moderada,1,0))

Conc2010<-Conc2010%>%
  filter(Small==1)

design_2010<-svydesign(id=~upm,strata = ~est_dis, weights = ~factor,data = Conc2010)

pobreza_rural_extrema_2010<-round(svymean(~pobreza_rural_extrema, design=design_2010)*100,2)

pobreza_rural_extrema_2010

pobreza_rural_moderada_2010<-round(svymean(~pobreza_rural_moderada, design=design_2010)*100,2)

pobreza_rural_moderada_2010

rm(list=ls())


#### Urbanos 2010

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
Conc2010<-read.dbf("ConcYucatan2010.dbf",as.is = T)


Conc2010<-Conc2010%>%
  mutate(Canasta_rural_extrema=712.77*3,
         Canasta_rural_moderada=1378.05*3,
         Canasta_urbana_extrema=1012.12*3,
         Canasta_urbana_moderada=2185.79*3)

Conc2010<-Conc2010%>%
  mutate(linea_de_pobreza_rural_extrema=ifelse(Small==1,Canasta_rural_extrema*tot_integ,0),
         linea_de_pobreza_rural_moderada=ifelse(Small==1,Canasta_rural_moderada*tot_integ,0),
         linea_de_pobreza_urbana_extrema=ifelse(Small==0,Canasta_urbana_extrema*tot_integ,0),
         linea_de_pobreza_urbana_moderada=ifelse(Small==0,Canasta_urbana_moderada*tot_integ,0),
         pobreza_rural_extrema=ifelse(ing_cor<linea_de_pobreza_rural_extrema,1,0),
         pobreza_rural_moderada=ifelse(ing_cor<linea_de_pobreza_rural_moderada,1,0),
         pobreza_urbana_extrema=ifelse(ing_cor<linea_de_pobreza_urbana_extrema,1,0),
         pobreza_urbana_moderada=ifelse(ing_cor<linea_de_pobreza_urbana_moderada,1,0))

Conc2010<-Conc2010%>%
  filter(Small==0)

design_2010<-svydesign(id=~upm,strata = ~est_dis, weights = ~factor,data = Conc2010)

pobreza_urbana_extrema_2010<-round(svymean(~pobreza_urbana_extrema, design=design_2010)*100,2)

pobreza_urbana_extrema_2010

pobreza_urbana_moderada_2010<-round(svymean(~pobreza_urbana_moderada, design=design_2010)*100,2)

pobreza_urbana_moderada_2010

rm(list=ls())

##### Rurales 2018


library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
Conc2018<-read.dbf("ConcYucatan2018.dbf",as.is = T)

Conc2018<-Conc2018%>%
  mutate(Canasta_rural_extrema=1113.23*3,
         Canasta_rural_moderada=2008.71*3,
         Canasta_urbana_extrema=1556.24*3,
         Canasta_urbana_moderada=3089.37*3)

Conc2018<-Conc2018%>%
  mutate(linea_de_pobreza_rural_extrema=ifelse(Small==1,Canasta_rural_extrema*tot_integ,0),
         linea_de_pobreza_rural_moderada=ifelse(Small==1,Canasta_rural_moderada*tot_integ,0),
         linea_de_pobreza_urbana_extrema=ifelse(Small==0,Canasta_urbana_extrema*tot_integ,0),
         linea_de_pobreza_urbana_moderada=ifelse(Small==0,Canasta_urbana_moderada*tot_integ,0),
         pobreza_rural_extrema=ifelse(ing_cor<linea_de_pobreza_rural_extrema,1,0),
         pobreza_rural_moderada=ifelse(ing_cor<linea_de_pobreza_rural_moderada,1,0),
         pobreza_urbana_extrema=ifelse(ing_cor<linea_de_pobreza_urbana_extrema,1,0),
         pobreza_urbana_moderada=ifelse(ing_cor<linea_de_pobreza_urbana_moderada,1,0))

Conc2018<-Conc2018%>%
  filter(Small==1)

design_2018<-svydesign(id=~upm,strata = ~est_dis, weights = ~factor,data = Conc2018)

pobreza_rural_extrema_2018<-round(svymean(~pobreza_rural_extrema, design=design_2018)*100,2)

pobreza_rural_extrema_2018

pobreza_rural_moderada_2018<-round(svymean(~pobreza_rural_moderada, design=design_2018)*100,2)

pobreza_rural_moderada_2018

rm(list=ls())


#### urbanos 2018

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
Conc2018<-read.dbf("ConcYucatan2018.dbf",as.is = T)

Conc2018<-Conc2018%>%
  mutate(Canasta_rural_extrema=1113.23*3,
         Canasta_rural_moderada=2008.71*3,
         Canasta_urbana_extrema=1556.24*3,
         Canasta_urbana_moderada=3089.37*3)

Conc2018<-Conc2018%>%
  mutate(linea_de_pobreza_rural_extrema=ifelse(Small==1,Canasta_rural_extrema*tot_integ,0),
         linea_de_pobreza_rural_moderada=ifelse(Small==1,Canasta_rural_moderada*tot_integ,0),
         linea_de_pobreza_urbana_extrema=ifelse(Small==0,Canasta_urbana_extrema*tot_integ,0),
         linea_de_pobreza_urbana_moderada=ifelse(Small==0,Canasta_urbana_moderada*tot_integ,0),
         pobreza_rural_extrema=ifelse(ing_cor<linea_de_pobreza_rural_extrema,1,0),
         pobreza_rural_moderada=ifelse(ing_cor<linea_de_pobreza_rural_moderada,1,0),
         pobreza_urbana_extrema=ifelse(ing_cor<linea_de_pobreza_urbana_extrema,1,0),
         pobreza_urbana_moderada=ifelse(ing_cor<linea_de_pobreza_urbana_moderada,1,0))

Conc2018<-Conc2018%>%
  filter(Small==0)

design_2018<-svydesign(id=~upm,strata = ~est_dis, weights = ~factor,data = Conc2018)

pobreza_urbana_extrema_2018<-round(svymean(~pobreza_urbana_extrema, design=design_2018)*100,2)

pobreza_urbana_extrema_2018

pobreza_urbana_moderada_2018<-round(svymean(~pobreza_urbana_moderada, design=design_2018)*100,2)

pobreza_urbana_moderada_2018

rm(list=ls())

########## Hogares debajo de la linea de pobreza rural y urbano por raza ######

###2010 rurales

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
Conc2010<-read.dbf("ConcYucatan2010.dbf",as.is = T)

Conc2010<-Conc2010%>%
  mutate(Canasta_rural_extrema=712.77*3,
         Canasta_rural_moderada=1378.05*3,
         Canasta_urbana_extrema=1012.12*3,
         Canasta_urbana_moderada=2185.79*3)

Conc2010<-Conc2010%>%
  mutate(linea_de_pobreza_rural_extrema=ifelse(Small==1,Canasta_rural_extrema*tot_integ,0),
         linea_de_pobreza_rural_moderada=ifelse(Small==1,Canasta_rural_moderada*tot_integ,0),
         linea_de_pobreza_urbana_extrema=ifelse(Small==0,Canasta_urbana_extrema*tot_integ,0),
         linea_de_pobreza_urbana_moderada=ifelse(Small==0,Canasta_urbana_moderada*tot_integ,0),
         pobreza_rural_extrema=ifelse(ing_cor<linea_de_pobreza_rural_extrema,1,0),
         pobreza_rural_moderada=ifelse(ing_cor<linea_de_pobreza_rural_moderada,1,0),
         pobreza_urbana_extrema=ifelse(ing_cor<linea_de_pobreza_urbana_extrema,1,0),
         pobreza_urbana_moderada=ifelse(ing_cor<linea_de_pobreza_urbana_moderada,1,0))

Conc2010<-Conc2010%>%
  filter(Small==1)

design_2010<-svydesign(id=~upm,strata = ~est_dis, weights = ~factor,data = Conc2010)

pobreza_rural_extrema_2010<-round(svyby(~pobreza_rural_extrema,by=Conc2010$HogarIndig, design=design_2010,svymean)*100,2)

pobreza_rural_extrema_2010

pobreza_rural_moderada_2010<-round(svyby(~pobreza_rural_moderada,by=Conc2010$HogarIndig, design=design_2010,svymean)*100,2)

pobreza_rural_moderada_2010


rm(list=ls())


#### Urbanos
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
Conc2010<-read.dbf("ConcYucatan2010.dbf",as.is = T)

Conc2010<-Conc2010%>%
  mutate(Canasta_rural_extrema=712.77*3,
         Canasta_rural_moderada=1378.05*3,
         Canasta_urbana_extrema=1012.12*3,
         Canasta_urbana_moderada=2185.79*3)

Conc2010<-Conc2010%>%
  mutate(linea_de_pobreza_rural_extrema=ifelse(Small==1,Canasta_rural_extrema*tot_integ,0),
         linea_de_pobreza_rural_moderada=ifelse(Small==1,Canasta_rural_moderada*tot_integ,0),
         linea_de_pobreza_urbana_extrema=ifelse(Small==0,Canasta_urbana_extrema*tot_integ,0),
         linea_de_pobreza_urbana_moderada=ifelse(Small==0,Canasta_urbana_moderada*tot_integ,0),
         pobreza_rural_extrema=ifelse(ing_cor<linea_de_pobreza_rural_extrema,1,0),
         pobreza_rural_moderada=ifelse(ing_cor<linea_de_pobreza_rural_moderada,1,0),
         pobreza_urbana_extrema=ifelse(ing_cor<linea_de_pobreza_urbana_extrema,1,0),
         pobreza_urbana_moderada=ifelse(ing_cor<linea_de_pobreza_urbana_moderada,1,0))

Conc2010<-Conc2010%>%
  filter(Small==0)

design_2010<-svydesign(id=~upm,strata = ~est_dis, weights = ~factor,data = Conc2010)

pobreza_urbana_extrema_2010<-round(svyby(~pobreza_urbana_extrema, by=Conc2010$HogarIndig,design=design_2010,svymean)*100,2)

pobreza_urbana_extrema_2010

pobreza_urbana_moderada_2010<-round(svyby(~pobreza_urbana_moderada, by=Conc2010$HogarIndig, design=design_2010,svymean)*100,2)

pobreza_urbana_moderada_2010

rm(list=ls())

##### Rurales 2018


library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
Conc2018<-read.dbf("ConcYucatan2018.dbf",as.is = T)

Conc2018<-Conc2018%>%
  mutate(Canasta_rural_extrema=1113.23*3,
         Canasta_rural_moderada=2008.71*3,
         Canasta_urbana_extrema=1556.24*3,
         Canasta_urbana_moderada=3089.37*3)

Conc2018<-Conc2018%>%
  mutate(linea_de_pobreza_rural_extrema=ifelse(Small==1,Canasta_rural_extrema*tot_integ,0),
         linea_de_pobreza_rural_moderada=ifelse(Small==1,Canasta_rural_moderada*tot_integ,0),
         linea_de_pobreza_urbana_extrema=ifelse(Small==0,Canasta_urbana_extrema*tot_integ,0),
         linea_de_pobreza_urbana_moderada=ifelse(Small==0,Canasta_urbana_moderada*tot_integ,0),
         pobreza_rural_extrema=ifelse(ing_cor<linea_de_pobreza_rural_extrema,1,0),
         pobreza_rural_moderada=ifelse(ing_cor<linea_de_pobreza_rural_moderada,1,0),
         pobreza_urbana_extrema=ifelse(ing_cor<linea_de_pobreza_urbana_extrema,1,0),
         pobreza_urbana_moderada=ifelse(ing_cor<linea_de_pobreza_urbana_moderada,1,0))

Conc2018<-Conc2018%>%
  filter(Small==1)

design_2018<-svydesign(id=~upm,strata = ~est_dis, weights = ~factor,data = Conc2018)

pobreza_rural_extrema_2018<-round(svyby(~pobreza_rural_extrema,by=Conc2018$HogarIndig, design=design_2018,svymean)*100,2)

pobreza_rural_extrema_2018

pobreza_rural_moderada_2018<-round(svyby(~pobreza_rural_moderada,by=Conc2018$HogarIndig, design=design_2018,svymean)*100,2)

pobreza_rural_moderada_2018

rm(list=ls())

#### urbanos 2018

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Yucatan")
Conc2018<-read.dbf("ConcYucatan2018.dbf",as.is = T)


Conc2018<-Conc2018%>%
  mutate(Canasta_rural_extrema=1113.23*3,
         Canasta_rural_moderada=2008.71*3,
         Canasta_urbana_extrema=1556.24*3,
         Canasta_urbana_moderada=3089.37*3)

Conc2018<-Conc2018%>%
  mutate(linea_de_pobreza_rural_extrema=ifelse(Small==1,Canasta_rural_extrema*tot_integ,0),
         linea_de_pobreza_rural_moderada=ifelse(Small==1,Canasta_rural_moderada*tot_integ,0),
         linea_de_pobreza_urbana_extrema=ifelse(Small==0,Canasta_urbana_extrema*tot_integ,0),
         linea_de_pobreza_urbana_moderada=ifelse(Small==0,Canasta_urbana_moderada*tot_integ,0),
         pobreza_rural_extrema=ifelse(ing_cor<linea_de_pobreza_rural_extrema,1,0),
         pobreza_rural_moderada=ifelse(ing_cor<linea_de_pobreza_rural_moderada,1,0),
         pobreza_urbana_extrema=ifelse(ing_cor<linea_de_pobreza_urbana_extrema,1,0),
         pobreza_urbana_moderada=ifelse(ing_cor<linea_de_pobreza_urbana_moderada,1,0))

Conc2018<-Conc2018%>%
  filter(Small==0)

design_2018<-svydesign(id=~upm,strata = ~est_dis, weights = ~factor,data = Conc2018)

pobreza_urbana_extrema_2018<-round(svyby(~pobreza_urbana_extrema, by=Conc2018$HogarIndig,design=design_2018, svymean)*100,2)

pobreza_urbana_extrema_2018

pobreza_urbana_moderada_2018<-round(svyby(~pobreza_urbana_moderada,by=Conc2018$HogarIndig, design=design_2018,svymean)*100,2)

pobreza_urbana_moderada_2018

