# Projet d'econometrie spatiale 
  # Duco, Adjerad

# libraries ----------------------------------------
library(sf)
library(dplyr)
library(ggplot2)
library(readxl)
library(GISTools)
library(readr)
library(readxl)
library(RColorBrewer)
library(sp)
library(spdep)
library(dplyr)
library(spdep)
library(xtable)
library(bnstruct)
library(stargazer)
library(DMwR) # pour l'imputation knn
library(cartography)
library(maptools)

# working directory --------------------------------
setwd("~/DocumentsRaph/Economie/M2 ENSAE/Econometrie spatiale")

# Import fichier shapefile pour cartes -------------
communes <- 
  sf::st_read("C:/Users/Gab/Documents/DocumentsRaph/Economie/M2 ENSAE/Econometrie spatiale/data_total/COMMUNE.shp")

# Definir le systeme de projection Lambert 93 pour assurer bonne lecture (x,y)
st_crs(communes) <- 2154
  # on lui dit comment interpreter le systeme 
  # le warning est normal on ne change pas la projection


# Donnees pour les regressions ----------------------

  # Les donnees ont été récupérées sur data.gouv
# données locales 2014 de l'Insee au niveau de la commune
donnees_locales_2014 <- read_delim("donnees_locales_2014.csv", 
                                   ";", escape_double = FALSE, trim_ws = TRUE)
# données collectivites.locales.gouv
city_all <- readr::read_csv("city_all.csv") 

# table d'appartenance aux EPCI + type EPCI
communes_2014 <- readr::read_delim("donnees_communes_epci_2014.csv", 
                                                          ";", escape_double = FALSE, trim_ws = TRUE, 
                                                          skip = 1)


# Données ménages pour TH, TFPNB,TFPB
data_loc_2014 <- donnees_locales_2014 %>% dplyr::select(CODGEO, `Nb Menages`,
                                                     `Nb Residences Principales`,
                                                     `Nb Residences Secondaires`,
                                                     `Nb proprietaire`, `Nb Femme`,
                                                     `Score PIB`,
                                                     Population, `Nb Majeurs`,
                                                     `Nb Etudiants`, `Nb Log Vacants`,
                                                     `Nb Logement`, `Nb Atifs`,
                                                     `Nb Hotel`,`Moyenne Revenus Fiscaux Departementaux`)#%>%
data_loc_2014 <- data_loc_2014 %>%   dplyr::rename(INSEE_COM = CODGEO,
                nb_menage = `Nb Menages`,
                nb_residences_princ = `Nb Residences Principales`, 
                nb_residences_second = `Nb Residences Secondaires`,
                nb_proprio =`Nb proprietaire` ,
                nb_femme = `Nb Femme`,
                pib =`Score PIB`,
                pop = Population,
                nb_majeurs = `Nb Majeurs`,
                nb_etudiants = `Nb Etudiants`,
                nb_log_vacant = `Nb Log Vacants`,
                nb_log = `Nb Logement`,
                nb_actif =`Nb Atifs`,
                nb_hotel =`Nb Hotel`,
                moyenne_revenu_fiscaux_dep =`Moyenne Revenus Fiscaux Departementaux`)
rm(donnees_locales_2014)
communes_2014 <- communes_2014 %>% dplyr::rename(INSEE_COM = CODGEO)%>%
  dplyr::select(INSEE_COM, REG, REG2016, EPCI, NATURE_EPCI)
dataloc_2014 <- data_loc_2014 %>% dplyr::right_join(communes_2014, by = "INSEE_COM")
rm(data_loc_2014, communes_2014)

# Constitution de la base de données ----------------
  # Selection des taux votes par conseil municipal
city_all <- city_all %>% 
  dplyr::rename(INSEE_COM = `cog (code officiel géographique)`,
                dotation_fonct = `dotation globale de fonctionnement`,
                taxe_habit = `taxe d'habitation (y compris thlv) - taux`,
                taxe_add_fonciere_non_bati = `taxe additionnelle à la taxe foncière sur les propriétés non bâties - taux`,
                impo_forfaitaire_entreprise_reseau = `impositions forfaitaires sur les entreprises de réseau - taux`,
                cotis_va_entreprise = `cotisation sur la valeur ajoutée des entreprises - taux`,
                cotis_fonciere_entreprise = `cotisation foncière des entreprises - taux`,
                taxe_prof = `taxe professionnelle (hors bases écrêtées) - taux`,
                encours_dette = `encours total de la dette au 31/12/n`,
                depense_equipement = `dépenses d'équipement`,
                caf = `capacité d'autofinancement = caf`,
                taxe_fonciere_non_bati = `taxe foncière sur les propriétés non bâties - taux`,
                impot_locaux =`impôts locaux`,
                resultat_comptable = `resultat comptable = a - b = r`, 
                taxe_fonciere_bati = `taxe foncière sur les propriétés bâties - taux`,
                taxe_surface_commerciales = `taxe sur les surfaces commerciales - taux`,
                annee = année,
                pop = population)%>%
  dplyr::select(INSEE_COM, dotation_fonct,taxe_habit,taxe_add_fonciere_non_bati ,
                impo_forfaitaire_entreprise_reseau, cotis_va_entreprise,cotis_fonciere_entreprise,
                taxe_prof,encours_dette ,depense_equipement ,caf , taxe_fonciere_non_bati ,
                impot_locaux , resultat_comptable ,taxe_fonciere_bati,
                taxe_surface_commerciales, annee , pop )%>%
  dplyr::mutate(taxe_add_fonciere_non_bati=as.numeric(taxe_add_fonciere_non_bati),
                impo_forfaitaire_entreprise_reseau= as.numeric(impo_forfaitaire_entreprise_reseau),
                cotis_va_entreprise = as.numeric(cotis_va_entreprise),
                cotis_fonciere_entreprise=as.numeric(cotis_fonciere_entreprise),
                taxe_surface_commerciales = as.numeric(taxe_surface_commerciales))

# premier travail en coupe
city <- filter(city_all, annee == 2014)
#city <- city_all
rm(city_all)

#city <- city  %>% dplyr::select(INSEE_COM, cotis)

# On fusionne avec les données locales 2014

city2 <- city %>% dplyr::left_join(dataloc_2014, by = "INSEE_COM")
dataset <- communes %>% dplyr::left_join(city2, by ="INSEE_COM")
data <- as(dataset, "Spatial")
save(data, file = "spatialpolygondata.RData")
save(dataset, file = "data2014.RData")
rm(city,city2,communes,dataloc_2014)
head(dataset)

# focus sur quelques régions
load("data2014.RData")
# Ile de France, Alsace Champagne Ardenne Lorraine, Bretagne, NPDC
# centre val de loire, 
data_regions <- dataset %>% filter(CODE_REG %in% c(11,32,44,28))
save(data_regions, file = "data_regions.RData")
data_reg <- as(data_regions, "Spatial")
save(data_reg, file = "data_reg.RData")

# -------------------------------------------------------------------------
load("data_regions.RData")
ggplot(data = data_regions) + geom_sf(aes(fill = taxe_habit),colour = NA) +
  coord_sf(datum = NA)+
  ggtitle("Régions spécifiques nord France TH 2014")+
  theme(text=element_text(size=12),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(face ="italic"),
        legend.position = "right")+  theme_bw()+ scale_fill_gradientn(colours = mycols)

ggplot(data = data_regions) + geom_sf(aes(fill = taxe_fonciere_non_bati),colour = NA) +
  coord_sf(datum = NA)+
  ggtitle("Régions spécifiques nord France TPFNB 2014")+
  theme(text=element_text(size=12),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(face ="italic"),
        legend.position = "right")+ theme_bw()+ scale_fill_gradientn(colours = mycols)

ggplot(data = data_regions) + geom_sf(aes(fill = taxe_fonciere_bati),colour = NA) +
  coord_sf(datum = NA)+
  ggtitle("Régions spécifiques nord France TPFB 2014")+
  theme(text=element_text(size=12),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(face ="italic"),
        legend.position = "right")+ theme_bw()+ scale_fill_gradientn(colours = mycols)

# Graphiques france ------------------------------------------------

plot_france_taux_annee <- function(dataset, num_annee, nom_var){
  dataset2 <- dataset %>% dplyr::filter(annee == num_annee)
  dataset2 <- dataset2 %>% dplyr::mutate_(.dots=setNames(nom_var,"var"))%>%
    dplyr::mutate(var = as.numeric(var))
  ggplot(data = dataset2) + geom_sf(aes(fill = var),colour = NA) +
    coord_sf(datum = NA)+  scale_fill_gradientn(colours = rev(heat.colors(11)))
  ggplot2::ggsave(paste0("France",num_annee,nom_var,".png"))
}
plot_france_taux_annee(dataset, 2014,"taxe_habit")
plot_france_taux_annee(dataset, 2014,"taxe_fonciere_bati")
plot_france_taux_annee(dataset, 2014,"taxe_fonciere_non_bati")

# Ne pas faire tourner, ce sera pour générer automatiquement toutes les cartes, mais
# c'est très long
annee <- c(2000,2015)
var <- c("taxe_habit","taxe_fonciere_non_bati","taxe_fonciere_bati")

# differents plots
for (i in annee){
  for (nom_var in var){
    plot_france_taux_annee(dataset, i,nom_var)
  }
}


# Analyse statistique -----------------------------------------

# -----------------------------------------------------------------
# checkpoint

#load("spatialpolygondata.RData") # all France
load("data_reg.RData") # specific regions
data <- data_reg
class(data)
# The object has slots:
  # - the first slot is the data
  # - the second slot is the polygons 
  # - the third slot is the bounding box drawn around the boundaries
  # - the fourth slot contains the porjections proj4string
# on repasse à un objet sp pour utiliser les packages de regression
str(slot(data,"data"))


# summary stat of variable of interest
summary(data@data$taxe_habit)
summary(data@data$taxe_fonciere_non_bati)
summary(data@data$taxe_fonciere_bati)

# Histogrammes --------------------------------

labelY <- "Taxe d'habitation 2014"
mycols <- carto.pal(pal1 = "green.pal", n1 = 5, pal2 = "orange.pal", n2 = 10)
hist(data@data$taxe_habit,breaks = 15,col=mycols,xlab = "Y",main=labelY)

labelY <- "TFNB 2014"
hist(data@data$taxe_fonciere_non_bati,breaks = 15,col=mycols,xlab = "Y",main=labelY)

labelY <- "TFB 2014"
hist(data@data$taxe_fonciere_bati,breaks = 15,col=mycols,xlab = "Y",main=labelY)

# Imputation par plus proche voisins des valeurs manquantes -------
    # seulement sur la France entiere
# imputation par 10 voisins plus proche par knn
y <- data@data$taxe_habit
summary(data@data$taxe_habit)
x <- cbind(data@data$NOM_DEP,data@data$SUPERFICIE,data@data$POPULATION)
df <- data.frame(cbind(y,x))
imput <- knnImputation(df, k =10, meth = "median")
data@data$taxe_habit <- imput$y

y <- data@data$taxe_fonciere_bati # to run for TPFB
summary(data@data$taxe_fonciere_bati)
x <- cbind(data@data$NOM_DEP,data@data$SUPERFICIE,data@data$POPULATION)
df <- data.frame(cbind(y,x))
imput <- knnImputation(df, k =10, meth = "median")
data@data$taxe_fonciere_bati <- imput$y

y <- data@data$taxe_fonciere_non_bati # to run for TPFNB
summary(data@data$taxe_fonciere_non_bati)
x <- cbind(data@data$NOM_DEP,data@data$SUPERFICIE,data@data$POPULATION)
df <- data.frame(cbind(y,x))
imput <- knnImputation(df, k =10, meth = "median")
data@data$taxe_fonciere_non_bati <- imput$y


y <- data@data$dotation_fonct 
x <- cbind(data@data$NOM_DEP,data@data$SUPERFICIE,data@data$POPULATION)
df <- data.frame(cbind(y,x))
imput <- knnImputation(df, k =10, meth = "median")
data@data$dotation_fonct <- imput$y


y <- data@data$encours_dette 
x <- cbind(data@data$NOM_DEP,data@data$SUPERFICIE,data@data$POPULATION)
df <- data.frame(cbind(y,x))
imput <- knnImputation(df, k =10, meth = "median")
data@data$encours_dette <- imput$y
# simple ols
# On met en forme les variables

data@data$lnb_log <- log(data@data$nb_log)
data@data$lnb_log[is.infinite(data@data$lnb_log)] <- 0
data@data$lnb_log[is.na(data@data$lnb_log)] <- 0 # eventuellement knn imput

data@data$prop_log_vacant <- data@data$nb_log_vacant/data@data$nb_log
data@data$prop_log_vacant[is.na(data@data$prop_log_vacant)] <- 0 # eventuellement knn imput

data@data$lnb_menage <- log(data@data$nb_menage)
data@data$lnb_menage[is.infinite(data@data$lnb_menage)] <- 0
data@data$lnb_menage[is.na(data@data$lnb_menage)] <- 0 # eventuellement knn imput

data@data$lnb_residences_princ <- log(data@data$nb_residences_princ)
data@data$lnb_actif <- log(data@data$nb_actif)
data@data$lnb_actif[is.infinite(data@data$lnb_actif)] <- 0
data@data$lnb_actif[is.na(data@data$lnb_actif)] <- 0 # eventuellement knn imput

data@data$ldepense_equipement <- log(data@data$depense_equipement)
data@data$ldepense_equipement[is.infinite(data@data$ldepense_equipement)] <- 0
data@data$lencours_dette <- log(data@data$encours_dette)
data@data$lencours_dette[is.infinite(data@data$lencours_dette)] <- 0
data@data$lencours_dette[is.na(data@data$lencours_dette)] <- 0
data@data$lpop <- log(data@data$POPULATION)
data@data$lpop[is.infinite(data@data$lpop)] <- 0

data@data$lnb_proprio <- log(data@data$nb_proprio)
data@data$lnb_proprio[is.infinite(data@data$lnb_proprio)] <- 0
data@data$lnb_proprio[is.na(data@data$lnb_proprio)] <- 0 # eventuellement knn imput
data@data$ldotation_fonct <- log(data@data$dotation_fonct)
data@data$ldotation_fonct[is.infinite(data@data$ldotation_fonct)] <- 0
data@data$lpib <- log(data@data$pib)
data@data$lpib[is.infinite(data@data$lpib)] <- 0
data@data$lpib[is.na(data@data$lpib)] <- 0


summary(data@data$depense_equipement)
summary(data@data$lencours_dette)
summary(data@data$ldotation_fonct)
summary(data@data$lnb_proprio)
summary(data@data$lnb_log)
summary(data@data$prop_log_vacant)
summary(data@data$lnb_menage)
summary(data@data$lnb_actif)
summary(data@data$lpib)
summary(data@data$lpop)



# lencours_dette, dotation_fonc
# eventuellement ajouter var sur activité éco, chomage, nb entreprises
fit_lm_th <- lm(taxe_habit~lnb_log+lpop+lnb_menage+
                  lencours_dette+ldotation_fonct, data = data@data,
               na.action ="na.exclude")
summary(fit_lm_th)

fit_lm_tfb <- lm(taxe_fonciere_bati~lnb_log+lpop+lnb_proprio+
                  lencours_dette + ldotation_fonct, data = data@data,
                 na.action ="na.exclude")
summary(fit_lm_tfb)

fit_lm_tfnb <- lm(taxe_fonciere_non_bati~lnb_log+lnb_proprio+
                  lencours_dette + ldotation_fonct, data = data@data,
                  na.action ="na.exclude")
summary(fit_lm_tfnb)
stargazer(fit_lm_th,fit_lm_tfnb,fit_lm_tfb, 
          dep.var.labels=c("Taxe d'Habitation","TPFNB","TPFB"),
          covariate.labels=c("Nb. logements (log)","Population (log)",
                             "Nb. ménages (log)", "Nb. propriétaires (log)",
                             "Encours dette (log)", "Dotation fonctionnelle (log)"))
# modeling spatial dependence
  # weight matrix based on queen
# attention cela prend du temps de créer la matrice des voisins
list_queen <- spdep::poly2nb(data, queen = T)
#save(list_queen, file = "liste_queen.RData")
save(list_queen, file = "liste_queen_reg.RData")

#load("liste_queen.RData")
load("liste_queen_reg.RData")
# this is rapid:
W_q <- spdep::nb2listw(list_queen, style = "W", zero.policy = TRUE)
print(W_q, zero.policy=TRUE) # 12 communes sans voisins
#plot(W_q,coordinates(data))

# weight matrix based on rook
# attention, pareil, cela prend du temps de créer cette matrice:
list_rook <- spdep::poly2nb(data, queen = F)
#save(list_rook, file = "liste_rook.RData")
save(list_rook, file = "liste_rook_reg.RData")
#load("liste_rook.RData")
load("liste_rook_reg.RData")
# this is rapid:
W_r <- spdep::nb2listw(list_rook, style = "W", zero.policy = T)
print(W_r, zero.policy=TRUE) # same 12 regions with no links
#plot(W_r,coordinates(data))


  # weight matrix based on distance
coords <- coordinates(data)
# long command:
W_dist <- dnearneigh(coords,0,1,longlat = FALSE)
#plot(W_dist,coordinates(data))

# Spatial Autocorrelation model -------------------------------------
  # similar to an autoregressive process in time series


# This implies that OLS estimates in the non spatial
# model will be biased and inconsistent.

# Spatial Error modem -----------------------------------------------

# If this is the "true" form of spatial dependence OLS estimates will be unbiased but inefficient

# Testing for spatial autocorrelation -------------------------------
# To perform a Moran test on our data we need two inputs, an lm regression object (estimated
# in the OLS section) and the spatial weight matrix
W <- W_q
#W <- W_r # run to get Moran stat with rook criteria for W
moran.lm<-lm.morantest(fit_lm_th,zero.policy = TRUE, W, alternative = "two.sided")
print(moran.lm)

# Diagramme de Moran ----------------------------------------------------
moran.plot(data@data$taxe_habit, listw = W, zero.policy = T, labels= F)
moran.plot(data@data$taxe_fonciere_non_bati, listw = W, zero.policy = T, labels= F)
moran.plot(data@data$taxe_fonciere_bati, listw = W, zero.policy = T, labels= F)

# construction diagramme de Moran
tab <- data@data
tab$Y <- tab$taxe_habit
tab$Y_std<-scale(tab$Y)
tab$Y_lag<-lag.listw(W,tab$Y)
tab$Y_std_lag<-lag.listw(W,tab$Y_std)
par(mfrow=c(1,1), mar=c(4,4,2,2))
plot(x=tab$Y_std,y=tab$Y_std_lag,main="Diagramme de Moran TH",xlab = "Observed",ylab
     ="Lagged", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.4))
abline(h=0,v=0)
modele_moran<-lm(tab$Y_std_lag~tab$Y_std)
abline(modele_moran, lty=2,lwd=1,col="gray20")
text(-1,-1,"Low-Low",col="grey")
text(-1,1,"Low-High",col="gray20")
text(1,-1,"High-Low",col="gray20")
text(1,1,"High-High",col="gray79")

tab <- data@data
tab$Y <- tab$taxe_fonciere_non_bati
tab$Y_std<-scale(tab$Y)
tab$Y_lag<-lag.listw(W,tab$Y)
tab$Y_std_lag<-lag.listw(W,tab$Y_std)
par(mfrow=c(1,1), mar=c(4,4,2,2))
plot(x=tab$Y_std,y=tab$Y_std_lag,main="Diagramme de Moran TPFNB",xlab = "Observed",ylab
     ="Lagged", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.4))
abline(h=0,v=0)
modele_moran<-lm(tab$Y_std_lag~tab$Y_std)
abline(modele_moran, lty=2,lwd=1,col="gray20")
text(-1,-1,"Low-Low",col="grey")
text(-1,1,"Low-High",col="gray20")
text(1,-1,"High-Low",col="gray20")
text(1,1,"High-High",col="gray79")

tab <- data@data
tab$Y <- tab$taxe_fonciere_bati
tab$Y_std<-scale(tab$Y)
tab$Y_lag<-lag.listw(W,tab$Y)
tab$Y_std_lag<-lag.listw(W,tab$Y_std)
par(mfrow=c(1,1), mar=c(4,4,2,2))
plot(x=tab$Y_std,y=tab$Y_std_lag,main="Diagramme de Moran TPFB",xlab = "Observed",ylab
     ="Lagged", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.4))
abline(h=0,v=0)
modele_moran<-lm(tab$Y_std_lag~tab$Y_std)
abline(modele_moran, lty=2,lwd=1,col="gray20")
text(-1,-1,"Low-Low",col="grey")
text(-1,1,"Low-High",col="gray20")
text(1,-1,"High-Low",col="gray20")
text(1,1,"High-High",col="gray79")

# Hot cold spots ------------------------------
# local Moran I
lm_th <- localmoran(data@data$taxe_habit, listw = W, zero.policy = T)
tab2 <- as.data.frame(lm_th)
  # La variable Ii est l'indice d'autocorrélation spatiale local de la région
    # l'indice sera positif pour les provinces de type Low-Low ou High-High mais négatif pour les
    # provinces de type Low-High ou High-Low.
  # la variable Pr(z>0) exprime la même chose en fournissant la p-value du test d'autocorrélation
    # spatiale locale.
tab <- data@data
tab$Y <- tab$taxe_habit
tab$Y_std<-scale(tab$Y)
tab$Y_lag<-lag.listw(W,tab$Y)
tab$Y_std_lag<-lag.listw(W,tab$Y_std)
tabres <- cbind(tab,tab2)
head(tabres)
par(mfrow=c(1,1),mar=c(2,2,2,2))
# Carte de Moran
q1<-as.factor(tabres$Y_std>0)
levels(q1)<-c("Low","High")
q2<-as.factor(tabres$Y_std_lag>0)
levels(q2)<-c("Low","High")
MapMoran<-paste(as.character(q1),as.character(q2),sep="-")
MapMoran[abs(tabres$Z.Ii)<2]<-"Non Sign."
labels=c("High-High","Low-Low","High-Low","Low-High","Non Sign.")
tab$Moran_type<-factor(MapMoran,levels=labels)
tab$Moran_color<-tab$Moran_type
colors=c("red","blue","lightpink","skyblue2","white")
levels(tab$Moran_color)<-colors
tab$Moran_color<-as.character(tab$Moran_color)
colors<-c("red","blue","lightpink","skyblue2","white")
plot(data,col=tab$Moran_color)
legend("topright",legend=labels, fill=colors,bty="n")

lm_tfnb <- localmoran(data@data$taxe_fonciere_non_bati, listw = W, zero.policy = T)
tab2 <- as.data.frame(lm_tfnb)
tab <- data@data
tab$Y <- tab$taxe_fonciere_non_bati
tab$Y_std<-scale(tab$Y)
tab$Y_lag<-lag.listw(W,tab$Y)
tab$Y_std_lag<-lag.listw(W,tab$Y_std)
tabres <- cbind(tab,tab2)
head(tabres)
par(mfrow=c(1,1),mar=c(2,2,2,2))
# Carte de Moran
q1<-as.factor(tabres$Y_std>0)
levels(q1)<-c("Low","High")
q2<-as.factor(tabres$Y_std_lag>0)
levels(q2)<-c("Low","High")
MapMoran<-paste(as.character(q1),as.character(q2),sep="-")
MapMoran[abs(tabres$Z.Ii)<2]<-"Non Sign."
labels=c("High-High","Low-Low","High-Low","Low-High","Non Sign.")
tab$Moran_type<-factor(MapMoran,levels=labels)
tab$Moran_color<-tab$Moran_type
colors=c("red","blue","lightpink","skyblue2","white")
levels(tab$Moran_color)<-colors
tab$Moran_color<-as.character(tab$Moran_color)
colors<-c("red","blue","lightpink","skyblue2","white")
plot(data,col=tab$Moran_color)
legend("topright",legend=labels, fill=colors,bty="n")

lm_tfb <- localmoran(data@data$taxe_fonciere_bati, listw = W, zero.policy = T)
tab2 <- as.data.frame(lm_tfb)
tab <- data@data
tab$Y <- tab$taxe_fonciere_bati
tab$Y_std<-scale(tab$Y)
tab$Y_lag<-lag.listw(W,tab$Y)
tab$Y_std_lag<-lag.listw(W,tab$Y_std)
tabres <- cbind(tab,tab2)
head(tabres)
par(mfrow=c(1,1),mar=c(2,2,2,2))
# Carte de Moran
q1<-as.factor(tabres$Y_std>0)
levels(q1)<-c("Low","High")
q2<-as.factor(tabres$Y_std_lag>0)
levels(q2)<-c("Low","High")
MapMoran<-paste(as.character(q1),as.character(q2),sep="-")
MapMoran[abs(tabres$Z.Ii)<2]<-"Non Sign."
labels=c("High-High","Low-Low","High-Low","Low-High","Non Sign.")
tab$Moran_type<-factor(MapMoran,levels=labels)
tab$Moran_color<-tab$Moran_type
colors=c("red","blue","lightpink","skyblue2","white")
levels(tab$Moran_color)<-colors
tab$Moran_color<-as.character(tab$Moran_color)
colors<-c("red","blue","lightpink","skyblue2","white")
plot(data,col=tab$Moran_color)
legend("topright",legend=labels, fill=colors,bty="n")


# Lagrande multiplier test --------------
# LM test for spatial dependence are included in lm.LMtests
# Taxe habitation
LM <- lm.LMtests(fit_lm_th, W, zero.policy = TRUE, test = "all")
print(LM)

LMerr <- LM[["LMerr"]]
LMlag <- LM[["LMlag"]]
RLMerr <- LM[["RLMerr"]]
RLMlag <- LM[["RLMlag"]]
SARMA <- LM[["SARMA"]]
df <- data.frame('SARMA' = c(SARMA$statistic,SARMA$p.value),
                 'LMerr' = c(LMerr$statistic,LMerr$p.value),
                 'LMlag' = c(LMlag$statistic,LMlag$p.value),
                 'RLMerr' = c(RLMerr$statistic,RLMerr$p.value),
                 'RLMlag' = c(RLMlag$statistic,RLMlag$p.value) )

table_tests <- xtable(t(df))
colnames(table_tests) <- c("stat","p_value")
knitr::kable(table_tests)

  # il y a de l'autocorrélation 
  


# LM test for spatial dependence are included in lm.LMtests
# Taxe foncier bati
LM <- lm.LMtests(fit_lm_tfb, W, zero.policy = TRUE, test = "all")
print(LM)

LMerr <- LM[["LMerr"]]
LMlag <- LM[["LMlag"]]
RLMerr <- LM[["RLMerr"]]
RLMlag <- LM[["RLMlag"]]
SARMA <- LM[["SARMA"]]
df <- data.frame('SARMA' = c(SARMA$statistic,SARMA$p.value),
                 'LMerr' = c(LMerr$statistic,LMerr$p.value),
                 'LMlag' = c(LMlag$statistic,LMlag$p.value),
                 'RLMerr' = c(RLMerr$statistic,RLMerr$p.value),
                 'RLMlag' = c(RLMlag$statistic,RLMlag$p.value) )

table_tests <- xtable(t(df))
colnames(table_tests) <- c("stat","p_value")
knitr::kable(table_tests)

# il y a de l'autocorrélation 


# LM test for spatial dependence are included in lm.LMtests
# Taxe foncier non bati
LM <- lm.LMtests(fit_lm_tfnb, W, zero.policy = TRUE, test = "all")
print(LM)

LMerr <- LM[["LMerr"]]
LMlag <- LM[["LMlag"]]
RLMerr <- LM[["RLMerr"]]
RLMlag <- LM[["RLMlag"]]
SARMA <- LM[["SARMA"]]
df <- data.frame('SARMA' = c(SARMA$statistic,SARMA$p.value),
                 'LMerr' = c(LMerr$statistic,LMerr$p.value),
                 'LMlag' = c(LMlag$statistic,LMlag$p.value),
                 'RLMerr' = c(RLMerr$statistic,RLMerr$p.value),
                 'RLMlag' = c(RLMlag$statistic,RLMlag$p.value) )

table_tests <- xtable(t(df))
colnames(table_tests) <- c("stat","p_value")
knitr::kable(table_tests)

# il y a de l'autocorrélation 
# on penche vers un modele SARMA

# Spatial regressions -------------------------------
# SAR model -----------------------------------------
memory.limit()
fit_sar_th <- lagsarlm(taxe_habit~lnb_log+lpop+
                         lencours_dette + ldotation_fonct+lpib, data = data@data, W,
                    zero.policy = T)
summary(fit_sar_th)
fit_sar_tfnb <- lagsarlm(taxe_fonciere_non_bati~lnb_log+lpop+
                         lencours_dette + ldotation_fonct+lpib, data = data@data, W,
                       zero.policy = T)
summary(fit_sar_tfnb)
fit_sar_tfb <- lagsarlm(taxe_fonciere_bati~lnb_log+lpop+
                         lencours_dette + ldotation_fonct+lpib, data = data@data, W,
                       zero.policy = T)
summary(fit_sar_tfb)
# sar output to latex
stargazer(fit_sar_th,fit_sar_tfnb,fit_sar_tfb, 
          dep.var.labels=c("Taxe d'Habitation","TPFNB","TPFB"),
          covariate.labels=c("Nb. logements (log)","Population (log)",
                             "Encours dette (log)", "Dotation fonctionnelle (log)",
                             "PIB (log)"))

# modele par MLE (fonctionne, alors que le premier non)
sar2sls_fit_th <-stsls(taxe_habit~lnb_log+lpop+
                      lencours_dette + ldotation_fonct+lpib, data = data@data, W,
                    zero.policy = T, na.action = "na.exclude")


summary(sar2sls_fit_th)
sar2sls_fit_tfnb <-stsls(taxe_fonciere_non_bati~lnb_log+lpop+
                         lencours_dette + ldotation_fonct+lpib, data = data@data, W,
                       zero.policy = T, na.action = "na.exclude")


summary(sar2sls_fit_tfnb)
sar2sls_fit_tfb <-stsls(taxe_fonciere_bati~lnb_log+lpop+
                           lencours_dette + ldotation_fonct+lpib, data = data@data, W,
                         zero.policy = T, na.action = "na.exclude")


summary(sar2sls_fit_tfb)

data@data$ols_res_th <- resid(fit_lm_th) # residuals ols
data@data$sar_res_th <- resid(sar2sls_fit_th) # residuals sar

data@data$ols_res_tfb <- resid(fit_lm_tfb) # residuals ols
data@data$sar_res_tfb <- resid(sar2sls_fit_tfb) # residuals sar

data@data$ols_res_tfnb <- resid(fit_lm_tfnb) # residuals ols
data@data$sar_res_tfnb <- resid(sar2sls_fit_tfnb) # residuals sar

# plot des résidus pour TH - takes long time
spplot(data,"ols_res_th",
       at=seq(min(data@data$ols_res_th,na.rm=TRUE),max(data@data$ols_res_th,na.rm=TRUE),length=12),col.regions=rev(brewer.pal(11,"RdBu")))

spplot(data,"sar_res_th",
       at=seq(min(data@data$sar_res_th,na.rm=TRUE),max(data@data$sar_res_th,na.rm=TRUE),length=12),col.regions=rev(brewer.pal(11,"RdBu")))

# Marginal effects ---------------------------------
imp_th <- impacts(fit_sar_th, listw=W )
imp_tfnb <- impacts(fit_sar_tfnb, listw=W )
imp_tfb <- impacts(fit_sar_tfb, listw=W )

# SEM models ---------------------------------------
errorsalm_th<-errorsarlm(taxe_habit~lnb_log+lpop+
                           lencours_dette + ldotation_fonct+lpib, data=data@data,
                          W,zero.policy = T, na.action = "na.exclude" )
summary(errorsalm_th)
errorsalm_tfnb<-errorsarlm(taxe_fonciere_non_bati~lnb_log+lpop+
                           lencours_dette + ldotation_fonct+lpib, data=data@data,
                         W,zero.policy = T, na.action = "na.exclude" )
summary(errorsalm_tfnb)
errorsalm_tfb<-errorsarlm(taxe_fonciere_bati~lnb_log+lpop+
                           lencours_dette + ldotation_fonct+lpib, data=data@data,
                         W,zero.policy = T, na.action = "na.exclude" )
summary(errorsalm_tfb)
# sem output to latex
stargazer(errorsalm_th,errorsalm_tfnb,errorsalm_tfb, 
          dep.var.labels=c("Taxe d'Habitation","TPFNB","TPFB"),
          covariate.labels=c("Nb. logements (log)","Population (log)",
                             "Encours dette (log)", "Dotation fonctionnelle (log)",
                             "PIB (log)"))
# approach with feasible generalized least squares
fgls_th<-GMerrorsar(taxe_habit~lnb_log+lpop+
                      lencours_dette + ldotation_fonct+lpib, data=data@data, W,zero.policy = T)
summary(fgls_th)
# Geographically weighted regression ------------------------------------
gwr_model_th<- gwr(taxe_fonciere_bati~lnb_log+lpop+
                     lencours_dette + ldotation_fonct+lpib,data=data@data,
                bandwidth=750000,fit.points=us_grid2)