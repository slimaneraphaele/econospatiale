# Projet d'econometrie spatiale 

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
library(xtable)
library(bnstruct)
library(stargazer)
library(DMwR) # pour l'imputation knn
library(cartography)
library(maptools)
library(reshape2)
library(plm)
library(splm)


# working directory --------------------------------
setwd("~/DocumentsRaph/Economie/M2 ENSAE/Econometrie spatiale")
#setwd("U:/2018-05-13-econospatialeRaph")
# Import fichier shapefile pour cartes -------------
communes <- 
  sf::st_read("C:/Users/Gab/Documents/DocumentsRaph/Economie/M2 ENSAE/Econometrie spatiale/data_total/COMMUNE.shp")

# Definir le systeme de projection Lambert 93 pour assurer bonne lecture (x,y)
st_crs(communes) <- 2154
  # on lui dit comment interpreter le systeme 
  # le warning est normal on ne change pas la projection


# Donnees pour les regressions ----------------------

  # Les donnees ont ?t? r?cup?r?es sur data.gouv
# donn?es locales 2014 de l'Insee au niveau de la commune
donnees_locales_2014 <- read_delim("donnees_locales_2014.csv", 
                                   ";", escape_double = FALSE, trim_ws = TRUE)
# donn?es collectivites.locales.gouv
city_all <- readr::read_csv("city_all.csv") 

# table d'appartenance aux EPCI + type EPCI
communes_2014 <- readr::read_delim("donnees_communes_epci_2014.csv", 
                                                          ";", escape_double = FALSE, trim_ws = TRUE, 
                                                          skip = 1)

# Types EPCI FPU ou FA (fiscalit? unique ou fiscalit? additionnelle)
base_epci_fisca2014 <- read_delim("epcisanscom2014.csv", 
                                              ";", escape_double = FALSE, trim_ws = TRUE)

# Donn?es m?nages pour TH, TFPNB,TFPB
data_loc_2014 <- donnees_locales_2014 %>% dplyr::select(CODGEO, `Nb Menages`,
                                                     `Nb Residences Principales`,
                                                     `Nb Residences Secondaires`,
                                                     `Nb proprietaire`, `Nb Femme`,
                                                     `PIB Regionnal`,
                                                     Population, `Nb Majeurs`,
                                                     `Nb Etudiants`, `Nb Log Vacants`,
                                                     `Nb Logement`, `Nb Atifs`,
                                                     `Nb Hotel`,`Moyenne Revenus Fiscaux Departementaux`,
                                                     `Orientation Economique`,
                                                     `Evolution Pop %`,
                                                     `Nb Mineurs`,
                                                     `Nb Entreprises Secteur Services`,
                                                     `Nb Entreprises Secteur Commerce`,
                                                     `Nb Entreprises Secteur Construction`,
                                                     `Nb Entreprises Secteur Industrie`,
                                                     `Moyenne Revenus Fiscaux Regionaux`,
                                                     `Urbanite Ruralite`,
                                                     `Score Urbanite`,
                                                     `Nb institution de Education, sante, action sociale, administration`,
                                                     `Capacite Fiscale`,
                                                     `Moyenne Revnus fiscaux`)#%>%
data_loc_2014 <- data_loc_2014 %>%   dplyr::rename(INSEE_COM = CODGEO,
                nb_menage = `Nb Menages`,
                nb_residences_princ = `Nb Residences Principales`, 
                nb_residences_second = `Nb Residences Secondaires`,
                nb_proprio =`Nb proprietaire` ,
                nb_femme = `Nb Femme`,
                pib_region =`PIB Regionnal`,
                pop = Population,
                nb_majeurs = `Nb Majeurs`,
                nb_etudiants = `Nb Etudiants`,
                nb_log_vacant = `Nb Log Vacants`,
                nb_log = `Nb Logement`,
                nb_actif =`Nb Atifs`,
                nb_hotel =`Nb Hotel`,
                moyenne_revenu_fiscaux_dep =`Moyenne Revenus Fiscaux Departementaux`,
                moyenne_revenu_fiscaux=`Moyenne Revnus fiscaux`,
                cap_fiscale=`Capacite Fiscale`,
                nb_educ_sante_actsoc_admin=`Nb institution de Education, sante, action sociale, administration`,
                score_urban=`Score Urbanite`,
                urban_rural=`Urbanite Ruralite`,
                moyenne_revenu_fiscaux_reg=`Moyenne Revenus Fiscaux Regionaux`,
                orientation_eco=`Orientation Economique`,
                evolu_pop=`Evolution Pop %`,
                nb_mineurs=`Nb Mineurs`,
                nb_entreprises_services=`Nb Entreprises Secteur Services`,
                nb_entreprises_comm=`Nb Entreprises Secteur Commerce`,
                nb_entreprises_constr=`Nb Entreprises Secteur Construction`,
                nb_entreprises_indus=`Nb Entreprises Secteur Industrie`)

rm(donnees_locales_2014)
communes_2014 <- communes_2014 %>% dplyr::rename(INSEE_COM = CODGEO)%>%
  dplyr::select(INSEE_COM, REG, REG2016, EPCI, NATURE_EPCI)
# on joint les données locales avec les donnees concernant les EPCI
dataloc_2014 <- data_loc_2014 %>% dplyr::right_join(communes_2014, by = "INSEE_COM")
rm(data_loc_2014, communes_2014)

# Constitution de la base de donn?es ----------------
  # Selection des taux votes par conseil municipal

city_all <- city_all %>% 
  dplyr::rename(INSEE_COM = `cog (code officiel géographique)`,
                dotation_fonct = `dotation globale de fonctionnement`,
                taxe_habit = `taxe d'habitation (y compris thlv) - taux`,
                taxe_habit_base = `taxe d'habitation (y compris thlv) - base nette imposée`,
                taxe_add_fonciere_non_bati = `taxe additionnelle à la taxe foncière sur les propriétés non bâties - taux`,
                impo_forfaitaire_entreprise_reseau = `impositions forfaitaires sur les entreprises de réseau - taux`,
                cotis_va_entreprise = `cotisation sur la valeur ajoutée des entreprises - taux`,
                cotis_fonciere_entreprise = `cotisation foncière des entreprises - taux`,
                taxe_prof = `taxe professionnelle (hors bases écrêtées) - taux`,
                encours_dette = `encours total de la dette au 31/12/n`,
                depense_equipement = `dépenses d'équipement`,
                caf = `capacité d'autofinancement = caf`,
                taxe_fonciere_non_bati = `taxe foncière sur les propriétés non bâties - taux`,
                taxe_fonciere_non_bati_base =`taxe foncière sur les propriétés non bâties - base nette imposée`,
                impot_locaux =`impôts locaux`,
                resultat_comptable = `resultat comptable = a - b = r`, 
                taxe_fonciere_bati = `taxe foncière sur les propriétés bâties - taux`,
                taxe_fonciere_bati_base = `taxe foncière sur les propriétés bâties - base nette imposée`,
                taxe_surface_commerciales = `taxe sur les surfaces commerciales - taux`,
                annee = année,
                pop = population)%>%
  dplyr::select(INSEE_COM, dotation_fonct,taxe_habit,taxe_add_fonciere_non_bati ,
                impo_forfaitaire_entreprise_reseau, cotis_va_entreprise,cotis_fonciere_entreprise,
                taxe_prof,encours_dette ,depense_equipement ,caf , taxe_fonciere_non_bati ,
                impot_locaux , resultat_comptable ,taxe_fonciere_bati,
                taxe_surface_commerciales, annee , pop ,
                taxe_fonciere_bati_base, taxe_habit_base,
                taxe_fonciere_non_bati_base)%>%
  dplyr::mutate(taxe_add_fonciere_non_bati=as.numeric(taxe_add_fonciere_non_bati),
                impo_forfaitaire_entreprise_reseau= as.numeric(impo_forfaitaire_entreprise_reseau),
                cotis_va_entreprise = as.numeric(cotis_va_entreprise),
                cotis_fonciere_entreprise=as.numeric(cotis_fonciere_entreprise),
                taxe_surface_commerciales = as.numeric(taxe_surface_commerciales))

# premier travail en coupe
city <- filter(city_all, annee == 2014)


# Base panel (pour suite du papier): quatre ann?es fusionn?es avec base collectivit?s
city_y <- city_all %>% dplyr::filter(annee %in% c(2000,2005,2010,2015))
data_epci_com <- readr::read_csv("data_epci_com.csv")
base_epci_com <- data_epci_com %>% dplyr::rename(INSEE_COM = insee)%>% dplyr::select(-X1,-starts_with("nb_com"))
base_epci_nbcom <- data_epci_com %>% dplyr::rename(INSEE_COM = insee)%>% dplyr::select(-X1,-starts_with("fisc"))
base_epci_com2 <- melt(base_epci_com, id.vars=c("INSEE_COM","siren_epci"))
base_epci_nbcom2 <- melt(base_epci_nbcom, id.vars=c("INSEE_COM","siren_epci"))
base_epci_com2 <- base_epci_com2%>% mutate(annee = stringr::str_sub(variable,5,length(variable)))%>%
  dplyr::select(-variable) %>% dplyr::rename(fisc = value)
base_epci_nbcom2 <- base_epci_nbcom2%>% mutate(annee = stringr::str_sub(variable,7,length(variable)))%>%
  dplyr::select(-variable) %>% dplyr::rename(nb_com = value)
base_epci_fiscalite <- base_epci_com2%>% 
  dplyr::left_join(base_epci_nbcom2, b =c("INSEE_COM","siren_epci","annee"))
head(base_epci_fiscalite)
table(base_epci_fiscalite$fisc)
# on remplace 4TX par FA et TPU par FPU
base_epci_fiscalite$fisc[base_epci_fiscalite$fisc == "4TX"] <- "FA"
base_epci_fiscalite$fisc[base_epci_fiscalite$fisc == "TPU"] <- "FPU"
base_epci_fiscalite$annee <- as.integer(base_epci_fiscalite$annee)
base_epci_fiscalite2 <- base_epci_fiscalite %>%
  dplyr::distinct(INSEE_COM,annee,.keep_all=TRUE)
city_y2 <- city_y %>% dplyr::left_join(base_epci_fiscalite2, by =c("INSEE_COM","annee"))
communes_reg <- communes %>% filter(CODE_REG %in% c(32))
# en fait on laisse le join suivant pour filtrer les communes qui ne sont pas dans 
  # le shapefile:
data_panel <- communes%>% left_join(city_y2, by ="INSEE_COM")
nrow(data_panel)
save(data_panel, file = "data_panel.RData")
load("data_panel.RData")
# On ne garde que Hauts de France
data_regions_panel <- data_panel %>% filter(CODE_REG %in% c(32))
nrow(data_regions_panel)
save(communes_reg, file = "communes_reg.RData")
save(data_regions_panel, file = "data_regions_panel.RData")
data_reg_panel <- as(data_regions_panel, "Spatial")
save(data_reg_panel, file = "data_reg_panel.RData")
rm(city_all,city_y,city_y2,data_panel, data_epci_com,base_epci_com,base_epci_com2,
   base_epci_nbcom,base_epci_nbcom2,base_epci_fiscalite,base_epci_fiscalite2)

# Base en coupe ----------------------------------------------
# On fusionne avec les donn?es locales 2014 pour la base en coupe
city2 <- city %>% dplyr::left_join(dataloc_2014, by = "INSEE_COM")
base_epci_fisc <- base_epci_fisca2014 %>% dplyr::rename(EPCI = siren_epci)
city3 <- city2 %>% dplyr::left_join(base_epci_fisc, by = "EPCI")
dataset <- communes %>% dplyr::left_join(city3, by ="INSEE_COM")
data <- as(dataset, "Spatial")
save(data, file = "spatialpolygondata.RData")
save(dataset, file = "data2014.RData")
rm(city,city2,city3,communes,dataloc_2014,city_all)
rm(base_epci_fisc,base_epci_fisca2014)
head(dataset)

# Estimation en coupe sur quelques r?gions -----------------
load("data2014.RData")
# Ile de France, Alsace Champagne Ardenne Lorraine, NPDC, Normandie
data_regions <- dataset %>% filter(CODE_REG %in% c(11,32,44,28))
save(data_regions, file = "data_regions.RData")
data_reg <- as(data_regions, "Spatial")
save(data_reg, file = "data_reg.RData")
rm(data,dataset)

# -------------------------------------------------------------------------
# CARTOGRAPHIE ------------------------------------------------------------
load("data_regions.RData")
ggplot(data = data_regions) + geom_sf(aes(fill = taxe_habit),colour = NA) +
  coord_sf(datum = NA)+
  ggtitle("R?gions sp?cifiques nord France TH 2014")+
  theme(text=element_text(size=12),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(face ="italic"),
        legend.position = "right")+  theme_bw()+ scale_fill_gradientn(colours = mycols)

ggplot(data = data_regions) + geom_sf(aes(fill = taxe_fonciere_non_bati),colour = NA) +
  coord_sf(datum = NA)+
  ggtitle("R?gions sp?cifiques nord France TPFNB 2014")+
  theme(text=element_text(size=12),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(face ="italic"),
        legend.position = "right")+ theme_bw()+ scale_fill_gradientn(colours = mycols)

ggplot(data = data_regions) + geom_sf(aes(fill = taxe_fonciere_bati),colour = NA) +
  coord_sf(datum = NA)+
  ggtitle("R?gions sp?cifiques nord France TPFB 2014")+
  theme(text=element_text(size=12),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(face ="italic"),
        legend.position = "right")+ theme_bw()+ scale_fill_gradientn(colours = mycols)


# ANALYSE EN COUPE 2014 -------------------------------------------
# Analyse statistique ---------------------------------------------

# -----------------------------------------------------------------

load("data_reg.RData") # specific regions
data <- data_reg
rm(data_reg)
class(data)
# The object has slots:
  # - the first slot is the data
  # - the second slot is the polygons 
  # - the third slot is the bounding box drawn around the boundaries
  # - the fourth slot contains the porjections proj4string
# on repasse ? un objet sp pour utiliser les packages de regression
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
  
# imputation par 10 voisins plus proche par knn
imput_var <- function(var){
  y <- var 
  x <- cbind(data@data$NOM_DEP,data@data$SUPERFICIE,data@data$POPULATION)
  df <- data.frame(cbind(y,x))
  imput <- knnImputation(df, k =10, meth = "median")
  return(imput$y)
}

data@data$taxe_habit <- imput_var(data@data$taxe_habit)
data@data$taxe_fonciere_bati <- imput_var(data@data$taxe_fonciere_bati)
data@data$taxe_fonciere_non_bati <- imput_var(data@data$taxe_fonciere_non_bati)

data@data$dotation_fonct <- imput_var(data@data$dotation_fonct)
data@data$encours_dette <- imput_var(data@data$encours_dette)

# Varable de base nette imposable
data@data$taxe_habit_base <- as.numeric(data@data$taxe_habit_base)
summary(data@data$taxe_habit_base)
data@data$taxe_fonciere_non_bati_base <- as.numeric(data@data$taxe_fonciere_non_bati_base)
summary(data@data$taxe_fonciere_non_bati_base)
data@data$taxe_fonciere_bati_base <- as.numeric(data@data$taxe_fonciere_bati_base)
summary(data@data$taxe_fonciere_bati_base)

# imput base nette imp
data@data$taxe_habit_base<- imput_var(data@data$taxe_habit_base)
data@data$taxe_fonciere_non_bati_base<- imput_var(data@data$taxe_fonciere_non_bati_base)
data@data$taxe_fonciere_bati_base<- imput_var(data@data$taxe_fonciere_bati_base)

#nb de services publics 
data@data$nb_educ_sante_actsoc_admin<- imput_var(data@data$nb_educ_sante_actsoc_admin)
data@data$score_urban<- imput_var(data@data$score_urban)
data@data$evolu_pop<- imput_var(data@data$evolu_pop)
data@data$moyenne_revenu_fiscaux<- imput_var(data@data$moyenne_revenu_fiscaux)
data@data$moyenne_revenu_fiscaux_dep<- imput_var(data@data$moyenne_revenu_fiscaux_dep)
data@data$NATURE_EPCI<- imput_var(data@data$NATURE_EPCI)

# On met en forme les variables explicatives
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
data@data$lnb_proprio[is.na(data@data$lnb_proprio)] <- 0 
data@data$ldotation_fonct <- log(data@data$dotation_fonct)
data@data$ldotation_fonct[is.infinite(data@data$ldotation_fonct)] <- 0
data@data$lpib_region <- log(data@data$pib_region)
data@data$lpib_region[is.infinite(data@data$lpib_region)] <- 0
data@data$lpib_region[is.na(data@data$lpib_region)] <- 0
data@data$lnb_educ_sante_actsoc_admin <- log(data@data$nb_educ_sante_actsoc_admin)
data@data$evolu_pop <- data@data$evolu_pop/100# en pourcentage
data@data$lmoy_revenu_fisc <- log(data@data$moyenne_revenu_fiscaux)
data@data$lmoy_revenu_fisc_dep <- log(data@data$moyenne_revenu_fiscaux_dep)
data@data$lsuperficie <- log(data@data$SUPERFICIE)
data@data$ltaxe_habit_base <- log(data@data$taxe_habit_base)
data@data$ltaxe_habit_base[is.infinite(data@data$ltaxe_habit_base)] <- 0
data@data$ltaxe_fonciere_bati_base <- log(data@data$taxe_fonciere_bati_base)
data@data$ltaxe_fonciere_bati_base[is.infinite(data@data$ltaxe_fonciere_bati_base)] <- 0
data@data$ltaxe_fonciere_non_bati_base <- log(data@data$taxe_fonciere_non_bati_base)
data@data$ltaxe_fonciere_non_bati_base[is.infinite(data@data$ltaxe_fonciere_non_bati_base)] <- 0

# Summary statistics variables explic ---------------------------------
summary(data@data$lencours_dette)
summary(data@data$ldotation_fonct)
summary(data@data$lnb_proprio)
summary(data@data$lnb_log)
summary(data@data$prop_log_vacant)
summary(data@data$lnb_menage)
summary(data@data$lnb_actif)
summary(data@data$lpib_region)
summary(data@data$lpop)
summary(data@data$score_urban)
summary(data@data$lnb_educ_sante_actsoc_admin)
summary(data@data$evolu_pop)
summary(data@data$lmoy_revenu_fisc)
summary(data@data$lsuperficie)
summary(data@data$lmoy_revenu_fisc_dep)
summary(data@data$ltaxe_habit_base)
summary(data@data$ltaxe_fonciere_bati_base)
summary(data@data$ltaxe_fonciere_non_bati_base)

# Variable fisca epci
table(data@data$fisc_epci2014)
sum(is.na(data@data$fisc_epci2014))
data@data$fisca <- data@data$fisc_epci2014 
table(data@data$fisca)
data@data$fisca[is.na(data@data$fisca)] <- "NFP" # NFP: non fiscalite propre
data@data$fisca <- factor(data@data$fisca, levels = c("NFP","FA","FPU"))
table(data@data$fisca)

# nombre de communes dans l'EPCI
summary(data@data$nb_com)
# NA signifie non EPCI, on remplace par 0 communes dans l'EPCI
data@data$nb_com[is.na(data@data$nb_com)] <- 0

# Nature de l'EPCI
table(data@data$NATURE_EPCI)
data@data$NATURE_EPCI <- factor(data@data$NATURE_EPCI)
data@data$CODE_REG <- factor(data@data$CODE_REG, levels = c(11,28,32,44))
table(data@data$CODE_REG)

# Les differents modeles -------------------------------------------
data@data$base <- data@data$ltaxe_habit_base
model_th <- taxe_habit~base + score_urban+lnb_log+
  lpop+lnb_menage+evolu_pop+lmoy_revenu_fisc+lmoy_revenu_fisc_dep+
  lencours_dette+ldotation_fonct+fisca + nb_com +lnb_educ_sante_actsoc_admin+CODE_REG
fit_lm_th <- lm(model_th, data = data@data,
               na.action ="na.exclude")
summary(fit_lm_th)
data@data$base <- data@data$ltaxe_fonciere_bati_base
model_tfb <- taxe_fonciere_bati~ base +score_urban+ lnb_log+
  lpop+lnb_proprio+evolu_pop+lmoy_revenu_fisc+lmoy_revenu_fisc_dep+
  lencours_dette + ldotation_fonct+fisca+ nb_com+lnb_educ_sante_actsoc_admin+CODE_REG
fit_lm_tfb <- lm(model_tfb, data = data@data,
                 na.action ="na.exclude")
summary(fit_lm_tfb)
data@data$base <- data@data$ltaxe_fonciere_non_bati_base
model_tfnb <- taxe_fonciere_non_bati~base +score_urban+lnb_log+
  lpop+lnb_proprio+evolu_pop+lmoy_revenu_fisc+lmoy_revenu_fisc_dep+
  lencours_dette + ldotation_fonct+fisca+ nb_com+lnb_educ_sante_actsoc_admin+CODE_REG
fit_lm_tfnb <- lm(model_tfnb, data = data@data,
                  na.action ="na.exclude")
summary(fit_lm_tfnb)
stargazer(fit_lm_th,fit_lm_tfnb,fit_lm_tfb, 
          dep.var.labels=c("Taxe d'Habitation","TPFNB","TPFB"),
          covariate.labels=c("Base nette imposable (log)",
                             "Niveau urbanisation com.",
            "Nb. logements (log)","Population (log)",
                             "Nb. menages (log)", "Nb. proprietaires (log)",
            "Evolution pop. (pourcent)",
            "Moy. revenus fiscaux (log)",
            "Moy. revenus fiscaux dep. (log)",
                             "Encours dette (log)", "Dotation fonctionnelle (log)",
                             "Type de fiscalite : FA","Type de fiscalite : FPU",
                             "Nb. communes EPCI",
            "Nb. services publics (log)"),
          omit = "CODE_REG")

# modeling spatial dependence --------------------------------------
  # weight matrix based on queen
# attention cela prend du temps de cr?er la matrice des voisins
#list_queen <- spdep::poly2nb(data, queen = T)
#save(list_queen, file = "liste_queen_reg.RData")
load("liste_queen_reg.RData")
# this is rapid:
W_q <- spdep::nb2listw(list_queen, style = "W", zero.policy = TRUE)
print(W_q, zero.policy=TRUE) # 12 communes sans voisins
#plot(W_q,coordinates(data))

  # weight matrix based on rook
# attention, pareil, cela prend du temps de cr?er cette matrice:
#list_rook <- spdep::poly2nb(data, queen = F)
#save(list_rook, file = "liste_rook_reg.RData")
load("liste_rook_reg.RData")
# this is rapid:
W_r <- spdep::nb2listw(list_rook, style = "W", zero.policy = T)
print(W_r, zero.policy=TRUE) # same 12 regions with no links
#plot(W_r,coordinates(data))


  # weight matrix based on distance
coords <- coordinates(data)
# long command:
nb <- dnearneigh(coords,0,5000,longlat = FALSE)
W_dist <- nb2listw(nb, style="W", zero.policy = T)
print(W_dist, zero.policy=TRUE)
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
#W <- W_dist
moran.lm_th<-lm.morantest(fit_lm_th,zero.policy = TRUE, W, alternative = "two.sided")
print(moran.lm_th)
moran.lm_tfb<-lm.morantest(fit_lm_tfb,zero.policy = TRUE, W, alternative = "two.sided")
print(moran.lm_tfb)
moran.lm_tfnb<-lm.morantest(fit_lm_tfnb,zero.policy = TRUE, W, alternative = "two.sided")
print(moran.lm_tfnb)

results_moran <- data.frame("TH" = c(moran.lm_th[["statistic"]],moran.lm_th[["p.value"]]),
                            "TPFNB" = c(moran.lm_tfnb[["statistic"]],moran.lm_tfnb[["p.value"]]),
                            "TPFB" =  c(moran.lm_tfb[["statistic"]],moran.lm_tfb[["p.value"]]))
row.names(results_moran) <- c("Statistic Moran", "P.value")
xtable(results_moran, caption = "Matrice contiguité Wq")
#xtable(results_moran, caption = "Matrice contiguité Wr")
#xtable(results_moran, caption = "Matrice contiguité Wdist")

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
  # La variable Ii est l'indice d'autocorr?lation spatiale local de la r?gion
    # l'indice sera positif pour les provinces de type Low-Low ou High-High mais n?gatif pour les
    # provinces de type Low-High ou High-Low.
  # la variable Pr(z>0) exprime la m?me chose en fournissant la p-value du test d'autocorr?lation
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


# Lagrange multiplier test --------------
# LM test for spatial dependence are included in lm.LMtests
# Taxe habitation
W <- W_q
# W <- W_r
# W <- W_dist
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

table_tests_th <- xtable(t(df))
colnames(table_tests_th) <- c("stat","p_value")
knitr::kable(table_tests_th)
  # il y a de l'autocorr?lation 
  


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

table_tests_tfb <- xtable(t(df))
colnames(table_tests_tfb) <- c("stat","p_value")
knitr::kable(table_tests_tfb)

# il y a de l'autocorr?lation 


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

table_tests_tfnb <- xtable(t(df))
colnames(table_tests_tfnb) <- c("stat","p_value")
knitr::kable(table_tests_tfnb)

df <- data.frame(table_tests_th,table_tests_tfnb,table_tests_tfb)
colnames(df) <- c("TH stat", "TH pval", "TFNB stat", "TFNB pval","TFB stat", 
                  "TFNB pval")
xtable(df, digits = 2, caption = "Résultats des tests de Lagrange")
# il y a de l'autocorr?lation 
# on penche vers un modele SARMA

# Spatial regressions -------------------------------

# Fit SAR model -----------------------------------------
# petite modif des modèles pour la variable base
model_th <- taxe_habit~ltaxe_habit_base + score_urban+lnb_log+
  lpop+lnb_menage+evolu_pop+lmoy_revenu_fisc+lmoy_revenu_fisc_dep+
  lencours_dette+ldotation_fonct+fisca + nb_com +lnb_educ_sante_actsoc_admin+CODE_REG

model_tfb <- taxe_fonciere_bati~ ltaxe_fonciere_bati_base +score_urban+ lnb_log+
  lpop+lnb_proprio+evolu_pop+lmoy_revenu_fisc+lmoy_revenu_fisc_dep+
  lencours_dette + ldotation_fonct+fisca+ nb_com+lnb_educ_sante_actsoc_admin+CODE_REG

model_tfnb <- taxe_fonciere_non_bati~ltaxe_fonciere_non_bati_base +score_urban+lnb_log+
  lpop+lnb_proprio+evolu_pop+lmoy_revenu_fisc+lmoy_revenu_fisc_dep+
  lencours_dette + ldotation_fonct+fisca+ nb_com+lnb_educ_sante_actsoc_admin+CODE_REG

# We fit a SAR model with 2SLS:

W <- W_q
fit_sar_th <- stsls(model_th, data = data@data, W,
                       zero.policy = T)
summary(fit_sar_th)
fit_sar_tfb <- stsls(model_tfb, data = data@data, W,
                    zero.policy = T)
summary(fit_sar_tfb)
fit_sar_tfnb <- stsls(model_tfnb, data = data@data, W,
                    zero.policy = T)
summary(fit_sar_tfnb)

# Mise en forme du tableau de résultats
covariate_labels <- c("Rho","Constant","Base nette impos. (log)",
                      "Niveau urbanisation ",
                      "Nb. logements (log)","Population (log)",
                      "Nb.men. (l) /Nb. prop. (l)",
                      "Evolution pop. (pourcent)",
                      "Moy. revenus fiscaux (log)",
                      "Moy. revenus fiscaux dep. (log)",
                      "Encours dette (log)", "Dotation fonction. (log)",
                      "Type de fisca.: FA","Type de fisca. : FPU",
                      "Nb. communes EPCI",
                      "Nb. services publ. (log)",
                      "Observations",
                      "SSE",
                      "df",
                      "W")
make_table_sar <- function(name_model, covariate_labels){
  
  tab <- xtable(name_model, digits = 2)
  df <- data.frame(tab)
  names <- row.names(df)
  df <- df %>% dplyr::rename(tvalue = Pr...t..) %>% dplyr::select(-t.value)
  df <- df %>% dplyr::mutate(se = round(Std..Error*1000)/1000)%>%
    dplyr::mutate(se = paste0("$(",se,")$"))
  df <- df %>% dplyr::mutate(signif = case_when(
    tvalue < 0.01 ~"***",
    tvalue < 0.05 ~"**",
    tvalue < 0.1~"*",
    TRUE ~''))
  df <- df%>% dplyr::select(-tvalue,- Std..Error)
  df <- df %>% dplyr::mutate(coef = round(Estimate*1000)/1000)%>%
    dplyr::mutate(coef = paste0("$",coef, "^{",signif,"}$"))
  df <- df %>% dplyr::select(-Estimate, -signif)
  row.names(df) <- names
  df <- df[-((nrow(df)-2):nrow(df)),]
  head(df)
  
  df <- df[,2:1]
  colnames(df) <- c("Estim.", "s.d.")
  df[nrow(df)+1,1] <- length(name_model[["residuals"]])
  df[nrow(df)+1,1] <- round(name_model[["sse"]]*100)/100
  df[nrow(df)+1,1] <- name_model[["df"]]
  df[nrow(df)+1,1] <- "$W_{queen}$"
  row.names(df) <- covariate_labels
  return(df)
}
df_th <- make_table_sar(fit_sar_th,covariate_labels) 
df_tfnb <- make_table_sar(fit_sar_tfnb,covariate_labels) 
df_tfb <- make_table_sar(fit_sar_tfb,covariate_labels) 

results <- cbind("TH" = df_th, "TPFNB" = df_tfnb, "TFB" = df_tfb)
hlines <- c(-1,-1, 0,1, nrow(df)-4,nrow(df),nrow(df))
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(df))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize \\textit{Note:}   $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}\n", sep = ""))
print(xtable(as.is(results), caption = "Modèle SAR", digits = 3, auto = TRUE,
             align= c("l","c","c","c","c","c","c")), add.to.row = comment, 
      hline.after = hlines,
      sanitize.text.function = identity,booktabs = T,size="\\fontsize{9pt}{10pt}\\selectfont")


# Marginal effects ---------------------------------
# a retravailler, trop long
imp_th <- impacts(fit_sar_th, listw=W )
imp_tfnb <- impacts(fit_sar_tfnb, listw=W )
imp_tfb <- impacts(fit_sar_tfb, listw=W )

# SEM models ---------------------------------------

# approach with feasible generalized least squares
# because n is large
W <- W_q
sem_th <- GMerrorsar(model_th, data=data@data, W,zero.policy = T)
summary(sem_th)
sem_tfb <- GMerrorsar(model_tfb, data=data@data, W,zero.policy = T)
summary(sem_tfb)
sem_tfnb <- GMerrorsar(model_tfnb, data=data@data, W,zero.policy = T)
summary(sem_tfnb)

# Mise en forme des résultats
covariate_labels <- c("Constant","Base nette impos. (log)",
                      "Niveau urbanisation ",
                      "Nb. logements (log)","Population (log)",
                      "Nb.men. (l) /Nb. prop. (l)",
                      "Evolution pop. (pourcent)",
                      "Moy. revenus fiscaux (log)",
                      "Moy. revenus fiscaux dep. (log)",
                      "Encours dette (log)", "Dotation fonction. (log)",
                      "Type de fisca.: FA","Type de fisca. : FPU",
                      "Nb. communes EPCI",
                      "Nb. services publ. (log)",
                      "Observations",
                      "SSE",
                      "df",
                      "W",
                      "lambda", "lambda s.d.")
make_table_sem <- function(name_model, covariate_labels){
  
  tab <- xtable(name_model, digits = 2)
  df <- data.frame(tab)
  names <- row.names(df)
  df <- df %>% dplyr::rename(zvalue = Pr...z..) %>% dplyr::select(-z.value)
  df <- df %>% dplyr::mutate(se = round(Std..Error*1000)/1000)%>%
    dplyr::mutate(se = paste0("$(",se,")$"))
  df <- df %>% dplyr::mutate(signif = case_when(
    zvalue < 0.01 ~"***",
    zvalue < 0.05 ~"**",
    zvalue < 0.1~"*",
    TRUE ~''))
  df <- df%>% dplyr::select(-zvalue,- Std..Error)
  df <- df %>% dplyr::mutate(coef = round(Estimate*1000)/1000)%>%
    dplyr::mutate(coef = paste0("$",coef, "^{",signif,"}$"))
  df <- df %>% dplyr::select(-Estimate, -signif)
  row.names(df) <- names
  df <- df[-((nrow(df)-2):nrow(df)),]
  head(df)
  
  df <- df[,2:1]
  colnames(df) <- c("Estim.", "s.d.")
  df[nrow(df)+1,1] <- length(name_model[["residuals"]])
  df[nrow(df)+1,1] <- round(name_model[["SSE"]]*100)/100
  df[nrow(df)+1,1] <- length(name_model[["residuals"]]) -name_model[["parameters"]]
  df[nrow(df)+1,1] <- "$W_{queen}$"
  df[nrow(df)+1,1] <- round(name_model[["lambda"]]*100)/100
  df[nrow(df)+1,1] <- round(name_model[["lambda.se"]]*1000)/1000
  row.names(df) <- covariate_labels
  return(df)
}
df_th <- make_table_sem(sem_th,covariate_labels) 
df_tfnb <- make_table_sem(sem_tfnb,covariate_labels) 
df_tfb <- make_table_sem(sem_tfb,covariate_labels) 

results <- cbind("TH" = df_th, "TPFNB" = df_tfnb, "TFB" = df_tfb)
hlines <- c(-1,-1, 0, nrow(df)-5,nrow(df),nrow(df))
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(df))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize \\textit{Note:}   $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}\n", sep = ""))
print(xtable(as.is(results), caption = "Modèle SEM", digits = 3, auto = TRUE,
             align= c("l","c","c","c","c","c","c")), add.to.row = comment, 
      hline.after = hlines,
      sanitize.text.function = identity,booktabs = T,size="\\fontsize{9pt}{10pt}\\selectfont")


# SARAR model ------------------------------------------
W <- W_q
W2 <- W_r
sarar_th <- gstsls(model_th, data=data@data, listw = W,listw2 = W2,
                   zero.policy = T, robust = T)
summary(sarar_th)

sarar_tfb <- gstsls(model_tfb, data=data@data, listw = W,listw2 = W2,
                    zero.policy = T, robust = T)
summary(sarar_tfb)

sarar_tfnb <- gstsls(model_tfnb, data=data@data, listw = W,listw2 = W2,
                     zero.policy = T, robust = T)
summary(sarar_tfnb)


# Mise en forme des résultats
covariate_labels <- c("Rho", "Constant","Base nette impos. (log)",
                      "Niveau urbanisation ",
                      "Nb. logements (log)","Population (log)",
                      "Nb.men. (l) /Nb. prop. (l)",
                      "Evolution pop. (pourcent)",
                      "Moy. revenus fiscaux (log)",
                      "Moy. revenus fiscaux dep. (log)",
                      "Encours dette (log)", "Dotation fonction. (log)",
                      "Type de fisca.: FA","Type de fisca. : FPU",
                      "Nb. communes EPCI",
                      "Nb. services publ. (log)",
                      "Observations",
                      "SSE",
                      "df",
                      "W",
                      "lambda")
make_table_sarar <- function(name_model, covariate_labels){
  tab <- xtable(name_model, digits = 2)
  df <- data.frame(tab)
  names <- row.names(df)
  df <- df %>% dplyr::rename(zvalue = Pr...z..) %>% dplyr::select(-z.value)
  df <- df %>% dplyr::mutate(se = round(Std..Error*1000)/1000)%>%
    dplyr::mutate(se = paste0("$(",se,")$"))
  df <- df %>% dplyr::mutate(signif = case_when(
    zvalue < 0.01 ~"***",
    zvalue < 0.05 ~"**",
    zvalue < 0.1~"*",
    TRUE ~''))
  df <- df%>% dplyr::select(-zvalue,- Std..Error)
  df <- df %>% dplyr::mutate(coef = round(Estimate*1000)/1000)%>%
    dplyr::mutate(coef = paste0("$",coef, "^{",signif,"}$"))
  df <- df %>% dplyr::select(-Estimate, -signif)
  row.names(df) <- names
  df <- df[-((nrow(df)-2):nrow(df)),]
  head(df)
  
  df <- df[,2:1]
  colnames(df) <- c("Estim.", "s.d.")
  df[nrow(df)+1,1] <- length(name_model[["residuals"]])
  df[nrow(df)+1,1] <- round(name_model[["SSE"]]*100)/100
  df[nrow(df)+1,1] <- length(name_model[["residuals"]]) -name_model[["parameters"]]
  df[nrow(df)+1,1] <- "$W_{queen}$"
  df[nrow(df)+1,1] <- round(name_model[["lambda"]]*100)/100
  row.names(df) <- covariate_labels
  return(df)
}
df_th <- make_table_sarar(sarar_th,covariate_labels) 
df_tfnb <- make_table_sarar(sarar_tfnb,covariate_labels) 
df_tfb <- make_table_sarar(sarar_tfb,covariate_labels) 

results <- cbind("TH" = df_th, "TPFNB" = df_tfnb, "TFB" = df_tfb)
hlines <- c(-1,-1, 0, 1, nrow(df)-4,nrow(df),nrow(df))
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(df))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize \\textit{Note:}   $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}\n", sep = ""))
print(xtable(as.is(results), caption = "Modèle SARAR", digits = 3, auto = TRUE,
             align= c("l","c","c","c","c","c","c")), add.to.row = comment, 
      hline.after = hlines,
      sanitize.text.function = identity,booktabs = T,size="\\fontsize{9pt}{10pt}\\selectfont")

  # Reste le calcul des effets directs et indirects
  # Vérifier que c'est la bonne variable base pour les différents modeles
  # Intégrer une matrice de poids avec vari : meme EPCI, même département

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# PANEL ANALYSIS


# Creation d'un objet panel
# objet pour d?finir la matrice de voisins: 
load("data_regions_panel.RData")#sf object
load("communes_reg.RData") #sf object

# objet pour d?finir les donn?es de panel
load("data_reg_panel.RData")#sp object
tab1 <- data_regions_panel

# Pooled OLS ---------------------------------------------
# variables expliqu?es

summary(tab1$taxe_habit)
summary(tab1$taxe_fonciere_bati)
summary(tab1$taxe_fonciere_non_bati)

# variables explicatives
tab1$ldotation_fonct <- log(tab1$dotation_fonct)
tab1$ldotation_fonct[is.infinite(tab1$ldotation_fonct)] <- 0
summary(tab1$ldotation_fonct)
tab1$lpop <- log(tab1$pop)
summary(tab1$lpop)
tab1$lencours_dette <- log(tab1$encours_dette)
tab1$lencours_dette[is.infinite(tab1$lencours_dette)] <- 0
tab1$lencours_dette[is.na(tab1$lencours_dette)] <- 0
summary(tab1$lencours_dette)
# creation du facteur d'appartenance fiscale de la com ds EPCI
table(tab1$fisc)
tab1$fisca <- tab1$fisc
tab1$fisca[is.na(tab1$fisca)] <-  "NFP" # non fiscalite propre
tab1$fisca <- factor(tab1$fisca, levels = c("NFP","FA","FPU"))
table(tab1$fisca)
summary(tab1$nb_com)
tab1$nb_com[is.na(tab1$nb_com)] <- 0 # quand NA, n'appartient pas ? EPCI
# on remplace par 0 communes (comme pour analyse en coupe)

# fonction d'imputation pour valeurs manquantes
imput_var <- function(var){
  y <- var 
  x <- cbind(tab1$NOM_DEP,tab1$SUPERFICIE,tab1$POPULATION)
  df <- data.frame(cbind(y,x))
  imput <- knnImputation(df, k =10, meth = "median")
  return(imput$y)
}
# variables explicatives
tab1$taxe_habit_base <- as.numeric(tab1$taxe_habit_base)
#tab1$taxe_habit_base <- imput_var(tab1$taxe_habit_base)
summary(tab1$taxe_habit_base)
  # trop de valeurs manUquantes on ne peut pas l'utiliser
summary(tab1$SUPERFICIE)
tab1$lsuperficie <- log(tab1$SUPERFICIE)
summary(tab1$lsuperficie)
tab1$dep <- factor(tab1$CODE_DEPT)
table(tab1$dep)
tab1$ldepense_equip <- log(tab1$depense_equipement)
tab1$ldepense_equip[is.infinite(tab1$ldepense_equip)] <- 0
tab1$ldepense_equip <- imput_var(tab1$ldepense_equip) # pour les valeurs manq
summary(tab1$ldepense_equip)
tab1$limpot_locaux <- log(tab1$impot_locaux)
tab1$limpot_locaux[is.infinite(tab1$limpot_locaux)] <- 0
tab1$limpot_locaux <- imput_var(tab1$limpot_locaux) # pour les valeurs manq
summary(tab1$limpot_locaux)

# Les modèles de base
panel_th <- taxe_habit~  lpop+lsuperficie+lencours_dette+ldotation_fonct+
  ldepense_equip + limpot_locaux+ fisca + nb_com +dep

panel_tfb <- taxe_fonciere_bati~  lpop+lsuperficie+lencours_dette+ldotation_fonct+
  ldepense_equip + limpot_locaux+ fisca + nb_com +dep

panel_tfnb <- taxe_fonciere_non_bati~ lpop+lsuperficie+lencours_dette+ldotation_fonct+
  ldepense_equip + limpot_locaux+ fisca + nb_com +dep

# On garde observations pr?sentes tous les ans pour un balanced panel:
# transformation en pdata.frame pour l'estimation

st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}
tab_temp <- st_drop_geometry(tab1)
tab_temp <- tab_temp[,-c(1,2)]
tab_temp[,2] <- tab_temp$annee
tab_temp <- tab_temp%>%dplyr::select(-annee)
colnames(tab_temp)[2] <- "annee"
head(tab_temp[,1:2])
tab_temp <- tab_temp %>% dplyr::select(INSEE_COM, annee, taxe_habit, taxe_fonciere_non_bati,taxe_fonciere_bati,
                               lpop, lsuperficie, lencours_dette, ldotation_fonct,
                               ldepense_equip, limpot_locaux, fisca, nb_com, dep)%>%
  dplyr::rename(id = INSEE_COM)
tab_temp <- tab_temp %>% dplyr::mutate(annee = (annee == 2000)*1+(annee==2005)*2+
                                         (annee == 2010)*3+(annee == 2015)*4)
tab_temp$id = as.character(tab_temp$id)
length(unique(tab_temp$id))
tab_temp$id <- plyr::mapvalues(tab_temp$id, from =unique(tab_temp$id), to = 1:3818)
tab_temp$annee <- as.numeric(tab_temp$annee)
head(tab_temp[,1:2])
tab <- plm::pdata.frame(tab_temp,index = c("id","annee"))
tab2 <- plm::make.pbalanced(tab,balance.type = "shared.individuals")
is.pbalanced(tab2)
tab2 <- data.frame(tab2)
table(tab2$annee)
rm(tab,tab_temp)
tab2$id <- factor(tab2$id)
  # tab2 objet pour les estimations
NT <- length(tab2$id)
N<-length(unique(tab2$id))
formula <- panel_th
mt <- terms(formula, data = tab2)
mf <- lm(formula, data  = tab2, na.action = na.fail, method = "model.frame")

y <- model.extract(mf, "response")
x <- model.matrix(mt, mf)
T_ <- max(tapply(x[,1],tab2$id,length))
T_

tab3 <- plm::pdata.frame(tab2,index = c("id","annee"))

# attention il faut enlever les obs du shapefile qui ne sont pas dans
# le balanced panel (pour la cr?ation de la matrice de voisins)
#communes_reg2 <- dplyr::filter(communes_reg, INSEE_COM %in% tab2$INSEE_COM)
#communes_regions <- as(communes_reg2, "Spatial")# creation objet sp
#rm(communes_reg2)
#rm(communes_reg)

# Definition des voisins : on prend les voisins ds le shapefile et on 
# suppose que les voisins sont constants sur la p?riode d'estimation
#list_queen <- spdep::poly2nb(communes_regions, queen = T)
#save(list_queen, file = "liste_queen_regions.RData")
load("liste_queen_regions.RData")
# this is rapid:
W_q <- spdep::nb2listw(list_queen, style = "W", zero.policy = TRUE)
print(W_q, zero.policy=TRUE) # 12 communes sans voisins

# Critère de Rook
#list_rook <- spdep::poly2nb(communes_regions, queen = F)
#save(list_rook, file = "liste_rook_regions.RData")
load("liste_rook_regions.RData")
# this is rapid:
W_r <- spdep::nb2listw(list_rook, style = "W", zero.policy = TRUE)
print(W_r, zero.policy=TRUE) # 12 communes sans voisins

W <- W_q


# Estimation diff?rents modeles ----------------------------------------------

# pooling
fit_pols_th <- plm(panel_th, data = tab3,model = "pooling")
summary(fit_pols_th)
# modele ? effets fixes
fit_fe_th <- plm(panel_th, data = tab3,model = "within",
                 effect = "individual")
summary(fit_fe_th)
# random effect
fit_re_th <- plm(panel_th, data = tab3,model = "random", 
                 effect = "individual")
summary(fit_re_th)


# pooling
fit_pols_tfb <- plm(panel_tfb, data = tab3,model = "pooling")
summary(fit_pols_tfb)
# modele ? effets fixes
fit_fe_tfb <- plm(panel_tfb, data = tab3,model = "within",
                 effect = "individual")
summary(fit_fe_tfb)
# random effect
fit_re_tfb <- plm(panel_tfb, data = tab3,model = "random", 
                  effect = "individual")
summary(fit_re_tfb)


# pooling
fit_pols_tfnb <- plm(panel_tfnb, data = tab3,model = "pooling")
summary(fit_pols_tfnb)
# modele ? effets fixes
fit_fe_tfnb <- plm(panel_tfnb, data = tab3,
                   model = "within",effect = "individual")
summary(fit_fe_tfnb)
# random effect
fit_re_tfnb <- plm(panel_tfnb, data = tab3,
                   model = "random", effect = "individual")
summary(fit_re_tfnb)

stargazer(fit_fe_th,fit_re_th,fit_fe_tfnb,fit_re_tfnb,fit_fe_tfb,fit_re_tfb, 
          dep.var.labels=c("TH","TPFNB","TPFB"),
          covariate.labels=c("Population (log)",
                             "Superficie (log)",
                             "Encours dette (log)", "Dotation fonctionnelle (log)",
                             "Dépense équipement (log)",
                             "Impots locaux (log)",
                             "Type de fiscalité : FA","Type de fiscalité : FPU",
                             "Nb. communes EPCI"),
          omit = "dep",
          column.sep.width = "0.3pt",
          font.size="small",
          df = FALSE)

# Test d'Hausman sans effet spatial (plm)--------------------
hausman_panel <- phtest(panel_th, data = tab3)
print(hausman_panel)

# Test d'Hausman robuste à l'autocorrélation spatiale (splm)
hausman_sar <- sphtest(x = panel_th,x2 = sar_random_spatial_th ,data=tab3,
                             listw =W, spatial.model = "lag", method="ML")
print(hausman_sar)

# Lagrange tests -----------------------------------------------
LM_th <- slmtest(fit_re_th, data=tab3, listw = W, test="lml")
print(LM_th)

# Modele a effets al?atoires spatial 
# random effect, no error term SAR
W2 <- W_r
sar_random_spatial_th <- spgm(formula = panel_th, data = tab3, 
     index = NULL, listw = W,model = "random", 
     lag = TRUE, spatial.error = FALSE, verbose = TRUE)
summary(sar_random_spatial_th)

# random effect, only error term SEM
sem_random_spatial_th <- spgm(formula = panel_th, data = tab3, 
     index = NULL, listw = W,model = "random", lag = F, spatial.error = TRUE)

# random effect, lag and error term SARAR
# random effect, no error term SAR
sarar_random_spatial_th <- spml(formula = model_th, data = tab3, 
    index = NULL, listw = W,model = "random", lag = T, spatial.error = "b")
sarar_random_spatial_th <- spml(formula = model_th, data = tab3, 
   index = NULL, listw = W,model = "random", lag = T, spatial.error = "kkp")


# Modele ? effets fixes spatial
# fixed effect,  error term SAR
sar_fe_spatial_th <- spml(formula = model_th, data = tab3, 
    index = NULL, listw = W, lag = T, spatial.error = "none",
    model = "within",effect = "individual", method = "eigen")

# Les tests
tests1 <- bsktests()
