# Paramètres par pays et modèles d'exemple par type de projet.
#
# ATTENTION : les valeurs ci-dessous sont des ordres de grandeur indicatifs pour
# démarrer. Remplacez-les par vos données de projet et des sources vérifiées
# (Banque mondiale pour le PIB et les taux de change, études de coûts locales,
# Global Burden of Disease pour les DALY).

pays <- list(
  KE = list(nom = "Kenya", monnaie = "KES", taux_change = 129, pib_hab_usd = 2000),
  UG = list(nom = "Ouganda", monnaie = "UGX", taux_change = 3700, pib_hab_usd = 1100),
  BF = list(nom = "Burkina Faso", monnaie = "XOF", taux_change = 600, pib_hab_usd = 900)
)

types_projet <- c(
  "Agents de santé communautaires" = "asc",
  "Santé maternelle et infantile" = "smi",
  "Prévention / sensibilisation" = "prevention"
)

# Coûts d'exemple en USD (convertis en monnaie locale au chargement).
couts_exemple_usd <- list(
  asc = data.frame(
    libelle = c("Personnel (motivation des ASC, superviseurs)", "Formation", "Équipement (kits, vélos, téléphones)",
                "Médicaments et intrants", "Fonctionnement et supervision", "Suivi-évaluation"),
    investissement = c(0, 15000, 20000, 0, 0, 5000),
    recurrent = c(40000, 5000, 2000, 15000, 8000, 4000)
  ),
  smi = data.frame(
    libelle = c("Personnel (sages-femmes, relais)", "Formation", "Équipement (kits d'accouchement, chaîne du froid)",
                "Vaccins, suppléments et intrants", "Fonctionnement et transport", "Suivi-évaluation"),
    investissement = c(0, 12000, 25000, 0, 0, 5000),
    recurrent = c(35000, 4000, 3000, 18000, 7000, 4000)
  ),
  prevention = data.frame(
    libelle = c("Personnel (animateurs, relais)", "Formation", "Matériel (moustiquaires, kits WASH)",
                "Supports de communication", "Fonctionnement et transport", "Suivi-évaluation"),
    investissement = c(0, 8000, 30000, 6000, 0, 4000),
    recurrent = c(25000, 3000, 10000, 3000, 6000, 3000)
  )
)

resultat_exemple <- function(libelle, unites, eco_systeme, eco_menages, productivite,
                             valeur_sociale, daly, duree_effet = 1, drop_off = 0,
                             croissance = 0.05, deadweight = 0.1, attribution = 0.2,
                             displacement = 0) {
  data.frame(
    libelle = libelle, unites = unites, croissance = croissance,
    eco_systeme = eco_systeme, eco_menages = eco_menages, productivite = productivite,
    valeur_sociale = valeur_sociale, daly = daly, deadweight = deadweight,
    attribution = attribution, displacement = displacement,
    duree_effet = duree_effet, drop_off = drop_off
  )
}

# Résultats d'exemple : valeurs monétaires par unité en USD, DALY évitées par unité.
resultats_exemple_usd <- list(
  asc = rbind(
    resultat_exemple("Cas de paludisme simple traités par les ASC", 3000, 8, 3, 4, 2, 0.02),
    resultat_exemple("Cas de diarrhée de l'enfant traités (SRO + zinc)", 1500, 4, 2, 2, 1, 0.01),
    resultat_exemple("Pneumonies de l'enfant traitées (amoxicilline)", 800, 12, 4, 3, 2, 0.05),
    resultat_exemple("Références réussies vers le centre de santé", 400, 5, 2, 1, 5, 0.03)
  ),
  smi = rbind(
    resultat_exemple("Accouchements assistés supplémentaires", 300, 20, 10, 0, 30, 0.30),
    resultat_exemple("Enfants complètement vaccinés", 1000, 6, 3, 2, 5, 0.05),
    resultat_exemple("Femmes ayant 4 consultations prénatales ou plus", 500, 8, 4, 0, 10, 0.04),
    resultat_exemple("Enfants malnutris dépistés et pris en charge", 200, 30, 10, 5, 20, 0.20)
  ),
  prevention = rbind(
    resultat_exemple("Moustiquaires imprégnées distribuées et utilisées", 5000, 3, 1, 2, 1, 0.01,
                     duree_effet = 3, drop_off = 0.3),
    resultat_exemple("Ménages adoptant de bonnes pratiques WASH", 1000, 2, 2, 1, 5, 0.005,
                     duree_effet = 2, drop_off = 0.5),
    resultat_exemple("Nouvelles utilisatrices de planification familiale", 400, 15, 10, 0, 20, 0.03),
    resultat_exemple("Personnes dépistées VIH/TB et orientées", 600, 10, 3, 2, 5, 0.02)
  )
)

colonnes_monetaires <- c("eco_systeme", "eco_menages", "productivite", "valeur_sociale")

couts_exemple <- function(type, taux_change) {
  c <- couts_exemple_usd[[type]]
  c$investissement <- c$investissement * taux_change
  c$recurrent <- c$recurrent * taux_change
  c
}

resultats_exemple <- function(type, taux_change) {
  r <- resultats_exemple_usd[[type]]
  r[colonnes_monetaires] <- r[colonnes_monetaires] * taux_change
  r
}
