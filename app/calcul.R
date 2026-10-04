# Moteur de calcul du ROI des projets de santé communautaire.
#
# Trois méthodes, calculées à partir des mêmes données :
#   - ROI financier  : (bénéfices monétaires - coûts) / coûts
#   - SROI           : (bénéfices monétaires + valeur sociale) / coûts
#   - Coût-efficacité: coût par DALY évitée, comparé au PIB par habitant
#
# Toutes les valeurs monétaires sont en monnaie locale. L'année 1 n'est pas
# actualisée ; l'année t est actualisée par 1 / (1 + taux)^(t - 1).

facteur_actualisation <- function(taux, n) {
  1 / (1 + taux)^(seq_len(n) - 1)
}

# Part du résultat réellement imputable au projet.
part_impact <- function(deadweight, attribution, displacement) {
  borne <- function(x) pmin(1, pmax(0, x))
  (1 - borne(deadweight)) * (1 - borne(attribution)) * (1 - borne(displacement))
}

# Unités produites chaque année du projet : unités de l'année 1, puis croissance annuelle.
unites_par_annee <- function(unites_an1, croissance, duree_projet) {
  unites_an1 * (1 + croissance)^(seq_len(duree_projet) - 1)
}

# Répartit dans le temps l'effet des unités produites : une unité produite
# l'année t continue de produire de l'effet pendant `duree_effet` années,
# avec une baisse de `drop_off` par an après la première année.
unites_effectives <- function(unites, duree_effet, drop_off, longueur) {
  duree_effet <- max(1, round(duree_effet))
  garde <- 1 - min(1, max(0, drop_off))
  out <- numeric(longueur)
  for (t in seq_along(unites)) {
    for (k in 0:(duree_effet - 1)) {
      if (t + k <= longueur) out[t + k] <- out[t + k] + unites[t] * garde^k
    }
  }
  out
}

# Durée d'analyse : années du projet + traîne des effets durables.
duree_analyse <- function(duree_projet, resultats) {
  traine <- if (nrow(resultats) > 0) max(1, round(resultats$duree_effet)) else 1
  duree_projet + traine - 1
}

ratio <- function(a, b) if (is.finite(b) && b > 0) a / b else NA_real_

# Flux annuels (non actualisés) de coûts, bénéfices et DALY évitées.
#
# parametres : list(annee_debut, duree, taux_actualisation, taux_change, pib_hab_usd)
# couts      : data.frame(libelle, investissement, recurrent)
#              investissement = coût unique en année 1, recurrent = coût chaque année
# resultats  : data.frame(libelle, unites, croissance, eco_systeme, eco_menages,
#              productivite, valeur_sociale, daly, deadweight, attribution,
#              displacement, duree_effet, drop_off)
flux_annuels <- function(parametres, couts, resultats) {
  duree <- parametres$duree
  n <- duree_analyse(duree, resultats)
  zero <- numeric(n)

  cout_annuel <- zero
  if (nrow(couts) > 0) {
    cout_annuel[seq_len(duree)] <- sum(couts$recurrent, na.rm = TRUE)
    cout_annuel[1] <- cout_annuel[1] + sum(couts$investissement, na.rm = TRUE)
  }

  eco_systeme <- eco_menages <- productivite <- valeur_sociale <- daly <- zero
  for (i in seq_len(nrow(resultats))) {
    r <- resultats[i, ]
    u <- unites_par_annee(r$unites, r$croissance, duree)
    eff <- unites_effectives(u, r$duree_effet, r$drop_off, n) *
      part_impact(r$deadweight, r$attribution, r$displacement)
    eco_systeme <- eco_systeme + eff * r$eco_systeme
    eco_menages <- eco_menages + eff * r$eco_menages
    productivite <- productivite + eff * r$productivite
    valeur_sociale <- valeur_sociale + eff * r$valeur_sociale
    daly <- daly + eff * r$daly
  }

  data.frame(
    annee = parametres$annee_debut + seq_len(n) - 1,
    couts = cout_annuel,
    eco_systeme = eco_systeme,
    eco_menages = eco_menages,
    productivite = productivite,
    benefices = eco_systeme + eco_menages + productivite,
    valeur_sociale = valeur_sociale,
    daly = daly
  )
}

calcul_roi <- function(parametres, couts, resultats) {
  flux <- flux_annuels(parametres, couts, resultats)
  fa <- facteur_actualisation(parametres$taux_actualisation, nrow(flux))
  va <- function(x) sum(x * fa)

  va_couts <- va(flux$couts)
  va_benefices <- va(flux$benefices)
  va_eco_soins <- va(flux$eco_systeme + flux$eco_menages)
  va_social <- va(flux$valeur_sociale)
  daly <- va(flux$daly)

  flux$couts_cumules <- cumsum(flux$couts * fa)
  flux$benefices_cumules <- cumsum(flux$benefices * fa)
  flux$valeur_totale_cumulee <- cumsum((flux$benefices + flux$valeur_sociale) * fa)

  rentable <- which(flux$benefices_cumules >= flux$couts_cumules & flux$couts_cumules > 0)
  annee_retour <- if (length(rentable) > 0) flux$annee[rentable[1]] else NA

  cout_par_daly <- ratio(va_couts, daly)
  cout_par_daly_usd <- ratio(cout_par_daly, parametres$taux_change)

  list(
    flux = flux,
    financier = list(
      va_couts = va_couts,
      va_benefices = va_benefices,
      van = va_benefices - va_couts,
      roi = ratio(va_benefices - va_couts, va_couts),
      ratio_bc = ratio(va_benefices, va_couts),
      annee_retour = annee_retour
    ),
    sroi = list(
      va_couts = va_couts,
      va_financiere = va_benefices,
      va_sociale = va_social,
      va_totale = va_benefices + va_social,
      ratio = ratio(va_benefices + va_social, va_couts)
    ),
    cea = list(
      va_couts = va_couts,
      va_couts_nets = va_couts - va_eco_soins,
      daly = daly,
      cout_par_daly = cout_par_daly,
      cout_net_par_daly = ratio(va_couts - va_eco_soins, daly),
      cout_par_daly_usd = cout_par_daly_usd,
      ratio_pib = ratio(cout_par_daly_usd, parametres$pib_hab_usd)
    )
  )
}

# Interprétation du coût par DALY par rapport au PIB/habitant.
# Repères OMS-CHOICE (< 1 PIB/hab : très coût-efficace ; < 3 : coût-efficace),
# aujourd'hui jugés trop généreux : on signale aussi le seuil plus prudent de 0,5.
interpretation_cea <- function(ratio_pib) {
  if (is.na(ratio_pib)) return("Pas de DALY évitée renseignée.")
  if (ratio_pib < 0.5) return("Très coût-efficace (moins de 0,5 × PIB/habitant par DALY évitée).")
  if (ratio_pib < 1) return("Coût-efficace (moins de 1 × PIB/habitant par DALY évitée).")
  if (ratio_pib < 3) return("Potentiellement coût-efficace selon l'ancien repère OMS (1 à 3 × PIB/habitant), à confirmer.")
  "Peu coût-efficace (plus de 3 × PIB/habitant par DALY évitée)."
}

# Analyse de sensibilité : recalcule les indicateurs sous des hypothèses alternatives.
sensibilite <- function(parametres, couts, resultats) {
  monetaire <- c("eco_systeme", "eco_menages", "productivite", "valeur_sociale")
  scenario <- function(nom, p = parametres, c = couts, r = resultats) {
    res <- calcul_roi(p, c, r)
    data.frame(
      Scenario = nom,
      ROI = res$financier$roi,
      SROI = res$sroi$ratio,
      CoutParDALY = res$cea$cout_par_daly
    )
  }
  echelle <- function(r, f) { r[monetaire] <- r[monetaire] * f; r }
  plus_prudent <- function(r) {
    r$deadweight <- pmin(1, r$deadweight * 1.5)
    r$attribution <- pmin(1, r$attribution * 1.5)
    r
  }
  rbind(
    scenario("Scénario de base"),
    scenario("Valeurs monétaires -20 %", r = echelle(resultats, 0.8)),
    scenario("Valeurs monétaires +20 %", r = echelle(resultats, 1.2)),
    scenario("Coûts +20 %", c = transform(couts, investissement = investissement * 1.2, recurrent = recurrent * 1.2)),
    scenario("Deadweight et attribution × 1,5", r = plus_prudent(resultats)),
    scenario("Taux d'actualisation 0 %", p = modifyList(parametres, list(taux_actualisation = 0))),
    scenario("Taux d'actualisation 6 %", p = modifyList(parametres, list(taux_actualisation = 0.06)))
  )
}
