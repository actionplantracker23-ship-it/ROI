# Moteur de calcul coût-efficacité pour des projets d'innovation en santé.
#
# Chaque projet décrit librement ses activités. Pour chaque activité :
#   - coûts : investissement (année 1) et coût récurrent annuel
#   - portée : bénéficiaires par an
#   - effet  : nombre de résultats par an (cas traités, enfants vaccinés…),
#              DALY évitées par résultat, économies générées par résultat
#
# Le DALY sert d'unité commune pour comparer des activités différentes.
# Les coûts, les résultats et les DALY sont actualisés au même taux
# (année 1 non actualisée) ; les bénéficiaires sont comptés sans actualisation.

facteur_actualisation <- function(taux, n) {
  1 / (1 + taux)^(seq_len(n) - 1)
}

ratio <- function(a, b) {
  if (length(a) != 1 || length(b) != 1 || !is.finite(a) || !is.finite(b) || b <= 0) NA_real_ else a / b
}

# Indicateurs d'une activité (ou d'un total) à partir de ses flux actualisés.
indicateurs <- function(cout, beneficiaires, resultats, daly, economies) {
  list(
    cout = cout,
    beneficiaires = beneficiaires,
    resultats = resultats,
    daly = daly,
    economies = economies,
    cout_net = cout - economies,
    cout_par_beneficiaire = ratio(cout, beneficiaires),
    cout_par_resultat = ratio(cout, resultats),
    cout_par_daly = ratio(cout, daly),
    cout_net_par_daly = ratio(cout - economies, daly),
    roi = ratio(economies - cout, cout)
  )
}

# parametres : list(duree, taux_actualisation, attribution)
#   attribution = part de l'effet réellement due à l'innovation (0–1)
# activites  : data.frame(nom, resultat, investissement, recurrent, beneficiaires,
#              resultats, daly_par_resultat, economie_par_resultat)
# reference  : list(cout_annuel, daly_annuels) — situation sans l'innovation (optionnelle)
calcul_ce <- function(parametres, activites, reference = NULL) {
  n <- max(1, round(parametres$duree))
  fa <- facteur_actualisation(parametres$taux_actualisation, n)
  somme_fa <- sum(fa)
  part <- min(1, max(0, parametres$attribution))

  par_activite <- lapply(seq_len(nrow(activites)), function(i) {
    a <- activites[i, ]
    cout <- a$investissement + a$recurrent * somme_fa
    resultats <- a$resultats * somme_fa * part
    ind <- indicateurs(
      cout = cout,
      beneficiaires = a$beneficiaires * n,
      resultats = resultats,
      daly = resultats * a$daly_par_resultat,
      economies = resultats * a$economie_par_resultat
    )
    c(list(nom = a$nom, resultat = a$resultat), ind)
  })

  somme <- function(champ) sum(vapply(par_activite, `[[`, 0, champ))
  total <- indicateurs(
    cout = somme("cout"),
    beneficiaires = somme("beneficiaires"),
    resultats = NA_real_,  # résultats d'activités différentes : non additionnables
    daly = somme("daly"),
    economies = somme("economies")
  )

  # Ratio coût-efficacité différentiel (ICER) par rapport à la situation de référence.
  icer <- NA_real_
  if (!is.null(reference) && (reference$cout_annuel > 0 || reference$daly_annuels > 0)) {
    cout_ref <- reference$cout_annuel * somme_fa
    daly_ref <- reference$daly_annuels * somme_fa
    icer <- ratio(total$cout - cout_ref, total$daly - daly_ref)
    total$cout_reference <- cout_ref
    total$daly_reference <- daly_ref
  }
  total$icer <- icer

  list(activites = par_activite, total = total)
}

# Seuil de coût-efficacité par DALY : celui saisi par l'utilisateur, sinon
# 0,5 × PIB/habitant (repère prudent recommandé depuis l'abandon des 1–3 × PIB).
seuil_effectif <- function(seuil, pib_hab) {
  if (is.finite(seuil) && seuil > 0) return(seuil)
  if (is.finite(pib_hab) && pib_hab > 0) return(0.5 * pib_hab)
  NA_real_
}

# Verdict : niveau (pour la couleur) et phrase d'interprétation.
verdict <- function(cout_par_daly, seuil) {
  if (is.na(cout_par_daly)) {
    return(list(niveau = "inconnu",
                texte = "Renseignez les DALY évitées par résultat pour obtenir le coût par DALY."))
  }
  if (cout_par_daly <= 0) {
    return(list(niveau = "excellent",
                texte = "Dominante : l'innovation évite des DALY tout en économisant de l'argent."))
  }
  if (is.na(seuil)) {
    return(list(niveau = "inconnu",
                texte = "Renseignez un seuil ou le PIB par habitant pour interpréter le coût par DALY."))
  }
  r <- cout_par_daly / seuil
  if (r <= 0.5) return(list(niveau = "excellent", texte = "Très coût-efficace : bien en dessous du seuil."))
  if (r <= 1) return(list(niveau = "bon", texte = "Coût-efficace : en dessous du seuil."))
  if (r <= 2) return(list(niveau = "limite", texte = "À la limite : au-dessus du seuil, à justifier ou optimiser."))
  list(niveau = "faible", texte = "Peu coût-efficace : nettement au-dessus du seuil.")
}

# Analyse de sensibilité sur le coût par DALY évitée du projet.
sensibilite <- function(parametres, activites, reference = NULL) {
  scenario <- function(nom, p = parametres, a = activites) {
    t <- calcul_ce(p, a, reference)$total
    data.frame(scenario = nom, cout_par_daly = t$cout_par_daly,
               cout_par_beneficiaire = t$cout_par_beneficiaire, icer = t$icer)
  }
  echelle <- function(a, cols, f) { a[cols] <- a[cols] * f; a }
  couts <- c("investissement", "recurrent")
  effets <- c("resultats")
  rbind(
    scenario("Scénario de base"),
    scenario("Coûts +20 %", a = echelle(activites, couts, 1.2)),
    scenario("Coûts −20 %", a = echelle(activites, couts, 0.8)),
    scenario("Résultats −20 %", a = echelle(activites, effets, 0.8)),
    scenario("Résultats +20 %", a = echelle(activites, effets, 1.2)),
    scenario("Attribution −20 points", p = modifyList(parametres, list(attribution = max(0, parametres$attribution - 0.2)))),
    scenario("Actualisation 0 %", p = modifyList(parametres, list(taux_actualisation = 0))),
    scenario("Actualisation 6 %", p = modifyList(parametres, list(taux_actualisation = 0.06)))
  )
}

# Projet d'exemple, pour montrer comment remplir l'outil (valeurs fictives).
exemple_activites <- function() {
  data.frame(
    nom = c("Formation des agents communautaires au test rapide",
            "Diagnostic et traitement du paludisme à domicile",
            "Suivi des femmes enceintes par SMS"),
    resultat = c("agents formés", "cas traités", "femmes suivies"),
    investissement = c(4000000, 2500000, 1500000),
    recurrent = c(1000000, 6000000, 1200000),
    beneficiaires = c(60, 12000, 800),
    resultats = c(60, 3000, 800),
    daly_par_resultat = c(0, 0.02, 0.05),
    economie_par_resultat = c(0, 2500, 3000)
  )
}
