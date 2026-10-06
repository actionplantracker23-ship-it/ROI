# Moteur d'évaluation économique d'un projet de santé (toutes natures de projets).
#
# Références méthodologiques :
#   Drummond et al., Methods for the Economic Evaluation of Health Care Programmes, 4e éd., 2015
#   OMS, Making Choices in Health: WHO Guide to Cost-Effectiveness Analysis, 2003
#   Wilkinson et al., iDSI Reference Case, Value in Health 2016;19:921-928
#   Stinnett & Mullahy, Med Decis Making 1998 (bénéfice net)
#   Murray, Bull WHO 1994 (DALY = YLL + YLD)
#
# Conventions :
#   - année 1 = t = 0, non actualisée ; année t actualisée par 1 / (1 + r)^(t - 1)
#   - coûts et bénéfices monétaires actualisés au taux r_couts, effets de santé au taux r_effets
#   - toutes les valeurs monétaires sont dans la monnaie du projet

facteur_actualisation <- function(taux, n) 1 / (1 + taux)^(seq_len(n) - 1)

ratio <- function(a, b) {
  if (length(a) != 1 || length(b) != 1 || !is.finite(a) || !is.finite(b) || b == 0) NA_real_ else a / b
}

# DALY évitées par résultat, calculées à partir des décès et des épisodes de maladie évités :
#   YLL = décès évités × années de vie perdues par décès
#   YLD = épisodes évités × poids d'incapacité × durée de l'épisode (années)
daly_par_resultat <- function(deces, annees_perdues, episodes, poids_incapacite, duree_episode) {
  deces * annees_perdues + episodes * poids_incapacite * duree_episode
}

# Colonnes attendues pour les activités (valeurs annuelles, sauf l'investissement) :
colonnes_activites <- c("investissement", "recurrent", "beneficiaires", "resultats",
                        "daly_par_resultat", "eco_sante", "eco_menages", "productivite")

completer_activites <- function(a) {
  for (col in colonnes_activites) if (is.null(a[[col]])) a[[col]] <- 0
  for (col in colonnes_activites) a[[col]][is.na(a[[col]])] <- 0
  if (is.null(a$nom)) a$nom <- paste("Activité", seq_len(nrow(a)))
  if (is.null(a$unite)) a$unite <- "résultats"
  a
}

# Flux annuels du projet (non actualisés).
flux_annuels <- function(p, a) {
  n <- p$duree
  part <- min(1, max(0, p$attribution))
  annee <- seq_len(n)
  invest <- c(sum(a$investissement), rep(0, n - 1))
  fonct <- rep(sum(a$recurrent), n)
  resultats_eff <- a$resultats * part
  data.frame(
    annee = p$annee_debut + annee - 1,
    investissement = invest,
    fonctionnement = fonct,
    couts = invest + fonct,
    beneficiaires = rep(sum(a$beneficiaires), n),
    daly = rep(sum(resultats_eff * a$daly_par_resultat), n),
    eco_sante = rep(sum(resultats_eff * a$eco_sante), n),
    eco_menages = rep(sum(resultats_eff * a$eco_menages), n),
    productivite = rep(sum(resultats_eff * a$productivite), n)
  )
}

# Classement dans le plan coût-efficacité et décision par le bénéfice monétaire net.
decision <- function(dC, dE, seuil) {
  quadrant <- if (dE > 0 && dC <= 0) "dominante" else if (dE <= 0 && dC >= 0) "dominee" else
    if (dE > 0) "nord_est" else "sud_ouest"
  nmb <- if (is.finite(seuil)) seuil * dE - dC else NA_real_
  efficace <- switch(quadrant,
                     dominante = TRUE,
                     dominee = FALSE,
                     if (is.na(nmb)) NA else nmb > 0)
  list(quadrant = quadrant, nmb = nmb, nhb = if (is.finite(seuil) && seuil > 0) dE - dC / seuil else NA_real_,
       cout_efficace = efficace)
}

# p : list(annee_debut, duree, taux_couts, taux_effets, attribution, seuil)
# a : data.frame des activités (voir colonnes_activites, + nom, unite)
# comp : list(cout_annuel, investissement, daly_annuels) — pratique actuelle ; NULL = aucune intervention
evaluer <- function(p, a, comp = NULL) {
  a <- completer_activites(a)
  p$duree <- max(1, round(p$duree))
  n <- p$duree
  fc <- facteur_actualisation(p$taux_couts, n)
  fe <- facteur_actualisation(p$taux_effets, n)
  part <- min(1, max(0, p$attribution))
  flux <- flux_annuels(p, a)

  va_c <- function(x) sum(x * fc)
  va_e <- function(x) sum(x * fe)

  # -- Coûts --
  cout <- va_c(flux$couts)
  couts <- list(
    total_nominal = sum(flux$couts),
    total = cout,
    investissement = sum(flux$investissement),
    fonctionnement = va_c(flux$fonctionnement),
    beneficiaires = sum(flux$beneficiaires),
    beneficiaires_an = sum(a$beneficiaires),
    cout_par_beneficiaire = ratio(cout, sum(flux$beneficiaires)),
    cout_par_beneficiaire_an = ratio(cout / n, sum(a$beneficiaires))
  )

  # -- Effets et bénéfices --
  daly <- va_e(flux$daly)
  economies <- va_c(flux$eco_sante + flux$eco_menages + flux$productivite)
  benefices <- list(
    eco_sante = va_c(flux$eco_sante),
    eco_menages = va_c(flux$eco_menages),
    productivite = va_c(flux$productivite),
    total = economies
  )

  # -- Par activité --
  par_activite <- do.call(rbind, lapply(seq_len(nrow(a)), function(i) {
    x <- a[i, ]
    c_i <- x$investissement + x$recurrent * sum(fc)
    r_i <- x$resultats * part * sum(fe)
    d_i <- r_i * x$daly_par_resultat
    b_i <- x$resultats * part * (x$eco_sante + x$eco_menages + x$productivite) * sum(fc)
    data.frame(
      nom = x$nom, unite = x$unite, cout = c_i, part_cout = ratio(c_i, cout),
      beneficiaires = x$beneficiaires * n, resultats = r_i, daly = d_i, economies = b_i,
      cout_par_beneficiaire = ratio(c_i, x$beneficiaires * n),
      cout_par_resultat = ratio(c_i, r_i),
      cout_par_daly = ratio(c_i, d_i),
      stringsAsFactors = FALSE
    )
  }))

  # -- Comparaison à la pratique actuelle (par défaut : aucune intervention) --
  if (is.null(comp)) comp <- list(cout_annuel = 0, investissement = 0, daly_annuels = 0)
  c0 <- val0(comp$investissement) + val0(comp$cout_annuel) * sum(fc)
  e0 <- val0(comp$daly_annuels) * sum(fe)
  dC <- cout - c0
  dE <- daly - e0
  ce <- c(
    list(
      acer = ratio(cout, daly),
      cout_net = cout - economies,
      acer_net = ratio(cout - economies, daly),
      cout_comparateur = c0, daly_comparateur = e0,
      delta_cout = dC, delta_daly = dE,
      icer = if (dE > 0 && dC > 0 || dE < 0 && dC < 0) ratio(dC, dE) else NA_real_,
      seuil = p$seuil
    ),
    decision(dC, dE, p$seuil)
  )

  # -- Rendement financier (analyse coût-bénéfice) --
  cumul <- cumsum((flux$eco_sante + flux$eco_menages + flux$productivite - flux$couts) * fc)
  retour <- which(cumul >= 0)
  financier <- list(
    van = economies - cout,
    roi = ratio(economies - cout, cout),
    ratio_bc = ratio(economies, cout),
    annee_retour = if (length(retour) > 0 && cout > 0) flux$annee[retour[1]] else NA
  )

  flux$couts_actualises <- flux$couts * fc
  flux$daly_actualisees <- flux$daly * fe

  list(couts = couts, daly = daly, benefices = benefices, activites = par_activite,
       ce = ce, financier = financier, flux = flux)
}

# Valeur numérique scalaire, ou 0 si absente.
val0 <- function(x) if (is.null(x) || length(x) != 1 || is.na(x)) 0 else x

# Seuil : saisi par l'utilisateur, sinon 0,5 × PIB par habitant (repère indicatif,
# cf. Woods et al. 2016, Ochalek et al. 2018), sinon NA.
seuil_effectif <- function(seuil, pib_hab) {
  if (is.finite(seuil) && seuil > 0) return(list(valeur = seuil, source = "saisi"))
  if (is.finite(pib_hab) && pib_hab > 0) return(list(valeur = 0.5 * pib_hab, source = "pib"))
  list(valeur = NA_real_, source = "aucun")
}

# ---- Incertitude --------------------------------------------------------------

# Analyse de sensibilité univariée (tornade) : effet de chaque groupe de paramètres
# sur le ratio de décision (ICER face au comparateur, ou ACER sans comparateur).
# La colonne `parametre` contient une clé (couts, resultats, daly, attribution,
# actualisation), traduite par l'interface.
tornade <- function(p, a, comp, incertitude) {
  ratio_decision <- function(pp = p, aa = a) {
    r <- evaluer(pp, aa, comp)$ce
    ratio(r$delta_cout, r$delta_daly)
  }
  base <- ratio_decision()
  varier <- function(cols, pct) {
    bas <- a; haut <- a
    bas[cols] <- bas[cols] * (1 - pct)
    haut[cols] <- haut[cols] * (1 + pct)
    c(ratio_decision(aa = bas), ratio_decision(aa = haut))
  }
  lignes <- list(
    couts = varier(c("investissement", "recurrent"), incertitude$couts),
    resultats = varier("resultats", incertitude$resultats),
    daly = varier("daly_par_resultat", incertitude$daly),
    attribution = c(ratio_decision(pp = modifyList(p, list(attribution = p$attribution * (1 - incertitude$resultats)))),
                           ratio_decision(pp = modifyList(p, list(attribution = min(1, p$attribution * (1 + incertitude$resultats)))))),
    actualisation = c(ratio_decision(pp = modifyList(p, list(taux_couts = 0, taux_effets = 0))),
                                           ratio_decision(pp = modifyList(p, list(taux_couts = 0.06, taux_effets = 0.06))))
  )
  d <- data.frame(parametre = names(lignes),
                  bas = vapply(lignes, `[`, 0, 1), haut = vapply(lignes, `[`, 0, 2),
                  base = base, row.names = NULL)
  d$ecart <- abs(d$haut - d$bas)
  d[order(d$ecart), ]
}

# Analyse probabiliste (Monte-Carlo). Chaque marge d'incertitude ±x % est traitée comme
# un intervalle à 95 % d'un facteur multiplicatif log-normal, tiré indépendamment
# pour chaque activité. Le comparateur est tenu fixe.
probabiliste <- function(p, a, comp, incertitude, n_sim = 1000, graine = 2024) {
  a <- completer_activites(a)
  set.seed(graine)
  k <- nrow(a)
  n <- max(1, round(p$duree))
  sfc <- sum(facteur_actualisation(p$taux_couts, n))
  sfe <- sum(facteur_actualisation(p$taux_effets, n))
  part <- min(1, max(0, p$attribution))
  facteur <- function(pct) {
    sd <- if (pct > 0) log(1 + pct) / 1.96 else 0
    matrix(exp(rnorm(n_sim * k, 0, sd)), n_sim, k)
  }
  fC <- facteur(incertitude$couts); fR <- facteur(incertitude$resultats)
  fD <- facteur(incertitude$daly); fB <- facteur(incertitude$economies)
  ligne <- function(x) matrix(x, n_sim, k, byrow = TRUE)

  cout <- rowSums(ligne(a$investissement + a$recurrent * sfc) * fC)
  res <- ligne(a$resultats * part) * fR
  daly <- rowSums(res * sfe * ligne(a$daly_par_resultat) * fD)
  eco <- rowSums(res * sfc * ligne(a$eco_sante + a$eco_menages + a$productivite) * fB)

  if (is.null(comp)) comp <- list(cout_annuel = 0, investissement = 0, daly_annuels = 0)
  c0 <- val0(comp$investissement) + val0(comp$cout_annuel) * sfc
  e0 <- val0(comp$daly_annuels) * sfe
  data.frame(delta_cout = cout - c0, delta_daly = daly - e0, cout = cout, economies = eco)
}

# Courbe d'acceptabilité : probabilité que le bénéfice monétaire net soit positif.
acceptabilite <- function(sim, seuils) {
  vapply(seuils, function(l) mean(l * sim$delta_daly - sim$delta_cout > 0), 0)
}

intervalle <- function(x, niveau = 0.95) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(NA_real_, NA_real_))
  unname(quantile(x, c((1 - niveau) / 2, 1 - (1 - niveau) / 2)))
}

# ---- Exemple -------------------------------------------------------------------

# Projet fictif illustrant comment remplir l'outil (libellés en français ou en anglais).
exemple_activites <- function(langue = "fr") {
  en <- identical(langue, "en")
  data.frame(
    nom = if (en) c("Training community health workers in rapid testing",
                    "Home-based malaria testing and treatment",
                    "SMS follow-up of pregnant women") else
      c("Formation des agents communautaires au test rapide",
        "Dépistage et traitement du paludisme à domicile",
        "Suivi des femmes enceintes par SMS"),
    unite = if (en) c("health workers trained", "cases treated", "women followed") else
      c("agents formés", "cas traités", "femmes suivies"),
    investissement = c(4000000, 2500000, 1500000),
    recurrent = c(1000000, 6000000, 1200000),
    beneficiaires = c(60, 12000, 800),
    resultats = c(60, 3000, 800),
    daly_par_resultat = c(0, 0.02, 0.05),
    eco_sante = c(0, 1500, 2000),
    eco_menages = c(0, 1000, 1000),
    productivite = c(0, 1500, 0),
    stringsAsFactors = FALSE
  )
}
