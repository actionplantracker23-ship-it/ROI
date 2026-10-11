# Moteur des indicateurs Q5 et de l'analyse coût-efficacité, en trois couches.
#
#   Couche A : développement non récurrent, exclu de R ; annualisé pour la vue économique.
#   Couche B : chaîne de volumes (dénominateurs), mensuelle :
#                MAU = B1 × B3 ; I = MAU × B4 ; E = I × B5 ; F = E × B6
#              A (épisodes avec action vérifiée) est mesuré séparément.
#   Couche C : coûts récurrents ; numérateur partagé R = C1 + … + C9 pour la période
#              de rapport (annuel converti), hors couche A. Estimation « partielle »
#              tant qu'une ligne de coût manque.
#
# Indicateurs Q5 : OC5a = R / MAU ; OC5b = R / I ; OC5c = R / A ; OC5d = R / F ;
#                  OC5e = (lignes d'accès gratuit, C4 + C5) / bénéficiaires atteints
#                  (composante de R, non additive).
# Niveau santé : RCEI = (R − coût du comparateur) / effet incrémental.
#
# Références : Drummond et al. 2015 ; OMS-CHOICE 2003 (annualisation) ;
# iDSI Reference Case 2016 ; Stinnett & Mullahy 1998.

ratio <- function(a, b) {
  if (length(a) != 1 || length(b) != 1 || !is.finite(a) || !is.finite(b) || b == 0) NA_real_ else a / b
}

val0 <- function(x) if (is.null(x) || length(x) != 1 || is.na(x)) 0 else as.numeric(x)

# Nombre de mois par période de rapport.
mois_periode <- function(periode) if (identical(periode, "an")) 12 else 1

# ---- Couche A : annualisation ------------------------------------------------

facteur_annuite <- function(taux, duree) {
  duree <- max(1, duree)
  if (taux <= 0) duree else (1 - (1 + taux)^(-duree)) / taux
}

annualiser <- function(montant, duree, taux) montant / facteur_annuite(taux, duree)

# a : data.frame(code, libelle, montant, statut) ; statut in connu, estime, attente
couche_a <- function(a, duree_vie, taux) {
  compte <- !is.na(a$montant) & a$statut != "attente"
  total <- sum(a$montant[compte])
  list(total = total, connu = sum(a$montant[compte & a$statut == "connu"]),
       facteur = facteur_annuite(taux, duree_vie),
       annuel = annualiser(total, duree_vie, taux),
       en_attente = a$code[a$statut == "attente" | is.na(a$montant)])
}

# ---- Couche B : chaîne de volumes --------------------------------------------------

# b : list(beneficiaires, asc, activation, interactions_par_actif, escalade, completion,
#          episodes_verifies (A, mesuré, par mois ; NA si non mesuré),
#          beneficiaires_atteints (accès gratuit, par mois ; NA = MAU))
couche_b <- function(b) {
  mau <- b$beneficiaires * b$activation
  i <- mau * b$interactions_par_actif
  e <- i * b$escalade
  f <- e * b$completion
  a <- if (is.null(b$episodes_verifies) || is.na(b$episodes_verifies)) NA_real_ else b$episodes_verifies
  atteints <- if (is.null(b$beneficiaires_atteints) || is.na(b$beneficiaires_atteints)) mau else b$beneficiaires_atteints
  list(mensuel = c(beneficiaires = b$beneficiaires, asc = b$asc, mau = mau, interactions = i,
                   escalades = e, resolus = f, verifies = a, atteints = atteints))
}

# Volumes de la période : les flux (interactions, escalades…) s'additionnent sur la
# période ; les stocks (bénéficiaires, ASC, MAU) restent des effectifs mensuels.
volumes_periode <- function(vol, periode) {
  m <- mois_periode(periode)
  v <- vol$mensuel
  flux <- c("interactions", "escalades", "resolus", "verifies")
  v[flux] <- v[flux] * m
  v
}

# ---- Couche C : coûts récurrents ---------------------------------------------------

# Bases : montant annuel fixe, ou coût par unité de volume mensuel.
bases_c <- c("fixe", "beneficiaires", "asc", "mau", "interactions", "escalades", "resolus")

# pc : data.frame(code, libelle, montant, base, statut, nature, gratuit)
#   statut  : connu | estime | attente | hors_budget
#   nature  : financier | economique
#   gratuit : TRUE pour les lignes d'accès gratuit (numérateur d'OC5e)
couche_c <- function(pc, vol) {
  if (is.null(pc$gratuit)) pc$gratuit <- FALSE
  pc$gratuit[is.na(pc$gratuit)] <- FALSE
  # Coût annuel : fixe, ou coût unitaire × volume mensuel × 12.
  quantite <- ifelse(pc$base == "fixe", 1, 12 * vol$mensuel[pc$base])
  annuel <- ifelse(is.na(pc$montant) | pc$statut == "attente", NA_real_, pc$montant * quantite)
  chiffre <- !is.na(annuel)
  postes <- data.frame(code = pc$code, libelle = pc$libelle, statut = pc$statut, nature = pc$nature,
                       base = pc$base, gratuit = as.logical(pc$gratuit), annuel = annuel, stringsAsFactors = FALSE)
  postes$plancher <- chiffre & pc$statut == "connu"
  postes$r <- chiffre
  somme <- function(garde) sum(annuel[garde & chiffre])
  list(
    postes = postes,
    plancher = somme(postes$plancher),
    r = somme(postes$r),
    decaisse = somme(postes$r & postes$nature == "financier"),
    non_decaisse = somme(postes$r & postes$nature == "economique"),
    gratuit = somme(postes$gratuit),
    gratuit_complet = all(chiffre[postes$gratuit]) && any(postes$gratuit),
    manquants = pc$code[!chiffre],
    hors_budget = pc$code[pc$statut == "hors_budget"],
    partiel = any(!chiffre)
  )
}

# ---- Effets de santé --------------------------------------------------------------

# s : list(compl_pour_1000, daly_par_compl, deces_pour_1000, economie_par_compl,
#          cout_comparateur (annuel)) — effets incrémentaux face au statu quo,
#          pour 1 000 suivis résolus (F).
effets_sante <- function(s, f) {
  complications <- f * val0(s$compl_pour_1000) / 1000
  list(complications = complications, daly = complications * val0(s$daly_par_compl),
       deces = f * val0(s$deces_pour_1000) / 1000, economies = complications * val0(s$economie_par_compl))
}

# ---- Évaluation ------------------------------------------------------------------

# plancher : lignes connues seulement ; r : R de Q5 (toutes les lignes chiffrées, hors A) ;
# economique : R + couche A annualisée.
perspectives <- c("plancher", "r", "economique")
indicateurs_q5 <- c(OC5a = "mau", OC5b = "interactions", OC5c = "verifies", OC5d = "resolus")

# p : list(taux, duree_vie, seuil, periode = "mois" | "an")
evaluer <- function(p, a, b, pc, s = list()) {
  periode <- p$periode %||% "mois"
  fraction <- mois_periode(periode) / 12
  A <- couche_a(a, p$duree_vie, p$taux)
  vol <- couche_b(b)
  C <- couche_c(pc, vol)
  v <- volumes_periode(vol, periode)

  couts_annuels <- c(plancher = C$plancher, r = C$r, economique = C$r + A$annuel)
  couts <- couts_annuels * fraction

  denominateurs <- c("beneficiaires", "asc", "mau", "interactions", "escalades", "resolus", "verifies")
  unitaires <- sapply(perspectives, function(k) vapply(denominateurs, function(d) ratio(couts[[k]], v[[d]]), 0))
  rownames(unitaires) <- denominateurs

  # OC5e : non calculable tant qu'aucune ligne d'accès gratuit n'est chiffrée.
  gratuit_chiffre <- any(C$postes$gratuit & !is.na(C$postes$annuel))
  oc5e <- if (gratuit_chiffre) ratio(C$gratuit * fraction, v[["atteints"]]) else NA_real_

  sante <- effets_sante(s, v[["resolus"]])
  cout_comp <- val0(s$cout_comparateur) * fraction
  niveau3 <- lapply(setNames(perspectives, perspectives), function(k) {
    dC <- couts[[k]] - cout_comp
    list(delta_cout = dC,
         rcei_daly = if (sante$daly > 0) dC / sante$daly else NA_real_,
         rcei_deces = if (sante$deces > 0) dC / sante$deces else NA_real_,
         nmb = if (is.finite(val0(p$seuil)) && val0(p$seuil) > 0 && sante$daly > 0) p$seuil * sante$daly - dC else NA_real_,
         roi = if (sante$economies > 0) ratio(sante$economies - couts[[k]], couts[[k]]) else NA_real_)
  })

  list(periode = periode, A = A, volumes = vol, v = v, C = C, couts = couts, couts_annuels = couts_annuels,
       unitaires = unitaires, oc5e = oc5e, sante = sante, niveau3 = niveau3, cout_comparateur = cout_comp)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

verdict_daly <- function(rcei, seuil) {
  if (is.na(rcei)) return("inconnu")
  if (rcei <= 0) return("dominant")
  if (is.na(seuil) || seuil <= 0) return("sans_seuil")
  if (rcei <= seuil) "cout_efficace" else "non_cout_efficace"
}

# ---- Sensibilité univariée ----------------------------------------------------------

sensibilite <- function(p, a, b, pc, s, plages, indicateur = "resolus", perspective = "r") {
  valeur <- function(pp = p, bb = b, cc = pc) evaluer(pp, a, bb, cc, s)$unitaires[indicateur, perspective]
  base <- valeur()
  lignes <- lapply(names(plages), function(k) {
    v <- plages[[k]]
    calc <- function(x) {
      if (k %in% names(b)) { bb <- b; bb[[k]] <- x; valeur(bb = bb) }
      else if (k == "duree_vie") valeur(pp = modifyList(p, list(duree_vie = x)))
      else if (k == "couts_c") { cc <- pc; cc$montant <- cc$montant * x; valeur(cc = cc) }
      else NA_real_
    }
    data.frame(parametre = k, bas = v[1], haut = v[2], valeur_bas = calc(v[1]), valeur_haut = calc(v[2]))
  })
  d <- do.call(rbind, lignes)
  d$base <- base
  d$ecart <- abs(d$valeur_haut - d$valeur_bas)
  d[order(d$ecart), ]
}

# ---- Données de référence : innovation, Phase 3, scénario central --------------------

reference_a <- function(langue = "fr") {
  en <- identical(langue, "en")
  data.frame(
    code = c("A1", "A2", "A3", "A4"),
    libelle = if (en) c("Phase 1 MVP build, committed", "AI-Triage public good, decision requested",
                        "Partner co-funding, not stated", "Phase 3 build and handover, indicative") else
      c("Construction du MVP Phase 1, engagée", "AI-Triage bien public, décision demandée",
        "Cofinancement partenaires, non précisé", "Construction et transfert Phase 3, indicatif"),
    montant = c(50000, 50000, NA, 90000),
    statut = c("connu", "estime", "attente", "estime"),
    stringsAsFactors = FALSE
  )
}

reference_b <- function() {
  list(beneficiaires = 30000, asc = 300, activation = 0.5, interactions_par_actif = 2,
       escalade = 0.1, completion = 0.75, episodes_verifies = NA_real_, beneficiaires_atteints = NA_real_)
}

# Avec estimations = TRUE, les lignes en attente reçoivent des estimations indicatives
# (scénario central Kenya/Ouganda, à remplacer par les devis et les mesures).
reference_c <- function(langue = "fr", estimations = FALSE) {
  en <- identical(langue, "en")
  d <- data.frame(
    code = paste0("C", 1:9),
    libelle = if (en) c("Platform subscription (Turn.io)", "WhatsApp conversation fees", "SMS fees, likely to dominate",
                        "Inbound voice / toll-free", "Callbacks and reverse billing", "AI inference (token)",
                        "CHW time, measured during UAT", "Clinical supervision and helpdesk",
                        "Technical support and usage allowance") else
      c("Abonnement plateforme (Turn.io)", "Frais de conversation WhatsApp", "Frais SMS, probablement dominants",
        "Voix entrante / numéro gratuit", "Rappels et facturation inversée", "Inférence IA (tokens)",
        "Temps des ASC, mesuré pendant l'UAT", "Supervision clinique et helpdesk",
        "Support technique et forfait d'usage"),
    montant = c(NA, NA, NA, NA, NA, NA, NA, NA, 10000),
    base = rep("fixe", 9),
    statut = c("attente", "attente", "attente", "hors_budget", "hors_budget", "attente", "attente",
               "hors_budget", "connu"),
    nature = c(rep("financier", 6), "economique", "financier", "financier"),
    gratuit = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  if (estimations) {
    d$montant[c(1, 2)] <- c(12000, 2500)
    d$montant[3] <- 0.0102; d$base[3] <- "interactions"
    d$montant[6] <- 0.0028; d$base[6] <- "interactions"
    d$montant[7] <- 0.1667; d$base[7] <- "escalades"
    d$statut[c(1, 2, 3, 6, 7)] <- "estime"
  }
  d
}

reference_sensibilite <- function() {
  list(activation = c(0.25, 0.75), interactions_par_actif = c(1, 3), escalade = c(0.05, 0.15),
       completion = c(0.5, 0.9), duree_vie = c(3, 7), couts_c = c(0.75, 1.5))
}
