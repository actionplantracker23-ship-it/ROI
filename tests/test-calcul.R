# Lancer depuis la racine du dépôt : Rscript tests/run.R (testthat se place dans tests/)
library(testthat)

source(file.path("..", "app", "calcul.R"))

p1 <- list(annee_debut = 2026, duree = 1, taux_couts = 0, taux_effets = 0, attribution = 1, seuil = 500)
act <- function(...) {
  a <- data.frame(nom = "A", unite = "cas traités", investissement = 1000, recurrent = 0,
                  beneficiaires = 50, resultats = 100, daly_par_resultat = 0.1,
                  eco_sante = 3, eco_menages = 1, productivite = 1)
  modifyList(a, list(...))
}

test_that("coûts, coût par bénéficiaire et ACER", {
  r <- evaluer(p1, act())
  expect_equal(r$couts$total, 1000)
  expect_equal(r$couts$cout_par_beneficiaire, 20)
  expect_equal(r$daly, 10)
  expect_equal(r$ce$acer, 100)
  expect_equal(r$activites$cout_par_resultat, 10)
  expect_equal(r$benefices$total, 500)
  expect_equal(r$ce$acer_net, 50)
})

test_that("ICER, bénéfice net et décision face au comparateur", {
  r <- evaluer(p1, act(), list(cout_annuel = 400, investissement = 0, daly_annuels = 4))
  expect_equal(r$ce$delta_cout, 600)
  expect_equal(r$ce$delta_daly, 6)
  expect_equal(r$ce$icer, 100)
  expect_equal(r$ce$nmb, 500 * 6 - 600)
  expect_equal(r$ce$nhb, 6 - 600 / 500)
  expect_equal(r$ce$quadrant, "nord_est")
  expect_true(r$ce$cout_efficace)
})

test_that("dominance et quadrants", {
  expect_equal(decision(-10, 5, 100)$quadrant, "dominante")
  expect_true(decision(-10, 5, 100)$cout_efficace)
  expect_equal(decision(10, -5, 100)$quadrant, "dominee")
  expect_false(decision(10, -5, 100)$cout_efficace)
  expect_equal(decision(-1000, -5, 100)$quadrant, "sud_ouest")
  expect_true(decision(-1000, -5, 100)$cout_efficace)   # économise 200 par DALY perdue > seuil
  expect_false(decision(1000, 5, 100)$cout_efficace)    # ICER 200 > seuil 100
  expect_true(is.na(decision(1000, 5, NA)$cout_efficace))
  r <- evaluer(p1, act(), list(cout_annuel = 2000, investissement = 0, daly_annuels = 4))
  expect_equal(r$ce$quadrant, "dominante")
  expect_true(is.na(r$ce$icer))
})

test_that("rendement financier : ROI, ratio B/C, VAN, année de retour", {
  r <- evaluer(p1, act(eco_sante = 15, eco_menages = 5, productivite = 0))
  expect_equal(r$financier$van, 1000)
  expect_equal(r$financier$roi, 1)
  expect_equal(r$financier$ratio_bc, 2)
  expect_equal(r$financier$annee_retour, 2026)
  r2 <- evaluer(p1, act(eco_sante = 0, eco_menages = 0, productivite = 0))
  expect_true(is.na(r2$financier$annee_retour))
  expect_equal(r2$financier$roi, -1)
})

test_that("actualisation séparée des coûts et des effets, attribution", {
  p <- modifyList(p1, list(duree = 2, taux_couts = 0.1, taux_effets = 0, attribution = 0.5))
  r <- evaluer(p, act(recurrent = 110))
  expect_equal(r$couts$total, 1000 + 110 + 100)
  expect_equal(r$couts$total_nominal, 1220)
  expect_equal(r$couts$beneficiaires, 100)
  expect_equal(r$daly, 2 * 100 * 0.5 * 0.1)
  expect_equal(nrow(r$flux), 2)
})

test_that("calcul des DALY à partir des décès et épisodes évités", {
  expect_equal(daly_par_resultat(0.01, 30, 1, 0.2, 0.1), 0.01 * 30 + 0.02)
})

test_that("plusieurs activités et activité de support", {
  a <- rbind(act(), act(nom = "Support", daly_par_resultat = 0, eco_sante = 0, eco_menages = 0, productivite = 0))
  r <- evaluer(p1, a)
  expect_equal(r$couts$total, 2000)
  expect_equal(r$ce$acer, 200)
  expect_true(is.na(r$activites$cout_par_daly[2]))
  expect_equal(r$activites$part_cout, c(0.5, 0.5))
})

test_that("seuil effectif", {
  expect_equal(seuil_effectif(NA, 1000)$valeur, 500)
  expect_equal(seuil_effectif(800, 1000)$source, "saisi")
  expect_true(is.na(seuil_effectif(NA, NA)$valeur))
})

# Projet de démonstration avec effets de santé (pour l'incertitude).
demo <- data.frame(
  nom = c("Formation", "Dépistage et traitement", "Suivi SMS"), unite = c("agents", "cas", "femmes"),
  investissement = c(4e6, 2.5e6, 1.5e6), recurrent = c(1e6, 6e6, 1.2e6), beneficiaires = c(60, 12000, 800),
  resultats = c(60, 3000, 800), daly_par_resultat = c(0, 0.02, 0.05), eco_sante = c(0, 1500, 2000),
  eco_menages = c(0, 1000, 1000), productivite = c(0, 1500, 0))

test_that("incertitude : tornade, Monte-Carlo, acceptabilité", {
  inc <- list(couts = 0.2, resultats = 0.2, daly = 0.3, economies = 0.3)
  p <- list(annee_debut = 2026, duree = 3, taux_couts = 0.03, taux_effets = 0.03, attribution = 0.8, seuil = 250000)
  t <- tornade(p, demo, NULL, inc)
  expect_equal(nrow(t), 5)
  expect_true(all(is.finite(c(t$bas, t$haut))))
  sim <- probabiliste(p, demo, NULL, inc, n_sim = 2000)
  expect_equal(nrow(sim), 2000)
  base <- evaluer(p, demo)
  expect_lt(abs(median(sim$delta_cout) / base$couts$total - 1), 0.05)
  ac <- acceptabilite(sim, c(0, 1e12))
  expect_equal(ac, c(0, 1))
  sans_incertitude <- probabiliste(p, demo, NULL, list(couts = 0, resultats = 0, daly = 0, economies = 0), n_sim = 10)
  expect_equal(sans_incertitude$delta_cout[1], base$couts$total)
  expect_equal(sans_incertitude$delta_daly[1], base$daly)
})

test_that("valeurs manquantes traitées comme zéro", {
  a <- act(); a$eco_sante <- NA
  expect_equal(evaluer(p1, a)$benefices$eco_sante, 0)
})

test_that("statut : les activités en attente sont listées mais exclues, estimation partielle", {
  a <- rbind(act(code = "A1", statut = "connu"), act(code = "C1", investissement = 500, statut = "attente"),
             act(code = "C4", investissement = 0, statut = "hors_budget"))
  r <- evaluer(p1, a)
  expect_equal(r$couts$total, 1000)
  expect_equal(nrow(r$activites), 3)
  expect_equal(r$activites$statut, c("connu", "attente", "hors_budget"))
  expect_setequal(r$manquantes, c("C1", "C4"))
  expect_true(r$partiel)
  a$investissement[3] <- 200
  r2 <- evaluer(p1, a)
  expect_equal(r2$couts$total, 1200)
  expect_equal(r2$manquantes, "C1")
})

test_that("coût par résultat du projet quand l'unité est commune", {
  a <- rbind(act(), act(nom = "B", investissement = 1000, resultats = 0))
  r <- evaluer(p1, a)
  expect_equal(r$couts$cout_par_resultat, 2000 / 100)
  expect_equal(r$couts$unite_resultat, "cas traités")
  r2 <- evaluer(p1, rbind(act(), act(unite = "autre")))
  expect_true(is.na(r2$couts$cout_par_resultat))
})

test_that("exemple : données de l'innovation (couches A, B, C)", {
  e <- exemple_activites()
  expect_equal(nrow(e), 14)
  expect_equal(sum(e$investissement), 190000)
  p5 <- list(annee_debut = 2026, duree = 5, taux_couts = 0.03, taux_effets = 0.03, attribution = 1, seuil = NA)
  r <- evaluer(p5, e)
  sfc <- sum(facteur_actualisation(0.03, 5))
  expect_equal(r$couts$total, 190000 + 10000 * sfc)
  expect_equal(r$couts$beneficiaires, 150000)
  expect_equal(r$couts$cout_par_resultat, (190000 + 10000 * sfc) / (27000 * sfc))
  expect_setequal(r$manquantes, c("A3", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"))
  expect_equal(exemple_activites("en")$unite[5], "resolved follow-ups")
})
