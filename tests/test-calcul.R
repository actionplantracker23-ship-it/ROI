# Lancer depuis la racine du dépôt : Rscript tests/run.R (testthat se place dans tests/)
library(testthat)

source(file.path("..", "app", "calcul.R"))
source(file.path("..", "app", "modeles.R"))

params <- list(annee_debut = 2026, duree = 1, taux_actualisation = 0, taux_change = 100, pib_hab_usd = 1000)
couts_simples <- data.frame(libelle = "Projet", investissement = 1000, recurrent = 0)
resultat_simple <- function(...) {
  r <- data.frame(libelle = "R", unites = 100, croissance = 0, eco_systeme = 15, eco_menages = 5,
                  productivite = 0, valeur_sociale = 10, daly = 0.1, deadweight = 0,
                  attribution = 0, displacement = 0, duree_effet = 1, drop_off = 0)
  modifyList(r, list(...))
}

test_that("ROI, SROI et coût par DALY sur un cas simple", {
  res <- calcul_roi(params, couts_simples, resultat_simple())
  expect_equal(res$financier$va_benefices, 2000)
  expect_equal(res$financier$roi, 1)
  expect_equal(res$financier$ratio_bc, 2)
  expect_equal(res$financier$annee_retour, 2026)
  expect_equal(res$sroi$ratio, 3)
  expect_equal(res$cea$daly, 10)
  expect_equal(res$cea$cout_par_daly, 100)
  expect_equal(res$cea$cout_par_daly_usd, 1)
  expect_equal(res$cea$ratio_pib, 0.001)
  expect_equal(res$cea$cout_net_par_daly, -100)
})

test_that("deadweight, attribution et déplacement réduisent l'impact", {
  r <- resultat_simple(deadweight = 0.5, attribution = 0.2, displacement = 0.5)
  res <- calcul_roi(params, couts_simples, r)
  expect_equal(res$financier$va_benefices, 2000 * 0.5 * 0.8 * 0.5)
  expect_equal(res$cea$daly, 10 * 0.2)
})

test_that("actualisation : l'année 1 n'est pas actualisée", {
  expect_equal(facteur_actualisation(0.1, 3), c(1, 1 / 1.1, 1 / 1.21))
  p <- modifyList(params, list(duree = 2, taux_actualisation = 0.1))
  c <- data.frame(libelle = "x", investissement = 0, recurrent = 110)
  res <- calcul_roi(p, c, resultat_simple(unites = 0))
  expect_equal(res$financier$va_couts, 110 + 100)
})

test_that("croissance annuelle des unités", {
  expect_equal(unites_par_annee(100, 0.1, 3), c(100, 110, 121))
})

test_that("effet sur plusieurs années avec baisse annuelle prolonge l'analyse", {
  expect_equal(unites_effectives(100, 3, 0.5, 3), c(100, 50, 25))
  r <- resultat_simple(duree_effet = 3, drop_off = 0.5)
  res <- calcul_roi(params, couts_simples, r)
  expect_equal(nrow(res$flux), 3)
  expect_equal(res$flux$annee, 2026:2028)
  expect_equal(res$cea$daly, (100 + 50 + 25) * 0.1)
  expect_equal(res$flux$couts, c(1000, 0, 0))
})

test_that("pas de division par zéro", {
  vide <- data.frame(libelle = "x", investissement = 0, recurrent = 0)
  res <- calcul_roi(params, vide, resultat_simple(daly = 0))
  expect_true(is.na(res$financier$roi))
  expect_true(is.na(res$sroi$ratio))
  expect_true(is.na(res$cea$cout_par_daly))
  expect_true(is.na(res$financier$annee_retour))
})

test_that("retour sur investissement non atteint", {
  res <- calcul_roi(params, couts_simples, resultat_simple(eco_systeme = 1, eco_menages = 0))
  expect_true(is.na(res$financier$annee_retour))
  expect_lt(res$financier$roi, 0)
})

test_that("interprétation du coût-efficacité", {
  expect_match(interpretation_cea(0.3), "Très coût-efficace")
  expect_match(interpretation_cea(0.8), "^Coût-efficace")
  expect_match(interpretation_cea(2), "Potentiellement")
  expect_match(interpretation_cea(5), "Peu")
})

test_that("tous les modèles d'exemple se calculent pour tous les pays", {
  for (code in names(pays)) for (type in types_projet) {
    fx <- pays[[code]]$taux_change
    p <- list(annee_debut = 2026, duree = 3, taux_actualisation = 0.03, taux_change = fx,
              pib_hab_usd = pays[[code]]$pib_hab_usd)
    res <- calcul_roi(p, couts_exemple(type, fx), resultats_exemple(type, fx))
    expect_true(is.finite(res$financier$roi))
    expect_true(is.finite(res$sroi$ratio))
    expect_true(is.finite(res$cea$cout_par_daly))
    s <- sensibilite(p, couts_exemple(type, fx), resultats_exemple(type, fx))
    expect_equal(nrow(s), 7)
  }
})
