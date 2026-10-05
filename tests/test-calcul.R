# Lancer depuis la racine du dépôt : Rscript tests/run.R (testthat se place dans tests/)
library(testthat)

source(file.path("..", "app", "calcul.R"))

params <- list(duree = 1, taux_actualisation = 0, attribution = 1)
activite <- function(...) {
  a <- data.frame(nom = "A", resultat = "cas traités", investissement = 1000, recurrent = 0,
                  beneficiaires = 50, resultats = 100, daly_par_resultat = 0.1,
                  economie_par_resultat = 5)
  modifyList(a, list(...))
}

test_that("indicateurs de coût-efficacité sur un cas simple", {
  r <- calcul_ce(params, activite())
  a <- r$activites[[1]]
  expect_equal(a$cout, 1000)
  expect_equal(a$cout_par_beneficiaire, 20)
  expect_equal(a$cout_par_resultat, 10)
  expect_equal(a$daly, 10)
  expect_equal(a$cout_par_daly, 100)
  expect_equal(a$economies, 500)
  expect_equal(a$cout_net_par_daly, 50)
  expect_equal(a$roi, -0.5)
  expect_equal(r$total$cout_par_daly, 100)
})

test_that("plusieurs années, actualisation et attribution", {
  p <- list(duree = 2, taux_actualisation = 0.1, attribution = 0.5)
  r <- calcul_ce(p, activite(recurrent = 110))
  a <- r$activites[[1]]
  expect_equal(a$cout, 1000 + 110 + 100)
  expect_equal(a$beneficiaires, 100)
  expect_equal(a$resultats, 100 * (1 + 1 / 1.1) * 0.5)
})

test_that("les totaux additionnent les activités", {
  acts <- rbind(activite(), activite(nom = "B", investissement = 3000, daly_par_resultat = 0.3))
  r <- calcul_ce(params, acts)
  expect_equal(r$total$cout, 4000)
  expect_equal(r$total$daly, 40)
  expect_equal(r$total$cout_par_daly, 100)
  expect_true(is.na(r$total$resultats))
})

test_that("ICER par rapport à la situation de référence", {
  r <- calcul_ce(params, activite(), list(cout_annuel = 400, daly_annuels = 4))
  expect_equal(r$total$icer, (1000 - 400) / (10 - 4))
  sans <- calcul_ce(params, activite(), list(cout_annuel = 0, daly_annuels = 0))
  expect_true(is.na(sans$total$icer))
})

test_that("pas de division par zéro", {
  r <- calcul_ce(params, activite(daly_par_resultat = 0, beneficiaires = 0, resultats = 0))
  expect_true(is.na(r$activites[[1]]$cout_par_daly))
  expect_true(is.na(r$activites[[1]]$cout_par_beneficiaire))
  vide <- calcul_ce(params, activite(investissement = 0))
  expect_true(is.na(vide$activites[[1]]$roi))
})

test_that("seuil et verdict", {
  expect_equal(seuil_effectif(NA, 1000), 500)
  expect_equal(seuil_effectif(800, 1000), 800)
  expect_true(is.na(seuil_effectif(NA, NA)))
  expect_equal(verdict(100, 500)$niveau, "excellent")
  expect_equal(verdict(400, 500)$niveau, "bon")
  expect_equal(verdict(800, 500)$niveau, "limite")
  expect_equal(verdict(2000, 500)$niveau, "faible")
  expect_equal(verdict(-5, 500)$niveau, "excellent")
  expect_equal(verdict(NA, 500)$niveau, "inconnu")
  expect_equal(verdict(100, NA)$niveau, "inconnu")
})

test_that("sensibilité et exemple", {
  p <- list(duree = 3, taux_actualisation = 0.03, attribution = 0.8)
  s <- sensibilite(p, exemple_activites(), list(cout_annuel = 0, daly_annuels = 0))
  expect_equal(nrow(s), 8)
  expect_true(all(is.finite(s$cout_par_daly)))
  expect_gt(s$cout_par_daly[2], s$cout_par_daly[1])
})
