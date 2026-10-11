# Lancer depuis la racine du dépôt : Rscript tests/run.R (testthat se place dans tests/)
library(testthat)

source(file.path("..", "app", "calcul.R"))

p <- list(taux = 0.03, duree_vie = 5, seuil = NA, periode = "mois")
ref <- function(estimations = FALSE, pp = p, b = reference_b()) {
  evaluer(pp, reference_a(), b, reference_c(estimations = estimations))
}

test_that("chaîne de volumes de la couche B (diapositive Q5)", {
  v <- couche_b(reference_b())$mensuel
  expect_equal(unname(v[c("mau", "interactions", "escalades", "resolus")]), c(15000, 30000, 3000, 2250))
  expect_true(is.na(v[["verifies"]]))
  expect_equal(v[["atteints"]], 15000)
})

test_that("R mensuel connu = 10 000 / 12 ; indicateurs Q5 partiels", {
  r <- ref()
  R <- 10000 / 12
  expect_equal(r$couts[["r"]], R)
  expect_equal(r$unitaires["mau", "r"], R / 15000)          # OC5a ≈ 0,056
  expect_equal(r$unitaires["interactions", "r"], R / 30000) # OC5b ≈ 0,028
  expect_equal(r$unitaires["resolus", "r"], R / 2250)       # OC5d ≈ 0,37
  expect_true(is.na(r$unitaires["verifies", "r"]))          # OC5c : A non mesuré
  expect_true(is.na(r$oc5e))                                # C4 + C5 sans montant
  expect_true(r$C$partiel)
  expect_setequal(r$C$manquants, c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"))
})

test_that("période annuelle : mêmes coûts unitaires pour les flux", {
  r <- ref(pp = modifyList(p, list(periode = "an")))
  expect_equal(r$couts[["r"]], 10000)
  expect_equal(r$v[["resolus"]], 27000)
  expect_equal(r$unitaires["resolus", "r"], 10000 / 27000)
  expect_equal(r$unitaires["interactions", "r"], 10000 / 360000)
})

test_that("couche A : 190 000 sur 5 ans à 3 % → facteur 4,58, 41 487 par an, 1,91 par suivi résolu", {
  r <- ref(pp = modifyList(p, list(periode = "an")))
  expect_equal(r$A$total, 190000)
  expect_equal(round(r$A$facteur, 2), 4.58)
  expect_equal(round(r$A$annuel), 41487)
  expect_equal(round(r$unitaires["resolus", "economique"], 2), 1.91)
  expect_equal(r$A$en_attente, "A3")
  expect_equal(annualiser(100, 4, 0), 25)
})

test_that("A mesuré et OC5e avec C4 + C5 chiffrés", {
  b <- reference_b(); b$episodes_verifies <- 1000; b$beneficiaires_atteints <- 5000
  pc <- reference_c(estimations = TRUE)
  pc$montant[4:5] <- c(6000, 3000)
  r <- evaluer(p, reference_a(), b, pc)
  expect_equal(r$oc5e, (9000 / 12) / 5000)
  expect_equal(r$unitaires["verifies", "r"], r$couts[["r"]] / 1000)
  expect_true(r$C$gratuit_complet)
  expect_equal(r$C$manquants, "C8")
  expect_true(r$C$partiel)
})

test_that("coûts variables et ventilation décaissé / non décaissé", {
  r <- ref(estimations = TRUE)
  annuel_c3 <- 0.0102 * 30000 * 12
  expect_equal(r$C$postes$annuel[3], annuel_c3)
  expect_equal(r$C$non_decaisse, 0.1667 * 3000 * 12)
  expect_equal(r$C$decaisse + r$C$non_decaisse, r$C$r)
  b <- reference_b(); b$activation <- 0.25
  r2 <- ref(estimations = TRUE, b = b)
  expect_equal(r2$C$postes$annuel[3], annuel_c3 / 2)
})

test_that("sensibilité : activation 25 % double le coût par suivi résolu (R connu, fixe)", {
  t <- sensibilite(p, reference_a(), reference_b(), reference_c(), list(), reference_sensibilite(),
                   indicateur = "resolus", perspective = "r")
  l <- t[t$parametre == "activation", ]
  expect_equal(l$valeur_bas, 2 * l$base)
  expect_equal(nrow(t), 6)
})

test_that("niveau santé : RCEI, bénéfice net, ROI, verdict", {
  s <- list(compl_pour_1000 = 10, daly_par_compl = 2, deces_pour_1000 = 1, economie_par_compl = 50,
            cout_comparateur = 1200)
  r <- evaluer(modifyList(p, list(seuil = 500)), reference_a(), reference_b(), reference_c(), s)
  expect_equal(r$sante$daly, 2250 * 10 / 1000 * 2)
  n <- r$niveau3$r
  dC <- 10000 / 12 - 100
  expect_equal(n$delta_cout, dC)
  expect_equal(n$rcei_daly, dC / 45)
  expect_equal(n$rcei_deces, dC / 2.25)
  expect_equal(n$nmb, 500 * 45 - dC)
  expect_equal(verdict_daly(n$rcei_daly, 500), "cout_efficace")
  expect_equal(verdict_daly(900, 500), "non_cout_efficace")
  expect_equal(verdict_daly(-1, 500), "dominant")
  expect_equal(verdict_daly(100, NA), "sans_seuil")
  expect_true(is.na(ref()$niveau3$r$rcei_daly))
})
