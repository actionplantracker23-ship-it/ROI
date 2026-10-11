# Coût-efficacité des innovations · Cost-effectiveness of innovations

Interface bilingue français / anglais (bouton en haut à droite). Bilingual French / English interface.

Outil R Shiny structuré en trois couches, publié sur GitHub Pages avec Shinylive :

| Couche | Rôle | Contenu |
|---|---|---|
| **A** | Investissement, exclu des coûts unitaires | Développement non récurrent ; annualisé (OMS-CHOICE) pour la vue économique |
| **B** | Volumes (dénominateurs) | MAU = B1 × B3 · I = MAU × B4 · E = I × B5 · F = E × B6 ; A (épisodes avec action vérifiée) mesuré séparément |
| **C** | Coût récurrent (numérateur) | R = C1 + … + C9 pour la période, hors A ; statut par ligne (connu, estimé, en attente, hors budget) ; estimation « partielle » si une ligne manque |

**Indicateurs Q5** : OC5a = R / MAU · OC5b = R / I · OC5c = R / A · OC5d = R / F ·
OC5e = (C4 + C5) / bénéficiaires atteints (composante de R, non additive).

**Trois vues de coût** : lignes connues (plancher) · R (Q5) · R + A annualisé (économique).

**Niveau santé** : RCEI = (coût intervention − coût comparateur) / effet incrémental (DALY, décès),
bénéfice monétaire net, ROI. **Sensibilité univariée** sur B3, B4, B5, B6, durée de vie de A et
niveau des coûts C (tornade). Rapport imprimable, export CSV, enregistrement du projet (.json).

Les données de l'innovation (Phase 3, scénario central) sont chargées par défaut ; une variante
remplace les lignes C en attente par des estimations indicatives à confirmer.

Méthode : Drummond et al. (2015), OMS-CHOICE (2003), iDSI Reference Case (2016).

## Structure

```
app/
  app.R      interface Shiny
  calcul.R   moteur : couches A, B, C, indicateurs Q5, santé, sensibilité (sans Shiny)
tests/
  test-calcul.R   tests du moteur (testthat)
  run.R
.github/workflows/deploy.yml   tests + publication sur GitHub Pages
```

## Lancer en local

```r
install.packages(c("shiny", "bslib", "jsonlite", "testthat"))
shiny::runApp("app")
```

Tests : `Rscript tests/run.R` depuis la racine du dépôt.

## Publication

Dans **Settings → Pages → Source**, choisir **GitHub Actions**. Chaque push sur `main`
lance les tests, exporte l'application avec Shinylive et la publie.
