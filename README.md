# ROI Santé Communautaire

Application R Shiny pour calculer le retour sur investissement des projets de santé
communautaire au **Kenya**, en **Ouganda** et au **Burkina Faso**.

L'application tourne entièrement dans le navigateur grâce à [Shinylive](https://posit-dev.github.io/r-shinylive/) :
elle est publiée sur GitHub Pages, sans serveur Shiny à payer ou à maintenir.

## Ce que fait l'outil

| Onglet | Contenu |
|---|---|
| 1. Projet | Pays, type de projet, durée, taux d'actualisation, taux de change, PIB/habitant, sauvegarde/ouverture d'un projet (.json) |
| 2. Coûts | Jusqu'à 8 postes de coût : investissement (année 1) + coût récurrent annuel |
| 3. Résultats | Jusqu'à 8 résultats de santé : unités produites, valeur par unité, DALY évitées, deadweight, attribution, déplacement, durée et baisse de l'effet |
| 4. Analyse | ROI financier, SROI, coût par DALY évitée, graphique cumulé, analyse de sensibilité, flux annuels (.csv) |
| Méthodologie | Formules et définitions |

Trois types de projets ont des valeurs d'exemple : agents de santé communautaires,
santé maternelle et infantile, prévention / sensibilisation.

> ⚠️ Les valeurs d'exemple (coûts, valeurs par unité, DALY, taux de change, PIB) sont des
> ordres de grandeur indicatifs. Remplacez-les par vos données de projet et des sources
> vérifiées avant toute utilisation dans un rapport.

## Les trois méthodes

- **ROI financier** = (VA des bénéfices monétaires − VA des coûts) / VA des coûts
- **SROI** = (VA des bénéfices monétaires + VA de la valeur sociale) / VA des coûts
- **Coût-efficacité** = VA des coûts / DALY évitées, comparé au PIB par habitant

Unités imputables au projet = unités × (1 − deadweight) × (1 − attribution) × (1 − déplacement).
VA = valeur actuelle ; l'année 1 n'est pas actualisée.

## Structure

```
app/
  app.R       interface Shiny
  calcul.R    moteur de calcul (sans dépendance à Shiny)
  modeles.R   paramètres par pays et valeurs d'exemple
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

## Publier sur GitHub Pages

1. Dans le dépôt GitHub : **Settings → Pages → Build and deployment → Source : GitHub Actions**.
2. Fusionner sur la branche `main`. Le workflow lance les tests, exporte l'application
   avec Shinylive et la publie sur `https://<compte>.github.io/<dépôt>/`.

Le premier chargement prend quelques secondes (téléchargement de R dans le navigateur).
