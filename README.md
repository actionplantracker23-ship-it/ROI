# Coût-efficacité des innovations en santé

Outil en ligne (R Shiny) pour évaluer la **coût-efficacité** de n'importe quel projet
d'innovation en santé. Chaque projet décrit librement son contexte et ses activités,
renseigne ses coûts et ses résultats, et obtient :

- le **coût par bénéficiaire** et le **coût par résultat** de chaque activité ;
- le **coût par DALY évitée** du projet et de chaque activité ;
- le **coût net** par DALY évitée (après économies générées) ;
- l'**ICER** par rapport à la pratique actuelle ;
- un **verdict** par rapport au seuil choisi (ou 0,5 × PIB par habitant) ;
- une **analyse de sensibilité** et l'export du tableau (.csv).

Rien n'est figé : pays, monnaie, PIB, seuil, nombre et nature des activités sont saisis
par l'utilisateur. Un projet peut être enregistré (.json) puis rouvert.

L'application tourne entièrement dans le navigateur grâce à
[Shinylive](https://posit-dev.github.io/r-shinylive/) et est publiée sur GitHub Pages.

## Structure

```
app/
  app.R      interface Shiny
  calcul.R   moteur de calcul coût-efficacité (sans dépendance à Shiny)
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
