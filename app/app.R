library(shiny)
library(bslib)

source("calcul.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Mise en forme ----------------------------------------------------------

fmt <- function(x, decimales = 0) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !is.finite(x)) return("—")
  formatC(x, format = "f", digits = decimales, big.mark = " ", decimal.mark = ",")
}
fmt_pct <- function(x) if (is.null(x) || is.na(x)) "—" else paste0(fmt(100 * x), " %")

couleurs <- list(sarcelle = "#0f5257", or = "#c08a2e", vert = "#1d7a4f", rouge = "#a23b2a",
                 gris = "#6c7a80", encre = "#1f2a2e")

css <- "
:root { --encre: #1f2a2e; --sarcelle: #0f5257; --or: #c08a2e; --papier: #faf7f2; }
body { background: var(--papier); }
.navbar { background: #ffffff !important; border-bottom: 1px solid #e8e1d4; }
.navbar-brand { font-weight: 600; letter-spacing: .02em; color: var(--sarcelle) !important; }
.nav-link { font-size: 1.02rem; padding-left: .55rem !important; padding-right: .55rem !important; }
.nav-link.active { color: var(--sarcelle) !important; border-bottom: 2px solid var(--or); }
h1, h2, h3, h4 { color: var(--sarcelle); font-weight: 600; }
h3.section { margin: 2rem 0 1rem; padding-bottom: .4rem; border-bottom: 1px solid #e3d9c6; }
.card { border: none; border-radius: 14px; box-shadow: 0 2px 16px rgba(15, 82, 87, .08); }
.card-header { background: transparent; border-bottom: 1px solid #efe8dc; font-size: 1.15rem;
  font-weight: 600; color: var(--sarcelle); }
.hero { background: linear-gradient(135deg, #0f5257 0%, #1d7874 55%, #2f8f83 80%, #c08a2e 140%);
  color: #fff; border-radius: 20px; padding: 3.5rem 3rem; margin-bottom: 2rem; }
.hero h1 { color: #fff; font-size: 2.9rem; margin-bottom: 1rem; }
.hero p { font-size: 1.3rem; max-width: 48rem; opacity: .95; }
.hero .btn { font-size: 1.1rem; padding: .6rem 1.4rem; border-radius: 30px; margin: .8rem .6rem 0 0; }
.btn-or { background: var(--or); color: #fff; border: none; }
.btn-or:hover { background: #a8761f; color: #fff; }
.btn-blanc { background: transparent; color: #fff; border: 1px solid rgba(255,255,255,.7); }
.btn-blanc:hover { background: rgba(255,255,255,.12); color: #fff; }
.etape { padding: 1.4rem; height: 100%; }
.etape .num { display: inline-flex; width: 2.3rem; height: 2.3rem; border-radius: 50%;
  background: var(--or); color: #fff; align-items: center; justify-content: center;
  font-size: 1.2rem; margin-bottom: .7rem; font-variant-numeric: lining-nums; }
.etape h4 { font-size: 1.3rem; }
.question { padding: 1.2rem 1.4rem; border-left: 4px solid var(--or); }
.question .shiny-options-group { margin-top: .4rem; }
.question .control-label { font-size: 1.15rem; font-weight: 600; color: var(--sarcelle); }
.activite { border-left: 4px solid var(--or); }
.activite .titre-activite input { font-size: 1.2rem; font-weight: 600; color: var(--sarcelle); }
.bloc { background: #f6f1e7; border-radius: 10px; padding: .9rem 1.1rem .2rem; margin-top: .6rem; }
.bloc-titre { font-weight: 600; color: var(--sarcelle); margin-bottom: .5rem; }
.calcule { color: var(--sarcelle); font-weight: 600; margin-bottom: .8rem; }
.kpi { background: #fff !important; border: none !important; border-top: 3px solid var(--or) !important;
  box-shadow: 0 2px 16px rgba(15, 82, 87, .08); }
.kpi .value-box-title { color: #5b6a6e; font-size: 1rem; }
.kpi .value-box-value { color: var(--sarcelle); font-weight: 600; font-size: 1.75rem; white-space: nowrap;
  font-variant-numeric: lining-nums tabular-nums; }
.kpi p { color: #6c7a80; font-size: .95rem; margin: 0; }
.verdict { border-radius: 18px; color: #fff; padding: 2rem 2.4rem; margin-bottom: 1rem; }
.verdict .chiffre { font-size: 2.8rem; font-weight: 600; line-height: 1.1; font-variant-numeric: lining-nums; }
.verdict .unite { font-size: 1.3rem; opacity: .9; }
.verdict .phrase { font-size: 1.35rem; margin-top: .6rem; }
.verdict .detail { opacity: .88; margin-top: .3rem; }
table { font-variant-numeric: lining-nums tabular-nums; }
.table > :not(caption) > * > * { padding: .5rem .7rem; }
.aide { color: #6c7a80; font-size: .98rem; }
.navigation { display: flex; justify-content: space-between; margin: 1rem 0 2rem; }
.rapport { background: #fff; padding: 2.5rem 3rem; border-radius: 14px; box-shadow: 0 2px 16px rgba(15,82,87,.08); }
.rapport h2 { border-bottom: 2px solid var(--or); padding-bottom: .4rem; }
.rapport h4 { margin-top: 1.6rem; }
.formule { font-family: Georgia, serif; background: #f6f1e7; border-radius: 8px; padding: .5rem .9rem;
  display: inline-block; margin: .2rem 0 .6rem; }
@media (max-width: 640px) {
  .navbar-brand { font-size: 1rem; white-space: normal; }
  .hero { padding: 2rem 1.4rem; border-radius: 14px; }
  .hero h1 { font-size: 2rem; }
  .hero p { font-size: 1.1rem; }
  .verdict { padding: 1.4rem; }
  .verdict .chiffre { font-size: 2rem; }
  .rapport { padding: 1.4rem; }
}
@media print {
  .navbar, .no-print, .navigation { display: none !important; }
  body { background: #fff; }
  .rapport { box-shadow: none; padding: 0; }
}
"

# ---- Composants -------------------------------------------------------------

question <- function(id, label, choix, selected, aide = NULL) {
  card(class = "question",
       radioButtons(id, label, choix, selected = selected, width = "100%"),
       if (!is.null(aide)) div(class = "aide", aide))
}

navigation <- function(precedent = NULL, suivant = NULL, label_suivant = "Étape suivante →") {
  div(class = "navigation",
      if (!is.null(precedent)) actionButton(precedent, "← Retour", class = "btn-outline-secondary") else span(),
      if (!is.null(suivant)) actionButton(suivant, label_suivant, class = "btn-primary") else span())
}

si_daly <- "input.effets_sante == 'oui'"

tableau_html <- function(df) {
  tags$table(class = "table table-sm table-striped",
             tags$thead(tags$tr(lapply(names(df), tags$th))),
             tags$tbody(lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(df[i, ], function(x) tags$td(x))))))
}

carte_activite <- function(id, v = NULL) {
  val <- function(col, defaut = 0) if (!is.null(v) && !is.null(v[[col]]) && !is.na(v[[col]])) v[[col]] else defaut
  i <- function(nom) paste0(nom, "_", id)
  nombre <- function(col, label, step = NA) numericInput(i(col), label, val(col), min = 0, step = step)
  div(
    id = paste0("activite_", id), class = "card activite mb-3",
    div(
      class = "card-body",
      div(class = "d-flex gap-3 align-items-start",
          div(class = "flex-grow-1 titre-activite",
              textInput(i("nom"), "Nom de l'activité", val("nom", ""), width = "100%",
                        placeholder = "ex. Dépistage communautaire par test rapide")),
          actionButton(i("suppr"), "Supprimer", class = "btn-outline-danger btn-sm mt-4")),
      layout_column_wrap(
        width = "210px",
        textInput(i("unite"), "Résultat mesuré", val("unite", ""), placeholder = "ex. cas traités"),
        nombre("investissement", "Investissement de départ"),
        nombre("recurrent", "Coût de fonctionnement / an"),
        nombre("beneficiaires", "Bénéficiaires / an"),
        nombre("resultats", "Résultats obtenus / an")
      ),
      conditionalPanel(
        paste(si_daly, "&& input.mode_daly == 'direct'"),
        div(class = "bloc",
            div(class = "bloc-titre", "Effet sur la santé"),
            layout_column_wrap(width = "210px",
                               nombre("daly_direct", "DALY évitées par résultat", step = 0.001)))
      ),
      conditionalPanel(
        paste(si_daly, "&& input.mode_daly == 'calcul'"),
        div(class = "bloc",
            div(class = "bloc-titre", "Effet sur la santé : calcul des DALY (DALY = YLL + YLD)"),
            layout_column_wrap(
              width = "210px",
              nombre("deces", "Décès évités par résultat", step = 0.001),
              nombre("annees_perdues", "Années de vie perdues par décès"),
              nombre("episodes", "Épisodes de maladie évités par résultat", step = 0.01),
              nombre("poids", "Poids d'incapacité (0 à 1)", step = 0.01),
              nombre("duree_episode", "Durée d'un épisode (années)", step = 0.01)
            ),
            div(class = "calcule", textOutput(i("daly_calcule"), inline = TRUE)))
      ),
      conditionalPanel(
        "input.economies == 'oui'",
        div(class = "bloc",
            div(class = "bloc-titre", "Économies générées par résultat"),
            layout_column_wrap(
              width = "210px",
              nombre("eco_sante", "Pour le système de santé"),
              nombre("eco_menages", "Pour les ménages"),
              nombre("productivite", "Gains de productivité")
            ))
      )
    )
  )
}

police <- font_collection("EB Garamond", "Garamond", "Georgia", "serif")

# ---- Interface --------------------------------------------------------------

ui <- page_navbar(
  id = "onglets",
  title = "Évaluation économique",
  fillable = FALSE,
  theme = bs_theme(version = 5, bg = "#faf7f2", fg = "#1f2a2e", primary = "#0f5257",
                   secondary = "#c08a2e", base_font = police, heading_font = police, font_scale = 1.08),
  header = tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=EB+Garamond:ital,wght@0,400;0,500;0,600;0,700;1,400&display=swap"),
    tags$style(HTML(css))
  ),

  nav_panel(
    "Accueil", value = "accueil",
    div(class = "hero",
        h1("Votre projet de santé vaut-il son coût ?"),
        p("Un outil pour tous les projets : décrivez votre projet, répondez à quelques questions, ",
          "renseignez vos activités. L'outil calcule vos coûts, le coût par bénéficiaire, le ratio ",
          "coût-efficacité (ACER et ICER), le bénéfice net, le ROI, et vous dit si le projet est coût-efficace."),
        actionButton("demarrer_vide", "Commencer mon projet", class = "btn-or"),
        actionButton("demarrer_exemple", "Voir un exemple", class = "btn-blanc")),
    layout_column_wrap(
      width = "220px",
      card(div(class = "etape", div(class = "num", "1"), h4("Le projet"),
               p("Pays, monnaie, durée, seuil : tout se saisit librement."))),
      card(div(class = "etape", div(class = "num", "2"), h4("Le profil"),
               p("Quelques questions pour adapter l'analyse à votre projet."))),
      card(div(class = "etape", div(class = "num", "3"), h4("Les activités"),
               p("Coûts, bénéficiaires et résultats de chaque activité."))),
      card(div(class = "etape", div(class = "num", "4"), h4("Les résultats"),
               p("Coûts, coût-efficacité, rendement, incertitude et rapport imprimable.")))
    )
  ),

  nav_panel(
    "1 · Projet", value = "projet",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Identité du projet"),
        textInput("nom", "Nom du projet", "", placeholder = "ex. Diagnostic mobile du paludisme"),
        textInput("organisation", "Organisation", "", placeholder = "ex. ONG, district sanitaire…"),
        layout_column_wrap(
          width = "180px",
          textInput("pays", "Pays / zone", "", placeholder = "ex. Ouganda"),
          textInput("monnaie", "Monnaie", "", placeholder = "ex. XOF, KES, UGX, USD")
        ),
        textAreaInput("description", "Description et sources des données", "", rows = 3,
                      placeholder = "Ce que fait le projet, pour qui ; d'où viennent vos chiffres…")
      ),
      card(
        card_header("Paramètres de l'analyse"),
        layout_column_wrap(
          width = "180px",
          numericInput("annee_debut", "Année de début", as.integer(format(Sys.Date(), "%Y")), min = 1990, max = 2100),
          numericInput("duree", "Durée de l'analyse (années)", 3, min = 1, max = 30),
          numericInput("taux_couts", "Actualisation des coûts (%)", 3, min = 0, max = 20, step = 0.5),
          numericInput("taux_effets", "Actualisation des effets (%)", 3, min = 0, max = 20, step = 0.5),
          numericInput("attribution", "Part de l'effet due au projet (%)", 100, min = 0, max = 100)
        ),
        div(class = "aide mb-3",
            "3 % est le taux de référence de l'OMS et de l'iDSI. La part de l'effet due au projet retire ",
            "ce qui serait arrivé sans lui ou grâce à d'autres acteurs."),
        layout_column_wrap(
          width = "180px",
          numericInput("pib_hab", "PIB par habitant (monnaie du projet)", NA, min = 0),
          numericInput("seuil", "Seuil par DALY évitée", NA, min = 0),
          numericInput("taux_change", "Monnaie du projet pour 1 USD", NA, min = 0)
        ),
        div(class = "aide",
            "Seuil : celui de votre ministère ou bailleur. Sans seuil, l'outil prend 0,5 × PIB par habitant ",
            "comme repère indicatif. Le taux de change (optionnel) sert à afficher les équivalents en USD.")
      )
    ),
    card(
      card_header("Enregistrer ou reprendre un projet"),
      layout_columns(
        col_widths = c(6, 6),
        downloadButton("sauver", "Enregistrer le projet (.json)", class = "btn-outline-primary"),
        fileInput("ouvrir", NULL, accept = ".json", buttonLabel = "Ouvrir un projet…", placeholder = "fichier .json")
      )
    ),
    navigation("retour_accueil", "vers_profil")
  ),

  nav_panel(
    "2 · Profil", value = "profil",
    p(class = "aide", "Vos réponses adaptent les écrans suivants à votre projet."),
    layout_column_wrap(
      width = "420px",
      question("effets_sante", "Votre projet a-t-il des effets sur la santé\u00a0?",
               c("Oui : il évite des décès, des maladies ou des incapacités" = "oui",
                 "Non : je veux seulement analyser les coûts" = "non"), "oui"),
      conditionalPanel(
        si_daly,
        question("mode_daly", "Comment renseigner ces effets en DALY\u00a0?",
                 c("Je connais les DALY évitées par résultat (littérature, étude)" = "direct",
                   "Calculer les DALY à partir des décès et des maladies évités" = "calcul"), "direct",
                 "DALY = années de vie perdues (YLL) + années vécues avec incapacité (YLD).")
      ),
      question("economies", "Votre projet génère-t-il des économies\u00a0?",
               c("Oui : soins évités, dépenses des ménages, productivité" = "oui",
                 "Non / je ne sais pas" = "non"), "non",
               "Permet de calculer le ROI, le ratio bénéfice/coût et la valeur actuelle nette."),
      question("comparer", "Voulez-vous comparer à la pratique actuelle\u00a0?",
               c("Oui : je connais le coût et l'effet de ce qui se fait aujourd'hui" = "oui",
                 "Non : comparer à l'absence d'intervention" = "non"), "non",
               "La comparaison donne l'ICER, l'indicateur de décision recommandé."),
      question("incertitude_on", "Voulez-vous une analyse d'incertitude probabiliste\u00a0?",
               c("Oui : simulation de Monte-Carlo et courbe d'acceptabilité" = "oui",
                 "Non : analyse de sensibilité simple seulement" = "non"), "non")
    ),
    navigation("retour_projet", "vers_activites")
  ),

  nav_panel(
    "3 · Activités", value = "activites",
    card(
      card_header("Activités du projet"),
      p("Ajoutez toutes les activités du projet. Montants dans la monnaie du projet ; ",
        strong("« / an »"), " = chaque année de l'analyse."),
      conditionalPanel(si_daly, div(class = "aide",
        "Une activité de support (formation, coordination) peut avoir 0 DALY : son coût compte quand même dans le total."))
    ),
    div(id = "liste_activites"),
    div(class = "mb-3", actionButton("ajouter", "+ Ajouter une activité", class = "btn-or")),
    navigation("retour_profil", "vers_suite")
  ),

  nav_panel(
    "4 · Comparateur", value = "comparateur",
    card(
      card_header("Pratique actuelle (sans le projet)"),
      p("Ce que coûte et produit aujourd'hui l'alternative à laquelle on compare le projet."),
      layout_column_wrap(
        width = "220px",
        numericInput("comp_investissement", "Investissement", 0, min = 0),
        numericInput("comp_cout", "Coût annuel", 0, min = 0),
        numericInput("comp_daly", "DALY évitées / an", 0, min = 0, step = 0.1)
      ),
      div(class = "aide", "ICER = (coût du projet − coût actuel) / (DALY du projet − DALY actuelles).")
    ),
    navigation("retour_activites", "vers_suite2")
  ),

  nav_panel(
    "5 · Incertitude", value = "incertitude",
    card(
      card_header("Marges d'incertitude"),
      p("De combien vos chiffres pourraient-ils varier\u00a0? Ces marges servent à l'analyse de sensibilité ",
        "et à la simulation de Monte-Carlo."),
      layout_column_wrap(
        width = "220px",
        numericInput("inc_couts", "Coûts (± %)", 20, min = 0, max = 100),
        numericInput("inc_resultats", "Résultats obtenus (± %)", 20, min = 0, max = 100),
        numericInput("inc_daly", "DALY par résultat (± %)", 30, min = 0, max = 100),
        numericInput("inc_economies", "Économies (± %)", 30, min = 0, max = 100),
        selectInput("n_sim", "Nombre de simulations", c(500, 1000, 2000, 5000), 1000)
      )
    ),
    navigation("retour_activites2", "vers_resultats", "Voir les résultats →")
  ),

  nav_panel(
    "Résultats", value = "resultats",
    uiOutput("bandeau"),
    h3(class = "section", "Coûts"),
    uiOutput("kpis_couts"),
    card(card_header("Coûts par année"), tableOutput("table_annees")),
    conditionalPanel(
      si_daly,
      h3(class = "section", "Coût-efficacité"),
      uiOutput("kpis_ce"),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Plan coût-efficacité"), plotOutput("plan", height = "380px")),
        card(card_header("Coût par DALY évitée, par activité"), plotOutput("barres", height = "380px"))
      )
    ),
    conditionalPanel(
      "input.economies == 'oui'",
      h3(class = "section", "Rendement financier"),
      uiOutput("kpis_fin")
    ),
    h3(class = "section", "Activités"),
    card(tableOutput("table_activites"),
         div(class = "no-print", downloadButton("telecharger", "Télécharger (.csv)", class = "btn-outline-primary btn-sm"))),
    conditionalPanel(
      si_daly,
      h3(class = "section", "Incertitude"),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Analyse de sensibilité (tornade)"), plotOutput("tornade", height = "330px"),
             p(class = "aide", "Variation du ratio coût par DALY quand chaque paramètre varie de sa marge.")),
        card(card_header("Courbe d'acceptabilité"),
             conditionalPanel("input.incertitude_on == 'oui'", plotOutput("ceac", height = "330px"),
                              uiOutput("resume_psa")),
             conditionalPanel("input.incertitude_on != 'oui'",
                              p(class = "aide mt-3", "Activez l'analyse probabiliste à l'étape « Profil » pour obtenir ",
                                "la probabilité que le projet soit coût-efficace.")))
      )
    ),
    div(class = "text-end mb-4 no-print", actionButton("vers_rapport", "Voir le rapport →", class = "btn-primary"))
  ),

  nav_panel(
    "Rapport", value = "rapport",
    div(class = "text-end mb-3 no-print",
        tags$button(class = "btn btn-or", onclick = "window.print()", "Imprimer / PDF")),
    div(class = "rapport", uiOutput("rapport"))
  ),

  nav_panel(
    "Méthode et sources", value = "methode",
    card(card_body(
      h3("Analyse coût-efficacité"),
      p("Méthode d'évaluation économique comparative qui met en regard les coûts et les effets sur la santé ",
        "d'au moins deux options, les effets étant mesurés en unités de santé (cas, décès, DALY) ",
        "(Drummond et al., 2015)."),
      h4("Formules utilisées"),
      p(strong("Coût actualisé : "), span(class = "formule", "C = Σ Cₜ / (1 + r)ᵗ⁻¹"),
        " — année 1 non actualisée ; coûts et effets actualisés (3 % par défaut, OMS / iDSI)."),
      p(strong("Coût par bénéficiaire : "), span(class = "formule", "C / nombre de bénéficiaires")),
      p(strong("Ratio coût-efficacité moyen : "), span(class = "formule", "ACER = C / E"),
        " — descriptif, pas un critère de décision."),
      p(strong("Ratio coût-efficacité différentiel : "), span(class = "formule", "ICER = (C₁ − C₀) / (E₁ − E₀)"),
        " — critère de décision (Weinstein & Stason, 1977). Sans comparateur, C₀ = E₀ = 0."),
      p(strong("Bénéfice monétaire net : "), span(class = "formule", "NMB = λ·ΔE − ΔC"),
        " ; ", strong("bénéfice sanitaire net : "), span(class = "formule", "NHB = ΔE − ΔC / λ"),
        " — coût-efficace si NMB > 0 (Stinnett & Mullahy, 1998)."),
      p(strong("DALY : "), span(class = "formule", "DALY = YLL + YLD ; YLL = décès × années perdues ; YLD = épisodes × poids × durée"),
        " (Murray, 1994 ; GBD)."),
      p(strong("Rendement financier : "), span(class = "formule", "ROI = (B − C) / C ; ratio B/C = B / C ; VAN = B − C"),
        " — B = économies actualisées (analyse coût-bénéfice)."),
      h4("Décision"),
      tags$ul(
        tags$li(strong("Dominant"), " : plus efficace et moins cher → à adopter."),
        tags$li(strong("Dominé"), " : moins efficace et plus cher → à rejeter."),
        tags$li(strong("Plus efficace et plus cher"), " : coût-efficace si ICER ≤ seuil λ."),
        tags$li(strong("Moins efficace et moins cher"), " : acceptable si l'économie par DALY perdue dépasse λ.")
      ),
      h4("Seuil"),
      p("Les repères 1–3 × PIB/habitant (Commission Macroéconomie et Santé, 2001) ne sont plus recommandés ",
        "comme règle de décision (Bertram et al., 2016). Des seuils fondés sur le coût d'opportunité, ",
        "environ 0,1–0,5 × PIB/habitant dans les pays à revenu faible et intermédiaire, sont proposés ",
        "(Woods et al., 2016 ; Ochalek et al., 2018). Préférez le seuil de votre ministère ou bailleur."),
      h4("Incertitude"),
      p("Sensibilité univariée (tornade) et analyse probabiliste : chaque marge ± x % est un intervalle à 95 % ",
        "d'un facteur log-normal tiré indépendamment par activité ; courbe d'acceptabilité = P(NMB > 0) ",
        "selon λ (Fenwick et al., 2001 ; Briggs et al., 2006). Le comparateur est tenu fixe."),
      h4("Simplifications"),
      tags$ul(
        tags$li("Les DALY sont comptées l'année où le résultat est obtenu."),
        tags$li("Un seul comparateur ; pas de calcul de dominance étendue entre plusieurs options."),
        tags$li("Les investissements sont imputés en année 1, sans valeur résiduelle.")
      ),
      h4("Références"),
      tags$ol(
        tags$li("Drummond MF, Sculpher MJ, Claxton K, Stoddart GL, Torrance GW. Methods for the Economic Evaluation of Health Care Programmes, 4e éd. Oxford University Press, 2015."),
        tags$li("Tan-Torres Edejer T, et al. Making Choices in Health: WHO Guide to Cost-Effectiveness Analysis. OMS, 2003."),
        tags$li("Wilkinson T, Sculpher MJ, Claxton K, et al. The International Decision Support Initiative Reference Case for Economic Evaluation. Value in Health 2016;19(8):921–928."),
        tags$li("Husereau D, et al. CHEERS 2022 Statement. Value in Health 2022;25(1):3–9."),
        tags$li("Weinstein MC, Stason WB. Foundations of cost-effectiveness analysis for health and medical practices. NEJM 1977;296:716–721."),
        tags$li("Murray CJL. Quantifying the burden of disease: the technical basis for DALYs. Bull World Health Organ 1994;72(3):429–445."),
        tags$li("Stinnett AA, Mullahy J. Net health benefits. Med Decis Making 1998;18(2 Suppl):S68–S80."),
        tags$li("Fenwick E, Claxton K, Sculpher M. Representing uncertainty: the role of CEACs. Health Econ 2001;10:779–787."),
        tags$li("Briggs A, Claxton K, Sculpher M. Decision Modelling for Health Economic Evaluation. Oxford University Press, 2006."),
        tags$li("Bertram MY, et al. Cost-effectiveness thresholds: pros and cons. Bull World Health Organ 2016;94:925–930."),
        tags$li("Woods B, Revill P, Sculpher M, Claxton K. Country-level cost-effectiveness thresholds. Value in Health 2016;19(8):929–935."),
        tags$li("Ochalek J, Lomas J, Claxton K. Estimating health opportunity costs in LMICs. BMJ Global Health 2018;3:e000964."),
        tags$li("Haute Autorité de Santé. Choix méthodologiques pour l'évaluation économique à la HAS, 2020.")
      )
    ))
  )
)

# ---- Serveur ----------------------------------------------------------------

server <- function(input, output, session) {

  num <- function(id, defaut = 0) {
    v <- input[[id]]
    if (is.null(v) || length(v) != 1 || is.na(v)) defaut else as.numeric(v)
  }
  txt <- function(id) trimws(input[[id]] %||% "")
  oui <- function(id) identical(input[[id]], "oui")

  monnaie <- reactive(txt("monnaie"))
  argent <- function(x) if (is.null(x) || is.na(x) || !is.finite(x)) "—" else paste(fmt(x), monnaie())
  en_usd <- function(x) {
    fx <- num("taux_change", NA)
    if (is.na(fx) || fx <= 0 || is.null(x) || is.na(x)) NULL else paste("≈", fmt(x / fx), "USD")
  }

  # -- Activités ajoutées et supprimées librement --

  ids <- reactiveVal(integer(0))
  prochain <- 0L

  champs_activite <- c("investissement", "recurrent", "beneficiaires", "resultats", "daly_direct",
                       "deces", "annees_perdues", "episodes", "poids", "duree_episode",
                       "eco_sante", "eco_menages", "productivite")

  daly_activite <- function(id) {
    v <- function(ch) num(paste0(ch, "_", id))
    if (!oui("effets_sante")) return(0)
    if (identical(input$mode_daly, "calcul")) {
      daly_par_resultat(v("deces"), v("annees_perdues"), v("episodes"), v("poids"), v("duree_episode"))
    } else v("daly_direct")
  }

  ajouter_activite <- function(valeurs = NULL) {
    prochain <<- prochain + 1L
    id <- prochain
    insertUI("#liste_activites", "beforeEnd", carte_activite(id, valeurs), immediate = TRUE)
    ids(c(isolate(ids()), id))
    output[[paste0("daly_calcule_", id)]] <- renderText(
      paste("→", fmt(daly_activite(id), 4), "DALY évitée(s) par résultat"))
    observeEvent(input[[paste0("suppr_", id)]], {
      removeUI(paste0("#activite_", id))
      ids(setdiff(ids(), id))
    }, ignoreInit = TRUE, once = TRUE)
  }

  vider_activites <- function() {
    for (id in isolate(ids())) removeUI(paste0("#activite_", id), immediate = TRUE)
    ids(integer(0))
  }

  remplir_activites <- function(df) {
    vider_activites()
    for (i in seq_len(nrow(df))) ajouter_activite(as.list(df[i, ]))
  }

  # Valeurs brutes saisies (pour l'enregistrement du projet).
  activites_brutes <- reactive({
    i <- ids()
    if (length(i) == 0) return(NULL)
    do.call(rbind, lapply(i, function(id) {
      ligne <- list(nom = txt(paste0("nom_", id)), unite = txt(paste0("unite_", id)))
      for (ch in champs_activite) ligne[[ch]] <- num(paste0(ch, "_", id))
      as.data.frame(ligne, stringsAsFactors = FALSE)
    }))
  })

  # Activités prêtes pour le calcul, selon le profil.
  activites <- reactive({
    i <- ids()
    if (length(i) == 0) return(NULL)
    eco <- oui("economies")
    do.call(rbind, lapply(seq_along(i), function(k) {
      id <- i[k]
      v <- function(ch) num(paste0(ch, "_", id))
      nom <- txt(paste0("nom_", id)); unite <- txt(paste0("unite_", id))
      data.frame(
        nom = if (nzchar(nom)) nom else paste("Activité", k),
        unite = if (nzchar(unite)) unite else "résultats",
        investissement = v("investissement"), recurrent = v("recurrent"),
        beneficiaires = v("beneficiaires"), resultats = v("resultats"),
        daly_par_resultat = daly_activite(id),
        eco_sante = if (eco) v("eco_sante") else 0,
        eco_menages = if (eco) v("eco_menages") else 0,
        productivite = if (eco) v("productivite") else 0,
        stringsAsFactors = FALSE
      )
    }))
  })

  seuil <- reactive(seuil_effectif(num("seuil", NA), num("pib_hab", NA)))

  parametres <- reactive(list(
    annee_debut = num("annee_debut", 2026),
    duree = max(1, round(num("duree", 1))),
    taux_couts = num("taux_couts") / 100,
    taux_effets = num("taux_effets") / 100,
    attribution = num("attribution", 100) / 100,
    seuil = seuil()$valeur
  ))

  comparateur <- reactive({
    if (!oui("comparer")) return(NULL)
    list(investissement = num("comp_investissement"), cout_annuel = num("comp_cout"),
         daly_annuels = num("comp_daly"))
  })

  incertitude <- reactive(list(
    couts = num("inc_couts", 20) / 100, resultats = num("inc_resultats", 20) / 100,
    daly = num("inc_daly", 30) / 100, economies = num("inc_economies", 30) / 100
  ))

  res <- reactive({
    a <- activites()
    if (is.null(a)) NULL else evaluer(parametres(), a, comparateur())
  })

  sim <- reactive({
    a <- activites()
    req(a, oui("incertitude_on"), oui("effets_sante"))
    probabiliste(parametres(), a, comparateur(), incertitude(), n_sim = as.integer(input$n_sim %||% 1000))
  })

  # -- Navigation et étapes optionnelles --

  observe({
    if (oui("comparer")) nav_show("onglets", "comparateur") else nav_hide("onglets", "comparateur")
    if (oui("incertitude_on")) nav_show("onglets", "incertitude") else nav_hide("onglets", "incertitude")
  })

  aller <- function(onglet) nav_select("onglets", onglet)
  apres_activites <- function() if (oui("comparer")) "comparateur" else if (oui("incertitude_on")) "incertitude" else "resultats"
  observeEvent(input$retour_accueil, aller("accueil"))
  observeEvent(input$vers_profil, aller("profil"))
  observeEvent(input$retour_projet, aller("projet"))
  observeEvent(input$vers_activites, aller("activites"))
  observeEvent(input$retour_profil, aller("profil"))
  observeEvent(input$vers_suite, aller(apres_activites()))
  observeEvent(input$retour_activites, aller("activites"))
  observeEvent(input$vers_suite2, aller(if (oui("incertitude_on")) "incertitude" else "resultats"))
  observeEvent(input$retour_activites2, aller(if (oui("comparer")) "comparateur" else "activites"))
  observeEvent(input$vers_resultats, aller("resultats"))
  observeEvent(input$vers_rapport, aller("rapport"))
  observeEvent(input$ajouter, ajouter_activite())

  # -- Projet vide, exemple, enregistrement --

  champs_texte <- c("nom", "organisation", "pays", "monnaie", "description")
  champs_nombre <- c("annee_debut", "duree", "taux_couts", "taux_effets", "attribution", "pib_hab", "seuil",
                     "taux_change", "comp_investissement", "comp_cout", "comp_daly",
                     "inc_couts", "inc_resultats", "inc_daly", "inc_economies")
  champs_profil <- c("effets_sante", "mode_daly", "economies", "comparer", "incertitude_on")

  appliquer <- function(textes = list(), nombres = list(), profil = list(), acts = NULL) {
    for (id in champs_texte) {
      v <- textes[[id]] %||% ""
      if (id == "description") updateTextAreaInput(session, id, value = v) else updateTextInput(session, id, value = v)
    }
    for (id in names(nombres)) updateNumericInput(session, id, value = nombres[[id]] %||% NA)
    for (id in names(profil)) updateRadioButtons(session, id, selected = profil[[id]])
    if (!is.null(acts)) remplir_activites(acts)
  }

  exemple <- function() {
    a <- exemple_activites()
    names(a)[names(a) == "daly_par_resultat"] <- "daly_direct"
    appliquer(
      textes = list(nom = "Diagnostic mobile du paludisme (exemple fictif)", organisation = "Projet de démonstration",
                    pays = "Burkina Faso", monnaie = "XOF",
                    description = "Exemple fictif : des agents communautaires dépistent et traitent le paludisme à domicile, avec un suivi des femmes enceintes par SMS."),
      nombres = list(duree = 3, taux_couts = 3, taux_effets = 3, attribution = 80, pib_hab = 500000,
                     seuil = NA, taux_change = 600, comp_investissement = 0, comp_cout = 4000000, comp_daly = 40),
      profil = list(effets_sante = "oui", mode_daly = "direct", economies = "oui", comparer = "oui",
                    incertitude_on = "oui"),
      acts = a
    )
  }

  exemple()

  observeEvent(input$demarrer_exemple, { exemple(); aller("resultats") })

  observeEvent(input$demarrer_vide, {
    appliquer(
      nombres = list(duree = 3, taux_couts = 3, taux_effets = 3, attribution = 100, pib_hab = NA, seuil = NA,
                     taux_change = NA, comp_investissement = 0, comp_cout = 0, comp_daly = 0),
      profil = list(effets_sante = "oui", mode_daly = "direct", economies = "non", comparer = "non",
                    incertitude_on = "non")
    )
    vider_activites()
    ajouter_activite()
    aller("projet")
  })

  output$sauver <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9_-]+", "_", if (nzchar(txt("nom"))) txt("nom") else "projet"), ".json"),
    content = function(file) {
      projet <- list(
        version = 2,
        textes = lapply(setNames(champs_texte, champs_texte), txt),
        nombres = lapply(setNames(champs_nombre, champs_nombre), function(id) num(id, NA)),
        profil = lapply(setNames(champs_profil, champs_profil), function(id) input[[id]]),
        activites = activites_brutes()
      )
      writeLines(jsonlite::toJSON(projet, auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "null"), file)
    }
  )

  observeEvent(input$ouvrir, {
    projet <- tryCatch(jsonlite::fromJSON(input$ouvrir$datapath), error = function(e) NULL)
    if (is.null(projet) || !is.data.frame(projet$activites)) {
      showNotification("Fichier de projet invalide.", type = "error")
      return()
    }
    appliquer(projet$textes %||% list(), projet$nombres %||% list(), projet$profil %||% list(), projet$activites)
    showNotification("Projet ouvert.", type = "message")
  })

  # -- Résultats --

  icer_texte <- function(ce) switch(ce$quadrant, dominante = "Dominant", dominee = "Dominé", argent(ce$icer))

  kpi <- function(titre, valeur, detail = NULL) {
    value_box(title = titre, value = valeur, if (!is.null(detail)) p(detail), class = "kpi")
  }

  texte_quadrant <- function(q, alt) switch(q,
    dominante = paste0("Dominant : plus efficace et moins coûteux que ", alt, "."),
    dominee = paste0("Dominé : moins efficace et plus coûteux que ", alt, "."),
    nord_est = paste0("Plus efficace et plus coûteux que ", alt, " : l'ICER est comparé au seuil."),
    sud_ouest = paste0("Moins efficace et moins coûteux que ", alt, "."))

  output$bandeau <- renderUI({
    r <- res()
    fond <- function(c) paste0("background:", c)
    if (is.null(r)) {
      return(div(class = "verdict", style = fond(couleurs$gris),
                 div(class = "phrase", "Ajoutez au moins une activité pour voir les résultats.")))
    }
    titre <- div(class = "detail", if (nzchar(txt("nom"))) txt("nom") else "Votre projet",
                 if (nzchar(txt("pays"))) paste(" ·", txt("pays")))
    if (!oui("effets_sante")) {
      return(div(class = "verdict", style = fond(couleurs$sarcelle), titre,
                 div(class = "chiffre", argent(r$couts$cout_par_beneficiaire), span(class = "unite", " par bénéficiaire")),
                 div(class = "phrase", paste("Coût total actualisé :", argent(r$couts$total))),
                 div(class = "detail", "Analyse des coûts uniquement (pas d'effets de santé renseignés).")))
    }
    ce <- r$ce
    s <- seuil()
    alt <- if (oui("comparer")) "la pratique actuelle" else "l'absence d'intervention"
    if (ce$quadrant %in% c("dominante", "dominee")) {
      chiffre <- if (ce$quadrant == "dominante") "Dominant" else "Dominé"
      unite <- ""
    } else {
      chiffre <- argent(ce$icer)
      unite <- if (ce$quadrant == "sud_ouest") " économisés par DALY perdue" else " par DALY évitée (ICER)"
    }
    phrase <- if (is.na(ce$cout_efficace)) {
      "Renseignez un seuil ou le PIB par habitant pour conclure."
    } else if (ce$cout_efficace) "Le projet est coût-efficace." else "Le projet n'est pas coût-efficace au seuil retenu."
    couleur <- if (is.na(ce$cout_efficace)) couleurs$gris else if (ce$cout_efficace) couleurs$vert else couleurs$rouge
    seuil_txt <- if (!is.na(s$valeur)) paste0(
      "Seuil : ", argent(s$valeur), " par DALY évitée",
      if (s$source == "pib") " (repère indicatif : 0,5 × PIB par habitant)" else "",
      " · bénéfice monétaire net : ", argent(ce$nmb), ".")
    div(class = "verdict", style = fond(couleur), titre,
        div(class = "chiffre", chiffre, span(class = "unite", unite)),
        div(class = "phrase", phrase),
        div(class = "detail", texte_quadrant(ce$quadrant, alt)),
        if (!is.null(seuil_txt)) div(class = "detail", seuil_txt))
  })

  output$kpis_couts <- renderUI({
    r <- res(); req(r)
    k <- r$couts
    layout_column_wrap(
      width = "250px", class = "mb-3",
      kpi("Coût total (non actualisé)", argent(k$total_nominal), en_usd(k$total_nominal)),
      kpi("Coût total actualisé", argent(k$total), en_usd(k$total)),
      kpi("Investissement", argent(k$investissement), fmt_pct(ratio(k$investissement, k$total))),
      kpi("Fonctionnement (actualisé)", argent(k$fonctionnement), fmt_pct(ratio(k$fonctionnement, k$total))),
      kpi("Bénéficiaires", fmt(k$beneficiaires), paste(fmt(k$beneficiaires_an), "par an")),
      kpi("Coût par bénéficiaire", argent(k$cout_par_beneficiaire), en_usd(k$cout_par_beneficiaire)),
      kpi("Coût par bénéficiaire et par an", argent(k$cout_par_beneficiaire_an), en_usd(k$cout_par_beneficiaire_an))
    )
  })

  output$table_annees <- renderTable({
    r <- res(); req(r)
    f <- r$flux
    t <- data.frame(`Année` = as.character(f$annee), Investissement = vapply(f$investissement, fmt, ""),
                    Fonctionnement = vapply(f$fonctionnement, fmt, ""), `Coût total` = vapply(f$couts, fmt, ""),
                    `Coût actualisé` = vapply(f$couts_actualises, fmt, ""),
                    `Bénéficiaires` = vapply(f$beneficiaires, fmt, ""), check.names = FALSE)
    if (oui("effets_sante")) t$`DALY évitées` <- vapply(f$daly, fmt, "", 1)
    t
  }, striped = TRUE, width = "100%")

  output$kpis_ce <- renderUI({
    r <- res(); req(r)
    ce <- r$ce
    layout_column_wrap(
      width = "250px", class = "mb-3",
      kpi("DALY évitées (actualisées)", fmt(r$daly, 1), "années de vie en bonne santé"),
      kpi("ACER : coût par DALY évitée", argent(ce$acer), en_usd(ce$acer)),
      kpi("ACER net", argent(ce$acer_net), "après économies générées"),
      kpi("Surcoût (ΔC)", argent(ce$delta_cout), if (oui("comparer")) "vs pratique actuelle" else "vs aucune intervention"),
      kpi("DALY gagnées (ΔE)", fmt(ce$delta_daly, 1), if (oui("comparer")) "vs pratique actuelle" else "vs aucune intervention"),
      kpi("ICER", icer_texte(ce),
          "coût par DALY évitée en plus"),
      kpi("Bénéfice monétaire net (NMB)", argent(ce$nmb), "λ·ΔE − ΔC ; > 0 = coût-efficace"),
      kpi("Bénéfice sanitaire net (NHB)", paste(fmt(ce$nhb, 1), "DALY"), "ΔE − ΔC / λ")
    )
  })

  output$kpis_fin <- renderUI({
    r <- res(); req(r)
    f <- r$financier; b <- r$benefices
    layout_column_wrap(
      width = "250px", class = "mb-3",
      kpi("Économies totales (actualisées)", argent(b$total),
          paste0("santé ", fmt(b$eco_sante), " · ménages ", fmt(b$eco_menages), " · productivité ", fmt(b$productivite))),
      kpi("Valeur actuelle nette", argent(f$van), en_usd(f$van)),
      kpi("ROI", fmt_pct(f$roi), "(économies − coûts) / coûts"),
      kpi("Ratio bénéfice / coût", fmt(f$ratio_bc, 2), "1 unité investie rapporte…"),
      kpi("Année de retour sur investissement", if (is.na(f$annee_retour)) "Non atteinte" else f$annee_retour)
    )
  })

  # Plan coût-efficacité : ΔE en abscisse, ΔC en ordonnée.
  output$plan <- renderPlot({
    r <- res(); req(r)
    ce <- r$ce
    nuage <- if (oui("incertitude_on")) tryCatch(sim(), error = function(e) NULL) else NULL
    xs <- c(0, ce$delta_daly, nuage$delta_daly); ys <- c(0, ce$delta_cout, nuage$delta_cout)
    ex <- max(abs(xs)) * 1.15; ey <- max(abs(ys)) * 1.15
    if (ex == 0) ex <- 1
    if (ey == 0) ey <- 1
    par(family = "serif", mar = c(4.5, 6.5, 1, 1), las = 1, bg = "#ffffff")
    plot(NA, xlim = c(-ex, ex), ylim = c(-ey, ey), xlab = "DALY évitées en plus (ΔE)", ylab = "",
         axes = FALSE)
    axis(1, col = "#cfc6b6", col.axis = "#5b6a6e")
    axis(2, at = pretty(c(-ey, ey)), labels = vapply(pretty(c(-ey, ey)), fmt, ""), col = "#cfc6b6",
         col.axis = "#5b6a6e", cex.axis = 0.8)
    mtext(paste("Surcoût (ΔC)", monnaie()), side = 2, line = 5.3, las = 0, col = "#5b6a6e")
    abline(h = 0, v = 0, col = "#cfc6b6")
    s <- seuil()$valeur
    if (!is.na(s)) {
      abline(a = 0, b = s, lty = 2, col = couleurs$or, lwd = 2)
      legend("topleft", bty = "n", legend = "seuil λ", lty = 2, lwd = 2, col = couleurs$or)
    }
    if (!is.null(nuage)) points(nuage$delta_daly, nuage$delta_cout, pch = 16, cex = 0.5,
                                col = adjustcolor(couleurs$sarcelle, 0.18))
    points(ce$delta_daly, ce$delta_cout, pch = 21, bg = couleurs$or, col = couleurs$encre, cex = 2.2)
    text(c(ex, -ex, ex, -ex) * 0.62, c(-ey, ey, ey, -ey) * 0.9,
         c("dominant", "dominé", "plus efficace, plus cher", "moins efficace, moins cher"),
         col = "#9aa6a9", cex = 0.9)
  })

  output$barres <- renderPlot({
    r <- res(); req(r)
    d <- r$activites[!is.na(r$activites$cout_par_daly), ]
    par(family = "serif", bg = "#ffffff", las = 1)
    if (nrow(d) == 0) {
      par(mar = c(1, 1, 1, 1)); plot.new()
      text(0.5, 0.5, "Aucune activité avec des DALY évitées.", col = couleurs$gris, cex = 1.2)
      return()
    }
    d <- d[order(d$cout_par_daly, decreasing = TRUE), ]
    noms <- ifelse(nchar(d$nom) > 34, paste0(substr(d$nom, 1, 32), "…"), d$nom)
    par(mar = c(4, min(22, 3 + max(nchar(noms)) * 0.5), 1, 2))
    s <- seuil()$valeur
    xmax <- max(c(d$cout_par_daly, s), 1, na.rm = TRUE) * 1.2
    col <- if (is.na(s)) rep(couleurs$sarcelle, nrow(d)) else ifelse(d$cout_par_daly <= s, couleurs$vert, couleurs$rouge)
    y <- barplot(d$cout_par_daly, horiz = TRUE, names.arg = noms, col = col, border = NA,
                 xlim = c(0, xmax), axes = FALSE, space = 0.6, cex.names = 1)
    axis(1, at = pretty(c(0, xmax)), labels = vapply(pretty(c(0, xmax)), fmt, ""), col = "#cfc6b6",
         col.axis = "#5b6a6e", cex.axis = 0.85)
    text(d$cout_par_daly, y, vapply(d$cout_par_daly, fmt, ""), pos = 4, cex = 0.9)
    if (!is.na(s)) abline(v = s, lty = 2, col = couleurs$or, lwd = 2)
    mtext(paste("Coût par DALY évitée", monnaie()), side = 1, line = 2.5, col = "#5b6a6e")
  })

  table_activites <- reactive({
    r <- res(); req(r)
    d <- r$activites
    t <- data.frame(
      `Activité` = d$nom,
      `Coût actualisé` = vapply(d$cout, fmt, ""),
      `Part du coût` = vapply(d$part_cout, fmt_pct, ""),
      `Bénéficiaires` = vapply(d$beneficiaires, fmt, ""),
      `Coût / bénéficiaire` = vapply(d$cout_par_beneficiaire, fmt, ""),
      `Coût par résultat` = ifelse(is.na(d$cout_par_resultat), "—",
                                   paste(vapply(d$cout_par_resultat, fmt, ""), "par", d$unite)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    if (oui("effets_sante")) {
      t$`DALY évitées` <- vapply(d$daly, fmt, "", 1)
      t$`Coût / DALY` <- ifelse(is.na(d$cout_par_daly), "support", vapply(d$cout_par_daly, fmt, ""))
    }
    if (oui("economies")) t$`Économies` <- vapply(d$economies, fmt, "")
    t
  })

  output$table_activites <- renderTable(table_activites(), striped = TRUE, hover = TRUE, width = "100%")

  output$telecharger <- downloadHandler(
    filename = function() "activites_evaluation.csv",
    content = function(file) write.csv(table_activites(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  output$tornade <- renderPlot({
    a <- activites(); req(a, oui("effets_sante"))
    t <- tornade(parametres(), a, comparateur(), incertitude())
    t <- t[is.finite(t$bas) & is.finite(t$haut), ]
    par(family = "serif", bg = "#ffffff", las = 1)
    if (nrow(t) == 0 || !is.finite(t$base[1])) {
      par(mar = c(1, 1, 1, 1)); plot.new()
      text(0.5, 0.5, "Pas assez de données (DALY nulles).", col = couleurs$gris, cex = 1.2)
      return()
    }
    base <- t$base[1]
    bornes <- range(c(t$bas, t$haut, base))
    marge <- diff(bornes) * 0.15 + 1
    par(mar = c(4, 14, 1, 2))
    plot(NA, xlim = c(bornes[1] - marge, bornes[2] + marge), ylim = c(0.5, nrow(t) + 0.5),
         axes = FALSE, xlab = "", ylab = "")
    for (k in seq_len(nrow(t))) {
      rect(min(t$bas[k], base), k - 0.32, max(t$bas[k], base), k + 0.32, col = couleurs$sarcelle, border = NA)
      rect(min(t$haut[k], base), k - 0.32, max(t$haut[k], base), k + 0.32, col = couleurs$or, border = NA)
    }
    abline(v = base, col = couleurs$encre)
    axis(2, at = seq_len(nrow(t)), labels = t$parametre, tick = FALSE, cex.axis = 0.95)
    ticks <- pretty(c(bornes[1] - marge, bornes[2] + marge))
    axis(1, at = ticks, labels = vapply(ticks, fmt, ""), col = "#cfc6b6", col.axis = "#5b6a6e", cex.axis = 0.8)
    mtext(paste("Coût par DALY", monnaie()), side = 1, line = 2.5, col = "#5b6a6e")
    legend("bottomright", bty = "n", fill = c(couleurs$sarcelle, couleurs$or), border = NA,
           legend = c("valeur basse", "valeur haute"), cex = 0.9)
  })

  output$ceac <- renderPlot({
    s <- sim()
    seuil_v <- seuil()$valeur
    ref <- max(c(seuil_v, abs(res()$ce$icer), 1), na.rm = TRUE)
    lambdas <- seq(0, 3 * ref, length.out = 120)
    pr <- acceptabilite(s, lambdas)
    par(family = "serif", bg = "#ffffff", las = 1, mar = c(4.5, 4.5, 1, 1))
    plot(lambdas, pr, type = "l", lwd = 3, col = couleurs$sarcelle, ylim = c(0, 1), axes = FALSE,
         xlab = paste("Seuil λ par DALY évitée", monnaie()), ylab = "Probabilité d'être coût-efficace")
    ticks <- pretty(lambdas)
    axis(1, at = ticks, labels = vapply(ticks, fmt, ""), col = "#cfc6b6", col.axis = "#5b6a6e", cex.axis = 0.8)
    axis(2, at = seq(0, 1, 0.25), labels = paste0(seq(0, 100, 25), " %"), col = "#cfc6b6", col.axis = "#5b6a6e")
    abline(h = 0.5, col = "#e3d9c6")
    if (!is.na(seuil_v)) abline(v = seuil_v, lty = 2, col = couleurs$or, lwd = 2)
  })

  output$resume_psa <- renderUI({
    s <- sim()
    seuil_v <- seuil()$valeur
    ic_c <- intervalle(s$delta_cout); ic_e <- intervalle(s$delta_daly)
    p_ce <- if (!is.na(seuil_v)) acceptabilite(s, seuil_v) else NA
    nmb <- if (!is.na(seuil_v)) intervalle(seuil_v * s$delta_daly - s$delta_cout) else c(NA, NA)
    tags$ul(class = "aide mt-2",
      tags$li(strong("Probabilité d'être coût-efficace au seuil : "), fmt_pct(p_ce)),
      tags$li("Surcoût, IC 95 % : ", fmt(ic_c[1]), " à ", fmt(ic_c[2]), " ", monnaie()),
      tags$li("DALY gagnées, IC 95 % : ", fmt(ic_e[1], 1), " à ", fmt(ic_e[2], 1)),
      tags$li("Bénéfice monétaire net, IC 95 % : ", fmt(nmb[1]), " à ", fmt(nmb[2]), " ", monnaie()),
      tags$li(nrow(s), " simulations"))
  })

  # -- Rapport imprimable --

  output$rapport <- renderUI({
    r <- res()
    if (is.null(r)) return(p("Ajoutez des activités pour générer le rapport."))
    ce <- r$ce; k <- r$couts; f <- r$financier
    s <- seuil()
    ligne <- function(label, valeur) tags$tr(tags$td(label), tags$td(strong(valeur)))
    tagList(
      h2(if (nzchar(txt("nom"))) txt("nom") else "Évaluation économique du projet"),
      p(class = "aide", paste(c(txt("organisation"), txt("pays"), format(Sys.Date(), "%d/%m/%Y")), collapse = " · ")),
      if (nzchar(txt("description"))) p(txt("description")),
      h4("Paramètres"),
      tags$table(class = "table table-sm",
        ligne("Durée de l'analyse", paste(parametres()$duree, "an(s) à partir de", parametres()$annee_debut)),
        ligne("Actualisation coûts / effets", paste0(fmt(num("taux_couts"), 1), " % / ", fmt(num("taux_effets"), 1), " %")),
        ligne("Part de l'effet due au projet", paste0(fmt(num("attribution")), " %")),
        ligne("Comparateur", if (oui("comparer")) "pratique actuelle" else "aucune intervention"),
        ligne("Seuil par DALY évitée", if (is.na(s$valeur)) "non renseigné" else
          paste(argent(s$valeur), if (s$source == "pib") "(0,5 × PIB par habitant, repère indicatif)" else ""))),
      h4("Coûts"),
      tags$table(class = "table table-sm",
        ligne("Coût total (non actualisé)", argent(k$total_nominal)),
        ligne("Coût total actualisé", argent(k$total)),
        ligne("Bénéficiaires", fmt(k$beneficiaires)),
        ligne("Coût par bénéficiaire", argent(k$cout_par_beneficiaire))),
      if (oui("effets_sante")) tagList(
        h4("Coût-efficacité"),
        tags$table(class = "table table-sm",
          ligne("DALY évitées (actualisées)", fmt(r$daly, 1)),
          ligne("ACER (coût par DALY évitée)", argent(ce$acer)),
          ligne("ICER", icer_texte(ce)),
          ligne("Bénéfice monétaire net", argent(ce$nmb)),
          ligne("Conclusion", if (is.na(ce$cout_efficace)) "seuil non renseigné" else
            if (ce$cout_efficace) "coût-efficace" else "non coût-efficace au seuil retenu"),
          if (oui("incertitude_on")) ligne("Probabilité d'être coût-efficace",
            tryCatch(fmt_pct(acceptabilite(sim(), s$valeur)), error = function(e) "—")))),
      if (oui("economies")) tagList(
        h4("Rendement financier"),
        tags$table(class = "table table-sm",
          ligne("Économies actualisées", argent(r$benefices$total)),
          ligne("Valeur actuelle nette", argent(f$van)),
          ligne("ROI", fmt_pct(f$roi)),
          ligne("Ratio bénéfice / coût", fmt(f$ratio_bc, 2)),
          ligne("Année de retour", if (is.na(f$annee_retour)) "non atteinte" else f$annee_retour))),
      h4("Activités"),
      tableau_html(table_activites()),
      p(class = "aide mt-4", "Méthode : Drummond et al. (2015), guide OMS-CHOICE (2003), iDSI Reference Case (2016). ",
        "Rapport généré avec l'outil d'évaluation économique des projets de santé.")
    )
  })
}

shinyApp(ui, server)
