library(shiny)
library(bslib)

source("calcul.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Langues ----------------------------------------------------------------
#
# Les textes fixes de l'interface existent dans les deux langues : tr() crée deux
# <span>, l'un affiché en français, l'autre en anglais, selon la classe du <body>.
# Le bouton de langue bascule cette classe et transmet la langue au serveur
# (input$langue), qui traduit les résultats, graphiques et rapports avec L().

tr <- function(fr, en) tagList(span(class = "t-fr", fr), span(class = "t-en", en))

# Textes indicatifs des champs, par expression régulière sur l'identifiant.
placeholders <- list(
  "^nom$" = c("ex. Diagnostic mobile du paludisme", "e.g. Mobile malaria diagnosis"),
  "^organisation$" = c("ex. ONG, district sanitaire…", "e.g. NGO, health district…"),
  "^pays$" = c("ex. Ouganda", "e.g. Uganda"),
  "^monnaie$" = c("ex. XOF, KES, UGX, USD", "e.g. XOF, KES, UGX, USD"),
  "^description$" = c("Ce que fait le projet, pour qui ; d'où viennent vos chiffres…",
                      "What the project does, for whom; where your figures come from…"),
  "^nom_[0-9]+$" = c("ex. Dépistage communautaire par test rapide", "e.g. Community rapid testing"),
  "^unite_[0-9]+$" = c("ex. cas traités", "e.g. cases treated")
)

js <- sprintf("
var placeholders = %s;
function langueCourante() { return document.body.classList.contains('lang-en') ? 'en' : 'fr'; }
function appliquerLangue(l) {
  document.body.classList.toggle('lang-en', l === 'en');
  document.documentElement.lang = l;
  document.querySelectorAll('input[id], textarea[id]').forEach(function (el) {
    for (var motif in placeholders) {
      if (new RegExp(motif).test(el.id)) el.placeholder = placeholders[motif][l === 'en' ? 1 : 0];
    }
  });
  if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('langue', l);
}
function basculerLangue() { appliquerLangue(langueCourante() === 'en' ? 'fr' : 'en'); }
$(document).on('shiny:connected', function () { appliquerLangue(langueCourante()); });
", jsonlite::toJSON(placeholders))

# ---- Style ------------------------------------------------------------------

couleurs <- list(sarcelle = "#0f5257", or = "#c08a2e", vert = "#1d7a4f", rouge = "#a23b2a",
                 gris = "#6c7a80", encre = "#1f2a2e")

css <- "
body.lang-en .t-fr, body:not(.lang-en) .t-en { display: none !important; }
:root { --encre: #1f2a2e; --sarcelle: #0f5257; --or: #c08a2e; --papier: #faf7f2; }
body { background: var(--papier); }
.navbar { background: #ffffff !important; border-bottom: 1px solid #e8e1d4; }
.navbar-brand { font-weight: 600; letter-spacing: .02em; color: var(--sarcelle) !important; }
.nav-link { font-size: 1.02rem; padding-left: .55rem !important; padding-right: .55rem !important; }
.nav-link.active { color: var(--sarcelle) !important; border-bottom: 2px solid var(--or); }
#bouton_langue { border-radius: 20px; padding: .2rem .9rem; margin-left: .5rem; }
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

question <- function(id, label, noms, valeurs, selected, aide = NULL) {
  card(class = "question",
       radioButtons(id, label, choiceNames = noms, choiceValues = valeurs, selected = selected, width = "100%"),
       if (!is.null(aide)) div(class = "aide", aide))
}

navigation <- function(precedent = NULL, suivant = NULL, label_suivant = tr("Étape suivante →", "Next step →")) {
  div(class = "navigation",
      if (!is.null(precedent)) actionButton(precedent, tr("← Retour", "← Back"), class = "btn-outline-secondary") else span(),
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
              textInput(i("nom"), tr("Nom de l'activité", "Activity name"), val("nom", ""), width = "100%")),
          actionButton(i("suppr"), tr("Supprimer", "Delete"), class = "btn-outline-danger btn-sm mt-4")),
      layout_column_wrap(
        width = "210px",
        textInput(i("unite"), tr("Résultat mesuré", "Output measured"), val("unite", "")),
        nombre("investissement", tr("Investissement de départ", "Initial investment")),
        nombre("recurrent", tr("Coût de fonctionnement / an", "Running cost / year")),
        nombre("beneficiaires", tr("Bénéficiaires / an", "Beneficiaries / year")),
        nombre("resultats", tr("Résultats obtenus / an", "Outputs achieved / year"))
      ),
      conditionalPanel(
        paste(si_daly, "&& input.mode_daly == 'direct'"),
        div(class = "bloc",
            div(class = "bloc-titre", tr("Effet sur la santé", "Health effect")),
            layout_column_wrap(width = "210px",
                               nombre("daly_direct", tr("DALY évitées par résultat", "DALYs averted per output"),
                                      step = 0.001)))
      ),
      conditionalPanel(
        paste(si_daly, "&& input.mode_daly == 'calcul'"),
        div(class = "bloc",
            div(class = "bloc-titre", tr("Effet sur la santé : calcul des DALY (DALY = YLL + YLD)",
                                         "Health effect: DALY calculation (DALY = YLL + YLD)")),
            layout_column_wrap(
              width = "210px",
              nombre("deces", tr("Décès évités par résultat", "Deaths averted per output"), step = 0.001),
              nombre("annees_perdues", tr("Années de vie perdues par décès", "Years of life lost per death")),
              nombre("episodes", tr("Épisodes de maladie évités par résultat", "Illness episodes averted per output"),
                     step = 0.01),
              nombre("poids", tr("Poids d'incapacité (0 à 1)", "Disability weight (0 to 1)"), step = 0.01),
              nombre("duree_episode", tr("Durée d'un épisode (années)", "Episode duration (years)"), step = 0.01)
            ),
            div(class = "calcule", textOutput(i("daly_calcule"), inline = TRUE)))
      ),
      conditionalPanel(
        "input.economies == 'oui'",
        div(class = "bloc",
            div(class = "bloc-titre", tr("Économies générées par résultat", "Savings generated per output")),
            layout_column_wrap(
              width = "210px",
              nombre("eco_sante", tr("Pour le système de santé", "For the health system")),
              nombre("eco_menages", tr("Pour les ménages", "For households")),
              nombre("productivite", tr("Gains de productivité", "Productivity gains"))
            ))
      )
    ),
    tags$script("appliquerLangue(langueCourante());")
  )
}

police <- font_collection("EB Garamond", "Garamond", "Georgia", "serif")

# ---- Méthode et sources (contenu bilingue) -----------------------------------

references <- tags$ol(
  tags$li("Drummond MF, Sculpher MJ, Claxton K, Stoddart GL, Torrance GW. Methods for the Economic Evaluation of Health Care Programmes, 4th ed. Oxford University Press, 2015."),
  tags$li("Tan-Torres Edejer T, et al. Making Choices in Health: WHO Guide to Cost-Effectiveness Analysis. WHO, 2003."),
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

formule <- function(x) span(class = "formule", x)

methode_fr <- div(
  class = "t-fr",
  h3("Analyse coût-efficacité"),
  p("Méthode d'évaluation économique comparative qui met en regard les coûts et les effets sur la santé ",
    "d'au moins deux options, les effets étant mesurés en unités de santé (cas, décès, DALY) (Drummond et al., 2015)."),
  h4("Formules utilisées"),
  p(strong("Coût actualisé : "), formule("C = Σ Cₜ / (1 + r)ᵗ⁻¹"),
    " — année 1 non actualisée ; coûts et effets actualisés (3 % par défaut, OMS / iDSI)."),
  p(strong("Coût par bénéficiaire : "), formule("C / nombre de bénéficiaires")),
  p(strong("Ratio coût-efficacité moyen : "), formule("ACER = C / E"), " — descriptif, pas un critère de décision."),
  p(strong("Ratio coût-efficacité différentiel : "), formule("ICER = (C₁ − C₀) / (E₁ − E₀)"),
    " — critère de décision (Weinstein & Stason, 1977). Sans comparateur, C₀ = E₀ = 0."),
  p(strong("Bénéfice monétaire net : "), formule("NMB = λ·ΔE − ΔC"), " ; ", strong("bénéfice sanitaire net : "),
    formule("NHB = ΔE − ΔC / λ"), " — coût-efficace si NMB > 0 (Stinnett & Mullahy, 1998)."),
  p(strong("DALY : "), formule("DALY = YLL + YLD ; YLL = décès × années perdues ; YLD = épisodes × poids × durée"),
    " (Murray, 1994 ; GBD)."),
  p(strong("Rendement financier : "), formule("ROI = (B − C) / C ; ratio B/C = B / C ; VAN = B − C"),
    " — B = économies actualisées (analyse coût-bénéfice)."),
  h4("Décision"),
  tags$ul(
    tags$li(strong("Dominant"), " : plus efficace et moins cher → à adopter."),
    tags$li(strong("Dominé"), " : moins efficace et plus cher → à rejeter."),
    tags$li(strong("Plus efficace et plus cher"), " : coût-efficace si ICER ≤ seuil λ."),
    tags$li(strong("Moins efficace et moins cher"), " : acceptable si l'économie par DALY perdue dépasse λ.")
  ),
  h4("Seuil"),
  p("Les repères 1–3 × PIB/habitant (Commission Macroéconomie et Santé, 2001) ne sont plus recommandés comme règle ",
    "de décision (Bertram et al., 2016). Des seuils fondés sur le coût d'opportunité, environ 0,1–0,5 × PIB/habitant ",
    "dans les pays à revenu faible et intermédiaire, sont proposés (Woods et al., 2016 ; Ochalek et al., 2018). ",
    "Préférez le seuil de votre ministère ou bailleur."),
  h4("Incertitude"),
  p("Sensibilité univariée (tornade) et analyse probabiliste : chaque marge ± x % est un intervalle à 95 % d'un facteur ",
    "log-normal tiré indépendamment par activité ; courbe d'acceptabilité = P(NMB > 0) selon λ ",
    "(Fenwick et al., 2001 ; Briggs et al., 2006). Le comparateur est tenu fixe."),
  h4("Simplifications"),
  tags$ul(
    tags$li("Les DALY sont comptées l'année où le résultat est obtenu."),
    tags$li("Un seul comparateur ; pas de calcul de dominance étendue entre plusieurs options."),
    tags$li("Les investissements sont imputés en année 1, sans valeur résiduelle.")
  ),
  h4("Références")
)

methode_en <- div(
  class = "t-en",
  h3("Cost-effectiveness analysis"),
  p("A comparative economic evaluation method that weighs the costs and health effects of at least two options, ",
    "with effects measured in health units (cases, deaths, DALYs) (Drummond et al., 2015)."),
  h4("Formulas used"),
  p(strong("Discounted cost: "), formule("C = Σ Cₜ / (1 + r)ᵗ⁻¹"),
    " — year 1 undiscounted; costs and effects discounted (3% by default, WHO / iDSI)."),
  p(strong("Cost per beneficiary: "), formule("C / number of beneficiaries")),
  p(strong("Average cost-effectiveness ratio: "), formule("ACER = C / E"), " — descriptive, not a decision rule."),
  p(strong("Incremental cost-effectiveness ratio: "), formule("ICER = (C₁ − C₀) / (E₁ − E₀)"),
    " — decision rule (Weinstein & Stason, 1977). Without a comparator, C₀ = E₀ = 0."),
  p(strong("Net monetary benefit: "), formule("NMB = λ·ΔE − ΔC"), "; ", strong("net health benefit: "),
    formule("NHB = ΔE − ΔC / λ"), " — cost-effective if NMB > 0 (Stinnett & Mullahy, 1998)."),
  p(strong("DALY: "), formule("DALY = YLL + YLD; YLL = deaths × years lost; YLD = episodes × weight × duration"),
    " (Murray, 1994; GBD)."),
  p(strong("Financial return: "), formule("ROI = (B − C) / C; B/C ratio = B / C; NPV = B − C"),
    " — B = discounted savings (cost-benefit analysis)."),
  h4("Decision"),
  tags$ul(
    tags$li(strong("Dominant"), ": more effective and cheaper → adopt."),
    tags$li(strong("Dominated"), ": less effective and more costly → reject."),
    tags$li(strong("More effective and more costly"), ": cost-effective if ICER ≤ threshold λ."),
    tags$li(strong("Less effective and cheaper"), ": acceptable if savings per DALY lost exceed λ.")
  ),
  h4("Threshold"),
  p("The 1–3 × GDP per capita benchmarks (Commission on Macroeconomics and Health, 2001) are no longer recommended ",
    "as a decision rule (Bertram et al., 2016). Opportunity-cost-based thresholds of about 0.1–0.5 × GDP per capita ",
    "in low- and middle-income countries have been proposed (Woods et al., 2016; Ochalek et al., 2018). ",
    "Use your ministry's or funder's threshold when available."),
  h4("Uncertainty"),
  p("One-way sensitivity analysis (tornado) and probabilistic analysis: each ± x% margin is treated as a 95% interval ",
    "of a log-normal factor drawn independently for each activity; the acceptability curve is P(NMB > 0) as a ",
    "function of λ (Fenwick et al., 2001; Briggs et al., 2006). The comparator is held fixed."),
  h4("Simplifications"),
  tags$ul(
    tags$li("DALYs are counted in the year the output is achieved."),
    tags$li("A single comparator; no extended dominance across several options."),
    tags$li("Investments are charged in year 1, with no residual value.")
  ),
  h4("References")
)

# ---- Interface --------------------------------------------------------------

ui <- page_navbar(
  id = "onglets",
  title = tr("Évaluation économique", "Economic evaluation"),
  window_title = "Évaluation économique · Economic evaluation",
  fillable = FALSE,
  theme = bs_theme(version = 5, bg = "#faf7f2", fg = "#1f2a2e", primary = "#0f5257",
                   secondary = "#c08a2e", base_font = police, heading_font = police, font_scale = 1.08),
  header = tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=EB+Garamond:ital,wght@0,400;0,500;0,600;0,700;1,400&display=swap"),
    tags$style(HTML(css)),
    tags$script(HTML(js))
  ),

  nav_panel(
    tr("Accueil", "Home"), value = "accueil",
    div(class = "hero",
        h1(tr("Votre projet de santé vaut-il son coût ?", "Is your health project worth its cost?")),
        p(tr(paste("Un outil pour tous les projets : décrivez votre projet, répondez à quelques questions,",
                   "renseignez vos activités. L'outil calcule vos coûts, le coût par bénéficiaire, le ratio",
                   "coût-efficacité (ACER et ICER), le bénéfice net, le ROI, et vous dit si le projet est coût-efficace."),
             paste("One tool for every project: describe your project, answer a few questions and enter your",
                   "activities. The tool calculates your costs, cost per beneficiary, cost-effectiveness ratios",
                   "(ACER and ICER), net benefit and ROI, and tells you whether the project is cost-effective."))),
        actionButton("demarrer_vide", tr("Commencer mon projet", "Start my project"), class = "btn-or"),
        actionButton("demarrer_exemple", tr("Voir un exemple", "See an example"), class = "btn-blanc")),
    layout_column_wrap(
      width = "220px",
      card(div(class = "etape", div(class = "num", "1"), h4(tr("Le projet", "The project")),
               p(tr("Pays, monnaie, durée, seuil : tout se saisit librement.",
                    "Country, currency, duration, threshold: everything is entered freely.")))),
      card(div(class = "etape", div(class = "num", "2"), h4(tr("Le profil", "The profile")),
               p(tr("Quelques questions pour adapter l'analyse à votre projet.",
                    "A few questions to tailor the analysis to your project.")))),
      card(div(class = "etape", div(class = "num", "3"), h4(tr("Les activités", "The activities")),
               p(tr("Coûts, bénéficiaires et résultats de chaque activité.",
                    "Costs, beneficiaries and outputs of each activity.")))),
      card(div(class = "etape", div(class = "num", "4"), h4(tr("Les résultats", "The results")),
               p(tr("Coûts, coût-efficacité, rendement, incertitude et rapport imprimable.",
                    "Costs, cost-effectiveness, return, uncertainty and a printable report."))))
    )
  ),

  nav_panel(
    tr("1 · Projet", "1 · Project"), value = "projet",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(tr("Identité du projet", "Project details")),
        textInput("nom", tr("Nom du projet", "Project name"), ""),
        textInput("organisation", tr("Organisation", "Organisation"), ""),
        layout_column_wrap(
          width = "180px",
          textInput("pays", tr("Pays / zone", "Country / area"), ""),
          textInput("monnaie", tr("Monnaie", "Currency"), "")
        ),
        textAreaInput("description", tr("Description et sources des données", "Description and data sources"),
                      "", rows = 3)
      ),
      card(
        card_header(tr("Paramètres de l'analyse", "Analysis parameters")),
        layout_column_wrap(
          width = "180px",
          numericInput("annee_debut", tr("Année de début", "Start year"), as.integer(format(Sys.Date(), "%Y")),
                       min = 1990, max = 2100),
          numericInput("duree", tr("Durée de l'analyse (années)", "Analysis period (years)"), 3, min = 1, max = 30),
          numericInput("taux_couts", tr("Actualisation des coûts (%)", "Discount rate, costs (%)"), 3,
                       min = 0, max = 20, step = 0.5),
          numericInput("taux_effets", tr("Actualisation des effets (%)", "Discount rate, effects (%)"), 3,
                       min = 0, max = 20, step = 0.5),
          numericInput("attribution", tr("Part de l'effet due au projet (%)", "Share of effect due to the project (%)"),
                       100, min = 0, max = 100)
        ),
        div(class = "aide mb-3",
            tr("3 % est le taux de référence de l'OMS et de l'iDSI. La part de l'effet due au projet retire ce qui serait arrivé sans lui ou grâce à d'autres acteurs.",
               "3% is the WHO and iDSI reference rate. The share of effect due to the project removes what would have happened without it or thanks to other actors.")),
        layout_column_wrap(
          width = "180px",
          numericInput("pib_hab", tr("PIB par habitant (monnaie du projet)", "GDP per capita (project currency)"),
                       NA, min = 0),
          numericInput("seuil", tr("Seuil par DALY évitée", "Threshold per DALY averted"), NA, min = 0),
          numericInput("taux_change", tr("Monnaie du projet pour 1 USD", "Project currency per 1 USD"), NA, min = 0)
        ),
        div(class = "aide",
            tr("Seuil : celui de votre ministère ou bailleur. Sans seuil, l'outil prend 0,5 × PIB par habitant comme repère indicatif. Le taux de change (optionnel) sert à afficher les équivalents en USD.",
               "Threshold: your ministry's or funder's. Without one, the tool uses 0.5 × GDP per capita as an indicative benchmark. The exchange rate (optional) is used to show USD equivalents."))
      )
    ),
    card(
      card_header(tr("Enregistrer ou reprendre un projet", "Save or reopen a project")),
      layout_columns(
        col_widths = c(6, 6),
        downloadButton("sauver", tr("Enregistrer le projet (.json)", "Save project (.json)"), class = "btn-outline-primary"),
        fileInput("ouvrir", NULL, accept = ".json", buttonLabel = tr("Ouvrir un projet…", "Open a project…"),
                  placeholder = ".json")
      )
    ),
    navigation("retour_accueil", "vers_profil")
  ),

  nav_panel(
    tr("2 · Profil", "2 · Profile"), value = "profil",
    p(class = "aide", tr("Vos réponses adaptent les écrans suivants à votre projet.",
                         "Your answers tailor the next screens to your project.")),
    layout_column_wrap(
      width = "420px",
      question("effets_sante", tr("Votre projet a-t-il des effets sur la santé ?", "Does your project have health effects?"),
               list(tr("Oui : il évite des décès, des maladies ou des incapacités", "Yes: it averts deaths, illness or disability"),
                    tr("Non : je veux seulement analyser les coûts", "No: I only want to analyse costs")),
               c("oui", "non"), "oui"),
      conditionalPanel(
        si_daly,
        question("mode_daly", tr("Comment renseigner ces effets en DALY ?", "How will you enter these effects in DALYs?"),
                 list(tr("Je connais les DALY évitées par résultat (littérature, étude)",
                         "I know the DALYs averted per output (literature, study)"),
                      tr("Calculer les DALY à partir des décès et des maladies évités",
                         "Calculate DALYs from deaths and illness averted")),
                 c("direct", "calcul"), "direct",
                 tr("DALY = années de vie perdues (YLL) + années vécues avec incapacité (YLD).",
                    "DALY = years of life lost (YLL) + years lived with disability (YLD)."))
      ),
      question("economies", tr("Votre projet génère-t-il des économies ?", "Does your project generate savings?"),
               list(tr("Oui : soins évités, dépenses des ménages, productivité", "Yes: care averted, household spending, productivity"),
                    tr("Non / je ne sais pas", "No / I don't know")),
               c("oui", "non"), "non",
               tr("Permet de calculer le ROI, le ratio bénéfice/coût et la valeur actuelle nette.",
                  "Used to calculate ROI, the benefit-cost ratio and net present value.")),
      question("comparer", tr("Voulez-vous comparer à la pratique actuelle ?", "Do you want to compare with current practice?"),
               list(tr("Oui : je connais le coût et l'effet de ce qui se fait aujourd'hui",
                       "Yes: I know the cost and effect of what is done today"),
                    tr("Non : comparer à l'absence d'intervention", "No: compare with no intervention")),
               c("oui", "non"), "non",
               tr("La comparaison donne l'ICER, l'indicateur de décision recommandé.",
                  "The comparison gives the ICER, the recommended decision indicator.")),
      question("incertitude_on", tr("Voulez-vous une analyse d'incertitude probabiliste ?",
                                    "Do you want a probabilistic uncertainty analysis?"),
               list(tr("Oui : simulation de Monte-Carlo et courbe d'acceptabilité",
                       "Yes: Monte Carlo simulation and acceptability curve"),
                    tr("Non : analyse de sensibilité simple seulement", "No: simple sensitivity analysis only")),
               c("oui", "non"), "non")
    ),
    navigation("retour_projet", "vers_activites")
  ),

  nav_panel(
    tr("3 · Activités", "3 · Activities"), value = "activites",
    card(
      card_header(tr("Activités du projet", "Project activities")),
      p(tr("Ajoutez toutes les activités du projet. Montants dans la monnaie du projet ; « / an » = chaque année de l'analyse.",
           "Add all project activities. Amounts in the project currency; “/ year” = each year of the analysis.")),
      conditionalPanel(si_daly, div(class = "aide",
        tr("Une activité de support (formation, coordination) peut avoir 0 DALY : son coût compte quand même dans le total.",
           "A support activity (training, coordination) can have 0 DALYs: its cost still counts in the total.")))
    ),
    div(id = "liste_activites"),
    div(class = "mb-3", actionButton("ajouter", tr("+ Ajouter une activité", "+ Add an activity"), class = "btn-or")),
    navigation("retour_profil", "vers_suite")
  ),

  nav_panel(
    tr("4 · Comparateur", "4 · Comparator"), value = "comparateur",
    card(
      card_header(tr("Pratique actuelle (sans le projet)", "Current practice (without the project)")),
      p(tr("Ce que coûte et produit aujourd'hui l'alternative à laquelle on compare le projet.",
           "What the alternative the project is compared with costs and achieves today.")),
      layout_column_wrap(
        width = "220px",
        numericInput("comp_investissement", tr("Investissement", "Investment"), 0, min = 0),
        numericInput("comp_cout", tr("Coût annuel", "Annual cost"), 0, min = 0),
        numericInput("comp_daly", tr("DALY évitées / an", "DALYs averted / year"), 0, min = 0, step = 0.1)
      ),
      div(class = "aide", tr("ICER = (coût du projet − coût actuel) / (DALY du projet − DALY actuelles).",
                             "ICER = (project cost − current cost) / (project DALYs − current DALYs)."))
    ),
    navigation("retour_activites", "vers_suite2")
  ),

  nav_panel(
    tr("5 · Incertitude", "5 · Uncertainty"), value = "incertitude",
    card(
      card_header(tr("Marges d'incertitude", "Uncertainty margins")),
      p(tr("De combien vos chiffres pourraient-ils varier ? Ces marges servent à l'analyse de sensibilité et à la simulation de Monte-Carlo.",
           "How much could your figures vary? These margins feed the sensitivity analysis and the Monte Carlo simulation.")),
      layout_column_wrap(
        width = "220px",
        numericInput("inc_couts", tr("Coûts (± %)", "Costs (± %)"), 20, min = 0, max = 100),
        numericInput("inc_resultats", tr("Résultats obtenus (± %)", "Outputs achieved (± %)"), 20, min = 0, max = 100),
        numericInput("inc_daly", tr("DALY par résultat (± %)", "DALYs per output (± %)"), 30, min = 0, max = 100),
        numericInput("inc_economies", tr("Économies (± %)", "Savings (± %)"), 30, min = 0, max = 100),
        selectInput("n_sim", tr("Nombre de simulations", "Number of simulations"), c(500, 1000, 2000, 5000), 1000)
      )
    ),
    navigation("retour_activites2", "vers_resultats", tr("Voir les résultats →", "See the results →"))
  ),

  nav_panel(
    tr("Résultats", "Results"), value = "resultats",
    uiOutput("bandeau"),
    h3(class = "section", tr("Coûts", "Costs")),
    uiOutput("kpis_couts"),
    card(card_header(tr("Coûts par année", "Costs by year")), tableOutput("table_annees")),
    conditionalPanel(
      si_daly,
      h3(class = "section", tr("Coût-efficacité", "Cost-effectiveness")),
      uiOutput("kpis_ce"),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header(tr("Plan coût-efficacité", "Cost-effectiveness plane")), plotOutput("plan", height = "380px")),
        card(card_header(tr("Coût par DALY évitée, par activité", "Cost per DALY averted, by activity")),
             plotOutput("barres", height = "380px"))
      )
    ),
    conditionalPanel(
      "input.economies == 'oui'",
      h3(class = "section", tr("Rendement financier", "Financial return")),
      uiOutput("kpis_fin")
    ),
    h3(class = "section", tr("Activités", "Activities")),
    card(tableOutput("table_activites"),
         div(class = "no-print", downloadButton("telecharger", tr("Télécharger (.csv)", "Download (.csv)"),
                                                class = "btn-outline-primary btn-sm"))),
    conditionalPanel(
      si_daly,
      h3(class = "section", tr("Incertitude", "Uncertainty")),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header(tr("Analyse de sensibilité (tornade)", "Sensitivity analysis (tornado)")),
             plotOutput("tornade", height = "330px"),
             p(class = "aide", tr("Variation du ratio coût par DALY quand chaque paramètre varie de sa marge.",
                                  "Change in the cost per DALY ratio when each parameter varies by its margin."))),
        card(card_header(tr("Courbe d'acceptabilité", "Acceptability curve")),
             conditionalPanel("input.incertitude_on == 'oui'", plotOutput("ceac", height = "330px"),
                              uiOutput("resume_psa")),
             conditionalPanel("input.incertitude_on != 'oui'",
                              p(class = "aide mt-3",
                                tr("Activez l'analyse probabiliste à l'étape « Profil » pour obtenir la probabilité que le projet soit coût-efficace.",
                                   "Turn on the probabilistic analysis in the “Profile” step to get the probability that the project is cost-effective."))))
      )
    ),
    div(class = "text-end mb-4 no-print", actionButton("vers_rapport", tr("Voir le rapport →", "See the report →"),
                                                       class = "btn-primary"))
  ),

  nav_panel(
    tr("Rapport", "Report"), value = "rapport",
    div(class = "text-end mb-3 no-print",
        tags$button(class = "btn btn-or", onclick = "window.print()", tr("Imprimer / PDF", "Print / PDF"))),
    div(class = "rapport", uiOutput("rapport"))
  ),

  nav_panel(
    tr("Méthode et sources", "Method and sources"), value = "methode",
    card(card_body(methode_fr, methode_en, references))
  ),

  nav_spacer(),
  nav_item(tags$button(id = "bouton_langue", type = "button", class = "btn btn-sm btn-outline-primary",
                       onclick = "basculerLangue()", tr("English", "Français")))
)

# ---- Serveur ----------------------------------------------------------------

server <- function(input, output, session) {

  en <- reactive(identical(input$langue, "en"))
  L <- function(fr, en_) if (en()) en_ else fr

  fmt <- function(x, decimales = 0) {
    if (is.null(x) || length(x) != 1 || is.na(x) || !is.finite(x)) return("—")
    if (en()) formatC(x, format = "f", digits = decimales, big.mark = ",")
    else formatC(x, format = "f", digits = decimales, big.mark = " ", decimal.mark = ",")
  }
  fmt_pct <- function(x) if (is.null(x) || is.na(x)) "—" else paste0(fmt(100 * x), L(" %", "%"))

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
      paste("→", fmt(daly_activite(id), 4), L("DALY évitée(s) par résultat", "DALY(s) averted per output")))
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
        nom = if (nzchar(nom)) nom else paste(L("Activité", "Activity"), k),
        unite = if (nzchar(unite)) unite else L("résultats", "outputs"),
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

  exemple <- function(langue = "fr") {
    anglais <- identical(langue, "en")
    a <- exemple_activites(langue)
    names(a)[names(a) == "daly_par_resultat"] <- "daly_direct"
    appliquer(
      textes = list(
        nom = if (anglais) "Mobile malaria diagnosis (fictitious example)" else "Diagnostic mobile du paludisme (exemple fictif)",
        organisation = if (anglais) "Demonstration project" else "Projet de démonstration",
        pays = "Burkina Faso", monnaie = "XOF",
        description = if (anglais) "Fictitious example: community health workers test and treat malaria at home, with SMS follow-up of pregnant women."
          else "Exemple fictif : des agents communautaires dépistent et traitent le paludisme à domicile, avec un suivi des femmes enceintes par SMS."),
      nombres = list(duree = 3, taux_couts = 3, taux_effets = 3, attribution = 80, pib_hab = 500000,
                     seuil = NA, taux_change = 600, comp_investissement = 0, comp_cout = 4000000, comp_daly = 40),
      profil = list(effets_sante = "oui", mode_daly = "direct", economies = "oui", comparer = "oui",
                    incertitude_on = "oui"),
      acts = a
    )
  }

  exemple()

  observeEvent(input$demarrer_exemple, { exemple(if (en()) "en" else "fr"); aller("resultats") })

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
    filename = function() paste0(gsub("[^A-Za-z0-9_-]+", "_", if (nzchar(txt("nom"))) txt("nom") else "project"), ".json"),
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
      showNotification(L("Fichier de projet invalide.", "Invalid project file."), type = "error")
      return()
    }
    appliquer(projet$textes %||% list(), projet$nombres %||% list(), projet$profil %||% list(), projet$activites)
    showNotification(L("Projet ouvert.", "Project opened."), type = "message")
  })

  # -- Résultats --

  icer_texte <- function(ce) switch(ce$quadrant, dominante = L("Dominant", "Dominant"),
                                    dominee = L("Dominé", "Dominated"), argent(ce$icer))

  kpi <- function(titre, valeur, detail = NULL) {
    value_box(title = titre, value = valeur, if (!is.null(detail)) p(detail), class = "kpi")
  }

  texte_quadrant <- function(q, alt) {
    if (en()) switch(q,
      dominante = paste0("Dominant: more effective and less costly than ", alt, "."),
      dominee = paste0("Dominated: less effective and more costly than ", alt, "."),
      nord_est = paste0("More effective and more costly than ", alt, ": the ICER is compared with the threshold."),
      sud_ouest = paste0("Less effective and less costly than ", alt, "."))
    else switch(q,
      dominante = paste0("Dominant : plus efficace et moins coûteux que ", alt, "."),
      dominee = paste0("Dominé : moins efficace et plus coûteux que ", alt, "."),
      nord_est = paste0("Plus efficace et plus coûteux que ", alt, " : l'ICER est comparé au seuil."),
      sud_ouest = paste0("Moins efficace et moins coûteux que ", alt, "."))
  }

  vs <- function() if (oui("comparer")) L("vs pratique actuelle", "vs current practice") else
    L("vs aucune intervention", "vs no intervention")

  output$bandeau <- renderUI({
    r <- res()
    fond <- function(c) paste0("background:", c)
    if (is.null(r)) {
      return(div(class = "verdict", style = fond(couleurs$gris),
                 div(class = "phrase", L("Ajoutez au moins une activité pour voir les résultats.",
                                         "Add at least one activity to see the results."))))
    }
    titre <- div(class = "detail", if (nzchar(txt("nom"))) txt("nom") else L("Votre projet", "Your project"),
                 if (nzchar(txt("pays"))) paste(" ·", txt("pays")))
    if (!oui("effets_sante")) {
      return(div(class = "verdict", style = fond(couleurs$sarcelle), titre,
                 div(class = "chiffre", argent(r$couts$cout_par_beneficiaire),
                     span(class = "unite", L(" par bénéficiaire", " per beneficiary"))),
                 div(class = "phrase", paste(L("Coût total actualisé :", "Total discounted cost:"), argent(r$couts$total))),
                 div(class = "detail", L("Analyse des coûts uniquement (pas d'effets de santé renseignés).",
                                         "Cost analysis only (no health effects entered)."))))
    }
    ce <- r$ce
    s <- seuil()
    alt <- if (oui("comparer")) L("la pratique actuelle", "current practice") else
      L("l'absence d'intervention", "no intervention")
    if (ce$quadrant %in% c("dominante", "dominee")) {
      chiffre <- icer_texte(ce)
      unite <- ""
    } else {
      chiffre <- argent(ce$icer)
      unite <- if (ce$quadrant == "sud_ouest") L(" économisés par DALY perdue", " saved per DALY lost") else
        L(" par DALY évitée (ICER)", " per DALY averted (ICER)")
    }
    phrase <- if (is.na(ce$cout_efficace)) {
      L("Renseignez un seuil ou le PIB par habitant pour conclure.",
        "Enter a threshold or GDP per capita to reach a conclusion.")
    } else if (ce$cout_efficace) L("Le projet est coût-efficace.", "The project is cost-effective.") else
      L("Le projet n'est pas coût-efficace au seuil retenu.", "The project is not cost-effective at the chosen threshold.")
    couleur <- if (is.na(ce$cout_efficace)) couleurs$gris else if (ce$cout_efficace) couleurs$vert else couleurs$rouge
    seuil_txt <- if (!is.na(s$valeur)) paste0(
      L("Seuil : ", "Threshold: "), argent(s$valeur), L(" par DALY évitée", " per DALY averted"),
      if (s$source == "pib") L(" (repère indicatif : 0,5 × PIB par habitant)", " (indicative benchmark: 0.5 × GDP per capita)") else "",
      L(" · bénéfice monétaire net : ", " · net monetary benefit: "), argent(ce$nmb), ".")
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
      kpi(L("Coût total (non actualisé)", "Total cost (undiscounted)"), argent(k$total_nominal), en_usd(k$total_nominal)),
      kpi(L("Coût total actualisé", "Total discounted cost"), argent(k$total), en_usd(k$total)),
      kpi(L("Investissement", "Investment"), argent(k$investissement), fmt_pct(ratio(k$investissement, k$total))),
      kpi(L("Fonctionnement (actualisé)", "Running costs (discounted)"), argent(k$fonctionnement),
          fmt_pct(ratio(k$fonctionnement, k$total))),
      kpi(L("Bénéficiaires", "Beneficiaries"), fmt(k$beneficiaires), paste(fmt(k$beneficiaires_an), L("par an", "per year"))),
      kpi(L("Coût par bénéficiaire", "Cost per beneficiary"), argent(k$cout_par_beneficiaire), en_usd(k$cout_par_beneficiaire)),
      kpi(L("Coût par bénéficiaire et par an", "Cost per beneficiary per year"), argent(k$cout_par_beneficiaire_an),
          en_usd(k$cout_par_beneficiaire_an))
    )
  })

  output$table_annees <- renderTable({
    r <- res(); req(r)
    f <- r$flux
    t <- data.frame(as.character(f$annee), vapply(f$investissement, fmt, ""), vapply(f$fonctionnement, fmt, ""),
                    vapply(f$couts, fmt, ""), vapply(f$couts_actualises, fmt, ""), vapply(f$beneficiaires, fmt, ""))
    names(t) <- if (en()) c("Year", "Investment", "Running costs", "Total cost", "Discounted cost", "Beneficiaries") else
      c("Année", "Investissement", "Fonctionnement", "Coût total", "Coût actualisé", "Bénéficiaires")
    if (oui("effets_sante")) t[[L("DALY évitées", "DALYs averted")]] <- vapply(f$daly, fmt, "", 1)
    t
  }, striped = TRUE, width = "100%")

  output$kpis_ce <- renderUI({
    r <- res(); req(r)
    ce <- r$ce
    layout_column_wrap(
      width = "250px", class = "mb-3",
      kpi(L("DALY évitées (actualisées)", "DALYs averted (discounted)"), fmt(r$daly, 1),
          L("années de vie en bonne santé", "healthy life years")),
      kpi(L("ACER : coût par DALY évitée", "ACER: cost per DALY averted"), argent(ce$acer), en_usd(ce$acer)),
      kpi(L("ACER net", "Net ACER"), argent(ce$acer_net), L("après économies générées", "after savings generated")),
      kpi(L("Surcoût (ΔC)", "Incremental cost (ΔC)"), argent(ce$delta_cout), vs()),
      kpi(L("DALY gagnées (ΔE)", "DALYs gained (ΔE)"), fmt(ce$delta_daly, 1), vs()),
      kpi("ICER", icer_texte(ce), L("coût par DALY évitée en plus", "cost per additional DALY averted")),
      kpi(L("Bénéfice monétaire net (NMB)", "Net monetary benefit (NMB)"), argent(ce$nmb),
          L("λ·ΔE − ΔC ; > 0 = coût-efficace", "λ·ΔE − ΔC; > 0 = cost-effective")),
      kpi(L("Bénéfice sanitaire net (NHB)", "Net health benefit (NHB)"), paste(fmt(ce$nhb, 1), "DALY"), "ΔE − ΔC / λ")
    )
  })

  output$kpis_fin <- renderUI({
    r <- res(); req(r)
    f <- r$financier; b <- r$benefices
    layout_column_wrap(
      width = "250px", class = "mb-3",
      kpi(L("Économies totales (actualisées)", "Total savings (discounted)"), argent(b$total),
          paste0(L("santé ", "health "), fmt(b$eco_sante), L(" · ménages ", " · households "), fmt(b$eco_menages),
                 L(" · productivité ", " · productivity "), fmt(b$productivite))),
      kpi(L("Valeur actuelle nette", "Net present value"), argent(f$van), en_usd(f$van)),
      kpi("ROI", fmt_pct(f$roi), L("(économies − coûts) / coûts", "(savings − costs) / costs")),
      kpi(L("Ratio bénéfice / coût", "Benefit-cost ratio"), fmt(f$ratio_bc, 2),
          L("économies par unité investie", "savings per unit invested")),
      kpi(L("Année de retour sur investissement", "Payback year"),
          if (is.na(f$annee_retour)) L("Non atteinte", "Not reached") else f$annee_retour)
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
    plot(NA, xlim = c(-ex, ex), ylim = c(-ey, ey), xlab = L("DALY évitées en plus (ΔE)", "Additional DALYs averted (ΔE)"),
         ylab = "", axes = FALSE)
    axis(1, col = "#cfc6b6", col.axis = "#5b6a6e")
    axis(2, at = pretty(c(-ey, ey)), labels = vapply(pretty(c(-ey, ey)), fmt, ""), col = "#cfc6b6",
         col.axis = "#5b6a6e", cex.axis = 0.8)
    mtext(paste(L("Surcoût (ΔC)", "Incremental cost (ΔC)"), monnaie()), side = 2, line = 5.3, las = 0, col = "#5b6a6e")
    abline(h = 0, v = 0, col = "#cfc6b6")
    s <- seuil()$valeur
    if (!is.na(s)) {
      abline(a = 0, b = s, lty = 2, col = couleurs$or, lwd = 2)
      legend("topleft", bty = "n", legend = L("seuil λ", "threshold λ"), lty = 2, lwd = 2, col = couleurs$or)
    }
    if (!is.null(nuage)) points(nuage$delta_daly, nuage$delta_cout, pch = 16, cex = 0.5,
                                col = adjustcolor(couleurs$sarcelle, 0.18))
    points(ce$delta_daly, ce$delta_cout, pch = 21, bg = couleurs$or, col = couleurs$encre, cex = 2.2)
    etiquettes <- if (en()) c("dominant", "dominated", "more effective, more costly", "less effective, cheaper") else
      c("dominant", "dominé", "plus efficace, plus cher", "moins efficace, moins cher")
    text(c(ex, -ex, ex, -ex) * 0.62, c(-ey, ey, ey, -ey) * 0.9, etiquettes, col = "#9aa6a9", cex = 0.9)
  })

  output$barres <- renderPlot({
    r <- res(); req(r)
    d <- r$activites[!is.na(r$activites$cout_par_daly), ]
    par(family = "serif", bg = "#ffffff", las = 1)
    if (nrow(d) == 0) {
      par(mar = c(1, 1, 1, 1)); plot.new()
      text(0.5, 0.5, L("Aucune activité avec des DALY évitées.", "No activity with DALYs averted."),
           col = couleurs$gris, cex = 1.2)
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
    mtext(paste(L("Coût par DALY évitée", "Cost per DALY averted"), monnaie()), side = 1, line = 2.5, col = "#5b6a6e")
  })

  table_activites <- reactive({
    r <- res(); req(r)
    d <- r$activites
    t <- data.frame(
      d$nom, vapply(d$cout, fmt, ""), vapply(d$part_cout, fmt_pct, ""), vapply(d$beneficiaires, fmt, ""),
      vapply(d$cout_par_beneficiaire, fmt, ""),
      ifelse(is.na(d$cout_par_resultat), "—",
             paste(vapply(d$cout_par_resultat, fmt, ""), L("par", "per"), d$unite)),
      stringsAsFactors = FALSE
    )
    names(t) <- if (en()) c("Activity", "Discounted cost", "Share of cost", "Beneficiaries", "Cost / beneficiary",
                            "Cost per output") else
      c("Activité", "Coût actualisé", "Part du coût", "Bénéficiaires", "Coût / bénéficiaire", "Coût par résultat")
    if (oui("effets_sante")) {
      t[[L("DALY évitées", "DALYs averted")]] <- vapply(d$daly, fmt, "", 1)
      t[[L("Coût / DALY", "Cost / DALY")]] <- ifelse(is.na(d$cout_par_daly), L("support", "support"),
                                                     vapply(d$cout_par_daly, fmt, ""))
    }
    if (oui("economies")) t[[L("Économies", "Savings")]] <- vapply(d$economies, fmt, "")
    t
  })

  output$table_activites <- renderTable(table_activites(), striped = TRUE, hover = TRUE, width = "100%")

  output$telecharger <- downloadHandler(
    filename = function() L("activites_evaluation.csv", "activities_evaluation.csv"),
    content = function(file) write.csv(table_activites(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  noms_parametres <- function() if (en()) c(couts = "Costs", resultats = "Outputs achieved", daly = "DALYs per output",
                                            attribution = "Share attributable", actualisation = "Discount rate (0%–6%)") else
    c(couts = "Coûts", resultats = "Résultats obtenus", daly = "DALY par résultat",
      attribution = "Part attribuable", actualisation = "Taux d'actualisation (0 % – 6 %)")

  output$tornade <- renderPlot({
    a <- activites(); req(a, oui("effets_sante"))
    t <- tornade(parametres(), a, comparateur(), incertitude())
    t <- t[is.finite(t$bas) & is.finite(t$haut), ]
    par(family = "serif", bg = "#ffffff", las = 1)
    if (nrow(t) == 0 || !is.finite(t$base[1])) {
      par(mar = c(1, 1, 1, 1)); plot.new()
      text(0.5, 0.5, L("Pas assez de données (DALY nulles).", "Not enough data (zero DALYs)."), col = couleurs$gris, cex = 1.2)
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
    axis(2, at = seq_len(nrow(t)), labels = noms_parametres()[t$parametre], tick = FALSE, cex.axis = 0.95)
    ticks <- pretty(c(bornes[1] - marge, bornes[2] + marge))
    axis(1, at = ticks, labels = vapply(ticks, fmt, ""), col = "#cfc6b6", col.axis = "#5b6a6e", cex.axis = 0.8)
    mtext(paste(L("Coût par DALY", "Cost per DALY"), monnaie()), side = 1, line = 2.5, col = "#5b6a6e")
    legend("bottomright", bty = "n", fill = c(couleurs$sarcelle, couleurs$or), border = NA,
           legend = L(c("valeur basse", "valeur haute"), c("low value", "high value")), cex = 0.9)
  })

  output$ceac <- renderPlot({
    s <- sim()
    seuil_v <- seuil()$valeur
    ref <- max(c(seuil_v, abs(res()$ce$icer), 1), na.rm = TRUE)
    lambdas <- seq(0, 3 * ref, length.out = 120)
    pr <- acceptabilite(s, lambdas)
    par(family = "serif", bg = "#ffffff", las = 1, mar = c(4.5, 4.5, 1, 1))
    plot(lambdas, pr, type = "l", lwd = 3, col = couleurs$sarcelle, ylim = c(0, 1), axes = FALSE,
         xlab = paste(L("Seuil λ par DALY évitée", "Threshold λ per DALY averted"), monnaie()),
         ylab = L("Probabilité d'être coût-efficace", "Probability of being cost-effective"))
    ticks <- pretty(lambdas)
    axis(1, at = ticks, labels = vapply(ticks, fmt, ""), col = "#cfc6b6", col.axis = "#5b6a6e", cex.axis = 0.8)
    axis(2, at = seq(0, 1, 0.25), labels = paste0(seq(0, 100, 25), L(" %", "%")), col = "#cfc6b6", col.axis = "#5b6a6e")
    abline(h = 0.5, col = "#e3d9c6")
    if (!is.na(seuil_v)) abline(v = seuil_v, lty = 2, col = couleurs$or, lwd = 2)
  })

  output$resume_psa <- renderUI({
    s <- sim()
    seuil_v <- seuil()$valeur
    ic_c <- intervalle(s$delta_cout); ic_e <- intervalle(s$delta_daly)
    p_ce <- if (!is.na(seuil_v)) acceptabilite(s, seuil_v) else NA
    nmb <- if (!is.na(seuil_v)) intervalle(seuil_v * s$delta_daly - s$delta_cout) else c(NA, NA)
    a <- L(" à ", " to ")
    tags$ul(class = "aide mt-2",
      tags$li(strong(L("Probabilité d'être coût-efficace au seuil : ", "Probability of being cost-effective at the threshold: ")),
              fmt_pct(p_ce)),
      tags$li(L("Surcoût, IC 95 % : ", "Incremental cost, 95% CI: "), fmt(ic_c[1]), a, fmt(ic_c[2]), " ", monnaie()),
      tags$li(L("DALY gagnées, IC 95 % : ", "DALYs gained, 95% CI: "), fmt(ic_e[1], 1), a, fmt(ic_e[2], 1)),
      tags$li(L("Bénéfice monétaire net, IC 95 % : ", "Net monetary benefit, 95% CI: "), fmt(nmb[1]), a, fmt(nmb[2]),
              " ", monnaie()),
      tags$li(nrow(s), L(" simulations", " simulations")))
  })

  # -- Rapport imprimable --

  output$rapport <- renderUI({
    r <- res()
    if (is.null(r)) return(p(L("Ajoutez des activités pour générer le rapport.", "Add activities to generate the report.")))
    ce <- r$ce; k <- r$couts; f <- r$financier
    s <- seuil()
    ligne <- function(label, valeur) tags$tr(tags$td(label), tags$td(strong(valeur)))
    date <- format(Sys.Date(), L("%d/%m/%Y", "%Y-%m-%d"))
    tagList(
      h2(if (nzchar(txt("nom"))) txt("nom") else L("Évaluation économique du projet", "Economic evaluation of the project")),
      p(class = "aide", paste(c(txt("organisation"), txt("pays"), date), collapse = " · ")),
      if (nzchar(txt("description"))) p(txt("description")),
      h4(L("Paramètres", "Parameters")),
      tags$table(class = "table table-sm",
        ligne(L("Durée de l'analyse", "Analysis period"),
              paste(parametres()$duree, L("an(s) à partir de", "year(s) from"), parametres()$annee_debut)),
        ligne(L("Actualisation coûts / effets", "Discount rate costs / effects"),
              paste0(fmt(num("taux_couts"), 1), "% / ", fmt(num("taux_effets"), 1), "%")),
        ligne(L("Part de l'effet due au projet", "Share of effect due to the project"), paste0(fmt(num("attribution")), "%")),
        ligne(L("Comparateur", "Comparator"),
              if (oui("comparer")) L("pratique actuelle", "current practice") else L("aucune intervention", "no intervention")),
        ligne(L("Seuil par DALY évitée", "Threshold per DALY averted"),
              if (is.na(s$valeur)) L("non renseigné", "not provided") else
                paste(argent(s$valeur), if (s$source == "pib") L("(0,5 × PIB par habitant, repère indicatif)",
                                                                 "(0.5 × GDP per capita, indicative benchmark)") else ""))),
      h4(L("Coûts", "Costs")),
      tags$table(class = "table table-sm",
        ligne(L("Coût total (non actualisé)", "Total cost (undiscounted)"), argent(k$total_nominal)),
        ligne(L("Coût total actualisé", "Total discounted cost"), argent(k$total)),
        ligne(L("Bénéficiaires", "Beneficiaries"), fmt(k$beneficiaires)),
        ligne(L("Coût par bénéficiaire", "Cost per beneficiary"), argent(k$cout_par_beneficiaire))),
      if (oui("effets_sante")) tagList(
        h4(L("Coût-efficacité", "Cost-effectiveness")),
        tags$table(class = "table table-sm",
          ligne(L("DALY évitées (actualisées)", "DALYs averted (discounted)"), fmt(r$daly, 1)),
          ligne(L("ACER (coût par DALY évitée)", "ACER (cost per DALY averted)"), argent(ce$acer)),
          ligne("ICER", icer_texte(ce)),
          ligne(L("Bénéfice monétaire net", "Net monetary benefit"), argent(ce$nmb)),
          ligne(L("Conclusion", "Conclusion"), if (is.na(ce$cout_efficace)) L("seuil non renseigné", "no threshold provided") else
            if (ce$cout_efficace) L("coût-efficace", "cost-effective") else
              L("non coût-efficace au seuil retenu", "not cost-effective at the chosen threshold")),
          if (oui("incertitude_on")) ligne(L("Probabilité d'être coût-efficace", "Probability of being cost-effective"),
            tryCatch(fmt_pct(acceptabilite(sim(), s$valeur)), error = function(e) "—")))),
      if (oui("economies")) tagList(
        h4(L("Rendement financier", "Financial return")),
        tags$table(class = "table table-sm",
          ligne(L("Économies actualisées", "Discounted savings"), argent(r$benefices$total)),
          ligne(L("Valeur actuelle nette", "Net present value"), argent(f$van)),
          ligne("ROI", fmt_pct(f$roi)),
          ligne(L("Ratio bénéfice / coût", "Benefit-cost ratio"), fmt(f$ratio_bc, 2)),
          ligne(L("Année de retour", "Payback year"), if (is.na(f$annee_retour)) L("non atteinte", "not reached") else f$annee_retour))),
      h4(L("Activités", "Activities")),
      tableau_html(table_activites()),
      p(class = "aide mt-4",
        L("Méthode : Drummond et al. (2015), guide OMS-CHOICE (2003), iDSI Reference Case (2016).",
          "Method: Drummond et al. (2015), WHO-CHOICE guide (2003), iDSI Reference Case (2016)."))
    )
  })
}

shinyApp(ui, server)
