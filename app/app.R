library(shiny)
library(bslib)

source("calcul.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Langues ----------------------------------------------------------------
#
# tr() crée deux <span> (français / anglais) affichés selon la classe du <body> ;
# le bouton de langue bascule cette classe et transmet la langue au serveur
# (input$langue), qui traduit résultats, graphiques et rapport avec L().

tr <- function(fr, en) tagList(span(class = "t-fr", fr), span(class = "t-en", en))

placeholders <- list(
  "^nom$" = c("ex. Triage numérique Phase 3", "e.g. Digital triage Phase 3"),
  "^organisation$" = c("ex. ONG, district sanitaire…", "e.g. NGO, health district…"),
  "^pays$" = c("Nom du pays", "Country name"),
  "^description$" = c("Ce que fait l'innovation ; d'où viennent vos chiffres…",
                      "What the innovation does; where your figures come from…"),
  "^(a|c)_libelle_[0-9]+$" = c("Libellé de la ligne", "Line description")
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

# Libellés des listes déroulantes (traduits côté serveur).
choix_statut_a <- function(en) setNames(c("connu", "estime", "attente"),
  if (en) c("Committed", "Indicative", "Pending") else c("Engagé", "Indicatif", "En attente"))
choix_statut_c <- function(en) setNames(c("connu", "estime", "attente", "hors_budget"),
  if (en) c("Known", "Estimated", "Pending", "Not in budget") else c("Connu", "Estimé", "En attente", "Hors budget"))
choix_nature <- function(en) setNames(c("financier", "economique"),
  if (en) c("Cash", "Opport.") else c("Décaissé", "Opport."))
choix_base <- function(en) setNames(bases_c,
  if (en) c("Fixed per year", "per beneficiary / month", "per CHW / month", "per MAU / month", "per interaction",
            "per escalation", "per resolved follow-up") else
    c("Fixe par an", "par bénéficiaire / mois", "par ASC / mois", "par MAU / mois", "par interaction",
      "par escalade", "par suivi résolu"))

# ---- Pays -------------------------------------------------------------------
# PIB par habitant indicatif (Banque mondiale ~2023-2024), en USD et en monnaie locale.
pays_defauts <- list(
  BF = list(fr = "Burkina Faso", en = "Burkina Faso", monnaie = "XOF", pib_usd = 900, pib_local = 520000),
  KE = list(fr = "Kenya", en = "Kenya", monnaie = "KES", pib_usd = 2200, pib_local = 284000),
  UG = list(fr = "Ouganda", en = "Uganda", monnaie = "UGX", pib_usd = 1100, pib_local = 4000000)
)
choix_pays <- c("Burkina Faso" = "BF", "Kenya" = "KE", "Ouganda / Uganda" = "UG", "Autre / Other" = "autre")

# ---- Style ------------------------------------------------------------------

couleurs <- list(sarcelle = "#0f5257", or = "#c08a2e", vert = "#1d7a4f", rouge = "#a23b2a",
                 gris = "#6c7a80", encre = "#1f2a2e", bleu_a = "#8fbfdc", bleu_c = "#1f5f9e", orange = "#c4580f")

css <- "
body.lang-en .t-fr, body:not(.lang-en) .t-en { display: none !important; }
:root { --encre: #1f2a2e; --sarcelle: #0f5257; --or: #c08a2e; --papier: #faf7f2;
  --couche-ab: #8fbfdc; --couche-c: #1f5f9e; --couche-c-entete: #c4580f; }
body { background: var(--papier); }
.navbar { background: #ffffff !important; border-bottom: 1px solid #e8e1d4; }
.navbar-brand { font-weight: 600; letter-spacing: .02em; color: var(--sarcelle) !important; }
.nav-link { font-size: 1rem; padding-left: .5rem !important; padding-right: .5rem !important; }
.nav-link.active { color: var(--sarcelle) !important; border-bottom: 2px solid var(--or); }
#bouton_langue { border-radius: 20px; padding: .2rem .9rem; margin-left: .5rem; }
h1, h2, h3, h4 { color: var(--sarcelle); font-weight: 600; }
h3.section { margin: 2rem 0 1rem; padding-bottom: .4rem; border-bottom: 1px solid #e3d9c6; }
.card { border: none; border-radius: 14px; box-shadow: 0 2px 16px rgba(15, 82, 87, .08); }
.card-header { background: transparent; border-bottom: 1px solid #efe8dc; font-size: 1.15rem;
  font-weight: 600; color: var(--sarcelle); }
.hero { background: linear-gradient(135deg, #0f5257 0%, #1d7874 55%, #2f8f83 80%, #c08a2e 140%);
  color: #fff; border-radius: 20px; padding: 3rem; margin-bottom: 1.6rem; }
.hero h1 { color: #fff; font-size: 2.6rem; margin-bottom: 1rem; }
.hero p { font-size: 1.25rem; max-width: 54rem; opacity: .95; }
.hero .btn { font-size: 1.05rem; padding: .55rem 1.3rem; border-radius: 30px; margin: .7rem .6rem 0 0; }
.btn-or { background: var(--or); color: #fff; border: none; }
.btn-or:hover { background: #a8761f; color: #fff; }
.btn-blanc { background: transparent; color: #fff; border: 1px solid rgba(255,255,255,.7); }
.btn-blanc:hover { background: rgba(255,255,255,.12); color: #fff; }
.couche { border-radius: 14px; overflow: hidden; height: 100%; box-shadow: 0 2px 16px rgba(15,82,87,.08); background: #fff; }
.couche .entete { padding: .9rem 1.2rem; letter-spacing: .16em; font-weight: 600; display: flex;
  justify-content: space-between; align-items: baseline; gap: 1rem; }
.couche .entete small { letter-spacing: 0; font-weight: 600; font-size: .95rem; }
.couche .corps { padding: 1.1rem 1.2rem; }
.couche-a .entete, .couche-b .entete { background: var(--couche-ab); color: #10324a; }
.couche-c .entete { background: var(--couche-c-entete); color: #fff; }
.couche-c .corps { background: var(--couche-c); color: #fff; }
.couche-c .corps h4 { color: #fff; }
.niveau { border-left: 4px solid var(--or); padding: 1rem 1.2rem; }
.niveau .num { color: var(--or); font-weight: 700; font-size: 1.4rem; }
.ligne-entete { font-weight: 600; color: #4b5a60; font-size: .9rem; padding: 0 .75rem .3rem; }
.ligne-poste { background: #fff; border-radius: 10px; padding: .55rem .75rem .1rem; margin-bottom: .45rem;
  box-shadow: 0 1px 6px rgba(15,82,87,.06); color: var(--encre); }
.ligne-poste .form-group { margin-bottom: .45rem; }
.ligne-poste .form-control, .ligne-poste .form-select { padding: .3rem .5rem; }
.ligne-poste .checkbox { margin-top: .35rem; }
.statut { display: inline-block; border-radius: 12px; padding: .05rem .6rem; font-size: .85rem; font-weight: 600; }
.statut-connu { background: #dcefe3; color: #1d6b43; }
.statut-estime { background: #f6ead2; color: #8a5a10; }
.statut-attente { background: #e9edf0; color: #4b5a60; }
.statut-hors_budget { background: #f6dcd6; color: #8e2f1f; }
.partiel { display: inline-block; background: #fff1d6; color: #8a5a10; border-radius: 12px; padding: .1rem .7rem; font-weight: 600; }
.chaine td { padding: .45rem .6rem; border-bottom: 1px solid #efe8dc; }
.chaine .barre { height: 1.3rem; background: var(--sarcelle); border-radius: 4px; }
.chaine .formule-chaine { font-family: Georgia, serif; color: #5b6a6e; }
.formule-ligne { font-family: Georgia, serif; background: #f6f1e7; border-radius: 8px; padding: .55rem .9rem;
  margin: .3rem 0; font-variant-numeric: lining-nums; }
.numerateur { background: var(--couche-c); color: #fff; border-radius: 14px; padding: 1.3rem 1.6rem; height: 100%; }
.numerateur h4 { color: #fff; letter-spacing: .14em; font-size: 1rem; }
.numerateur .r { font-size: 1.5rem; font-family: Georgia, serif; margin: .4rem 0; }
.kpi { background: #fff !important; border: none !important; border-top: 3px solid var(--or) !important;
  box-shadow: 0 2px 16px rgba(15, 82, 87, .08); }
.kpi .value-box-title { color: #5b6a6e; font-size: 1rem; }
.kpi .value-box-value { color: var(--sarcelle); font-weight: 600; font-size: 1.7rem; white-space: nowrap;
  font-variant-numeric: lining-nums tabular-nums; }
.kpi p { color: #6c7a80; font-size: .93rem; margin: 0; }
.verdict { border-radius: 18px; color: #fff; padding: 1.8rem 2.2rem; margin-bottom: 1rem; }
.verdict .chiffre { font-size: 2.5rem; font-weight: 600; line-height: 1.15; font-variant-numeric: lining-nums; }
.verdict .unite { font-size: 1.2rem; opacity: .9; }
.verdict .phrase { font-size: 1.25rem; margin-top: .5rem; }
.verdict .detail { opacity: .9; margin-top: .3rem; }
.alerte { background: #fff6e6; border-left: 4px solid var(--or); border-radius: 8px; padding: .8rem 1.1rem; margin-bottom: 1rem; }
table { font-variant-numeric: lining-nums tabular-nums; }
.table > :not(caption) > * > * { padding: .45rem .65rem; }
.table-q5 thead th { background: var(--couche-c); color: #fff; letter-spacing: .06em; }
.table-q5 .code { color: var(--couche-c); font-weight: 700; }
.table-q5 .groupe { background: #f6f1e7; color: var(--sarcelle); font-weight: 600; }
.aide { color: #6c7a80; font-size: .97rem; }
.navigation { display: flex; justify-content: space-between; margin: 1rem 0 2rem; }
.rapport { background: #fff; padding: 2.5rem 3rem; border-radius: 14px; box-shadow: 0 2px 16px rgba(15,82,87,.08); }
.rapport h2 { border-bottom: 2px solid var(--or); padding-bottom: .4rem; }
.rapport h4 { margin-top: 1.5rem; }
.formule { font-family: Georgia, serif; background: #f6f1e7; border-radius: 8px; padding: .4rem .8rem;
  display: inline-block; margin: .2rem 0 .5rem; }
@media (max-width: 640px) {
  .navbar-brand { font-size: 1rem; white-space: normal; }
  .hero { padding: 1.8rem 1.3rem; border-radius: 14px; }
  .hero h1 { font-size: 1.9rem; }
  .verdict .chiffre { font-size: 1.9rem; }
  .rapport { padding: 1.4rem; }
  .ligne-entete { display: none; }
}
@media print {
  .navbar, .no-print, .navigation { display: none !important; }
  body { background: #fff; }
  .rapport { box-shadow: none; padding: 0; }
}
"

# ---- Composants -------------------------------------------------------------

navigation <- function(precedent = NULL, suivant = NULL, label_suivant = tr("Étape suivante →", "Next step →")) {
  div(class = "navigation",
      if (!is.null(precedent)) actionButton(precedent, tr("← Retour", "← Back"), class = "btn-outline-secondary") else span(),
      if (!is.null(suivant)) actionButton(suivant, label_suivant, class = "btn-primary") else span())
}

tableau_html <- function(df, classe = "table table-sm table-striped") {
  tags$table(class = classe,
             tags$thead(tags$tr(lapply(names(df), tags$th))),
             tags$tbody(lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(df[i, ], function(x) tags$td(x))))))
}

# Ligne d'une couche A ou C (sans libellés : un en-tête commun les porte).
ligne_poste <- function(couche, id, v = NULL, en = FALSE) {
  val <- function(col, defaut) {
    x <- if (!is.null(v)) v[[col]] else NULL
    if (is.null(x) || (length(x) == 1 && is.na(x))) defaut else x
  }
  i <- function(nom) paste0(couche, "_", nom, "_", id)
  choix <- function(choices, col, defaut) selectInput(i(col), NULL, choices, val(col, defaut), selectize = FALSE, width = "100%")
  cellules <- if (couche == "a") list(
    column(1, textInput(i("code"), NULL, val("code", ""), width = "100%")),
    column(6, textInput(i("libelle"), NULL, val("libelle", ""), width = "100%")),
    column(2, numericInput(i("montant"), NULL, val("montant", NA), min = 0, width = "100%")),
    column(2, choix(choix_statut_a(en), "statut", "estime"))
  ) else list(
    column(1, textInput(i("code"), NULL, val("code", ""), width = "100%")),
    column(3, textInput(i("libelle"), NULL, val("libelle", ""), width = "100%")),
    column(2, numericInput(i("montant"), NULL, val("montant", NA), min = 0, width = "100%")),
    column(2, choix(choix_base(en), "base", "fixe")),
    column(2, choix(choix_statut_c(en), "statut", "attente")),
    column(1, choix(choix_nature(en), "nature", "financier"))
  )
  suppr <- actionButton(i("suppr"), "✕", class = "btn btn-sm btn-outline-danger", title = "Supprimer / Delete")
  derniere <- if (couche == "a") column(1, suppr) else
    column(1, div(class = "d-flex align-items-center gap-2",
                  checkboxInput(i("gratuit"), NULL, isTRUE(val("gratuit", FALSE))), suppr))
  div(id = paste0(couche, "_ligne_", id), class = "ligne-poste",
      fluidRow(cellules, derniere),
      tags$script("appliquerLangue(langueCourante());"))
}

entete_a <- fluidRow(class = "ligne-entete",
  column(1, "Code"), column(6, tr("Ligne", "Line")), column(2, tr("Montant", "Amount")), column(2, tr("Statut", "Status")))
entete_c <- fluidRow(class = "ligne-entete", style = "color:#fff",
  column(1, "Code"), column(3, tr("Ligne", "Line")), column(2, tr("Montant (annuel ou unitaire)", "Amount (annual or unit)")),
  column(2, tr("Base", "Basis")), column(2, tr("Statut", "Status")), column(1, tr("Nature", "Type")),
  column(1, tr("Gratuit", "Free")))

police <- font_collection("EB Garamond", "Garamond", "Georgia", "serif")

pct_input <- function(id, label, valeur, ...) numericInput(id, label, valeur, min = 0, max = 100, ...)

# ---- Méthode (bilingue) -------------------------------------------------------

formule <- function(x) span(class = "formule", x)

references <- tags$ol(
  tags$li("Drummond MF, Sculpher MJ, Claxton K, Stoddart GL, Torrance GW. Methods for the Economic Evaluation of Health Care Programmes, 4th ed. Oxford University Press, 2015."),
  tags$li("Tan-Torres Edejer T, et al. Making Choices in Health: WHO Guide to Cost-Effectiveness Analysis. WHO, 2003."),
  tags$li("Wilkinson T, Sculpher MJ, Claxton K, et al. The International Decision Support Initiative Reference Case for Economic Evaluation. Value in Health 2016;19(8):921–928."),
  tags$li("Husereau D, et al. CHEERS 2022 Statement. Value in Health 2022;25(1):3–9."),
  tags$li("Weinstein MC, Stason WB. Foundations of cost-effectiveness analysis for health and medical practices. NEJM 1977;296:716–721."),
  tags$li("Murray CJL. Quantifying the burden of disease: the technical basis for DALYs. Bull World Health Organ 1994;72(3):429–445."),
  tags$li("Stinnett AA, Mullahy J. Net health benefits. Med Decis Making 1998;18(2 Suppl):S68–S80."),
  tags$li("Bertram MY, et al. Cost-effectiveness thresholds: pros and cons. Bull World Health Organ 2016;94:925–930."),
  tags$li("Woods B, Revill P, Sculpher M, Claxton K. Country-level cost-effectiveness thresholds. Value in Health 2016;19(8):929–935."),
  tags$li("Ochalek J, Lomas J, Claxton K. Estimating health opportunity costs in LMICs. BMJ Global Health 2018;3:e000964.")
)

methode_fr <- div(class = "t-fr",
  h3("Structure du modèle"),
  tags$ul(
    tags$li(strong("Couche A — développement non récurrent"), ", exclue de R. Pour la vue économique, elle est annualisée : ",
            formule("A annuel = A / facteur d'annuité"), ", ", formule("facteur = (1 − (1 + r)⁻ⁿ) / r"),
            " (OMS-CHOICE). Ex. 190 000 sur 5 ans à 3 % : facteur 4,58 → 41 487 par an."),
    tags$li(strong("Couche B — chaîne de volumes"), " : ", formule("MAU = B1 × B3"), " ", formule("I = MAU × B4"), " ",
            formule("E = I × B5"), " ", formule("F = E × B6"),
            ". A (épisodes avec action vérifiée) compte chaque épisode de soins une fois, sur toutes les voies : il est mesuré séparément."),
    tags$li(strong("Couche C — numérateur partagé"), " : ", formule("R = C1 + C2 + … + C9"),
            " pour la période de rapport (montants annuels convertis), hors couche A. Une ligne peut être un montant fixe ou un coût par unité de volume. ",
            "Une estimation avec des lignes manquantes est signalée « partielle ».")
  ),
  h3("Indicateurs Q5"),
  tags$ul(
    tags$li(strong("OC5a"), " coût par utilisateur actif mensuel = R / MAU"),
    tags$li(strong("OC5b"), " coût par interaction = R / I"),
    tags$li(strong("OC5c"), " coût par épisode avec action vérifiée = R / A"),
    tags$li(strong("OC5d"), " coût par suivi résolu = R / F"),
    tags$li(strong("OC5e"), " coût par bénéficiaire de l'accès gratuit = (C4 + C5) / bénéficiaires atteints — composante de R, non additive.")
  ),
  h3("Trois niveaux de coût-efficacité"),
  tags$ol(
    tags$li(strong("Coût par output"), " : par MAU, interaction, escalade (OC5a, OC5b)."),
    tags$li(strong("Coût par résultat intermédiaire"), " : par suivi résolu, par épisode d'action vérifié (OC5c, OC5d)."),
    tags$li(strong("Coût par résultat de santé"), " : par DALY ou décès évité, face au statu quo : ",
            formule("RCEI = (Coût intervention − Coût comparateur) / (Effet intervention − Effet comparateur)"))
  ),
  h3("Vues de coût"),
  tags$ul(
    tags$li(strong("Lignes connues"), " : plancher, pas une estimation."),
    tags$li(strong("R (Q5)"), " : toutes les lignes chiffrées, y compris le temps des ASC (coût d'opportunité) et les lignes hors budget chiffrées ; couche A exclue."),
    tags$li(strong("R + A annualisé"), " : vue économique complète. Montrez les deux versions.")
  ),
  h3("Points méthodologiques"),
  tags$ul(
    tags$li("Les lignes hors budget (C4, C5, C8) existent quand même : chiffrez-les ou signalez-les comme exclues. L'accès gratuit est obligatoire et la supervision clinique porte le modèle de sécurité."),
    tags$li("B3, B4, B5 et B6 sont des hypothèses : sensibilité univariée au minimum. Une activation de 25 % au lieu de 50 % double le coût unitaire si R est fixe."),
    tags$li("Le volume ne prouve pas l'impact : mesurez A et estimez, face au comparateur, combien de suivis résolus évitent une complication."),
    tags$li("Seuil : l'ancien repère OMS 1–3 × PIB/habitant n'est plus recommandé (Bertram et al., 2016). Comparez à des interventions alternatives financées par le même bailleur ou à des seuils fondés sur le coût d'opportunité (Woods et al., 2016 ; Ochalek et al., 2018).")
  ),
  h4("Références"))

methode_en <- div(class = "t-en",
  h3("Model structure"),
  tags$ul(
    tags$li(strong("Layer A — non-recurrent development"), ", excluded from R. For the economic view it is annualised: ",
            formule("annual A = A / annuity factor"), ", ", formule("factor = (1 − (1 + r)⁻ⁿ) / r"),
            " (WHO-CHOICE). E.g. 190,000 over 5 years at 3%: factor 4.58 → 41,487 per year."),
    tags$li(strong("Layer B — volume chain"), ": ", formule("MAU = B1 × B3"), " ", formule("I = MAU × B4"), " ",
            formule("E = I × B5"), " ", formule("F = E × B6"),
            ". A (episodes with verified action) counts each care episode once across all pathways: it is measured separately."),
    tags$li(strong("Layer C — shared numerator"), ": ", formule("R = C1 + C2 + … + C9"),
            " for the reporting period (annual inputs converted), excluding layer A. A line can be a fixed amount or a cost per unit of volume. ",
            "Estimates with missing cost lines are reported as partial.")
  ),
  h3("Q5 indicators"),
  tags$ul(
    tags$li(strong("OC5a"), " cost per monthly active user = R / MAU"),
    tags$li(strong("OC5b"), " cost per interaction = R / I"),
    tags$li(strong("OC5c"), " cost per episode with verified action = R / A"),
    tags$li(strong("OC5d"), " cost per resolved follow-up = R / F"),
    tags$li(strong("OC5e"), " cost per beneficiary of free access = (C4 + C5) / beneficiaries reached — a component of R, not additive.")
  ),
  h3("Three levels of cost-effectiveness"),
  tags$ol(
    tags$li(strong("Cost per output"), ": per MAU, interaction, escalation (OC5a, OC5b)."),
    tags$li(strong("Cost per intermediate outcome"), ": per resolved follow-up, per verified action episode (OC5c, OC5d)."),
    tags$li(strong("Cost per health outcome"), ": per DALY or death averted, versus the status quo: ",
            formule("ICER = (Intervention cost − Comparator cost) / (Intervention effect − Comparator effect)"))
  ),
  h3("Cost views"),
  tags$ul(
    tags$li(strong("Known lines"), ": a floor, not an estimate."),
    tags$li(strong("R (Q5)"), ": all costed lines, including CHW time (opportunity cost) and costed not-in-budget lines; layer A excluded."),
    tags$li(strong("R + annualised A"), ": full economic view. Show both versions.")
  ),
  h3("Methodological points"),
  tags$ul(
    tags$li("Not-in-budget lines (C4, C5, C8) still exist: cost them or flag them as excluded. Free access is mandatory and clinical supervision is the backbone of the safety model."),
    tags$li("B3, B4, B5 and B6 are assumptions: one-way sensitivity at least. A 25% activation instead of 50% doubles the unit cost if R is fixed."),
    tags$li("Volume does not prove impact: measure A and estimate, versus the comparator, how many resolved follow-ups avert a complication."),
    tags$li("Threshold: the former WHO 1–3 × GDP per capita benchmark is no longer recommended (Bertram et al., 2016). Compare with alternative interventions funded by the same donor or opportunity-cost-based thresholds (Woods et al., 2016; Ochalek et al., 2018).")
  ),
  h4("References"))

# ---- Interface --------------------------------------------------------------

ui <- page_navbar(
  id = "onglets",
  title = tr("Coût-efficacité · Q5", "Cost-effectiveness · Q5"),
  window_title = "Coût-efficacité · Cost-effectiveness",
  fillable = FALSE,
  theme = bs_theme(version = 5, bg = "#faf7f2", fg = "#1f2a2e", primary = "#0f5257",
                   secondary = "#c08a2e", base_font = police, heading_font = police, font_scale = 1.06),
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
        h1(tr("Coûts et coût-efficacité de l'innovation", "Costs and cost-effectiveness of the innovation")),
        p(tr("Couche A : investissement. Couche B : volumes (dénominateurs). Couche C : coût récurrent (numérateur R). ",
             "Layer A: investment. Layer B: volumes (denominators). Layer C: recurrent cost (numerator R). "),
          tr("L'outil calcule les cinq indicateurs Q5, la vue économique et, avec les effets de santé, le RCEI.",
             "The tool computes the five Q5 indicators, the economic view and, with health effects, the ICER.")),
        actionButton("charger_reference", tr("Données de l'innovation (Phase 3)", "Innovation data (Phase 3)"), class = "btn-or"),
        actionButton("charger_estimations", tr("Données + estimations indicatives", "Data + indicative estimates"), class = "btn-blanc"),
        actionButton("demarrer_vide", tr("Projet vide", "Blank project"), class = "btn-blanc")),
    layout_columns(
      col_widths = c(4, 4, 4),
      div(class = "couche couche-a",
          div(class = "entete", "LAYER A", tags$small(tr("Exclue des coûts unitaires", "Excluded from unit costs"))),
          div(class = "corps", h4(tr("Développement non récurrent", "Non-recurrent development")),
              p(tr("Annualisé seulement dans la vue économique.", "Annualised only in the economic view.")))),
      div(class = "couche couche-b",
          div(class = "entete", "LAYER B", tags$small(tr("Scénario central Phase 3", "Phase 3 central scenario"))),
          div(class = "corps", h4(tr("Volumes d'activité", "Activity volumes")),
              p("MAU = B1 × B3 · I = MAU × B4 · E = I × B5 · F = E × B6"))),
      div(class = "couche couche-c",
          div(class = "entete", "LAYER C", tags$small(tr("Base de chaque indicateur Q5", "Basis of every Q5 indicator"))),
          div(class = "corps", h4(tr("Coût récurrent annuel à l'échelle", "Recurrent operating cost, annual at scale")),
              p("R = C1 + C2 + … + C9")))
    ),
    h3(class = "section", tr("Trois niveaux de coût-efficacité", "Three levels of cost-effectiveness")),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(div(class = "niveau", span(class = "num", "1"), h4(tr("Coût par output", "Cost per output")),
               p(tr("OC5a par MAU, OC5b par interaction. Calculable dès maintenant.", "OC5a per MAU, OC5b per interaction. Computable now.")))),
      card(div(class = "niveau", span(class = "num", "2"), h4(tr("Coût par résultat intermédiaire", "Cost per intermediate outcome")),
               p(tr("OC5d par suivi résolu, OC5c par épisode d'action vérifié.", "OC5d per resolved follow-up, OC5c per verified action episode.")))),
      card(div(class = "niveau", span(class = "num", "3"), h4(tr("Coût par résultat de santé", "Cost per health outcome")),
               p(tr("Par DALY ou décès évité, face au statu quo (RCEI).", "Per DALY or death averted, versus the status quo (ICER)."))))
    )
  ),

  nav_panel(
    tr("1 · Projet", "1 · Project"), value = "projet",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(tr("Identité de l'innovation", "Innovation details")),
        textInput("nom", tr("Nom", "Name"), ""),
        textInput("organisation", tr("Organisation", "Organisation"), ""),
        layout_column_wrap(
          width = "180px",
          selectInput("pays_choix", tr("Pays", "Country"), choix_pays, "autre"),
          textInput("monnaie", tr("Monnaie", "Currency"), "USD")
        ),
        conditionalPanel("input.pays_choix == 'autre'", textInput("pays", tr("Autre pays", "Other country"), "")),
        textAreaInput("description", tr("Description et sources des données", "Description and data sources"), "", rows = 3)
      ),
      card(
        card_header(tr("Paramètres", "Parameters")),
        layout_column_wrap(
          width = "200px",
          selectInput("periode", tr("Période de rapport", "Reporting period"),
                      c("Mois / Month" = "mois", "Année / Year" = "an"), "mois"),
          numericInput("taux", tr("Taux d'actualisation (%)", "Discount rate (%)"), 3, min = 0, max = 20, step = 0.5),
          numericInput("duree_vie", tr("Durée de vie de la couche A (ans)", "Lifetime of layer A (years)"), 5, min = 1, max = 30)
        ),
        div(class = "aide mb-3", tr("Les montants annuels de la couche C sont convertis à la période de rapport. Taux et durée de vie servent à annualiser la couche A (vue économique).",
                                    "Annual layer C amounts are converted to the reporting period. Rate and lifetime annualise layer A (economic view).")),
        layout_column_wrap(
          width = "200px",
          numericInput("seuil", tr("Seuil par DALY évitée", "Threshold per DALY averted"), NA, min = 0),
          numericInput("pib_hab", tr("PIB par habitant (monnaie du projet)", "GDP per capita (project currency)"), NA, min = 0)
        ),
        actionButton("proposer_seuil", tr("Proposer un seuil = 0,5 × PIB par habitant", "Suggest a threshold = 0.5 × GDP per capita"),
                     class = "btn-outline-primary btn-sm"),
        div(class = "aide mt-2",
            tr("L'ancien repère OMS (1 à 3 × PIB par habitant) n'est plus recommandé. Préférez le seuil du bailleur, ou le coût par DALY d'interventions alternatives qu'il finance.",
               "The former WHO benchmark (1 to 3 × GDP per capita) is no longer recommended. Prefer the funder's threshold, or the cost per DALY of alternative interventions it funds."))
      )
    ),
    card(
      card_header(tr("Enregistrer ou reprendre un projet", "Save or reopen a project")),
      layout_columns(
        col_widths = c(6, 6),
        downloadButton("sauver", tr("Enregistrer le projet (.json)", "Save project (.json)"), class = "btn-outline-primary"),
        fileInput("ouvrir", NULL, accept = ".json", buttonLabel = tr("Ouvrir un projet…", "Open a project…"), placeholder = ".json")
      )
    ),
    navigation("retour_accueil", "vers_a")
  ),

  nav_panel(
    tr("2 · Couche A", "2 · Layer A"), value = "couche_a",
    div(class = "couche couche-a mb-3",
        div(class = "entete", "LAYER A", tags$small(tr("Exclue des coûts unitaires", "Excluded from unit costs"))),
        div(class = "corps",
            h4(tr("Développement non récurrent", "Non-recurrent development")),
            p(class = "aide", tr("« En attente » = montant pas encore connu (non compté).", "“Pending” = amount not yet known (not counted).")),
            entete_a, div(id = "liste_a"),
            actionButton("ajouter_a", tr("+ Ajouter une ligne", "+ Add a line"), class = "btn-or btn-sm mt-1"))),
    uiOutput("resume_a"),
    navigation("retour_projet", "vers_b")
  ),

  nav_panel(
    tr("3 · Couche B", "3 · Layer B"), value = "couche_b",
    layout_columns(
      col_widths = c(5, 7),
      div(class = "couche couche-b",
          div(class = "entete", "LAYER B", tags$small(tr("Volumes mensuels", "Monthly volumes"))),
          div(class = "corps",
              numericInput("b_beneficiaires", tr("B1 · Bénéficiaires couverts", "B1 · Beneficiaries covered"), 30000, min = 0),
              numericInput("b_asc", tr("B2 · ASC soutenus", "B2 · CHWs supported"), 300, min = 0),
              pct_input("b_activation", tr("B3 · Taux d'activation (%) — hypothèse", "B3 · Activation rate (%) — assumption"), 50),
              numericInput("b_interactions", tr("B4 · Interactions par utilisateur actif (par mois) — hypothèse",
                                                "B4 · Interactions per active user (per month) — assumption"), 2, min = 0, step = 0.1),
              pct_input("b_escalade", tr("B5 · Taux d'escalade (%) — hypothèse", "B5 · Escalation rate (%) — assumption"), 10),
              pct_input("b_completion", tr("B6 · Complétion des suivis d'escalade (%) — hypothèse de planification",
                                           "B6 · Escalated follow-up completion (%) — planning assumption"), 75),
              numericInput("b_episodes", tr("A · Épisodes avec action vérifiée par mois — mesuré", "A · Episodes with verified action per month — measured"),
                           NA, min = 0),
              numericInput("b_atteints", tr("Bénéficiaires atteints par l'accès gratuit, par mois (vide = MAU)",
                                            "Beneficiaries reached by free access, per month (blank = MAU)"), NA, min = 0),
              div(class = "aide", tr("A compte chaque épisode de soins une fois quand un ASC ou un bénéficiaire réalise une action qualifiante vérifiée, sur toutes les voies. Il doit être mesuré séparément.",
                                     "A counts each care episode once when a CHW or beneficiary completes a verified qualifying action, across all pathways. It must be measured separately.")))),
      card(card_header(tr("Chaîne de volumes", "Volume chain")), uiOutput("chaine"))
    ),
    navigation("retour_a", "vers_c")
  ),

  nav_panel(
    tr("4 · Couche C", "4 · Layer C"), value = "couche_c",
    div(class = "couche couche-c mb-3",
        div(class = "entete", "LAYER C", tags$small(tr("Base de chaque indicateur Q5", "Basis of every Q5 indicator"))),
        div(class = "corps",
            h4(tr("Coût récurrent annuel à l'échelle", "Recurrent operating cost, annual at scale")),
            p(tr("Montant fixe par an, ou coût par unité de volume (ex. SMS par interaction) : les coûts variables suivent alors la couche B. ",
                 "Fixed amount per year, or cost per unit of volume (e.g. SMS per interaction): variable costs then follow layer B. "),
              tr("« Opportunité » = coût non décaissé (ex. temps des ASC). Cochez « accès gratuit » pour C4 et C5 (numérateur d'OC5e).",
                 "“Opportunity” = cost not paid out (e.g. CHW time). Tick “free access” for C4 and C5 (OC5e numerator).")),
            entete_c, div(id = "liste_c"),
            actionButton("ajouter_c", tr("+ Ajouter une ligne", "+ Add a line"), class = "btn-or btn-sm mt-1"))),
    uiOutput("resume_c"),
    navigation("retour_b", "vers_sante")
  ),

  nav_panel(
    tr("5 · Effets", "5 · Effects"), value = "sante",
    card(
      card_header(tr("Niveau 3 · Effets de santé face au statu quo", "Level 3 · Health effects versus the status quo")),
      p(tr("Le volume ne prouve pas l'impact. Indiquez, par rapport au statu quo sans l'outil, combien de suivis résolus évitent réellement une complication. ",
           "Volume does not prove impact. Enter, versus the status quo without the tool, how many resolved follow-ups actually avert a complication. "),
        tr("Laissez 0 tant que ces données n'existent pas : les indicateurs Q5 restent calculés.",
           "Leave 0 until these data exist: Q5 indicators are still computed.")),
      layout_column_wrap(
        width = "230px",
        numericInput("s_compl", tr("Complications évitées pour 1 000 suivis résolus", "Complications averted per 1,000 resolved follow-ups"), 0, min = 0),
        numericInput("s_daly", tr("DALY par complication évitée", "DALYs per complication averted"), 0, min = 0, step = 0.01),
        numericInput("s_deces", tr("Décès évités pour 1 000 suivis résolus", "Deaths averted per 1,000 resolved follow-ups"), 0, min = 0, step = 0.1),
        numericInput("s_economie", tr("Économies par complication évitée", "Savings per complication averted"), 0, min = 0),
        numericInput("s_comparateur", tr("Coût annuel du comparateur (statu quo)", "Annual cost of the comparator (status quo)"), 0, min = 0)
      ),
      div(class = "aide", tr("RCEI = (coût de l'intervention − coût du comparateur) / effet incrémental, sur la période de rapport.",
                             "ICER = (intervention cost − comparator cost) / incremental effect, over the reporting period."))
    ),
    navigation("retour_c", "vers_sensibilite")
  ),

  nav_panel(
    tr("6 · Sensibilité", "6 · Sensitivity"), value = "sensibilite",
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header(tr("Plages testées", "Ranges tested")),
        layout_column_wrap(
          width = "130px",
          pct_input("sens_activation_bas", tr("B3 bas (%)", "B3 low (%)"), 25),
          pct_input("sens_activation_haut", tr("B3 haut (%)", "B3 high (%)"), 75),
          numericInput("sens_interactions_bas", tr("B4 bas", "B4 low"), 1, min = 0, step = 0.1),
          numericInput("sens_interactions_haut", tr("B4 haut", "B4 high"), 3, min = 0, step = 0.1),
          pct_input("sens_escalade_bas", tr("B5 bas (%)", "B5 low (%)"), 5),
          pct_input("sens_escalade_haut", tr("B5 haut (%)", "B5 high (%)"), 15),
          pct_input("sens_completion_bas", tr("B6 bas (%)", "B6 low (%)"), 50),
          pct_input("sens_completion_haut", tr("B6 haut (%)", "B6 high (%)"), 90),
          numericInput("sens_duree_bas", tr("Durée de vie A basse", "Lifetime A low"), 3, min = 1),
          numericInput("sens_duree_haut", tr("Durée de vie A haute", "Lifetime A high"), 7, min = 1),
          numericInput("sens_couts_bas", tr("Coûts C bas (% de la base)", "Layer C low (% of base)"), 75, min = 0),
          numericInput("sens_couts_haut", tr("Coûts C hauts (% de la base)", "Layer C high (% of base)"), 150, min = 0)
        ),
        uiOutput("choix_sensibilite")
      ),
      card(card_header(tr("Analyse univariée (tornade)", "One-way analysis (tornado)")),
           plotOutput("tornade", height = "340px"), tableOutput("table_sensibilite"))
    ),
    navigation("retour_sante", "vers_resultats", tr("Voir les indicateurs →", "See the indicators →"))
  ),

  nav_panel(
    tr("Indicateurs", "Indicators"), value = "resultats",
    uiOutput("bandeau"),
    uiOutput("alertes"),
    card(card_header(tr("Les cinq indicateurs Q5", "The five Q5 indicators")), uiOutput("table_q5")),
    layout_columns(
      col_widths = c(6, 6),
      uiOutput("numerateur"),
      card(card_header(tr("Composition de R + A annualisé", "Composition of R + annualised A")),
           plotOutput("composition", height = "320px"))
    ),
    card(card_header(tr("Autres coûts unitaires et niveau santé", "Other unit costs and health level")), uiOutput("table_autres"),
         div(class = "no-print", downloadButton("telecharger", tr("Télécharger (.csv)", "Download (.csv)"), class = "btn-outline-primary btn-sm"))),
    div(class = "text-end mb-4 no-print", actionButton("vers_rapport", tr("Voir le rapport →", "See the report →"), class = "btn-primary"))
  ),

  nav_panel(
    tr("Rapport", "Report"), value = "rapport",
    div(class = "text-end mb-3 no-print",
        tags$button(class = "btn btn-or", onclick = "window.print()", tr("Imprimer / PDF", "Print / PDF"))),
    div(class = "rapport", uiOutput("rapport"))
  ),

  nav_panel(
    tr("Méthode", "Method"), value = "methode",
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
  # Décimales adaptées à l'ordre de grandeur (0,028 ; 0,37 ; 33,3 ; 41 487).
  fmt_auto <- function(x) {
    if (is.null(x) || length(x) != 1 || is.na(x) || !is.finite(x)) return("—")
    a <- abs(x)
    fmt(x, if (a >= 1000) 0 else if (a >= 10) 1 else if (a >= 0.1) 2 else 3)
  }
  fmt_pct <- function(x) if (is.null(x) || is.na(x)) "—" else paste0(fmt(100 * x), L(" %", "%"))

  num <- function(id, defaut = 0) {
    v <- input[[id]]
    if (is.null(v) || length(v) != 1 || is.na(v)) defaut else as.numeric(v)
  }
  txt <- function(id) trimws(input[[id]] %||% "")
  monnaie <- reactive(txt("monnaie"))
  argent <- function(x, auto = TRUE) if (is.null(x) || is.na(x) || !is.finite(x)) "—" else
    paste(if (auto) fmt_auto(x) else fmt(x), monnaie())
  periode_txt <- function() if (identical(input$periode, "an")) L("par an", "per year") else L("par mois", "per month")

  pays_nom <- reactive({
    code <- input$pays_choix %||% "autre"
    if (code %in% names(pays_defauts)) pays_defauts[[code]][[if (en()) "en" else "fr"]] else txt("pays")
  })

  observeEvent(input$pays_choix, {
    d <- pays_defauts[[input$pays_choix]]
    if (is.null(d)) return()
    if (identical(toupper(txt("monnaie")), "USD")) updateNumericInput(session, "pib_hab", value = d$pib_usd)
    else if (identical(toupper(txt("monnaie")), d$monnaie)) updateNumericInput(session, "pib_hab", value = d$pib_local)
  }, ignoreInit = TRUE)

  observeEvent(input$proposer_seuil, {
    pib <- num("pib_hab", NA)
    if (is.na(pib) || pib <= 0) {
      showNotification(L("Renseignez d'abord le PIB par habitant.", "Enter GDP per capita first."), type = "warning")
    } else updateNumericInput(session, "seuil", value = round(0.5 * pib))
  })

  # -- Lignes des couches A et C, ajoutées et supprimées librement --

  ids <- list(a = reactiveVal(integer(0)), c = reactiveVal(integer(0)))
  compteur <- 0L

  ajouter_ligne <- function(couche, valeurs = NULL) {
    compteur <<- compteur + 1L
    id <- compteur
    insertUI(paste0("#liste_", couche), "beforeEnd", ligne_poste(couche, id, valeurs, identical(langue_listes, "en")), immediate = TRUE)
    ids[[couche]](c(isolate(ids[[couche]]()), id))
    observeEvent(input[[paste0(couche, "_suppr_", id)]], {
      removeUI(paste0("#", couche, "_ligne_", id))
      ids[[couche]](setdiff(ids[[couche]](), id))
    }, ignoreInit = TRUE, once = TRUE)
  }

  remplir <- function(couche, df) {
    for (id in isolate(ids[[couche]]())) removeUI(paste0("#", couche, "_ligne_", id), immediate = TRUE)
    ids[[couche]](integer(0))
    if (!is.null(df)) for (i in seq_len(nrow(df))) ajouter_ligne(couche, as.list(df[i, ]))
  }

  lire <- function(couche) {
    i <- ids[[couche]]()
    if (length(i) == 0) return(NULL)
    d <- do.call(rbind, lapply(i, function(id) {
      v <- function(ch) input[[paste0(couche, "_", ch, "_", id)]]
      m <- v("montant")
      ligne <- list(code = trimws(v("code") %||% ""), libelle = trimws(v("libelle") %||% ""),
                    montant = if (is.null(m) || length(m) != 1) NA_real_ else as.numeric(m),
                    statut = v("statut") %||% (if (couche == "a") "estime" else "attente"))
      if (couche == "c") {
        ligne$base <- v("base") %||% "fixe"
        ligne$nature <- v("nature") %||% "financier"
        ligne$gratuit <- isTRUE(v("gratuit"))
      }
      as.data.frame(ligne, stringsAsFactors = FALSE)
    }))
    vide <- !nzchar(d$code)
    d$code[vide] <- paste0(toupper(couche), seq_len(nrow(d)))[vide]
    d
  }

  # Les listes déroulantes des lignes suivent la langue (seulement quand elle change :
  # au démarrage, les valeurs des listes ne sont pas encore connues du serveur).
  langue_listes <- "fr"
  observeEvent(input$langue, {
    if (identical(input$langue, langue_listes)) return()
    langue_listes <<- input$langue
    maj <- function(id, choix) {
      actuel <- input[[id]]
      if (!is.null(actuel)) updateSelectInput(session, id, choices = choix, selected = actuel)
    }
    for (id in ids$a()) maj(paste0("a_statut_", id), choix_statut_a(en()))
    for (id in ids$c()) {
      maj(paste0("c_base_", id), choix_base(en()))
      maj(paste0("c_statut_", id), choix_statut_c(en()))
      maj(paste0("c_nature_", id), choix_nature(en()))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ajouter_a, ajouter_ligne("a"))
  observeEvent(input$ajouter_c, ajouter_ligne("c"))

  # -- Données du modèle --

  vide_a <- data.frame(code = character(), libelle = character(), montant = numeric(), statut = character())
  vide_c <- data.frame(code = character(), libelle = character(), montant = numeric(), base = character(),
                       statut = character(), nature = character(), gratuit = logical())
  pa <- reactive(lire("a") %||% vide_a)
  pc <- reactive(lire("c") %||% vide_c)
  b <- reactive(list(
    beneficiaires = num("b_beneficiaires"), asc = num("b_asc"), activation = num("b_activation") / 100,
    interactions_par_actif = num("b_interactions"), escalade = num("b_escalade") / 100,
    completion = num("b_completion") / 100, episodes_verifies = num("b_episodes", NA),
    beneficiaires_atteints = num("b_atteints", NA)
  ))
  params <- reactive(list(taux = num("taux", 3) / 100, duree_vie = max(1, num("duree_vie", 5)), seuil = num("seuil", NA),
                     periode = input$periode %||% "mois"))
  s <- reactive(list(compl_pour_1000 = num("s_compl"), daly_par_compl = num("s_daly"), deces_pour_1000 = num("s_deces"),
                     economie_par_compl = num("s_economie"), cout_comparateur = num("s_comparateur")))

  res <- reactive(evaluer(params(), pa(), b(), pc(), s()))

  # -- Chargement des projets --

  champs_nombres <- c("taux", "duree_vie", "seuil", "pib_hab", "b_beneficiaires", "b_asc", "b_activation", "b_interactions",
                      "b_escalade", "b_completion", "b_episodes", "b_atteints", "s_compl", "s_daly", "s_deces",
                      "s_economie", "s_comparateur")

  appliquer <- function(projet) {
    for (id in c("nom", "organisation", "pays", "monnaie")) updateTextInput(session, id, value = projet$textes[[id]] %||% "")
    updateTextAreaInput(session, "description", value = projet$textes$description %||% "")
    updateSelectInput(session, "pays_choix", selected = projet$pays_choix %||% "autre")
    updateSelectInput(session, "periode", selected = projet$periode %||% "mois")
    for (id in names(projet$nombres)) updateNumericInput(session, id, value = projet$nombres[[id]] %||% NA)
    remplir("a", projet$a)
    remplir("c", projet$c)
  }

  projet_reference <- function(estimations, langue) {
    anglais <- identical(langue, "en")
    list(
      textes = list(
        nom = if (anglais) "Digital triage innovation — Phase 3 central scenario" else "Innovation de triage numérique — scénario central Phase 3",
        organisation = "", pays = "", monnaie = "USD",
        description = if (estimations) {
          if (anglais) "Innovation data (cost-model layers A, B, C); pending layer C lines replaced by indicative estimates to be confirmed (Turn.io quote, SMS share and price, CHW time measured during UAT)."
          else "Données de l'innovation (couches A, B, C) ; lignes C en attente remplacées par des estimations indicatives à confirmer (devis Turn.io, part et prix des SMS, temps des ASC mesuré pendant l'UAT)."
        } else {
          if (anglais) "Innovation data: cost-model layers A, B and C (Phase 3 central scenario). Turn.io tariff pending; free access (C4, C5) and clinical supervision (C8) have no budget owner."
          else "Données de l'innovation : couches A, B et C du modèle de coûts (scénario central Phase 3). Tarif Turn.io en attente ; accès gratuit (C4, C5) et supervision clinique (C8) sans budget."
        }),
      pays_choix = "autre", periode = "mois",
      nombres = list(taux = 3, duree_vie = 5, seuil = NA, pib_hab = NA,
                     b_beneficiaires = 30000, b_asc = 300, b_activation = 50, b_interactions = 2, b_escalade = 10,
                     b_completion = 75, b_episodes = NA, b_atteints = NA,
                     s_compl = 0, s_daly = 0, s_deces = 0, s_economie = 0, s_comparateur = 0),
      a = reference_a(langue),
      c = reference_c(langue, estimations)
    )
  }

  appliquer(projet_reference(FALSE, "fr"))

  langue <- function() if (en()) "en" else "fr"
  observeEvent(input$charger_reference, { appliquer(projet_reference(FALSE, langue())); nav_select("onglets", "resultats") })
  observeEvent(input$charger_estimations, { appliquer(projet_reference(TRUE, langue())); nav_select("onglets", "resultats") })
  observeEvent(input$demarrer_vide, {
    appliquer(list(textes = list(monnaie = "USD"), pays_choix = "autre", periode = "mois",
                   nombres = list(taux = 3, duree_vie = 5, seuil = NA, pib_hab = NA, b_beneficiaires = 0, b_asc = 0,
                                  b_activation = 50, b_interactions = 1, b_escalade = 10, b_completion = 75,
                                  b_episodes = NA, b_atteints = NA, s_compl = 0, s_daly = 0, s_deces = 0,
                                  s_economie = 0, s_comparateur = 0),
                   a = data.frame(code = "A1", libelle = "", montant = NA, statut = "estime"),
                   c = data.frame(code = "C1", libelle = "", montant = NA, base = "fixe", statut = "attente",
                                  nature = "financier", gratuit = FALSE)))
    nav_select("onglets", "projet")
  })

  output$sauver <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9_-]+", "_", if (nzchar(txt("nom"))) txt("nom") else "project"), ".json"),
    content = function(file) {
      projet <- list(version = 5, pays_choix = input$pays_choix, periode = input$periode,
                     textes = lapply(setNames(nm = c("nom", "organisation", "pays", "monnaie", "description")), txt),
                     nombres = lapply(setNames(nm = champs_nombres), function(id) num(id, NA)),
                     a = pa(), c = pc())
      writeLines(jsonlite::toJSON(projet, auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "null"), file)
    }
  )

  observeEvent(input$ouvrir, {
    projet <- tryCatch(jsonlite::fromJSON(input$ouvrir$datapath), error = function(e) NULL)
    if (is.null(projet) || !is.data.frame(projet$a) || !is.data.frame(projet$c)) {
      showNotification(L("Fichier de projet invalide (format couches A, B, C attendu).",
                         "Invalid project file (layers A, B, C format expected)."), type = "error")
      return()
    }
    appliquer(projet)
    showNotification(L("Projet ouvert.", "Project opened."), type = "message")
  })

  # -- Navigation --

  etapes <- c(retour_accueil = "accueil", vers_a = "couche_a", retour_projet = "projet", vers_b = "couche_b",
              retour_a = "couche_a", vers_c = "couche_c", retour_b = "couche_b", vers_sante = "sante",
              retour_c = "couche_c", vers_sensibilite = "sensibilite", retour_sante = "sante",
              vers_resultats = "resultats", vers_rapport = "rapport")
  lapply(names(etapes), function(bouton) observeEvent(input[[bouton]], nav_select("onglets", etapes[[bouton]])))

  # -- Libellés traduits --

  noms_statut <- function(x) unname(c(connu = L("connu", "known"), estime = L("estimé", "estimated"),
                                      attente = L("en attente", "pending"), hors_budget = L("hors budget", "not in budget"))[x])
  badge <- function(statut) span(class = paste0("statut statut-", statut), noms_statut(statut))
  noms_vues <- function() if (en()) c(plancher = "Known lines", r = "R (Q5)", economique = "R + annualised A") else
    c(plancher = "Lignes connues", r = "R (Q5)", economique = "R + A annualisé")
  noms_denominateurs <- function() if (en()) c(
    beneficiaires = "Beneficiary covered", asc = "CHW supported", mau = "Monthly active user", interactions = "Interaction",
    escalades = "Escalation", resolus = "Resolved follow-up", verifies = "Episode with verified action") else c(
    beneficiaires = "Bénéficiaire couvert", asc = "ASC soutenu", mau = "Utilisateur actif mensuel", interactions = "Interaction",
    escalades = "Escalade", resolus = "Suivi résolu", verifies = "Épisode avec action vérifiée")

  # -- Résumés des couches --

  output$resume_a <- renderUI({
    A <- res()$A
    layout_column_wrap(
      width = "240px",
      value_box(L("Total couche A", "Total layer A"), argent(A$total, FALSE), p(paste(L("dont engagé :", "of which committed:"), argent(A$connu, FALSE))), class = "kpi"),
      value_box(L("Facteur d'annuité", "Annuity factor"), fmt(A$facteur, 2),
                p(paste0(fmt(num("duree_vie", 5)), L(" ans à ", " years at "), fmt(num("taux", 3), 1), "%")), class = "kpi"),
      value_box(L("Coût annuel équivalent", "Equivalent annual cost"), argent(A$annuel, FALSE),
                p(L("vue économique seulement", "economic view only")), class = "kpi"),
      value_box(L("En attente", "Pending"), if (length(A$en_attente)) paste(A$en_attente, collapse = ", ") else "—",
                p(L("non comptés", "not counted")), class = "kpi")
    )
  })

  output$chaine <- renderUI({
    v <- res()$volumes$mensuel
    lignes <- list(
      list("B1", L("Bénéficiaires couverts", "Beneficiaries covered"), "", v[["beneficiaires"]]),
      list("MAU", L("Utilisateurs actifs mensuels", "Monthly active users"), "= B1 × B3", v[["mau"]]),
      list("I", L("Interactions", "Interactions"), "= MAU × B4", v[["interactions"]]),
      list("E", L("Escalades", "Escalations"), "= I × B5", v[["escalades"]]),
      list("F", L("Suivis résolus", "Resolved follow-ups"), "= E × B6", v[["resolus"]]),
      list("A", L("Épisodes avec action vérifiée", "Episodes with verified action"), L("mesuré", "measured"), v[["verifies"]])
    )
    maxi <- max(1, v[["beneficiaires"]], na.rm = TRUE)
    tagList(
      tags$table(class = "chaine", style = "width:100%",
        tags$tr(tags$th(""), tags$th(""), tags$th(""), tags$th(L("Par mois", "Per month")), tags$th(L("Par an", "Per year")), tags$th("")),
        lapply(lignes, function(l) {
          flux <- !(l[[1]] %in% c("B1", "MAU"))
          tags$tr(tags$td(strong(l[[1]])), tags$td(l[[2]]), tags$td(class = "formule-chaine", l[[3]]),
                  tags$td(strong(fmt(l[[4]]))),
                  tags$td(if (flux) fmt(12 * l[[4]]) else if (l[[1]] == "MAU") paste(fmt(12 * l[[4]]), L("mois-util.", "user-months")) else ""),
                  tags$td(style = "width:28%", if (!is.na(l[[4]])) div(class = "barre", style = sprintf("width:%.1f%%", 100 * l[[4]] / maxi))))
        }),
        tags$tr(tags$td(strong("B2")), tags$td(L("ASC soutenus", "CHWs supported")), tags$td(""), tags$td(strong(fmt(v[["asc"]]))),
                tags$td(""), tags$td(""))),
      if (is.na(v[["verifies"]])) p(class = "aide mt-2", L("A n'est pas encore mesuré : OC5c ne peut pas être calculé.",
                                                          "A is not measured yet: OC5c cannot be computed."))
    )
  })

  output$resume_c <- renderUI({
    r <- res(); C <- r$C
    postes <- C$postes
    tagList(
      layout_column_wrap(
        width = "230px",
        value_box(L("Lignes connues (an)", "Known lines (year)"), argent(C$plancher, FALSE), p(L("plancher", "floor")), class = "kpi"),
        value_box(L("R annuel", "Annual R"), argent(C$r, FALSE),
                  p(if (C$partiel) paste(L("partiel — manque", "partial — missing"), paste(C$manquants, collapse = ", ")) else L("complet", "complete")), class = "kpi"),
        value_box(L("dont décaissé / opportunité", "of which cash / opportunity"), paste(fmt(C$decaisse), "/", fmt(C$non_decaisse)),
                  p(monnaie()), class = "kpi"),
        value_box(L("Accès gratuit (OC5e)", "Free access (OC5e)"), argent(C$gratuit, FALSE),
                  p(paste(postes$code[postes$gratuit], collapse = " + ")), class = "kpi")
      ),
      card(card_header(L("Coût annuel calculé par ligne", "Annual cost computed per line")),
           tags$table(class = "table table-sm",
             tags$thead(tags$tr(tags$th(L("Ligne", "Line")), tags$th(L("Statut", "Status")), tags$th(L("Coût annuel", "Annual cost")),
                                tags$th(L("Coût de la période", "Period cost")), tags$th(L("Dans R", "In R")))),
             tags$tbody(lapply(seq_len(nrow(postes)), function(k) {
               x <- postes[k, ]
               tags$tr(tags$td(strong(x$code), " ", x$libelle, if (x$gratuit) span(class = "aide", L(" · accès gratuit", " · free access"))),
                       tags$td(badge(x$statut)), tags$td(argent(x$annuel, FALSE)),
                       tags$td(argent(x$annuel * mois_periode(r$periode) / 12)), tags$td(if (x$r) "✓" else L("manquant", "missing")))
             }))))
    )
  })

  # -- Sensibilité --

  output$choix_sensibilite <- renderUI({
    d <- noms_denominateurs()
    layout_column_wrap(
      width = "180px",
      selectInput("sens_indicateur", L("Indicateur", "Indicator"), setNames(names(d), d), isolate(input$sens_indicateur) %||% "resolus"),
      selectInput("sens_vue", L("Vue de coût", "Cost view"), setNames(perspectives, noms_vues()), isolate(input$sens_vue) %||% "r")
    )
  })

  plages <- reactive(list(
    activation = c(num("sens_activation_bas"), num("sens_activation_haut")) / 100,
    interactions_par_actif = c(num("sens_interactions_bas"), num("sens_interactions_haut")),
    escalade = c(num("sens_escalade_bas"), num("sens_escalade_haut")) / 100,
    completion = c(num("sens_completion_bas"), num("sens_completion_haut")) / 100,
    duree_vie = c(max(1, num("sens_duree_bas", 1)), max(1, num("sens_duree_haut", 1))),
    couts_c = c(num("sens_couts_bas"), num("sens_couts_haut")) / 100
  ))

  table_sens <- reactive(sensibilite(params(), pa(), b(), pc(), s(), plages(), indicateur = input$sens_indicateur %||% "resolus",
                                     perspective = input$sens_vue %||% "r"))

  noms_parametres <- function() if (en()) c(activation = "B3 activation rate", interactions_par_actif = "B4 interactions per user",
    escalade = "B5 escalation rate", completion = "B6 follow-up completion", duree_vie = "Lifetime of A", couts_c = "Layer C cost level") else
    c(activation = "B3 taux d'activation", interactions_par_actif = "B4 interactions par utilisateur", escalade = "B5 taux d'escalade",
      completion = "B6 complétion des suivis", duree_vie = "Durée de vie de A", couts_c = "Niveau des coûts C")

  valeur_param <- function(k, x) if (k %in% c("activation", "escalade", "completion")) fmt_pct(x) else
    if (k == "couts_c") paste0("×\u00a0", fmt(x, 2)) else fmt(x, if (k == "interactions_par_actif") 1 else 0)

  output$tornade <- renderPlot({
    t <- table_sens()
    t <- t[is.finite(t$valeur_bas) & is.finite(t$valeur_haut), ]
    par(family = "serif", bg = "#ffffff", las = 1)
    if (nrow(t) == 0 || !is.finite(t$base[1])) {
      par(mar = c(1, 1, 1, 1)); plot.new()
      text(0.5, 0.5, L("Indicateur non calculable (volume ou coût nul, ou A non mesuré).",
                       "Indicator cannot be computed (zero volume or cost, or A not measured)."), col = couleurs$gris, cex = 1.1)
      return()
    }
    base <- t$base[1]
    bornes <- range(c(t$valeur_bas, t$valeur_haut, base))
    marge <- diff(bornes) * 0.12 + abs(base) * 0.02 + 1e-9
    par(mar = c(4, 15, 1, 2))
    plot(NA, xlim = c(bornes[1] - marge, bornes[2] + marge), ylim = c(0.5, nrow(t) + 0.5), axes = FALSE, xlab = "", ylab = "")
    for (k in seq_len(nrow(t))) {
      rect(min(t$valeur_bas[k], base), k - 0.32, max(t$valeur_bas[k], base), k + 0.32, col = couleurs$sarcelle, border = NA)
      rect(min(t$valeur_haut[k], base), k - 0.32, max(t$valeur_haut[k], base), k + 0.32, col = couleurs$or, border = NA)
    }
    abline(v = base, col = couleurs$encre, lwd = 1.5)
    axis(2, at = seq_len(nrow(t)), labels = noms_parametres()[t$parametre], tick = FALSE, cex.axis = 0.95)
    ticks <- pretty(c(bornes[1] - marge, bornes[2] + marge))
    axis(1, at = ticks, labels = vapply(ticks, fmt_auto, ""), col = "#cfc6b6", col.axis = "#5b6a6e", cex.axis = 0.85)
    mtext(paste(L("Coût par", "Cost per"), tolower(noms_denominateurs()[[input$sens_indicateur %||% "resolus"]]), "—", monnaie()),
          side = 1, line = 2.5, col = "#5b6a6e")
    legend("bottomright", bty = "n", fill = c(couleurs$sarcelle, couleurs$or), border = NA,
           legend = L(c("valeur basse", "valeur haute"), c("low value", "high value")), cex = 0.9)
  })

  output$table_sensibilite <- renderTable({
    t <- table_sens()
    t <- t[rev(seq_len(nrow(t))), ]
    d <- data.frame(noms_parametres()[t$parametre],
                    mapply(valeur_param, t$parametre, t$bas), vapply(t$valeur_bas, fmt_auto, ""),
                    mapply(valeur_param, t$parametre, t$haut), vapply(t$valeur_haut, fmt_auto, ""),
                    vapply(t$valeur_bas / t$base, function(x) paste0("×\u00a0", fmt(x, 2)), ""),
                    vapply(t$valeur_haut / t$base, function(x) paste0("×\u00a0", fmt(x, 2)), ""))
    names(d) <- if (en()) c("Parameter", "Low", "Result", "High", "Result", "Low / base", "High / base") else
      c("Paramètre", "Bas", "Résultat", "Haut", "Résultat", "Bas / base", "Haut / base")
    d
  }, striped = TRUE, width = "100%")

  # -- Indicateurs --

  output$bandeau <- renderUI({
    r <- res()
    u <- r$unitaires["resolus", ]
    titre <- div(class = "detail", if (nzchar(txt("nom"))) txt("nom") else L("Votre innovation", "Your innovation"),
                 if (nzchar(pays_nom())) paste(" ·", pays_nom()), " · ", periode_txt())
    rcei <- r$niveau3$r$rcei_daly
    v <- verdict_daly(rcei, num("seuil", NA))
    couleur <- switch(v, dominant = , cout_efficace = couleurs$vert, non_cout_efficace = couleurs$rouge,
                      if (r$C$partiel) couleurs$gris else couleurs$sarcelle)
    phrase_sante <- switch(v,
      dominant = L("Niveau santé : dominant (moins coûteux que le statu quo et plus efficace).", "Health level: dominant (cheaper than the status quo and more effective)."),
      cout_efficace = paste(L("Niveau santé : coût-efficace —", "Health level: cost-effective —"), argent(rcei), L("par DALY évitée, sous le seuil.", "per DALY averted, below the threshold.")),
      non_cout_efficace = paste(L("Niveau santé : non coût-efficace —", "Health level: not cost-effective —"), argent(rcei), L("par DALY évitée, au-dessus du seuil.", "per DALY averted, above the threshold.")),
      sans_seuil = paste(L("Niveau santé :", "Health level:"), argent(rcei), L("par DALY évitée — renseignez un seuil pour conclure.", "per DALY averted — enter a threshold to conclude.")),
      L("Niveau santé : effets non renseignés (onglet 5).", "Health level: effects not entered (tab 5)."))
    div(class = "verdict", style = paste0("background:", couleur), titre,
        div(class = "chiffre", "OC5d ", argent(u[["r"]]), span(class = "unite", L(" par suivi résolu", " per resolved follow-up"))),
        div(class = "phrase", paste0(L("R + A annualisé : ", "R + annualised A: "), argent(u[["economique"]]),
                                     L(" · lignes connues : ", " · known lines: "), argent(u[["plancher"]]))),
        div(class = "detail", if (r$C$partiel) paste(L("Estimation partielle — lignes manquantes :", "Partial estimate — missing lines:"),
                                                     paste(r$C$manquants, collapse = ", ")) else
          L("Toutes les lignes C sont chiffrées.", "All layer C lines are costed.")),
        div(class = "detail", phrase_sante))
  })

  output$alertes <- renderUI({
    r <- res()
    msgs <- list()
    hb <- intersect(r$C$hors_budget, r$C$manquants)
    if (length(hb)) msgs <- c(msgs, list(paste(L("Lignes requises sans budget ni montant :", "Required lines with no budget and no amount:"),
      paste(hb, collapse = ", "), L("— l'accès gratuit est obligatoire et la supervision clinique porte le modèle de sécurité ; chiffrez-les ou signalez-les comme exclues.",
                                    "— free access is mandatory and clinical supervision is the backbone of the safety model; cost them or flag them as excluded."))))
    attente <- setdiff(r$C$manquants, r$C$hors_budget)
    if (length(attente)) msgs <- c(msgs, list(paste(L("Lignes en attente :", "Pending lines:"), paste(attente, collapse = ", "),
                                                   L("— rapportez les coûts récurrents partiels séparément jusqu'à ce que toutes les lignes soient disponibles.",
                                                     "— report partial recurrent costs separately until all cost lines are available."))))
    if (is.na(r$volumes$mensuel[["verifies"]])) msgs <- c(msgs, list(L("A (épisodes avec action vérifiée) non mesuré : OC5c non calculable.",
                                                                      "A (episodes with verified action) not measured: OC5c cannot be computed.")))
    if (length(r$A$en_attente)) msgs <- c(msgs, list(paste(L("Couche A en attente :", "Layer A pending:"), paste(r$A$en_attente, collapse = ", "))))
    if (length(msgs) == 0) return(NULL)
    div(class = "alerte", strong(L("À compléter ou à signaler", "To complete or to flag")), tags$ul(lapply(msgs, tags$li)))
  })

  partiel_badge <- function(valeur, partiel) tagList(valeur, if (partiel && valeur != "—") span(class = "partiel ms-2", L("partiel", "partial")))

  q5_df <- reactive({
    r <- res()
    v <- r$v
    ligne <- function(code, cle, formule, denominateur) {
      list(code = code, indicateur = noms_denominateurs()[[cle]], formule = formule, denominateur = denominateur,
           volume = fmt(v[[cle]]), plancher = argent(r$unitaires[cle, "plancher"]), r = argent(r$unitaires[cle, "r"]),
           economique = argent(r$unitaires[cle, "economique"]))
    }
    gratuits <- r$C$postes$code[r$C$postes$gratuit]
    list(
      ligne("OC5a", "mau", "R / MAU", L("Utilisateurs actifs mensuels", "Monthly active users")),
      ligne("OC5b", "interactions", "R / I", L("Interactions de la période", "Interactions in the period")),
      ligne("OC5c", "verifies", "R / A", L("Épisodes uniques avec action qualifiante vérifiée", "Unique episodes with a verified qualifying action")),
      ligne("OC5d", "resolus", "R / F", L("Suivis résolus", "Resolved follow-ups")),
      list(code = "OC5e", indicateur = L("Bénéficiaire de l'accès gratuit", "Beneficiary of free access"),
           formule = paste0("(", if (length(gratuits)) paste(gratuits, collapse = " + ") else "C4 + C5", ") / ", L("atteints", "reached")),
           denominateur = L("Bénéficiaires atteints", "Beneficiaries reached"), volume = fmt(v[["atteints"]]),
           plancher = "", r = argent(r$oc5e), economique = "")
    )
  })

  output$table_q5 <- renderUI({
    r <- res()
    lignes <- q5_df()
    tagList(
      tags$table(class = "table table-q5",
        tags$thead(tags$tr(tags$th("CODE"), tags$th(L("INDICATEUR", "INDICATOR")), tags$th(L("FORMULE", "FORMULA")),
                           tags$th(L("DÉNOMINATEUR", "DENOMINATOR")), tags$th(L("VOLUME", "VOLUME")),
                           tags$th(noms_vues()[["plancher"]]), tags$th(noms_vues()[["r"]]), tags$th(noms_vues()[["economique"]]))),
        tags$tbody(lapply(lignes, function(l) {
          partiel <- if (l$code == "OC5e") !r$C$gratuit_complet else r$C$partiel
          tags$tr(tags$td(class = "code", l$code), tags$td(l$indicateur), tags$td(span(class = "formule", l$formule)),
                  tags$td(l$denominateur), tags$td(l$volume), tags$td(l$plancher),
                  tags$td(strong(partiel_badge(l$r, partiel))), tags$td(partiel_badge(l$economique, partiel && l$economique != "")))
        }))),
      p(class = "aide", L(paste("Coûts", periode_txt(), "· OC5e est une composante de R et ne s'additionne pas."),
                          paste("Costs", periode_txt(), "· OC5e is a component of R and is not additive.")))
    )
  })

  output$numerateur <- renderUI({
    r <- res()
    postes <- r$C$postes
    termes <- lapply(seq_len(nrow(postes)), function(k) {
      x <- postes[k, ]
      span(style = if (is.na(x$annuel)) "opacity:.45" else "", x$code)
    })
    plus <- function(l) { out <- list(); for (k in seq_along(l)) { if (k > 1) out <- c(out, list(" + ")); out <- c(out, list(l[[k]])) }; out }
    f <- r$v[["resolus"]]
    div(class = "numerateur",
        h4(L("LE NUMÉRATEUR PARTAGÉ", "THE SHARED NUMERATOR")),
        div(class = "r", "R = ", plus(termes)),
        p(L(paste("Coûts récurrents de la période, hors couche A. Lignes grisées : manquantes. R", periode_txt(), "="),
            paste("Recurrent costs for the period, excluding layer A. Greyed lines: missing. R", periode_txt(), "=")),
          strong(argent(r$couts[["r"]], FALSE)), if (r$C$partiel) span(class = "partiel ms-2", L("partiel", "partial"))),
        div(class = "formule-ligne", style = "color:#1f2a2e",
            "OC5d = R / F = ", fmt(r$couts[["r"]]), " / ", fmt(f), " = ", strong(argent(r$unitaires["resolus", "r"]))),
        div(class = "formule-ligne", style = "color:#1f2a2e",
            L("Vue économique : (R + A annualisé) / F = ", "Economic view: (R + annualised A) / F = "),
            fmt(r$couts[["economique"]]), " / ", fmt(f), " = ", strong(argent(r$unitaires["resolus", "economique"]))))
  })

  output$composition <- renderPlot({
    r <- res()
    postes <- r$C$postes[!is.na(r$C$postes$annuel), ]
    fraction <- mois_periode(r$periode) / 12
    valeurs <- c(setNames(postes$annuel * fraction, postes$code),
                 if (r$A$annuel > 0) setNames(r$A$annuel * fraction, L("A annualisé", "Annualised A")))
    par(family = "serif", bg = "#ffffff", las = 1)
    if (length(valeurs) == 0 || sum(valeurs) <= 0) {
      par(mar = c(1, 1, 1, 1)); plot.new()
      text(0.5, 0.5, L("Aucune ligne chiffrée.", "No costed line."), col = couleurs$gris, cex = 1.2)
      return()
    }
    valeurs <- sort(valeurs)
    col <- ifelse(names(valeurs) %in% postes$code, couleurs$bleu_c, couleurs$bleu_a)
    par(mar = c(4, 8, 1, 6))
    y <- barplot(valeurs, horiz = TRUE, col = col, border = NA, axes = FALSE, space = 0.5, xlim = c(0, max(valeurs) * 1.35),
                 cex.names = 0.95)
    axis(1, at = pretty(c(0, max(valeurs))), labels = vapply(pretty(c(0, max(valeurs))), fmt, ""), col = "#cfc6b6",
         col.axis = "#5b6a6e", cex.axis = 0.85)
    text(valeurs, y, paste0(vapply(valeurs, fmt, ""), "  (", vapply(valeurs / sum(valeurs), fmt_pct, ""), ")"), pos = 4, cex = 0.85)
    mtext(paste(L("Coût", "Cost"), periode_txt(), "—", monnaie()), side = 1, line = 2.5, col = "#5b6a6e")
  })

  autres_df <- reactive({
    r <- res()
    d <- noms_denominateurs()
    ligne <- function(lib, vol, cle) data.frame(indicateur = lib, volume = vol, plancher = argent(r$unitaires[cle, "plancher"]),
      r = argent(r$unitaires[cle, "r"]), economique = argent(r$unitaires[cle, "economique"]), stringsAsFactors = FALSE)
    n3 <- r$niveau3
    l3 <- function(lib, vol, cle, f = argent) data.frame(indicateur = lib, volume = vol, plancher = f(n3$plancher[[cle]]),
      r = f(n3$r[[cle]]), economique = f(n3$economique[[cle]]), stringsAsFactors = FALSE)
    rbind(
      ligne(paste(d[["beneficiaires"]], L("(par mois de rapport)", "(per reporting month)")), fmt(r$v[["beneficiaires"]]), "beneficiaires"),
      ligne(d[["asc"]], fmt(r$v[["asc"]]), "asc"),
      ligne(d[["escalades"]], fmt(r$v[["escalades"]]), "escalades"),
      l3(L("DALY évitée (RCEI)", "DALY averted (ICER)"), fmt(r$sante$daly, 1), "rcei_daly"),
      l3(L("Décès évité (RCEI)", "Death averted (ICER)"), fmt(r$sante$deces, 2), "rcei_deces"),
      l3(L("Bénéfice monétaire net", "Net monetary benefit"), "", "nmb"),
      l3("ROI", "", "roi", fmt_pct)
    )
  })

  entetes_autres <- function() c(L("Coût par…", "Cost per…"), L("Volume de la période", "Period volume"), noms_vues())

  output$table_autres <- renderUI({ d <- autres_df(); names(d) <- entetes_autres(); tableau_html(d) })

  output$telecharger <- downloadHandler(
    filename = function() L("indicateurs_q5.csv", "q5_indicators.csv"),
    content = function(file) {
      q <- do.call(rbind, lapply(q5_df(), as.data.frame))
      names(q) <- c("Code", L("Indicateur", "Indicator"), L("Formule", "Formula"), L("Dénominateur", "Denominator"),
                    L("Volume", "Volume"), noms_vues())
      a <- autres_df(); a <- cbind(Code = "", a[, 1], Formule = "", Denominateur = "", a[, -1])
      names(a) <- names(q)
      write.csv(rbind(q, a), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # -- Rapport --

  output$rapport <- renderUI({
    r <- res()
    ligne <- function(label, valeur) tags$tr(tags$td(label), tags$td(strong(valeur)))
    q <- do.call(rbind, lapply(q5_df(), function(l) data.frame(l$code, l$indicateur, l$formule, l$volume, l$plancher, l$r, l$economique)))
    names(q) <- c("Code", L("Indicateur", "Indicator"), L("Formule", "Formula"), L("Volume", "Volume"), noms_vues())
    postes <- r$C$postes
    autres <- autres_df(); names(autres) <- entetes_autres()
    tagList(
      h2(if (nzchar(txt("nom"))) txt("nom") else L("Analyse coût-efficacité", "Cost-effectiveness analysis")),
      p(class = "aide", paste(c(txt("organisation"), pays_nom(), periode_txt(), format(Sys.Date(), L("%d/%m/%Y", "%Y-%m-%d"))), collapse = " · ")),
      if (nzchar(txt("description"))) p(txt("description")),
      if (r$C$partiel) p(span(class = "partiel", L("Estimation partielle", "Partial estimate")), " ",
                         L("— lignes manquantes : ", "— missing lines: "), paste(r$C$manquants, collapse = ", ")),
      h4(L("Indicateurs Q5", "Q5 indicators")),
      tableau_html(q),
      h4(L("Couche A — développement non récurrent (exclu de R)", "Layer A — non-recurrent development (excluded from R)")),
      tags$table(class = "table table-sm",
        ligne("Total", argent(r$A$total, FALSE)),
        ligne(L("Annualisé", "Annualised"), paste0(argent(r$A$annuel, FALSE), " (", fmt(num("duree_vie", 5)), L(" ans, ", " years, "), fmt(num("taux", 3), 1), "%)")),
        if (length(r$A$en_attente)) ligne(L("En attente", "Pending"), paste(r$A$en_attente, collapse = ", "))),
      h4(L("Couche B — volumes mensuels", "Layer B — monthly volumes")),
      tags$table(class = "table table-sm",
        ligne("MAU = B1 × B3", fmt(r$volumes$mensuel[["mau"]])),
        ligne("I = MAU × B4", fmt(r$volumes$mensuel[["interactions"]])),
        ligne("E = I × B5", fmt(r$volumes$mensuel[["escalades"]])),
        ligne("F = E × B6", fmt(r$volumes$mensuel[["resolus"]])),
        ligne(L("A (mesuré)", "A (measured)"), fmt(r$volumes$mensuel[["verifies"]]))),
      h4(L("Couche C — coûts récurrents (annuels)", "Layer C — recurrent costs (annual)")),
      tableau_html(setNames(data.frame(paste(postes$code, postes$libelle), noms_statut(postes$statut),
                                       vapply(postes$annuel, function(x) argent(x, FALSE), "")),
                            L(c("Ligne", "Statut", "Coût annuel"), c("Line", "Status", "Annual cost")))),
      h4(L("Autres coûts unitaires et niveau santé", "Other unit costs and health level")),
      tableau_html(autres),
      h4(L("Limites", "Limitations")),
      tags$ul(
        tags$li(L("B3, B4, B5 et B6 sont des hypothèses de planification : voir l'analyse de sensibilité.",
                  "B3, B4, B5 and B6 are planning assumptions: see the sensitivity analysis.")),
        if (length(intersect(r$C$hors_budget, r$C$manquants))) tags$li(paste(L("Lignes hors budget non chiffrées :", "Not-in-budget lines not costed:"),
                                                                            paste(intersect(r$C$hors_budget, r$C$manquants), collapse = ", "))),
        if (r$sante$daly <= 0) tags$li(L("Effets de santé non estimés : pas encore de coût par DALY.", "Health effects not estimated: no cost per DALY yet."))),
      p(class = "aide mt-4", L("Méthode : Drummond et al. (2015), OMS-CHOICE (2003), iDSI Reference Case (2016).",
                               "Method: Drummond et al. (2015), WHO-CHOICE (2003), iDSI Reference Case (2016)."))
    )
  })
}

shinyApp(ui, server)
