library(shiny)
library(bslib)

source("calcul.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Mise en forme ----------------------------------------------------------

fmt <- function(x, decimales = 0) {
  if (is.null(x) || length(x) != 1 || is.na(x)) return("—")
  formatC(x, format = "f", digits = decimales, big.mark = " ", decimal.mark = ",")
}
fmt_pct <- function(x) if (is.na(x)) "—" else paste0(fmt(100 * x), " %")

couleurs_verdict <- c(excellent = "#1d7a4f", bon = "#3f8f5e", limite = "#c08a2e",
                      faible = "#a23b2a", inconnu = "#6c7a80")

css <- "
:root { --encre: #1f2a2e; --sarcelle: #0f5257; --or: #c08a2e; --papier: #faf7f2; }
body { background: var(--papier); }
.navbar { background: #ffffff !important; border-bottom: 1px solid #e8e1d4; }
.navbar-brand { font-weight: 600; letter-spacing: .02em; color: var(--sarcelle) !important; }
.nav-link { font-size: 1.08rem; }
.nav-link.active { color: var(--sarcelle) !important; border-bottom: 2px solid var(--or); }
h1, h2, h3, h4 { color: var(--sarcelle); font-weight: 600; }
.card { border: none; border-radius: 14px; box-shadow: 0 2px 16px rgba(15, 82, 87, .08); }
.card-header { background: transparent; border-bottom: 1px solid #efe8dc; font-size: 1.15rem;
  font-weight: 600; color: var(--sarcelle); }
.hero { background: linear-gradient(135deg, #0f5257 0%, #1d7874 55%, #2f8f83 80%, #c08a2e 140%);
  color: #fff; border-radius: 20px; padding: 3.5rem 3rem; margin-bottom: 2rem; }
.hero h1 { color: #fff; font-size: 2.9rem; font-weight: 600; margin-bottom: 1rem; }
.hero p { font-size: 1.3rem; max-width: 46rem; opacity: .95; }
.hero .btn { font-size: 1.1rem; padding: .6rem 1.4rem; border-radius: 30px; margin-right: .6rem; margin-top: .8rem; }
.btn-or { background: var(--or); color: #fff; border: none; }
.btn-or:hover { background: #a8761f; color: #fff; }
.btn-blanc { background: transparent; color: #fff; border: 1px solid rgba(255,255,255,.7); }
.btn-blanc:hover { background: rgba(255,255,255,.12); color: #fff; }
.etape { text-align: left; padding: 1.6rem; height: 100%; }
.etape .num { display: inline-flex; width: 2.4rem; height: 2.4rem; border-radius: 50%;
  background: var(--or); color: #fff; align-items: center; justify-content: center;
  font-size: 1.3rem; margin-bottom: .8rem; font-variant-numeric: lining-nums; }
.etape h3 { font-size: 1.4rem; }
.activite { border-left: 4px solid var(--or); }
.activite .titre-activite input { font-size: 1.2rem; font-weight: 600; color: var(--sarcelle); }
.kpi { background: #fff !important; border: none !important; border-top: 3px solid var(--or) !important;
  box-shadow: 0 2px 16px rgba(15, 82, 87, .08); }
.kpi .value-box-title { color: #5b6a6e; font-size: 1rem; }
.kpi .value-box-value { color: var(--sarcelle); font-weight: 600; font-size: 1.9rem; white-space: nowrap; font-variant-numeric: lining-nums tabular-nums; }
.kpi p { color: #6c7a80; font-size: .95rem; margin: 0; }
.verdict { border-radius: 18px; color: #fff; padding: 2rem 2.4rem; margin-bottom: 1.5rem; }
.verdict .chiffre { font-size: 3rem; font-weight: 600; line-height: 1.1; font-variant-numeric: lining-nums; }
.verdict .unite { font-size: 1.3rem; opacity: .9; }
.verdict .phrase { font-size: 1.35rem; margin-top: .6rem; }
.verdict .detail { opacity: .85; margin-top: .3rem; }
table { font-variant-numeric: lining-nums tabular-nums; }
.table > :not(caption) > * > * { padding: .55rem .7rem; }
.aide { color: #6c7a80; font-size: .98rem; }
.form-label, .control-label { color: #3c4a4e; }
@media (max-width: 640px) {
  .navbar-brand { font-size: 1rem; white-space: normal; }
  .hero { padding: 2rem 1.4rem; border-radius: 14px; }
  .hero h1 { font-size: 2rem; }
  .hero p { font-size: 1.1rem; }
  .verdict { padding: 1.4rem; }
  .verdict .chiffre { font-size: 2.1rem; }
  .kpi .value-box-value { font-size: 1.6rem; }
}
"

champ <- function(id, label, valeur = NA, aide = NULL, ...) {
  tagList(numericInput(id, label, valeur, min = 0, ...),
          if (!is.null(aide)) div(class = "aide mb-3", style = "margin-top:-.6rem", aide))
}

carte_activite <- function(id, v = NULL) {
  val <- function(col, defaut) if (!is.null(v) && !is.null(v[[col]])) v[[col]] else defaut
  i <- function(nom) paste0(nom, "_", id)
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
        width = "220px",
        textInput(i("resultat"), "Résultat mesuré", val("resultat", ""), placeholder = "ex. cas traités"),
        numericInput(i("investissement"), "Investissement de départ", val("investissement", 0), min = 0),
        numericInput(i("recurrent"), "Coût de fonctionnement / an", val("recurrent", 0), min = 0),
        numericInput(i("beneficiaires"), "Bénéficiaires / an", val("beneficiaires", 0), min = 0),
        numericInput(i("resultats"), "Résultats obtenus / an", val("resultats", 0), min = 0),
        numericInput(i("daly_par_resultat"), "DALY évitées par résultat", val("daly_par_resultat", 0),
                     min = 0, step = 0.01),
        numericInput(i("economie_par_resultat"), "Économies par résultat", val("economie_par_resultat", 0), min = 0)
      )
    )
  )
}

# ---- Interface --------------------------------------------------------------

police <- font_collection("EB Garamond", "Garamond", "Georgia", "serif")

ui <- page_navbar(
  id = "onglets",
  title = "Coût-efficacité · Innovations en santé",
  fillable = FALSE,
  theme = bs_theme(version = 5, bg = "#faf7f2", fg = "#1f2a2e", primary = "#0f5257",
                   secondary = "#c08a2e", base_font = police, heading_font = police,
                   font_scale = 1.1),
  header = tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=EB+Garamond:ital,wght@0,400;0,500;0,600;0,700;1,400&display=swap"),
    tags$style(HTML(css))
  ),

  nav_panel(
    "Accueil", value = "accueil",
    div(class = "hero",
        h1("Votre innovation en santé vaut-elle son coût\u00a0?"),
        p("Décrivez votre projet et ses activités, renseignez vos coûts et vos résultats : ",
          "l'outil calcule le coût par bénéficiaire, le coût par résultat et le coût par DALY évitée, ",
          "et vous dit si votre innovation est coût-efficace."),
        actionButton("demarrer_vide", "Commencer mon projet", class = "btn-or"),
        actionButton("demarrer_exemple", "Voir un exemple", class = "btn-blanc")),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(div(class = "etape", div(class = "num", "1"), h3("Le projet"),
               p("Nom, pays, monnaie, durée. Tout se saisit librement : l'outil s'adapte à n'importe quel contexte."))),
      card(div(class = "etape", div(class = "num", "2"), h3("Les activités"),
               p("Ajoutez autant d'activités que nécessaire, avec leurs coûts, leurs bénéficiaires et leurs résultats."))),
      card(div(class = "etape", div(class = "num", "3"), h3("La coût-efficacité"),
               p("Verdict, comparaison des activités, analyse de sensibilité et export du rapport.")))
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
          width = "200px",
          textInput("pays", "Pays / zone", "", placeholder = "ex. Burkina Faso"),
          textInput("monnaie", "Monnaie", "XOF", placeholder = "ex. XOF, KES, UGX, USD")
        ),
        textAreaInput("description", "Description de l'innovation", "", rows = 3,
                      placeholder = "Ce que fait l'innovation, pour qui, en quoi elle est nouvelle…")
      ),
      card(
        card_header("Paramètres de l'analyse"),
        layout_column_wrap(
          width = "200px",
          numericInput("duree", "Durée de l'analyse (années)", 3, min = 1, max = 30),
          numericInput("taux_actualisation", "Taux d'actualisation (%)", 3, min = 0, max = 20, step = 0.5),
          numericInput("attribution", "Part de l'effet due à l'innovation (%)", 80, min = 0, max = 100)
        ),
        div(class = "aide mb-3",
            "La part de l'effet due à l'innovation retire ce qui serait arrivé sans elle ",
            "ou grâce à d'autres acteurs."),
        layout_column_wrap(
          width = "200px",
          numericInput("pib_hab", "PIB par habitant (en monnaie du projet)", NA, min = 0),
          numericInput("seuil", "Seuil par DALY évitée (optionnel)", NA, min = 0),
          numericInput("taux_change", "Monnaie du projet pour 1 USD (optionnel)", NA, min = 0)
        ),
        div(class = "aide",
            "Sans seuil saisi, l'outil utilise 0,5 × PIB par habitant. ",
            "Le taux de change sert seulement à afficher les équivalents en USD.")
      )
    ),
    card(
      card_header("Enregistrer ou reprendre un projet"),
      layout_columns(
        col_widths = c(6, 6),
        downloadButton("sauver", "Enregistrer le projet (.json)", class = "btn-outline-primary"),
        fileInput("ouvrir", NULL, accept = ".json", buttonLabel = "Ouvrir un projet…",
                  placeholder = "fichier .json")
      )
    ),
    div(class = "text-end mb-4",
        actionButton("vers_activites", "Étape suivante : les activités →", class = "btn-primary"))
  ),

  nav_panel(
    "2 · Activités", value = "activites",
    card(
      card_header("Activités de l'innovation"),
      p("Pour chaque activité : ce qu'elle coûte, combien de personnes elle touche et quels résultats elle produit. ",
        "Les montants sont dans la monnaie du projet ; ", strong("« par an »"), " = chaque année de l'analyse."),
      div(class = "aide",
          strong("DALY évitées par résultat : "),
          "années de vie en bonne santé gagnées pour chaque résultat (ex. 0,02 par cas de paludisme traité). ",
          "Laissez 0 si l'activité est un support (formation, coordination) : son coût compte dans le total.")
    ),
    div(id = "liste_activites"),
    div(class = "mb-4", actionButton("ajouter", "+ Ajouter une activité", class = "btn-or")),
    card(
      card_header("Situation de référence (optionnel)"),
      p("Ce que coûte et produit la pratique actuelle, sans l'innovation. Sert à calculer le ",
        strong("ratio coût-efficacité différentiel (ICER)"), " : le coût supplémentaire par DALY évitée en plus."),
      layout_column_wrap(
        width = "220px",
        numericInput("ref_cout", "Coût annuel de la pratique actuelle", 0, min = 0),
        numericInput("ref_daly", "DALY évitées / an par la pratique actuelle", 0, min = 0)
      )
    ),
    div(class = "text-end mb-4",
        actionButton("vers_resultats", "Voir la coût-efficacité →", class = "btn-primary"))
  ),

  nav_panel(
    "3 · Résultats", value = "resultats",
    uiOutput("bandeau"),
    uiOutput("kpis"),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Coût par DALY évitée, par activité"),
           plotOutput("graphique", height = "340px")),
      card(card_header("Analyse de sensibilité"),
           p(class = "aide", "Le résultat tient-il si les hypothèses changent ?"),
           tableOutput("table_sensibilite"))
    ),
    card(
      card_header("Comparaison des activités"),
      tableOutput("table_activites"),
      div(downloadButton("telecharger", "Télécharger le tableau (.csv)", class = "btn-outline-primary btn-sm"))
    )
  ),

  nav_panel(
    "Méthode", value = "methode",
    card(card_body(
      h3("Indicateurs calculés"),
      tags$ul(
        tags$li(strong("Coût par bénéficiaire"), " = coût total actualisé / nombre total de bénéficiaires."),
        tags$li(strong("Coût par résultat"), " = coût actualisé de l'activité / résultats obtenus (propre à chaque activité)."),
        tags$li(strong("Coût par DALY évitée"), " = coût total actualisé / DALY évitées actualisées. ",
                "Le DALY est l'unité commune qui permet de comparer des activités différentes."),
        tags$li(strong("Coût net par DALY évitée"), " : on retire les économies générées (soins évités, dépenses des ménages…)."),
        tags$li(strong("ICER"), " = (coût de l'innovation − coût de la pratique actuelle) / ",
                "(DALY évitées par l'innovation − DALY évitées par la pratique actuelle)."),
        tags$li(strong("Retour financier"), " = (économies − coûts) / coûts.")
      ),
      h3("Calcul"),
      p("Coût d'une activité = investissement + coût de fonctionnement chaque année. ",
        "Résultats = résultats par an × part due à l'innovation. DALY = résultats × DALY par résultat. ",
        "Coûts, résultats et DALY sont actualisés au même taux (3 % par défaut) ; l'année 1 n'est pas actualisée."),
      h3("Interprétation"),
      p("Le coût par DALY évitée est comparé à un seuil : celui que vous saisissez (par exemple celui de votre ",
        "ministère de la Santé ou de votre bailleur), sinon 0,5 × PIB par habitant. Les anciens repères de l'OMS ",
        "(1 à 3 × PIB par habitant) sont aujourd'hui jugés trop généreux."),
      tags$ul(
        tags$li(strong("Très coût-efficace"), " : moins de la moitié du seuil."),
        tags$li(strong("Coût-efficace"), " : en dessous du seuil."),
        tags$li(strong("À la limite"), " : entre 1 et 2 fois le seuil."),
        tags$li(strong("Peu coût-efficace"), " : plus de 2 fois le seuil."),
        tags$li(strong("Dominante"), " : l'innovation évite des DALY et économise plus qu'elle ne coûte.")
      ),
      h3("Où trouver les DALY par résultat ?"),
      p("Études de coût-efficacité publiées pour des interventions proches, Global Burden of Disease (IHME), ",
        "WHO-CHOICE, Tufts CEA Registry. Indiquez vos sources dans la description du projet.")
    ))
  )
)

# ---- Serveur ----------------------------------------------------------------

server <- function(input, output, session) {

  num <- function(id, defaut = 0) {
    v <- input[[id]]
    if (is.null(v) || length(v) != 1 || is.na(v)) defaut else v
  }
  txt <- function(id) trimws(input[[id]] %||% "")

  monnaie <- reactive(if (nzchar(txt("monnaie"))) txt("monnaie") else "")
  argent <- function(x) if (is.null(x) || is.na(x)) "—" else paste(fmt(x), monnaie())
  en_usd <- function(x) {
    fx <- num("taux_change", NA)
    if (is.na(fx) || fx <= 0 || is.na(x)) NULL else paste("≈", fmt(x / fx), "USD")
  }

  # -- Activités ajoutées et supprimées librement --

  ids <- reactiveVal(integer(0))
  prochain <- 0L

  ajouter_activite <- function(valeurs = NULL) {
    prochain <<- prochain + 1L
    id <- prochain
    insertUI("#liste_activites", "beforeEnd", carte_activite(id, valeurs), immediate = TRUE)
    ids(c(isolate(ids()), id))
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

  champs_numeriques <- c("investissement", "recurrent", "beneficiaires", "resultats",
                         "daly_par_resultat", "economie_par_resultat")

  activites <- reactive({
    i <- ids()
    if (length(i) == 0) return(NULL)
    do.call(rbind, lapply(seq_along(i), function(k) {
      id <- i[k]
      ligne <- list(nom = txt(paste0("nom_", id)), resultat = txt(paste0("resultat_", id)))
      if (!nzchar(ligne$nom)) ligne$nom <- paste("Activité", k)
      if (!nzchar(ligne$resultat)) ligne$resultat <- "résultats"
      for (ch in champs_numeriques) ligne[[ch]] <- num(paste0(ch, "_", id))
      as.data.frame(ligne)
    }))
  })

  parametres <- reactive(list(
    duree = max(1, round(num("duree", 1))),
    taux_actualisation = num("taux_actualisation") / 100,
    attribution = num("attribution", 100) / 100
  ))

  reference <- reactive(list(cout_annuel = num("ref_cout"), daly_annuels = num("ref_daly")))

  res <- reactive({
    a <- activites()
    if (is.null(a)) NULL else calcul_ce(parametres(), a, reference())
  })

  seuil <- reactive(seuil_effectif(num("seuil", NA), num("pib_hab", NA)))

  # -- Démarrage, exemple et navigation --

  charger_exemple <- function() {
    updateTextInput(session, "nom", value = "Diagnostic mobile du paludisme (exemple)")
    updateTextInput(session, "organisation", value = "Projet fictif")
    updateTextInput(session, "pays", value = "Burkina Faso")
    updateTextInput(session, "monnaie", value = "XOF")
    updateTextAreaInput(session, "description",
                        value = "Exemple fictif : des agents communautaires dépistent et traitent le paludisme à domicile.")
    updateNumericInput(session, "pib_hab", value = 500000)
    updateNumericInput(session, "taux_change", value = 600)
    remplir_activites(exemple_activites())
  }

  charger_exemple()

  observeEvent(input$demarrer_exemple, {
    charger_exemple()
    nav_select("onglets", "resultats")
  })

  observeEvent(input$demarrer_vide, {
    for (id in c("nom", "organisation", "pays", "description")) updateTextInput(session, id, value = "")
    updateTextAreaInput(session, "description", value = "")
    for (id in c("pib_hab", "seuil", "taux_change")) updateNumericInput(session, id, value = NA)
    updateNumericInput(session, "ref_cout", value = 0)
    updateNumericInput(session, "ref_daly", value = 0)
    vider_activites()
    ajouter_activite()
    nav_select("onglets", "projet")
  })

  observeEvent(input$ajouter, ajouter_activite())
  observeEvent(input$vers_activites, nav_select("onglets", "activites"))
  observeEvent(input$vers_resultats, nav_select("onglets", "resultats"))

  # -- Résultats --

  output$bandeau <- renderUI({
    r <- res()
    if (is.null(r)) {
      return(div(class = "verdict", style = paste0("background:", couleurs_verdict[["inconnu"]]),
                 div(class = "phrase", "Ajoutez au moins une activité pour voir les résultats.")))
    }
    t <- r$total
    v <- verdict(t$cout_par_daly, seuil())
    titre <- if (nzchar(txt("nom"))) txt("nom") else "Votre innovation"
    detail <- if (!is.na(seuil()) && !is.na(t$cout_par_daly)) {
      paste0("Seuil retenu : ", argent(seuil()), " par DALY évitée — soit ",
             fmt(t$cout_par_daly / seuil(), 2), " × le seuil.")
    }
    div(class = "verdict", style = paste0("background:", couleurs_verdict[[v$niveau]]),
        div(class = "detail", titre, if (nzchar(txt("pays"))) paste(" ·", txt("pays"))),
        div(class = "chiffre", if (is.na(t$cout_par_daly)) "—" else argent(t$cout_par_daly),
            span(class = "unite", " par DALY évitée")),
        div(class = "phrase", v$texte),
        if (!is.null(detail)) div(class = "detail", detail))
  })

  kpi <- function(titre, valeur, detail = NULL) {
    value_box(title = titre, value = valeur, if (!is.null(detail)) p(detail), class = "kpi")
  }

  output$kpis <- renderUI({
    r <- res()
    if (is.null(r)) return(NULL)
    t <- r$total
    layout_column_wrap(
      width = 1 / 4, class = "mb-4",
      kpi("Coût total (actualisé)", argent(t$cout), en_usd(t$cout)),
      kpi("Bénéficiaires", fmt(t$beneficiaires), paste("sur", parametres()$duree, "an(s)")),
      kpi("Coût par bénéficiaire", argent(t$cout_par_beneficiaire), en_usd(t$cout_par_beneficiaire)),
      kpi("DALY évitées", fmt(t$daly, 1), "années de vie en bonne santé"),
      kpi("Coût par DALY évitée", argent(t$cout_par_daly), en_usd(t$cout_par_daly)),
      kpi("Coût net par DALY évitée", argent(t$cout_net_par_daly), "après économies générées"),
      kpi("ICER vs pratique actuelle", argent(t$icer),
          if (is.na(t$icer)) "renseignez la situation de référence" else "coût par DALY évitée en plus"),
      kpi("Retour financier", fmt_pct(t$roi), paste("économies :", argent(t$economies)))
    )
  })

  output$graphique <- renderPlot({
    r <- res()
    req(r)
    noms <- vapply(r$activites, `[[`, "", "nom")
    valeurs <- vapply(r$activites, function(a) a$cout_par_daly %||% NA_real_, 0)
    garde <- !is.na(valeurs)
    par(family = "serif", mar = c(4, 1, 1, 1), bg = "#ffffff", las = 1)
    if (!any(garde)) {
      plot.new()
      text(0.5, 0.5, "Aucune activité avec des DALY évitées.", cex = 1.2, col = "#6c7a80")
      return()
    }
    noms <- noms[garde]; valeurs <- valeurs[garde]
    o <- order(valeurs, decreasing = TRUE)
    noms <- noms[o]; valeurs <- valeurs[o]
    etiquettes <- ifelse(nchar(noms) > 38, paste0(substr(noms, 1, 36), "…"), noms)
    par(mar = c(4, min(22, 2 + max(nchar(etiquettes)) * 0.45), 1, 2))
    s <- seuil()
    xmax <- max(c(valeurs, if (!is.na(s)) s), 1) * 1.15
    couleurs <- if (is.na(s)) rep("#0f5257", length(valeurs)) else
      ifelse(valeurs <= s, "#1d7a4f", ifelse(valeurs <= 2 * s, "#c08a2e", "#a23b2a"))
    y <- barplot(valeurs, horiz = TRUE, col = couleurs, border = NA, xlim = c(0, xmax),
                 axes = FALSE, space = 0.6, names.arg = etiquettes, cex.names = 1.05,
                 col.axis = "#1f2a2e")
    axis(1, at = pretty(c(0, xmax)), labels = vapply(pretty(c(0, xmax)), fmt, ""),
         col = "#cfc6b6", col.axis = "#5b6a6e", cex.axis = 0.9)
    text(valeurs, y, vapply(valeurs, fmt, ""), pos = 4, cex = 0.95, col = "#1f2a2e")
    if (!is.na(s)) {
      abline(v = s, lty = 2, col = "#1f2a2e")
      mtext("seuil", side = 3, at = s, line = -1, cex = 0.9, col = "#1f2a2e")
    }
    mtext(paste("Coût par DALY évitée", if (nzchar(monnaie())) paste0("(", monnaie(), ")")),
          side = 1, line = 2.5, col = "#5b6a6e")
  })

  table_activites <- reactive({
    r <- res()
    req(r)
    cout_total <- r$total$cout
    s <- seuil()
    do.call(rbind, lapply(r$activites, function(a) data.frame(
      `Activité` = a$nom,
      `Coût (actualisé)` = fmt(a$cout),
      `Part du coût` = fmt_pct(ratio(a$cout, cout_total)),
      `Bénéficiaires` = fmt(a$beneficiaires),
      `Coût / bénéficiaire` = fmt(a$cout_par_beneficiaire),
      `Coût par résultat` = if (is.na(a$cout_par_resultat)) "—" else paste(fmt(a$cout_par_resultat), "par", a$resultat),
      `DALY évitées` = fmt(a$daly, 1),
      `Coût / DALY` = fmt(a$cout_par_daly),
      `Verdict` = if (is.na(a$cout_par_daly)) "activité de support" else
        sub(" :.*", "", verdict(a$cout_par_daly, s)$texte),
      check.names = FALSE
    )))
  })

  output$table_activites <- renderTable(table_activites(), striped = TRUE, hover = TRUE, width = "100%")

  output$table_sensibilite <- renderTable({
    a <- activites()
    req(a)
    s <- sensibilite(parametres(), a, reference())
    data.frame(`Scénario` = s$scenario, `Coût / DALY` = vapply(s$cout_par_daly, fmt, ""),
               `Coût / bénéficiaire` = vapply(s$cout_par_beneficiaire, fmt, ""),
               check.names = FALSE)
  }, striped = TRUE, width = "100%")

  output$telecharger <- downloadHandler(
    filename = function() "cout_efficacite_activites.csv",
    content = function(file) write.csv(table_activites(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  # -- Enregistrer / ouvrir un projet --

  champs_texte <- c("nom", "organisation", "pays", "monnaie", "description")
  champs_param <- c("duree", "taux_actualisation", "attribution", "pib_hab", "seuil", "taux_change",
                    "ref_cout", "ref_daly")

  output$sauver <- downloadHandler(
    filename = function() {
      base <- gsub("[^A-Za-z0-9_-]+", "_", if (nzchar(txt("nom"))) txt("nom") else "projet")
      paste0(base, ".json")
    },
    content = function(file) {
      projet <- list(
        textes = lapply(setNames(champs_texte, champs_texte), txt),
        parametres = lapply(setNames(champs_param, champs_param), function(id) num(id, NA)),
        activites = activites()
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
    for (id in champs_texte) {
      v <- projet$textes[[id]] %||% ""
      if (id == "description") updateTextAreaInput(session, id, value = v)
      else updateTextInput(session, id, value = v)
    }
    for (id in champs_param) {
      v <- projet$parametres[[id]]
      updateNumericInput(session, id, value = if (is.null(v)) NA else v)
    }
    remplir_activites(projet$activites)
    showNotification("Projet ouvert.", type = "message")
  })
}

shinyApp(ui, server)
