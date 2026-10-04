library(shiny)
library(bslib)

source("calcul.R")
source("modeles.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

MAX_COUTS <- 8
MAX_RESULTATS <- 8

pays_defaut <- "BF"
type_defaut <- "asc"
couts_defaut <- couts_exemple(type_defaut, pays[[pays_defaut]]$taux_change)
resultats_defaut <- resultats_exemple(type_defaut, pays[[pays_defaut]]$taux_change)

choix_pays <- setNames(names(pays), vapply(pays, `[[`, "", "nom"))

fmt <- function(x, decimales = 0) {
  if (is.null(x) || is.na(x)) return("—")
  formatC(x, format = "f", digits = decimales, big.mark = "\u00a0", decimal.mark = ",")
}
# Espaces insécables pour que « -35 % » ou « 1 : 0,75 » ne soient jamais coupés.
fmt_pct <- function(x) if (is.na(x)) "—" else paste0(fmt(100 * x, 0), "\u00a0%")
fmt_sroi <- function(x) if (is.na(x)) "—" else paste0("1\u00a0:\u00a0", fmt(x, 2))

# ---- Composants d'interface -------------------------------------------------

ligne_cout <- function(i, ligne = NULL) {
  val <- function(col, defaut) if (!is.null(ligne)) ligne[[col]] else defaut
  conditionalPanel(
    condition = sprintf("input.n_couts >= %d", i),
    layout_columns(
      col_widths = c(6, 3, 3),
      textInput(paste0("cout_libelle_", i), if (i == 1) "Poste de coût" else NULL,
                val("libelle", "")),
      numericInput(paste0("cout_invest_", i), if (i == 1) "Investissement (année 1)" else NULL,
                   val("investissement", 0), min = 0),
      numericInput(paste0("cout_recurrent_", i), if (i == 1) "Coût récurrent / an" else NULL,
                   val("recurrent", 0), min = 0)
    )
  )
}

champs_resultat <- list(
  list(id = "unites", label = "Unités en année 1", pct = FALSE),
  list(id = "croissance", label = "Croissance annuelle (%)", pct = TRUE),
  list(id = "eco_systeme", label = "Économie système de santé / unité", pct = FALSE),
  list(id = "eco_menages", label = "Économie ménages / unité", pct = FALSE),
  list(id = "productivite", label = "Gain de productivité / unité", pct = FALSE),
  list(id = "valeur_sociale", label = "Valeur sociale / unité (SROI)", pct = FALSE),
  list(id = "daly", label = "DALY évitées / unité", pct = FALSE),
  list(id = "duree_effet", label = "Durée de l'effet (années)", pct = FALSE),
  list(id = "deadweight", label = "Deadweight (%)", pct = TRUE),
  list(id = "attribution", label = "Attribution à d'autres (%)", pct = TRUE),
  list(id = "displacement", label = "Déplacement (%)", pct = TRUE),
  list(id = "drop_off", label = "Baisse annuelle de l'effet (%)", pct = TRUE)
)

carte_resultat <- function(i, ligne = NULL) {
  champs <- lapply(champs_resultat, function(ch) {
    v <- if (!is.null(ligne)) ligne[[ch$id]] else 0
    if (ch$pct) v <- 100 * v
    numericInput(paste0("res_", ch$id, "_", i), ch$label, v, min = 0)
  })
  conditionalPanel(
    condition = sprintf("input.n_resultats >= %d", i),
    card(
      card_header(textInput(paste0("res_libelle_", i), paste("Résultat", i),
                            if (!is.null(ligne)) ligne$libelle else "", width = "100%")),
      do.call(layout_column_wrap, c(list(width = "200px"), champs))
    )
  )
}

ligne_ou_null <- function(df, i) if (i <= nrow(df)) df[i, ] else NULL

# ---- Interface --------------------------------------------------------------

ui <- page_navbar(
  title = "ROI Santé Communautaire",
  fillable = FALSE,
  theme = bs_theme(version = 5, primary = "#1b7f5c"),

  nav_panel(
    "1. Projet",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Description du projet"),
        textInput("nom", "Nom du projet", "Mon projet de santé communautaire"),
        selectInput("pays", "Pays", choix_pays, pays_defaut),
        selectInput("type", "Type de projet", types_projet, type_defaut),
        actionButton("charger", "Charger les valeurs d'exemple pour ce type et ce pays",
                     class = "btn-outline-primary"),
        p(class = "text-muted small mt-2",
          "Les valeurs d'exemple sont indicatives : remplacez-les par vos données.")
      ),
      card(
        card_header("Paramètres économiques"),
        layout_column_wrap(
          width = "200px",
          numericInput("annee_debut", "Année de début", 2026, min = 2000, max = 2100),
          numericInput("duree", "Durée du projet (années)", 3, min = 1, max = 20),
          numericInput("taux_actualisation", "Taux d'actualisation (%)", 3, min = 0, max = 20, step = 0.5),
          numericInput("taux_change", "Monnaie locale pour 1 USD", pays[[pays_defaut]]$taux_change, min = 0),
          numericInput("pib_hab", "PIB par habitant (USD)", pays[[pays_defaut]]$pib_hab_usd, min = 0)
        ),
        uiOutput("info_monnaie"),
        p(class = "text-muted small",
          "Taux de change et PIB/habitant : vérifiez les valeurs à jour (Banque mondiale, banque centrale).")
      )
    ),
    card(
      card_header("Sauvegarder / reprendre un projet"),
      layout_columns(
        col_widths = c(6, 6),
        downloadButton("sauver", "Enregistrer le projet (.json)"),
        fileInput("ouvrir", NULL, accept = ".json", buttonLabel = "Ouvrir un projet…",
                  placeholder = "fichier .json")
      )
    )
  ),

  nav_panel(
    "2. Coûts",
    card(
      card_header("Coûts du projet (en monnaie locale)"),
      numericInput("n_couts", "Nombre de postes de coût", nrow(couts_defaut), min = 1, max = MAX_COUTS),
      lapply(seq_len(MAX_COUTS), function(i) ligne_cout(i, ligne_ou_null(couts_defaut, i))),
      uiOutput("total_couts")
    )
  ),

  nav_panel(
    "3. Résultats",
    card(
      card_header("Résultats de santé produits par le projet"),
      p("Pour chaque résultat : combien d'unités le projet produit, ce que vaut une unité ",
        "(en monnaie locale) et quelle part revient réellement au projet."),
      numericInput("n_resultats", "Nombre de résultats", nrow(resultats_defaut), min = 1, max = MAX_RESULTATS)
    ),
    lapply(seq_len(MAX_RESULTATS), function(i) carte_resultat(i, ligne_ou_null(resultats_defaut, i)))
  ),

  nav_panel(
    "4. Analyse",
    radioButtons("methode", "Méthode", inline = TRUE,
                 c("Toutes" = "toutes", "ROI financier" = "roi", "SROI" = "sroi",
                   "Coût-efficacité (DALY)" = "cea")),
    conditionalPanel(
      "input.methode == 'toutes' || input.methode == 'roi'",
      h4("ROI financier"),
      uiOutput("boites_roi")
    ),
    conditionalPanel(
      "input.methode == 'toutes' || input.methode == 'sroi'",
      h4("Retour social sur investissement (SROI)"),
      uiOutput("boites_sroi")
    ),
    conditionalPanel(
      "input.methode == 'toutes' || input.methode == 'cea'",
      h4("Coût-efficacité"),
      uiOutput("boites_cea")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Coûts et bénéfices cumulés (actualisés)"), plotOutput("graphique", height = "320px")),
      card(card_header("Analyse de sensibilité"), tableOutput("table_sensibilite"))
    ),
    card(
      card_header("Flux annuels (non actualisés)"),
      tableOutput("table_flux"),
      downloadButton("telecharger_flux", "Télécharger (.csv)", class = "btn-sm")
    )
  ),

  nav_panel(
    "Méthodologie",
    card(
      card_body(
        h4("ROI financier"),
        p("ROI = (VA des bénéfices − VA des coûts) / VA des coûts. Les bénéfices monétaires sont ",
          "les économies du système de santé, les économies des ménages et les gains de productivité. ",
          "Un ROI de 150 % signifie que chaque unité investie rapporte 1,5 unité en plus de son coût."),
        h4("SROI (Social Return on Investment)"),
        p("SROI = (VA des bénéfices monétaires + VA de la valeur sociale) / VA des coûts. ",
          "La valeur sociale donne un équivalent monétaire (proxy) aux effets non marchands : ",
          "bien-être, dignité, cohésion, confiance dans le système de santé… Un SROI de 1 : 3 signifie ",
          "que 1 unité investie crée 3 unités de valeur sociale."),
        h4("Coût-efficacité"),
        p("Coût par DALY évitée = VA des coûts / DALY évitées (actualisées au même taux). ",
          "Le coût net déduit les économies pour le système de santé et les ménages. On compare le résultat ",
          "au PIB par habitant : les anciens repères OMS-CHOICE (1 et 3 × PIB/hab) sont aujourd'hui jugés ",
          "trop généreux, et des seuils plus prudents (environ 0,5 × PIB/hab) sont souvent recommandés."),
        h4("Ajustements d'impact"),
        tags$ul(
          tags$li(strong("Deadweight : "), "part du résultat qui serait arrivée sans le projet."),
          tags$li(strong("Attribution : "), "part due à d'autres acteurs (État, autres ONG…)."),
          tags$li(strong("Déplacement : "), "part qui ne fait que déplacer le problème ailleurs."),
          tags$li(strong("Durée et baisse de l'effet : "),
                  "une unité peut produire de l'effet plusieurs années (ex. moustiquaire sur 3 ans), en diminuant chaque année.")
        ),
        p("Unités imputables au projet = unités × (1 − deadweight) × (1 − attribution) × (1 − déplacement)."),
        h4("Actualisation"),
        p("L'année 1 n'est pas actualisée ; l'année t est multipliée par 1 / (1 + taux)^(t − 1). ",
          "Le taux de 3 % est la référence courante pour les évaluations économiques en santé.")
      )
    )
  )
)

# ---- Serveur ----------------------------------------------------------------

server <- function(input, output, session) {

  monnaie <- reactive(pays[[input$pays]]$monnaie)

  # Paramètres économiques d'un projet ouvert, à appliquer après le changement de pays.
  parametres_en_attente <- reactiveVal(NULL)

  observeEvent(input$pays, {
    p <- parametres_en_attente()
    if (!is.null(p)) {
      parametres_en_attente(NULL)
      updateNumericInput(session, "taux_change", value = p$taux_change)
      updateNumericInput(session, "pib_hab", value = p$pib_hab_usd)
    } else {
      updateNumericInput(session, "taux_change", value = pays[[input$pays]]$taux_change)
      updateNumericInput(session, "pib_hab", value = pays[[input$pays]]$pib_hab_usd)
    }
  }, ignoreInit = TRUE)

  output$info_monnaie <- renderUI({
    p(strong("Monnaie : "), paste0(monnaie(), ". Toutes les valeurs monétaires des onglets ",
                                    "Coûts et Résultats sont en ", monnaie(), "."))
  })

  num <- function(id) {
    v <- input[[id]]
    if (is.null(v) || is.na(v)) 0 else v
  }

  parametres <- reactive(list(
    annee_debut = num("annee_debut"),
    duree = max(1, round(num("duree"))),
    taux_actualisation = num("taux_actualisation") / 100,
    taux_change = num("taux_change"),
    pib_hab_usd = num("pib_hab")
  ))

  couts <- reactive({
    n <- max(1, min(MAX_COUTS, round(num("n_couts"))))
    do.call(rbind, lapply(seq_len(n), function(i) data.frame(
      libelle = input[[paste0("cout_libelle_", i)]] %||% "",
      investissement = num(paste0("cout_invest_", i)),
      recurrent = num(paste0("cout_recurrent_", i))
    )))
  })

  resultats <- reactive({
    n <- max(1, min(MAX_RESULTATS, round(num("n_resultats"))))
    do.call(rbind, lapply(seq_len(n), function(i) {
      ligne <- list(libelle = input[[paste0("res_libelle_", i)]] %||% "")
      for (ch in champs_resultat) {
        v <- num(paste0("res_", ch$id, "_", i))
        ligne[[ch$id]] <- if (ch$pct) v / 100 else v
      }
      as.data.frame(ligne)
    }))
  })

  res <- reactive(calcul_roi(parametres(), couts(), resultats()))

  # Remplit les champs à partir de données (modèle d'exemple ou projet ouvert).
  remplir <- function(c, r) {
    updateNumericInput(session, "n_couts", value = nrow(c))
    for (i in seq_len(nrow(c))) {
      updateTextInput(session, paste0("cout_libelle_", i), value = c$libelle[i])
      updateNumericInput(session, paste0("cout_invest_", i), value = round(c$investissement[i]))
      updateNumericInput(session, paste0("cout_recurrent_", i), value = round(c$recurrent[i]))
    }
    updateNumericInput(session, "n_resultats", value = nrow(r))
    for (i in seq_len(nrow(r))) {
      updateTextInput(session, paste0("res_libelle_", i), value = r$libelle[i])
      for (ch in champs_resultat) {
        v <- r[[ch$id]][i]
        updateNumericInput(session, paste0("res_", ch$id, "_", i), value = if (ch$pct) 100 * v else v)
      }
    }
  }

  observeEvent(input$charger, {
    fx <- num("taux_change")
    remplir(couts_exemple(input$type, fx), resultats_exemple(input$type, fx))
    showNotification("Valeurs d'exemple chargées.", type = "message")
  })

  output$total_couts <- renderUI({
    c <- couts()
    d <- parametres()$duree
    total <- sum(c$investissement) + d * sum(c$recurrent)
    p(strong("Coût total sur ", d, " an(s) (non actualisé) : "), fmt(total), " ", monnaie(),
      " (≈ ", fmt(total / max(1, parametres()$taux_change)), " USD)")
  })

  boite <- function(titre, valeur, detail = NULL, theme = "primary") {
    value_box(title = titre, value = valeur, p(detail), theme = theme)
  }

  output$boites_roi <- renderUI({
    f <- res()$financier
    layout_columns(
      boite("ROI", fmt_pct(f$roi), "(bénéfices − coûts) / coûts",
            if (!is.na(f$roi) && f$roi >= 0) "success" else "danger"),
      boite("Ratio bénéfices / coûts", fmt(f$ratio_bc, 2)),
      boite("Valeur actuelle nette", paste(fmt(f$van), monnaie()),
            paste("≈", fmt(f$van / max(1, parametres()$taux_change)), "USD")),
      boite("Année de retour sur investissement",
            if (is.na(f$annee_retour)) "Non atteinte" else f$annee_retour)
    )
  })

  output$boites_sroi <- renderUI({
    s <- res()$sroi
    layout_columns(
      boite("Ratio SROI", fmt_sroi(s$ratio),
            "valeur créée pour 1 unité investie",
            if (!is.na(s$ratio) && s$ratio >= 1) "success" else "danger"),
      boite("Valeur financière (VA)", paste(fmt(s$va_financiere), monnaie())),
      boite("Valeur sociale (VA)", paste(fmt(s$va_sociale), monnaie())),
      boite("Coûts (VA)", paste(fmt(s$va_couts), monnaie()))
    )
  })

  output$boites_cea <- renderUI({
    e <- res()$cea
    layout_columns(
      boite("DALY évitées", fmt(e$daly, 1), "actualisées"),
      boite("Coût par DALY évitée", paste(fmt(e$cout_par_daly), monnaie()),
            paste("≈", fmt(e$cout_par_daly_usd), "USD")),
      boite("Coût net par DALY évitée", paste(fmt(e$cout_net_par_daly), monnaie()),
            "après économies de soins"),
      boite("Coût par DALY / PIB par habitant", fmt(e$ratio_pib, 2), interpretation_cea(e$ratio_pib),
            if (!is.na(e$ratio_pib) && e$ratio_pib < 1) "success" else "warning")
    )
  })

  output$graphique <- renderPlot({
    f <- res()$flux
    ymax <- max(c(f$couts_cumules, f$valeur_totale_cumulee), 1)
    par(mar = c(4, 6, 1, 1), las = 1)
    plot(f$annee, f$couts_cumules, type = "b", pch = 16, col = "#c0392b", lwd = 2,
         ylim = c(0, ymax), xlab = "Année", ylab = "", xaxt = "n", yaxt = "n")
    axis(1, at = f$annee)
    axis(2, at = pretty(c(0, ymax)), labels = formatC(pretty(c(0, ymax)), format = "d", big.mark = " "),
         cex.axis = 0.8)
    lines(f$annee, f$benefices_cumules, type = "b", pch = 16, col = "#1b7f5c", lwd = 2)
    lines(f$annee, f$valeur_totale_cumulee, type = "b", pch = 1, col = "#2471a3", lwd = 2, lty = 2)
    legend("topleft", bty = "n",
           legend = c("Coûts", "Bénéfices financiers", "Valeur totale (SROI)"),
           col = c("#c0392b", "#1b7f5c", "#2471a3"), lty = c(1, 1, 2), lwd = 2)
  })

  output$table_sensibilite <- renderTable({
    s <- sensibilite(parametres(), couts(), resultats())
    data.frame(
      `Scénario` = s$Scenario,
      ROI = vapply(s$ROI, fmt_pct, ""),
      SROI = vapply(s$SROI, fmt_sroi, ""),
      `Coût / DALY` = vapply(s$CoutParDALY, fmt, ""),
      check.names = FALSE
    )
  })

  table_flux <- reactive({
    f <- res()$flux
    data.frame(
      `Année` = f$annee,
      `Coûts` = f$couts,
      `Économies système de santé` = f$eco_systeme,
      `Économies ménages` = f$eco_menages,
      `Productivité` = f$productivite,
      `Valeur sociale` = f$valeur_sociale,
      `DALY évitées` = f$daly,
      check.names = FALSE
    )
  })

  output$table_flux <- renderTable({
    t <- table_flux()
    t[-1] <- lapply(names(t)[-1], function(n) vapply(t[[n]], fmt, "", if (n == "DALY évitées") 1 else 0))
    t$`Année` <- as.character(t$`Année`)
    t
  })

  output$telecharger_flux <- downloadHandler(
    filename = function() "flux_annuels.csv",
    content = function(file) write.csv(table_flux(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  output$sauver <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9_-]+", "_", input$nom), ".json"),
    content = function(file) {
      projet <- list(
        nom = input$nom, pays = input$pays, type = input$type,
        parametres = parametres(), couts = couts(), resultats = resultats()
      )
      writeLines(jsonlite::toJSON(projet, auto_unbox = TRUE, pretty = TRUE, digits = NA), file)
    }
  )

  observeEvent(input$ouvrir, {
    projet <- tryCatch(jsonlite::fromJSON(input$ouvrir$datapath), error = function(e) NULL)
    if (is.null(projet) || is.null(projet$couts) || is.null(projet$resultats)) {
      showNotification("Fichier de projet invalide.", type = "error")
      return()
    }
    p <- projet$parametres
    if (!identical(projet$pays, input$pays)) parametres_en_attente(p)
    updateTextInput(session, "nom", value = projet$nom)
    updateSelectInput(session, "pays", selected = projet$pays)
    updateSelectInput(session, "type", selected = projet$type)
    updateNumericInput(session, "annee_debut", value = p$annee_debut)
    updateNumericInput(session, "duree", value = p$duree)
    updateNumericInput(session, "taux_actualisation", value = 100 * p$taux_actualisation)
    updateNumericInput(session, "taux_change", value = p$taux_change)
    updateNumericInput(session, "pib_hab", value = p$pib_hab_usd)
    remplir(projet$couts, projet$resultats)
    showNotification("Projet ouvert.", type = "message")
  })
}

shinyApp(ui, server)
