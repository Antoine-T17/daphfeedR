library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(scales)
library(ggiraph)
library(bslib)
library(shinydashboard)

ui <- bslib::page_navbar(
  title = "Nutrition des daphnies",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

  # CSS qui permet de centrer mes valeurs dans les différents tableaux

  tags$head(
    tags$style(HTML("
    /* Tableaux renderTable */
    #unknown_table table th,
    #unknown_table table td,
    #reg_table table th,
    #reg_table table td {
      text-align: center !important;
      vertical-align: middle !important;
    }

    /* Petits tableaux HTML de l'onglet Calculs */
    .table th,
    .table td {
      text-align: center !important;
      vertical-align: middle !important;
    }

    /* Champs textInput dans les petits tableaux */
    .table input.form-control {
      text-align: center !important;
      padding-left: 6px !important;
      padding-right: 6px !important;
    }

    /* Enlève l'espace inutile des textInput compacts */
    .table .form-group {
      margin-bottom: 0 !important;
    }
  "))
  ),

  # ---- Page 1 : Visualisation ----
  bslib::nav_panel(
    "Visualisation",

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 320,
        fileInput("file", "Fichier Excel", accept = c(".xlsx", ".xls")),
        numericInput("sheet", "Feuille", value = 2, min = 1),
        selectInput(
          "species_view", "Afficher",
          choices = c("Les deux", "Chlorella", "Pita"),
          selected = "Les deux"
        ),
        selectInput(
          "reg_mode", "Mode de régression",
          choices = c("Poolée par espèce", "Par date et espèce"),
          selected = "Poolée par espèce"
        ),
        selectizeInput(
          "dates_selected",
          "Dates à inclure",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "Sélectionner une ou plusieurs dates")
        )
      ),

      div(
        style = "display: flex; flex-direction: column; gap: 14px; width: 100%;",

        bslib::card(
          full_screen = TRUE,
          girafeOutput("plot_final", width = "100%", height = "700px")
        ),

        bslib::card(
          bslib::card_header("Équations et R²"),
          div(
            style = "overflow-x: auto;",
            tableOutput("reg_table")
          )
        )
      )
    )
  ),

  # ---- Page 2 : Calculs ----
  bslib::nav_panel(
    "Calculs",

    div(
      style = "display: flex; flex-direction: column; gap: 14px;",

      # Ligne 1 : deux tableaux côte à côte
      bslib::layout_columns(
        col_widths = c(6, 6),

        bslib::card(
          bslib::card_header("Chlorella"),
          tags$table(
            class = "table table-sm table-bordered align-middle mb-0",
            tags$thead(
              tags$tr(
                tags$th("Abs 1"),
                tags$th("Abs 2"),
                tags$th("Abs 3"),
                tags$th("FD mesure"),
                tags$th("Ratio concentration"),
                tags$th("Volume final (mL)")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(textInput("chl_a1", NULL, value = "", width = "80px")),
                tags$td(textInput("chl_a2", NULL, value = "", width = "80px")),
                tags$td(textInput("chl_a3", NULL, value = "", width = "80px")),
                tags$td(textInput("chl_meas_fd", NULL, value = "1", width = "80px")),
                tags$td(textInput("chl_ratio", NULL, value = "216000", width = "90px")),
                tags$td(textInput("volume_a_nourrir", NULL, value = "2000", width = "90px"))
              )
            )
          )
        ),

        bslib::card(
          bslib::card_header("Pita"),
          tags$table(
            class = "table table-sm table-bordered align-middle mb-0",
            tags$thead(
              tags$tr(
                tags$th("Abs 1"),
                tags$th("Abs 2"),
                tags$th("Abs 3"),
                tags$th("FD mesure"),
                tags$th("Ratio concentration"),
                tags$th("Volume final (mL)")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(textInput("pita_a1", NULL, value = "", width = "80px")),
                tags$td(textInput("pita_a2", NULL, value = "", width = "80px")),
                tags$td(textInput("pita_a3", NULL, value = "", width = "80px")),
                tags$td(textInput("pita_meas_fd", NULL, value = "1", width = "80px")),
                tags$td(textInput("pita_ratio", NULL, value = "144000", width = "90px")),
                tags$td(textInput("volume_a_nourrir", NULL, value = "2000", width = "90px"))
              )
            )
          )
        )
      ),

      # Ligne 2 : résultats
      bslib::card(
        bslib::card_header("Résultats"),
        tableOutput("unknown_table")
      )
    )
  )
)

server <- function(input, output, session) {

  # ==================================================
  # HELPER qui accepte les "." ou les "," en décimale
  # ==================================================

  parse_num <- function(x) {
    if (is.null(x) || identical(trimws(x), "")) return(NA_real_)
    x <- gsub(",", ".", trimws(x), fixed = TRUE)
    suppressWarnings(as.numeric(x))
  }

  # =========================
  # Données brutes Excel
  # =========================
  df_data <- reactive({
    req(input$file)

    df <- tryCatch(
      read_excel(input$file$datapath, sheet = input$sheet),
      error = function(e) NULL
    )

    validate(
      need(
        !is.null(df),
        "Impossible de lire cette feuille Excel. Vérifiez le numéro de feuille ou chargez un autre fichier."
      ),
      need(
        ncol(df) >= 16,
        "Les colonnes attendues n'ont pas été trouvées dans cette feuille. Vérifiez le numéro de feuille ou chargez un autre fichier Excel."
      )
    )

    # On ne garde que les 16 premières colonnes attendues
    df <- df[, 1:16]

    names(df) <- c(
      "sample", "date", "species", "FD", "inv_FD", "A1", "A2", "A3",
      "A_mean", "A_var", "A_sd", "A_sem",
      "C_mean", "C_sem", "C_mean_fd", "C_sem_fd"
    )

    # -------------------------
    # Conversion prudente des colonnes numériques
    # -------------------------
    num_cols <- c(
      "FD", "inv_FD", "A1", "A2", "A3",
      "A_mean", "A_var", "A_sd", "A_sem",
      "C_mean", "C_sem", "C_mean_fd", "C_sem_fd"
    )

    df[num_cols] <- lapply(df[num_cols], function(x) {
      if (is.numeric(x)) return(x)
      x <- gsub(",", ".", trimws(as.character(x)), fixed = TRUE)
      suppressWarnings(as.numeric(x))
    })

    # Vérifie que les colonnes essentielles sont exploitables
    essential_num <- c("A_mean", "A_sem", "C_mean_fd", "C_sem_fd")

    validate(
      need(
        all(vapply(df[essential_num], function(x) !all(is.na(x)), logical(1))),
        paste(
          "Les colonnes attendues n'ont pas été trouvées dans un format exploitable",
          "(A_mean, A_sem, C_mean_fd, C_sem_fd).",
          "Vérifiez le numéro de la feuille ou chargez un autre fichier Excel."
        )
      )
    )

    # -------------------------
    # Vérifie la colonne species
    # -------------------------
    species_chr <- trimws(as.character(df$species))
    valid_species <- c("Chlorella", "Pita")

    validate(
      need(
        any(species_chr %in% valid_species),
        "La colonne 'species' ne contient pas les valeurs attendues ('Chlorella' et/ou 'Pita'). Vérifiez le numéro de la feuille ou chargez un autre fichier Excel."
      )
    )

    df$species <- species_chr

    # -------------------------
    # Gestion robuste des dates (sans crash)
    # -------------------------
    raw_date <- df$date

    date_chr <- if (inherits(raw_date, "Date") || inherits(raw_date, "POSIXt")) {
      format(as.Date(raw_date), "%d/%m/%Y")
    } else {
      as.character(raw_date)
    }

    date_try1 <- suppressWarnings(
      tryCatch(
        as.Date(date_chr, format = "%d/%m/%Y"),
        error = function(e) rep(as.Date(NA), length(date_chr))
      )
    )

    date_try2 <- suppressWarnings(
      tryCatch(
        as.Date(date_chr, format = "%Y-%m-%d"),
        error = function(e) rep(as.Date(NA), length(date_chr))
      )
    )

    date_order <- dplyr::coalesce(date_try1, date_try2)

    date_levels <- df %>%
      mutate(
        date_chr = date_chr,
        date_order = date_order
      ) %>%
      distinct(date_chr, date_order) %>%
      arrange(is.na(date_order), date_order, date_chr) %>%
      pull(date_chr)

    # -------------------------
    # Sortie finale propre
    # -------------------------
    df %>%
      mutate(
        date_chr = date_chr,
        date_order = date_order,
        date = factor(date_chr, levels = date_levels),
        lower_y = A_mean - A_sem,
        upper_y = A_mean + A_sem,
        lower_x = C_mean_fd - C_sem_fd,
        upper_x = C_mean_fd + C_sem_fd,
        tooltip_txt = paste0(
          "Espèce : ", species,
          "\nDate : ", as.character(date),
          "\nConcentration : ", format(round(C_mean_fd, 0), big.mark = " ", scientific = FALSE),
          " cellules/mL",
          "\nAbsorbance : ", format(round(A_mean, 3), decimal.mark = ",")
        )
      )
  })

  # =========================
  # Mise à jour des dates sélectionnables
  # =========================
  observe({
    req(input$file)
    df <- df_data()

    if (input$species_view != "Les deux") {
      df <- df %>% filter(species == input$species_view)
    }

    date_choices <- df %>%
      distinct(date, date_order) %>%
      arrange(is.na(date_order), date_order, date) %>%
      pull(date) %>%
      as.character()

    previous <- isolate(input$dates_selected)
    selected_keep <- intersect(previous, date_choices)

    if (length(selected_keep) == 0) {
      selected_keep <- date_choices
    }

    updateSelectizeInput(
      session,
      "dates_selected",
      choices = date_choices,
      selected = selected_keep,
      server = TRUE
    )
  })

  # =========================
  # Données filtrées pour la visualisation
  # =========================
  df_plot <- reactive({
    df <- df_data()

    if (input$species_view != "Les deux") {
      df <- df %>% filter(species == input$species_view)
    }

    if (!is.null(input$dates_selected) && length(input$dates_selected) > 0) {
      df <- df %>% filter(as.character(date) %in% input$dates_selected)
    }

    req(nrow(df) > 0)
    df
  })

  # =========================
  # Modèles poolés (pour l'onglet Calculs) - inchangé
  # =========================
  calib_models <- reactive({
    df <- df_data()

    list(
      chlorella = lm(A_mean ~ C_mean_fd, data = df %>% filter(species == "Chlorella")),
      pita      = lm(A_mean ~ C_mean_fd, data = df %>% filter(species == "Pita"))
    )
  })

  # =========================
  # Tableau des régressions (visualisation)
  # =========================
  reg_summary <- reactive({
    df <- df_plot()

    fit_one <- function(dat) {
      if (nrow(dat) < 2 || dplyr::n_distinct(dat$C_mean_fd) < 2) {
        return(tibble(
          slope = NA_real_,
          intercept = NA_real_,
          r2 = NA_real_
        ))
      }

      mod <- lm(A_mean ~ C_mean_fd, data = dat)
      co <- coef(mod)

      tibble(
        slope = unname(co["C_mean_fd"]),
        intercept = unname(co["(Intercept)"]),
        r2 = unname(summary(mod)$r.squared)
      )
    }

    if (input$reg_mode == "Poolée par espèce") {
      out <- df %>%
        group_by(species) %>%
        group_modify(~ fit_one(.x)) %>%
        ungroup() %>%
        mutate(date_lab = "Dates sélectionnées")
    } else {
      out <- df %>%
        group_by(species, date) %>%
        group_modify(~ fit_one(.x)) %>%
        ungroup() %>%
        mutate(date_lab = as.character(date))
    }

    out %>%
      mutate(
        Equation = ifelse(
          is.na(slope),
          "Impossible à calculer",
          paste0(
            "y = ",
            sprintf("%.3e", slope),
            "x + ",
            sprintf("%.3e", intercept)
          )
        ),
        `R²` = ifelse(is.na(r2), "", sprintf("%.3f", r2))
      ) %>%
      transmute(
        Espèce = species,
        Date = date_lab,
        Equation,
        `R²`
      )
  })

  output$reg_table <- renderTable({
    reg_summary()
  },
  striped = TRUE, bordered = TRUE, spacing = "s", na = "")

  # =========================
  # Calcul solution inconnue (reste poolé sur toutes les dates)
  # =========================
  unknown_calc <- reactive({
    req(input$file)
    mods <- calib_models()

    # Constantes préremplies
    ratio_chl <- parse_num(input$chl_ratio)
    ratio_pit <- parse_num(input$pita_ratio)
    final_volume_ml <- parse_num(input$volume_a_nourrir)

    # Coefficients y = a*x + b
    co_chl <- coef(mods$chlorella)
    a_chl <- unname(co_chl["C_mean_fd"])
    b_chl <- unname(co_chl["(Intercept)"])

    co_pit <- coef(mods$pita)
    a_pit <- unname(co_pit["C_mean_fd"])
    b_pit <- unname(co_pit["(Intercept)"])

    # Saisie utilisateur (textInput -> conversion numérique)
    A_chl_vals <- c(
      parse_num(input$chl_a1),
      parse_num(input$chl_a2),
      parse_num(input$chl_a3)
    )
    A_pit_vals <- c(
      parse_num(input$pita_a1),
      parse_num(input$pita_a2),
      parse_num(input$pita_a3)
    )

    fd_chl_meas <- parse_num(input$chl_meas_fd)
    fd_pit_meas <- parse_num(input$pita_meas_fd)

    # Moyennes
    A_chl_mean <- mean(A_chl_vals, na.rm = TRUE)
    A_pit_mean <- mean(A_pit_vals, na.rm = TRUE)

    if (all(is.na(A_chl_vals))) A_chl_mean <- NA_real_
    if (all(is.na(A_pit_vals))) A_pit_mean <- NA_real_

    # Concentration calculée (solution diluée)
    C_chl_measured <- if (!is.na(A_chl_mean) && !is.na(a_chl) && a_chl != 0) {
      (A_chl_mean - b_chl) / a_chl
    } else {
      NA_real_
    }

    C_pit_measured <- if (!is.na(A_pit_mean) && !is.na(a_pit) && a_pit != 0) {
      (A_pit_mean - b_pit) / a_pit
    } else {
      NA_real_
    }

    # Correction facteur de dilution de mesure
    C_chl_unknown <- if (!is.na(C_chl_measured) && !is.na(fd_chl_meas)) {
      C_chl_measured * fd_chl_meas
    } else {
      NA_real_
    }

    C_pit_unknown <- if (!is.na(C_pit_measured) && !is.na(fd_pit_meas)) {
      C_pit_measured * fd_pit_meas
    } else {
      NA_real_
    }

    # Facteur de dilution de préparation
    FD_chl <- if (!is.na(C_chl_unknown) && !is.na(ratio_chl) && ratio_chl != 0) {
      C_chl_unknown / ratio_chl
    } else {
      NA_real_
    }

    FD_pit <- if (!is.na(C_pit_unknown) && !is.na(ratio_pit) && ratio_pit != 0) {
      C_pit_unknown / ratio_pit
    } else {
      NA_real_
    }

    # Volume à pipeter
    Vpip_chl <- if (!is.na(FD_chl) && FD_chl != 0) {
      final_volume_ml / FD_chl
    } else {
      NA_real_
    }

    Vpip_pit <- if (!is.na(FD_pit) && FD_pit != 0) {
      final_volume_ml / FD_pit
    } else {
      NA_real_
    }

    data.frame(
      Espece = c("Chlorella", "Pita"),
      Abs1 = c(A_chl_vals[1], A_pit_vals[1]),
      Abs2 = c(A_chl_vals[2], A_pit_vals[2]),
      Abs3 = c(A_chl_vals[3], A_pit_vals[3]),
      Abs_moyenne = c(A_chl_mean, A_pit_mean),
      FD_mesure = c(fd_chl_meas, fd_pit_meas),
      Conc_calculee_diluee = c(C_chl_measured, C_pit_measured),
      Conc_inconnue = c(C_chl_unknown, C_pit_unknown),
      Ratio = c(ratio_chl, ratio_pit),
      Facteur_dilution = c(FD_chl, FD_pit),
      Volume_final_mL = c(final_volume_ml, final_volume_ml),
      Volume_a_pipetter_mL = c(Vpip_chl, Vpip_pit)
    )
  })

  output$unknown_table <- renderTable({
    out <- unknown_calc()

    out %>%
      mutate(
        Abs1 = ifelse(is.na(Abs1), "", sprintf("%.3f", Abs1)),
        Abs2 = ifelse(is.na(Abs2), "", sprintf("%.3f", Abs2)),
        Abs3 = ifelse(is.na(Abs3), "", sprintf("%.3f", Abs3)),
        Abs_moyenne = ifelse(is.na(Abs_moyenne), "", sprintf("%.3f", Abs_moyenne)),
        FD_mesure = ifelse(is.na(FD_mesure), "", sprintf("%.0f", FD_mesure)),
        Conc_calculee_diluee = ifelse(is.na(Conc_calculee_diluee), "", sprintf("%.0f", round(Conc_calculee_diluee))),
        Conc_inconnue = ifelse(is.na(Conc_inconnue), "", sprintf("%.0f", round(Conc_inconnue))),
        Ratio = ifelse(is.na(Ratio), "", sprintf("%.0f", round(Ratio))),
        Facteur_dilution = ifelse(is.na(Facteur_dilution), "", sprintf("%.2f", Facteur_dilution)),
        Volume_final_mL = ifelse(is.na(Volume_final_mL), "", sprintf("%.0f", round(Volume_final_mL))),
        Volume_a_pipetter_mL = ifelse(is.na(Volume_a_pipetter_mL), "", sprintf("%.3f", Volume_a_pipetter_mL))
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # =========================
  # Figure interactive
  # =========================
  output$plot_final <- renderGirafe({
    df <- df_plot()

    p <- ggplot(df, aes(x = C_mean_fd, y = A_mean)) +
      geom_point_interactive(
        aes(
          shape = date,
          color = species,
          tooltip = tooltip_txt,
          data_id = paste(species, date, FD, sep = "_")
        ),
        size = 5
      ) +
      geom_errorbar(
        aes(ymin = lower_y, ymax = upper_y, color = species),
        width = 0
      ) +
      geom_errorbar(
        aes(y = A_mean, xmin = lower_x, xmax = upper_x, color = species),
        orientation = "y",
        width = 0
      )

    if (input$reg_mode == "Poolée par espèce") {
      p <- p +
        geom_smooth(
          aes(color = species, group = species),
          method = "lm",
          se = FALSE
        ) +
        guides(linetype = "none")
    } else {
      p <- p +
        geom_smooth(
          aes(
            color = species,
            linetype = date,
            group = interaction(species, date)
          ),
          method = "lm",
          se = FALSE
        )
    }

    p <- p +
      labs(
        x = "Concentration cellulaire (cellules/mL)",
        y = "Absorbance moyenne",
        shape = "Date solution",
        color = "Espèce",
        linetype = "Date solution"
      ) +
      theme_classic() +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "bottom"
      ) +
      scale_color_manual(values = c("Chlorella" = "#04721F", "Pita" = "#9AFF1F")) +
      scale_x_continuous(labels = label_number(big.mark = "", decimal.mark = ","))

    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "stroke:black;stroke-width:2px;"),
        opts_tooltip(css = "background-color:white;color:black;padding:8px;border:1px solid #ccc;border-radius:4px;"),
        opts_zoom(min = 0.5, max = 10),
        opts_toolbar(saveaspng = TRUE)
      )
    )
  })
}

shinyApp(ui, server)
