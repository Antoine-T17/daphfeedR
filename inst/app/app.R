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

    bslib::layout_column_wrap(
      width = 1,

      bslib::card(
        bslib::card_header("Calcul solution inconnue"),

        fluidRow(
          column(
            width = 6,
            tags$h4("Chlorella"),
            numericInput("chl_a1", "Absorbance 1", value = NA),
            numericInput("chl_a2", "Absorbance 2", value = NA),
            numericInput("chl_a3", "Absorbance 3", value = NA),
            numericInput("chl_meas_fd", "Facteur de dilution mesure (Chlorella)", value = 1, min = 1),
            numericInput("chl_ratio", "Ratio concentration (Chlorella)", value = 216000)
          ),
          column(
            width = 6,
            tags$h4("Pita"),
            numericInput("pita_a1", "Absorbance 1", value = NA),
            numericInput("pita_a2", "Absorbance 2", value = NA),
            numericInput("pita_a3", "Absorbance 3", value = NA),
            numericInput("pita_meas_fd", "Facteur de dilution mesure (Pita)", value = 1, min = 1),
            numericInput("pita_ratio", "Ratio concentration (Pita)", value = 144000)
          )
        ),

        fluidRow(
          column(
            width = 4,
            numericInput("final_volume_ml", "Volume final souhaité (mL)", value = 2000)
          )
        )
      ),

      bslib::card(
        bslib::card_header("Résultats"),
        tableOutput("unknown_table")
      )
    )
  )
)

server <- function(input, output, session) {

  # =========================
  # Données brutes Excel
  # =========================
  df_data <- reactive({
    req(input$file)

    df <- read_excel(input$file$datapath, sheet = input$sheet)

    names(df) <- c(
      "sample", "date", "species", "FD", "inv_FD", "A1", "A2", "A3",
      "A_mean", "A_var", "A_sd", "A_sem",
      "C_mean", "C_sem", "C_mean_fd", "C_sem_fd"
    )

    # --- Gestion robuste + tri chronologique des dates ---
    raw_date <- df$date

    date_chr <- if (inherits(raw_date, "Date") || inherits(raw_date, "POSIXt")) {
      format(as.Date(raw_date), "%d/%m/%Y")
    } else {
      as.character(raw_date)
    }

    date_try1 <- suppressWarnings(as.Date(date_chr, format = "%d/%m/%Y"))
    date_try2 <- suppressWarnings(as.Date(date_chr))
    date_order <- dplyr::coalesce(date_try1, date_try2)

    date_levels <- df %>%
      mutate(
        date_chr = date_chr,
        date_order = date_order
      ) %>%
      distinct(date_chr, date_order) %>%
      arrange(is.na(date_order), date_order, date_chr) %>%
      pull(date_chr)

    df <- df %>%
      mutate(
        date_chr = date_chr,
        date_order = date_order,
        date = factor(date_chr, levels = date_levels)
      )

    df %>%
      mutate(
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

    co_chl <- coef(mods$chlorella)
    a_chl <- unname(co_chl["C_mean_fd"])
    b_chl <- unname(co_chl["(Intercept)"])

    co_pit <- coef(mods$pita)
    a_pit <- unname(co_pit["C_mean_fd"])
    b_pit <- unname(co_pit["(Intercept)"])

    A_chl_vals <- c(input$chl_a1, input$chl_a2, input$chl_a3)
    A_pit_vals <- c(input$pita_a1, input$pita_a2, input$pita_a3)

    A_chl_mean <- mean(A_chl_vals, na.rm = TRUE)
    A_pit_mean <- mean(A_pit_vals, na.rm = TRUE)

    if (all(is.na(A_chl_vals))) A_chl_mean <- NA_real_
    if (all(is.na(A_pit_vals))) A_pit_mean <- NA_real_

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

    C_chl_unknown <- if (!is.na(C_chl_measured) && !is.na(input$chl_meas_fd)) {
      C_chl_measured * input$chl_meas_fd
    } else {
      NA_real_
    }

    C_pit_unknown <- if (!is.na(C_pit_measured) && !is.na(input$pita_meas_fd)) {
      C_pit_measured * input$pita_meas_fd
    } else {
      NA_real_
    }

    FD_chl <- if (!is.na(C_chl_unknown) && !is.na(input$chl_ratio) && input$chl_ratio != 0) {
      C_chl_unknown / input$chl_ratio
    } else {
      NA_real_
    }

    FD_pit <- if (!is.na(C_pit_unknown) && !is.na(input$pita_ratio) && input$pita_ratio != 0) {
      C_pit_unknown / input$pita_ratio
    } else {
      NA_real_
    }

    Vpip_chl <- if (!is.na(FD_chl) && !is.na(input$final_volume_ml) && FD_chl != 0) {
      input$final_volume_ml / FD_chl
    } else {
      NA_real_
    }

    Vpip_pit <- if (!is.na(FD_pit) && !is.na(input$final_volume_ml) && FD_pit != 0) {
      input$final_volume_ml / FD_pit
    } else {
      NA_real_
    }

    data.frame(
      Espece = c("Chlorella", "Pita"),
      Abs1 = c(input$chl_a1, input$pita_a1),
      Abs2 = c(input$chl_a2, input$pita_a2),
      Abs3 = c(input$chl_a3, input$pita_a3),
      Abs_moyenne = c(A_chl_mean, A_pit_mean),
      FD_mesure = c(input$chl_meas_fd, input$pita_meas_fd),
      Conc_calculee_diluee = c(C_chl_measured, C_pit_measured),
      Conc_inconnue = c(C_chl_unknown, C_pit_unknown),
      Ratio = c(input$chl_ratio, input$pita_ratio),
      Facteur_dilution = c(FD_chl, FD_pit),
      Volume_final_mL = c(input$final_volume_ml, input$final_volume_ml),
      Volume_a_pipetter_mL = c(Vpip_chl, Vpip_pit)
    )
  })

  output$unknown_table <- renderTable({
    out <- unknown_calc()

    out %>%
      mutate(
        Abs_moyenne = round(Abs_moyenne, 4),
        Conc_calculee_diluee = round(Conc_calculee_diluee, 0),
        Conc_inconnue = round(Conc_inconnue, 0),
        Facteur_dilution = round(Facteur_dilution, 2),
        Volume_final_mL = round(Volume_final_mL, 2),
        Volume_a_pipetter_mL = round(Volume_a_pipetter_mL, 3)
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
