# PhD app - Harmful stable States - Gabriel Zanella
# September/2026
# Restructured navigation: 7 top-level pages, some with internal tabs.

library(shiny)
library(shinydashboard)
library(ggplot2)


# -----------------------------
# Hypothetical dynamic model
# -----------------------------

node_names <- c("Stress", "Anhedonia", "Insomnia", "Fatigue", "Activity")

node_abbr <- c(
  Stress = "S",
  Anhedonia = "A",
  Insomnia = "I",
  Fatigue = "F",
  Activity = "C"
)

# Baseline state: low/moderate symptom activation.
baseline <- c(
  Stress = 0.8,
  Anhedonia = 0.8,
  Insomnia = 0.8,
  Fatigue = 0.8,
  Activity = 0.8
)

# Base directed temporal coupling matrix.
# Rows = receiving node at t+1
# Columns = predictor node at t
# These are illustrative, not empirical estimates.
base_A <- matrix(
  c(
    0.55, 0.00, 0.00, 0.00, 0.05,  # Stress
    0.20, 0.55, 0.00, 0.15, 0.00,  # Anhedonia
    0.15, 0.18, 0.55, 0.10, 0.00,  # Insomnia
    0.05, 0.12, 0.22, 0.55, 0.12,  # Fatigue
    0.00, 0.00, -0.15, -0.15, 0.60 # Activity
  ),
  nrow = 5,
  byrow = TRUE,
  dimnames = list(node_names, node_names)
)

# Recovery pulls the system toward baseline.
simulate_network <- function(
    perturb_target = "Anhedonia",
    perturbation = 1.0,
    coupling = 0.55,
    recovery = 0.30,
    days = 40,
    noise = 0.035
) {
  n <- length(node_names)
  
  A <- base_A
  diag(A) <- diag(base_A) * (0.70 + 0.30 * coupling)
  
  for (r in seq_len(n)) {
    for (c in seq_len(n)) {
      if (r != c) {
        A[r, c] <- base_A[r, c] * (coupling / 0.55)
      }
    }
  }
  
  # Small saturating nonlinearity so very high activation does not grow
  # without bound.
  squash <- function(x) {
    pmax(0, pmin(6, x))
  }
  
  X <- matrix(NA_real_, nrow = days, ncol = n,
              dimnames = list(NULL, node_names))
  X[1, ] <- baseline
  
  target_index <- match(perturb_target, node_names)
  X[1, target_index] <- X[1, target_index] + perturbation
  
  for (t in 2:days) {
    previous <- X[t - 1, ]
    
    propagated <- as.numeric(A %*% previous)
    next_state <- (1 - recovery) * propagated + recovery * baseline
    next_state <- squash(next_state)
    next_state <- next_state + rnorm(n, mean = 0, sd = noise)
    X[t, ] <- squash(next_state)
  }
  
  data.frame(
    day = seq_len(days),
    X,
    check.names = FALSE
  )
}

# Simple stability proxy based on the spectral radius of the temporal matrix.
system_stability <- function(coupling = 0.55, recovery = 0.30) {
  A <- base_A
  
  for (r in seq_along(node_names)) {
    for (c in seq_along(node_names)) {
      if (r != c) {
        A[r, c] <- base_A[r, c] * (coupling / 0.55)
      }
    }
  }
  
  A_eff <- (1 - recovery) * A
  eigenvalues <- eigen(A_eff, only.values = TRUE)$values
  max_modulus <- max(Mod(eigenvalues))
  
  if (max_modulus < 0.90) {
    state <- "Stable / recovery likely"
  } else if (max_modulus < 1.00) {
    state <- "Near criticality"
  } else {
    state <- "Potential amplification"
  }
  
  list(
    spectral_radius = max_modulus,
    state = state
  )
}

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  tags$head(
    tags$title("Harmful attractor states"),
    tags$style(HTML("
      :root {
        --bg: #f4f8fa;
        --panel: #ffffff;
        --ink: #10212b;
        --muted: #667780;
        --line: #d9e4e8;
        --cyan: #16c6d9;
        --cyan-dark: #087f8d;
        --cyan-soft: #dcf8fb;
      }

      html, body {
        height: 100%;
        margin: 0;
        background: var(--bg);
        color: var(--ink);
        font-family: Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI',
                     Roboto, Helvetica, Arial, sans-serif;
      }

      .container-fluid { padding: 0; }

      .sidebar {
        position: fixed;
        top: 0;
        left: 0;
        bottom: 0;
        width: 250px;
        padding: 28px 18px;
        background: #edf4f7;
        border-right: 1px solid var(--line);
        overflow-y: auto;
      }

      .brand { margin-bottom: 26px; padding: 0 4px; }

      .brand-name {
        font-size: 18px;
        font-weight: 800;
        letter-spacing: 0.04em;
        color: var(--ink);
      }

      .brand-sub {
        font-size: 11px;
        color: var(--muted);
        margin-top: 5px;
        line-height: 1.35;
      }

      /* ---- Sidebar menu rendered as stacked 'menu boxes' ---- */
      .sidebar-menu, .sidebar-menu > li {
        list-style: none;
        margin: 0;
        padding: 0;
      }

      .sidebar-menu { display: flex; flex-direction: column; gap: 8px; }

      .sidebar-menu > li > a,
      .sidebar .nav-pills > li > a {
        display: block;
        background: #ffffff;
        border: 1px solid var(--line);
        border-left: 3px solid transparent;
        border-radius: 10px;
        padding: 12px 14px;
        color: #42545c;
        font-size: 13px;
        font-weight: 650;
        letter-spacing: 0.01em;
        transition: background 0.12s ease, border-color 0.12s ease, color 0.12s ease;
      }

      .sidebar-menu > li > a:hover,
      .sidebar .nav-pills > li > a:hover {
        background: #f2fafb;
        color: var(--cyan-dark);
      }

      .sidebar-menu > li.active > a,
      .sidebar-menu > li.active > a:hover,
      .sidebar-menu > li.active > a:focus,
      .sidebar .nav-pills > li.active > a,
      .sidebar .nav-pills > li.active > a:hover,
      .sidebar .nav-pills > li.active > a:focus {
        background: var(--cyan-soft);
        border-left: 3px solid var(--cyan-dark);
        color: var(--cyan-dark);
      }

      /* ---- In-page tabs (Tab A / Tab B) ---- */
      .page-tabs.nav-pills {
        display: flex;
        gap: 8px;
        margin-bottom: 22px;
        border-bottom: 1px solid var(--line);
        padding-bottom: 16px;
      }

      .page-tabs.nav-pills > li > a {
        background: #ffffff;
        border: 1px solid var(--line);
        border-radius: 999px;
        padding: 7px 16px;
        font-size: 12px;
        font-weight: 650;
        color: #53666e;
      }

      .page-tabs.nav-pills > li.active > a,
      .page-tabs.nav-pills > li.active > a:hover,
      .page-tabs.nav-pills > li.active > a:focus {
        background: var(--cyan-dark);
        border-color: var(--cyan-dark);
        color: #ffffff;
      }

      .sidebar-footer {
        position: absolute;
        left: 18px;
        right: 18px;
        bottom: 20px;
        border-top: 1px solid var(--line);
        padding-top: 14px;
        font-size: 10px;
        color: #788990;
      }

      .main {
        margin-left: 250px;
        min-height: 100vh;
        padding: 42px 52px 60px;
      }

      .eyebrow {
        color: var(--cyan-dark);
        font-size: 10px;
        font-weight: 800;
        letter-spacing: 0.14em;
        text-transform: uppercase;
        margin-bottom: 10px;
      }

      h1, h2, h3 { font-weight: 750; letter-spacing: -0.03em; }
      h1 { font-size: 44px; line-height: 1.05; margin: 0 0 18px; }
      h2 { font-size: 32px; margin: 0 0 14px; }
      h3 { font-size: 18px; margin: 0 0 8px; }

      .lead {
        color: #53666e;
        font-size: 17px;
        line-height: 1.55;
        max-width: 800px;
      }

      .panel-card {
        background: var(--panel);
        border: 1px solid var(--line);
        border-radius: 14px;
        padding: 24px;
        margin-bottom: 18px;
      }

      .small-muted { font-size: 12px; color: var(--muted); line-height: 1.5; }

      .placeholder-note {
        font-size: 12px;
        color: #8a9aa1;
        font-style: italic;
      }

      .disclaimer {
        font-size: 10px;
        color: #7b8c92;
        background: #f0f6f8;
        border: 1px solid var(--line);
        border-radius: 8px;
        padding: 10px 12px;
        line-height: 1.45;
      }

      .metric { border-left: 3px solid var(--cyan); padding-left: 12px; margin: 16px 0; }
      .metric-label { font-size: 10px; color: var(--muted); letter-spacing: 0.10em; text-transform: uppercase; }
      .metric-value { font-size: 23px; font-weight: 750; margin-top: 2px; }

      .btn-primary { background: var(--cyan-dark); border-color: var(--cyan-dark); border-radius: 8px; font-weight: 700; }
      .well { background: #f7fbfc; border: 1px solid var(--line); border-radius: 10px; box-shadow: none; }

      pre.code-block {
        background: #0f1b21;
        color: #dbeef1;
        border-radius: 10px;
        padding: 18px;
        font-size: 12px;
        max-height: 620px;
        overflow-y: auto;
      }

      @media (max-width: 900px) {
        .sidebar { width: 190px; }
        .main { margin-left: 190px; padding: 30px; }
        h1 { font-size: 34px; }
      }
    "))
  ),
  
  div(class = "sidebar",
      div(class = "brand",
          div(class = "brand-name", "GABRIEL ZANELLA"),
          div(class = "brand-sub", "MSc Clinical Psychology")
      ),
      
      sidebarMenu(
        id = "main_tabs",
        menuItem("Home", tabName = "home", icon = NULL),
        menuItem("About", tabName = "about", icon = NULL),
        menuItem("Theoretical Framework", tabName = "theory", icon = NULL),
        menuItem("Research design", tabName = "design", icon = NULL),
        menuItem("Network model", tabName = "network", icon = NULL),
        menuItem("Contribution", tabName = "contribution", icon = NULL),
        menuItem("References", tabName = "references", icon = NULL)
      ),
      
      div(class = "sidebar-footer", "R · Shiny · Dynamic modelling")
  ),
  
  div(class = "main",
      tabItems(
        
        # 1. Home
        tabItem(
          tabName = "home",
          div(class = "eyebrow", "PhD application"),
          h1("Investigating harmful stable states in network theory"),
          p(class = "placeholder-note", "Content to be added.")
        ),
        
        # 2. About
        tabItem(
          tabName = "about",
          div(class = "eyebrow", "About"),
          h2("About me"),
          tabsetPanel(
            id = "about_tabs",
            type = "pills",
            tabPanel(
              "Work & Study",
              div(class = "panel-card", style = "margin-top:22px;",
                  p(class = "placeholder-note", "Content to be added.")
              )
            ),
            tabPanel(
              "Previous research",
              div(class = "panel-card", style = "margin-top:22px;",
                  p(class = "placeholder-note", "Content to be added.")
              )
            )
          )
        ),
        
        # 3. Theoretical Framework
        tabItem(
          tabName = "theory",
          div(class = "eyebrow", "Theoretical framework"),
          h2("Theoretical framework"),
          tabsetPanel(
            id = "theory_tabs",
            type = "pills",
            tabPanel(
              "Complex systems",
              div(class = "panel-card", style = "margin-top:22px;",
                  p(class = "placeholder-note", "Content to be added.")
              )
            ),
            tabPanel(
              "Harmful attractor states",
              div(class = "panel-card", style = "margin-top:22px;",
                  p(class = "placeholder-note", "Content to be added.")
              )
            )
          )
        ),
        
        # 4. Research design
        tabItem(
          tabName = "design",
          div(class = "eyebrow", "Research design"),
          h2("Research design"),
          p(class = "placeholder-note", "Content to be added.")
        ),
        
        # 5. Network model
        tabItem(
          tabName = "network",
          div(class = "eyebrow", "Interactive model"),
          h2("Network model"),
          tabsetPanel(
            id = "network_tabs",
            type = "pills",
            
            tabPanel(
              "Simulation",
              div(style = "margin-top:22px;",
                  p(class = "lead",
                    "Live simulation generated by R. The parameters are hypothetical and are intended to illustrate the proposed analysis."
                  ),
                  fluidRow(
                    column(
                      width = 8,
                      div(class = "panel-card",
                          plotOutput("network_plot", height = "580px")
                      )
                    ),
                    column(
                      width = 4,
                      div(class = "panel-card",
                          h3("Model controls"),
                          selectInput(
                            "network_target",
                            "Perturbation target",
                            choices = node_names,
                            selected = "Anhedonia"
                          ),
                          sliderInput(
                            "network_perturbation",
                            "Perturbation size",
                            min = 0, max = 5, value = 1.5, step = 0.1
                          ),
                          sliderInput(
                            "network_coupling",
                            "Coupling strength",
                            min = 0.10, max = 1.00, value = 0.55, step = 0.05
                          ),
                          sliderInput(
                            "network_recovery",
                            "Recovery strength",
                            min = 0.05, max = 0.80, value = 0.30, step = 0.05
                          ),
                          actionButton("run_network", "Run simulation", class = "btn-primary"),
                          br(), br(),
                          verbatimTextOutput("network_state"),
                          div(class = "disclaimer",
                              "Illustrative simulation only. In the empirical study, dynamic parameters would be estimated from repeated longitudinal observations."
                          )
                      )
                    )
                  )
              )
            ),
            
            tabPanel(
              "Code",
              div(style = "margin-top:22px;",
                  div(class = "panel-card",
                      h3("Simulation function"),
                      p(class = "small-muted", "The R function driving the simulation shown in the Simulation tab."),
                      verbatimTextOutput("network_code")
                  )
              )
            )
          )
        ),
        
        # 6. Contribution
        tabItem(
          tabName = "contribution",
          div(class = "eyebrow", "Contribution"),
          h2("Contribution"),
          tabsetPanel(
            id = "contribution_tabs",
            type = "pills",
            tabPanel(
              "Psychopathology theory",
              div(class = "panel-card", style = "margin-top:22px;",
                  p(class = "placeholder-note", "Content to be added.")
              )
            ),
            tabPanel(
              "Clinical application",
              div(class = "panel-card", style = "margin-top:22px;",
                  p(class = "placeholder-note", "Content to be added.")
              )
            )
          )
        ),
        
        # 7. References
        tabItem(
          tabName = "references",
          div(class = "eyebrow", "References"),
          h2("References"),
          p(class = "placeholder-note", "Content to be added.")
        )
      )
  )
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output, session) {
  
  network_params <- eventReactive(input$run_network, {
    list(
      target = input$network_target,
      perturbation = input$network_perturbation,
      coupling = input$network_coupling,
      recovery = input$network_recovery
    )
  }, ignoreInit = FALSE)
  
  output$network_plot <- renderPlot({
    p <- network_params()
    
    coords <- data.frame(
      node = node_names,
      x = c(0.10, 0.35, 0.68, 0.88, 0.44),
      y = c(0.52, 0.78, 0.78, 0.45, 0.18)
    )
    
    par(mar = c(0, 0, 0, 0), bg = "#ffffff")
    plot(
      coords$x, coords$y,
      type = "n", xlim = c(0, 1), ylim = c(0, 1),
      axes = FALSE, xlab = "", ylab = ""
    )
    
    A <- base_A
    for (r in seq_along(node_names)) {
      for (c in seq_along(node_names)) {
        if (r != c && abs(A[r, c]) > 0.03) {
          from <- coords[coords$node == node_names[c], ]
          to   <- coords[coords$node == node_names[r], ]
          width <- 1 + 5 * min(abs(A[r, c]), 0.35) / 0.35
          col <- ifelse(A[r, c] > 0, "#16c6d9", "#8a9aa1")
          arrows(
            from$x, from$y, to$x, to$y,
            length = 0.08, angle = 22, lwd = width, col = col, code = 2
          )
        }
      }
    }
    
    target <- p$target
    target_row <- coords[coords$node == target, ]
    
    points(coords$x, coords$y, pch = 21, bg = "#ffffff", col = "#10212b", lwd = 2, cex = 3.6)
    points(target_row$x, target_row$y, pch = 21, bg = "#16c6d9", col = "#087f8d", lwd = 2, cex = 3.9)
    
    text(coords$x, coords$y - 0.075, labels = coords$node, col = "#10212b", cex = 1.05, font = 2)
    
    title(main = "Illustrative temporal symptom network", col.main = "#10212b", cex.main = 1.35, line = -1)
    
    text(target_row$x, target_row$y + 0.08,
         labels = paste0("PERTURB: ", toupper(target)),
         col = "#087f8d", cex = 0.80, font = 2)
    
    legend(
      "bottomleft",
      legend = c("Positive temporal effect", "Negative temporal effect", "Perturbed node"),
      lty = c(1, 1, NA), pch = c(NA, NA, 21),
      col = c("#16c6d9", "#8a9aa1", "#087f8d"),
      pt.bg = c(NA, NA, "#16c6d9"), pt.cex = 1.5,
      bty = "n", cex = 0.78, horiz = FALSE
    )
  })
  
  output$network_state <- renderPrint({
    p <- network_params()
    s <- system_stability(p$coupling, p$recovery)
    
    cat("SYSTEM STATE\n")
    cat(s$state, "\n\n")
    cat("Target:", p$target, "\n")
    cat("Perturbation:", round(p$perturbation, 2), "\n")
    cat("Coupling:", round(p$coupling, 2), "\n")
    cat("Recovery:", round(p$recovery, 2), "\n")
  })
  
  output$network_code <- renderPrint({
    cat(paste(deparse(simulate_network), collapse = "\n"))
  })
}

shinyApp(ui, server)