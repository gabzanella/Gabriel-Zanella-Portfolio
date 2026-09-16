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
# Complex systems demo: bio-psycho-social feature network
# -----------------------------
# A small illustrative network spanning psychological, biological, and
# social features. Clicking a node marks it active (darker fill) and
# animates a marching-dash pulse along its direct connections, to make the
# idea of activation spreading through a connected system tangible.

cs_nodes <- data.frame(
  node = c(
    "Negative mood", "Rumination", "Suicidal ideation",
    "Sleep problems",
    "Social isolation", "Reduced social support"
  ),
  category = c(
    "Psychological", "Psychological", "Psychological",
    "Biological",
    "Social", "Social"
  ),
  x = c(0.50, 0.66, 0.50, 0.76, 0.24, 0.27),
  y = c(0.55, 0.85, 0.12, 0.38, 0.35, 0.80),
  stringsAsFactors = FALSE
)

cs_edges <- list(
  c("Negative mood", "Rumination"),
  c("Negative mood", "Suicidal ideation"),
  c("Negative mood", "Sleep problems"),
  c("Negative mood", "Social isolation"),
  c("Negative mood", "Reduced social support"),
  c("Rumination", "Suicidal ideation"),
  c("Rumination", "Social isolation"),
  c("Social isolation", "Reduced social support")
)

cs_category_colors <- list(
  Psychological = c(light = "#16c6d9", dark = "#087f8d"),
  Biological    = c(light = "#f0997b", dark = "#993c1d"),
  Social        = c(light = "#c9a6f0", dark = "#5b3a99")
)

# Finds the node nearest a click, within max_dist (data units, plot is 0-1
# on both axes); returns NULL if the click was too far from any node.
cs_nearest_node <- function(x, y, max_dist = 0.09) {
  d <- sqrt((cs_nodes$x - x)^2 + (cs_nodes$y - y)^2)
  idx <- which.min(d)
  if (d[idx] <= max_dist) cs_nodes$node[idx] else NULL
}

# Draws a "marching ants" dashed segment from (x0,y0) to (x1,y1): a fixed
# dash pattern whose offset shifts with `phase`, so repeated redraws at
# increasing phase read as movement along the line.
cs_draw_marching <- function(x0, y0, x1, y1, phase, color) {
  dash_frac <- 0.10
  period_frac <- 0.22
  offset <- (phase * 0.02) %% period_frac
  starts <- seq(-period_frac, 1, by = period_frac) + offset

  for (s in starts) {
    t0 <- max(s, 0)
    t1 <- min(s + dash_frac, 1)
    if (t1 > t0) {
      segments(
        x0 + t0 * (x1 - x0), y0 + t0 * (y1 - y0),
        x0 + t1 * (x1 - x0), y0 + t1 * (y1 - y0),
        col = color, lwd = 2.6, lend = 1
      )
    }
  }
}

# -----------------------------
# Harmful attractor states demo: vulnerable vs. resilient under force
# -----------------------------
# Two systems, each with two stable states (current / alternative) connected
# by a bowed, card-like boundary (cf. the playing-card analogy for
# vulnerable vs. resilient mental health states). The same applied force
# flips the vulnerable system to its alternative state while the resilient
# one barely bends, since it needs considerably more force to move.
vr_thresholds <- c(vulnerable = 1.4, resilient = 4.0)

# -----------------------------
# Work & Study timeline (About page)
# -----------------------------

work_study_timeline <- list(
  list(side = "left",  year = "2016–2018",
       title = "Bachelor of Psychology (started)",
       description = "PUCRS, Brazil. Transferred to VU Amsterdam."),
  list(side = "right", year = "2017–2019",
       title = "Undergraduate Research Fellow",
       description = "PUCRS/CNPq, Brazil.<br>Day-to-day research operations, including data collection, data processing and writing manuscripts.<br>Supervisor: Prof Dr. Angelo Brandelli Costa.",
       offset = 20),
  list(side = "left",  year = "2018–2021",
       title = "BSc in Psychology",
       description = "VU Amsterdam.<br>Cum Laude & Honours degree. GPA: 8.9.",
       offset = 20),
  list(side = "right", year = "2021",
       title = "Bachelor Student Award",
       description = "Faculty of Behavioural and Movement Sciences, VU Amsterdam."),
  list(side = "right", year = "2021",
       title = "Bright Minds Fellowship",
       description = "Utrecht University. Merit-based full scholarship awarded for high performance.",
       offset = 5),
  list(side = "left",  year = "2021–2022",
       title = "MSc in Clinical Psychology",
       description = "Utrecht University.<br>Cum Laude degree. GPA: 8.6.",
       offset = 30),
  list(side = "right", year = "2021-2022",
       title = "Research Internship: Interoception and Depression",
       description = "Utrecht University.<br>Co-designed and conducted a pilot experimental study in interoceptive awareness and mental health problems.<br>Supervisor: Prof Dr. Lotte Gerritsen. Grade: 8.5."),
  list(side = "left",  year = "2022",
       title = "Master's Thesis: Network Analysis of Depression, Anxiety and Family Functioning",
       description = "Research project examining network relations between family factors and individual symptoms of depression and anxiety.<br>Supervisor: Prof Dr Lynn Boschloo. Grade: 9.0.",
       offset = 5),
  list(side = "right", year = "2022–2025",
       title = "Junior Lecturer — Psychology",
       description = "VU Amsterdam. Docent 4 in the courses:<br>Research Methods 1<br>Statistics 1<br>Measurement Theory and Assessment 1 & 2.<br><b>Head tutor:</b><br>Big Data in Psychology<br>Developmental Psychopathology.<br></br><b>Blended Learning Team (Coordinator)</b><br>Project leader in course/tutorials redesign across the Bachelor of Psychology program.",
       offset = 20),
  list(side = "right", year = "2024–2026",
       title = "Statistics Consultant (Freelancer)",
       description = "Independent statistical consultant advising researchers and students in Psychology/Psychiatry with data analyses, interpretation and manuscripts."),
  list(side = "left",  year = "2025",
       title = "University Teaching Qualification (UTQ/BKO)",
       description = "",
       offset = 120),
  list(side = "left",  year = "2025",
       title = "Acceptance and Commitment Therapy for Depression and Anxiety",
       description = "Contextual Consulting (UK). Clinical training."),
  list(side = "left",  year = "2026",
       title = "Clinical Psychology Internship",
       description = "Overcome, UK.<br>Supervised practice delivering one-to-one psychological coaching sessions to international clients with evidence-based techniques (CBT & ACT).")
)

# Timeline render
timeline_box <- function(item) {
  extra_top <- if (!is.null(item$offset)) item$offset else 0

  div(class = paste("timeline-box", item$side),
      style = if (extra_top != 0) paste0("margin-top:", extra_top, "px;"),
      div(class = "timeline-year-label", item$year),
      div(class = "timeline-box-title", item$title),
      if (nzchar(item$description)) div(class = "timeline-box-desc", HTML(item$description))
  )
}

# Renders the full timeline as two independently-stacking columns (left =
# education, right = work/achievements) so each side packs tightly on its
# own instead of sharing row heights with the other side.
render_timeline <- function(items) {
  left_items  <- Filter(function(x) x$side == "left", items)
  right_items <- Filter(function(x) x$side == "right", items)

  div(class = "timeline-container",
      div(class = "timeline-columns-header",
          div(class = "timeline-header-left", "EDUCATION & TRAINING"),
          div(class = "timeline-header-right", "WORK & ACHIEVEMENTS")
      ),
      div(class = "timeline-columns",
          div(class = "timeline-col", lapply(left_items, timeline_box)),
          div(class = "timeline-col", lapply(right_items, timeline_box))
      )
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
      h1 { font-size: 32px; line-height: 1.15; margin: 0 0 18px; }
      h2 { font-size: 32px; margin: 0 0 14px; }
      h3 { font-size: 18px; margin: 0 0 8px; }

      .lead {
        color: #53666e;
        font-size: 17px;
        line-height: 1.55;
        max-width: 800px;
      }

      .letter-body {
        color: var(--ink);
        font-size: 15px;
        line-height: 1.75;
        max-width: 760px;
        margin-bottom: 16px;
      }

      .letter-signature {
        font-weight: 650;
        margin-top: 4px;
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

      .intro-photo {
        width: 100%;
        height: 340px;
        object-fit: cover;
        object-position: top;
        border-radius: 16px;
        display: block;
      }

      .card-pastel-1 { background: #e4f2f7; }
      .card-pastel-2 { background: #fbeadd; }
      .card-pastel-3 { background: #ece5f8; }
      .card-pastel-4 { background: #eaf5df; }
      .card-pastel-5 { background: #fbe4ea; }

      .card-icon { font-size: 22px; color: #087f8d; margin-bottom: 8px; display: block; }

      .card-text { font-size: 15px; color: #42545c; line-height: 1.6; }

      .social-links {
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
        margin-top: 6px;
      }

      .social-link {
        display: inline-flex;
        align-items: center;
        gap: 7px;
        background: #ffffff;
        border: 1px solid var(--line);
        border-radius: 999px;
        padding: 8px 16px;
        font-size: 12px;
        font-weight: 650;
        color: #42545c;
        text-decoration: none;
        transition: background 0.12s ease, color 0.12s ease, border-color 0.12s ease;
      }

      .social-link:hover {
        background: var(--cyan-soft);
        border-color: var(--cyan-dark);
        color: var(--cyan-dark);
        text-decoration: none;
      }

      .social-link i { font-size: 14px; }

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

      /* ---- Education & Training timeline ---- */
      .timeline-container {
        padding: 4px 0 8px;
      }

      .timeline-columns-header {
        display: grid;
        grid-template-columns: 1fr 1fr;
        column-gap: 24px;
        margin-bottom: 12px;
      }

      .timeline-header-left,
      .timeline-header-right {
        font-size: 10px;
        font-weight: 800;
        letter-spacing: 0.12em;
        color: var(--muted);
      }

      /* Each column stacks its own items independently, so the page height
         tracks the taller side instead of the sum of every entry. A central
         spine still runs the full height, with each box growing a short
         connector + dot out to it at its own vertical position. */
      .timeline-columns {
        position: relative;
        display: grid;
        grid-template-columns: 1fr 1fr;
        column-gap: 24px;
        align-items: start;
      }

      .timeline-columns::before {
        content: '';
        position: absolute;
        left: 50%;
        top: 0;
        bottom: 0;
        width: 2px;
        background: var(--line);
        transform: translateX(-50%);
        z-index: 0;
      }

      .timeline-col {
        display: flex;
        flex-direction: column;
      }

      .timeline-box {
        position: relative;
        z-index: 1;
        background: var(--panel);
        border: 1px solid var(--line);
        border-radius: 12px;
        padding: 14px 16px;
        margin-bottom: 14px;
      }

      .timeline-box.left {
        border-left: 3px solid var(--cyan-dark);
      }

      .timeline-box.right {
        border-left: 3px solid var(--ink);
      }

      /* Connector line from the box out to the central spine. */
      .timeline-box::before {
        content: '';
        position: absolute;
        top: 23px;
        width: 12px;
        height: 2px;
        background: var(--line);
      }

      .timeline-box.left::before { right: -12px; }
      .timeline-box.right::before { left: -12px; }

      /* Dot marking the connector's junction with the spine. */
      .timeline-box::after {
        content: '';
        position: absolute;
        top: 19px;
        width: 10px;
        height: 10px;
        border-radius: 50%;
        background: var(--cyan-dark);
        border: 2px solid #ffffff;
        box-shadow: 0 0 0 1px var(--cyan-dark);
        box-sizing: border-box;
      }

      .timeline-box.left::after { right: -17px; }
      .timeline-box.right::after { left: -17px; }

      .timeline-year-label {
        font-size: 10px;
        font-weight: 700;
        color: var(--cyan-dark);
        white-space: nowrap;
        margin-bottom: 6px;
      }

      .timeline-box-title {
        font-size: 13px;
        font-weight: 750;
        color: var(--ink);
        margin-bottom: 4px;
      }

      .timeline-box-desc {
        font-size: 12px;
        color: #53666e;
        line-height: 1.5;
      }

      @media (max-width: 900px) {
        .sidebar { width: 190px; }
        .main { margin-left: 190px; padding: 30px; }
        h1 { font-size: 26px; }
        .timeline-columns-header,
        .timeline-columns { grid-template-columns: 1fr; }
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
          h1("PhD: Investigating mental disorders as harmful stable states"),
          div(style = "margin-top:22px;",
              fluidRow(
                style = "display:flex; align-items:center; flex-wrap:wrap;",
                column(
                  width = 7,
                  p(class = "lead", style = "margin-bottom:10px;", "Hello!"),
                  p(class = "lead",
                    "My name is Gabriel Zanella. I am a quantitative researcher and teacher with expertise in clinical psychology and statistical modelling. Skilled in R-based data analysis, network models, university teaching and interdisciplinary mental health research. Interested in computational psychopathology and dynamic systems approach to mental health problems."
                  )
                ),
                column(
                  width = 5,
                  img(src = "profile.png", class = "intro-photo", alt = "Gabriel Zanella")
                )
              ),
              div(class = "social-links", style = "margin-top:8px;",
                  span(class = "social-link", icon("envelope"), "gabriel.i.zanella@gmail.com"),
                  tags$a(href = "https://orcid.org/0009-0002-2499-6159", class = "social-link",
                         target = "_blank", icon("orcid"), "ORCID"),
                  tags$a(href = "https://github.com/gabzanella", class = "social-link",
                         target = "_blank", icon("github"), "GitHub"),
                  tags$a(href = "https://linkedin.com/in/zanellagabriel/", class = "social-link",
                         target = "_blank", icon("linkedin"), "LinkedIn")
              ),
              fluidRow(
                style = "margin-top:18px;",
                column(
                  width = 4,
                  div(class = "panel-card card-pastel-1",
                      h3("Career & goals"),
                      p(class = "card-text", "My goal in research is to advance the understanding, modelling and treatment of mental health problems, with a focus on mood disorders.")
                  )
                ),
                column(
                  width = 4,
                  div(class = "panel-card card-pastel-2",
                      h3("Profile"),
                      p(class = "card-text", "I combine analytical skills, statistical modelling, programming and clinical knowledge to contribute to high-quality scientific output.")
                  )
                ),
                column(
                  width = 4,
                  div(class = "panel-card card-pastel-3",
                      h3("Topics of interest"),
                      p(class = "card-text", "Computational psychopathology, transdiagnostic approach, network theory, translational research, mood disorders.")
                  )
                )
              )
          )
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
              "Education & Training",
              div(style = "margin-top:22px;",
                  render_timeline(work_study_timeline)
              )
            ),
            tabPanel(
              "Previous research",
              div(class = "panel-card", style = "margin-top:22px;",
                  p(class = "placeholder-note", "Content to be added.")
              )
            ),
            tabPanel(
              "PhD motivation",
              div(class = "panel-card", style = "margin-top:22px;",
                  p(class = "letter-body", "Dear Prof Dr. Eiko Fried,"),
                  p(class = "letter-body",
                    "Upon learning about the PhD position at your department, I was immediately excited by the opportunity to investigate mental disorders from a network theory perspective within your research group. The project strongly aligns with my research interests at the intersection of clinical psychology, computational psychopathology and dynamical systems theory. I am particularly enthusiastic about investigating how symptoms and contextual factors interact, evolve and become self-maintaining over time in chronic presentations of common mental health problems, such as depressive and anxiety states."
                  ),
                  p(class = "letter-body", HTML(
                    "Two of your publications particularly motivated me to apply, namely <em>Studying Mental Health Problems as Systems, Not Syndromes</em> (2022) and <em>Mental Disorders as Networks of Problems: a Review of Recent Insights</em> (2017). Your conceptualisation of mental states as complex within-person processes resonates strongly with my perspective in psychopathology, and it reflects the pressing need for personalised psychological interventions. I would welcome the opportunity to apply methodological innovation and intensive longitudinal patient data to better understand how these individual-level systems emerge, persist, and transition across time. I’m keen to join efforts towards a theoretical framework that accommodates complexity and allows an ecological approach to mental health."
                  )),
                  p(class = "letter-body",
                    "Alongside the strong motivation for the research topic, my background combines statistical, analytical and interpersonal skills that would allow me to produce high-quality research output for the project. As an Undergraduate Research Assistant, I learned how to contribute to every stage of the research process, from data collection and management, to ethical approval and manuscript writing. During two years of participation, I co-authored 3 journal articles and one book chapter in interdisciplinary projects in health care, psychology and social factors. During my Master’s program (Clinical Psychology, Utrecht University, cum laude), I was first introduced to network theories while completing my thesis under the supervision of Dr. Lynn Boschloo. This marked a turning point in my academic development, as I became increasingly interested in network models as a powerful transdiagnostic framework of mental health. My thesis project examined the relationships between family functioning dimensions and symptoms of depression and anxiety, applying network analysis to data from a cohort of offspring of parents who received psychological treatment to visualise pathways of interconnections and patterns of symptoms. I performed data preparation and analyses including linear and mixed graphical models, estimating a network of symptoms and family factors with the mgm and qgraph packages in R. While this was a cross-sectional project, it also sparked my curiosity in extending these methods to longitudinal and within-person modelling. Examples of my research output, code, and graphs are shared on my GitHub page, reflecting reproducible and open science standards."
                  ),
                  p(class = "letter-body",
                    "My recent professional experiences have further strengthened my interest in dynamic theories and collaborative efforts in research. In my clinical internship, I directly observed the substantial heterogeneity of symptom presentations among individuals with similar diagnoses, reinforcing my interest in personalised approaches to treatment. Concurrently, working as a freelance statistics consultant strengthened my ability to independently design, conduct, and communicate statistical analyses; while also showing me how valuable it is to collaborate with other professionals to achieve impactful interdisciplinary action in mental health. Together, these experiences further motivated me to apply to this position and contribute to the NSMD consortium’s mission of advancing personalised network-informed interventions and data-driven approaches. I would be thrilled to be part of a group of researchers who meaningfully advance clinical science as your next PhD student."
                  ),
                  p(class = "letter-body", "Thank you for your consideration!"),
                  p(class = "letter-body letter-signature", "Gabriel Zanella")
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
              "Network approach",
              div(style = "margin-top:22px;",
                  fluidRow(
                    column(
                      width = 8,
                      div(class = "panel-card",
                          img(
                            src = "network-approach.png",
                            style = "width:100%; border-radius:10px; display:block;",
                            alt = "Empirical network of 120 psychiatric symptoms"
                          ),
                          div(class = "small-muted", style = "margin-top:14px; line-height:1.6;",
                              strong("Figure. "), "Empirical network of 120 psychiatric symptoms.",
                              br(),
                              "Boschloo, L., van Borkulo, C. D., Rhemtulla, M., Keyes, K. M., Borsboom, D., & Schoevers, R. A. (2015). ",
                              HTML("<em>The Network Structure of Symptoms of the Diagnostic and Statistical Manual of Mental Disorders</em>."),
                              " PLOS ONE. Published September 14, 2015.",
                              br(),
                              tags$a(
                                href = "https://doi.org/10.1371/journal.pone.0137621.g001",
                                target = "_blank",
                                "doi.org/10.1371/journal.pone.0137621.g001"
                              )
                          )
                      )
                    ),
                    column(
                      width = 4,
                      div(class = "panel-card card-pastel-1",
                          h3("Network approach"),
                          p(class = "card-text", "The network approach, in contrast, assumes that psychopathology results from the causal interplay between psychiatric symptoms and focuses specifically on these symptoms and their complex associations. (Boschloo et al., 2015)")
                      ),
                      div(class = "panel-card card-pastel-2",
                          p(class = "placeholder-note", "Content to be added.")
                      )
                    )
                  )
              )
            ),
            tabPanel(
              "Principles",
              div(style = "margin-top:22px;",
                  p(class = "lead", "The five principles of network theory, according to Borsboom (2017)."),
                  div(class = "small-muted", style = "margin-bottom:18px; line-height:1.6;",
                      strong("Source. "),
                      "Borsboom, D. (2017). ",
                      HTML("<em>A network theory of mental disorders</em>."),
                      " World Psychiatry, 16(1), 5–13.",
                      br(),
                      tags$a(
                        href = "https://doi.org/10.1002/wps.20375",
                        target = "_blank",
                        "doi.org/10.1002/wps.20375"
                      )
                  ),
                  div(style = "display:grid; grid-template-columns:1fr 1fr; gap:18px; align-items:start;",
                      div(class = "panel-card card-pastel-1",
                          icon("circle-nodes", class = "card-icon"),
                          h3("Principle 1. Complexity"),
                          p(class = "card-text", "Mental disorders are best characterized in terms of the interaction between different components in a psychopathology network.")
                      ),
                      div(class = "panel-card card-pastel-2",
                          icon("clipboard-list", class = "card-icon"),
                          h3("Principle 2. Symptom-component correspondence"),
                          p(class = "card-text", "The components in the psychopathology network correspond to the problems that have been codified as symptoms in the past century and appear as such in current diagnostic manuals.")
                      ),
                      div(class = "panel-card card-pastel-3",
                          icon("link", class = "card-icon"),
                          h3("Principle 3. Direct causal connections"),
                          p(class = "card-text", "The network structure is generated by a pattern of direct causal connections between symptoms.")
                      ),
                      div(class = "panel-card card-pastel-4",
                          icon("sitemap", class = "card-icon"),
                          h3("Principle 4. Mental disorders follow network structure"),
                          p(class = "card-text", "The psychopathology network has a non-trivial topology, in which certain symptoms are more tightly connected than others. These symptom groupings give rise to the phenomenological manifestation of mental disorders as groups of symptoms that often arise together.")
                      ),
                      div(class = "panel-card card-pastel-5",
                          icon("arrows-rotate", class = "card-icon"),
                          h3("Principle 5. Hysteresis"),
                          p(class = "card-text", "Mental disorders arise due to the presence of hysteresis in strongly connected symptom networks, which implies that symptoms continue to activate each other, even after the triggering cause of the disorder has disappeared.")
                      )
                  )
              )
            ),
            tabPanel(
              "Complex systems",
              div(style = "margin-top:22px;",
                  p(class = "lead",
                    "Mental states emerge from interacting psychological, biological, and social features. Click any feature to toggle it active and watch activation spread along its direct connections; click it again to turn it off."
                  ),
                  div(class = "panel-card",
                      plotOutput("cs_network_plot", height = "560px", click = "cs_plot_click")
                  ),
                  div(class = "disclaimer", style = "margin-top:10px;",
                      "Click a node to activate it (darker fill) and animate its direct connections; click the same node again to deactivate it. Illustrative connectivity, not fitted to data."
                  )
              )
            ),
            tabPanel(
              "Harmful attractor states",
              div(style = "margin-top:22px;",
                  p(class = "lead",
                    "A harmful attractor state is stable not because nothing pushes against it, but because the system resists being pushed elsewhere. Two systems can look identical at rest yet need very different amounts of force to reach an alternative stable state — that difference is what makes one vulnerable and the other resilient."
                  ),
                  fluidRow(
                    column(
                      width = 8,
                      div(class = "panel-card",
                          plotOutput("vr_force_plot", height = "420px")
                      )
                    ),
                    column(
                      width = 4,
                      div(class = "panel-card",
                          h3("Apply force"),
                          sliderInput(
                            "applied_force",
                            "Force magnitude",
                            min = 0, max = 5, value = 1.5, step = 0.1,
                            width = "100%"
                          ),
                          actionButton("apply_force", "Apply force", class = "btn-primary"),
                          actionButton("reset_force", "Reset"),
                          br(), br(),
                          verbatimTextOutput("vr_state_text"),
                          div(class = "disclaimer",
                              "Inspired by the playing-card analogy for vulnerable vs. resilient mental health states (Fried, 2022 lecture). The same push that flips a vulnerable system into an alternative state barely bends a resilient one — illustrating why some harmful states are far easier to escape than others."
                          )
                      )
                    )
                  )
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

  # ---- Complex systems tab: bio-psycho-social feature network ----

  cs_active <- reactiveVal(setNames(rep(FALSE, nrow(cs_nodes)), cs_nodes$node))
  cs_phase <- reactiveVal(0)

  observeEvent(input$cs_plot_click, {
    clicked <- cs_nearest_node(input$cs_plot_click$x, input$cs_plot_click$y)
    if (!is.null(clicked)) {
      current <- cs_active()
      current[clicked] <- !current[clicked]
      cs_active(current)
    }
  })

  # Keeps animating (redrawing at an advancing phase) for as long as at
  # least one node is toggled on; stops issuing new ticks once all are off.
  observe({
    if (any(cs_active())) {
      invalidateLater(120, session)
      isolate(cs_phase(cs_phase() + 1))
    }
  })

  output$cs_network_plot <- renderPlot({
    active <- cs_active()
    phase <- cs_phase()

    par(mar = c(0, 0, 0, 0), bg = "#ffffff")
    plot(
      cs_nodes$x, cs_nodes$y, type = "n", xlim = c(0, 1), ylim = c(0, 1),
      axes = FALSE, xlab = "", ylab = ""
    )

    for (e in cs_edges) {
      a <- cs_nodes[cs_nodes$node == e[1], ]
      b <- cs_nodes[cs_nodes$node == e[2], ]
      a_active <- active[[e[1]]]
      b_active <- active[[e[2]]]

      if (a_active || b_active) {
        edge_color <- if (a_active && b_active) {
          "#10212b"
        } else {
          endpoint <- if (a_active) e[1] else e[2]
          cs_category_colors[[cs_nodes$category[cs_nodes$node == endpoint]]][["dark"]]
        }
        cs_draw_marching(a$x, a$y, b$x, b$y, phase, edge_color)
      } else {
        lines(c(a$x, b$x), c(a$y, b$y), col = "#8a9aa1", lwd = 1.2, lty = 1)
      }
    }

    for (i in seq_len(nrow(cs_nodes))) {
      cat_colors <- cs_category_colors[[cs_nodes$category[i]]]
      fill <- if (active[[cs_nodes$node[i]]]) cat_colors[["dark"]] else cat_colors[["light"]]
      points(cs_nodes$x[i], cs_nodes$y[i], pch = 21, bg = fill, col = "#10212b", lwd = 1.8, cex = 12.0)
    }
    text(cs_nodes$x, cs_nodes$y - 0.17, labels = cs_nodes$node, cex = 0.82, font = 2, col = "#10212b")

    legend(
      "topleft",
      legend = names(cs_category_colors),
      pch = 21,
      pt.bg = vapply(cs_category_colors, function(c) c[["light"]], character(1)),
      col = "#10212b",
      pt.cex = 2.0,
      bty = "n",
      cex = 0.85
    )
  })

  # ---- Harmful attractor states tab: vulnerable vs. resilient under force ----

  force_state <- reactiveValues(
    applied = 0,
    flipped = c(vulnerable = FALSE, resilient = FALSE)
  )

  observeEvent(input$apply_force, {
    f <- input$applied_force
    force_state$applied <- f
    force_state$flipped["vulnerable"] <- f >= vr_thresholds["vulnerable"]
    force_state$flipped["resilient"] <- f >= vr_thresholds["resilient"]
  })

  observeEvent(input$reset_force, {
    force_state$applied <- 0
    force_state$flipped["vulnerable"] <- FALSE
    force_state$flipped["resilient"] <- FALSE
  })

  output$vr_force_plot <- renderPlot({
    f <- force_state$applied
    flipped <- force_state$flipped
    t <- seq(0, 1, length.out = 40)

    draw_system <- function(cx, color_dark, color_mid, threshold, is_flipped) {
      y_bottom <- 0.12
      y_top <- 0.88
      bow_ratio <- if (is_flipped) 0 else min(f / threshold, 1)
      bow <- 0.045 + bow_ratio * 0.12

      y_seq <- y_bottom + t * (y_top - y_bottom)
      lines(cx - 0.045 * sin(pi * t), y_seq, col = "#c9d3d6", lwd = 1.4, lty = 2)
      lines(cx + bow * sin(pi * t), y_seq, col = color_mid, lwd = 2.6, lty = 2)

      points(
        cx, y_bottom, pch = 21, cex = 3.6, lwd = 2, col = color_dark,
        bg = if (!is_flipped) color_dark else "#ffffff"
      )
      points(
        cx, y_top, pch = 21, cex = 3.6, lwd = 2, col = color_dark,
        bg = if (is_flipped) color_dark else "#ffffff"
      )
    }

    par(mar = c(1, 0, 3, 0), bg = "#ffffff")
    plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")

    draw_system(0.25, "#993c1d", "#d85a30", vr_thresholds["vulnerable"], flipped["vulnerable"])
    draw_system(0.75, "#087f8d", "#16c6d9", vr_thresholds["resilient"], flipped["resilient"])

    text(0.25, 0.99, "Vulnerable system", cex = 1.0, font = 2, col = "#993c1d")
    text(0.75, 0.99, "Resilient system", cex = 1.0, font = 2, col = "#087f8d")

    arrows(0.40, 0.5, 0.60, 0.5, length = 0.1, lwd = 2, col = "#10212b")
    text(0.5, 0.58, paste("Applied force:", round(f, 1)), cex = 0.78, col = "#10212b")
  })

  output$vr_state_text <- renderPrint({
    f <- force_state$applied
    flipped <- force_state$flipped

    cat("Applied force:", round(f, 2), "\n\n")
    cat(
      "Vulnerable system (needs ≥", vr_thresholds["vulnerable"], "): ",
      if (flipped["vulnerable"]) "moved to ALTERNATIVE state" else "still in CURRENT state",
      "\n", sep = ""
    )
    cat(
      "Resilient system  (needs ≥", vr_thresholds["resilient"], "): ",
      if (flipped["resilient"]) "moved to ALTERNATIVE state" else "still in CURRENT state",
      "\n", sep = ""
    )
  })

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