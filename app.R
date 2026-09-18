# PhD app - Harmful stable States - Gabriel Zanella
# September/2026
# Restructured navigation: 7 top-level pages, some with internal tabs.

library(shiny)
library(shinydashboard)
library(ggplot2)
library(mhsmm)
library(Isinglandr)
library(qgraph)


# -----------------------------
# HSMM symptom-state simulation (Data analysis > Simulation)
# -----------------------------
# Simulates one person's EMA time series under a 3-state Hidden Semi-Markov
# Model (Low / Moderate / High symptom severity) using the mhsmm package,
# then builds the two-panel presentation figure: a schematic of the state /
# duration / observation structure, and the segmented observed series with
# the hidden states shown as background shading.

hsmm_state_names   <- c("Low", "Moderate", "High")
hsmm_state_means   <- c(0.15, 0.45, 0.80)
hsmm_state_sds     <- c(0.08, 0.10, 0.10)
hsmm_sojourn_mean  <- c(40, 15, 10)
hsmm_target_length <- 200
hsmm_n_segments    <- 30
hsmm_seed          <- 42
hsmm_colors        <- c(Low = "#b7e4c7", Moderate = "#ffd9a0", High = "#f4a6a6")

hsmm_init <- c(1, 0, 0)

# Excludes self-transitions: duration is handled by the sojourn
# distribution, not the transition matrix.
hsmm_transition <- matrix(c(
  0,   0.9, 0.1,
  0.5, 0,   0.5,
  0.1, 0.9, 0
), nrow = 3, byrow = TRUE)

# Builds the HSMM, simulates one realization, and returns both the
# timepoint-level series and the run-length (contiguous same-state) table
# used by both panels.
hsmm_build_simulation <- function() {
  model <- hsmmspec(
    init = hsmm_init,
    transition = hsmm_transition,
    # mhsmm's rnorm.hsmm/dnorm.hsmm take sigma as variance (they call
    # sqrt(sigma) internally), so the SDs above are squared here.
    parms.emission = list(mu = hsmm_state_means, sigma = hsmm_state_sds^2),
    sojourn = list(type = "poisson", lambda = hsmm_sojourn_mean, shift = rep(1, 3)),
    dens.emission = dnorm.hsmm
  )

  # nsim here is a number of state SEGMENTS (embedded-chain draws), not
  # timepoints, so we over-simulate and truncate to the target length.
  sim <- simulate(model, nsim = hsmm_n_segments, seed = hsmm_seed, rand.emission = rnorm.hsmm)

  states <- sim$s[seq_len(hsmm_target_length)]
  scores <- pmin(pmax(sim$x[seq_len(hsmm_target_length)], 0), 1)
  time <- seq(as.POSIXct("2026-01-05 08:00:00", tz = "UTC"), by = "3 hours", length.out = hsmm_target_length)

  df <- data.frame(
    t = seq_len(hsmm_target_length),
    time = time,
    state = factor(hsmm_state_names[states], levels = hsmm_state_names),
    score = scores
  )

  runs <- rle(as.integer(states))
  run_end <- cumsum(runs$lengths)
  run_start <- run_end - runs$lengths + 1
  run_df <- data.frame(
    state = factor(hsmm_state_names[runs$values], levels = hsmm_state_names),
    start = run_start,
    end = run_end,
    start_time = time[run_start],
    end_time = time[run_end]
  )

  list(df = df, run_df = run_df)
}

# Panel 1: schematic of the state / duration / observation structure, drawn
# from the first `n_show` contiguous state segments (not the full series).
hsmm_schematic_plot <- function(run_df, n_show = 4) {
  seg <- run_df[seq_len(n_show), ]
  seg$idx <- seq_len(n_show)
  seg$duration <- seg$end - seg$start + 1
  seg$box_x <- seg$idx
  seg$obs_label <- paste0("x_", seg$start, ", ..., x_", seg$end)
  seg$state_label <- paste0("State ", match(as.character(seg$state), hsmm_state_names), " - ", seg$state)
  seg$dur_label <- paste0(seg$duration, " timepoints")

  box_w <- 0.8; box_h <- 0.5
  box_y <- 2; oval_y <- 0.6
  oval_rx <- 0.42; oval_ry <- 0.32

  ellipse_pts <- function(cx, cy, rx, ry, id, n = 60) {
    th <- seq(0, 2 * pi, length.out = n)
    data.frame(x = cx + rx * cos(th), y = cy + ry * sin(th), id = id)
  }
  ovals <- do.call(rbind, lapply(seq_len(n_show), function(i) {
    ellipse_pts(seg$box_x[i], oval_y, oval_rx, oval_ry, i)
  }))

  p <- ggplot() +
    geom_rect(
      data = seg,
      aes(xmin = box_x - box_w / 2, xmax = box_x + box_w / 2,
          ymin = box_y - box_h / 2, ymax = box_y + box_h / 2, fill = state),
      color = "#10212b", linewidth = 0.4
    ) +
    geom_text(data = seg, aes(x = box_x, y = box_y + 0.08, label = state_label), size = 3.2, fontface = "bold") +
    geom_text(data = seg, aes(x = box_x, y = box_y - 0.14, label = dur_label), size = 2.8) +
    geom_polygon(data = ovals, aes(x = x, y = y, group = id), fill = "white", color = "#10212b", linewidth = 0.4) +
    geom_text(data = seg, aes(x = box_x, y = oval_y, label = obs_label), size = 2.6) +
    geom_segment(
      data = seg,
      aes(x = box_x, xend = box_x, y = box_y - box_h / 2, yend = oval_y + oval_ry + 0.05),
      arrow = arrow(length = unit(0.12, "cm")), color = "#53666e"
    ) +
    scale_fill_manual(values = hsmm_colors, guide = "none") +
    xlim(0.3, n_show + 0.7) + ylim(0, 2.7) +
    theme_void()

  if (n_show > 1) {
    p <- p + geom_segment(
      data = seg[-n_show, ],
      aes(x = box_x + box_w / 2, xend = box_x + 1 - box_w / 2, y = box_y, yend = box_y),
      arrow = arrow(length = unit(0.15, "cm")), color = "#10212b"
    )
  }

  p
}

# Panel 2: the full observed series with background shading per hidden state.
hsmm_segmented_plot <- function(df, run_df) {
  ggplot() +
    geom_rect(
      data = run_df,
      aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf, fill = state),
      alpha = 0.55
    ) +
    geom_line(data = df, aes(x = time, y = score), color = "#10212b", linewidth = 0.6) +
    scale_fill_manual(values = hsmm_colors, name = "Hidden state") +
    scale_x_datetime(name = "Assessment time") +
    scale_y_continuous(name = "Symptom score", limits = c(0, 1)) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

hsmm_sim_data <- hsmm_build_simulation()

# -----------------------------
# Ising landscape simulation (Data analysis > Simulation 2.0)
# -----------------------------
# CONCEPTUAL ILLUSTRATION ONLY: a binary Ising model showing how symptom
# connectivity reshapes a system's stability landscape (low / moderate /
# high connectivity). This is a different, simpler model than the
# project's actual empirical pipeline (continuous EMA data with
# graphicalVAR / partial-correlation networks) -- used here purely to
# visualize the theoretical mechanism.
#
# The full script below is kept as a single string and eval()'d once, so
# the "Code 2.0" tab can display exactly the code that produced these
# panels (no risk of the displayed code drifting from what actually ran).
ising_script <- r"---(
library(Isinglandr)
library(qgraph)
library(ggplot2)

# ---- toy network: 8 depression-adjacent symptoms ----
Nvar <- 8
node_labels <- c("Sad mood", "Anhedonia", "Fatigue", "Sleep problems",
                  "Concentration", "Appetite", "Guilt", "Psychomotor")
fatigue_idx <- which(node_labels == "Fatigue")

m <- rep(-3, Nvar)                                    # thresholds (shared across conditions)
w_base <- matrix(0.1, Nvar, Nvar); diag(w_base) <- 0  # base low-connectivity weights

# Connectivity multipliers -- tuned empirically (not hardcoded blindly) by
# scanning the landscape's local minima across a range of multipliers:
# mult=1 gives a single well (low, resilient); bimodality (two competing
# wells) first appears around mult=8-9; by mult=18 the high-symptom well
# dominates almost completely (near-absorbing high state).
mult_low  <- 1    # low connectivity
mult_mod  <- 9    # moderate connectivity: genuinely bimodal landscape
mult_high <- 18   # high connectivity: single dominant high-symptom well

seed <- 1614
sim_length <- 300
# Simulation "temperature": tuned so the moderate condition shows visible
# switching between plateaus within sim_length steps; applied identically
# to all three conditions so only connectivity differs between them.
beta2_sim <- 0.45

w_low  <- w_base * mult_low
w_mod  <- w_base * mult_mod
w_high <- w_base * mult_high

# ---- stability landscapes (middle column) ----
result_low  <- make_2d_Isingland(m, w_low)
result_mod  <- make_2d_Isingland(m, w_mod)
result_high <- make_2d_Isingland(m, w_high)

# ---- simulated trajectories (right column) ----
set.seed(seed); sim_low  <- simulate_Isingland(result_low,  initial = 0, length = sim_length, beta2 = beta2_sim)
set.seed(seed); sim_mod  <- simulate_Isingland(result_mod,  initial = 0, length = sim_length, beta2 = beta2_sim)
set.seed(seed); sim_high <- simulate_Isingland(result_high, initial = 0, length = sim_length, beta2 = beta2_sim)

# ---- panel builders ----

# Left column: the symptom network itself (qgraph, not part of Isinglandr),
# with "Fatigue" highlighted and edge thickness on a common scale across
# all three conditions so the increase in connectivity is visible.
ising_network_plot <- function(w, title) {
  node_colors <- rep("#dcf8fb", Nvar)
  node_colors[fatigue_idx] <- "#ffd9a0"
  qgraph(w, labels = node_labels, layout = "circle",
         color = node_colors, edge.color = "#087f8d",
         label.cex = 0.9, label.scale = FALSE, vsize = 13, esize = 8,
         maximum = max(w_high), title = title, title.cex = 1.2)
}

# Right column: the simulated symptom-count trajectory. Built directly
# from sim$output rather than plot.sim_Isingland(), which returns an
# animated (gganimate) plot -- not appropriate for a static figure.
ising_timeseries_plot <- function(sim, title) {
  ggplot(sim$output, aes(x = time, y = n_active)) +
    geom_line(color = "#10212b", linewidth = 0.5) +
    scale_y_continuous(name = "Active symptoms", limits = c(0, Nvar)) +
    scale_x_continuous(name = "Simulated time step") +
    ggtitle(title) +
    theme_minimal(base_size = 11)
}
)---"

ising_env <- new.env()
eval(parse(text = ising_script), envir = ising_env)

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
       description = "Overcome, UK.<br>Supervised practice delivering one-to-one psychological coaching sessions to international clients with evidence-based techniques (CBT & ACT)."),
  list(side = "left",  year = "2026",
       title = "MicroMaster in Statistics and Data Science (started)",
       description = "MITx. Online program focused on probability, data analysis and machine learning with Python.")
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
# Deduction flowchart (Framework page)
# -----------------------------
# Two-column research-cycle diagram: a stage label on the left (e.g.
# "Theory"), its content on the right in the same row, and a down-arrow
# connecting consecutive labels in the left column only (the same paired
# left/right correspondence style used on the Research design page).

# Small centered label box for the left column.
flow_label <- function(label) {
  div(class = "panel-card",
      style = "display:flex; align-items:center; justify-content:center; text-align:center; min-height:64px;",
      div(class = "eyebrow", style = "margin:0;", label)
  )
}

# Content box for the right column. `items` is a list of
# list(title = "..." or NULL, text = "...").
flow_content <- function(items) {
  div(class = "panel-card",
      lapply(items, function(it) {
        div(style = "margin-bottom:10px;",
            if (!is.null(it$title)) tags$strong(paste0(it$title, ": ")),
            span(class = "card-text", HTML(it$text))
        )
      })
  )
}

# Down-arrow occupying only the left-column cell of a grid row.
flow_row_arrow <- function() {
  div(style = "display:flex; align-items:center; justify-content:center; padding:2px 0;",
      icon("arrow-down", style = "font-size:18px; color:#8a9aa1;")
  )
}

# -----------------------------
# Publication list (About > Previous research)
# -----------------------------
# One list, oldest to newest, with a coloured category badge on the left
# (Article / Book chapter / Poster) instead of separate section headers.
pub_category_colors <- list(
  Article      = c(bg = "#dcf8fb", fg = "#087f8d"),
  "Book chapter" = c(bg = "#ece5f8", fg = "#5b3a99"),
  Poster       = c(bg = "#fbeadd", fg = "#993c1d")
)

pub_entry <- function(category, html_text) {
  colors <- pub_category_colors[[category]]

  div(style = "display:flex; align-items:flex-start; gap:14px; margin-bottom:18px;",
      div(style = paste0(
            "flex-shrink:0; min-width:96px; text-align:center; padding:4px 10px; ",
            "border-radius:8px; font-size:11px; font-weight:700; letter-spacing:0.04em; ",
            "text-transform:uppercase; background:", colors[["bg"]], "; color:", colors[["fg"]], ";"
          ),
          category
      ),
      p(class = "reference-item", style = "margin-bottom:0; padding-left:0; text-indent:0;", HTML(html_text))
  )
}

# -----------------------------
# Time-series data pipeline stepper (Data analysis page)
# -----------------------------
# One numbered step in the vertical stepper: a circled number, bold title,
# one-sentence description, and a row of small tool-tag pills.
step_item <- function(number, title, desc, tool_tags) {
  div(class = "step-row",
      div(class = "step-circle", number),
      div(
        div(class = "step-title", title),
        p(class = "step-desc", desc),
        div(lapply(tool_tags, function(t) span(class = "tool-tag", t)))
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

      .reference-item {
        font-size: 14px;
        line-height: 1.65;
        color: var(--ink);
        padding-left: 28px;
        text-indent: -28px;
        margin-bottom: 16px;
      }

      .reference-item a { color: var(--cyan-dark); }

      .kicker-mono {
        font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;
        font-size: 11px;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        color: #087f8d;
        font-weight: 700;
        margin-bottom: 8px;
      }

      .stepper {
        position: relative;
        margin: 4px 0 20px;
      }

      .stepper::before {
        content: '';
        position: absolute;
        left: 19px;
        top: 6px;
        bottom: 6px;
        width: 2px;
        background: var(--line);
        z-index: 0;
      }

      .step-row {
        position: relative;
        display: flex;
        gap: 18px;
        margin-bottom: 26px;
        z-index: 1;
      }

      .step-circle {
        flex-shrink: 0;
        width: 40px;
        height: 40px;
        border-radius: 50%;
        background: #087f8d;
        color: #ffffff;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: 750;
        font-size: 13px;
      }

      .step-title { font-weight: 750; color: #10212b; margin-bottom: 4px; }
      .step-desc { font-size: 14px; line-height: 1.6; color: #53666e; margin-bottom: 10px; }

      .tool-tag {
        display: inline-block;
        background: #f1f5f7;
        border: 1px solid var(--line);
        border-radius: 999px;
        padding: 3px 10px;
        font-size: 11px;
        color: #53666e;
        margin-right: 6px;
        margin-bottom: 6px;
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
      .card-pastel-6 { background: #fdf2c4; }

      .feature-card-icon { font-size: 24px; color: #10212b; margin-bottom: 10px; display: block; }

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
        .framework-flow { grid-template-columns: 1fr !important; }
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
        menuItem("Framework", tabName = "theory", icon = NULL),
        menuItem("Research design", tabName = "design", icon = NULL),
        menuItem("Data analysis", tabName = "network", icon = NULL),
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
                  ),
                  div(class = "social-links", style = "margin-top:10px;",
                      span(class = "social-link", icon("envelope"), "gabriel.i.zanella@gmail.com"),
                      span(class = "social-link", icon("phone"), "+31 616307092"),
                      tags$a(href = "https://orcid.org/0009-0002-2499-6159", class = "social-link",
                             target = "_blank", icon("orcid"), "ORCID"),
                      tags$a(href = "https://github.com/gabzanella", class = "social-link",
                             target = "_blank", icon("github"), "GitHub"),
                      tags$a(href = "https://linkedin.com/in/zanellagabriel/", class = "social-link",
                             target = "_blank", icon("linkedin"), "LinkedIn")
                  )
                ),
                column(
                  width = 5,
                  img(src = "profile.png", class = "intro-photo", alt = "Gabriel Zanella")
                )
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
              "Master's Thesis",
              div(style = "margin-top:22px;",
                  p(class = "lead", style = "margin-bottom:20px;",
                    "Master's thesis project (Clinical Psychology, Utrecht University, 2022), supervised by Dr. Lynn Boschloo."
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      div(class = "panel-card card-pastel-1", style = "height:100%;",
                          h3("Introduction"),
                          p(class = "card-text", "The presence of parental depression and/or anxiety can disrupt family functioning – here defined in the dimensions of cohesion (the emotional connection between family members) and flexibility (family rules and negotiations). This study investigated the associations between family functioning dimensions and the presence of symptoms of depression and anxiety in the offspring of parents treated for depression/anxiety disorders.")
                      )
                    ),
                    column(
                      width = 6,
                      div(class = "panel-card card-pastel-2", style = "height:100%;",
                          h3("Methods"),
                          tags$ul(class = "card-text", style = "padding-left:18px; margin:0;",
                              tags$li(tags$strong("Design: "), "cross-sectional"),
                              tags$li(tags$strong("Sample: "), "483 offspring (13 to 25 years) of parents treated for depression/anxiety"),
                              tags$li(tags$strong("Analyses: "), "Multiple linear regression examined associations of cohesion and flexibility with the number of symptoms in offspring; logistic regressions were performed for each of the symptoms; a network was estimated, with a mixed graphical model of how each symptom of mental health disorder (nodes) connects directly or indirectly to each other and to the family functioning dimensions."),
                              tags$li(tags$strong("Code: "), actionButton("thesis_code_btn", "View code", class = "btn-primary", style = "padding:3px 12px; font-size:12px; margin-left:4px;"))
                          )
                      )
                    )
                  ),
                  fluidRow(
                    style = "margin-top:18px;",
                    column(
                      width = 12,
                      div(class = "panel-card", style = "padding:32px;",
                          h2(style = "margin-top:0;", "Results"),
                          img(
                            src = "thesis-network.png",
                            style = "max-width:700px; width:100%; display:block; margin:0 auto 16px auto; border-radius:10px;",
                            alt = "Estimated network of family functioning dimensions and offspring symptoms"
                          ),
                          p(class = "card-text", style = "max-width:760px; margin:0 auto;",
                            "In the network structure, cohesion showed two negative links to symptoms (anhedonia and suicidality) and one positive link (insomnia), whereas flexibility showed two positive links (guilt and insomnia). Bold lines indicate stronger connections."
                          )
                      )
                    )
                  ),
                  fluidRow(
                    style = "margin-top:18px;",
                    column(
                      width = 12,
                      div(class = "panel-card card-pastel-3",
                          h3("Discussion"),
                          p(class = "card-text", "High family cohesion has a protective role in offspring depression/anxiety, especially against anhedonia, sadness and worthlessness. Excessive family flexibility is detrimental to offspring, especially via guilt and sleeping problems that are further related to anxiety. Findings suggest that different family factors relate to distinct pathways of symptoms in the offspring and that family interventions should increase cohesion and reduce excessive flexibility.")
                      )
                    )
                  ),
                  div(class = "small-muted", style = "margin-top:14px;",
                      "URI: ",
                      tags$a(href = "https://studenttheses.uu.nl/handle/20.500.12932/41957", target = "_blank",
                             "studenttheses.uu.nl/handle/20.500.12932/41957")
                  )
              )
            ),
            tabPanel(
              "Previous research",
              div(class = "panel-card", style = "margin-top:22px;",
                  pub_entry("Book chapter", "Catelan, R., Bercht, A., Pase, P., Stucky, J., Chinazzo, I., Vezzosi, J. P., Azevedo, F., Brazil, A., Portalino, E., Ramos, M., Zanella, G., Lucas, A., & Costa, A. (2019). Sexual and gender diversity: Theoretical models from recent research in Brazil. In <em>Prejudice and social exclusion: Studies in psychology in Brazil</em> (1st ed., pp. 180–217). ISBN: 978-85-509-0501-3."),
                  pub_entry("Article", "Fontanari, A. M. V., Zanella, G., Feijo, M., Churchill, S., Lobato, M. I., & Costa, A. B. (2019). HIV-related care for transgender people: A systematic review of studies from around the world. <em>Social Science & Medicine, 230</em>, 280–294. <a href=\"https://doi.org/10.1016/j.socscimed.2019.03.016\" target=\"_blank\">https://doi.org/10.1016/j.socscimed.2019.03.016</a>"),
                  pub_entry("Article", "Catelan, R., Sbicigo, J., Azevedo, M., Vilanova, F., Silva, P., Zanella, G., Ramos, M., Costa, A. B., & Nardi, H. C. (2020). Anticipated stigma in HIV testing among Brazilian male soldiers. <em>Psychology & Sexuality, 13</em>(2), 1–26. <a href=\"https://doi.org/10.1080/19419899.2020.1773909\" target=\"_blank\">https://doi.org/10.1080/19419899.2020.1773909</a>"),
                  pub_entry("Article", "Pase, P., Schultz Aguida, L., Lucas, A., Zanella, G., Ignácio, G., Stock, S., Dotta, M., & Costa, A. B. (2021). Gender relations in healthcare work in a female prison facility. <em>Psychosocial Researches and Practices, 16</em>(3), 1–17. <a href=\"https://seer.ufsj.edu.br/revista_ppp/article/view/e3256\" target=\"_blank\">seer.ufsj.edu.br/revista_ppp/article/view/e3256</a>"),
                  pub_entry("Poster", "Monteiro, R., Bolzan, P., Comissoli, T., Zanella, G., & Ferrao, Y. (2025). Emotional response to suicidal patients and burnout syndrome: The impact on physicians’ mental health. <em>European Congress of Psychiatry 2025</em>, S342–S343. <a href=\"https://doi.org/10.1192/j.eurpsy.2025.727\" target=\"_blank\">doi.org/10.1192/j.eurpsy.2025.727</a>"),
                  pub_entry("Poster", "Monteiro, R., Bolzan, P., Comissoli, T., Zanella, G., & Ferrao, Y. (2025). Relations between physicians’ emotional response and stigma around suicide. <em>European Congress of Psychiatry 2025</em>, S225. <a href=\"https://doi.org/10.1192/j.eurpsy.2025.51\" target=\"_blank\">doi.org/10.1192/j.eurpsy.2025.51</a>")
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
          div(class = "eyebrow", "Framework"),
          h2("Framework"),
          tabsetPanel(
            id = "theory_tabs",
            type = "pills",
            tabPanel(
              "Deduction",
              div(style = "margin-top:22px;",
                  p(class = "lead", "The research cycle in which this PhD project is nested — from theory to empirical test, and back to implications for theory and practice."),
                  div(class = "framework-flow", style = "display:grid; grid-template-columns:200px 1fr; gap:12px 24px; align-items:stretch;",
                      flow_label("Theory"),
                      flow_content(list(
                        list(title = NULL, text = "<b>Central tenet:</b> Mental disorders arise from the causal interaction between symptoms in a network (Borsboom, 2017).")
                      )),
                      flow_row_arrow(), div(),
                      flow_label("Principles and concepts"),
                      flow_content(list(
                        list(title = "Complex systems", text = "Interactions between numerous biological, psychological, and social features. (Fried, 2022)."),
                        list(title = "Mental states", text = "Emergent properties arising out of interactions across complex systems. (Fried, 2022)."),
                        list(title = "Stable state", text = "In a dynamical system, a stable state (or “attractor” state) is a point in the system’s state space the system will tend to move toward and remain in over time.")
                      )),
                      flow_row_arrow(), div(),
                      flow_label("Predictions"),
                      div(class = "panel-card",
                          tags$ul(class = "card-text", style = "padding-left:18px; margin:0;",
                              tags$li("Complex systems can transition and settle into attractor states."),
                              tags$li("Perturbations can affect complex systems; more severe perturbations lead to transition into alternative states more easily."),
                              tags$li("Removing the perturbation does not return the system to prior state."),
                              tags$li("Positive feedback loops increase the magnitude of a perturbation to the system."),
                              tags$li("Negative feedback loops dampen the perturbation to the system."),
                              tags$li("Complex systems can be more vulnerable or resilient to alternative states."),
                              tags$li("The effect of interventions on systems depends on the history of the systems."),
                              tags$li("Interventions with greater impact on the system indicate larger changes in the system.")
                          )
                      ),
                      flow_row_arrow(), div(),
                      flow_label("Empirical data"),
                      flow_content(list(
                        list(title = "Cross-sectional observational data", text = "Individual differences networks."),
                        list(title = "Time-series longitudinal patient data", text = "Temporal and contemporaneous networks.")
                      )),
                      flow_row_arrow(), div(),
                      flow_label("Statistical analyses"),
                      div(class = "panel-card",
                          tags$strong("Network estimation:"),
                          tags$ul(class = "card-text", style = "padding-left:18px; margin:6px 0 0;",
                              tags$li("Gaussian graphical models"),
                              tags$li("Mixed graphical models"),
                              tags$li("Multilevel graphical vector autoregression")
                          )
                      ),
                      flow_row_arrow(), div(),
                      flow_label("Implications"),
                      flow_content(list(
                        list(title = "Theory", text = "Empirical evidence for or against specific dynamical predictions of network theory."),
                        list(title = "Practice", text = "Establishing evidence-based practice and refining the treatment protocol.")
                      ))
                  )
              )
            ),
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
          div(style = "margin-top:22px;",
              p(class = "lead", style = "margin-bottom:22px;",
                "Project: Empirical study on harmful stable states for testing the network theory of mental disorders."
              ),
              div(class = "panel-card", style = "margin-bottom:24px; border-color:#087f8d;",
                  div(style = "display:flex; align-items:center; gap:10px; margin-bottom:8px;",
                      icon("magnifying-glass", style = "font-size:20px; color:#087f8d;"),
                      h3(style = "margin:0;", "Research question")
                  ),
                  p(class = "card-text", style = "font-size:18px; line-height:1.6;", HTML(
                    "How do putative harmful stable states <strong style=\"color:#087f8d;\">emerge</strong>, <strong style=\"color:#c2410c;\">evolve</strong>, and <strong style=\"color:#5b3a99;\">respond</strong> to targeted intervention in mental-health networks?"
                  ))
              ),
              div(style = "display:flex; flex-direction:column; gap:18px;",
                  div(style = "display:grid; grid-template-columns: 1fr 50px 1fr; gap:8px;",
                      actionLink("rq_toggle1",
                          label = tagList(
                            h3("Aim 1 - Identifying"),
                            p(class = "card-text", "What are the recurring states in networks of mental health problems?")
                          ),
                          class = "panel-card card-pastel-1",
                          style = "display:block; text-decoration:none; color:inherit; cursor:pointer;"
                      ),
                      conditionalPanel(condition = "input.rq_toggle1 % 2 == 1",
                          div(style = "display:flex; align-items:center; justify-content:center; height:100%;",
                              icon("arrow-right", style = "font-size:22px; color:#10212b;")
                          )
                      ),
                      conditionalPanel(condition = "input.rq_toggle1 % 2 == 1",
                          div(class = "panel-card card-pastel-1",
                              tags$ul(class = "card-text", style = "padding-left:18px; margin:0;",
                                  tags$li("Literature review"),
                                  tags$li("Analyse the collected time-series dataset"),
                                  tags$li("A statistical model to identify recurrent states based on symptom fluctuations over time (EMA data)"),
                                  tags$li("Hidden Semi-Markov Model (HSMM) fit per person (to address symptom duration)"),
                                  tags$li("Network-comparison follow-up check: differences in configuration or severity?"),
                              )
                          )
                      )
                  ),
                  div(style = "display:grid; grid-template-columns: 1fr 50px 1fr; gap:8px;",
                      actionLink("rq_toggle2",
                          label = tagList(
                            h3("Aim 2 - Explaining"),
                            p(class = "card-text", "What are the dynamical properties of persistence and transition of states in such networks?")
                          ),
                          class = "panel-card card-pastel-2",
                          style = "display:block; text-decoration:none; color:inherit; cursor:pointer;"
                      ),
                      conditionalPanel(condition = "input.rq_toggle2 % 2 == 1",
                          div(style = "display:flex; align-items:center; justify-content:center; height:100%;",
                              icon("arrow-right", style = "font-size:22px; color:#10212b;")
                          )
                      ),
                      conditionalPanel(condition = "input.rq_toggle2 % 2 == 1",
                          div(class = "panel-card card-pastel-2",
                              tags$ul(class = "card-text", style = "padding-left:18px; margin:0;",
                                  tags$li("Track (in)stability and state transitions"),
                                  tags$li("Compute autocorrelations and variance in the symptom-level time series"),
                                  tags$li("Early-warning-signal analysis"),
                                  tags$li("Exploratory analyses: Check network connectivity")
                              )
                          )
                      )
                  ),
                  div(style = "display:grid; grid-template-columns: 1fr 50px 1fr; gap:8px;",
                      actionLink("rq_toggle3",
                          label = tagList(
                            h3("Aim 3 - Testing"),
                            p(class = "card-text", "How do such networks respond to element-targeted intervention?")
                          ),
                          class = "panel-card card-pastel-3",
                          style = "display:block; text-decoration:none; color:inherit; cursor:pointer;"
                      ),
                      conditionalPanel(condition = "input.rq_toggle3 % 2 == 1",
                          div(style = "display:flex; align-items:center; justify-content:center; height:100%;",
                              icon("arrow-right", style = "font-size:22px; color:#10212b;")
                          )
                      ),
                      conditionalPanel(condition = "input.rq_toggle3 % 2 == 1",
                          div(class = "panel-card card-pastel-3",
                              tags$ul(class = "card-text", style = "padding-left:18px; margin:0;",
                                  tags$li("State characteristics and transition patterns → predicting severity, trajectory, relapse"),
                                  tags$li("Validate networks vs. associated clinical outcomes"),
                                  tags$li("Single-person intervention design"),
                                  tags$li("Network Intervention Analysis: node-level vs. link-level targeting"),
                                  tags$li("Structural vs temporal changes (follow-up)")
                              )
                          )
                      )
                  )
              )
          )
        ),
        
        # 5. Network model
        tabItem(
          tabName = "network",
          div(class = "eyebrow", "Data analysis"),
          h2("Data analysis"),
          tabsetPanel(
            id = "network_tabs",
            type = "pills",

            tabPanel(
              "Time-series data",
              div(style = "margin-top:22px; max-width:820px;",
                  div(class = "stepper",
                      step_item("01", "Data collection",
                        "Access existing longitudinal / EMA datasets.",
                        c("OSF", "Data storage", "GDPR compliance")
                      ),
                      step_item("02", "Data cleaning & processing",
                        "Combine instruments and data points into one long-format file. Manage missing data.",
                        c("tidyverse", "codebook")
                      ),
                      step_item("03", "Operationalisation of symptoms",
                        "Item coding, individual-level time series for each individual symptom/node.",
                        c("psych", "qgraph", "corrplot", "scoring scripts")
                      ),
                      step_item("04", "Within-person/Idiographic model estimation",
                        "Fit a Hidden Semi-Markov Model to individual subjects, applying model fit indices (BIC) to identify recurring symptom states and their typical duration.",
                        c("mhsmm", "BIC comparison")
                      ),
                      step_item("05", "Validation of states",
                        "Pool timepoints by assigned state, estimate a separate symptom network for each, and test whether states reflect genuinely different symptom configurations rather than the same pattern at differing severity.",
                        c("graphicalVAR", "NetworkComparisonTest", "bootnet")
                      ),
                      step_item("06", "Dynamic characterization",
                        "Read persistence and switching probability from the HSMM's transition and duration structure; compute rolling-window autocorrelation and variance in the symptom time series as early-warning indicators of an approaching state change.",
                        c("mhsmm output", "earlywarnings", "rolling-window scripts")
                      ),
                      step_item("07", "Criterion validation & intervention testing",
                        "Use per-person state features (dwell time, number of transitions) to predict clinical outcomes (relapse, severity, functioning), then run a single-person Network Intervention Analysis comparing node-level vs. link-level targeting; store and document data and derived features for reuse.",
                        c("survival / lm", "mgm", "SQLite · OSF")
                      )
                  ),
                  div(class = "disclaimer",
                      tags$strong("Disorder-agnostic by design: "),
                      "because the pipeline works on the derived proportion-score rather than raw item content, depression, anxiety and other diagnostic datasets pass through the same seven stations — allowing cross-disorder comparison of attractor structure before any new data collection."
                  )
              )
            ),

            tabPanel(
              "Simulation",
              div(style = "margin-top:22px;",
                  p(class = "lead",
                    "One person's simulated EMA time series under a 3-state Hidden Semi-Markov Model (Low / Moderate / High symptom severity), used to demonstrate HSMM-based state segmentation."
                  ),
                  div(class = "panel-card",
                      h3("State / duration / observation structure"),
                      p(class = "small-muted", "The first four state segments from the simulated series, shown schematically."),
                      plotOutput("hsmm_schematic", height = "320px")
                  ),
                  div(class = "panel-card",
                      h3("Segmented time series"),
                      p(class = "small-muted", "The full simulated series, with background shading showing the true (simulated) hidden state at each timepoint."),
                      plotOutput("hsmm_segmented", height = "380px")
                  ),
                  div(class = "disclaimer",
                      "Illustrative simulation only — parameters (state means/SDs, sojourn durations, transition probabilities) are hypothetical, fixed with a random seed for reproducibility. In the empirical study, these would be estimated from real longitudinal EMA data."
                  )
              )
            ),

            tabPanel(
              "Code",
              div(style = "margin-top:22px;",
                  div(class = "panel-card",
                      h3("HSMM simulation & figure code"),
                      p(class = "small-muted", "The R code used to build the simulation and the two panels shown in the Simulation tab."),
                      verbatimTextOutput("hsmm_code")
                  )
              )
            ),

            tabPanel(
              "Simulation 2.0",
              div(style = "margin-top:22px;",
                  div(class = "disclaimer", style = "margin-bottom:18px;",
                      "Illustrative simulation (binary Ising model) of how symptom connectivity reshapes system stability — the empirical analysis instead uses continuous EMA data and partial-correlation networks."
                  ),
                  div(
                    style = "display:grid; grid-template-columns:120px repeat(3, 1fr); gap:10px; align-items:center;",
                    div(),
                    div(class = "eyebrow", style = "text-align:center;", "System network"),
                    div(class = "eyebrow", style = "text-align:center;", "Stability landscape"),
                    div(class = "eyebrow", style = "text-align:center;", "Time series"),

                    div(class = "eyebrow", "Low"),
                    div(class = "panel-card", plotOutput("ising_net_low", height = "230px")),
                    div(class = "panel-card", plotOutput("ising_land_low", height = "230px")),
                    div(class = "panel-card", plotOutput("ising_ts_low", height = "230px")),

                    div(class = "eyebrow", "Moderate"),
                    div(class = "panel-card", plotOutput("ising_net_mod", height = "230px")),
                    div(class = "panel-card", plotOutput("ising_land_mod", height = "230px")),
                    div(class = "panel-card", plotOutput("ising_ts_mod", height = "230px")),

                    div(class = "eyebrow", "High"),
                    div(class = "panel-card", plotOutput("ising_net_high", height = "230px")),
                    div(class = "panel-card", plotOutput("ising_land_high", height = "230px")),
                    div(class = "panel-card", plotOutput("ising_ts_high", height = "230px"))
                  )
              )
            ),

            tabPanel(
              "Code 2.0",
              div(style = "margin-top:22px;",
                  div(class = "panel-card",
                      h3("Ising landscape simulation code"),
                      p(class = "small-muted", "The full R script used to build the network, landscape, and time-series panels above — copy and run directly (requires Isinglandr, qgraph, ggplot2)."),
                      verbatimTextOutput("ising_code")
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
              "Forecast model",
              div(style = "margin-top:22px;",
                  p(class = "lead", style = "margin-bottom:22px;",
                    "How can we construct dynamical model of an individual's symptom network that allows us to simulate perturbations and forecast transitions between states?"
                  ),
                  div(style = "display:grid; grid-template-columns: 1fr 40px 1fr 40px 1fr 40px 1fr; gap:8px; align-items:stretch; margin-bottom:24px;",
                      div(class = "panel-card card-pastel-1", style = "display:flex; align-items:center; justify-content:center; text-align:center;",
                          h3(style = "margin:0;", "Describe the network")
                      ),
                      div(style = "display:flex; align-items:center; justify-content:center;",
                          icon("arrow-right", style = "font-size:20px; color:#10212b;")
                      ),
                      div(class = "panel-card card-pastel-2", style = "display:flex; align-items:center; justify-content:center; text-align:center;",
                          h3(style = "margin:0;", "Modelling dynamics")
                      ),
                      div(style = "display:flex; align-items:center; justify-content:center;",
                          icon("arrow-right", style = "font-size:20px; color:#10212b;")
                      ),
                      div(class = "panel-card card-pastel-3", style = "display:flex; align-items:center; justify-content:center; text-align:center;",
                          h3(style = "margin:0;", "Simulating trajectories")
                      ),
                      div(style = "display:flex; align-items:center; justify-content:center;",
                          icon("arrow-right", style = "font-size:20px; color:#10212b;")
                      ),
                      div(class = "panel-card card-pastel-4", style = "display:flex; align-items:center; justify-content:center; text-align:center;",
                          h3(style = "margin:0;", "Forecasting model")
                      )
                  ),
                  withMathJax(
                    div(class = "panel-card", style = "margin-bottom:18px;",
                        h3("Parameters"),
                        p("$$X_{t+1} = A X_t + B E_t + \\epsilon_t$$"),
                        p(class = "card-text", "where:"),
                        tags$ul(class = "card-text", style = "padding-left:18px; margin:0;",
                            tags$li("\\(X_t\\) = symptom state at time t"),
                            tags$li("\\(A\\) = network of symptom-to-symptom effects"),
                            tags$li("\\(E_t\\) = external events/stressors"),
                            tags$li("\\(B\\) = effects of those external events"),
                            tags$li("\\(\\epsilon_t\\) = unexplained/random variation")
                        )
                    )
                  ),
                  div(class = "panel-card", style = "margin-bottom:18px;",
                      h3("From prediction models to forecast models"),
                      p(style = "margin-bottom:4px;", tags$strong("Prediction:")),
                      p(class = "card-text", style = "font-style:italic; margin-bottom:14px;", "“The person will enter a depressive state.”"),
                      p(style = "margin-bottom:4px;", tags$strong("Forecasting:")),
                      p(class = "card-text", style = "font-style:italic; margin-bottom:0;", "“Given the person’s current state, estimated dynamics and uncertainty, these are the plausible trajectories and their associated probabilities.”")
                  ),
                  div(class = "panel-card",
                      h3("Individualized Dynamical Forecast Model"),
                      p(class = "card-text", "Long-term vision: an individualized dynamical Forecast Model of mental-health trajectories, analogous in principle to ensemble forecasting in meteorology.")
                  )
              )
            ),
            tabPanel(
              "Clinical application",
              div(style = "margin-top:22px; display:grid; grid-template-columns:1fr 1fr; gap:18px;",
                  div(class = "panel-card card-pastel-1",
                      icon("stethoscope", class = "feature-card-icon"),
                      h3(style = "font-weight:400;", "Personalised assessment and diagnostic profiling")
                  ),
                  div(class = "panel-card card-pastel-2",
                      icon("layer-group", class = "feature-card-icon"),
                      h3(style = "font-weight:400;", "Risk stratification")
                  ),
                  div(class = "panel-card card-pastel-3",
                      icon("bullseye", class = "feature-card-icon"),
                      h3(style = "font-weight:400;", "Personalised treatment with element-focused interventions")
                  ),
                  div(class = "panel-card card-pastel-4",
                      icon("triangle-exclamation", class = "feature-card-icon"),
                      h3(style = "font-weight:400;", "Preventive and timed interventions (warning systems)")
                  ),
                  div(class = "panel-card card-pastel-5",
                      icon("chart-line", class = "feature-card-icon"),
                      h3(style = "font-weight:400;", "Integrating EMA insights into ROM")
                  ),
                  div(class = "panel-card card-pastel-6",
                      icon("shield", class = "feature-card-icon"),
                      h3(style = "font-weight:400;", "Relapse prevention and profiling")
                  )
              )
            )
          )
        ),
        
        # 7. References
        tabItem(
          tabName = "references",
          div(class = "eyebrow", "References"),
          h2("References"),
          div(class = "panel-card",
              p(class = "reference-item", HTML(
                "Borsboom, D. (2017). A network theory of mental disorders. <em>World Psychiatry, 16</em>(1), 5–13. <a href=\"https://doi.org/10.1002/wps.20375\" target=\"_blank\">https://doi.org/10.1002/wps.20375</a>"
              )),
              p(class = "reference-item", HTML(
                "Borsboom, D., Deserno, M. K., Rhemtulla, M., Epskamp, S., Fried, E. I., McNally, R. J., Robinaugh, D. J., Perugini, M., Dalege, J., Costantini, G., Isvoranu, A.-M., Wysocki, A. C., van Borkulo, C. D., & van Bork, R. (2021). Network analysis of multivariate data in psychological science. <em>Nature Reviews Methods Primers, 1</em>, Article 58. <a href=\"https://doi.org/10.1038/s43586-021-00055-w\" target=\"_blank\">https://doi.org/10.1038/s43586-021-00055-w</a>"
              )),
              p(class = "reference-item", HTML(
                "Fried, E. I., & Cramer, A. O. J. (2017). Moving forward: Challenges and directions for psychopathological network theory and methodology. <em>Perspectives on Psychological Science, 12</em>(6), 999–1020. <a href=\"https://doi.org/10.1177/1745691617705892\" target=\"_blank\">https://doi.org/10.1177/1745691617705892</a>"
              )),
              p(class = "reference-item", HTML(
                "Fried, E. I. (2022). Studying mental health problems as systems, not syndromes. <em>Current Directions in Psychological Science, 31</em>(6), 500–508. <a href=\"https://doi.org/10.1177/09637214221114089\" target=\"_blank\">https://doi.org/10.1177/09637214221114089</a>"
              )),
              p(class = "reference-item", HTML(
                "Henry, T. R., Robinaugh, D. J., & Fried, E. I. (2022). On the control of psychological networks. <em>Psychometrika, 87</em>(1), 188–213. <a href=\"https://doi.org/10.1007/s11336-021-09796-9\" target=\"_blank\">https://doi.org/10.1007/s11336-021-09796-9</a>"
              )),
              p(class = "reference-item", style = "margin-bottom:0;", HTML(
                "Robinaugh, D. J., Blanken, T. F., Bridger, E. K., Casamento-Moran, A., de Ron, J., Henry, T. R., Hoekstra, R. H. A., Stratis, G., van de Leemput, I. A., van Nes, E. H., Wang, S. B., Wheatley, T., & Fried, E. I. (2026). The future of the biopsychosocial model: Toward a transdisciplinary systems science of mental health. <em>Clinical Psychological Science, 14</em>(4), 470–496. <a href=\"https://doi.org/10.1177/21677026261435464\" target=\"_blank\">https://doi.org/10.1177/21677026261435464</a>"
              ))
          )
        )
      )
  )
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output, session) {

  # ---- About > Master's Thesis tab: code pop-up ----

  observeEvent(input$thesis_code_btn, {
    showModal(modalDialog(
      title = "Analysis code",
      pre(class = "code-block", "# R analysis code\n# Add your mixed graphical model / regression code here."),
      easyClose = TRUE,
      size = "l"
    ))
  })

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

  output$hsmm_schematic <- renderPlot({
    hsmm_schematic_plot(hsmm_sim_data$run_df)
  })

  output$hsmm_segmented <- renderPlot({
    hsmm_segmented_plot(hsmm_sim_data$df, hsmm_sim_data$run_df)
  })

  output$hsmm_code <- renderPrint({
    cat(paste(deparse(hsmm_build_simulation), collapse = "\n"))
    cat("\n\n")
    cat(paste(deparse(hsmm_schematic_plot), collapse = "\n"))
    cat("\n\n")
    cat(paste(deparse(hsmm_segmented_plot), collapse = "\n"))
  })

  output$ising_net_low   <- renderPlot({ ising_env$ising_network_plot(ising_env$w_low,  "Low connectivity") })
  output$ising_net_mod   <- renderPlot({ ising_env$ising_network_plot(ising_env$w_mod,  "Moderate connectivity") })
  output$ising_net_high  <- renderPlot({ ising_env$ising_network_plot(ising_env$w_high, "High connectivity") })

  output$ising_land_low  <- renderPlot({ plot(ising_env$result_low) })
  output$ising_land_mod  <- renderPlot({ plot(ising_env$result_mod) })
  output$ising_land_high <- renderPlot({ plot(ising_env$result_high) })

  output$ising_ts_low  <- renderPlot({ ising_env$ising_timeseries_plot(ising_env$sim_low,  "Low") })
  output$ising_ts_mod  <- renderPlot({ ising_env$ising_timeseries_plot(ising_env$sim_mod,  "Moderate") })
  output$ising_ts_high <- renderPlot({ ising_env$ising_timeseries_plot(ising_env$sim_high, "High") })

  output$ising_code <- renderPrint({
    cat(ising_script)
  })
}

shinyApp(ui, server)