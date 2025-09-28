# ============================================================
#  Project: Political Canvassing Resource Optimization Tool
#  Branch:  MCMC (Markov Chain Monte Carlo Optimization)
#  Purpose: Implements stochastic (MCMC) search and visualization
#           for optimizing Output Area (OA) visit allocations.
#
#  Description:
#  This branch extends the baseline optimization code (from main)
#  by introducing a Monte Carlo Markov Chain (MCMC) search process
#  to explore alternative splits of Tier 1, Tier 2, and Tier 3
#  Output Areas (OAs). Each candidate allocation is evaluated using
#  either deterministic expected voter return or a stochastic
#  simulation (using κ heterogeneity and response-rate variance).
#
#  Key Features Added in MCMC Branch:
#   • Full MCMC sampling loop with user-specified SDs for Tier 1,
#     Tier 2, and (Tier1+Tier2) distributions.
#   • Optional stochastic evaluation of each candidate using
#     independent simulations for robustness.
#   • Progress bar and data frame storage of all iterations.
#   • Diagnostics tab with scatter, histogram, and top-candidate tables.
#   • Best-found OA split automatically visualized in the same format
#     as the front-page allocation plot.
#   • Improved UI: multi-column collapsible top control panel with
#     hamburger toggle for compact display.
#   • Numerical safety and input validation for NA and zero values.
#
#  Branch Relationship:
#   • The `main` branch contains the original deterministic allocator.
#   • The `MCMC` branch adds stochastic optimization and diagnostics,
#     retaining compatibility with the same input/output structure.
#
#  Author: Charles Brewer
#  Last Updated: 28/10/2025
# ============================================================



# app.R
library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(htmltools)

tier_cols <- c("Tier 1" = "#1f77b4", "Tier 2" = "#ff7f0e", "Tier 3" = "#2ca02c")

# ---------- NA / bounds safe helpers ----------
safe_int <- function(x, default = 0L, minv = -Inf, maxv = Inf) {
  y <- suppressWarnings(as.integer(x))
  if (is.na(y)) y <- default
  y <- max(minv, min(y, maxv))
  y
}
safe_num <- function(x, default = 0, minv = -Inf, maxv = Inf) {
  y <- suppressWarnings(as.numeric(x))
  if (is.na(y)) y <- default
  y <- max(minv, min(y, maxv))
  y
}

# ---------- Top-box styles ----------
custom_css <- tags$head(
  tags$style(HTML("
    .controls-wrapper {
      position: sticky; top: 0; z-index: 1000;
      background: #f8f9fa; border-bottom: 1px solid #dee2e6;
      padding: 10px 12px 14px 12px;
    }
    .controls-header {
      display: flex; align-items: center; gap: 10px;
      margin-bottom: 10px;
    }
    .controls-title { font-size: 20px; font-weight: 600; margin: 0; }
    #controls-box-body {
      border: 1px solid #e5e7eb; border-radius: 10px;
      background: #fff; padding: 12px;
      display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
      gap: 14px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.04);
    }
    .control-card {
      border: 1px solid #eef1f5; border-radius: 10px; padding: 10px 12px; background: #fafafa;
    }
    .control-card h4 { margin-top: 0; font-size: 16px; font-weight: 600; }
    .spacer-hr { border: none; border-top: 1px dashed #e5e7eb; margin: 10px 0; }
    .btn-icon {
      border: 1px solid #dee2e6; background: white;
    }
    .note-muted { color: #6b7280; font-size: 12px; }
  "))
)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Political Canvassing Resource Optimization Tool"),
  custom_css,
  
  # ======= Collapsible Top Controls =======
  div(class = "controls-wrapper",
      div(class = "controls-header",
          actionButton("toggle_controls", NULL, icon = icon("bars"), class = "btn btn-sm btn-icon"),
          tags$h3(class = "controls-title", "Inputs"),
          span(class = "note-muted", "Show/hide the input panel with the hamburger")
      ),
      div(id = "controls-box-body",
          # Row 1: Party + Global parameters
          div(class = "control-card",
              h4("General"),
              selectInput(
                "party", "Select Party:",
                choices = c("Con", "Lab", "LD", "Ref", "Grn", "PC", "SNP"),
                selected = "Ref"
              ),
              numericInput("total_visits", "Total Visits Available:", value = 400, min = 1, step = 1),
              numericInput("total_oas", "Total number of OAs in Electoral Division:", value = 320, min = 1, step = 1),
              numericInput("voters_per_oa", "Voters per Output Area:", value = 300, min = 1, step = 1),
              numericInput("response_rate", "Canvassing Response Rate (%):", value = 20, min = 0.1, max = 100, step = 0.1)
          ),
          
          # Row 2: Tier 1 & 2
          div(class = "control-card",
              h4("Tier 1 (Highest Priority)"),
              numericInput("tier1_oas", "Number of OAs in Tier 1 (baseline mean):", value = 107, min = 0, step = 1),
              numericInput("tier1_likelihood", "Support Likelihood (%):", value = 45, min = 0, max = 100, step = 0.1),
              tags$hr(class = "spacer-hr"),
              h4("Tier 2 (Medium Priority)"),
              numericInput("tier2_oas", "Number of OAs in Tier 2 (baseline mean):", value = 107, min = 0, step = 1),
              numericInput("tier2_likelihood", "Support Likelihood (%):", value = 33, min = 0, max = 100, step = 0.1)
          ),
          
          # Row 3: Tier 3 + Pass cap
          div(class = "control-card",
              h4("Tier 3 (Lowest Priority)"),
              checkboxInput("lock_t3", "Set Tier 3 OAs as remainder so T1+T2+T3 = Total OAs", TRUE),
              numericInput("tier3_oas", "Number of OAs in Tier 3 (baseline):", value = 106, min = 0, step = 1),
              numericInput("tier3_likelihood", "Support Likelihood (%):", value = 15, min = 0, max = 100, step = 0.1),
              tags$hr(class = "spacer-hr"),
              h4("Pass Settings"),
              numericInput("max_passes_per_oa", "Max passes per OA (cap):", value = 5, min = 1, step = 1)
          ),
          
          # Row 4: MCMC + Stochastic block
          div(class = "control-card",
              h4("MCMC Search (Tier OA counts)"),
              checkboxInput("use_mcmc", "Use MCMC to optimize Tier OA split", TRUE),
              numericInput("mcmc_runs", "Number of MCMC runs:", value = 2000, min = 10, step = 10),
              numericInput("sd_t1", "SD for Tier 1 OAs (Normal draw):", value = 20, min = 0, step = 1),
              numericInput("sd_t2", "SD for Tier 2 OAs (Normal draw):", value = 20, min = 0, step = 1),
              numericInput("sd_sum12", "SD for (Tier1 + Tier2) (Normal draw):", value = 30, min = 0, step = 1),
              numericInput("mcmc_seed", "Random seed (optional):", value = NA),
              tags$hr(class = "spacer-hr"),
              h4("Stochastic Evaluation (Independent Test)"),
              checkboxInput("use_stochastic_eval", "Use stochastic scoring for MCMC", TRUE),
              numericInput("stoch_sims", "Simulations per candidate (S):", value = 50, min = 5, step = 5),
              numericInput("kappa", "OA heterogeneity κ (larger = less variance):", value = 50, min = 2, step = 1),
              numericInput("r_sd_pct", "Response rate SD (% of r):", value = 5, min = 0, step = 1),
              numericInput("N_sd_pct", "Voters/OA SD (% of N):", value = 0, min = 0, step = 1)
          ),
          
          # Row 5: Action button
          div(class = "control-card",
              h4("Run"),
              actionButton("optimize", "Optimize Allocation", class = "btn-primary")
          )
      )
  ),
  
  # ======= Main content =======
  tabsetPanel(
    tabPanel(
      "Optimization Results",
      br(),
      h3("Optimal Visit Allocation"),
      DTOutput("results_table"),
      br(),
      h3("Summary Statistics"),
      verbatimTextOutput("summary_stats"),
      br(),
      h3("Resource Allocation Visualization"),
      plotlyOutput("allocation_plot")
    ),
    tabPanel(
      "Sensitivity Analysis",
      br(),
      h3("First-Pass Returns per Visit by Tier"),
      plotlyOutput("returns_plot"),
      br(),
      h3("Marginal Returns (Selected Top-V Sequence)"),
      plotlyOutput("marginal_plot")
    ),
    tabPanel(
      "MCMC Results",
      br(),
      h3("Best OA Split Found"),
      DTOutput("mcmc_best_table"),
      br(),
      h3("Best Split Allocation (same format as front page)"),
      plotlyOutput("mcmc_best_allocation_plot"),
      br(),
      h3("Distribution of Total Expected Voters (MCMC samples)"),
      plotlyOutput("mcmc_hist"),
      br(),
      h3("Top Candidates (OA Splits)"),
      DTOutput("mcmc_top_table"),
      br(),
      h3("Diagnostics"),
      plotlyOutput("mcmc_scatter"),
      br()
    )
  )
)

server <- function(input, output, session) {
  
  # Hamburger toggle for the inputs panel
  observeEvent(input$toggle_controls, {
    toggle(id = "controls-box-body", anim = TRUE, animType = "slide")
  })
  
  # Keep Tier 3 as remainder of TOTAL OAs (not visits)
  observeEvent(list(input$total_oas, input$tier1_oas, input$tier2_oas, input$lock_t3), {
    if (isTRUE(input$lock_t3)) {
      tot  <- safe_int(input$total_oas, 0L, 0L)
      t1   <- safe_int(input$tier1_oas, 0L, 0L)
      t2   <- safe_int(input$tier2_oas, 0L, 0L)
      remainder <- max(0L, tot - t1 - t2)
      if (!identical(remainder, input$tier3_oas)) {
        updateNumericInput(session, "tier3_oas", value = remainder)
      }
    }
  }, ignoreInit = TRUE)
  
  # ---------- Core deterministic allocator ----------
  run_optimizer_once <- function(V, Otot, N, r, Kcap,
                                 t1_oas, t2_oas, t3_oas,
                                 like1, like2, like3) {
    # NA/Bounds safety
    V    <- safe_int(V,    0L, 0L)
    Otot <- safe_int(Otot, 0L, 0L)
    N    <- safe_int(N,    1L, 1L)
    r    <- safe_num(r,    0,   0, 1)
    Kcap <- safe_int(Kcap, 1L,  1L)
    
    t1_oas <- safe_int(t1_oas, 0L, 0L)
    t2_oas <- safe_int(t2_oas, 0L, 0L)
    t3_oas <- safe_int(t3_oas, 0L, 0L)
    
    like1 <- safe_num(like1, 0, 0, 100)
    like2 <- safe_num(like2, 0, 0, 100)
    like3 <- safe_num(like3, 0, 0, 100)
    
    tiers <- data.frame(
      tier       = c("Tier 1","Tier 2","Tier 3"),
      num_oas    = pmax(0L, c(as.integer(t1_oas), as.integer(t2_oas), as.integer(t3_oas))),
      likelihood = c(like1, like2, like3),
      stringsAsFactors = FALSE
    )
    
    if (sum(tiers$num_oas) == 0 || V == 0 || r <= 0) {
      tiers$S <- N * (tiers$likelihood / 100)
      tiers$visits_allocated <- 0L
      tiers$total_voters_found <- 0
      Kmax <- Kcap
      pass_dist <- setNames(vector("list", 3), tiers$tier)
      for (tr in tiers$tier) pass_dist[[tr]] <- integer(Kmax + 1)
      return(list(
        tiers = tiers,
        pass_distributions = pass_dist,
        selected_sequence = numeric(0),
        total_visits_used = 0L,
        total_voters_found = 0L,
        status = "No capacity or zero response rate.",
        Kmax = Kcap,
        Otot = Otot
      ))
    }
    
    Kmax <- Kcap
    pow_vec <- if (isTRUE(r < 1)) (1 - r)^(seq_len(Kmax) - 1) else c(1, rep(0, max(0, Kmax - 1)))
    tiers$S <- N * (tiers$likelihood / 100)
    
    # Pass "slots"
    slots <- do.call(rbind, lapply(1:nrow(tiers), function(i) {
      data.frame(
        tier     = tiers$tier[i],
        k        = 1:Kmax,
        marginal = tiers$S[i] * r * pow_vec,
        capacity = tiers$num_oas[i]
      )
    }))
    slots <- slots[order(-slots$marginal, slots$k), ]
    
    per_tier_k <- lapply(setNames(tiers$tier, tiers$tier), function(x) integer(Kmax))
    remaining <- V
    selected_sequence <- numeric(0)
    
    for (row_i in seq_len(nrow(slots))) {
      if (remaining <= 0) break
      tr  <- slots$tier[row_i]
      kk  <- slots$k[row_i]
      cap <- slots$capacity[row_i]
      
      if (kk == 1) {
        cap <- max(0, cap - per_tier_k[[tr]][1])
      } else {
        cap <- min(cap, per_tier_k[[tr]][kk - 1] - per_tier_k[[tr]][kk])
        cap <- max(0, cap)
      }
      
      if (cap <= 0) next
      take <- min(cap, remaining)
      if (take > 0) {
        per_tier_k[[tr]][kk] <- per_tier_k[[tr]][kk] + take
        remaining <- remaining - take
        selected_sequence <- c(selected_sequence, rep(slots$marginal[row_i], take))
      }
    }
    
    visits_allocated <- sapply(tiers$tier, function(tr) sum(per_tier_k[[tr]]))
    
    voters_found <- sapply(tiers$tier, function(tr) {
      mks <- tiers$S[tiers$tier == tr] * r * if (length(pow_vec)) pow_vec else 1
      sum(per_tier_k[[tr]] * mks)
    })
    
    # Distributions: exactly n passes per OA
    pass_distributions <- setNames(vector("list", length(tiers$tier)), tiers$tier)
    max_passes_used <- setNames(integer(length(tiers$tier)), tiers$tier)
    avg_passes <- setNames(numeric(length(tiers$tier)), tiers$tier)
    
    for (tr in tiers$tier) {
      ck <- per_tier_k[[tr]]
      exact <- integer(Kmax + 1)  # bins 0..Kmax
      exact[1] <- tiers$num_oas[tiers$tier == tr] - ck[1]      # exactly 0 passes
      for (kk in 1:(Kmax - 1)) exact[kk + 1] <- ck[kk] - ck[kk + 1]
      exact[Kmax + 1] <- ck[Kmax]
      pass_distributions[[tr]] <- exact
      max_passes_used[tr] <- if (any(exact[-1] > 0)) max(which(exact[-1] > 0)) else 0
      nn <- 0:Kmax
      denom <- tiers$num_oas[tiers$tier == tr]
      avg_passes[tr] <- if (denom > 0) sum(nn * exact) / denom else 0
    }
    
    tiers$visits_allocated    <- as.integer(visits_allocated)
    tiers$total_voters_found  <- as.numeric(voters_found)
    tiers$avg_passes_per_oa   <- as.numeric(avg_passes[tiers$tier])
    tiers$max_passes_used     <- as.integer(max_passes_used[tiers$tier])
    
    list(
      tiers = tiers,
      pass_distributions = pass_distributions,
      selected_sequence = selected_sequence,
      total_visits_used = as.integer(sum(visits_allocated)),
      total_voters_found = sum(voters_found),
      status = if (remaining > 0) sprintf("%d visits unallocatable (no capacity or zero marginal).", remaining) else "OK",
      Kmax = Kcap,
      Otot = Otot
    )
  }
  
  # ---------- Stochastic evaluation helpers ----------
  .beta_from_mean_kappa <- function(mean_prob, kappa) {
    mean_prob <- pmin(pmax(mean_prob, 1e-6), 1 - 1e-6)
    alpha <- mean_prob * kappa
    beta  <- (1 - mean_prob) * kappa
    list(alpha = alpha, beta = beta)
  }
  
  simulate_realized_supporters <- function(res_alloc, base_r, base_N, like_vec, kappa,
                                           r_sd = 0, N_sd = 0, S = 50) {
    kappa <- safe_num(kappa, 50, 2)
    r_sd  <- safe_num(r_sd,  0,  0)
    N_sd  <- safe_num(N_sd,  0,  0)
    S     <- safe_int(S,    50L, 5L)
    
    tiers <- res_alloc$tiers$tier
    Kmax  <- safe_int(res_alloc$Kmax, 1L, 1L)
    
    base_r <- safe_num(base_r, 0, 0, 1)
    base_N <- safe_int(base_N, 1L, 1L)
    
    mean_probs <- like_vec / 100
    betas <- lapply(mean_probs, function(m) .beta_from_mean_kappa(m, kappa))
    
    sim_once <- function() {
      r_draw <- if (r_sd > 0) {
        rr <- rnorm(1, mean = base_r, sd = pmax(1e-9, base_r * r_sd))
        pmin(pmax(rr, 1e-6), 0.999)
      } else base_r
      
      N_draw <- if (N_sd > 0) {
        nn <- rnorm(1, mean = base_N, sd = pmax(1e-9, base_N * N_sd))
        pmax(round(nn), 1)
      } else base_N
      
      pow_vec <- if (r_draw < 1) (1 - r_draw)^(seq_len(Kmax) - 1) else c(1, rep(0, max(0, Kmax - 1)))
      total <- 0L
      
      for (ti in seq_along(tiers)) {
        tr <- tiers[ti]
        exact <- res_alloc$pass_distributions[[tr]]  # length Kmax+1 (k=0..Kmax)
        a <- betas[[ti]]$alpha
        b <- betas[[ti]]$beta
        
        for (k in 1:Kmax) {
          oa_k <- exact[k + 1]
          if (oa_k <= 0) next
          
          p_vec <- rbeta(oa_k, a, b)
          
          for (j in 1:k) {
            q <- p_vec * r_draw * pow_vec[j]
            q <- pmin(pmax(q, 1e-8), 1 - 1e-8)
            total <- total + sum(rbinom(oa_k, size = N_draw, prob = q))
          }
        }
      }
      as.integer(total)
    }
    
    mean(vapply(seq_len(S), function(i) sim_once(), integer(1)))
  }
  
  # ================
  # Main optimizer (eventReactive) — runs direct or via MCMC
  # ================
  mcmc_payload <- reactiveVal(NULL)
  
  optimization_results <- eventReactive(input$optimize, {
    V    <- safe_int(input$total_visits,      0L, 0L)
    Otot <- safe_int(input$total_oas,         0L, 0L)
    N    <- safe_int(input$voters_per_oa,     1L, 1L)
    r    <- safe_num(input$response_rate,     0,   0, 100) / 100
    Kcap <- safe_int(input$max_passes_per_oa, 1L, 1L)
    
    like1 <- safe_num(input$tier1_likelihood, 0, 0, 100)
    like2 <- safe_num(input$tier2_likelihood, 0, 0, 100)
    like3 <- safe_num(input$tier3_likelihood, 0, 0, 100)
    
    # MCMC route
    if (isTRUE(input$use_mcmc)) {
      R <- safe_int(input$mcmc_runs, 2000L, 10L)
      if (!is.na(input$mcmc_seed)) set.seed(as.integer(input$mcmc_seed))
      
      base_t1  <- safe_int(input$tier1_oas, 0L, 0L)
      base_t2  <- safe_int(input$tier2_oas, 0L, 0L)
      mu_sum12 <- base_t1 + base_t2
      
      sd_t1  <- safe_num(input$sd_t1,   0, 0)
      sd_t2  <- safe_num(input$sd_t2,   0, 0)
      sd_sum <- safe_num(input$sd_sum12,0, 0)
      
      best_score <- -Inf
      best_split <- c(NA_integer_, NA_integer_, NA_integer_)
      best_res   <- NULL
      
      store <- data.frame(
        run = integer(R),
        t1 = integer(R), t2 = integer(R), t3 = integer(R),
        total_voters = numeric(R),
        stringsAsFactors = FALSE
      )
      
      withProgress(message = "Running MCMC search…", value = 0, {
        inc <- 1 / R
        for (i in seq_len(R)) {
          # Draw T1 and (T1+T2); couple T2, derive T3
          t1_draw   <- round(rnorm(1, mean = base_t1,  sd = sd_t1))
          sum_draw  <- round(rnorm(1, mean = mu_sum12, sd = sd_sum))
          
          t1 <- max(0L, min(Otot, t1_draw))
          t2 <- sum_draw - t1
          t2 <- max(0L, min(Otot, t2))
          if (t1 + t2 > Otot) t2 <- max(0L, Otot - t1)
          t3 <- max(0L, Otot - t1 - t2)
          
          if ((t1 + t2 + t3) == 0L) {
            store[i, ] <- list(i, t1, t2, t3, 0L)
            incProgress(inc); next
          }
          
          res_i <- run_optimizer_once(V, Otot, N, r, Kcap, t1, t2, t3, like1, like2, like3)
          
          score <- if (isTRUE(input$use_stochastic_eval)) {
            simulate_realized_supporters(
              res_alloc = res_i,
              base_r = r,
              base_N = N,
              like_vec = c(like1, like2, like3),
              kappa = safe_num(input$kappa,    50, 2),
              r_sd  = safe_num(input$r_sd_pct,  0, 0)/100,
              N_sd  = safe_num(input$N_sd_pct,  0, 0)/100,
              S     = safe_int(input$stoch_sims, 50L, 5L)
            )
          } else {
            res_i$total_voters_found
          }
          
          store[i, ] <- list(i, t1, t2, t3, score)
          
          if (is.finite(score) && score > best_score) {
            best_score <- score
            best_split <- c(t1, t2, t3)
            best_res   <- res_i
          }
          incProgress(inc)
        }
      })
      
      mcmc_payload(list(
        diag = store,
        best_split = setNames(best_split, c("Tier 1","Tier 2","Tier 3")),
        best_score = best_score
      ))
      
      return(best_res)
    }
    
    # Direct (non-MCMC) route
    base_t1 <- safe_int(input$tier1_oas, 0L, 0L)
    base_t2 <- safe_int(input$tier2_oas, 0L, 0L)
    t3_val  <- if (isTRUE(input$lock_t3)) {
      max(0L, Otot - base_t1 - base_t2)
    } else {
      safe_int(input$tier3_oas, 0L, 0L)
    }
    
    run_optimizer_once(
      V, Otot, N, r, Kcap,
      t1_oas = base_t1, t2_oas = base_t2, t3_oas = t3_val,
      like1 = like1, like2 = like2, like3 = like3
    )
  })
  
  # ========================
  # Outputs (integer formatted)
  # ========================
  
  output$results_table <- renderDT({
    res <- optimization_results(); req(res)
    df <- res$tiers
    party <- input$party
    
    # Pretty distribution like "0:12, 1:95, 2:30, 3:0, ..."
    dist_str <- vapply(df$tier, function(tr) {
      exact <- res$pass_distributions[[tr]]
      paste0(paste0(0:res$Kmax, ":", as.integer(exact)), collapse = ", ")
    }, character(1))
    
    r_ui <- safe_num(input$response_rate, 0, 0, 100) / 100
    
    out <- data.frame(
      Tier = df$tier,
      "Number of OAs" = as.integer(df$num_oas),
      "Support Likelihood" = paste0(as.integer(round(df$likelihood)), "%"),
      "Visits Allocated (total)" = as.integer(df$visits_allocated),
      "Avg Passes/OA" = as.integer(round(df$avg_passes_per_oa)),
      "Max Passes Used" = as.integer(df$max_passes_used),
      "Voters per Visit (First Pass)" = as.integer(round(df$S * r_ui)),
      "Total Voters (expected)" = as.integer(round(df$total_voters_found)),
      "Pass Distribution (passes:number of OAs)" = dist_str,
      check.names = FALSE
    )
    
    datatable(
      out,
      rownames = FALSE,
      options = list(pageLength = 10, dom = 't'),
      caption = tags$caption(
        style = 'caption-side: top; text-align: left; font-weight:600;',
        paste("Party:", party)
      )
    )
  })
  
  output$summary_stats <- renderText({
    res <- optimization_results(); req(res)
    party <- input$party
    eff <- if (safe_int(input$total_visits,0L,0L) > 0) {
      100 * res$total_visits_used / safe_int(input$total_visits,0L,0L)
    } else 0
    
    avg_return_int <- if (res$total_visits_used > 0) {
      as.integer(round(res$total_voters_found / res$total_visits_used))
    } else {
      0L
    }
    
    bp <- mcmc_payload()
    
    paste0(
      "Status: ", res$status, "\n",
      "Total OAs (Electoral Division): ", as.integer(res$Otot), "\n",
      "Tier OAs sum: ", as.integer(sum(res$tiers$num_oas)), "\n",
      "Total Visits Allocated: ", as.integer(res$total_visits_used), " / ", as.integer(safe_int(input$total_visits,0L,0L)), "\n",
      "Total ", party, " Voters (expected): ", as.integer(round(res$total_voters_found)), "\n",
      "Average Return per Visit: ", avg_return_int, " ", party, " voters\n",
      "Efficiency (Allocated/Available): ", sprintf("%d%%", as.integer(round(eff))),
      if (isTRUE(input$use_mcmc) && !is.null(bp)) {
        paste0(
          "\nMCMC Best Split (T1,T2,T3): ",
          paste(as.integer(bp$best_split), collapse = ", "),
          "  | Best Total Voters: ",
          as.integer(round(bp$best_score))
        )
      } else ""
    )
  })
  
  # Allocation bar (front page)
  output$allocation_plot <- renderPlotly({
    res <- optimization_results(); req(res)
    df <- res$tiers
    party <- input$party
    ymax <- if (nrow(df)) max(df$visits_allocated, na.rm = TRUE) else 1
    pad  <- if (ymax > 0) ymax * 0.2 else 1
    
    plot_ly(
      df,
      x = ~tier, y = ~visits_allocated, type = 'bar',
      text = ~paste(
        "Visits:", as.integer(visits_allocated),
        "<br>Expected ", party, " voters:", as.integer(round(total_voters_found)),
        "<br>Avg passes/OA:", as.integer(round(avg_passes_per_oa))
      ),
      textposition = 'outside',
      marker = list(color = tier_cols[df$tier]),
      cliponaxis = FALSE
    ) %>%
      layout(
        title = paste("Visit Allocation by Tier — Party:", party),
        xaxis = list(title = "Tier"),
        yaxis = list(title = "Number of Visits", range = c(0, ymax + pad), tickformat = ",d"),
        margin = list(t = 90),
        showlegend = FALSE
      )
  })
  
  # First-pass returns per visit
  output$returns_plot <- renderPlotly({
    res <- optimization_results(); req(res)
    df <- res$tiers
    party <- input$party
    r_ui <- safe_num(input$response_rate, 0, 0, 100) / 100
    first_pass_rpv <- as.integer(round(df$S * r_ui))
    ymax <- if (length(first_pass_rpv)) max(first_pass_rpv, na.rm = TRUE) else 1
    pad  <- if (ymax > 0) ymax * 0.2 else 1
    
    plot_ly(
      data = data.frame(tier = df$tier, rpv = first_pass_rpv),
      x = ~tier, y = ~rpv, type = 'bar',
      text = ~paste("First-pass ", party, " voters/visit:", as.integer(rpv)),
      textposition = 'outside',
      marker = list(color = tier_cols[df$tier]),
      cliponaxis = FALSE
    ) %>%
      layout(
        title = paste("Expected ", party, " Voters per Visit (First Pass)", sep = ""),
        xaxis = list(title = "Tier"),
        yaxis = list(
          title = paste(party, " Voters per Visit"),
          range = c(0, ymax + pad),
          tickformat = ",d"
        ),
        margin = list(t = 90),
        showlegend = FALSE
      )
  })
  
  # Marginal & cumulative sequences (integerized)
  output$marginal_plot <- renderPlotly({
    res <- optimization_results(); req(res)
    party <- input$party
    seqv <- res$selected_sequence
    if (length(seqv) == 0) seqv <- 0
    seqv_i <- as.integer(round(seqv))
    cumv_i <- cumsum(seqv_i)
    
    plot_ly() %>%
      add_trace(
        x = seq_along(cumv_i), y = cumv_i, type = 'scatter', mode = 'lines',
        name = paste("Cumulative Expected", party, "Voters")
      ) %>%
      add_trace(
        x = seq_along(seqv_i), y = seqv_i, type = 'scatter', mode = 'lines',
        name = 'Marginal per Visit', yaxis = 'y2'
      ) %>%
      layout(
        title = paste("Cumulative and Marginal Returns — Party:", party),
        xaxis = list(title = "Visit Number (global)", tickformat = ",d"),
        yaxis = list(title = paste("Cumulative Expected", party, "Voters"), tickformat = ",d"),
        yaxis2 = list(title = "Marginal Return", overlaying = 'y', side = 'right', tickformat = ",d"),
        legend = list(x = 0.7, y = 0.9)
      )
  })
  
  # -----------------
  # MCMC tab outputs
  # -----------------
  output$mcmc_best_table <- renderDT({
    bp <- mcmc_payload(); req(bp)
    best <- data.frame(
      `Tier 1 OAs` = as.integer(bp$best_split[1]),
      `Tier 2 OAs` = as.integer(bp$best_split[2]),
      `Tier 3 OAs` = as.integer(bp$best_split[3]),
      `Best Total Voters (expected)` = as.integer(round(bp$best_score)),
      check.names = FALSE
    )
    datatable(best, rownames = FALSE, options = list(dom = 't'))
  })
  
  output$mcmc_top_table <- renderDT({
    bp <- mcmc_payload(); req(bp)
    dd <- bp$diag
    dd <- dd[order(-dd$total_voters), ]
    dd$t1 <- as.integer(dd$t1)
    dd$t2 <- as.integer(dd$t2)
    dd$t3 <- as.integer(dd$t3)
    dd$total_voters <- as.integer(round(dd$total_voters))
    topn <- head(dd, 20)
    datatable(
      topn,
      rownames = FALSE,
      options = list(pageLength = 10, dom = 'tip'),
      colnames = c("Run","Tier 1 OAs","Tier 2 OAs","Tier 3 OAs","Total Voters (expected)")
    )
  })
  
  output$mcmc_scatter <- renderPlotly({
    bp <- mcmc_payload(); req(bp)
    dd <- bp$diag
    plot_ly(dd, x = ~t1, y = ~t2, type = 'scatter', mode = 'markers',
            text = ~paste("Total voters:", as.integer(round(total_voters)),
                          "<br>T1:", as.integer(t1),
                          " T2:", as.integer(t2),
                          " T3:", as.integer(t3))) %>%
      layout(title = "MCMC Samples: Tier1 vs Tier2 (color = voters)",
             xaxis = list(title = "Tier 1 OAs", tickformat = ",d"),
             yaxis = list(title = "Tier 2 OAs", tickformat = ",d"))
  })
  
  output$mcmc_hist <- renderPlotly({
    bp <- mcmc_payload(); req(bp)
    dd <- bp$diag
    plot_ly(dd, x = ~as.integer(round(total_voters)), type = 'histogram', nbinsx = 40) %>%
      layout(
        title = "Distribution of Total Expected Voters (MCMC samples)",
        xaxis = list(title = "Total expected voters", tickformat = ",d"),
        yaxis = list(title = "Count", tickformat = ",d"),
        margin = list(t = 90),
        showlegend = FALSE
      )
  })
  
  # Best split allocation chart (same style as front page)
  output$mcmc_best_allocation_plot <- renderPlotly({
    bp <- mcmc_payload(); req(bp)
    # Bail out cleanly if best_split is missing/NA
    if (is.null(bp$best_split) || any(is.na(bp$best_split))) {
      validate(need(FALSE, "Run Optimize to compute a best OA split (MCMC)."))
    }
    
    V    <- safe_int(input$total_visits,      0L, 0L)
    Otot <- safe_int(input$total_oas,         0L, 0L)
    N    <- safe_int(input$voters_per_oa,     1L, 1L)
    r    <- safe_num(input$response_rate,     0,   0, 100) / 100
    Kcap <- safe_int(input$max_passes_per_oa, 1L, 1L)
    like1 <- safe_num(input$tier1_likelihood, 0, 0, 100)
    like2 <- safe_num(input$tier2_likelihood, 0, 0, 100)
    like3 <- safe_num(input$tier3_likelihood, 0, 0, 100)
    party <- input$party
    
    best_t1 <- as.integer(bp$best_split[1])
    best_t2 <- as.integer(bp$best_split[2])
    best_t3 <- as.integer(bp$best_split[3])
    
    res <- run_optimizer_once(
      V, Otot, N, r, Kcap,
      t1_oas = best_t1, t2_oas = best_t2, t3_oas = best_t3,
      like1 = like1, like2 = like2, like3 = like3
    )
    
    df <- res$tiers
    ymax <- if (nrow(df)) max(df$visits_allocated, na.rm = TRUE) else 1
    pad  <- if (ymax > 0) ymax * 0.2 else 1
    
    plot_ly(
      df,
      x = ~tier, y = ~visits_allocated, type = 'bar',
      text = ~paste(
        "Visits:", as.integer(visits_allocated),
        "<br>Expected ", party, " voters:", as.integer(round(total_voters_found)),
        "<br>Avg passes/OA:", as.integer(round(avg_passes_per_oa))
      ),
      textposition = 'outside',
      marker = list(color = tier_cols[df$tier]),
      cliponaxis = FALSE
    ) %>%
      layout(
        title = paste("Visit Allocation by Tier — Best OA Split (Party:", party, ")"),
        xaxis = list(title = "Tier"),
        yaxis = list(title = "Number of Visits", range = c(0, ymax + pad), tickformat = ",d"),
        margin = list(t = 90),
        showlegend = FALSE
      )
  })
}

shinyApp(ui = ui, server = server)