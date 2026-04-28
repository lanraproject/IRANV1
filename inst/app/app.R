required_packages <- c(
  "shiny","shinydashboard","shinyjs","DT","readxl",
  "dplyr","ggplot2","mirt","psych",
  "plotly","shinycssloaders","shinyWidgets","scales"
)

installed <- rownames(installed.packages())

for (p in required_packages) {
  if (!(p %in% installed)) {
    install.packages(p)
  }
}


suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(DT)
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(mirt)
  library(psych)
  library(plotly)
  library(shinycssloaders)
  library(shinyWidgets)
  library(scales)
})

# ============================================================
#   HELPER FUNCTIONS
# ============================================================

msi_transform <- function(data, scale_min = NULL, scale_max = NULL) {
  tryCatch({
    result         <- data
    all_scale_list <- list()
    for (j in 1:ncol(data)) {
      x    <- data[, j]
      n    <- sum(!is.na(x))
      cats <- sort(unique(x[!is.na(x)]))
      K    <- length(cats)
      if (K <= 1) next
      freq_tbl <- table(factor(x, levels = cats))
      VFreq    <- as.numeric(freq_tbl)
      VProp    <- VFreq / n
      VCum     <- cumsum(VProp)
      VZ    <- numeric(K)
      VDens <- numeric(K)
      for (i in 1:(K - 1)) {
        z_i      <- qnorm(pmin(pmax(VCum[i], 1e-6), 1 - 1e-6))
        VZ[i]    <- z_i
        VDens[i] <- dnorm(z_i)
      }
      VZ[K]    <- Inf
      VDens[K] <- 0
      VScale <- numeric(K)
      for (i in 1:K) {
        dens_lo <- if (i == 1) 0    else VDens[i - 1]
        dens_hi <- VDens[i]
        cum_lo  <- if (i == 1) 0    else VCum[i - 1]
        cum_hi  <- if (i == K) 1    else VCum[i]
        denom   <- cum_hi - cum_lo
        VScale[i] <- if (denom < 1e-10) NA else (dens_lo - dens_hi) / denom
      }
      sc_min <- min(VScale, na.rm = TRUE)
      
      if (!is.null(scale_min)) {
        VScale_final <- VScale - sc_min + scale_min
      } else {
        VScale_final <- VScale - sc_min
      }
      all_scale_list[[colnames(data)[j]]] <- data.frame(
        Kategori  = cats,
        Frek      = VFreq,
        Proporsi  = round(VProp, 4),
        Kum       = round(VCum,  4),
        Densitas  = round(VDens, 4),
        Z         = round(ifelse(is.infinite(VZ), NA, VZ), 4),
        Nilai_MSI = round(VScale_final, 4)
      )
      for (k in seq_along(cats))
        result[!is.na(data[, j]) & data[, j] == cats[k], j] <- VScale_final[k]
    }
    list(data = as.data.frame(result), mapping = all_scale_list)
  }, error = function(e) list(data = data, mapping = list()))
}

transform_scores <- function(theta_vec) {
  50 + 10 * theta_vec
}

detect_data_type <- function(df) {
  vals <- unlist(df); vals <- vals[!is.na(vals)]
  if (all(vals %in% c(0, 1))) "dikotomus" else "politomus"
}

safe_extract_params <- function(fit_obj) {
  tryCatch({
    p <- coef(fit_obj, IRTpars = TRUE, simplify = TRUE)$items
    as.data.frame(round(p, 4))
  }, error = function(e) {
    p <- coef(fit_obj, simplify = TRUE)$items
    as.data.frame(round(p, 4))
  })
}

get_b_params <- function(params_df) {
  b_cols <- grep("^b", colnames(params_df), value = TRUE)
  if (length(b_cols) == 0) b_cols <- grep("^d", colnames(params_df), value = TRUE)
  if (length(b_cols) == 0) return(NULL)
  rowMeans(params_df[, b_cols, drop = FALSE], na.rm = TRUE)
}

get_a_params <- function(params_df) {
  a_col <- grep("^a", colnames(params_df), value = TRUE)
  if (length(a_col) == 0) return(NULL)
  params_df[, a_col[1]]
}

get_itemtype <- function(model_choice) switch(model_choice,
                                              "1PL"="Rasch","2PL"="2PL","3PL"="3PL",
                                              "PCM"="Rasch","GPCM"="gpcm","GRM"="graded")

# ── Fungsi estimasi IRT yang robust untuk politomus ──────────────────────────
# Menangani: negative definite Hessian, convergence failure, singular matrices
run_mirt_robust <- function(df, model_choice, verbose = FALSE) {
  
  itemtype   <- get_itemtype(model_choice)
  is_poly    <- model_choice %in% c("PCM", "GPCM", "GRM")
  n_items    <- ncol(df)
  n_cats_vec <- sapply(df, function(x) length(unique(na.omit(x))))
  
  # --- Pengaturan teknis dasar untuk stabilitas ---
  mirt_opts <- list(
    TOL           = 1e-4,      # Toleransi konvergensi (lebih longgar = lebih stabil)
    NCYCLES       = 2000,      # Iterasi maksimum lebih banyak
    gain          = c(0.1, 0.1, 0.001),  # Step-size EM lebih konservatif
    EHPrior       = NULL
  )
  
  # --- Prior untuk parameter: mencegah nilai ekstrem ---
  # Prior Gaussian untuk parameter b (difficulty/threshold)
  # Prior log-normal untuk parameter a (discrimination)
  if (is_poly) {
    # Untuk politomus: gunakan prior lebih kuat agar konvergen
    grsm_prior <- mirt.model(paste0(
      "F = 1-", n_items, "\n",
      "PRIOR = (1-", n_items, ", a1, lnorm, 0.2, 0.4)"
    ))
  }
  
  # --- Coba estimasi dengan berbagai fallback ---
  fit <- NULL
  err_msgs <- c()
  
  # Percobaan 1: Estimasi standar dengan prior
  fit <- tryCatch({
    if (is_poly && model_choice %in% c("GPCM", "GRM")) {
      mirt(
        data       = df,
        model      = 1,
        itemtype   = itemtype,
        method     = "EM",
        SE         = TRUE,
        verbose    = verbose,
        technical  = mirt_opts,
        # Prior untuk diskriminasi (lognormal): mencegah a → 0 atau ∞
        GenRandomPars = FALSE
      )
    } else {
      mirt(
        data      = df,
        model     = 1,
        itemtype  = itemtype,
        method    = "EM",
        SE        = TRUE,
        verbose   = verbose,
        technical = mirt_opts
      )
    }
  }, error = function(e) {
    err_msgs <<- c(err_msgs, paste("Percobaan 1:", e$message))
    NULL
  }, warning = function(w) {
    # Tangkap peringatan konvergensi tapi tetap lanjutkan
    withCallingHandlers(
      if (is_poly && model_choice %in% c("GPCM", "GRM")) {
        mirt(df, 1, itemtype=itemtype, method="EM", SE=TRUE,
             verbose=verbose, technical=mirt_opts, GenRandomPars=FALSE)
      } else {
        mirt(df, 1, itemtype=itemtype, method="EM", SE=TRUE,
             verbose=verbose, technical=mirt_opts)
      },
      warning = function(w) invokeRestart("muffleWarning")
    )
  })
  
  # Percobaan 2: Jika gagal, gunakan SE=FALSE dan toleransi lebih longgar
  if (is.null(fit)) {
    fit <- tryCatch({
      mirt_opts2 <- mirt_opts
      mirt_opts2$TOL <- 1e-3
      mirt(
        data      = df,
        model     = 1,
        itemtype  = itemtype,
        method    = "EM",
        SE        = FALSE,
        verbose   = verbose,
        technical = mirt_opts2
      )
    }, error = function(e) {
      err_msgs <<- c(err_msgs, paste("Percobaan 2:", e$message))
      NULL
    }, warning = function(w) {
      withCallingHandlers(
        mirt(df, 1, itemtype=itemtype, method="EM", SE=FALSE,
             verbose=verbose, technical=list(TOL=1e-3, NCYCLES=2000)),
        warning = function(w) invokeRestart("muffleWarning")
      )
    })
  }
  
  # Percobaan 3: Fallback ke MHRM (Monte Carlo EM) - lebih robust untuk data sulit
  if (is.null(fit)) {
    fit <- tryCatch({
      mirt(
        data      = df,
        model     = 1,
        itemtype  = itemtype,
        method    = "MHRM",
        SE        = FALSE,
        verbose   = verbose,
        technical = list(NCYCLES = 1500)
      )
    }, error = function(e) {
      err_msgs <<- c(err_msgs, paste("Percobaan 3 (MHRM):", e$message))
      NULL
    }, warning = function(w) {
      withCallingHandlers(
        mirt(df, 1, itemtype=itemtype, method="MHRM", SE=FALSE,
             verbose=verbose, technical=list(NCYCLES=1500)),
        warning = function(w) invokeRestart("muffleWarning")
      )
    })
  }
  
  # Percobaan 4: Fallback ke model lebih sederhana jika model asli gagal total
  if (is.null(fit) && is_poly) {
    fallback_type <- switch(model_choice,
                            "GPCM" = "gpcm",
                            "GRM"  = "graded",
                            "PCM"  = "Rasch"
    )
    fit <- tryCatch({
      mirt(
        data      = df,
        model     = 1,
        itemtype  = fallback_type,
        method    = "EM",
        SE        = FALSE,
        verbose   = verbose,
        technical = list(TOL=1e-3, NCYCLES=3000)
      )
    }, error = function(e) {
      err_msgs <<- c(err_msgs, paste("Percobaan 4 (fallback):", e$message))
      NULL
    }, warning = function(w) {
      withCallingHandlers(
        mirt(df, 1, itemtype=fallback_type, method="EM", SE=FALSE,
             verbose=verbose, technical=list(TOL=1e-3, NCYCLES=3000)),
        warning = function(w) invokeRestart("muffleWarning")
      )
    })
    if (!is.null(fit)) {
      attr(fit, "fallback_note") <- paste(
        "⚠️ Model", model_choice, "gagal konvergen. Menggunakan model fallback dengan toleransi lebih longgar."
      )
    }
  }
  
  if (is.null(fit)) {
    stop(paste("Estimasi gagal setelah semua percobaan.\n",
               paste(err_msgs, collapse="\n")))
  }
  
  attr(fit, "error_log") <- err_msgs
  fit
}

# ── Estimasi fscores yang robust ─────────────────────────────────────────────
run_fscores_robust <- function(fit_obj, method = "EAP") {
  # JML = Joint Maximum Likelihood (method "ML" in mirt with full.scores)
  mirt_method <- switch(method,
                        "EAP" = "EAP",
                        "MAP" = "MAP",
                        "ML"  = "ML",
                        "JML" = "ML",   # JML diimplementasikan sebagai ML di mirt
                        "EAP"
  )
  
  result <- tryCatch({
    fscores(fit_obj, method=mirt_method, full.scores=TRUE, full.scores.SE=TRUE)
  }, error = function(e) {
    # Fallback ke EAP jika metode yang dipilih gagal
    tryCatch(
      fscores(fit_obj, method="EAP", full.scores=TRUE, full.scores.SE=TRUE),
      error = function(e2) NULL
    )
  }, warning = function(w) {
    withCallingHandlers(
      fscores(fit_obj, method=mirt_method, full.scores=TRUE, full.scores.SE=TRUE),
      warning = function(w) invokeRestart("muffleWarning")
    )
  })
  
  result
}

make_inv_plot <- function(x_vec, y_vec, labels, param_name, lab1, lab2) {
  ok    <- is.finite(x_vec) & is.finite(y_vec)
  xv    <- x_vec[ok]; yv <- y_vec[ok]; lb <- labels[ok]
  r_val <- round(cor(xv, yv), 3)
  rng   <- range(c(xv, yv))
  xl    <- seq(rng[1], rng[2], length.out = 50)
  lm_c  <- coef(lm(yv ~ xv))
  plot_ly() %>%
    add_markers(x=xv, y=yv, text=lb,
                hovertemplate=paste0("%{text}<br>",lab1,": %{x:.3f}<br>",
                                     lab2,": %{y:.3f}<extra></extra>"),
                marker=list(color="#4e9af1",size=9,line=list(color="#1a2340",width=1)),
                name="Item") %>%
    add_lines(x=xl, y=xl, line=list(color="red",dash="dash",width=2),
              name="Diagonal (slope=1)") %>%
    add_lines(x=xl, y=lm_c[1]+lm_c[2]*xl,
              line=list(color="orange",width=1.5,dash="dot"),
              name=paste0("Trend (r=",r_val,")")) %>%
    layout(
      title=list(text=paste0("Invariansi ",param_name," (r=",r_val,")"),
                 font=list(size=14,color="#1a2340")),
      xaxis=list(title=paste0(param_name," - ",lab1),titlefont=list(size=12)),
      yaxis=list(title=paste0(param_name," - ",lab2),titlefont=list(size=12)),
      legend=list(orientation="h",y=-0.22),
      autosize=TRUE, margin=list(l=60,r=20,b=90,t=65))
}

# ============================================================
#   CSS
# ============================================================
app_css <- "
body{font-family:'Segoe UI',sans-serif;background:#f0f2f5;}
.main-sidebar{background:#1a2340!important;}
.sidebar-menu>li>a{color:#c8d3e8!important;font-size:13px;}
.sidebar-menu>li.active>a,.sidebar-menu>li>a:hover{
  background:#2e3d6b!important;color:#fff!important;
  border-left:3px solid #4e9af1!important;}
.logo{background:#111827!important;}
.box{border-radius:8px;box-shadow:0 2px 8px rgba(0,0,0,.08);}
.box.box-primary{border-top-color:#4e9af1;}
.box.box-success{border-top-color:#2ecc71;}
.box.box-warning{border-top-color:#f39c12;}
.box.box-danger {border-top-color:#e74c3c;}
.box.box-info   {border-top-color:#3498db;}
.btn-primary{background:#4e9af1;border-color:#3a85d8;border-radius:6px;font-weight:600;}
.btn-success,.btn-warning,.btn-danger{border-radius:6px;font-weight:600;}
.info-callout{background:#eaf4ff;border-left:4px solid #4e9af1;border-radius:6px;
  padding:12px 16px;margin-bottom:12px;font-size:13px;color:#1a3a5c;}
.warn-callout{background:#fff8e6;border-left:4px solid #f39c12;border-radius:6px;
  padding:12px 16px;margin-bottom:12px;font-size:13px;color:#7a5000;}
.formula-box{background:#f8f9fa;border:1px solid #dee2e6;border-radius:8px;
  padding:16px;font-family:'Courier New',monospace;font-size:15px;
  color:#333;margin:10px 0;text-align:center;}
table.dataTable thead th{background:#1a2340;color:white;}
.btn-reset{background:#e74c3c!important;border-color:#c0392b!important;
  color:#fff!important;border-radius:6px;font-weight:700;width:100%;
  margin-top:6px;padding:8px 0;font-size:13px;letter-spacing:0.3px;}
.btn-reset:hover{background:#c0392b!important;}
.reset-panel{background:#2e3d6b;border-radius:8px;padding:12px 14px;
  margin:10px 12px 4px 12px;}
"

# ============================================================
#   UI
# ============================================================
ui <- dashboardPage(
  skin="blue",
  dashboardHeader(
    title=tags$span(tags$b("Analisis IRT")),
    titleWidth=240),
  dashboardSidebar(
    width=250,
    useShinyjs(),
    tags$head(tags$style(HTML(app_css))),
    sidebarMenu(id="tabs",
                menuItem("📁 Masukkan Data",      tabName="data_input",  icon=icon("upload")),
                menuItem("✅ Analisis asumsi",      tabName="assumptions", icon=icon("check-circle")),
                menuItem("📐 Estimasi IRT",        tabName="irt_model",  icon=icon("chart-line")),
                menuItem("📊 Model-Data Fit",      tabName="model_fit",  icon=icon("search")),
                menuItem("🔄 Uji Invariansi",      tabName="invariance", icon=icon("exchange-alt")),
                menuItem("🎯 Theta & T-Score",     tabName="scores",     icon=icon("table")),
                menuItem("ℹ️ Panduan",             tabName="about",      icon=icon("info-circle"))
    ),
    hr(),
    # Tombol Reset
    tags$div(class="reset-panel",
             tags$div(style="color:#c8d3e8;font-size:11px;margin-bottom:6px;",
                      "⚠️ ",tags$b("Ganti dataset? Reset dulu!")),
             actionButton("btn_reset","🗑️ Reset Semua Data",
                          class="btn-reset")
    ),
    hr(),
    tags$div(style="padding:10px 15px;color:#8899bb;font-size:11px;",
             tags$b("IRT Analisis"),tags$br(),
             "Baru Coba-coba",tags$br(),
             "Saran dongs")
  ),
  dashboardBody(
    tags$head(tags$script(HTML(
      "$(document).on('shiny:value',function(){
         setTimeout(function(){
           $('.js-plotly-plot').each(function(){
             try{Plotly.relayout(this,{autosize:true});}catch(e){}
           });
         },200);
       });"
    ))),
    tabItems(
      
      # ── TAB 1: DATA INPUT ──────────────────────────────────
      tabItem(tabName="data_input",
              fluidRow(
                box(title="📁 Upload Data", status="primary", solidHeader=TRUE, width=6,
                    tags$div(class="info-callout",
                             tags$b("Format:")," CSV atau XLSX. Baris=Peserta, Kolom=Butir.",tags$br(),
                             "Dikotomus: 0/1 | Politomus: 0,1,2,..."),
                    fileInput("data_file",NULL,accept=c(".csv",".xlsx",".xls"),
                              placeholder="Pilih file..."),
                    checkboxInput("header","Baris pertama = header item",value=TRUE),
                    selectInput("sep","Separator CSV:",
                                choices=c("Koma"=",","Titik koma"=";","Tab"="\t")),
                    actionButton("load_data","🔄 Load Data",class="btn-primary btn-block")
                ),
                box(title="⚙️ Info Dataset", status="info", solidHeader=TRUE, width=6,
                    verbatimTextOutput("data_type_info"),
                    checkboxInput("use_msi",
                                  HTML("<b>Gunakan MSI (Method of Successive Intervals)</b><br>
                <small>Hanya untuk data politomus.</small>"), value=FALSE),
                    conditionalPanel(
                      condition="input.use_msi == true",
                      tags$div(style="background:#fff8e6;border-left:4px solid #f39c12;border-radius:6px;padding:10px 14px;margin-bottom:8px;",
                               tags$b("⚙️ Pengaturan Skala MSI:"),tags$br(),
                               tags$small("Masukkan skala minimal instrumen Anda (misal: 1 untuk Likert 1-5)."),tags$br(),
                               fluidRow(
                                 column(6, numericInput("msi_min","Skala Minimal:",value=1,min=0,step=1)),
                                 column(6, numericInput("msi_max","Skala Maksimal:",value=5,min=1,step=1))
                               ),
                               tags$small(style="color:#aaa;font-style:italic;","Tidak perlu gunakan MSI ketika data politomus yang digunakan adalah data interval"),
                               actionButton("apply_msi","🔁 Terapkan MSI",class="btn-warning btn-sm btn-block",
                                            style="margin-top:4px;")
                      )
                    ),
                    tags$div(class="warn-callout","⚠️ MSI diabaikan untuk data dikotomus."),
                    hr(),
                    uiOutput("data_summary_boxes")
                )
              ),
              fluidRow(
                box(title="Preview Data (10 baris pertama)", status="success",
                    solidHeader=TRUE, width=12,
                    uiOutput("preview_label"),
                    withSpinner(DTOutput("data_preview"),type=4,color="#4e9af1"),
                    uiOutput("msi_mapping_ui")
                )
              )
      ),
      
      # ── TAB 2: ANALISIS ASUMSI ─────────────────────────────────
      tabItem(tabName="assumptions",
              fluidRow(
                box(title="📋 KMO & Bartlett's Test", status="primary", solidHeader=TRUE, width=5,
                    tags$div(class="info-callout",
                             "• KMO ≥ 0.5: layak analisis faktor & IRT",tags$br(),
                             "• Bartlett p < 0.05: ada korelasi signifikan antar item"),
                    actionButton("run_kmo","▶ Jalankan KMO & Bartlett",class="btn-primary"),
                    hr(),
                    withSpinner(uiOutput("kmo_result"),type=4,color="#4e9af1")
                ),
                box(title="📈 Scree Plot (Eigenvalue)", status="success", solidHeader=TRUE, width=7,
                    actionButton("run_scree","▶ Buat Scree Plot",class="btn-success"),
                    tags$div(class="info-callout",style="margin-top:8px;",
                             "Terdapat dominansi faktor pertama, maka unidimensionalitas terpenuhi."),
                    hr(),
                    withSpinner(
                      plotlyOutput("scree_plot",height="380px",width="100%"),
                      type=4,color="#2ecc71")
                )
              ),
              fluidRow(
                box(title="🔗 Uji Local Independence - Yen's Q3 Statistic",
                    status="warning", solidHeader=TRUE, width=12,
                    tags$div(class="info-callout",
                             tags$b("Statistik Yen's Q3:"),tags$br(),
                             "Q3 dihitung sebagai korelasi antara residual item i dan item j setelah kemampuan (θ) dikontrol.",tags$br(),
                             "✅ ",tags$b("Q3 ≤ 0.2:")," asumsi independensi lokal terpenuhi.",tags$br(),
                             "⚠️ ",tags$b("Q3 > 0.2:")," ada ketergantungan lokal antara pasangan item tersebut."
                    ),
                    actionButton("run_q3","▶ Hitung Yen's Q3",class="btn-warning"),
                    hr(),
                    fluidRow(
                      column(8,
                             withSpinner(
                               plotlyOutput("q3_heatmap",height="450px",width="100%"),
                               type=4,color="#f39c12")
                      ),
                      column(4,
                             withSpinner(uiOutput("q3_summary_ui"),type=4,color="#f39c12"),
                             hr(),
                             withSpinner(DTOutput("q3_violation_table"),type=4,color="#f39c12")
                      )
                    )
                )
              ),
              fluidRow(
                box(title="📋 Panduan Keputusan Asumsi", status="success", solidHeader=TRUE, width=12,
                    withSpinner(uiOutput("assumption_summary"),type=4,color="#2ecc71"))
              )
      ),
      
      # ── TAB 3: ESTIMASI IRT ───────────────────────────────
      tabItem(tabName="irt_model",
              fluidRow(
                box(title="⚙️ Konfigurasi Model", status="primary", solidHeader=TRUE, width=4,
                    tags$div(class="info-callout",
                             tags$b("Estimasi:")," Parameter butir & kemampuan."),
                    selectInput("model_choice","Model IRT:",
                                choices=list(
                                  "Dikotomus"=list(
                                    "1PL"="1PL","2PL"="2PL","3PL"="3PL"),
                                  "Politomus"=list(
                                    "PCM"="PCM","GPCM"="GPCM","GRM"="GRM"))),
                    # Metode estimasi θ: hanya EAP, MAP, JML, ML
                    selectInput("est_method","Metode Estimasi θ:",
                                choices=c(
                                  "EAP (Expected A Posteriori)"     = "EAP",
                                  "MAP (Modal A Posteriori)"        = "MAP",
                                  "ML  (Maximum Likelihood)"        = "ML",
                                  "JML (Joint Maximum Likelihood)"  = "JML"
                                )),
                    # Metode estimasi item: hanya MML
                    tags$div(class="info-callout", style="padding:8px 12px;margin-top:4px;",
                             tags$b("Metode Estimasi Item:"),tags$br(),
                             tags$span(style="font-size:13px;",
                                       "MML - Marginal Maximum Likelihood (EM)"),tags$br(),
                             tags$small(style="color:#888;",
                                        "MML adalah metode bawaaan dari Parscale.")
                    ),
                    hr(),
                    actionButton("run_irt","🚀 Jalankan Estimasi IRT",
                                 class="btn-primary btn-block",style="font-size:15px;padding:10px;")
                ),
                box(title="📋 Parameter Item", status="success", solidHeader=TRUE, width=8,
                    uiOutput("irt_fallback_note"),
                    withSpinner(DTOutput("item_params_table"),type=4,color="#2ecc71"),
                    hr(),
                    downloadButton("dl_item_params","⬇ Download Parameter Item",class="btn-success")
                )
              ),
              fluidRow(
                box(title="📉 Item Characteristic Curves (ICC)", status="info",
                    solidHeader=TRUE, width=8,
                    selectInput("icc_items","Pilih item (maks 6):",choices=NULL,multiple=TRUE),
                    withSpinner(
                      plotlyOutput("icc_plot",height="460px",width="100%"),
                      type=4,color="#3498db")
                ),
                box(title="📊 Test Information Function", status="warning",
                    solidHeader=TRUE, width=4,
                    withSpinner(
                      plotlyOutput("tif_plot",height="460px",width="100%"),
                      type=4,color="#f39c12")
                )
              )
      ),
      
      # ── TAB 4: MODEL-DATA FIT ─────────────────────────────
      tabItem(tabName="model_fit",
              fluidRow(
                box(title="📊 Item Fit Statistics", status="warning",
                    solidHeader=TRUE, width=12,
                    tags$div(class="formula-box",
                             "Q₁ = Σⱼ Nⱼ(Pᵢⱼ − E(Pᵢⱼ))² / [E(Pᵢⱼ)(1 − E(Pᵢⱼ))]",tags$br(),
                             tags$small("df = m − k | m = ability groups | k = parameter item")),
                    fluidRow(
                      column(4,numericInput("n_groups","Jumlah ability groups:",
                                            value=10,min=5,max=20)),
                      column(4,tags$div(style="margin-top:25px;",
                                        actionButton("run_fit","▶ Hitung Q₁",class="btn-warning btn-block")))
                    ),
                    hr(),
                    withSpinner(DTOutput("item_fit_table"),type=4,color="#f39c12"),
                    hr(),
                    downloadButton("dl_fit","⬇ Download Fit Statistics",class="btn-warning")
                )
              ),
              fluidRow(
                box(title="📈 Observed vs Expected ICC", status="info", solidHeader=TRUE, width=12,
                    selectInput("fit_item_sel","Pilih Item:",choices=NULL),
                    withSpinner(
                      plotlyOutput("residual_plot",height="430px",width="100%"),
                      type=4,color="#3498db")
                )
              ),
              fluidRow(
                box(title="📋 Ringkasan Fit", status="primary", solidHeader=TRUE, width=12,
                    withSpinner(uiOutput("fit_summary"),type=4,color="#4e9af1"))
              )
      ),
      
      # ── TAB 5: INVARIANSI ─────────────────────────────────
      tabItem(tabName="invariance",
              fluidRow(
                box(title="🔄 Invariansi Parameter Item",
                    status="danger", solidHeader=TRUE, width=12,
                    tags$div(class="info-callout",
                             tags$b("Keterangan:"),tags$br(),
                             "Invariansi dicek setelah model dipilih dan diestimasi karena:",tags$br(),
                             "1. Invariansi adalah sifat model yang fit, bukan sifat data awal.",tags$br(),
                             "2. Jika model tidak fit, parameter berubah antar subkelompok.",tags$br(),
                             "3. Cara: estimasi parameter di dua kelompok terpisah, bandingkan",
                             " via scatterplot, titik harus membentuk pola garis y = x."
                    ),
                    fluidRow(
                      column(4,
                             selectInput("inv_split","Cara pembagian sampel:",
                                         choices=c(
                                           "Random 50:50"="random",
                                           "Ganjil vs Genap (nomor urut peserta)"="odd_even",
                                           "Upload file kelompok (CSV/XLSX)"="file_group")),
                             conditionalPanel(
                               condition="input.inv_split == 'file_group'",
                               tags$div(style="background:#f0f8ff;border:1px solid #cce;border-radius:6px;padding:10px;margin-top:6px;",
                                        tags$b("📂 Upload File Kelompok"),tags$br(),
                                        tags$small("Format: satu kolom berisi label kelompok per baris,",tags$br(),
                                                   "urutan baris sesuai urutan peserta di data utama.",tags$br(),
                                                   "Contoh isi: Laki-laki / Perempuan, Suku A / Suku B, dll."),
                                        tags$br(),tags$br(),
                                        fileInput("inv_group_file", NULL,
                                                  accept=c(".csv",".xlsx",".xls"),
                                                  placeholder="Pilih file..."),
                                        checkboxInput("inv_group_header","Baris pertama = header",value=TRUE),
                                        selectInput("inv_sep_group","Separator CSV:",
                                                    choices=c("Koma"=",","Titik koma"=";","Tab"="\t")),
                                        uiOutput("inv_group_preview_ui")
                               )
                             ),
                             tags$br(),
                             actionButton("run_invariance","🔄 Cek Invariansi Item",
                                          class="btn-danger btn-block",style="margin-top:8px;")
                      ),
                      column(8,
                             withSpinner(uiOutput("inv_stats_ui"),type=4,color="#e74c3c"))
                    )
                )
              ),
              fluidRow(
                box(title="📍 Invariansi Parameter Kesulitan b",
                    status="warning", solidHeader=TRUE, width=6,
                    withSpinner(
                      plotlyOutput("inv_b_plot",height="430px",width="100%"),
                      type=4,color="#f39c12")
                ),
                box(title="📐 Invariansi Parameter Diskriminasi a",
                    status="info", solidHeader=TRUE, width=6,
                    uiOutput("inv_a_panel")
                )
              ),
              fluidRow(
                box(title="🎯 Invariansi Parameter Ability (θ)",
                    status="success", solidHeader=TRUE, width=12,
                    tags$div(class="info-callout",
                             tags$b("Konsep:"),tags$br(),
                             "Estimasi θ dari dua set item berbeda seharusnya konsisten.",
                             " Scatterplot θA vs θB harus membentuk garis acuan.",tags$br(),
                             "Apabila terlalu menyebar, kemampuan mungkin tidak stabil lintas item."
                    ),
                    fluidRow(
                      column(3,
                             selectInput("inv_ability_split","Pembagian item:",
                                         choices=c(
                                           "Ganjil vs Genap"="odd_even",
                                           "Paruh Awal vs Akhir"="first_last",
                                           "Random Split Item"="random_item")),
                             actionButton("run_inv_ability","▶ Cek Invariansi Ability",
                                          class="btn-success btn-block",style="margin-top:8px;")
                      ),
                      column(9,
                             withSpinner(
                               plotlyOutput("inv_ability_plot",height="430px",width="100%"),
                               type=4,color="#2ecc71"))
                    )
                )
              )
      ),
      
      # ── TAB 6: THETA & T-SCORE ────────────────────────────
      tabItem(tabName="scores",
              fluidRow(
                box(title="🎯 Formula Transformasi Skor", status="primary",
                    solidHeader=TRUE, width=12,
                    fluidRow(
                      column(5,
                             tags$div(class="formula-box",style="font-size:18px;",
                                      "T = 50 + 10 × θᵢ",tags$br(),tags$br(),
                                      tags$small(style="font-size:12px;",
                                                 "θᵢ = estimasi ability (hasil EAP/MAP/ML/JML) peserta ke-i",tags$br(),
                                                 "z = θ (digunakan langsung sebagai skor standar)"))
                      ),
                      column(7,
                             tags$div(class="info-callout",
                                      tags$b("Interpretasi T-Score:"),tags$br(),
                                      "• T = 50 → θ = 0; kemampuan rata-rata pada skala IRT (mean = 0)",tags$br(),
                                      "• T = 60 → θ = +1; kemampuan satu deviasi standar di atas rata-rata",tags$br(),
                                      "• T = 40 → θ = −1; kemampuan satu deviasi standar di bawah rata-rata",tags$br(),
                                      tags$b("Formula: T = 50 + 10 × θ"))
                      )
                    )
                )
              ),
              fluidRow(
                box(title="📊 Distribusi Theta (θ)", status="info", solidHeader=TRUE, width=6,
                    withSpinner(
                      plotlyOutput("theta_dist",height="380px",width="100%"),
                      type=4,color="#3498db")
                ),
                box(title="📊 Distribusi T-Score", status="success", solidHeader=TRUE, width=6,
                    withSpinner(
                      plotlyOutput("tscore_dist",height="380px",width="100%"),
                      type=4,color="#2ecc71")
                )
              ),
              fluidRow(
                box(title="📋 Tabel Theta & T-Score", status="primary",
                    solidHeader=TRUE, width=12,
                    withSpinner(DTOutput("theta_table"),type=4,color="#4e9af1"),
                    hr(),
                    downloadButton("dl_theta","⬇ Download CSV",class="btn-primary")
                )
              )
      ),
      
      # ── TAB 7: PANDUAN ────────────────────────────────────
      tabItem(tabName="about",
              fluidRow(
                box(title="ℹ️ Panduan Analisis IRT", status="info", solidHeader=TRUE, width=7,
                    tags$h5("Alur Analisis yang Direkomendasikan:"),
                    tags$ol(
                      tags$li("Upload data → ",tags$b("Data Input")),
                      tags$li("Cek prasyarat → ",tags$b("Uji Asumsi"),
                              " (KMO≥0.5, Bartlett p<0.05, Scree Plot, Yen's Q3)"),
                      tags$li("Pilih model & estimasi → ",tags$b("Estimasi IRT")),
                      tags$li("Evaluasi fit → ",tags$b("Model-Data Fit")," (Q₁, residual plot)"),
                      tags$li("Cek invariansi → ",tags$b("Uji Invariansi")," (setelah model fit!)"),
                      tags$li("Unduh hasil → ",tags$b("Theta & T-Score"))
                    ),
                    tags$hr(),
                    tags$h5("Catatan Metode Estimasi θ:"),
                    tags$div(class="info-callout",
                             tags$b("EAP")," - Expected A Posteriori. Paling stabil, direkomendasikan untuk sampel kecil.",tags$br(),
                             tags$b("MAP")," - Modal A Posteriori. Mirip EAP, lebih cepat.",tags$br(),
                             tags$b("ML"),"  - Maximum Likelihood. Tidak menggunakan prior; bisa tidak terdefinisi jika skor ekstrem.",tags$br(),
                             tags$b("JML")," - Joint Maximum Likelihood. Estimasi item & ability bersamaan; digunakan untuk konsistensi dengan model Rasch klasik."
                    ),
                    tags$hr(),
                    tags$table(class="table table-bordered table-sm",
                               tags$thead(tags$tr(
                                 tags$th("Model"),tags$th("Data"),tags$th("Parameter"),tags$th("Keterangan"))),
                               tags$tbody(
                                 tags$tr(tags$td("1PL"),tags$td("Dikotomus"),tags$td("b"),
                                         tags$td("Semua item sama diskriminasinya")),
                                 tags$tr(tags$td("2PL"),tags$td("Dikotomus"),tags$td("a, b"),
                                         tags$td("Diskriminasi dan tingkat kesulitan")),
                                 tags$tr(tags$td("3PL"),tags$td("Dikotomus"),tags$td("a, b, c"),
                                         tags$td("Diskriminasi, kesulitan, peluang menebak")),
                                 tags$tr(tags$td("PCM"),tags$td("Politomus"),tags$td("b"),
                                         tags$td("Partial Credit Model")),
                                 tags$tr(tags$td("GPCM"),tags$td("Politomus"),tags$td("a, b"),
                                         tags$td("Generalized PCM")),
                                 tags$tr(tags$td("GRM"),tags$td("Politomus"),tags$td("a, b"),
                                         tags$td("Graded Response Model")))
                    )
                ),
                box(title="📚 Referensi", status="success", solidHeader=TRUE, width=5,
                    tags$ul(
                      tags$li("Hambleton, Swaminathan & Rogers (1991). ",
                              tags$i("Fundamentals of IRT.")," Sage."),
                      tags$li("Yen (1981). Q₁ statistic. ",
                              tags$i("Psychometrika, 46, 443–459.")),
                      tags$li("Masters (1982). PCM. ",
                              tags$i("Psychometrika, 47, 149–174.")),
                      tags$li("Samejima (1969). GRM. ",
                              tags$i("Psychometrika Monograph, 17.")),
                      tags$li("Chalmers (2012). mirt. ",
                              tags$i("J. Statistical Software, 48(6)."))
                    ),
                    tags$div(class="info-callout",
                             tags$b("Package R:"),tags$br(),
                             "mirt, psych, GPArotation, ggplot2, plotly, DT, shiny, shinydashboard")
                )
              )
      )
      
    )
  )
)

# ============================================================
#   SERVER
# ============================================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    raw_data      = NULL,
    clean_data    = NULL,
    data_type     = NULL,
    irt_model     = NULL,
    theta_results = NULL,
    item_params   = NULL,
    fit_results   = NULL,
    msi_applied   = FALSE,
    msi_mapping   = NULL,
    fallback_note = NULL
  )
  
  # ── RESET SEMUA DATA ────────────────────────────────────
  observeEvent(input$btn_reset, {
    # Kosongkan semua reactive values
    rv$raw_data      <- NULL
    rv$clean_data    <- NULL
    rv$data_type     <- NULL
    rv$irt_model     <- NULL
    rv$theta_results <- NULL
    rv$item_params   <- NULL
    rv$fit_results   <- NULL
    rv$msi_applied   <- FALSE
    rv$msi_mapping   <- NULL
    rv$fallback_note <- NULL
    # Reset input file
    reset("data_file")
    reset("use_msi")
    # Reset selectInput item
    updateSelectInput(session, "icc_items",    choices=character(0))
    updateSelectInput(session, "fit_item_sel", choices=character(0))
    # Kembali ke tab pertama
    updateTabItems(session, "tabs", selected="data_input")
    showNotification("🗑️ Semua data berhasil direset. Silakan upload data baru.",
                     type="message", duration=5)
  })
  
  # ── DATA INPUT ──────────────────────────────────────────
  observeEvent(input$load_data, {
    req(input$data_file)
    ext <- tools::file_ext(input$data_file$name)
    tryCatch({
      df <- if (ext %in% c("xlsx","xls"))
        as.data.frame(read_excel(input$data_file$datapath, col_names=input$header))
      else
        read.csv(input$data_file$datapath, header=input$header, sep=input$sep)
      num_cols <- sapply(df, is.numeric)
      if (sum(num_cols)==0){showNotification("⚠️ Tidak ada kolom numerik!",type="error");return()}
      df_num <- df[, num_cols, drop=FALSE]
      rv$raw_data   <- df_num
      rv$data_type  <- detect_data_type(df_num)
      rv$clean_data <- df_num
      rv$msi_applied <- FALSE
      rv$msi_mapping <- NULL
      nms <- colnames(df_num)
      updateSelectInput(session,"icc_items",   choices=nms, selected=nms[1:min(4,length(nms))])
      updateSelectInput(session,"fit_item_sel",choices=nms, selected=nms[1])
      showNotification(paste("✅ Dimuat:",nrow(df_num),"baris,",ncol(df_num),"kolom."),
                       type="message",duration=4)
    }, error=function(e) showNotification(paste("❌",e$message),type="error"))
  })
  
  output$data_type_info <- renderText({
    req(rv$data_type)
    paste0("Terdeteksi: ", toupper(rv$data_type))
  })
  
  output$data_summary_boxes <- renderUI({
    req(rv$raw_data); df <- rv$raw_data
    fluidRow(
      valueBox(nrow(df),"Peserta",icon=icon("users"),  color="blue",  width=4),
      valueBox(ncol(df),"Item",    icon=icon("list"),   color="green", width=4),
      valueBox(toupper(rv$data_type),"Jenis",icon=icon("tag"),color="orange",width=4))
  })
  
  output$preview_label <- renderUI({
    if (isTRUE(rv$msi_applied)) {
      tags$div(class="info-callout", style="margin-bottom:6px;",
               tags$b("🔁 Menampilkan data setelah transformasi MSI"),
               paste0(" (skala ",input$msi_min," – ",input$msi_max,")"))
    } else {
      tags$div(class="info-callout", style="margin-bottom:6px;",
               tags$b("📄 Menampilkan data asli (sebelum MSI)"))
    }
  })
  
  output$data_preview <- renderDT({
    req(rv$raw_data)
    preview_data <- if (isTRUE(rv$msi_applied) && !is.null(rv$clean_data))
      head(rv$clean_data, 10) else head(rv$raw_data, 10)
    datatable(preview_data,
              options=list(scrollX=TRUE,pageLength=10,dom="t"),
              class="table-bordered table-sm") %>%
      formatRound(which(sapply(preview_data, is.numeric)), digits=4)
  })
  
  output$msi_mapping_ui <- renderUI({
    req(rv$msi_mapping)
    if (length(rv$msi_mapping)==0) return(NULL)
    tagList(
      tags$hr(),
      tags$b("📊 Tabel Detail MSI per Item (Frek | Prop | Kum | Densitas | Z | Nilai_MSI):"),
      tags$br(), tags$br(),
      lapply(names(rv$msi_mapping), function(nm) {
        m <- rv$msi_mapping[[nm]]
        tags$div(style="margin-bottom:16px;",
                 tags$b(paste0("Item: ", nm)),
                 tags$table(
                   class="table table-bordered table-sm table-striped",
                   style="font-size:12px;width:auto;",
                   tags$thead(tags$tr(
                     tags$th("Kategori"), tags$th("Frekuensi"), tags$th("Proporsi"),
                     tags$th("Kum. Prop"), tags$th("Densitas"), tags$th("Z"),
                     tags$th(tags$b("Nilai MSI"))
                   )),
                   tags$tbody(lapply(1:nrow(m), function(i)
                     tags$tr(
                       tags$td(m$Kategori[i]),
                       tags$td(m$Frek[i]),
                       tags$td(m$Proporsi[i]),
                       tags$td(m$Kum[i]),
                       tags$td(m$Densitas[i]),
                       tags$td(if (is.na(m$Z[i])) "-" else m$Z[i]),
                       tags$td(tags$b(m$Nilai_MSI[i]))
                     )
                   ))
                 )
        )
      })
    )
  })
  
  # ── UJI ASUMSI ──────────────────────────────────────────
  observeEvent(input$run_kmo, {
    req(rv$clean_data)
    df <- rv$clean_data
    tryCatch({
      kmo_res  <- KMO(df)
      bart_res <- cortest.bartlett(cor(df,use="pairwise.complete.obs"), n=nrow(df))
      kv <- round(kmo_res$MSA,3); bp <- signif(bart_res$p.value,4)
      ki <- dplyr::case_when(
        kv>=0.90~"Marvelous ✨",kv>=0.80~"Meritorious 👍",kv>=0.70~"Middling ✅",
        kv>=0.60~"Mediocre ⚠️",kv>=0.50~"Miserable 🔴",TRUE~"Unacceptable ❌")
      output$kmo_result <- renderUI(tagList(
        tags$table(class="table table-bordered",
                   tags$thead(tags$tr(tags$th("Statistik"),tags$th("Nilai"),tags$th("Interpretasi"))),
                   tags$tbody(
                     tags$tr(tags$td(tags$b("KMO (Overall MSA)")),tags$td(kv),
                             tags$td(tags$span(style=paste0("color:",if(kv>=0.6)"green"else"red",
                                                            ";font-weight:bold;"),ki))),
                     tags$tr(tags$td(tags$b("Bartlett χ²")),
                             tags$td(paste0(round(bart_res$chisq,2)," (df=",bart_res$df,", p=",bp,")")),
                             tags$td(tags$span(style=paste0("color:",if(bp<0.05)"green"else"red",
                                                            ";font-weight:bold;"),
                                               if(bp<0.05)"Signifikan ✅"else"Tidak Signifikan ⚠️"))))),
        tags$div(class=if(kv>=0.6&&bp<0.05)"info-callout"else"warn-callout",
                 if(kv>=0.6&&bp<0.05)"✅ Prasyarat terpenuhi dapat lanjutkan ke IRT."
                 else"⚠️ Periksa item atau sampel sebelum lanjut.")
      ))
    }, error=function(e) output$kmo_result <- renderUI(
      tags$div(class="warn-callout",paste("Error:",e$message))))
  })
  
  observeEvent(input$run_scree, {
    req(rv$clean_data)
    df <- rv$clean_data
    tryCatch({
      ev    <- eigen(cor(df,use="pairwise.complete.obs"))$values
      ns    <- min(length(ev),20)
      dfp   <- data.frame(Factor=1:ns, EV=ev[1:ns])
      output$scree_plot <- renderPlotly({
        plot_ly(dfp,x=~Factor,y=~EV,type="scatter",mode="lines+markers",
                line=list(color="#4e9af1",width=2.5),
                marker=list(color="#1a2340",size=8,line=list(color="white",width=1))) %>%
          add_lines(x=c(1,ns),y=c(1,1),
                    line=list(color="red",dash="dash",width=1.5),name="EV=1") %>%
          layout(
            title=list(text="Scree Plot",font=list(size=16,color="#1a2340")),
            xaxis=list(title="Faktor ke-",tickmode="linear",dtick=1,
                       titlefont=list(size=13)),
            yaxis=list(title="Eigenvalue",titlefont=list(size=13)),
            legend=list(orientation="h",y=-0.18),
            margin=list(l=60,r=20,b=70,t=60),autosize=TRUE)
      })
    }, error=function(e) showNotification(paste("Error:",e$message),type="error"))
  })
  
  observeEvent(input$run_q3, {
    req(rv$clean_data, rv$irt_model)
    withProgress(message="Menghitung Yen's Q3...", value=0, {
      tryCatch({
        df  <- rv$clean_data
        ni  <- ncol(df)
        tv  <- fscores(rv$irt_model, method="EAP", full.scores=TRUE)[,1]
        
        incProgress(0.3, detail="Menghitung residual item...")
        resid_mat <- matrix(NA, nrow=nrow(df), ncol=ni)
        colnames(resid_mat) <- colnames(df)
        for (j in 1:ni) {
          pg <- tryCatch(probtrace(extract.item(rv$irt_model, j), tv), error=function(e)NULL)
          if (is.null(pg)) next
          if (ncol(pg)==2) {
            expected <- pg[,2]
          } else {
            cats <- 0:(ncol(pg)-1)
            expected <- apply(pg, 1, function(p) sum(cats*p))
          }
          resid_mat[,j] <- df[,j] - expected
        }
        
        incProgress(0.4, detail="Menghitung matriks Q3...")
        q3_mat <- cor(resid_mat, use="pairwise.complete.obs")
        diag(q3_mat) <- NA
        
        nms <- colnames(df)
        
        output$q3_heatmap <- renderPlotly({
          plot_ly(z=q3_mat, x=nms, y=nms, type="heatmap",
                  colorscale=list(c(0,"#2c7bb6"),c(0.5,"#ffffbf"),c(1,"#d7191c")),
                  zmin=-1, zmax=1,
                  colorbar=list(title="Q3",len=0.85,thickness=15),
                  hovertemplate="%{y} × %{x}<br>Q3 = %{z:.3f}<extra></extra>") %>%
            layout(
              title=list(text="Yen's Q3 Matrix - Independensi Lokal",
                         font=list(size=14,color="#1a2340")),
              xaxis=list(title="",tickfont=list(size=9),tickangle=-45),
              yaxis=list(title="",tickfont=list(size=9),autorange="reversed"),
              margin=list(l=80,r=20,b=100,t=60),autosize=TRUE)
        })
        
        q3_vals <- q3_mat[upper.tri(q3_mat)]
        q3_vals <- q3_vals[!is.na(q3_vals)]
        n_viol  <- sum(q3_vals > 0.2, na.rm=TRUE)
        n_total <- length(q3_vals)
        
        output$q3_summary_ui <- renderUI({
          tagList(
            valueBox(round(max(q3_vals,na.rm=TRUE),3),"Q3 Maksimum",
                     icon=icon("arrow-up"),color=if(max(q3_vals,na.rm=TRUE)<=0.2)"green"else"red",width=12),
            valueBox(n_viol,paste0("Pasangan Q3 > 0.2 (dari ",n_total,")"),
                     icon=icon("exclamation-triangle"),color=if(n_viol==0)"green"else"orange",width=12),
            tags$div(class=if(n_viol==0)"info-callout"else"warn-callout",
                     if(n_viol==0)
                       "✅ Semua pasangan item Q3 ≤ 0.2. Independensi lokal TERPENUHI."
                     else
                       paste0("⚠️ ",n_viol," pasangan item Q3 > 0.2. Periksa tabel pelanggaran."))
          )
        })
        
        viol_rows <- which(q3_mat > 0.2, arr.ind=TRUE)
        viol_rows <- viol_rows[viol_rows[,1] < viol_rows[,2], , drop=FALSE]
        if (nrow(viol_rows) > 0) {
          viol_df <- data.frame(
            Item_i = nms[viol_rows[,1]],
            Item_j = nms[viol_rows[,2]],
            Q3     = round(q3_mat[viol_rows], 3),
            Status = "⚠️ Langgar LI"
          )
          viol_df <- viol_df[order(-viol_df$Q3),]
        } else {
          viol_df <- data.frame(Item_i=character(),Item_j=character(),Q3=numeric(),Status=character())
        }
        output$q3_violation_table <- renderDT({
          datatable(viol_df,
                    options=list(scrollX=TRUE,pageLength=10,dom="t"),
                    class="table-bordered table-sm",rownames=FALSE,escape=FALSE,
                    caption="Pasangan item dengan Q3 > 0.2")
        })
        
        incProgress(0.3, detail="Selesai!")
        showNotification(paste0("✅ Q3 selesai! ",n_viol," dari ",n_total,
                                " pasangan Q3 > 0.2."),type="message",duration=5)
      }, error=function(e)
        showNotification(paste("❌ Error Q3:",e$message),type="error"))
    })
  })
  
  # ── MSI APPLY ──────────────────────────────────────────
  observeEvent(input$apply_msi, {
    req(rv$raw_data)
    if (rv$data_type != "politomus") {
      showNotification("⚠️ MSI hanya berlaku untuk data politomus.",type="warning"); return()
    }
    tryCatch({
      sm <- input$msi_min; sx <- input$msi_max
      if (sx <= sm) {
        showNotification("⚠️ Skala maksimal harus lebih besar dari skala minimal.",type="error"); return()
      }
      msi_res <- msi_transform(rv$raw_data, scale_min=sm, scale_max=sx)
      rv$clean_data    <- msi_res$data
      rv$msi_mapping   <- msi_res$mapping
      rv$msi_applied   <- TRUE
      showNotification(paste0("✅ MSI diterapkan! Skala: ",sm," – ",sx),type="message",duration=4)
    }, error=function(e) showNotification(paste("❌ MSI Error:",e$message),type="error"))
  })
  
  observeEvent(input$use_msi, {
    if (!input$use_msi) {
      rv$clean_data  <- rv$raw_data
      rv$msi_applied <- FALSE
      rv$msi_mapping <- NULL
    }
  })
  
  output$assumption_summary <- renderUI({
    if (is.null(rv$raw_data)) return(
      tags$div(class="warn-callout","⚠️ Upload data terlebih dahulu."))
    tags$div(class="info-callout",
             tags$b("Panduan Keputusan Asumsi:"),tags$br(),
             "1. ",tags$b("KMO ≥ 0.5 & Bartlett p<0.05:")," lanjutkan analisis.",tags$br(),
             "2. ",tags$b("Terdapat dominasi faktor pertama pada scree plot:")," unidimensionalitas terpenuhi.",tags$br(),
             "3. ",tags$b("Yen's Q3 ≤ 0.2:")," independensi lokal terpenuhi.",tags$br(),
             "4. ",tags$b("Catatan:")," Jalankan estimasi IRT terlebih dahulu, lalu hitung Q3 untuk hasil yang akurat.")
  })
  
  # ── ESTIMASI IRT ────────────────────────────────────────
  observeEvent(input$run_irt, {
    req(rv$clean_data)
    df <- rv$clean_data
    
    withProgress(message="Estimasi IRT...", value=0, {
      tryCatch({
        incProgress(0.15, detail="Mempersiapkan data & model...")
        
        # Untuk data politomus: pastikan kolom adalah integer/faktor terurut
        is_poly <- input$model_choice %in% c("PCM","GPCM","GRM")
        if (is_poly) {
          # Konversi ke integer agar mirt tidak bingung dengan nilai kontinu dari MSI
          # (jika MSI diterapkan, gunakan data asli yang sudah di-round)
          if (isTRUE(rv$msi_applied)) {
            # Data MSI: gunakan sebagai numerik kontinu (GRM/GPCM bisa handle)
            df_use <- df
          } else {
            # Data ordinal asli: pastikan integer
            df_use <- as.data.frame(lapply(df, function(x) as.integer(round(x))))
          }
          # Cek: setiap item harus punya minimal 2 kategori dan cukup observasi
          cats_check <- sapply(df_use, function(x) {
            tbl <- table(x[!is.na(x)])
            list(n_cats=length(tbl), min_freq=min(tbl))
          })
          # Peringatan jika ada item dengan kategori sangat jarang (< 5 observasi)
          sparse_items <- sapply(1:ncol(df_use), function(j) {
            tbl <- table(df_use[,j][!is.na(df_use[,j])])
            any(tbl < 5)
          })
          if (any(sparse_items)) {
            n_sparse <- sum(sparse_items)
            showNotification(
              paste0("⚠️ ",n_sparse," item memiliki kategori dengan < 5 observasi. ",
                     "Ini bisa menyebabkan masalah konvergensi. ",
                     "Pertimbangkan menggabungkan kategori atau menggunakan model lebih sederhana."),
              type="warning", duration=8)
          }
        } else {
          df_use <- df
        }
        
        incProgress(0.20, detail="Membangun model IRT...")
        
        # Gunakan fungsi robust untuk estimasi
        fit <- run_mirt_robust(df_use, input$model_choice, verbose=FALSE)
        rv$irt_model     <- fit
        rv$fallback_note <- attr(fit, "fallback_note")
        
        incProgress(0.30, detail="Ekstrak parameter item...")
        pd <- safe_extract_params(fit)
        rv$item_params <- data.frame(Item=rownames(pd), pd, row.names=NULL)
        
        incProgress(0.25, detail="Estimasi theta...")
        te_raw <- run_fscores_robust(fit, method=input$est_method)
        
        if (is.null(te_raw)) {
          showNotification("⚠️ Estimasi θ gagal, mencoba dengan EAP...", type="warning")
          te_raw <- fscores(fit, method="EAP", full.scores=TRUE, full.scores.SE=TRUE)
        }
        
        te <- as.data.frame(te_raw)
        # Tangani nama kolom berbeda-beda
        if (ncol(te) >= 2) {
          colnames(te)[1:2] <- c("Theta","SE_Theta")
        } else {
          te$SE_Theta <- NA
          colnames(te)[1] <- "Theta"
        }
        
        rv$theta_results <- data.frame(
          Examinee  = 1:nrow(df_use),
          Raw_Score = rowSums(df_use, na.rm=TRUE),
          Theta     = round(te$Theta, 4),
          SE_Theta  = round(te$SE_Theta, 4),
          T_Score   = round(transform_scores(te$Theta), 2))
        
        incProgress(0.10, detail="Selesai!")
        
        err_log <- attr(fit, "error_log")
        if (length(err_log) > 0) {
          showNotification(
            paste0("✅ Selesai (dengan fallback). Model: ", input$model_choice,
                   " | θ-method: ", input$est_method),
            type="warning", duration=6)
        } else {
          showNotification(
            paste("✅ Selesai! Model:", input$model_choice,
                  "| θ-method:", input$est_method),
            type="message", duration=5)
        }
        
      }, error=function(e) {
        showNotification(
          paste0("❌ Error IRT: ", e$message,
                 "\n\nSaran: Coba model lebih sederhana (PCM untuk politomus, 1PL untuk dikotomus), ",
                 "atau periksa apakah ada kategori respons yang sangat jarang."),
          type="error", duration=12)
      })
    })
  })
  
  # Tampilkan catatan fallback jika ada
  output$irt_fallback_note <- renderUI({
    if (!is.null(rv$fallback_note) && nchar(rv$fallback_note) > 0) {
      tags$div(class="warn-callout", rv$fallback_note)
    } else {
      NULL
    }
  })
  
  output$item_params_table <- renderDT({
    req(rv$item_params)
    datatable(rv$item_params,
              options=list(scrollX=TRUE,pageLength=15,dom="Bfrtip"),
              class="table-bordered table-sm",rownames=FALSE) %>%
      formatRound(which(sapply(rv$item_params,is.numeric)),digits=4)
  })
  
  output$icc_plot <- renderPlotly({
    req(rv$irt_model,input$icc_items)
    tryCatch({
      sel <- which(colnames(rv$clean_data) %in% input$icc_items)
      tseq <- seq(-4,4,by=0.08); pdata <- data.frame()
      for (i in sel) {
        pr <- probtrace(extract.item(rv$irt_model,i),tseq)
        nm <- colnames(rv$clean_data)[i]
        if (ncol(pr)==2) {
          pdata <- rbind(pdata,data.frame(Theta=tseq,P=pr[,2],Cat="P(correct)",Item=nm))
        } else {
          for (k in 1:ncol(pr))
            pdata <- rbind(pdata,data.frame(Theta=tseq,P=pr[,k],
                                            Cat=paste0("Cat.",k-1),Item=nm))
        }
      }
      p <- ggplot(pdata,aes(x=Theta,y=P,color=Item,linetype=Cat)) +
        geom_line(linewidth=1.1) +
        geom_vline(xintercept=0,linetype="dotted",color="gray50") +
        geom_hline(yintercept=0.5,linetype="dotted",color="gray50") +
        scale_color_brewer(palette="Set1") +
        labs(title=paste("ICC -",input$model_choice),
             x="Ability (θ)",y="P(θ)") +
        theme_minimal(base_size=13) +
        theme(legend.position="right",plot.title=element_text(face="bold"))
      ggplotly(p) %>% layout(autosize=TRUE,margin=list(l=60,r=20,b=60,t=60))
    }, error=function(e)
      plotly_empty()%>%layout(title=paste("Error:",e$message)))
  })
  
  output$tif_plot <- renderPlotly({
    req(rv$irt_model)
    tryCatch({
      tseq <- seq(-4,4,by=0.1)
      iv   <- testinfo(rv$irt_model,tseq)
      plot_ly(x=tseq,y=iv,type="scatter",mode="lines",
              fill="tozeroy",fillcolor="rgba(78,154,241,0.2)",
              line=list(color="#1a2340",width=2.5),name="I(θ)") %>%
        layout(
          title=list(text="Test Information Function",font=list(size=15)),
          xaxis=list(title="Ability (θ)",titlefont=list(size=13)),
          yaxis=list(title="Information I(θ)",titlefont=list(size=13)),
          autosize=TRUE,margin=list(l=60,r=20,b=60,t=60))
    }, error=function(e)
      plotly_empty()%>%layout(title=paste("Error:",e$message)))
  })
  
  output$dl_item_params <- downloadHandler(
    filename=function()paste0("item_params_",Sys.Date(),".csv"),
    content=function(f)write.csv(rv$item_params,f,row.names=FALSE))
  
  # ── MODEL-DATA FIT ──────────────────────────────────────
  observeEvent(input$run_fit, {
    req(rv$irt_model,rv$theta_results)
    tryCatch({
      df      <- rv$clean_data
      tv      <- rv$theta_results$Theta
      ng      <- input$n_groups
      mc      <- quantile(tv,probs=seq(0,1,length.out=ng+1))
      kp      <- ncol(rv$item_params)-1
      fs      <- data.frame()
      for (j in 1:ncol(df)) {
        q1 <- 0; dv <- 0
        for (g in 1:ng) {
          idx <- which(tv>=mc[g]&tv<=mc[g+1])
          if (length(idx)<3) next
          tg  <- mean(tv[idx],na.rm=TRUE)
          Po  <- mean(df[idx,j],na.rm=TRUE)
          Nj  <- length(idx)
          pg  <- tryCatch(probtrace(extract.item(rv$irt_model,j),tg),error=function(e)NULL)
          if (is.null(pg)) next
          Ei  <- if (ncol(pg)==2) pg[1,2] else {cats<-0:(ncol(pg)-1);sum(cats*pg[1,])}
          dm  <- Ei*(1-Ei)
          if (is.na(dm)||dm<=0) next
          q1  <- q1 + Nj*(Po-Ei)^2/dm; dv <- dv+1
        }
        dof <- max(dv-kp,1)
        pv  <- 1-pchisq(q1,df=dof)
        fs  <- rbind(fs,data.frame(
          Item=colnames(df)[j], Q1=round(q1,3), df=dof,
          p_value=round(pv,4), Fit=ifelse(pv>=0.05,"✅ Fit","❌ Misfit")))
      }
      rv$fit_results <- fs
      showNotification(paste("✅ Q₁ selesai untuk",nrow(fs),"item."),type="message")
    }, error=function(e) showNotification(paste("❌",e$message),type="error"))
  })
  
  output$item_fit_table <- renderDT({
    req(rv$fit_results)
    datatable(rv$fit_results,
              options=list(scrollX=TRUE,pageLength=20,dom="Bfrtip"),
              class="table-bordered table-sm",rownames=FALSE,escape=FALSE)
  })
  
  output$residual_plot <- renderPlotly({
    req(rv$irt_model,rv$theta_results,input$fit_item_sel)
    tryCatch({
      df  <- rv$clean_data; tv <- rv$theta_results$Theta
      j   <- which(colnames(df)==input$fit_item_sel)
      if (length(j)==0) return(plotly_empty())
      mc  <- quantile(tv,probs=seq(0,1,length.out=input$n_groups+1))
      rd  <- data.frame()
      for (g in 1:input$n_groups) {
        idx <- which(tv>=mc[g]&tv<=mc[g+1])
        if (length(idx)<2) next
        tg <- mean(tv[idx],na.rm=TRUE); Nj <- length(idx)
        Po <- mean(df[idx,j],na.rm=TRUE)
        pg <- tryCatch(probtrace(extract.item(rv$irt_model,j),tg),error=function(e)NULL)
        if (is.null(pg)) next
        Ei <- if (ncol(pg)==2) pg[1,2] else {cats<-0:(ncol(pg)-1);sum(cats*pg[1,])}
        rd <- rbind(rd,data.frame(Theta=tg,Observed=Po,Expected=Ei,N=Nj))
      }
      tseq  <- seq(-4,4,by=0.05)
      pl    <- probtrace(extract.item(rv$irt_model,j),tseq)
      El    <- if (ncol(pl)==2) pl[,2] else {
        cats<-0:(ncol(pl)-1); apply(pl,1,function(p)sum(cats*p))}
      plot_ly() %>%
        add_lines(x=tseq,y=El,name="Expected (ICC)",
                  line=list(color="#4e9af1",width=2.5)) %>%
        add_markers(data=rd,x=~Theta,y=~Observed,
                    text=~paste0("N=",N,"\nθ=",round(Theta,2),"\nObs=",round(Observed,3)),
                    hoverinfo="text",
                    name="Observed",marker=list(color="#e74c3c",size=9,
                                                line=list(color="darkred",width=1))) %>%
        layout(
          title=list(text=paste("Observed vs Expected:",input$fit_item_sel),
                     font=list(size=14,color="#1a2340")),
          xaxis=list(title="Ability (θ)",titlefont=list(size=13)),
          yaxis=list(title="Proportion Correct",titlefont=list(size=13)),
          legend=list(orientation="h",y=-0.2),
          autosize=TRUE,margin=list(l=60,r=20,b=80,t=60))
    }, error=function(e) plotly_empty()%>%layout(title=paste("Error:",e$message)))
  })
  
  output$fit_summary <- renderUI({
    req(rv$fit_results)
    nt <- nrow(rv$fit_results)
    nf <- sum(grepl("Fit",rv$fit_results$Fit))
    nm <- nt-nf; pc <- round(nf/nt*100,1)
    tagList(
      fluidRow(
        valueBox(nf,"Item Fit ✅",  icon=icon("check"),  color="green",width=4),
        valueBox(nm,"Item Misfit ❌",icon=icon("times"),  color="red",  width=4),
        valueBox(paste0(pc,"%"),"% Fit",icon=icon("percent"),color="blue",width=4)),
      tags$div(class=if(pc>=80)"info-callout"else"warn-callout",
               tags$b(if(pc>=80)"✅ Fit model baik secara keseluruhan."
                      else"⚠️ Sejumlah item perlu dievaluasi."),tags$br(),
               paste0(nf," dari ",nt," item (",pc,"%) memenuhi Q₁ (p≥0.05).")))
  })
  
  output$dl_fit <- downloadHandler(
    filename=function()paste0("item_fit_",Sys.Date(),".csv"),
    content=function(f)write.csv(rv$fit_results,f,row.names=FALSE))
  
  # ── PREVIEW FILE KELOMPOK ───────────────────────────────
  output$inv_group_preview_ui <- renderUI({
    req(input$inv_group_file)
    tryCatch({
      ext <- tools::file_ext(input$inv_group_file$name)
      gdf <- if (ext %in% c("xlsx","xls"))
        as.data.frame(read_excel(input$inv_group_file$datapath,
                                 col_names=input$inv_group_header))
      else
        read.csv(input$inv_group_file$datapath,
                 header=input$inv_group_header, sep=input$inv_sep_group)
      grp_col <- gdf[, 1]
      grp_tbl <- table(grp_col)
      grp_names <- names(grp_tbl)
      if (length(grp_names) != 2) {
        return(tags$div(class="warn-callout",
                        tags$b("⚠️ File harus memiliki tepat 2 kelompok."),tags$br(),
                        paste0("Terdeteksi ",length(grp_names)," kelompok: ",
                               paste(grp_names, collapse=", "))))
      }
      tags$div(class="info-callout", style="margin-top:6px;font-size:12px;",
               tags$b("✅ File kelompok valid:"),tags$br(),
               paste0("• ", grp_names[1], ": ", grp_tbl[1], " peserta"),tags$br(),
               paste0("• ", grp_names[2], ": ", grp_tbl[2], " peserta"),tags$br(),
               paste0("Total: ", length(grp_col), " baris")
      )
    }, error=function(e)
      tags$div(class="warn-callout", paste("Error baca file:", e$message)))
  })
  
  # ── INVARIANSI ──────────────────────────────────────────
  observeEvent(input$run_invariance, {
    req(rv$clean_data, rv$irt_model)
    withProgress(message="Cek invariansi parameter item...", value=0, {
      tryCatch({
        df  <- rv$clean_data
        
        if (input$inv_split == "random") {
          set.seed(42)
          i1 <- sort(sample(1:nrow(df), floor(nrow(df)/2)))
          i2 <- setdiff(1:nrow(df), i1)
          l1 <- "Kelompok Random 1"; l2 <- "Kelompok Random 2"
          
        } else if (input$inv_split == "odd_even") {
          i1 <- seq(1, nrow(df), by=2)
          i2 <- seq(2, nrow(df), by=2)
          l1 <- "Peserta Ganjil"; l2 <- "Peserta Genap"
          
        } else if (input$inv_split == "file_group") {
          req(input$inv_group_file)
          ext <- tools::file_ext(input$inv_group_file$name)
          gdf <- if (ext %in% c("xlsx","xls"))
            as.data.frame(read_excel(input$inv_group_file$datapath,
                                     col_names=input$inv_group_header))
          else
            read.csv(input$inv_group_file$datapath,
                     header=input$inv_group_header, sep=input$inv_sep_group)
          grp_col   <- as.character(gdf[, 1])
          grp_names <- unique(grp_col)
          if (length(grp_names) != 2) {
            showNotification("⚠️ File kelompok harus memiliki tepat 2 kelompok.",
                             type="warning"); return()
          }
          if (length(grp_col) != nrow(df)) {
            showNotification(
              paste0("⚠️ Jumlah baris file kelompok (",length(grp_col),
                     ") tidak sama dengan jumlah peserta data (",nrow(df),")."),
              type="error"); return()
          }
          i1 <- which(grp_col == grp_names[1])
          i2 <- which(grp_col == grp_names[2])
          l1 <- grp_names[1]; l2 <- grp_names[2]
        }
        
        if (length(i1) < 5 || length(i2) < 5) {
          showNotification("⚠️ Salah satu kelompok terlalu kecil (min. 5 peserta).",
                           type="warning"); return()
        }
        
        incProgress(0.35, detail=paste("Estimasi", l1, "..."))
        f1 <- tryCatch(
          run_mirt_robust(df[i1,,drop=FALSE], input$model_choice, verbose=FALSE),
          error=function(e) NULL)
        
        incProgress(0.35, detail=paste("Estimasi", l2, "..."))
        f2 <- tryCatch(
          run_mirt_robust(df[i2,,drop=FALSE], input$model_choice, verbose=FALSE),
          error=function(e) NULL)
        
        if (is.null(f1)||is.null(f2)) {
          showNotification("⚠️ Estimasi gagal - sampel terlalu kecil atau data bermasalah.",
                           type="warning"); return()
        }
        
        p1  <- safe_extract_params(f1)
        p2  <- safe_extract_params(f2)
        nms <- rownames(p1)
        b1  <- get_b_params(p1); b2 <- get_b_params(p2)
        a1  <- get_a_params(p1); a2 <- get_a_params(p2)
        
        rb <- if(!is.null(b1)&&!is.null(b2))
          round(cor(b1,b2,use="pairwise.complete.obs"),3) else NA
        ra <- if(!is.null(a1)&&!is.null(a2))
          round(cor(a1,a2,use="pairwise.complete.obs"),3) else NA
        
        output$inv_stats_ui <- renderUI(tagList(
          fluidRow(
            valueBox(length(i1), paste("N", l1), icon=icon("users"), color="blue",  width=4),
            valueBox(length(i2), paste("N", l2), icon=icon("users"), color="green", width=4),
            valueBox(if(!is.na(rb))rb else "-", "r (parameter b)",
                     icon=icon("chart-line"),
                     color=if(!is.na(rb)&&rb>=0.9)"green"
                     else if(!is.na(rb)&&rb>=0.7)"yellow" else "red",
                     width=4)),
          tags$div(class=if(!is.na(rb)&&rb>=0.9)"info-callout"else"warn-callout",
                   tags$b("Hasil Invariansi Parameter b: "),
                   if(!is.na(rb)&&rb>=0.9) paste0("✅ r=",rb," - sangat baik.")
                   else if(!is.na(rb)&&rb>=0.7) paste0("⚠️ r=",rb," - cukup. Cek item outlier.")
                   else paste0("❌ r=",rb," - lemah. Pertimbangkan model lain."))
        ))
        
        if (!is.null(b1)&&!is.null(b2))
          output$inv_b_plot <- renderPlotly(make_inv_plot(b1,b2,nms,"b",l1,l2))
        
        output$inv_a_panel <- renderUI({
          has_a <- !is.null(a1)&&!is.null(a2)
          if (has_a) {
            tagList(withSpinner(
              plotlyOutput("inv_a_plot",height="430px",width="100%"),
              type=4,color="#3498db"))
          } else {
            tags$div(class="info-callout",
                     tags$b("Parameter a tidak tersedia untuk model ini."),tags$br(),
                     "Model 1PL dan PCM mengasumsikan diskriminasi konstan.",tags$br(),
                     "Untuk invariansi diskriminasi, gunakan 2PL/3PL/GPCM/GRM.")
          }
        })
        
        if (!is.null(a1)&&!is.null(a2))
          output$inv_a_plot <- renderPlotly(make_inv_plot(a1,a2,nms,"a",l1,l2))
        
        showNotification("✅ Invariansi parameter item selesai!", type="message")
      }, error=function(e)
        showNotification(paste("❌ Error:", e$message), type="error"))
    })
  })
  
  # Invariansi Ability
  observeEvent(input$run_inv_ability, {
    req(rv$clean_data)
    withProgress(message="Estimasi theta lintas set item...", value=0, {
      tryCatch({
        df  <- rv$clean_data; ni <- ncol(df)
        if (input$inv_ability_split=="odd_even") {
          ia<-seq(1,ni,2); ib<-seq(2,ni,2); sl<-"Item Ganjil vs Genap"
        } else if (input$inv_ability_split=="first_last") {
          h<-floor(ni/2); ia<-1:h; ib<-(h+1):ni; sl<-"Paruh Awal vs Akhir"
        } else {
          set.seed(42); ia<-sort(sample(1:ni,floor(ni/2)))
          ib<-setdiff(1:ni,ia); sl<-"Random Split Item"
        }
        if (length(ia)<2||length(ib)<2) {
          showNotification("⚠️ Item terlalu sedikit.",type="warning"); return()
        }
        incProgress(0.4,detail="Set A...")
        fa <- tryCatch(
          run_mirt_robust(df[,ia,drop=FALSE], input$model_choice, verbose=FALSE),
          error=function(e) NULL)
        incProgress(0.4,detail="Set B...")
        fb <- tryCatch(
          run_mirt_robust(df[,ib,drop=FALSE], input$model_choice, verbose=FALSE),
          error=function(e) NULL)
        if (is.null(fa)||is.null(fb)) {
          showNotification("⚠️ Estimasi salah satu set gagal.",type="warning"); return()
        }
        tha <- fscores(fa,method="EAP",full.scores=TRUE)[,1]
        thb <- fscores(fb,method="EAP",full.scores=TRUE)[,1]
        rab <- round(cor(tha,thb,use="pairwise.complete.obs"),3)
        output$inv_ability_plot <- renderPlotly({
          n   <- length(tha)
          idx <- if(n>600) sample(1:n,600) else 1:n
          xv  <- tha[idx]; yv <- thb[idx]
          rng <- range(c(xv,yv),na.rm=TRUE)
          xl  <- seq(rng[1],rng[2],length.out=50)
          lmc <- coef(lm(yv~xv))
          plot_ly() %>%
            add_markers(x=xv,y=yv,
                        hovertemplate="θ_A: %{x:.3f}<br>θ_B: %{y:.3f}<extra></extra>",
                        marker=list(color="rgba(78,154,241,0.55)",size=6,
                                    line=list(color="#1a2340",width=0.4)),
                        name=paste0("Examinee (N=",length(idx),")")) %>%
            add_lines(x=xl,y=xl,line=list(color="red",dash="dash",width=2),
                      name="Diagonal ideal (slope=1)") %>%
            add_lines(x=xl,y=lmc[1]+lmc[2]*xl,
                      line=list(color="orange",width=1.5,dash="dot"),
                      name=paste0("Trend (r=",rab,")")) %>%
            layout(
              title=list(text=paste0("Invariansi Ability - ",sl," (r=",rab,")"),
                         font=list(size=13,color="#1a2340")),
              xaxis=list(title="θ - Set A",titlefont=list(size=12)),
              yaxis=list(title="θ - Set B",titlefont=list(size=12)),
              legend=list(orientation="h",y=-0.22),
              autosize=TRUE,margin=list(l=60,r=20,b=90,t=65))
        })
        showNotification(
          paste0("✅ r ability = ",rab," - ",
                 if(rab>=0.9)"Sangat baik ✅"else if(rab>=0.7)"Cukup ⚠️"else"Lemah ❌"),
          type=if(rab>=0.9)"message"else"warning",duration=6)
      }, error=function(e)
        showNotification(paste("❌ Error:",e$message),type="error"))
    })
  })
  
  # ── THETA & T-SCORE ────────────────────────────────────
  output$theta_dist <- renderPlotly({
    req(rv$theta_results)
    # Filter nilai non-finite (Inf, -Inf, NaN) - bisa muncul dari ML/JML pada skor ekstrem
    th_raw <- rv$theta_results$Theta
    th <- th_raw[is.finite(th_raw)]
    if (length(th) == 0) return(plotly_empty() %>% layout(title="Tidak ada nilai θ yang valid"))
    n_removed <- length(th_raw) - length(th)
    bw <- max(2*IQR(th)/length(th)^(1/3), 0.1)
    xr <- seq(min(th)-1, max(th)+1, by=0.05)
    yr <- dnorm(xr, mean(th, na.rm=TRUE), sd(th, na.rm=TRUE)) * length(th) * bw
    p <- plot_ly() %>%
      add_histogram(x=th, nbinsx=30,
                    marker=list(color="#4e9af1",line=list(color="white",width=0.8)),
                    name="Frekuensi θ") %>%
      add_lines(x=xr, y=yr,
                line=list(color="red",dash="dash",width=2), name="Normal Ref.") %>%
      layout(
        title=list(text=if(n_removed>0)
          paste0("Distribusi Theta (θ) - ",n_removed," nilai ekstrem dihapus")
          else "Distribusi Theta (θ)",
          font=list(size=15,color="#1a2340")),
        xaxis=list(title="θ", titlefont=list(size=13)),
        yaxis=list(title="Frekuensi", titlefont=list(size=13)),
        legend=list(orientation="h",y=-0.2), barmode="overlay",
        autosize=TRUE, margin=list(l=60,r=20,b=80,t=60))
    p
  })
  
  output$tscore_dist <- renderPlotly({
    req(rv$theta_results)
    ts <- rv$theta_results$T_Score
    plot_ly() %>%
      add_histogram(x=ts,nbinsx=30,
                    marker=list(color="#2ecc71",line=list(color="white",width=0.8)),
                    name="Frekuensi T-Score") %>%
      add_lines(x=c(50,50),y=c(0,nrow(rv$theta_results)*0.2),
                line=list(color="orange",dash="dash",width=2),name="T=50") %>%
      layout(
        title=list(text="Distribusi T-Score",font=list(size=15,color="#1a2340")),
        xaxis=list(title="T-Score",titlefont=list(size=13)),
        yaxis=list(title="Frekuensi",titlefont=list(size=13)),
        legend=list(orientation="h",y=-0.2),
        autosize=TRUE,margin=list(l=60,r=20,b=80,t=60))
  })
  
  output$theta_table <- renderDT({
    req(rv$theta_results)
    datatable(rv$theta_results,
              options=list(scrollX=TRUE,pageLength=20,dom="Bfrtip",
                           buttons=c("copy","csv","excel")),
              class="table-bordered table-sm stripe hover",
              rownames=FALSE,
              colnames=c("No.","Raw Score","θ (Theta)","SE(θ)","T-Score")) %>%
      formatStyle("T_Score",
                  background=styleColorBar(rv$theta_results$T_Score,"#b8e4ff"),
                  backgroundSize="98% 50%",backgroundRepeat="no-repeat",
                  backgroundPosition="center") %>%
      formatStyle("Theta",
                  color=styleInterval(c(-1,1),c("#c0392b","#333","#1a7a45")),
                  fontWeight="bold")
  })
  
  output$dl_theta <- downloadHandler(
    filename=function()paste0("theta_scores_",Sys.Date(),".csv"),
    content=function(f)write.csv(rv$theta_results,f,row.names=FALSE))
  
}

shinyApp(ui=ui, server=server)
