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

msi_transform <- function(data) {
  tryCatch({
    result <- data
    for (j in 1:ncol(data)) {
      x    <- data[, j]
      cats <- sort(unique(x[!is.na(x)]))
      if (length(cats) <= 1) next
      cum_props  <- cumsum(table(x) / sum(!is.na(x)))
      cum_props  <- cum_props[-length(cum_props)]
      z_vals     <- qnorm(pmin(pmax(cum_props, 0.001), 0.999))
      boundaries <- c(-3.5, z_vals, 3.5)
      midpoints  <- numeric(length(cats))
      for (k in seq_along(cats)) {
        lo <- boundaries[k]; hi <- boundaries[k + 1]
        mp <- (dnorm(lo) - dnorm(hi)) / (pnorm(hi) - pnorm(lo) + 1e-10)
        midpoints[k] <- mp
      }
      for (k in seq_along(cats))
        result[!is.na(data[,j]) & data[,j] == cats[k], j] <- midpoints[k]
    }
    as.data.frame(result)
  }, error = function(e) data)
}

transform_scores <- function(theta_vec) {
  theta_max <- max(abs(theta_vec), na.rm = TRUE)
  if (theta_max == 0) theta_max <- 1
  50 + (theta_vec / theta_max) * 100
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
                                              "1PL"="Rasch","2PL"="2PL","3PL"="3PLu",
                                              "PCM"="PC","GPCM"="gpcm","GRM"="graded")

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
      xaxis=list(title=paste0(param_name," — ",lab1),titlefont=list(size=12)),
      yaxis=list(title=paste0(param_name," — ",lab2),titlefont=list(size=12)),
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
                menuItem("📐 Estimasi IRT",     tabName="irt_model",  icon=icon("chart-line")),
                menuItem("📊 Model-Data Fit",   tabName="model_fit",  icon=icon("search")),
                menuItem("🔄 Uji Invariansi",   tabName="invariance", icon=icon("exchange-alt")),
                menuItem("🎯 Theta & T-Score",  tabName="scores",     icon=icon("table")),
                menuItem("ℹ️ Panduan",          tabName="about",      icon=icon("info-circle"))
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
                    tags$div(class="warn-callout",
                             "⚠️ MSI diabaikan untuk data dikotomus."),
                    hr(),
                    uiOutput("data_summary_boxes")
                )
              ),
              fluidRow(
                box(title="👁️ Preview Data (10 baris pertama)", status="success",
                    solidHeader=TRUE, width=12,
                    withSpinner(DTOutput("data_preview"),type=4,color="#4e9af1"))
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
                             "EV₁ >> EV₂: dominansi faktor pertama → unidimensionalitas terpenuhi."),
                    hr(),
                    withSpinner(
                      plotlyOutput("scree_plot",height="380px",width="100%"),
                      type=4,color="#2ecc71")
                )
              ),
              fluidRow(
                box(title="🔗 Uji Local Independence — Inter-item Correlation Matrix",
                    status="warning", solidHeader=TRUE, width=12,
                    tags$div(class="info-callout",
                             tags$b("Interpretasi Local Independence dari heatmap ini:"),tags$br(),
                             "✅ ",tags$b("Warna oranye muda dominan (r ≈ 0):"),
                             " korelasi bivariat antar item rendah = indikasi LI terpenuhi.",tags$br(),
                             "⚠️ ",tags$b("Bercak merah sistemik:"),
                             " ada dimensi lain yang mempengaruhi respons = LI mungkin terlanggar.",tags$br(),
                             "📌 ",tags$b("Catatan penting:"),
                             " Heatmap ini SEBELUM ability dikontrol."
                    ),
                    actionButton("run_li","▶ Hitung Matriks Korelasi",class="btn-warning"),
                    hr(),
                    withSpinner(
                      plotlyOutput("corr_heatmap",height="450px",width="100%"),
                      type=4,color="#f39c12")
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
                             tags$b("Estimasi:"),"Parameter butir & kemampuan."),
                    selectInput("model_choice","Model IRT:",
                                choices=list(
                                  "Dikotomus"=list(
                                    "1PL"="1PL","2PL"="2PL",
                                    "3PL"="3PL"),
                                  "Politomus"=list(
                                    "PCM"="PCM","GPCM"="GPCM",
                                    "GRM"="GRM"))),
                    selectInput("est_method","Metode Estimasi θ:",
                                choices=c("EAP"="EAP","MAP"="MAP","MLE"="ML","WLE"="WLE")),
                    selectInput("item_est","Metode Estimasi Item:",
                                choices=c("MML (Marginal ML)"="EM","MHRM (Monte Carlo EM)"="MHRM")),
                    hr(),
                    actionButton("run_irt","🚀 Jalankan Estimasi IRT",
                                 class="btn-primary btn-block",style="font-size:15px;padding:10px;")
                ),
                box(title="📋 Parameter Item", status="success", solidHeader=TRUE, width=8,
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
                box(title="📊 Item Fit Statistics — Q₁ (Yen, 1981)", status="warning",
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
                box(title="📈 Observed vs Expected ICC", status="info", solidHeader=TRUE, width=7,
                    selectInput("fit_item_sel","Pilih Item:",choices=NULL),
                    withSpinner(
                      plotlyOutput("residual_plot",height="430px",width="100%"),
                      type=4,color="#3498db")
                ),
                box(title="📊 Distribusi Standardized Residuals (zᵢⱼ)", status="success",
                    solidHeader=TRUE, width=5,
                    tags$div(class="info-callout",
                             "zᵢⱼ ≈ Normal(0,1) → model fit.",tags$br(),
                             "Distribusi real (biru) vs referensi normal (merah putus-putus)."),
                    withSpinner(
                      plotlyOutput("std_resid_hist",height="430px",width="100%"),
                      type=4,color="#2ecc71")
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
                             tags$b("Dasar Teori — Hambleton et al. (1991, Ch. 2 & 4):"),tags$br(),
                             "Invariansi dicek SETELAH model dipilih dan diestimasi karena:",tags$br(),
                             "① Invariansi adalah SIFAT MODEL yang fit, bukan sifat data mentah.",tags$br(),
                             "② Jika model tidak fit, parameter berubah antar subkelompok.",tags$br(),
                             "③ Cara: estimasi parameter di dua kelompok terpisah, bandingkan",
                             " via scatterplot, titik harus dekat diagonal (slope ≈ 1, r ≈ 1)."
                    ),
                    fluidRow(
                      column(4,
                             selectInput("inv_split","Cara pembagian sampel:",
                                         choices=c(
                                           "Median Raw Score (Low vs High)"="median",
                                           "Tertile (Bawah vs Atas 33%)"="tertile",
                                           "Random 50:50"="random")),
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
                             tags$b("Konsep (Hambleton et al., 1991, Gambar 4.3 & 4.4):"),tags$br(),
                             "Estimasi θ dari dua set item berbeda seharusnya konsisten.",
                             " Scatterplot θ_A vs θ_B harus mendekati diagonal (slope≈1).",tags$br(),
                             "Scatter besar, ability tidak stabil lintas item, model mungkin tidak fit."
                    ),
                    fluidRow(
                      column(3,
                             selectInput("inv_ability_split","Pembagian item:",
                                         choices=c(
                                           "Ganjil vs Genap (Odd-Even)"="odd_even",
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
                                      "T = 50 + ( θᵢ / θ_max ) × 100",tags$br(),tags$br(),
                                      tags$small(style="font-size:12px;",
                                                 "θᵢ = estimasi ability examinee ke-i",tags$br(),
                                                 "θ_max = max|θ| dalam sampel"))
                      ),
                      column(7,
                             tags$div(class="info-callout",
                                      tags$b("Interpretasi T-Score:"),tags$br(),
                                      "• T = 50 → θ = 0; kemampuan rata-rata sampel",tags$br(),
                                      "• T = 150 → θ_max; kemampuan tertinggi",tags$br(),
                                      "• T = −50 → −θ_max; kemampuan terendah",tags$br(),
                                      tags$b("Catatan:")," Berbeda dari standar T-score (mean=50, SD=10)."
                             )
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
                              " (KMO≥0.5, Bartlett p<0.05, Scree Plot)"),
                      tags$li("Pilih model & estimasi → ",tags$b("Estimasi IRT")),
                      tags$li("Evaluasi fit → ",tags$b("Model-Data Fit")," (Q₁, residual plot)"),
                      tags$li("Cek invariansi → ",tags$b("Uji Invariansi"),
                              " (setelah model fit!)"),
                      tags$li("Unduh hasil → ",tags$b("Theta & T-Score"))
                    ),
                    tags$hr(),
                    tags$table(class="table table-bordered table-sm",
                               tags$thead(tags$tr(
                                 tags$th("Model"),tags$th("Data"),tags$th("Parameter"),tags$th("Keterangan"))),
                               tags$tbody(
                                 tags$tr(tags$td("1PL"),tags$td("Dikotomus"),tags$td("b"),
                                         tags$td("Rasch — semua item sama diskriminasinya")),
                                 tags$tr(tags$td("2PL"),tags$td("Dikotomus"),tags$td("a, b"),
                                         tags$td("Birnbaum — diskriminasi + kesulitan")),
                                 tags$tr(tags$td("3PL"),tags$td("Dikotomus"),tags$td("a, b, c"),
                                         tags$td("+ pseudo-chance (efek menebak)")),
                                 tags$tr(tags$td("PCM"),tags$td("Politomus"),tags$td("b"),
                                         tags$td("Partial Credit (Masters, 1982)")),
                                 tags$tr(tags$td("GPCM"),tags$td("Politomus"),tags$td("a, b"),
                                         tags$td("Generalized PCM")),
                                 tags$tr(tags$td("GRM"),tags$td("Politomus"),tags$td("a, b"),
                                         tags$td("Graded Response (Samejima, 1969)")))
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
    fit_results   = NULL
  )
  
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
      nms <- colnames(df_num)
      updateSelectInput(session,"icc_items",  choices=nms, selected=nms[1:min(4,length(nms))])
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
      valueBox(nrow(df),"Examinee",icon=icon("users"),  color="blue",  width=4),
      valueBox(ncol(df),"Item",    icon=icon("list"),   color="green", width=4),
      valueBox(toupper(rv$data_type),"Jenis",icon=icon("tag"),color="orange",width=4))
  })
  
  output$data_preview <- renderDT({
    req(rv$raw_data)
    datatable(head(rv$raw_data,10),
              options=list(scrollX=TRUE,pageLength=10,dom="t"),
              class="table-bordered table-sm")
  })
  
  # ── UJI ASUMSI ──────────────────────────────────────────
  observeEvent(input$run_kmo, {
    req(rv$clean_data)
    df <- rv$clean_data
    if (input$use_msi && rv$data_type=="polytomous") df <- msi_transform(df)
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
                 if(kv>=0.6&&bp<0.05)"✅ Prasyarat terpenuhi — lanjutkan ke IRT."
                 else"⚠️ Periksa item atau sampel sebelum lanjut.")
      ))
    }, error=function(e) output$kmo_result <- renderUI(
      tags$div(class="warn-callout",paste("Error:",e$message))))
  })
  
  observeEvent(input$run_scree, {
    req(rv$clean_data)
    df <- rv$clean_data
    if (input$use_msi && rv$data_type=="polytomous") df <- msi_transform(df)
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
  
  observeEvent(input$run_li, {
    req(rv$clean_data)
    df <- rv$clean_data
    tryCatch({
      ns  <- min(ncol(df),40)
      df2 <- df[,1:ns,drop=FALSE]
      cm  <- cor(df2,use="pairwise.complete.obs")
      nms <- colnames(df2)
      output$corr_heatmap <- renderPlotly({
        plot_ly(z=cm,x=nms,y=nms,type="heatmap",
                colorscale=list(c(0,"#2c7bb6"),c(0.5,"#ffffbf"),c(1,"#d7191c")),
                zmin=-1,zmax=1,
                colorbar=list(title="r",len=0.85,thickness=15),
                hovertemplate="%{y} × %{x}<br>r = %{z:.3f}<extra></extra>") %>%
          layout(
            title=list(text=paste0("Inter-Item Correlation Matrix",
                                   if(ncol(df)>40)" (40 item pertama)"else""),
                       font=list(size=14,color="#1a2340")),
            xaxis=list(title="",tickfont=list(size=9),tickangle=-45),
            yaxis=list(title="",tickfont=list(size=9),autorange="reversed"),
            margin=list(l=80,r=20,b=100,t=60),autosize=TRUE)
      })
      if (ncol(df)>40) showNotification("Hanya 40 item pertama.",type="warning")
    }, error=function(e) showNotification(paste("Error:",e$message),type="error"))
  })
  
  output$assumption_summary <- renderUI({
    if (is.null(rv$raw_data)) return(
      tags$div(class="warn-callout","⚠️ Upload data terlebih dahulu."))
    tags$div(class="info-callout",
             tags$b("Panduan Keputusan Asumsi:"),tags$br(),
             "1. ",tags$b("KMO ≥ 0.6 & Bartlett p<0.05:")," → lanjutkan analisis.",tags$br(),
             "2. ",tags$b("Scree Plot — EV₁>>EV₂:")," → unidimensionalitas terpenuhi.",tags$br(),
             "3. ",tags$b("Heatmap warna oranye muda dominan:")," → LI kemungkinan terpenuhi.",tags$br(),
             "4. ",tags$b("Konfirmasi LI:")," gunakan residual korelasi dari model mirt setelah estimasi.")
  })
  
  # ── ESTIMASI IRT ────────────────────────────────────────
  observeEvent(input$run_irt, {
    req(rv$clean_data)
    df <- rv$clean_data
    if (input$use_msi && rv$data_type=="polytomous") df <- msi_transform(df)
    withProgress(message="Estimasi IRT...", value=0, {
      tryCatch({
        incProgress(0.2,detail="Membangun model...")
        fit <- mirt(df,model=1,itemtype=get_itemtype(input$model_choice),
                    method=input$item_est, SE=TRUE, verbose=FALSE)
        rv$irt_model <- fit
        incProgress(0.3,detail="Ekstrak parameter item...")
        pd <- safe_extract_params(fit)
        rv$item_params <- data.frame(Item=rownames(pd),pd,row.names=NULL)
        incProgress(0.3,detail="Estimasi theta...")
        te  <- as.data.frame(fscores(fit,method=input$est_method,
                                     full.scores=TRUE,full.scores.SE=TRUE))
        colnames(te)[1:2] <- c("Theta","SE_Theta")
        rv$theta_results <- data.frame(
          Examinee  = 1:nrow(df),
          Raw_Score = rowSums(df,na.rm=TRUE),
          Theta     = round(te$Theta,4),
          SE_Theta  = round(te$SE_Theta,4),
          T_Score   = round(transform_scores(te$Theta),2))
        incProgress(0.2,detail="Selesai!")
        showNotification(paste("✅ Selesai! Model:",input$model_choice,
                               "| θ-method:",input$est_method),type="message",duration=5)
      }, error=function(e)
        showNotification(paste("❌ Error IRT:",e$message),type="error",duration=8))
    })
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
        labs(title=paste("ICC —",input$model_choice),
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
  
  output$std_resid_hist <- renderPlotly({
    req(rv$irt_model,rv$theta_results)
    tryCatch({
      df  <- rv$clean_data; tv <- rv$theta_results$Theta
      mc  <- quantile(tv,probs=seq(0,1,length.out=input$n_groups+1))
      az  <- c()
      for (j in 1:ncol(df)) {
        for (g in 1:input$n_groups) {
          idx <- which(tv>=mc[g]&tv<=mc[g+1])
          if (length(idx)<2) next
          Nj  <- length(idx); tg <- mean(tv[idx],na.rm=TRUE)
          Po  <- mean(df[idx,j],na.rm=TRUE)
          pg  <- tryCatch(probtrace(extract.item(rv$irt_model,j),tg),error=function(e)NULL)
          if (is.null(pg)) next
          Ei  <- if (ncol(pg)==2) pg[1,2] else {cats<-0:(ncol(pg)-1);sum(cats*pg[1,])}
          dn  <- sqrt(Ei*(1-Ei)/Nj+1e-8)
          az  <- c(az,(Po-Ei)/dn)
        }
      }
      az <- az[is.finite(az)&abs(az)<10]
      bw <- max(2*IQR(az)/length(az)^(1/3), 0.3)
      xr <- seq(-5,5,by=0.1)
      yr <- dnorm(xr)*length(az)*bw
      plot_ly() %>%
        add_histogram(x=az,nbinsx=30,
                      marker=list(color="#4e9af1",line=list(color="white",width=0.8)),
                      name="Standardized Residuals") %>%
        add_lines(x=xr,y=yr,
                  line=list(color="red",dash="dash",width=2),
                  name="Normal Ref. (scaled)") %>%
        layout(
          title=list(text="Distribusi Standardized Residuals (zᵢⱼ)",
                     font=list(size=14,color="#1a2340")),
          xaxis=list(title="z",range=c(-6,6),titlefont=list(size=13)),
          yaxis=list(title="Frekuensi",titlefont=list(size=13)),
          legend=list(orientation="h",y=-0.2),
          barmode="overlay",autosize=TRUE,margin=list(l=60,r=20,b=80,t=60))
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
  
  # ── INVARIANSI ──────────────────────────────────────────
  observeEvent(input$run_invariance, {
    req(rv$clean_data, rv$irt_model)
    withProgress(message="Cek invariansi parameter item...", value=0, {
      tryCatch({
        df  <- rv$clean_data
        sc  <- rowSums(df,na.rm=TRUE)
        itp <- get_itemtype(input$model_choice)
        
        if (input$inv_split=="median") {
          md<-median(sc); i1<-which(sc<=md); i2<-which(sc>md)
          l1<-"Low Ability"; l2<-"High Ability"
        } else if (input$inv_split=="tertile") {
          q33<-quantile(sc,1/3); q67<-quantile(sc,2/3)
          i1<-which(sc<=q33); i2<-which(sc>=q67)
          l1<-"Bottom Tertile"; l2<-"Top Tertile"
        } else {
          set.seed(42); i1<-sample(1:nrow(df),floor(nrow(df)/2))
          i2<-setdiff(1:nrow(df),i1)
          l1<-"Random G1"; l2<-"Random G2"
        }
        
        incProgress(0.35,detail=paste("Estimasi",l1,"..."))
        f1 <- tryCatch(mirt(df[i1,,drop=FALSE],1,itemtype=itp,
                            verbose=FALSE,SE=FALSE),error=function(e)NULL)
        incProgress(0.35,detail=paste("Estimasi",l2,"..."))
        f2 <- tryCatch(mirt(df[i2,,drop=FALSE],1,itemtype=itp,
                            verbose=FALSE,SE=FALSE),error=function(e)NULL)
        
        if (is.null(f1)||is.null(f2)) {
          showNotification("⚠️ Estimasi gagal — sampel terlalu kecil.",type="warning")
          return()
        }
        
        p1  <- safe_extract_params(f1)
        p2  <- safe_extract_params(f2)
        nms <- rownames(p1)
        b1  <- get_b_params(p1); b2 <- get_b_params(p2)
        a1  <- get_a_params(p1); a2 <- get_a_params(p2)
        
        rb  <- if(!is.null(b1)&&!is.null(b2))
          round(cor(b1,b2,use="pairwise.complete.obs"),3) else NA
        ra  <- if(!is.null(a1)&&!is.null(a2))
          round(cor(a1,a2,use="pairwise.complete.obs"),3) else NA
        
        output$inv_stats_ui <- renderUI(tagList(
          fluidRow(
            valueBox(length(i1),paste("N",l1),icon=icon("users"),color="blue",width=4),
            valueBox(length(i2),paste("N",l2),icon=icon("users"),color="green",width=4),
            valueBox(if(!is.na(rb))rb else"—","r (parameter b)",
                     icon=icon("chart-line"),
                     color=if(!is.na(rb)&&rb>=0.9)"green"else if(!is.na(rb)&&rb>=0.7)"yellow"else"red",
                     width=4)),
          tags$div(class=if(!is.na(rb)&&rb>=0.9)"info-callout"else"warn-callout",
                   tags$b("Hasil Invariansi Parameter b: "),
                   if(!is.na(rb)&&rb>=0.9) paste0("✅ r=",rb," — sangat baik.")
                   else if(!is.na(rb)&&rb>=0.7) paste0("⚠️ r=",rb," — cukup. Cek item outlier.")
                   else paste0("❌ r=",rb," — lemah. Pertimbangkan model lain."))
        ))
        
        if (!is.null(b1)&&!is.null(b2))
          output$inv_b_plot <- renderPlotly(make_inv_plot(b1,b2,nms,"b",l1,l2))
        
        output$inv_a_panel <- renderUI({
          has_a <- !is.null(a1)&&!is.null(a2)
          if (has_a) {
            tagList(
              withSpinner(
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
        
        showNotification("✅ Invariansi parameter item selesai!",type="message")
      }, error=function(e)
        showNotification(paste("❌ Error:",e$message),type="error"))
    })
  })
  
  # Invariansi Ability
  observeEvent(input$run_inv_ability, {
    req(rv$clean_data)
    withProgress(message="Estimasi theta lintas set item...", value=0, {
      tryCatch({
        df  <- rv$clean_data; ni <- ncol(df)
        itp <- get_itemtype(input$model_choice)
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
        fa <- tryCatch(mirt(df[,ia,drop=FALSE],1,itemtype=itp,verbose=FALSE,SE=FALSE),
                       error=function(e)NULL)
        incProgress(0.4,detail="Set B...")
        fb <- tryCatch(mirt(df[,ib,drop=FALSE],1,itemtype=itp,verbose=FALSE,SE=FALSE),
                       error=function(e)NULL)
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
              title=list(text=paste0("Invariansi Ability — ",sl," (r=",rab,")"),
                         font=list(size=13,color="#1a2340")),
              xaxis=list(title="θ — Set A",titlefont=list(size=12)),
              yaxis=list(title="θ — Set B",titlefont=list(size=12)),
              legend=list(orientation="h",y=-0.22),
              autosize=TRUE,margin=list(l=60,r=20,b=90,t=65))
        })
        showNotification(
          paste0("✅ r ability = ",rab," — ",
                 if(rab>=0.9)"Sangat baik ✅"else if(rab>=0.7)"Cukup ⚠️"else"Lemah ❌"),
          type=if(rab>=0.9)"message"else"warning",duration=6)
      }, error=function(e)
        showNotification(paste("❌ Error:",e$message),type="error"))
    })
  })
  
  # ── THETA & T-SCORE ────────────────────────────────────
  output$theta_dist <- renderPlotly({
    req(rv$theta_results)
    th <- rv$theta_results$Theta
    bw <- max(2*IQR(th)/length(th)^(1/3),0.1)
    xr <- seq(min(th)-1,max(th)+1,by=0.05)
    yr <- dnorm(xr,mean(th,na.rm=TRUE),sd(th,na.rm=TRUE))*length(th)*bw
    plot_ly() %>%
      add_histogram(x=th,nbinsx=30,
                    marker=list(color="#4e9af1",line=list(color="white",width=0.8)),
                    name="Frekuensi θ") %>%
      add_lines(x=xr,y=yr,
                line=list(color="red",dash="dash",width=2),name="Normal Ref.") %>%
      layout(
        title=list(text="Distribusi Theta (θ)",font=list(size=15,color="#1a2340")),
        xaxis=list(title="θ",titlefont=list(size=13)),
        yaxis=list(title="Frekuensi",titlefont=list(size=13)),
        legend=list(orientation="h",y=-0.2),barmode="overlay",
        autosize=TRUE,margin=list(l=60,r=20,b=80,t=60))
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