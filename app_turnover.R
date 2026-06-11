library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(plotly)
library(readxl)
library(readr)
library(tidyr)
library(tidytext)
library(ggrepel)
library(RColorBrewer)
library(writexl)
library(stringr)
library(purrr)
library(zoo)
library(Microsoft365R)

tema_bg <- "#1e222d"
tema_panel <- "#2a2e39"
tema_texto <- "#d1d4dc"
cor_vol <- "#f39c12"     # Amarelo/Laranja (Voluntário)
cor_invol <- "#e74c3c"   # Vermelho (Involuntário)
cor_total <- "#ffffff"   # Branco (Total)
cor_azul <- "#3498db"


cor_vol_proj <- "#f39c1280"
cor_invol_proj <- "#e74c3c80"
cor_total_proj <- "#ffffff80"


plot_vazio <- function() {
  plot_ly() %>% layout(
    title = list(text = "Sem dados para essa seleção", font = list(color = tema_texto, size = 14)),
    xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
    plot_bgcolor = tema_bg, paper_bgcolor = tema_bg
  )
}

# ============================================================================
# INTERFACE DO USUÁRIO

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$div(
      "Turnover & Desligamentos",
      style = "color: #f39c12; font-weight: bold; font-size: 20px;"
    ),
    titleWidth = 300,
    tags$li(class = "dropdown",
            tags$div(style = "padding: 8px 15px; color: #d1d4dc;",
                     icon("calendar-alt"), " ", format(Sys.Date(), "%d/%m/%Y")))
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Visão Geral", tabName = "visao_geral", icon = icon("chart-pie"),
               badgeLabel = "principal", badgeColor = "green"),
      menuItem("Turnover por Área", tabName = "turnover_area", icon = icon("users")),
      menuItem("Análise Temporal", tabName = "analise_temporal", icon = icon("chart-line")),
      menuItem("Qualitativa (Empresa)", tabName = "qualitativa", icon = icon("comment-dots")),
      menuItem("Qualitativa (Funcionário)", tabName = "qualitativa_func", icon = icon("user-tag")),
      menuItem("Tabelas Detalhadas", tabName = "tabelas", icon = icon("table")),
      menuItem("Sobre", tabName = "sobre", icon = icon("info-circle"))
    ),
    
    div(class = "filtros-container",
        style = paste0("padding: 15px; background-color: ", tema_panel, "; margin: 10px; border-radius: 8px;"),
        
        h4(icon("filter"), " Filtros", style = "color: white; margin-top: 0; margin-bottom: 15px;"),
        
        conditionalPanel(
          condition = "input.sidebar != 'sobre'",
          
          selectInput("ano_referencia",
                      label = div(style = "color: #d1d4dc;", icon("calendar"), " Ano de Referência:"),
                      choices = seq(2026, 2020, -1),
                      selected = year(Sys.Date()),
                      multiple = FALSE),
          
          selectInput("area_filtro",
                      label = div(style = "color: #d1d4dc;", icon("building"), " Área:"),
                      choices = c("Todas as Áreas"),
                      multiple = FALSE, selectize = TRUE),
          
          selectInput("equipe_filtro",
                      label = div(style = "color: #d1d4dc;", icon("users"), " Equipe:"),
                      choices = c("Todas as Equipes"),
                      multiple = FALSE, selectize = TRUE),
          
          selectInput("gestor_filtro",
                      label = div(style = "color: #d1d4dc;", icon("user-tie"), " Gestor:"),
                      choices = c("Todos os Gestores"),
                      multiple = FALSE, selectize = TRUE),
          
          hr(style = "border-color: #46637f;"),
          
          radioButtons("tipo_turnover",
                       label = div(style = "color: #d1d4dc;", icon("exchange-alt"), " Tipo de Turnover:"),
                       choices = c("Total" = "total", "Voluntário" = "voluntario", "Involuntário" = "involuntario"),
                       selected = "total", inline = TRUE),
          
          br(),
          
          actionButton("aplicar_filtros", 
                       label = "Aplicar Filtros",
                       icon = icon("check"),
                       class = "btn-success btn-sm",
                       style = "width: 100%; background-color: #27ae60; border: none;")
        )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('removeClass', function(message) {
          $(message.selector).removeClass(message.className);
        });
        Shiny.addCustomMessageHandler('addClass', function(message) {
          $(message.selector).addClass(message.className);
        });
      ")),
      tags$style(HTML(paste0("
        body, .content-wrapper, .right-side { background-color: ", tema_bg, " !important; font-family: 'Segoe UI', Roboto, sans-serif; color: ", tema_texto, "; }
        .skin-black .main-header .navbar { background-color: ", tema_panel, "; }
        .skin-black .main-header .logo { background-color: ", tema_panel, "; color: white; border-right: 1px solid ", tema_bg, ";}
        .skin-black .main-sidebar { background-color: ", tema_panel, "; }
        
        .box { background-color: ", tema_panel, "; border-top: none; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.3); }
        
        /* KPIs - Ajuste de altura e margem corrigidos para não sobrepor */
        .kpi-row { display: flex; gap: 15px; margin-bottom: 25px; }
        .kpi-card { flex: 1; background: ", tema_panel, "; border-radius: 8px; padding: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.2); border-left: 4px solid; height: auto; margin-bottom: 20px;}
        .kpi-total { border-left-color: ", cor_total, "; }
        .kpi-voluntario { border-left-color: ", cor_vol, "; }
        .kpi-involuntario { border-left-color: ", cor_invol, "; }
        .kpi-headcount { border-left-color: ", cor_azul, "; }
        
        .kpi-value { font-size: 32px; font-weight: 700; margin: 5px 0; color: white; }
        .kpi-label { color: #95a5a6; font-size: 13px; font-weight: 500; text-transform: uppercase; }
        .kpi-sub { font-size: 12px; color: #7f8c8d; margin-top: 5px; }
        
        /* Gráficos */
        .chart-container { background: ", tema_panel, "; border-radius: 8px; padding: 20px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.2); }
        h4 { margin-top: 0; color: white !important; font-weight: 600; }
        h3 { margin: 0; color: white !important; font-weight: 600; margin-bottom: 10px; }
        
        /* Botões de seleção */
        .graph-selector { display: flex; gap: 10px; margin-bottom: 15px; }
        .graph-btn { flex: 1; padding: 8px; border: 1px solid #46637f; background: transparent; border-radius: 8px; cursor: pointer; color: ", tema_texto, "; transition: all 0.2s; }
        .graph-btn.active { background: ", cor_vol, "; color: ", tema_bg, "; border-color: ", cor_vol, "; font-weight: bold; }
        
        /* Tabelas */
        .dataTables_wrapper { background: ", tema_panel, "; color: ", tema_texto, "; border-radius: 8px; padding: 10px; }
        table.dataTable tbody tr { background-color: ", tema_panel, " !important; color: white !important; }
        table.dataTable tbody tr:hover { background-color: #3a3f4b !important; }
        table.dataTable thead th { background-color: ", tema_bg, " !important; color: white !important; border-bottom: 1px solid #46637f !important; }
        .dataTables_info, .dataTables_length, .dataTables_filter, .dataTables_paginate { color: ", tema_texto, " !important; }
        .paginate_button { color: white !important; }
        
        /* Insights */
        .insight-card { background: #3a3f4b; border-radius: 8px; padding: 20px; margin-bottom: 15px; border-left: 4px solid; color: ", tema_texto, ";}
        .insight-category { font-size: 12px; font-weight: 600; text-transform: uppercase; margin-bottom: 10px; color: ", cor_vol, "; }
        .insight-text { color: white; line-height: 1.6; font-size: 14px; margin-bottom: 15px; background: ", tema_panel, "; padding: 15px; border-radius: 8px; font-style: italic; border-left: 2px solid ", cor_azul, "; }
        .insight-tags { display: flex; flex-wrap: wrap; gap: 5px; }
        .insight-tag { background: ", tema_bg, "; color: #d1d4dc; padding: 4px 12px; border-radius: 20px; font-size: 11px; }
        .insight-tag.negative { color: ", cor_invol, "; border: 1px solid ", cor_invol, "; }
        .insight-tag.positive { color: #2ecc71; border: 1px solid #2ecc71; }
      ")))
    ),
    
    tabItems(
      # ======================================================================
      # VISÃO GERAL
      tabItem(
        tabName = "visao_geral",
        fluidRow(column(12, h3(icon("chart-pie"), " Visão Geral do Turnover"))),
        fluidRow(
          column(3, div(class = "kpi-card kpi-total",
                        div(class = "kpi-label", "Turnover Total"),
                        div(class = "kpi-value", textOutput("kpi_turnover_total")),
                        div(class = "kpi-sub", icon("chart-line"), " Projetado Ano: ", textOutput("kpi_turnover_total_proj", inline = TRUE)))),
          column(3, div(class = "kpi-card kpi-voluntario",
                        div(class = "kpi-label", "Turnover Voluntário"),
                        div(class = "kpi-value", textOutput("kpi_turnover_voluntario")),
                        div(class = "kpi-sub", icon("hand-peace"), " ", textOutput("kpi_deslig_voluntarios", inline = TRUE), " deslig."))),
          column(3, div(class = "kpi-card kpi-involuntario",
                        div(class = "kpi-label", "Turnover Involuntário"),
                        div(class = "kpi-value", textOutput("kpi_turnover_involuntario")),
                        div(class = "kpi-sub", icon("user-slash"), " ", textOutput("kpi_deslig_involuntarios", inline = TRUE), " deslig."))),
          column(3, div(class = "kpi-card kpi-headcount",
                        div(class = "kpi-label", "Headcount"),
                        div(class = "kpi-value", textOutput("kpi_headcount_atual")),
                        div(class = "kpi-sub", icon("users"), " Ativos no fim de ", textOutput("kpi_ano_ref", inline = TRUE))))
        ),
        
        fluidRow(
          column(8, div(class = "chart-container",
                        div(style = "display: flex; justify-content: space-between; align-items: center;",
                            h4(icon("chart-line"), " Evolução Mensal/Anual do Turnover (%)"),
                            radioButtons("visao_evolucao_mensal", "", choices = c("Mensal", "Anual"), selected = "Mensal", inline = TRUE)
                        ),
                        plotlyOutput("grafico_evolucao_mensal", height = 400))),
          column(4, div(class = "chart-container",
                        h4(icon("pie-chart"), " Distribuição por Tipo"),
                        plotlyOutput("grafico_distribuicao_tipo", height = 400)))
        ),
        
        fluidRow(
          column(12, div(class = "chart-container",
                         h4(icon("chart-bar"), " Turnover Projetado vs Realizado (%)"),
                         plotlyOutput("grafico_projetado_vs_real", height = 400)))
        ),
        
        fluidRow(
          column(6, div(class = "chart-container",
                        h4(icon("trophy"), " Top Rankings"),
                        div(class = "graph-selector",
                            actionButton("btn_rank_area", "Áreas", class = "graph-btn active"),
                            actionButton("btn_rank_equipe", "Equipes", class = "graph-btn"),
                            actionButton("btn_rank_gestor", "Gestores", class = "graph-btn")
                        ),
                        plotlyOutput("grafico_top_rankings", height = 450))),
          column(6, 
                 div(class = "chart-container",
                     h4(icon("exclamation-triangle"), " Áreas Críticas (Turnover > 15%)"),
                     DTOutput("tabela_areas_criticas")),
                 div(class = "chart-container",
                     h4(icon("check-circle"), " Áreas Saudáveis (Turnover <= 15%)"),
                     DTOutput("tabela_areas_ok"))
          )
        )
      ),
      
      # ======================================================================
      # TURNOVER POR ÁREA E GESTOR
      tabItem(
        tabName = "turnover_area",
        fluidRow(column(12, h3(icon("users"), " Análise de Turnover por Área e Gestor"))),
        fluidRow(
          column(6, div(class = "chart-container", h4(icon("building"), " Turnover por Área (Vol x Invol)"), plotlyOutput("grafico_turnover_area_detalhado", height = 450))),
          column(6, div(class = "chart-container", h4(icon("user-tie"), " Turnover por Gestor (Vol x Invol)"), plotlyOutput("grafico_turnover_gestor_detalhado", height = 450)))
        ),
        fluidRow(
          column(12, div(class = "chart-container", h4(icon("table"), " Matriz de Turnover por Área e Equipe"), DTOutput("tabela_matriz_turnover")))
        ),
        fluidRow(
          column(12, div(class = "chart-container", h4(icon("table"), " Matriz de Turnover (Ano x Total/Liderança)"), DTOutput("tabela_area_ano_lideranca")))
        )
      ),
      
      # ======================================================================
      # ANÁLISE TEMPORAL
      tabItem(
        tabName = "analise_temporal",
        fluidRow(column(12, h3(icon("chart-line"), " Análise Temporal do Turnover"))),
        fluidRow(
          column(12, div(class = "chart-container", 
                         div(style = "display: flex; justify-content: space-between; align-items: center;",
                             h4(icon("exchange-alt"), " Headcount | Entradas x Saídas e Turnover (%)"),
                             radioButtons("visao_hc_entradas", "", choices = c("Mensal", "Anual"), selected = "Mensal", inline = TRUE)
                         ),
                         plotlyOutput("grafico_hc_entradas_saidas", height = 450)))
        ),
        fluidRow(
          column(12, div(class = "chart-container", h4(icon("users"), " Evolução de Headcount"), plotlyOutput("grafico_hc_lideranca", height = 400)))
        ),
        fluidRow(
          column(12, div(class = "chart-container", h4(icon("calendar"), " Série Histórica Mensal"), plotlyOutput("grafico_serie_historica", height = 400)))
        ),
        fluidRow(
          column(6, div(class = "chart-container", h4(icon("chart-bar"), " Desligamentos por Mês/Ano"), plotlyOutput("grafico_desligamentos_mensal", height = 400))),
          column(6, div(class = "chart-container", h4(icon("clock"), " Desligamentos por Período de Casa"), plotlyOutput("grafico_periodo_casa", height = 400)))
        ),
        fluidRow(
          column(6, div(class = "chart-container", h4(icon("balance-scale"), " Comparativo Anual"), plotlyOutput("grafico_comparativo_anual", height = 400))),
          column(6, div(class = "chart-container", h4(icon("chart-area"), " Previsão vs Realizado YTD (Bootstrap Histórico)"), plotlyOutput("grafico_ytd_comparativo", height = 400)))
        )
      ),
      
      # ======================================================================
      # ANÁLISE QUALITATIVA (EMPRESA)
      tabItem(
        tabName = "qualitativa",
        fluidRow(column(12, h3(icon("comment-dots"), " Análise Qualitativa dos Desligamentos (Visão Empresa)"))),
        fluidRow(
          column(3, div(class = "kpi-card", div(class = "kpi-label", "Principal Causa"), div(class = "kpi-value", textOutput("kpi_principal_causa")))),
          column(3, div(class = "kpi-card", div(class = "kpi-label", "Sentimento Médio"), div(class = "kpi-value", textOutput("kpi_sentimento_medio")))),
          column(3, div(class = "kpi-card", div(class = "kpi-label", "Total de Tags"), div(class = "kpi-value", textOutput("kpi_total_tags")))),
          column(3, div(class = "kpi-card", div(class = "kpi-label", "Casos Analisados"), div(class = "kpi-value", textOutput("kpi_casos_qualitativos"))))
        ),
        fluidRow(
          column(5, div(class = "chart-container", h4(icon("chart-pie"), " Distribuição de Causas por Área"), plotlyOutput("grafico_distribuicao_causas", height = 400))),
          column(7, div(class = "chart-container", h4(icon("smile"), " Análise de Sentimento por Categoria"), plotlyOutput("grafico_sentimento_categorias", height = 400)))
        ),
        fluidRow(
          column(6, div(class = "chart-container", h4(icon("chart-bar"), " Principais Motivos por Área"), plotlyOutput("grafico_motivos_area", height = 400))),
          column(6, div(class = "chart-container", h4(icon("tags"), " Tags Mais Frequentes"), plotlyOutput("grafico_frequencia_tags", height = 400)))
        ),
        fluidRow(
          column(12, div(class = "chart-container", h4(icon("comments"), " Insights das Entrevistas (Trechos por Causa)"), uiOutput("cards_insights_qualitativos")))
        ),
        fluidRow(
          column(12, div(class = "chart-container", h4(icon("table"), " Análise Qualitativa - Detalhada"), DTOutput("tabela_qualitativa_detalhada")))
        )
      ),
      
      # ======================================================================
      # ANÁLISE QUALITATIVA (FUNCIONÁRIO)
      tabItem(
        tabName = "qualitativa_func",
        fluidRow(column(12, h3(icon("user-tag"), " Análise Qualitativa dos Desligamentos (Visão Funcionário)"))),
        fluidRow(
          column(3, div(class = "kpi-card", div(class = "kpi-label", "Principal Causa (Func)"), div(class = "kpi-value", textOutput("kpi_principal_causa_func")))),
          column(3, div(class = "kpi-card", div(class = "kpi-label", "Sentimento Médio"), div(class = "kpi-value", textOutput("kpi_sentimento_medio_func")))),
          column(3, div(class = "kpi-card", div(class = "kpi-label", "Total de Tags"), div(class = "kpi-value", textOutput("kpi_total_tags_func")))),
          column(3, div(class = "kpi-card", div(class = "kpi-label", "Casos Analisados"), div(class = "kpi-value", textOutput("kpi_casos_qualitativos_func"))))
        ),
        fluidRow(
          column(5, div(class = "chart-container", h4(icon("chart-pie"), " Distribuição de Causas (Func) por Área"), plotlyOutput("grafico_distribuicao_causas_func", height = 400))),
          column(7, div(class = "chart-container", h4(icon("smile"), " Análise de Sentimento por Categoria (Func)"), plotlyOutput("grafico_sentimento_categorias_func", height = 400)))
        ),
        fluidRow(
          column(6, div(class = "chart-container", h4(icon("chart-bar"), " Principais Motivos (Func) por Área"), plotlyOutput("grafico_motivos_area_func", height = 400))),
          column(6, div(class = "chart-container", h4(icon("tags"), " Tags Mais Frequentes"), plotlyOutput("grafico_frequencia_tags_func", height = 400)))
        ),
        fluidRow(
          column(12, div(class = "chart-container", h4(icon("comments"), " Insights das Entrevistas (Trechos por Causa - Func)"), uiOutput("cards_insights_qualitativos_func")))
        ),
        fluidRow(
          column(12, div(class = "chart-container", h4(icon("table"), " Análise Qualitativa (Visão Func) - Detalhada"), DTOutput("tabela_qualitativa_detalhada_func")))
        )
      ),
      
      # ======================================================================
      # TABELAS DETALHADAS
      tabItem(
        tabName = "tabelas",
        fluidRow(column(12, h3(icon("table"), " Dados Detalhados"))),
        fluidRow(
          column(12, div(class = "chart-container", h4(icon("users"), " Desligamentos por Período"), DTOutput("tabela_desligamentos_detalhada")))
        )
      ),
      
      # ======================================================================
      # SOBRE
      tabItem(
        tabName = "sobre",
        fluidRow(column(12, h3(icon("info-circle"), " Sobre o Dashboard"))),
        fluidRow(
          column(6, div(class = "chart-container",
                        h4(icon("calculator"), " Metodologia de Cálculo"),
                        h5("Turnover Total:"), p("(Desligamentos no período ÷ Headcount médio do período) × 100"),
                        h5("Turnover Projetado (Bootstrap Histórico):"), p("A projeção para meses futuros avalia o volume histórico daquele mês específico nos anos anteriores, usando simulação para manter a sazonalidade exata de cada mês."),
                        h5("Headcount Final:"), p("Colaboradores ativos no último dia do período"),
                        br(),
                        h4(icon("database"), " Fonte dos Dados"),
                        p("• Dados Perfil: Base principal de colaboradores"),
                        p("• Desligamentos_API: Entrevistas e análise qualitativa"))),
          column(6, div(class = "chart-container",
                        h4(icon("chart-bar"), " Interpretação dos Indicadores"),
                        h5("🔴 Turnover Crítico (>15%):"), p("Necessita atenção e análise de causas"),
                        h5("🟡 Turnover Moderado (10-15%):"), p("Acompanhamento mensal recomendado"),
                        h5("🟢 Turnover Saudável (<10%):"), p("Indicador dentro do esperado"),
                        br(),
                        h4(icon("tags"), " Categorias de Análise"),
                        p("• Liderança • Cultura • Carreira • Ambiente • Flexibilidade • Empresa")))
        )
      )
    )
  )
)

# ============================================================================
# SERVER

server <- function(input, output, session) {
  
  observe({
    df <- gerar_dados_sinteticos()
    
    # Processamento padrão de tags (mantém lógica original)
    df_qual <- gerar_dados_qualitativos(df)
    tags_list <- lapply(df_qual$aprofundamento, function(texto) {
      tags <- str_extract_all(texto, "#[a-zA-ZÀ-ÿáéíóúãõâêîôûç]+_[NP]")[[1]]
      list(neg = tags[grepl("_N$", tags)], pos = tags[grepl("_P$", tags)])
    })
    df_qual$tags_negativas  <- sapply(tags_list, function(x) paste(x$neg, collapse = "; "))
    df_qual$tags_positivas  <- sapply(tags_list, function(x) paste(x$pos, collapse = "; "))
    df_qual$n_tags_neg      <- sapply(tags_list, function(x) length(x$neg))
    df_qual$n_tags_pos      <- sapply(tags_list, function(x) length(x$pos))
    df_qual$total_tags      <- df_qual$n_tags_neg + df_qual$n_tags_pos
    df_qual$sentimento_score <- ifelse(
      df_qual$total_tags > 0,
      (df_qual$n_tags_pos - df_qual$n_tags_neg) / df_qual$total_tags, 0
    )
    df_qual$area <- str_split_fixed(df_qual$time, " - ", 3)[, 1]
    df_qual$data_desligamento <- as.Date(df_qual$data_do_desligamento)
    
    dados_quantitativos(df)
    dados_qualitativos_processados(df_qual)
    
    updateSelectInput(session, "area_filtro",
                      choices = c("Todas as Áreas", sort(unique(df$area))))
    updateSelectInput(session, "equipe_filtro",
                      choices = c("Todas as Equipes", sort(unique(df$equipe))))
    updateSelectInput(session, "gestor_filtro",
                      choices = c("Todos os Gestores", sort(unique(df$nome_gestor))))
  })
  
  # Reativos
  dados_quantitativos <- reactiveVal(NULL)
  dados_qualitativos_processados <- reactiveVal(NULL)
  ranking_selecionado <- reactiveVal("area")
  
  observeEvent(input$btn_rank_area, {
    ranking_selecionado("area")
    session$sendCustomMessage(type = 'removeClass', message = list(selector = '.graph-btn', className = 'active'))
    session$sendCustomMessage(type = 'addClass', message = list(selector = '#btn_rank_area', className = 'active'))
  })
  observeEvent(input$btn_rank_equipe, {
    ranking_selecionado("equipe")
    session$sendCustomMessage(type = 'removeClass', message = list(selector = '.graph-btn', className = 'active'))
    session$sendCustomMessage(type = 'addClass', message = list(selector = '#btn_rank_equipe', className = 'active'))
  })
  observeEvent(input$btn_rank_gestor, {
    ranking_selecionado("gestor")
    session$sendCustomMessage(type = 'removeClass', message = list(selector = '.graph-btn', className = 'active'))
    session$sendCustomMessage(type = 'addClass', message = list(selector = '#btn_rank_gestor', className = 'active'))
  })
  
  # ========================================================================
  # CARREGAMENTO DOS DADOS

  
  gerar_dados_sinteticos <- function() {
    set.seed(42)
    n_total <- 800
    
    areas <- c("Tecnologia", "Produto", "Comercial", "Financeiro", "RH",
               "Operações", "Marketing", "Jurídico", "Dados & Analytics",
               "Infraestrutura", "Atendimento", "Segurança")
    
    equipes_por_area <- list(
      "Tecnologia"        = c("Backend", "Frontend", "DevOps", "QA", "Mobile", "Arquitetura"),
      "Produto"           = c("Design", "UX Research", "Product Management", "Growth Product"),
      "Comercial"         = c("Inside Sales", "Field Sales", "Customer Success", "Pré-vendas", "Parcerias"),
      "Financeiro"        = c("Controladoria", "FP&A", "Contabilidade", "Tesouraria", "Auditoria"),
      "RH"                = c("Talent Acquisition", "People Analytics", "T&D", "Benefícios", "HRBP"),
      "Operações"         = c("Logística", "Processos", "Suporte", "Facilities", "Compras"),
      "Marketing"         = c("Growth", "Brand", "Performance", "Conteúdo", "CRM"),
      "Jurídico"          = c("Contratos", "Compliance", "Contencioso", "Propriedade Intelectual"),
      "Dados & Analytics" = c("Data Engineering", "Data Science", "BI", "Analytics Engineering"),
      "Infraestrutura"    = c("Cloud", "Redes", "Sistemas", "Monitoramento"),
      "Atendimento"       = c("Suporte N1", "Suporte N2", "Suporte N3", "Sucesso do Cliente"),
      "Segurança"         = c("AppSec", "InfoSec", "GRC", "SOC")
    )
    
    gestores <- c(
      "Ana Lima", "Bruno Souza", "Carla Mendes", "Diego Rocha",
      "Elena Costa", "Felipe Nunes", "Gabriela Dias", "Henrique Alves",
      "Isabela Ferreira", "João Martins", "Karen Oliveira", "Lucas Pereira",
      "Marina Santos", "Nathan Cruz", "Olivia Ramos", "Paulo Teixeira",
      "Renata Borges", "Sérgio Melo", "Tatiana Vieira", "Ulisses Campos"
    )
    
    cargos <- c(
      "Analista Jr", "Analista Pleno", "Analista Sênior", "Especialista",
      "Coordenador", "Gerente", "Gerente Sênior", "Diretor", "Head",
      "Estagiário", "Assistente", "Consultor"
    )
    
    # Pesos de área para simular empresa real (Tech e Comercial maiores)
    area_prob <- c(.18, .10, .18, .08, .06, .10, .07, .04, .07, .05, .09, .03) 
    area_vec   <- sample(areas, n_total, replace = TRUE, prob = area_prob)
    equipe_vec <- mapply(function(a) sample(equipes_por_area[[a]], 1), area_vec)
    gestor_vec <- sample(gestores, n_total, replace = TRUE)
    cargo_vec  <- sample(cargos, n_total, replace = TRUE,
                         prob = c(.15,.20,.15,.08,.10,.08,.04,.02,.03,.07,.05,.03))
    
    # Admissões de 2015 a hoje — empresa com histórico longo
    admissao_vec <- as.Date("2015-01-01") + sample(0:3800, n_total, replace = TRUE)
    
    # ~35% desligados, com sazonalidade (mais saídas no começo do ano e meados)
    desligado_idx <- sample(1:n_total, round(n_total * 0.35))
    desligamento_vec <- as.Date(rep(NA, n_total))
    
    for (i in desligado_idx) {
      min_data <- admissao_vec[i] + 60
      max_data <- min(Sys.Date() - 1, admissao_vec[i] + 2500)
      if (min_data >= max_data) next
      # Sazonalidade: peso maior em Jan-Mar e Jun-Jul
      dias_possiveis <- seq(min_data, max_data, by = "day")
      meses <- as.integer(format(dias_possiveis, "%m"))
      peso_mes <- ifelse(meses %in% c(1,2,3), 1.8,
                         ifelse(meses %in% c(6,7),   1.4, 1.0))
      desligamento_vec[i] <- sample(dias_possiveis, 1, prob = peso_mes)
    }
    
    motivo_vec <- rep(NA_character_, n_total)
    motivo_vec[desligado_idx] <- sample(
      c("voluntário", "involuntário"), length(desligado_idx),
      replace = TRUE, prob = c(0.60, 0.40)
    )
    
    lideranca_vec <- ifelse(
      grepl("Coordenador|Gerente|Diretor|Head", cargo_vec), "Sim", "Não"
    )
    
    dias_casa <- as.numeric(desligamento_vec - admissao_vec)
    periodo_casa <- dplyr::case_when(
      is.na(dias_casa)  ~ NA_character_,
      dias_casa <= 90   ~ "Até 90 dias",
      dias_casa <= 180  ~ "91-180 dias",
      dias_casa <= 365  ~ "6-12 meses",
      dias_casa <= 730  ~ "1-2 anos",
      TRUE              ~ "Mais de 2 anos"
    )
    
    data.frame(
      nome              = paste("Colaborador", 1:n_total),
      area              = area_vec,
      equipe            = equipe_vec,
      cargo             = cargo_vec,
      nome_gestor       = gestor_vec,
      data_admissao     = admissao_vec,
      data_desligamento = desligamento_vec,
      motivo            = motivo_vec,
      e_lideranca       = lideranca_vec,
      dias_casa         = dias_casa,
      periodo_casa      = periodo_casa,
      stringsAsFactors  = FALSE
    )
  }
  
  gerar_dados_qualitativos <- function(df_quant) {
    set.seed(99)
    desligados <- df_quant %>%
      dplyr::filter(!is.na(data_desligamento)) %>%
      dplyr::select(nome, area, equipe, data_desligamento)
    
    motivos_empresa <- c("Liderança", "Cultura", "Carreira",
                         "Ambiente", "Flexibilidade", "Empresa")
    
    tags_neg <- c(
      "#liderança_N", "#comunicação_N", "#feedback_N", "#cultura_N",
      "#salário_N", "#carreira_N", "#remoto_N", "#ambiente_N",
      "#beneficios_N", "#promoção_N", "#transparencia_N", "#onboarding_N",
      "#direcionamento_N", "#tomada_N", "#diversidade_N", "#equipamento_N"
    )
    tags_pos <- c(
      "#desafio_P", "#equipe_P", "#aprendizado_P", "#flexibilidade_P",
      "#liderança_P", "#cultura_P", "#beneficios_P", "#reconhecimento_P"
    )
    
    frases_pool <- c(
      "A comunicação com a liderança era bastante falha e gerava insegurança.",
      "Senti falta de perspectivas claras de crescimento na carreira.",
      "O ambiente de trabalho ficou difícil nos últimos meses.",
      "Recebi uma proposta com melhor remuneração e não foi possível reter.",
      "Gostaria de mais flexibilidade no modelo de trabalho remoto.",
      "Falta de reconhecimento foi o principal fator da saída.",
      "A cultura não estava alinhada com meus valores pessoais.",
      "O onboarding foi confuso e me senti perdido nos primeiros meses.",
      "Não havia clareza sobre a direção estratégica da empresa.",
      "Os equipamentos e ferramentas disponíveis eram inadequados.",
      "Sentia que meu desenvolvimento estava estagnado.",
      "A relação com o gestor imediato ficou desgastada.",
      "Buscava mais desafios técnicos do que os disponíveis na função.",
      "O pacote de benefícios não era competitivo com o mercado.",
      "Havia falta de diversidade e inclusão no ambiente.",
      "A empresa passou por muitas mudanças e gerou instabilidade."
    )
    
    n <- nrow(desligados)
    motivo_vec <- sample(motivos_empresa, n, replace = TRUE,
                         prob = c(.25, .15, .22, .12, .13, .13))
    
    aprofundamento_vec <- sapply(seq_len(n), function(i) {
      t_neg <- sample(tags_neg, sample(2:4, 1))
      t_pos <- sample(tags_pos, sample(0:2, 1))
      frase <- sample(frases_pool, sample(1:2, 1))
      paste(paste(frase, collapse = " "), paste(t_neg, collapse = " "), paste(t_pos, collapse = " "))
    })
    
    data.frame(
      name                 = desligados$nome,
      time                 = paste0(desligados$area, " - Equipe - ", desligados$equipe),
      motivo               = motivo_vec,
      categoria_principal  = motivo_vec,
      aprofundamento       = aprofundamento_vec,
      data_do_desligamento = desligados$data_desligamento,
      stringsAsFactors     = FALSE
    )
  }
  
  observeEvent(input$area_filtro, {
    df <- dados_quantitativos()
    if (is.null(df)) return()
    if (input$area_filtro == "Todas as Áreas") {
      equipes <- c("Todas as Equipes", sort(unique(df$equipe[!is.na(df$equipe)])))
    } else {
      equipes <- c("Todas as Equipes", sort(unique(df$equipe[df$area == input$area_filtro & !is.na(df$equipe)])))
    }
    updateSelectInput(session, "equipe_filtro", choices = equipes)
  })
  
  dados_filtrados <- reactive({
    df <- dados_quantitativos()
    req(df)
    if (input$area_filtro != "Todas as Áreas") df <- df %>% filter(area == input$area_filtro)
    if (input$equipe_filtro != "Todas as Equipes") df <- df %>% filter(equipe == input$equipe_filtro)
    if (input$gestor_filtro != "Todos os Gestores") df <- df %>% filter(nome_gestor == input$gestor_filtro)
    df
  })
  
  dados_qual_filtrados <- reactive({
    df <- dados_qualitativos_processados()
    if (is.null(df)) return(NULL)
    if (input$area_filtro != "Todas as Áreas") df <- df %>% filter(area == input$area_filtro)
    if ("data_desligamento" %in% names(df)) df <- df %>% filter(year(data_desligamento) == input$ano_referencia | is.na(data_desligamento))
    df
  })
  
  dados_qual_filtrados_func <- reactive({
    df <- dados_qual_filtrados()
    if (is.null(df)) return(NULL)
    
    df <- df %>%
      mutate(
        tags_analise = tolower(paste(tags_negativas, tags_positivas, sep = "; ")),
        categoria_funcionario = case_when(
          str_detect(tags_analise, "liderança|comunicação|direcionamento|tomada|feedback") ~ "Liderança",
          str_detect(tags_analise, "cultura|relação|equipe|diversidade|interesse") ~ "Cultura",
          str_detect(tags_analise, "promoção|treinamento|carreira|migração|desafio|onboarding") ~ "Carreira",
          str_detect(tags_analise, "local|equipamento|tecnologia|ambiente") ~ "Ambiente",
          str_detect(tags_analise, "remoto|híbrido|presencial|horário|flexibilidade") ~ "Flexibilidade",
          str_detect(tags_analise, "salário|beneficios|trabalho|transparencia") ~ "Empresa",
          TRUE ~ "Outros"
        )
      )
    return(df)
  })
  
  
  calcular_headcount <- function(df, data_ref) {
    if (is.null(df) || nrow(df) == 0) return(0)
    df %>%
      filter(!is.na(data_admissao),
             data_admissao <= data_ref,
             (is.na(data_desligamento) | data_desligamento > data_ref)) %>%
      nrow()
  }
  
  calcular_headcount_fim_ano <- function(df, ano) {
    data_fim <- as.Date(paste0(ano, "-12-31"))
    if(data_fim > Sys.Date()) data_fim <- Sys.Date()
    calcular_headcount(df, data_fim)
  }
  
  calcular_desligamentos_periodo <- function(df, data_inicio, data_fim, tipo = "total") {
    if (is.null(df) || nrow(df) == 0) return(0)
    base <- df %>% filter(!is.na(data_desligamento), data_desligamento >= data_inicio, data_desligamento <= data_fim)
    if (tipo == "voluntario") base <- base %>% filter(motivo == "voluntário")
    else if (tipo == "involuntario") base <- base %>% filter(motivo == "involuntário")
    nrow(base)
  }
  
  calcular_desligamentos_ano <- function(df, ano, tipo = "total") {
    calcular_desligamentos_periodo(df, as.Date(paste0(ano, "-01-01")), as.Date(paste0(ano, "-12-31")), tipo)
  }
  
  calcular_turnover <- function(df, ano, tipo = "total") {
    if (is.null(df) || nrow(df) == 0) return(0)
    headcount_fim <- calcular_headcount_fim_ano(df, ano)
    if (headcount_fim == 0) return(0)
    desligamentos <- calcular_desligamentos_ano(df, ano, tipo)
    round((desligamentos / headcount_fim) * 100, 2)
  }
  
  calcular_projecao_bootstrap_abs <- function(df, ano_ref, tipo = "total", iter = 500) {
    hoje <- Sys.Date()
    if (ano_ref < year(hoje)) return(calcular_desligamentos_ano(df, ano_ref, tipo))
    if (ano_ref > year(hoje)) return(0)
    
    meses_passados <- month(hoje)
    meses_restantes <- 12 - meses_passados
    if(meses_restantes <= 0) return(calcular_desligamentos_ano(df, ano_ref, tipo))
    
    deslig_ytd <- calcular_desligamentos_periodo(df, as.Date(paste0(ano_ref, "-01-01")), hoje, tipo)
    
    projetado_resto <- 0
    set.seed(42) 
    
    for(m in (meses_passados + 1):12) {
      hist_m <- df %>% filter(!is.na(data_desligamento), month(data_desligamento) == m, year(data_desligamento) < ano_ref)
      if (tipo == "voluntario") hist_m <- hist_m %>% filter(motivo == "voluntário")
      else if (tipo == "involuntario") hist_m <- hist_m %>% filter(motivo == "involuntário")
      
      counts <- hist_m %>% count(ano_hist = year(data_desligamento)) %>% pull(n)
      
      if(length(counts) == 0) {
        hist_all <- df %>% filter(!is.na(data_desligamento), year(data_desligamento) < ano_ref)
        if(tipo == "voluntario") hist_all <- hist_all %>% filter(motivo == "voluntário")
        else if(tipo == "involuntario") hist_all <- hist_all %>% filter(motivo == "involuntário")
        counts <- hist_all %>% count(ano_mes = format(data_desligamento, "%Y-%m")) %>% pull(n)
        if(length(counts) == 0) counts <- 0
      }
      projetado_resto <- projetado_resto + mean(sample(counts, iter, replace = TRUE))
    }
    return(deslig_ytd + projetado_resto)
  }
  
  calcular_turnover_projetado <- function(df, ano_ref, tipo = "total") {
    if (is.null(df) || nrow(df) == 0) return(0)
    projecao_abs <- calcular_projecao_bootstrap_abs(df, ano_ref, tipo)
    if(is.na(projecao_abs)) projecao_abs <- 0
    headcount_atual <- calcular_headcount(df, Sys.Date())
    if (headcount_atual == 0) return(0)
    round((projecao_abs / headcount_atual) * 100, 2)
  }
  
  calcular_turnover_por_grupo <- function(df, ano, grupo, tipo = "total") {
    if (is.null(df) || nrow(df) == 0) return(data.frame())
    
    data_fim <- as.Date(paste0(ano, "-12-31"))
    if(data_fim > Sys.Date()) data_fim <- Sys.Date()
    
    hc_df <- df %>%
      filter(!is.na(data_admissao), data_admissao <= data_fim,
             (is.na(data_desligamento) | data_desligamento > data_fim)) %>%
      count(.data[[grupo]], name = "headcount")
    
    deslig_df <- df %>% filter(!is.na(data_desligamento), year(data_desligamento) == ano)
    
    if (tipo == "voluntario") {
      deslig_df <- deslig_df %>% filter(motivo == "voluntário")
    } else if (tipo == "involuntario") {
      deslig_df <- deslig_df %>% filter(motivo == "involuntário")
    }
    deslig_df <- deslig_df %>% count(.data[[grupo]], name = "desligamentos")
    
    resultados <- full_join(hc_df, deslig_df, by = grupo) %>%
      mutate(headcount = replace_na(headcount, 0),
             desligamentos = replace_na(desligamentos, 0)) %>%
      filter(headcount > 0, !is.na(.data[[grupo]]), .data[[grupo]] != "Não informado", .data[[grupo]] != "") %>%
      mutate(turnover = round((desligamentos / headcount) * 100, 2)) %>%
      arrange(desc(turnover))
    
    if(nrow(resultados) == 0) return(data.frame())
    
    return(resultados)
  }
  
  calcular_evolucao_mensal <- function(df, anos = NULL) {
    if (is.null(df) || nrow(df) == 0) return(data.frame())
    if (is.null(anos)) anos <- c(year(Sys.Date())-1, year(Sys.Date()))
    
    resultado <- data.frame()
    meses_pt <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
    set.seed(42)
    
    for (ano in anos) {
      for (mes in 1:12) {
        data_inicio <- as.Date(paste0(ano, "-", mes, "-01"))
        data_fim <- ceiling_date(data_inicio, "month") - days(1)
        
        is_future <- (ano == year(Sys.Date()) & mes > month(Sys.Date()))
        
        headcount_medio <- (calcular_headcount(df, data_inicio - days(1)) + calcular_headcount(df, data_fim)) / 2
        if(headcount_medio == 0) headcount_medio <- calcular_headcount(df, Sys.Date())
        if(headcount_medio == 0) next
        
        if (!is_future && data_inicio <= Sys.Date()) {
          deslig_vol <- df %>% filter(!is.na(data_desligamento), data_desligamento >= data_inicio, data_desligamento <= data_fim, motivo == "voluntário") %>% nrow()
          deslig_invol <- df %>% filter(!is.na(data_desligamento), data_desligamento >= data_inicio, data_desligamento <= data_fim, motivo == "involuntário") %>% nrow()
          deslig_total <- deslig_vol + deslig_invol
          
          resultado <- rbind(resultado, data.frame(
            ano = ano, mes = mes, data = data_inicio, mes_ano = paste0(meses_pt[mes], "/", substr(ano,3,4)),
            vol_real = deslig_vol, invol_real = deslig_invol, total_real = deslig_total,
            vol_proj = NA, invol_proj = NA, total_proj = NA,
            to_vol_real = (deslig_vol/headcount_medio)*100, to_invol_real = (deslig_invol/headcount_medio)*100, to_total_real = (deslig_total/headcount_medio)*100,
            to_vol_proj = NA, to_invol_proj = NA, to_total_proj = NA
          ))
        } else if (is_future) {
          hist_m <- df %>% filter(!is.na(data_desligamento), month(data_desligamento) == mes, year(data_desligamento) < ano)
          
          counts_vol <- hist_m %>% filter(motivo == "voluntário") %>% count(ano_hist = year(data_desligamento)) %>% pull(n)
          counts_invol <- hist_m %>% filter(motivo == "involuntário") %>% count(ano_hist = year(data_desligamento)) %>% pull(n)
          
          if(length(counts_vol) == 0) counts_vol <- 0
          if(length(counts_invol) == 0) counts_invol <- 0
          
          vol_proj_val <- mean(sample(counts_vol, 500, replace=TRUE))
          invol_proj_val <- mean(sample(counts_invol, 500, replace=TRUE))
          total_proj_val <- vol_proj_val + invol_proj_val
          
          resultado <- rbind(resultado, data.frame(
            ano = ano, mes = mes, data = data_inicio, mes_ano = paste0(meses_pt[mes], "/", substr(ano,3,4)),
            vol_real = NA, invol_real = NA, total_real = NA,
            vol_proj = vol_proj_val, invol_proj = invol_proj_val, total_proj = total_proj_val,
            to_vol_real = NA, to_invol_real = NA, to_total_real = NA,
            to_vol_proj = (vol_proj_val/headcount_medio)*100, to_invol_proj = (invol_proj_val/headcount_medio)*100, to_total_proj = (total_proj_val/headcount_medio)*100
          ))
        }
      }
    }
    return(resultado %>% arrange(data))
  }
  extrair_contexto_da_tag <- function(texto, tags_string) {
    if(is.na(texto) || texto == "") return("Sem relato detalhado.")
    tags_list <- unlist(strsplit(tags_string, "; "))
    tags_list <- tags_list[tags_list != ""]
    
    if(length(tags_list) == 0) return(paste0("\"", substr(texto, 1, 200), "...\""))
    
    tag_busca <- tags_list[1]
    pos <- str_locate(texto, fixed(tag_busca))
    
    if(is.na(pos[1])) return(paste0("\"", substr(texto, 1, 200), "...\""))
    
    inicio <- max(1, pos[1] - 80)
    fim <- min(nchar(texto), pos[2] + 150)
    trecho <- substr(texto, inicio, fim)
    
    trecho <- gsub("\r|\n", " ", trecho)
    
    prefixo <- ifelse(inicio > 1, "... ", "")
    sufixo <- ifelse(fim < nchar(texto), " ...", "")
    
    return(paste0("\"", prefixo, trimws(trecho), sufixo, "\""))
  }
  
  # ========================================================================
  # KPIs
  
  output$kpi_ano_ref <- renderText({ input$ano_referencia })
  
  output$kpi_turnover_total <- renderText({
    df <- dados_filtrados()
    paste0(calcular_turnover(df, as.numeric(input$ano_referencia), "total"), "%")
  })
  
  output$kpi_turnover_total_proj <- renderText({
    df <- dados_filtrados()
    if (as.numeric(input$ano_referencia) == year(Sys.Date())) {
      paste0(calcular_turnover_projetado(df, as.numeric(input$ano_referencia)), "%")
    } else {
      "-"
    }
  })
  
  output$kpi_turnover_voluntario <- renderText({
    df <- dados_filtrados()
    paste0(calcular_turnover(df, as.numeric(input$ano_referencia), "voluntario"), "%")
  })
  
  output$kpi_deslig_voluntarios <- renderText({
    df <- dados_filtrados()
    calcular_desligamentos_ano(df, as.numeric(input$ano_referencia), "voluntario")
  })
  
  output$kpi_turnover_involuntario <- renderText({
    df <- dados_filtrados()
    paste0(calcular_turnover(df, as.numeric(input$ano_referencia), "involuntario"), "%")
  })
  
  output$kpi_deslig_involuntarios <- renderText({
    df <- dados_filtrados()
    calcular_desligamentos_ano(df, as.numeric(input$ano_referencia), "involuntario")
  })
  
  output$kpi_headcount_atual <- renderText({
    df <- dados_filtrados()
    data_fim <- as.Date(paste0(input$ano_referencia, "-12-31"))
    if(data_fim > Sys.Date()) data_fim <- Sys.Date()
    # Aviso format resolvido tirando duplo mark
    format(calcular_headcount(df, data_fim), big.mark = ".")
  })
  
  # ========================================================================
  # GRÁFICOS - VISÃO GERAL
  
  output$grafico_evolucao_mensal <- renderPlotly({
    df <- dados_filtrados()
    req(df)
    
    if (input$visao_evolucao_mensal == "Mensal") {
      anos <- c(2024, 2025, 2026) 
    } else {
      anos <- 2020:2026     
    }
    
    evolucao <- calcular_evolucao_mensal(df, anos)
    if (nrow(evolucao) == 0) return(plot_vazio())
    
    if (input$visao_evolucao_mensal == "Anual") {
      evol_anual <- data.frame()
      for(a in unique(evolucao$ano)) {
        hc_ano <- calcular_headcount_fim_ano(df, a)
        if(hc_ano == 0) next
        v_r <- sum(evolucao$vol_real[evolucao$ano == a], na.rm=TRUE)
        i_r <- sum(evolucao$invol_real[evolucao$ano == a], na.rm=TRUE)
        v_p <- sum(evolucao$vol_proj[evolucao$ano == a], na.rm=TRUE)
        i_p <- sum(evolucao$invol_proj[evolucao$ano == a], na.rm=TRUE)
        
        evol_anual <- rbind(evol_anual, data.frame(
          ano = a, mes_ano = as.character(a),
          vol_real = v_r, invol_real = i_r,
          vol_proj = ifelse(v_p>0, v_p, NA), invol_proj = ifelse(i_p>0, i_p, NA),
          to_vol_real = (v_r/hc_ano)*100, to_invol_real = (i_r/hc_ano)*100, to_total_real = ((v_r+i_r)/hc_ano)*100,
          to_vol_proj = ifelse(v_p>0, (v_p/hc_ano)*100, NA), to_invol_proj = ifelse(i_p>0, (i_p/hc_ano)*100, NA), to_total_proj = ifelse((v_p+i_p)>0, ((v_p+i_p)/hc_ano)*100, NA)
        ))
      }
      evolucao <- evol_anual
    }
    
    evolucao$mes_ano <- factor(evolucao$mes_ano, levels = unique(evolucao$mes_ano))
    
    if(any(is.na(evolucao$to_vol_real))) {
      last_real_idx <- max(which(!is.na(evolucao$to_vol_real)))
      evolucao$to_vol_proj[last_real_idx] <- evolucao$to_vol_real[last_real_idx]
      evolucao$to_invol_proj[last_real_idx] <- evolucao$to_invol_real[last_real_idx]
      evolucao$to_total_proj[last_real_idx] <- evolucao$to_total_real[last_real_idx]
    }
    
    plot_ly(evolucao, x = ~mes_ano) %>%
      add_bars(y = ~vol_real, name = "Voluntários (Real)", marker = list(color = cor_vol), yaxis = "y1") %>%
      add_bars(y = ~invol_real, name = "Involuntários (Real)", marker = list(color = cor_invol), yaxis = "y1") %>%
      add_bars(y = ~vol_proj, name = "Voluntários (Proj)", marker = list(color = cor_vol_proj, pattern=list(shape="/")), yaxis = "y1") %>%
      add_bars(y = ~invol_proj, name = "Involuntários (Proj)", marker = list(color = cor_invol_proj, pattern=list(shape="/")), yaxis = "y1") %>%
      
      add_lines(y = ~to_vol_real, name = "Vol % (Real)", line = list(color = cor_vol, width = 2), yaxis = "y2", connectgaps=FALSE) %>%
      add_lines(y = ~to_invol_real, name = "Invol % (Real)", line = list(color = cor_invol, width = 2), yaxis = "y2", connectgaps=FALSE) %>%
      add_lines(y = ~to_total_real, name = "Total % (Real)", line = list(color = cor_total, width = 3), yaxis = "y2", connectgaps=FALSE) %>%
      
      add_lines(y = ~to_vol_proj, name = "Vol % (Proj)", line = list(color = cor_vol_proj, width = 2, dash="dot"), yaxis = "y2", connectgaps=FALSE) %>%
      add_lines(y = ~to_invol_proj, name = "Invol % (Proj)", line = list(color = cor_invol_proj, width = 2, dash="dot"), yaxis = "y2", connectgaps=FALSE) %>%
      add_lines(y = ~to_total_proj, name = "Total % (Proj)", line = list(color = cor_total_proj, width = 3, dash="dot"), yaxis = "y2", connectgaps=FALSE) %>%
      
      layout(plot_bgcolor = tema_bg, paper_bgcolor = tema_bg, font = list(color = tema_texto), barmode = "stack", hovermode = "x unified",
             xaxis = list(title = "", tickangle = -45, showgrid = FALSE),
             yaxis = list(title = "Volume", side = "left", showgrid = TRUE, gridcolor = tema_panel),
             yaxis2 = list(title = "Turnover (%)", side = "right", overlaying = "y", showgrid = FALSE, tickformat = ".1f"),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.2))
  })
  
  output$grafico_distribuicao_tipo <- renderPlotly({
    df <- dados_filtrados()
    ano_ref <- as.numeric(input$ano_referencia)
    total <- calcular_desligamentos_ano(df, ano_ref, "total")
    vol <- calcular_desligamentos_ano(df, ano_ref, "voluntario")
    invol <- calcular_desligamentos_ano(df, ano_ref, "involuntario")
    
    if (total == 0) return(plot_vazio())
    
    dados <- data.frame(Tipo = c("Voluntário", "Involuntário"), Quantidade = c(vol, invol))
    plot_ly(dados, labels = ~Tipo, values = ~Quantidade, type = "pie", hole = 0.4,
            marker = list(colors = c(cor_vol, cor_invol), line = list(color = tema_bg, width = 2)), textinfo = "label+percent") %>%
      layout(plot_bgcolor = tema_bg, paper_bgcolor = tema_bg, font = list(color = tema_texto), showlegend = FALSE)
  })
  
  output$grafico_projetado_vs_real <- renderPlotly({
    df <- dados_filtrados()
    ano_ref <- as.numeric(input$ano_referencia)
    
    evolucao <- calcular_evolucao_mensal(df, c(ano_ref))
    if (nrow(evolucao) == 0) return(plot_vazio())
    
    tot_proj_ano <- calcular_turnover_projetado(df, ano_ref)
    
    evolucao$mes_ano <- factor(evolucao$mes_ano, levels = unique(evolucao$mes_ano))
    evolucao$linha_proj_ano <- tot_proj_ano
    
    if(any(is.na(evolucao$to_total_real))) {
      last_real_idx <- max(which(!is.na(evolucao$to_total_real)))
      if(last_real_idx < 12) {
        evolucao$to_total_proj[last_real_idx] <- evolucao$to_total_real[last_real_idx]
      }
    }
    
    plot_ly(evolucao, x = ~mes_ano) %>%
      add_bars(y = ~to_vol_real, name = "Voluntário (Real)", marker = list(color = cor_vol)) %>%
      add_bars(y = ~to_invol_real, name = "Involuntário (Real)", marker = list(color = cor_invol)) %>%
      add_bars(y = ~to_vol_proj, name = "Voluntário (Proj)", marker = list(color = cor_vol_proj, pattern=list(shape="/"))) %>%
      add_bars(y = ~to_invol_proj, name = "Involuntário (Proj)", marker = list(color = cor_invol_proj, pattern=list(shape="/"))) %>%
      add_lines(y = ~to_total_real, name = "Total (Real)", line = list(color = cor_total, width = 3), connectgaps=FALSE) %>%
      add_lines(y = ~to_total_proj, name = "Total (Proj Bootstrap)", line = list(color = cor_total_proj, width = 3, dash = "dash"), connectgaps=FALSE) %>%
      add_lines(y = ~linha_proj_ano, name = paste0("Total Projetado Ano (", tot_proj_ano, "%)"),
                line = list(color = cor_azul, dash = "dot", width = 2)) %>%
      layout(plot_bgcolor = tema_bg, paper_bgcolor = tema_bg, font = list(color = tema_texto),
             barmode = "stack", hovermode="x unified",
             xaxis = list(title = "", tickangle=-45), yaxis = list(title = "Turnover (%)", gridcolor = tema_panel), 
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1))
  })
  
  output$grafico_top_rankings <- renderPlotly({
    df <- dados_filtrados()
    ano_ref <- as.numeric(input$ano_referencia)
    tipo <- input$tipo_turnover
    ranking <- ranking_selecionado()
    
    nome_col <- switch(ranking, "area" = "area", "equipe" = "equipe", "gestor" = "nome_gestor")
    
    dados <- calcular_turnover_por_grupo(df, ano_ref, nome_col, tipo)
    if (nrow(dados) == 0) return(plot_vazio())
    
    if (ranking == "gestor") dados <- dados %>% filter(nome_gestor != "Não informado")
    dados <- dados %>% head(10)
    
    dados$cor <- case_when(dados$turnover > 20 ~ cor_invol, dados$turnover > 15 ~ cor_vol, dados$turnover > 10 ~ cor_azul, TRUE ~ "#27ae60")
    
    plot_ly(dados, x = ~reorder(.data[[nome_col]], turnover), y = ~turnover, type = "bar", marker = list(color = ~cor)) %>%
      layout(plot_bgcolor = tema_bg, paper_bgcolor = tema_bg, font = list(color = tema_texto),
             xaxis = list(title = "", tickangle = -45), yaxis = list(title = "Turnover (%)", gridcolor = tema_panel), showlegend = FALSE)
  })
  
  output$tabela_areas_criticas <- renderDT({
    df <- dados_filtrados()
    ano_ref <- as.numeric(input$ano_referencia)
    dados_area <- calcular_turnover_por_grupo(df, ano_ref, "area", input$tipo_turnover)
    
    if (nrow(dados_area) == 0) return(datatable(data.frame(Mensagem = "Sem dados para essa seleção"), options = list(dom = 't'), rownames = FALSE))
    
    dt <- dados_area %>% filter(turnover > 15) %>% arrange(desc(turnover)) %>%
      mutate(status = case_when(turnover > 20 ~ "🔴 Crítico", TRUE ~ "🟡 Atenção"), turnover = round(turnover, 1)) %>%
      select(Área = area, Headcount = headcount, Desligamentos = desligamentos, `Turnover %` = turnover, Status = status)
    
    if (nrow(dt) == 0) return(datatable(data.frame(Mensagem = "Sem áreas críticas para esta seleção"), options = list(dom = 't'), rownames = FALSE))
    
    datatable(dt, options = list(pageLength = 5, dom = 't', scrollX = TRUE), rownames = FALSE, class = "display compact") %>%
      formatStyle('Turnover %', backgroundColor = cor_invol, color = 'white') # Valores maiores que 15 diretamente para vermelho
  }, server = FALSE)
  
  output$tabela_areas_ok <- renderDT({
    df <- dados_filtrados()
    ano_ref <- as.numeric(input$ano_referencia)
    dados_area <- calcular_turnover_por_grupo(df, ano_ref, "area", input$tipo_turnover)
    
    if (nrow(dados_area) == 0) return(datatable(data.frame(Mensagem = "Sem dados para essa seleção"), options = list(dom = 't'), rownames = FALSE))
    
    dt <- dados_area %>% 
      filter(turnover <= 15 & !grepl("diretoria", tolower(area))) %>% 
      arrange(turnover) %>%
      mutate(status = case_when(turnover <= 10 ~ "🟢 Saudável", TRUE ~ "🟡 Moderado"), turnover = round(turnover, 1)) %>%
      select(Área = area, Headcount = headcount, Desligamentos = desligamentos, `Turnover %` = turnover, Status = status)
    
    if (nrow(dt) == 0) return(datatable(data.frame(Mensagem = "Sem áreas saudáveis para esta seleção"), options = list(dom = 't'), rownames = FALSE))
    
    datatable(dt, options = list(pageLength = 5, dom = 't', scrollX = TRUE), rownames = FALSE, class = "display compact") %>%
      formatStyle('Turnover %', backgroundColor = styleInterval(c(10, 15), c('#2ecc71', cor_vol, cor_invol)), color = 'white')
  }, server = FALSE)
  
  
  output$grafico_hc_entradas_saidas <- renderPlotly({
    df <- dados_filtrados()
    req(df)
    ano_ref <- as.numeric(input$ano_referencia)
    
    if (input$visao_hc_entradas == "Mensal") {
      anos <- c(ano_ref-1, ano_ref)
    } else {
      anos <- 2020:2026             
    }
    
    evolucao <- calcular_evolucao_mensal(df, anos)
    if (nrow(evolucao) == 0) return(plot_vazio())
    
    evolucao <- evolucao %>% filter(data <= Sys.Date() | ano < year(Sys.Date()))
    
    if (input$visao_hc_entradas == "Anual") {
      evol_anual <- data.frame()
      for(a in unique(evolucao$ano)) {
        hc_ano <- calcular_headcount_fim_ano(df, a)
        if(hc_ano == 0) next
        
        v_r <- sum(evolucao$vol_real[evolucao$ano == a], na.rm=TRUE)
        i_r <- sum(evolucao$invol_real[evolucao$ano == a], na.rm=TRUE)
        
        evol_anual <- rbind(evol_anual, data.frame(
          ano = a, 
          mes_ano = as.character(a),
          vol_real = v_r, 
          invol_real = i_r,
          to_total_real = ((v_r+i_r)/hc_ano)*100,
          to_vol_real = (v_r/hc_ano)*100, 
          to_invol_real = (i_r/hc_ano)*100
        ))
      }
      evolucao <- evol_anual
    }
    
    evolucao$mes_ano <- factor(evolucao$mes_ano, levels = unique(evolucao$mes_ano))
    
    plot_ly(evolucao, x = ~mes_ano) %>%
      add_bars(y = ~vol_real, name = "Desligamentos Voluntários", marker = list(color = cor_vol), yaxis = "y1") %>%
      add_bars(y = ~invol_real, name = "Desligamentos Involuntários", marker = list(color = cor_invol), yaxis = "y1") %>%
      add_lines(y = ~to_total_real, name = "TO% Total", line = list(color = cor_total, width = 3), yaxis = "y2") %>%
      add_lines(y = ~to_vol_real, name = "TO% Vol", line = list(color = cor_vol, width = 2, dash = "dash"), yaxis = "y2") %>%
      add_lines(y = ~to_invol_real, name = "TO% Invol", line = list(color = cor_invol, width = 2, dash = "dash"), yaxis = "y2") %>%
      layout(plot_bgcolor = tema_bg, paper_bgcolor = tema_bg, font = list(color = tema_texto), barmode = "stack", hovermode = "x unified",
             xaxis = list(title = "", tickangle = -45, showgrid = FALSE),
             yaxis = list(title = "Quantidade", side = "left", showgrid = TRUE, gridcolor = tema_panel),
             yaxis2 = list(title = "Turnover (%)", side = "right", overlaying = "y", showgrid = FALSE, tickformat = ".1f"),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.2))
  })
  
  output$grafico_hc_lideranca <- renderPlotly({
    df <- dados_filtrados()
    req(df)
    anos <- 2003:2026
    resultado <- data.frame()
    
    for(ano in anos) {
      d_fim <- as.Date(paste0(ano, "-12-31"))
      if(d_fim > Sys.Date()) d_fim <- Sys.Date()
      
      hc_total <- df %>% filter(data_admissao <= d_fim, (is.na(data_desligamento) | data_desligamento > d_fim)) %>% nrow()
      
      resultado <- rbind(resultado, data.frame(
        ano = as.character(ano),
        hc_total = hc_total
      ))
    }
    
    if(nrow(resultado) == 0) return(plot_vazio())
    
    resultado$ano <- factor(resultado$ano, levels = unique(resultado$ano))
    
    plot_ly(resultado, x = ~ano) %>%
      add_lines(y = ~hc_total, name = "Headcount Total", line = list(color = cor_azul, width = 3)) %>%
      layout(plot_bgcolor = tema_bg, paper_bgcolor = tema_bg, font = list(color = tema_texto), hovermode = "x unified",
             xaxis = list(title = "", tickangle = -45), yaxis = list(title = "Headcount", gridcolor = tema_panel),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1))
  })
  
  output$grafico_turnover_area_detalhado <- renderPlotly({
    df <- dados_filtrados()
    ano_ref <- as.numeric(input$ano_referencia)
    
    data_fim <- as.Date(paste0(ano_ref, "-12-31"))
    if(data_fim > Sys.Date()) data_fim <- Sys.Date()
    
    hc_df <- df %>% filter(!is.na(data_admissao), data_admissao <= data_fim, (is.na(data_desligamento) | data_desligamento > data_fim), !is.na(area), area != "Não informado") %>% count(area, name = "hc")
    
    deslig_df <- df %>% filter(!is.na(data_desligamento), year(data_desligamento) == ano_ref, !is.na(area), area != "Não informado") %>% group_by(area) %>% summarise(deslig_vol = sum(motivo == "voluntário", na.rm = TRUE), deslig_invol = sum(motivo == "involuntário", na.rm = TRUE), .groups = 'drop')
    
    areas <- full_join(hc_df, deslig_df, by = "area") %>% mutate(hc = replace_na(hc, 0), deslig_vol = replace_na(deslig_vol, 0), deslig_invol = replace_na(deslig_invol, 0)) %>% filter(hc > 0) %>% mutate(to_vol = (deslig_vol/hc)*100, to_invol = (deslig_invol/hc)*100, to_total = to_vol + to_invol) %>% filter(to_total > 0) %>% arrange(desc(to_total)) %>% head(15)
    
    if (nrow(areas) == 0) return(plot_vazio())
    
    plot_ly(areas, x = ~reorder(area, to_total)) %>%
      add_bars(y = ~to_vol, name = "Voluntário", marker = list(color = cor_vol)) %>%
      add_bars(y = ~to_invol, name = "Involuntário", marker = list(color = cor_invol)) %>%
      layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), barmode="stack", 
             xaxis=list(title="", tickangle=-45), yaxis=list(title="Turnover (%)", gridcolor=tema_panel), legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1))
  })
  
  output$grafico_turnover_gestor_detalhado <- renderPlotly({
    df <- dados_filtrados()
    ano_ref <- as.numeric(input$ano_referencia)
    
    data_fim <- as.Date(paste0(ano_ref, "-12-31"))
    if(data_fim > Sys.Date()) data_fim <- Sys.Date()
    
    hc_df <- df %>% filter(!is.na(data_admissao), data_admissao <= data_fim, (is.na(data_desligamento) | data_desligamento > data_fim), !is.na(nome_gestor), nome_gestor != "Não informado") %>% count(nome_gestor, name = "hc")
    
    deslig_df <- df %>% filter(!is.na(data_desligamento), year(data_desligamento) == ano_ref, !is.na(nome_gestor), nome_gestor != "Não informado") %>% group_by(nome_gestor) %>% summarise(deslig_vol = sum(motivo == "voluntário", na.rm = TRUE), deslig_invol = sum(motivo == "involuntário", na.rm = TRUE), .groups = 'drop')
    
    gestores <- full_join(hc_df, deslig_df, by = "nome_gestor") %>% mutate(hc = replace_na(hc, 0), deslig_vol = replace_na(deslig_vol, 0), deslig_invol = replace_na(deslig_invol, 0)) %>% filter(hc > 0) %>% mutate(to_vol = (deslig_vol/hc)*100, to_invol = (deslig_invol/hc)*100, to_total = to_vol + to_invol) %>% filter(to_total > 0) %>% arrange(desc(to_total)) %>% head(15)
    
    if (nrow(gestores) == 0) return(plot_vazio())
    
    plot_ly(gestores, x = ~reorder(nome_gestor, to_total)) %>%
      add_bars(y = ~to_vol, name = "Voluntário", marker = list(color = cor_vol)) %>%
      add_bars(y = ~to_invol, name = "Involuntário", marker = list(color = cor_invol)) %>%
      layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), barmode="stack", 
             xaxis=list(title="", tickangle=-45), yaxis=list(title="Turnover (%)", gridcolor=tema_panel), legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1))
  })
  
  output$tabela_matriz_turnover <- renderDT({
    df <- dados_filtrados()
    ano_ref <- as.numeric(input$ano_referencia)
    
    data_fim <- as.Date(paste0(ano_ref, "-12-31"))
    if(data_fim > Sys.Date()) data_fim <- Sys.Date()
    
    hc_df <- df %>% filter(!is.na(data_admissao), data_admissao <= data_fim, (is.na(data_desligamento) | data_desligamento > data_fim), !is.na(area), !is.na(equipe)) %>% count(area, equipe, name = "Headcount")
    
    deslig_df <- df %>% filter(!is.na(data_desligamento), year(data_desligamento) == ano_ref, !is.na(area), !is.na(equipe)) %>% count(area, equipe, name = "Desligamentos")
    
    matriz <- full_join(hc_df, deslig_df, by = c("area", "equipe")) %>% mutate(Headcount = replace_na(Headcount, 0), Desligamentos = replace_na(Desligamentos, 0)) %>% filter(Headcount > 0) %>% mutate(Turnover = round((Desligamentos / Headcount) * 100, 1)) %>% rename(Área = area, Equipe = equipe) %>% arrange(Área, desc(Turnover))
    
    if (nrow(matriz) == 0) return(datatable(data.frame(Mensagem = "Sem dados para essa seleção"), options = list(dom = 't'), rownames = FALSE))
    
    datatable(matriz, options=list(pageLength=10, scrollX=TRUE, dom='frtip'), rownames=FALSE, class="display compact stripe") %>%
      formatStyle('Turnover', backgroundColor = styleInterval(c(10, 15), c('#27ae60', cor_vol, cor_invol)), color = 'white')
  }, server = FALSE)
  
  output$tabela_area_ano_lideranca <- renderDT({
    df <- dados_filtrados()
    req(df)
    ano_ref <- as.numeric(input$ano_referencia)
    anos <- seq(max(2020, ano_ref - 2), ano_ref)
    
    resultados <- list()
    for(ano in anos) {
      data_fim <- as.Date(paste0(ano, "-12-31"))
      if(data_fim > Sys.Date()) data_fim <- Sys.Date()
      
      hc_total <- df %>% filter(data_admissao <= data_fim, (is.na(data_desligamento) | data_desligamento > data_fim)) %>% count(area, name="hc_tot")
      hc_lid <- df %>% filter(e_lideranca=="Sim", data_admissao <= data_fim, (is.na(data_desligamento) | data_desligamento > data_fim)) %>% count(area, name="hc_lid")
      
      deslig_tot <- df %>% filter(!is.na(data_desligamento), year(data_desligamento)==ano) %>% count(area, name="d_tot")
      deslig_lid <- df %>% filter(!is.na(data_desligamento), year(data_desligamento)==ano, e_lideranca=="Sim") %>% count(area, name="d_lid")
      
      base_area <- data.frame(area = unique(df$area[!is.na(df$area) & df$area != "Não informado"]))
      temp <- base_area %>%
        left_join(hc_total, by="area") %>% left_join(hc_lid, by="area") %>%
        left_join(deslig_tot, by="area") %>% left_join(deslig_lid, by="area") %>%
        replace(is.na(.), 0) %>%
        mutate(
          !!paste0(ano, "_Total") := ifelse(hc_tot > 0, round((d_tot/hc_tot)*100, 1), 0),
          !!paste0(ano, "_Lideranças") := ifelse(hc_lid > 0, round((d_lid/hc_lid)*100, 1), 0)
        ) %>%
        select(area, starts_with(as.character(ano)))
      
      resultados[[as.character(ano)]] <- temp
    }
    
    final_df <- purrr::reduce(resultados, full_join, by="area")
    final_df[is.na(final_df)] <- 0
    
    val_cols <- setdiff(names(final_df), "area")
    max_val <- max(final_df[, val_cols], na.rm = TRUE)
    brks <- seq(0.1, max(20, max_val), length.out = 10)
    clrs <- colorRampPalette(c(tema_panel, cor_invol))(11)
    
    sketch <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Área Atual', style="text-align:left; vertical-align:middle; border-bottom:1px solid #46637f;"),
          lapply(anos, function(a) th(colspan = 2, style="text-align:center; border-bottom:1px solid #46637f;", as.character(a)))
        ),
        tr(
          lapply(anos, function(a) { list(th('Total', style="text-align:center;"), th('Lideranças', style="text-align:center;")) })
        )
      )
    ))
    
    datatable(final_df, container = sketch, rownames = FALSE, class = "display compact stripe",
              options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip')) %>%
      formatStyle(val_cols, backgroundColor = styleInterval(brks, clrs), color = "white")
  }, server = FALSE)
  
  output$grafico_serie_historica <- renderPlotly({
    df <- dados_filtrados()
    anos <- seq(as.numeric(input$ano_referencia)-6, as.numeric(input$ano_referencia))
    anos <- anos[anos >= 2003]
    serie <- calcular_evolucao_mensal(df, anos)
    if (nrow(serie) == 0) return(plot_vazio())
    
    serie[is.na(serie)] <- 0
    serie$media_movel <- rollmean(serie$to_total_real, 3, fill = NA, align = "right")
    serie$mes_ano <- factor(serie$mes_ano, levels = unique(serie$mes_ano))
    
    plot_ly(serie, x = ~mes_ano) %>%
      add_bars(y = ~to_vol_real, name = "Voluntário", marker = list(color = cor_vol)) %>%
      add_bars(y = ~to_invol_real, name = "Involuntário", marker = list(color = cor_invol)) %>%
      add_lines(y = ~media_movel, name = "Média Móvel", line = list(color = cor_total, width = 3)) %>%
      layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), barmode="stack", xaxis=list(title="", tickangle=-45, showgrid=FALSE), yaxis=list(title="Turnover (%)", gridcolor=tema_panel))
  })
  
  output$grafico_desligamentos_mensal <- renderPlotly({
    df <- dados_filtrados()
    ano_ref <- as.numeric(input$ano_referencia)
    dados_mensal <- data.frame()
    meses_pt <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
    
    for (mes in 1:12) {
      d_ini <- as.Date(paste0(ano_ref, "-", mes, "-01"))
      d_fim <- ceiling_date(d_ini, "month") - days(1)
      if (d_ini > Sys.Date() & ano_ref == year(Sys.Date())) break 
      vol <- calcular_desligamentos_periodo(df, d_ini, d_fim, "voluntario")
      invol <- calcular_desligamentos_periodo(df, d_ini, d_fim, "involuntario")
      dados_mensal <- rbind(dados_mensal, data.frame(mes_ano=meses_pt[mes], vol=vol, invol=invol, total=vol+invol))
    }
    if (nrow(dados_mensal) == 0) return(plot_vazio())
    dados_mensal$mes_ano <- factor(dados_mensal$mes_ano, levels=meses_pt)
    plot_ly(dados_mensal, x=~mes_ano) %>%
      add_lines(y=~vol, name="Vol", line=list(color=cor_vol, shape='spline', width=3), mode='lines+markers', marker=list(size=8)) %>%
      add_lines(y=~invol, name="Invol", line=list(color=cor_invol, shape='spline', width=3), mode='lines+markers', marker=list(size=8)) %>%
      add_lines(y=~total, name="Total", line=list(color=cor_total, shape='spline', width=2, dash='dot'), mode='lines') %>%
      layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), hovermode="x unified", xaxis=list(title="", showgrid=FALSE), yaxis=list(title="Desligamentos", gridcolor=tema_panel))
  })
  
  output$grafico_periodo_casa <- renderPlotly({
    df <- dados_filtrados() %>% filter(year(data_desligamento) == as.numeric(input$ano_referencia))
    if(nrow(df) == 0) return(plot_vazio())
    periodo <- df %>% group_by(periodo_casa, motivo) %>% summarise(qtd=n(), .groups='drop') %>%
      mutate(periodo_casa = factor(periodo_casa, levels=c("Até 90 dias", "91-180 dias", "6-12 meses", "1-2 anos", "Mais de 2 anos", "Não informado")))
    plot_ly(periodo, x=~periodo_casa, y=~qtd, color=~motivo, colors=c("voluntário"=cor_vol, "involuntário"=cor_invol), type="bar") %>%
      layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), barmode="group", xaxis=list(title="", tickangle=-45), yaxis=list(gridcolor=tema_panel))
  })
  
  output$grafico_comparativo_anual <- renderPlotly({
    df <- dados_filtrados()
    anos <- seq(as.numeric(input$ano_referencia)-3, as.numeric(input$ano_referencia))
    anos <- anos[anos >= 2020]
    comp <- data.frame()
    for (a in anos) {
      comp <- rbind(comp, data.frame(ano=as.character(a), tipo="Total", to=calcular_turnover(df, a, "total")))
      comp <- rbind(comp, data.frame(ano=as.character(a), tipo="Voluntário", to=calcular_turnover(df, a, "voluntario")))
      comp <- rbind(comp, data.frame(ano=as.character(a), tipo="Involuntário", to=calcular_turnover(df, a, "involuntario")))
    }
    if (nrow(comp) == 0) return(plot_vazio())
    plot_ly(comp, x=~ano, y=~to, color=~tipo, colors=c("Total"=cor_azul, "Voluntário"=cor_vol, "Involuntário"=cor_invol), type="bar") %>%
      layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), barmode="group", xaxis=list(title=""), yaxis=list(gridcolor=tema_panel))
  })
  
  output$grafico_ytd_comparativo <- renderPlotly({
    df <- dados_filtrados()
    ano_ref <- as.numeric(input$ano_referencia)
    hoje <- Sys.Date()
    if (ano_ref != year(hoje)) return(plot_ly() %>% layout(title=list(text="Disponível apenas para o ano atual (YTD)", font=list(color=tema_texto)), xaxis=list(visible=F), yaxis=list(visible=F), plot_bgcolor=tema_bg, paper_bgcolor=tema_bg))
    
    evolucao <- calcular_evolucao_mensal(df, c(ano_ref))
    if(nrow(evolucao) == 0) return(plot_vazio())
    
    tipo <- input$tipo_turnover
    if(tipo == "voluntario") {
      evolucao$val_real <- evolucao$vol_real
      evolucao$val_proj <- evolucao$vol_proj
    } else if (tipo == "involuntario") {
      evolucao$val_real <- evolucao$invol_real
      evolucao$val_proj <- evolucao$invol_proj
    } else {
      evolucao$val_real <- evolucao$total_real
      evolucao$val_proj <- evolucao$total_proj
    }
    
    evolucao$cum_real <- cumsum(ifelse(is.na(evolucao$val_real), 0, evolucao$val_real))
    evolucao$cum_real[is.na(evolucao$val_real)] <- NA
    
    last_real_idx <- max(which(!is.na(evolucao$val_real)))
    last_real_val <- evolucao$cum_real[last_real_idx]
    
    evolucao$cum_proj <- NA
    evolucao$cum_proj[last_real_idx] <- last_real_val
    
    if(last_real_idx < 12) {
      for(i in (last_real_idx + 1):12) {
        evolucao$cum_proj[i] <- evolucao$cum_proj[i-1] + evolucao$val_proj[i]
      }
    }
    
    evolucao$mes_ano <- factor(evolucao$mes_ano, levels=unique(evolucao$mes_ano))
    
    plot_ly(evolucao, x=~mes_ano) %>%
      add_lines(y=~cum_real, name="Acumulado Real", line=list(color=cor_azul, width=4, shape="spline"), fill="tozeroy", fillcolor="rgba(52, 152, 219, 0.2)") %>%
      add_lines(y=~cum_proj, name="Projeção (Bootstrap Histórico)", line=list(color="#f39c12", width=3, dash="dash", shape="spline")) %>%
      layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), hovermode="x unified", xaxis=list(title=""), yaxis=list(title="Desligamentos Acumulados", gridcolor=tema_panel), legend=list(orientation="h", x=0.5, xanchor="center", y=1.1))
  })
  
  # ========================================================================
  # ABAS QUALITATIVAS - EMPRESA
  
  output$kpi_principal_causa <- renderText({
    df <- dados_qual_filtrados()
    if (is.null(df) || nrow(df) == 0 || !"categoria_principal" %in% names(df)) return("N/A")
    causa <- df %>% filter(categoria_principal != "Não identificado", !is.na(categoria_principal)) %>% count(categoria_principal, sort=TRUE) %>% slice(1) %>% pull(categoria_principal)
    if (length(causa) == 0) return("N/A") else return(tools::toTitleCase(as.character(causa)))
  })
  output$kpi_sentimento_medio <- renderText({
    df <- dados_qual_filtrados()
    if (is.null(df) || nrow(df) == 0 || !"sentimento_score" %in% names(df)) return("N/A")
    round(mean(df$sentimento_score, na.rm=TRUE), 2)
  })
  output$kpi_total_tags <- renderText({
    df <- dados_qual_filtrados()
    if (is.null(df) || nrow(df) == 0 || !"total_tags" %in% names(df)) return("0")
    format(sum(df$total_tags, na.rm=TRUE), big.mark=".")
  })
  output$kpi_casos_qualitativos <- renderText({
    df <- dados_qual_filtrados()
    if (is.null(df)) return("0")
    format(nrow(df), big.mark=".")
  })
  
  output$grafico_distribuicao_causas <- renderPlotly({
    df <- dados_qual_filtrados()
    if (is.null(df) || nrow(df) == 0 || !"categoria_principal" %in% names(df)) return(plot_vazio())
    
    dist <- df %>% filter(area != "Não informado", !is.na(categoria_principal), categoria_principal != "Não identificado") %>% count(area, categoria_principal)
    if(nrow(dist)==0) return(plot_vazio())
    
    plot_ly(dist, x=~area, y=~n, color=~categoria_principal, type="bar") %>% 
      layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), barmode="stack", xaxis=list(title="", tickangle=-45))
  })
  
  output$grafico_sentimento_categorias <- renderPlotly({
    df <- dados_qual_filtrados()
    if (is.null(df) || nrow(df) == 0 || !"categoria_principal" %in% names(df)) return(plot_vazio())
    sent <- df %>% filter(!is.na(categoria_principal), categoria_principal != "Não identificado") %>% group_by(categoria_principal) %>% summarise(s=mean(sentimento_score,na.rm=TRUE), .groups='drop')
    if(nrow(sent)==0) return(plot_vazio())
    plot_ly(sent, x=~categoria_principal, y=~s, type="bar", marker=list(color=ifelse(sent$s<0, cor_invol, cor_vol))) %>% layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), yaxis=list(range=c(-1,1), gridcolor=tema_panel), xaxis=list(title="", tickangle=-45))
  })
  
  output$grafico_motivos_area <- renderPlotly({
    df <- dados_qual_filtrados()
    if (is.null(df) || nrow(df) == 0 || !"categoria_principal" %in% names(df)) return(plot_vazio())
    
    df_plot <- df %>% filter(!is.na(categoria_principal), categoria_principal != "Não identificado", area != "Não informado") %>% count(categoria_principal, area) %>% arrange(desc(n))
    if(nrow(df_plot)==0) return(plot_vazio())
    
    plot_ly(df_plot, x=~reorder(categoria_principal, -n), y=~n, color=~area, type="bar") %>% 
      layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), barmode="stack", xaxis=list(title="", tickangle=-45), yaxis=list(gridcolor=tema_panel))
  })
  
  output$grafico_frequencia_tags <- renderPlotly({
    df <- dados_qual_filtrados()
    if (is.null(df) || nrow(df) == 0 || !("tags_negativas" %in% names(df))) return(plot_vazio())
    todas <- c(unlist(strsplit(paste(df$tags_negativas[!is.na(df$tags_negativas)], collapse="; "), "; ")),
               unlist(strsplit(paste(df$tags_positivas[!is.na(df$tags_positivas)], collapse="; "), "; ")))
    todas <- todas[todas != ""]
    if(length(todas)==0) return(plot_vazio())
    freq <- data.frame(table(todas)) %>% arrange(desc(Freq)) %>% head(15)
    freq$tipo <- ifelse(grepl("_N$", freq$todas), "Negativa", "Positiva")
    freq$todas <- gsub("_[NP]$|#", "", freq$todas)
    plot_ly(freq, x=~reorder(todas, Freq), y=~Freq, color=~tipo, colors=c("Negativa"=cor_invol, "Positiva"=cor_vol), type="bar") %>%
      layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), xaxis=list(title="", tickangle=-45), yaxis=list(gridcolor=tema_panel))
  })
  
  output$cards_insights_qualitativos <- renderUI({
    df <- dados_qual_filtrados()
    if (is.null(df) || nrow(df) == 0) return(div("Sem dados para essa seleção", style="color: white; padding: 20px; text-align: center;"))
    
    casos <- df %>% 
      filter(!is.na(aprofundamento), nchar(aprofundamento) > 20) %>%
      group_by(categoria_principal) %>%
      slice_sample(n = 1) %>%
      ungroup() %>%
      head(3)
    
    if(nrow(casos)==0) return(div("Sem textos analíticos para esta seleção.", style="color: white; padding: 20px; text-align: center;"))
    
    cards <- lapply(1:nrow(casos), function(i) {
      caso <- casos[i,]
      todas_tags_string <- paste(caso$tags_negativas, caso$tags_positivas, sep = "; ")
      
      trecho_inteligente <- extrair_contexto_da_tag(caso$aprofundamento, caso$tags_negativas) 
      
      tags_arr <- c(unlist(strsplit(caso$tags_negativas, "; ")), unlist(strsplit(caso$tags_positivas, "; ")))
      tags_arr <- tags_arr[tags_arr != ""]
      
      tags_html <- lapply(tags_arr, function(t) {
        if (t == "") return(NULL)
        classe <- ifelse(grepl("_N$", t), "insight-tag negative", "insight-tag positive")
        span(class=classe, gsub("_[NP]$", "", t))
      })
      
      div(class="insight-card",
          div(class="insight-category", caso$categoria_principal %||% "Diversos"),
          div(class="insight-text", trecho_inteligente),
          div(class="insight-tags", tags_html)
      )
    })
    div(style="display:grid; grid-template-columns: repeat(3, 1fr); gap: 15px;", cards)
  })
  
  output$tabela_qualitativa_detalhada <- renderDT({
    df <- dados_qual_filtrados()
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame(Mensagem="Sem dados para essa seleção"), options=list(dom='t'), rownames=FALSE))
    datatable(df %>% select(Nome=name, Time=time, Motivo=categoria_principal, Aprofundamento=aprofundamento) %>% head(15), 
              options=list(pageLength=5, scrollX=TRUE, dom='t'), rownames=FALSE, class="display compact stripe")
  }, server = FALSE)
  
  # ========================================================================
  # ABAS QUALITATIVAS - FUNCIONÁRIO 
  
  output$kpi_principal_causa_func <- renderText({
    df <- dados_qual_filtrados_func()
    if (is.null(df) || nrow(df) == 0 || !"categoria_funcionario" %in% names(df)) return("N/A")
    causa <- df %>% filter(categoria_funcionario != "Outros", !is.na(categoria_funcionario)) %>% count(categoria_funcionario, sort=TRUE) %>% slice(1) %>% pull(categoria_funcionario)
    if (length(causa) == 0) return("N/A") else return(tools::toTitleCase(as.character(causa)))
  })
  output$kpi_sentimento_medio_func <- renderText({
    df <- dados_qual_filtrados_func()
    if (is.null(df) || nrow(df) == 0 || !"sentimento_score" %in% names(df)) return("N/A")
    round(mean(df$sentimento_score, na.rm=TRUE), 2)
  })
  output$kpi_total_tags_func <- renderText({
    df <- dados_qual_filtrados_func()
    if (is.null(df) || nrow(df) == 0 || !"total_tags" %in% names(df)) return("0")
    format(sum(df$total_tags, na.rm=TRUE), big.mark=".")
  })
  output$kpi_casos_qualitativos_func <- renderText({
    df <- dados_qual_filtrados_func()
    if (is.null(df)) return("0")
    format(nrow(df), big.mark=".")
  })
  
  output$grafico_distribuicao_causas_func <- renderPlotly({
    df <- dados_qual_filtrados_func()
    if (is.null(df) || nrow(df) == 0 || !"categoria_funcionario" %in% names(df)) return(plot_vazio())
    
    dist <- df %>% filter(area != "Não informado", !is.na(categoria_funcionario)) %>% count(area, categoria_funcionario)
    if(nrow(dist)==0) return(plot_vazio())
    
    plot_ly(dist, x=~area, y=~n, color=~categoria_funcionario, type="bar") %>% 
      layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), barmode="stack", xaxis=list(title="", tickangle=-45))
  })
  
  output$grafico_sentimento_categorias_func <- renderPlotly({
    df <- dados_qual_filtrados_func()
    if (is.null(df) || nrow(df) == 0 || !"categoria_funcionario" %in% names(df)) return(plot_vazio())
    sent <- df %>% filter(!is.na(categoria_funcionario)) %>% group_by(categoria_funcionario) %>% summarise(s=mean(sentimento_score,na.rm=TRUE), .groups='drop')
    if(nrow(sent)==0) return(plot_vazio())
    plot_ly(sent, x=~categoria_funcionario, y=~s, type="bar", marker=list(color=ifelse(sent$s<0, cor_invol, cor_vol))) %>% layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), yaxis=list(range=c(-1,1), gridcolor=tema_panel), xaxis=list(title="", tickangle=-45))
  })
  
  output$grafico_motivos_area_func <- renderPlotly({
    df <- dados_qual_filtrados_func()
    if (is.null(df) || nrow(df) == 0 || !"categoria_funcionario" %in% names(df)) return(plot_vazio())
    
    df_plot <- df %>% filter(!is.na(categoria_funcionario), area != "Não informado") %>% count(categoria_funcionario, area) %>% arrange(desc(n))
    if(nrow(df_plot)==0) return(plot_vazio())
    
    plot_ly(df_plot, x=~reorder(categoria_funcionario, -n), y=~n, color=~area, type="bar") %>% 
      layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), barmode="stack", xaxis=list(title="", tickangle=-45), yaxis=list(gridcolor=tema_panel))
  })
  
  output$grafico_frequencia_tags_func <- renderPlotly({
    df <- dados_qual_filtrados_func()
    if (is.null(df) || nrow(df) == 0 || !("tags_negativas" %in% names(df))) return(plot_vazio())
    todas <- c(unlist(strsplit(paste(df$tags_negativas[!is.na(df$tags_negativas)], collapse="; "), "; ")),
               unlist(strsplit(paste(df$tags_positivas[!is.na(df$tags_positivas)], collapse="; "), "; ")))
    todas <- todas[todas != ""]
    if(length(todas)==0) return(plot_vazio())
    freq <- data.frame(table(todas)) %>% arrange(desc(Freq)) %>% head(15)
    freq$tipo <- ifelse(grepl("_N$", freq$todas), "Negativa", "Positiva")
    freq$todas <- gsub("_[NP]$|#", "", freq$todas)
    plot_ly(freq, x=~reorder(todas, Freq), y=~Freq, color=~tipo, colors=c("Negativa"=cor_invol, "Positiva"=cor_vol), type="bar") %>%
      layout(plot_bgcolor=tema_bg, paper_bgcolor=tema_bg, font=list(color=tema_texto), xaxis=list(title="", tickangle=-45), yaxis=list(gridcolor=tema_panel))
  })
  
  output$cards_insights_qualitativos_func <- renderUI({
    df <- dados_qual_filtrados_func()
    if (is.null(df) || nrow(df) == 0) return(div("Sem dados para essa seleção", style="color: white; padding: 20px; text-align: center;"))
    
    casos <- df %>% 
      filter(!is.na(aprofundamento), nchar(aprofundamento) > 20) %>%
      group_by(categoria_funcionario) %>%
      slice_sample(n = 1) %>%
      ungroup() %>%
      head(3)
    
    if(nrow(casos)==0) return(div("Sem textos analíticos para esta seleção.", style="color: white; padding: 20px; text-align: center;"))
    
    cards <- lapply(1:nrow(casos), function(i) {
      caso <- casos[i,]
      todas_tags_string <- paste(caso$tags_negativas, caso$tags_positivas, sep = "; ")
      
      trecho_inteligente <- extrair_contexto_da_tag(caso$aprofundamento, caso$tags_negativas)
      
      tags_arr <- c(unlist(strsplit(caso$tags_negativas, "; ")), unlist(strsplit(caso$tags_positivas, "; ")))
      tags_arr <- tags_arr[tags_arr != ""]
      tags_html <- lapply(tags_arr, function(t) {
        if (t == "") return(NULL)
        classe <- ifelse(grepl("_N$", t), "insight-tag negative", "insight-tag positive")
        span(class=classe, gsub("_[NP]$", "", t))
      })
      
      div(class="insight-card",
          div(class="insight-category", caso$categoria_funcionario %||% "Diversos"),
          div(class="insight-text", trecho_inteligente),
          div(class="insight-tags", tags_html)
      )
    })
    div(style="display:grid; grid-template-columns: repeat(3, 1fr); gap: 15px;", cards)
  })
  
  output$tabela_qualitativa_detalhada_func <- renderDT({
    df <- dados_qual_filtrados_func()
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame(Mensagem="Sem dados para essa seleção"), options=list(dom='t'), rownames=FALSE))
    datatable(df %>% select(Nome=name, Time=time, Motivo_Funcionario=categoria_funcionario, Aprofundamento=aprofundamento) %>% head(15), 
              options=list(pageLength=5, scrollX=TRUE, dom='t'), rownames=FALSE, class="display compact stripe")
  }, server = FALSE)
  
  # ========================================================================
  # TABELA DESLIGAMENTOS
  output$tabela_desligamentos_detalhada <- renderDT({
    df <- dados_filtrados()
    if(is.null(df) || nrow(df) == 0) return(datatable(data.frame(Mensagem="Sem dados para essa seleção"), options=list(dom='t'), rownames=FALSE))
    tabela <- df %>% filter(!is.na(data_desligamento)) %>% select(Nome=nome, Área=area, Cargo=cargo, Admissão=data_admissao, Desligamento=data_desligamento, Gestor=nome_gestor, Motivo=motivo) %>% arrange(desc(Desligamento))
    
    datatable(tabela, options=list(pageLength=15, scrollX=TRUE, dom='frtip'), rownames=FALSE, class="display compact stripe") %>%
      formatStyle('Motivo', backgroundColor = styleEqual(c('voluntário', 'involuntário'), c(cor_vol, cor_invol)), color = 'white')
  }, server = FALSE)
}

shinyApp(ui = ui, server = server)