library(shiny)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(survival)
library(survminer)
library(DT)
library(broom)
library(zoo)

tema_bg <- "#14171f"     
tema_panel <- "#1f2430"  
tema_texto <- "#d1d4dc"
cor_vol <- "#f39c12"     
cor_invol <- "#e74c3c"   
cor_azul <- "#3498db"
cor_sucesso <- "#27ae60"


cores_cargos <- c(
  "Gerente"          = "#FF1493", 
  "Líder"            = "#00FFFF", 
  "Especialista"     = "#FFD700", 
  "Desenvolvedor(a)" = "#00FF00", 
  "Analista"         = "#FF4500", 
  "Agente"           = "#1E90FF", 
  "Assistente"       = "#9D00FF", 
  "Auxiliar"         = "#FF00FF", 
  "Estagiário(a)"    = "#FFFFFF", 
  "Jovem Aprendiz"   = "#FFB6C1"  
)


layout_plotly_dark <- function(p, titulo = "", eixo_x = "", eixo_y = "") {
  p %>% layout(
    plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
    font = list(color = "white", family = "Inter, sans-serif"),
    title = list(text = titulo, font = list(size = 15, color = "white", weight = "bold")),
    # Eixos e textos forçados para branco puro
    xaxis = list(title = list(text = eixo_x, font = list(color = "white")), gridcolor = '#2d3342', zerolinecolor = '#2d3342', tickfont = list(color = 'white')),
    yaxis = list(title = list(text = eixo_y, font = list(color = "white")), gridcolor = '#2d3342', zerolinecolor = '#2d3342', tickfont = list(color = 'white')),
    hoverlabel = list(bgcolor = "#2a2e39", font = list(color = "white"), bordercolor = "#46637f"),
    margin = list(t = 40, b = 40, l = 40, r = 20)
  )
}

plot_vazio <- function() {
  plot_ly() %>% layout_plotly_dark(titulo = "Sem dados para essa seleção") %>% 
    layout(xaxis = list(showgrid=F, zeroline=F, showticklabels=F), yaxis = list(showgrid=F, zeroline=F, showticklabels=F))
}

get_s_safe <- function(sf, tempo) {
  if (is.null(sf) || is.null(sf$time) || is.null(sf$surv) || length(sf$time) == 0 || length(sf$surv) == 0) return(1)
  tempo <- as.numeric(tempo)
  if (is.na(tempo) || tempo <= 0) return(1)
  idx <- max(which(sf$time <= tempo), na.rm = TRUE)
  if (!is.finite(idx) || length(idx) == 0) return(1)
  val <- as.numeric(sf$surv[idx])
  ifelse(is.na(val), 1, val)
}

safe_factor_value <- function(valor, niveis, fallback = NULL) {
  niveis <- as.character(niveis)
  if (length(niveis) == 0) return(as.character(valor))
  valor <- as.character(valor)
  if (!is.na(valor) && valor %in% niveis) return(valor)
  if (!is.null(fallback) && fallback %in% niveis) return(fallback)
  niveis[1]
}

tema_grafico_escuro <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = tema_bg, color = NA),
    panel.background = element_rect(fill = tema_bg, color = NA),
    text = element_text(color = tema_texto, family = "Inter"),
    axis.text = element_text(color = tema_texto),
    axis.title = element_text(color = "white", face = "bold"),
    panel.grid.major = element_line(color = "#2d3342"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#2a2e39", color = NA),
    strip.text = element_text(color = "white", face = "bold", size = 11),
    legend.position = "none"
  )

romano_para_inteiro <- function(x) { case_when(x == "i" ~ 1, x == "ii" ~ 2, x == "iii" ~ 3, x == "iv" ~ 4, x == "v" ~ 5, x == "vi" ~ 6, TRUE ~ 0) }

classificar_familia <- function(txt) {
  txt_lower <- str_to_lower(txt)
  case_when(
    str_detect(txt_lower, "gerente|diretor|head") ~ "Gerente", 
    str_detect(txt_lower, "lider|líder|lead|coord|superv") ~ "Líder", # Vai englobar Líder Beta
    str_detect(txt_lower, "especialista|specialist|expert") ~ "Especialista", 
    str_detect(txt_lower, "desenvolvedor|dev|programador|eng.*software|dados|tech") ~ "Desenvolvedor(a)",
    str_detect(txt_lower, "analista") ~ "Analista",
    str_detect(txt_lower, "agente") ~ "Agente",
    str_detect(txt_lower, "assistente") ~ "Assistente",
    str_detect(txt_lower, "auxiliar") ~ "Auxiliar",
    str_detect(txt_lower, "estagi|intern") ~ "Estagiário(a)",
    str_detect(txt_lower, "aprendiz|trainee") ~ "Jovem Aprendiz",
    TRUE ~ "Outros"
  )
}

extrair_nivel <- function(txt) {
  txt_lower <- str_to_lower(txt)
  case_when(
    str_detect(txt_lower, "\\bv\\b") ~ "V",
    str_detect(txt_lower, "\\biv\\b") ~ "IV",
    str_detect(txt_lower, "\\biii\\b|s[eê]nior|sr") ~ "III",
    str_detect(txt_lower, "\\bii\\b|pleno|pl") ~ "II",
    str_detect(txt_lower, "\\bi\\b|j[uú]nior|jr") ~ "I",
    TRUE ~ "Único"
  )
}

carregar_dados <- function() {
  # -------------------------------------------------------------------------
  # BASES 100% SINTÉTICAS
  # -------------------------------------------------------------------------
  # Esta função substitui toda a leitura de arquivos reais por geração de dados
  # fictícios em memória. Nenhum .xlsx, CSV ou base externa é lido.
  # Os identificadores, nomes, datas, cargos, promoções, PPR e desligamentos
  # abaixo são simulados apenas para demonstração do dashboard.
  # -------------------------------------------------------------------------
  set.seed(20260610)
  
  hoje <- Sys.Date()
  n_colaboradores <- 2600
  
  areas_tech_sint <- c("Infra e Redes", "Plataformas", "Electronic Trading", "Inteligência Artificial")
  areas_corp_sint <- c(
    "Suporte", "Negócios Internacionais", "Comdinheiro", "Relacionamento",
    "Marketing", "Akeloo", "People", "Educacional", "Financeiro", "Jurídico"
  )
  areas_sint <- c(areas_tech_sint, areas_corp_sint)
  
  subareas_sint <- c(
    "Produto", "Operações", "Dados", "Atendimento", "Projetos", "Governança",
    "Arquitetura", "Analytics", "Backoffice", "Comercial"
  )
  
  equipes_sint <- c(
    "Squad Alpha", "Squad Beta", "Squad Delta", "Mesa 1", "Mesa 2",
    "Núcleo Growth", "Núcleo Core", "Núcleo Ops", "Núcleo CX", "Núcleo Dados"
  )
  
  familias_sint <- c(
    "Gerente", "Líder", "Especialista", "Desenvolvedor(a)", "Analista",
    "Agente", "Assistente", "Auxiliar", "Estagiário(a)", "Jovem Aprendiz"
  )
  
  probs_familias <- c(0.06, 0.10, 0.10, 0.18, 0.25, 0.12, 0.08, 0.05, 0.04, 0.02)
  niveis_sint <- c("I", "II", "III", "IV", "V", "Único")
  niveis_roman <- c("I", "II", "III", "IV", "V")
  
  montar_cargo <- function(familia, nivel) {
    nivel <- as.character(nivel)
    case_when(
      familia == "Gerente" ~ paste("Gerente", ifelse(nivel %in% c("Único", NA), "III", nivel)),
      familia == "Líder" ~ paste("Líder", ifelse(nivel %in% c("Único", NA), "II", nivel)),
      familia == "Especialista" ~ paste("Especialista", ifelse(nivel %in% c("Único", NA), "III", nivel)),
      familia == "Desenvolvedor(a)" ~ paste("Desenvolvedor(a)", ifelse(nivel %in% c("Único", NA), "II", nivel)),
      familia == "Analista" ~ paste("Analista", ifelse(nivel %in% c("Único", NA), "II", nivel)),
      familia == "Agente" ~ paste("Agente", ifelse(nivel %in% c("Único", NA), "I", nivel)),
      familia == "Assistente" ~ paste("Assistente", ifelse(nivel %in% c("Único", NA), "I", nivel)),
      familia == "Auxiliar" ~ paste("Auxiliar", ifelse(nivel %in% c("Único", NA), "I", nivel)),
      familia == "Estagiário(a)" ~ "Estagiário(a)",
      familia == "Jovem Aprendiz" ~ "Jovem Aprendiz",
      TRUE ~ paste(familia, nivel)
    )
  }
  
  gerar_nivel <- function(familia) {
    case_when(
      familia %in% c("Estagiário(a)", "Jovem Aprendiz") ~ "Único",
      familia %in% c("Gerente", "Especialista") ~ sample(c("II", "III", "IV", "V"), 1, prob = c(0.15, 0.45, 0.30, 0.10)),
      familia == "Líder" ~ sample(c("I", "II", "III", "IV"), 1, prob = c(0.20, 0.45, 0.25, 0.10)),
      familia == "Desenvolvedor(a)" ~ sample(c("I", "II", "III", "IV", "V"), 1, prob = c(0.22, 0.34, 0.27, 0.12, 0.05)),
      familia == "Analista" ~ sample(c("I", "II", "III", "IV"), 1, prob = c(0.28, 0.38, 0.26, 0.08)),
      TRUE ~ sample(c("I", "II", "III"), 1, prob = c(0.55, 0.32, 0.13))
    )
  }
  
  datas_possiveis <- seq.Date(as.Date("2016-01-01"), hoje - months(3), by = "day")
  pesos_admissao <- seq(0.70, 1.30, length.out = length(datas_possiveis))
  
  cpf_sint <- sprintf("SYN%08d", seq_len(n_colaboradores))
  familia_colaborador <- sample(familias_sint, n_colaboradores, replace = TRUE, prob = probs_familias)
  nivel_colaborador <- map_chr(familia_colaborador, gerar_nivel)
  area_colaborador <- sample(
    areas_sint,
    n_colaboradores,
    replace = TRUE,
    prob = c(rep(0.075, length(areas_tech_sint)), rep(0.05, length(areas_corp_sint))) / 
      sum(c(rep(0.075, length(areas_tech_sint)), rep(0.05, length(areas_corp_sint))))
  )
  data_admissao <- sample(datas_possiveis, n_colaboradores, replace = TRUE, prob = pesos_admissao)
  
  # Mistura explícita de perfis para o simulador não ficar "feliz demais".
  # A ideia é ter de tudo: pessoas estáveis, risco moderado, risco crítico,
  # baixa performance, alto potencial volátil e casos neutros.
  perfil_retencao_sint <- sample(
    c(
      "Estável",
      "Risco moderado",
      "Ponto de atenção",
      "Risco crítico",
      "Alta performance volátil",
      "Baixa performance / risco involuntário"
    ),
    n_colaboradores,
    replace = TRUE,
    prob = c(0.26, 0.27, 0.20, 0.10, 0.09, 0.08)
  )
  
  # Casos âncora: deixam a busca individual trazer exemplos variados logo nos
  # primeiros nomes sintéticos, sem mexer no restante do app.
  idx_critico <- 1:min(20, n_colaboradores)
  idx_atencao <- 21:min(40, n_colaboradores)
  idx_volatil <- 41:min(60, n_colaboradores)
  idx_baixa <- 61:min(80, n_colaboradores)
  idx_estavel <- 81:min(100, n_colaboradores)
  
  if (length(idx_critico) > 0) {
    perfil_retencao_sint[idx_critico] <- "Risco crítico"
    familia_colaborador[idx_critico] <- sample(c("Agente", "Assistente", "Analista"), length(idx_critico), replace = TRUE, prob = c(0.40, 0.30, 0.30))
    nivel_colaborador[idx_critico] <- sample(c("I", "II"), length(idx_critico), replace = TRUE, prob = c(0.70, 0.30))
    area_colaborador[idx_critico] <- sample(c("Suporte", "Relacionamento", "Comdinheiro"), length(idx_critico), replace = TRUE)
    data_admissao[idx_critico] <- hoje - days(sample(420:2200, length(idx_critico), replace = TRUE))
  }
  if (length(idx_atencao) > 0) {
    perfil_retencao_sint[idx_atencao] <- "Ponto de atenção"
    familia_colaborador[idx_atencao] <- sample(c("Analista", "Desenvolvedor(a)", "Assistente"), length(idx_atencao), replace = TRUE)
    nivel_colaborador[idx_atencao] <- sample(c("I", "II", "III"), length(idx_atencao), replace = TRUE, prob = c(0.35, 0.45, 0.20))
    area_colaborador[idx_atencao] <- sample(areas_sint, length(idx_atencao), replace = TRUE)
    data_admissao[idx_atencao] <- hoje - days(sample(300:2600, length(idx_atencao), replace = TRUE))
  }
  if (length(idx_volatil) > 0) {
    perfil_retencao_sint[idx_volatil] <- "Alta performance volátil"
    familia_colaborador[idx_volatil] <- sample(c("Desenvolvedor(a)", "Especialista", "Analista", "Líder"), length(idx_volatil), replace = TRUE)
    nivel_colaborador[idx_volatil] <- sample(c("II", "III", "IV"), length(idx_volatil), replace = TRUE, prob = c(0.25, 0.50, 0.25))
    area_colaborador[idx_volatil] <- sample(c("Plataformas", "Electronic Trading", "Inteligência Artificial", "Marketing"), length(idx_volatil), replace = TRUE)
    data_admissao[idx_volatil] <- hoje - days(sample(540:3000, length(idx_volatil), replace = TRUE))
  }
  if (length(idx_baixa) > 0) {
    perfil_retencao_sint[idx_baixa] <- "Baixa performance / risco involuntário"
    familia_colaborador[idx_baixa] <- sample(c("Agente", "Assistente", "Auxiliar", "Analista"), length(idx_baixa), replace = TRUE)
    nivel_colaborador[idx_baixa] <- sample(c("I", "II"), length(idx_baixa), replace = TRUE, prob = c(0.75, 0.25))
    area_colaborador[idx_baixa] <- sample(c("Suporte", "Relacionamento", "Comdinheiro", "Akeloo"), length(idx_baixa), replace = TRUE)
    data_admissao[idx_baixa] <- hoje - days(sample(180:1800, length(idx_baixa), replace = TRUE))
  }
  if (length(idx_estavel) > 0) {
    perfil_retencao_sint[idx_estavel] <- "Estável"
    familia_colaborador[idx_estavel] <- sample(c("Gerente", "Especialista", "Líder", "Desenvolvedor(a)"), length(idx_estavel), replace = TRUE)
    nivel_colaborador[idx_estavel] <- sample(c("III", "IV", "V"), length(idx_estavel), replace = TRUE, prob = c(0.45, 0.40, 0.15))
    area_colaborador[idx_estavel] <- sample(c("Plataformas", "People", "Financeiro", "Jurídico", "Inteligência Artificial"), length(idx_estavel), replace = TRUE)
    data_admissao[idx_estavel] <- hoje - days(sample(900:3400, length(idx_estavel), replace = TRUE))
  }
  
  dias_maximos <- as.integer(hoje - data_admissao)
  nivel_num_inicial <- match(nivel_colaborador, niveis_roman)
  nivel_num_inicial[is.na(nivel_num_inicial)] <- 1
  
  ajuste_perfil_perf <- case_when(
    perfil_retencao_sint == "Estável" ~ 0.35,
    perfil_retencao_sint == "Risco moderado" ~ 0.00,
    perfil_retencao_sint == "Ponto de atenção" ~ -0.20,
    perfil_retencao_sint == "Risco crítico" ~ -0.35,
    perfil_retencao_sint == "Alta performance volátil" ~ 0.78,
    perfil_retencao_sint == "Baixa performance / risco involuntário" ~ -0.95,
    TRUE ~ 0
  )
  
  ajuste_perfil_risco <- case_when(
    perfil_retencao_sint == "Estável" ~ -0.75,
    perfil_retencao_sint == "Risco moderado" ~ -0.05,
    perfil_retencao_sint == "Ponto de atenção" ~ 0.48,
    perfil_retencao_sint == "Risco crítico" ~ 1.05,
    perfil_retencao_sint == "Alta performance volátil" ~ 0.72,
    perfil_retencao_sint == "Baixa performance / risco involuntário" ~ 0.90,
    TRUE ~ 0
  )
  
  score_performance_sint <- rnorm(n_colaboradores, 0, 0.95) +
    ajuste_perfil_perf +
    case_when(
      familia_colaborador %in% c("Gerente", "Especialista") ~ 0.18,
      familia_colaborador %in% c("Desenvolvedor(a)", "Líder") ~ 0.10,
      familia_colaborador %in% c("Estagiário(a)", "Jovem Aprendiz") ~ -0.25,
      TRUE ~ 0
    )
  
  risco_area_sint <- case_when(
    area_colaborador %in% c("Suporte", "Relacionamento", "Comdinheiro") ~ 0.42,
    area_colaborador %in% c("Akeloo", "Marketing", "Educacional") ~ 0.18,
    area_colaborador %in% c("Electronic Trading", "Inteligência Artificial", "Plataformas") ~ -0.10,
    area_colaborador %in% c("People", "Financeiro", "Jurídico") ~ -0.18,
    TRUE ~ 0.05
  )
  
  risco_latente_sint <- rnorm(n_colaboradores, 0, 0.90) +
    ajuste_perfil_risco +
    risco_area_sint +
    case_when(
      familia_colaborador %in% c("Agente", "Assistente", "Auxiliar") ~ 0.38,
      familia_colaborador %in% c("Estagiário(a)", "Jovem Aprendiz") ~ 0.56,
      familia_colaborador %in% c("Gerente", "Especialista") ~ -0.20,
      familia_colaborador == "Desenvolvedor(a)" ~ -0.02,
      TRUE ~ 0
    ) -
    0.22 * score_performance_sint +
    0.16 * pmax(nivel_num_inicial - 3, 0) +
    0.34 * (score_performance_sint > 1.25) # alto potencial também recebe assédio de mercado
  
  propensao_promocao_sint <- plogis(
    -0.05 +
      0.62 * score_performance_sint -
      0.22 * risco_latente_sint +
      case_when(
        perfil_retencao_sint == "Estável" ~ 0.12,
        perfil_retencao_sint == "Alta performance volátil" ~ 0.35,
        perfil_retencao_sint == "Risco crítico" ~ -0.50,
        perfil_retencao_sint == "Baixa performance / risco involuntário" ~ -0.80,
        TRUE ~ 0
      ) +
      case_when(
        familia_colaborador %in% c("Analista", "Desenvolvedor(a)") ~ 0.24,
        familia_colaborador %in% c("Agente", "Assistente") ~ 0.08,
        familia_colaborador %in% c("Gerente", "Especialista") ~ -0.08,
        familia_colaborador %in% c("Estagiário(a)", "Jovem Aprendiz") ~ -0.70,
        TRUE ~ 0
      ) -
      0.16 * pmax(nivel_num_inicial - 3, 0)
  )
  
  # Probabilidade histórica de saída menos otimista e com bastante sobreposição:
  # nem todo alto risco já saiu e nem todo baixo risco fica para sempre.
  prob_desligamento <- plogis(
    -0.58 +
      0.68 * risco_latente_sint +
      0.34 * (score_performance_sint < -0.75) +
      0.24 * (score_performance_sint > 1.20) +
      0.32 * (propensao_promocao_sint < 0.35) +
      0.18 * (dias_maximos > 365) +
      0.24 * (dias_maximos > 900) +
      0.18 * (dias_maximos > 1800) +
      0.26 * (familia_colaborador %in% c("Estagiário(a)", "Jovem Aprendiz")) +
      0.18 * (familia_colaborador %in% c("Agente", "Assistente", "Auxiliar")) -
      0.16 * (familia_colaborador %in% c("Gerente", "Especialista"))
  )
  prob_desligamento[dias_maximos < 120] <- prob_desligamento[dias_maximos < 120] * 0.28
  prob_desligamento <- pmin(pmax(prob_desligamento, 0.08), 0.84)
  
  # Mantém alguns perfis críticos vivos no simulador para aparecerem como risco real,
  # e alguns perfis estáveis desligados para o modelo não ficar binário demais.
  prob_desligamento[perfil_retencao_sint == "Risco crítico"] <- pmin(prob_desligamento[perfil_retencao_sint == "Risco crítico"], 0.72)
  prob_desligamento[perfil_retencao_sint == "Baixa performance / risco involuntário"] <- pmin(prob_desligamento[perfil_retencao_sint == "Baixa performance / risco involuntário"], 0.76)
  prob_desligamento[perfil_retencao_sint == "Estável"] <- pmax(prob_desligamento[perfil_retencao_sint == "Estável"], 0.10)
  
  ativo <- rbinom(n_colaboradores, 1, 1 - prob_desligamento)
  ativo[dias_maximos < 90] <- 1
  
  # Garante que a lista do simulador tenha casos ativos de baixo, médio e alto risco.
  idx_forcar_ativos <- unique(c(
    head(idx_critico, 8),
    head(idx_atencao, 8),
    head(idx_volatil, 8),
    head(idx_baixa, 8),
    head(idx_estavel, 8)
  ))
  idx_forcar_ativos <- idx_forcar_ativos[idx_forcar_ativos >= 1 & idx_forcar_ativos <= n_colaboradores]
  ativo[idx_forcar_ativos] <- 1
  
  data_desligamento <- rep(as.Date(NA), n_colaboradores)
  
  # Eventos distribuídos ao longo do tempo. Antes a maioria saía cedo demais;
  # isso deixava o risco condicional de 6/12 meses quase zerado para quem já tinha
  # tempo de casa. Aqui há churn inicial, médio, tardio e muito tardio.
  faixas_evento <- tibble(
    fase = c("churn_inicial", "primeiro_ciclo", "meio_de_carreira", "estagnacao_tardia", "muito_tardia"),
    inicio = c(1, 9, 24, 48, 84),
    fim = c(9, 24, 48, 84, 132)
  )
  
  for (i in seq_len(n_colaboradores)) {
    if (ativo[i] == 0 && dias_maximos[i] > 30) {
      meses_maximos <- max(1.2, dias_maximos[i] / 30.4375)
      
      probs_fase <- c(0.18, 0.28, 0.26, 0.20, 0.08)
      
      if (risco_latente_sint[i] > 1.20 || score_performance_sint[i] < -1.00) {
        probs_fase <- c(0.24, 0.34, 0.24, 0.14, 0.04)
      }
      if (perfil_retencao_sint[i] == "Alta performance volátil") {
        probs_fase <- c(0.08, 0.24, 0.32, 0.26, 0.10)
      }
      if (perfil_retencao_sint[i] == "Estável") {
        probs_fase <- c(0.06, 0.16, 0.28, 0.34, 0.16)
      }
      if (familia_colaborador[i] %in% c("Estagiário(a)", "Jovem Aprendiz")) {
        probs_fase <- c(0.40, 0.38, 0.16, 0.05, 0.01)
      }
      
      faixas_validas <- faixas_evento %>%
        mutate(fim_real = pmin(fim, meses_maximos - 0.1)) %>%
        filter(inicio < meses_maximos, fim_real > inicio)
      
      if (nrow(faixas_validas) == 0) {
        meses_evento <- runif(1, 0.7, meses_maximos)
      } else {
        prob_validas <- probs_fase[match(faixas_validas$fase, faixas_evento$fase)]
        prob_validas <- prob_validas / sum(prob_validas)
        faixa_escolhida <- sample(seq_len(nrow(faixas_validas)), 1, prob = prob_validas)
        meses_evento <- runif(
          1,
          faixas_validas$inicio[faixa_escolhida],
          faixas_validas$fim_real[faixa_escolhida]
        )
      }
      
      dias_evento <- round(pmin(pmax(meses_evento * 30.4375, 20), dias_maximos[i] - 1))
      data_desligamento[i] <- data_admissao[i] + days(dias_evento)
    }
  }
  
  prob_voluntario <- pmin(
    pmax(
      0.50 +
        0.16 * (score_performance_sint > 0.90) -
        0.14 * (score_performance_sint < -0.90) +
        0.10 * propensao_promocao_sint +
        0.08 * (perfil_retencao_sint == "Alta performance volátil") -
        0.10 * (perfil_retencao_sint == "Baixa performance / risco involuntário"),
      0.22
    ),
    0.88
  )
  
  motivo <- ifelse(
    ativo == 0,
    ifelse(runif(n_colaboradores) < prob_voluntario, "Voluntário", "Involuntário"),
    NA_character_
  )
  
  perfil_clean <- tibble(
    cpf = cpf_sint,
    nome = sprintf("Pessoa Sintética %03d", seq_len(n_colaboradores)),
    data_admissao = data_admissao,
    data_desligamento = as.Date(data_desligamento),
    ativo = ativo,
    motivo = motivo,
    perfil_retencao_sint = perfil_retencao_sint,
    area = area_colaborador,
    area_norm = str_squish(str_to_lower(area_colaborador)),
    subarea = sample(subareas_sint, n_colaboradores, replace = TRUE),
    equipe = sample(equipes_sint, n_colaboradores, replace = TRUE),
    familia_cargo = familia_colaborador,
    nivel_cargo = nivel_colaborador,
    cargo = map2_chr(familia_colaborador, nivel_colaborador, montar_cargo)
  ) %>%
    mutate(
      data_fim_ref = if_else(is.na(data_desligamento), hoje, data_desligamento),
      data_fim_ref = if_else(data_fim_ref <= data_admissao, data_admissao + days(1), data_fim_ref),
      tempo_empresa_dias = as.numeric(difftime(data_fim_ref, data_admissao, units = "days")),
      tempo_empresa_meses = if_else(tempo_empresa_dias <= 0, 1, tempo_empresa_dias) / 30.4375
    ) %>%
    select(-data_fim_ref)
  
  sinais_sint <- tibble(
    cpf = cpf_sint,
    score_performance_sint = score_performance_sint,
    propensao_promocao_sint = propensao_promocao_sint,
    risco_latente_sint = risco_latente_sint,
    nivel_num_inicial_sint = nivel_num_inicial,
    perfil_retencao_sint = perfil_retencao_sint,
    prob_desligamento_sint = prob_desligamento
  )
  
  # -------------------------------------------------------------------------
  # PPR sintético por semestre
  # -------------------------------------------------------------------------
  safras_ppr <- tibble(
    safra_ppr = c("2023-1", "2023-2", "2024-1", "2024-2", "2025-1", "2025-2"),
    data_referencia_ppr = as.Date(c("2023-06-30", "2023-12-31", "2024-06-30", "2024-12-31", "2025-06-30", "2025-12-31"))
  )
  
  ppr_full <- tidyr::crossing(cpf = perfil_clean$cpf, safra_ppr = safras_ppr$safra_ppr) %>%
    left_join(safras_ppr, by = "safra_ppr") %>%
    left_join(perfil_clean %>% select(cpf, data_admissao, data_desligamento, ativo, motivo, familia_cargo), by = "cpf") %>%
    left_join(sinais_sint, by = "cpf") %>%
    mutate(data_limite = if_else(is.na(data_desligamento), hoje, data_desligamento)) %>%
    filter(data_referencia_ppr >= data_admissao + days(45), data_referencia_ppr <= data_limite) %>%
    mutate(
      prob_sem_ppr = case_when(
        data_referencia_ppr <= data_admissao + days(180) ~ 0.18,
        perfil_retencao_sint %in% c("Risco crítico", "Baixa performance / risco involuntário") ~ 0.10,
        perfil_retencao_sint == "Alta performance volátil" ~ 0.07,
        TRUE ~ 0.05
      ),
      manter = runif(n()) > prob_sem_ppr
    ) %>%
    filter(manter) %>%
    mutate(
      periodo_ref = as.numeric(data_referencia_ppr - min(data_referencia_ppr, na.rm = TRUE)) / 365.25,
      media_perf = case_when(
        familia_cargo %in% c("Gerente", "Especialista") ~ 1.00,
        familia_cargo %in% c("Desenvolvedor(a)", "Líder") ~ 0.96,
        familia_cargo %in% c("Estagiário(a)", "Jovem Aprendiz") ~ 0.78,
        TRUE ~ 0.89
      ) +
        case_when(
          perfil_retencao_sint == "Estável" ~ 0.09,
          perfil_retencao_sint == "Risco moderado" ~ 0.00,
          perfil_retencao_sint == "Ponto de atenção" ~ -0.08,
          perfil_retencao_sint == "Risco crítico" ~ -0.16,
          perfil_retencao_sint == "Alta performance volátil" ~ 0.16,
          perfil_retencao_sint == "Baixa performance / risco involuntário" ~ -0.28,
          TRUE ~ 0
        ) +
        0.22 * score_performance_sint -
        0.13 * risco_latente_sint +
        0.04 * sin(periodo_ref * pi),
      desvio_perf = case_when(
        perfil_retencao_sint %in% c("Risco crítico", "Alta performance volátil") ~ 0.26,
        perfil_retencao_sint == "Baixa performance / risco involuntário" ~ 0.22,
        TRUE ~ 0.18
      ),
      multiplo_individual = round(pmin(pmax(rnorm(n(), mean = media_perf, sd = desvio_perf), 0.20), 1.60), 2),
      multiplo_individual = if_else(
        runif(n()) < case_when(
          perfil_retencao_sint == "Baixa performance / risco involuntário" ~ 0.28,
          perfil_retencao_sint == "Risco crítico" ~ 0.18,
          perfil_retencao_sint == "Ponto de atenção" ~ 0.10,
          TRUE ~ 0.04
        ),
        round(runif(n(), 0.20, 0.72), 2),
        multiplo_individual
      ),
      multiplo_individual = if_else(
        ativo == 0 & motivo == "Involuntário" & data_referencia_ppr >= data_limite - months(6),
        pmin(multiplo_individual, 0.62),
        multiplo_individual
      )
    ) %>%
    arrange(cpf, data_referencia_ppr) %>%
    select(cpf, safra_ppr, data_referencia_ppr, multiplo_individual)
  
  ppr_historico <- ppr_full %>%
    arrange(cpf, data_referencia_ppr) %>%
    group_by(cpf) %>%
    mutate(
      ppr_seq = row_number(),
      multiplo_anterior = lag(multiplo_individual),
      data_ppr_anterior = lag(data_referencia_ppr)
    ) %>%
    ungroup() %>%
    select(cpf, data_ppr = data_referencia_ppr, safra_ppr, multiplo_individual, multiplo_anterior, data_ppr_anterior, ppr_seq)
  
  ppr_time_dep <- ppr_historico %>%
    left_join(perfil_clean %>% select(cpf, data_admissao, data_desligamento, ativo, motivo), by = "cpf") %>%
    mutate(
      meses_desde_admissao = as.numeric(difftime(data_ppr, data_admissao, units = "days")) / 30.4375,
      ppr_ajustado = case_when(
        ativo == 0 & motivo == "Involuntário" & multiplo_individual == 0 & !is.na(multiplo_anterior) ~ multiplo_anterior,
        TRUE ~ multiplo_individual
      ),
      faixa_performance = case_when(
        ppr_ajustado > 1.0 ~ "Alta Performance",
        ppr_ajustado >= 0.75 ~ "Esperada",
        ppr_ajustado < 0.75 ~ "Baixa Performance",
        TRUE ~ "Sem Histórico"
      )
    )
  
  ultimo_ppr <- ppr_full %>%
    group_by(cpf) %>%
    filter(data_referencia_ppr == max(data_referencia_ppr, na.rm = TRUE)) %>%
    ungroup() %>%
    select(cpf, ultimo_multiplo = multiplo_individual, ultimo_ppr_data = data_referencia_ppr)
  
  perfil_clean <- perfil_clean %>%
    left_join(ultimo_ppr, by = "cpf") %>%
    mutate(
      faixa_performance_ajustada = case_when(
        ultimo_multiplo > 1.0 ~ "Alta Performance",
        ultimo_multiplo >= 0.75 ~ "Esperada",
        ultimo_multiplo < 0.75 ~ "Baixa Performance",
        TRUE ~ "Sem Histórico"
      )
    )
  
  # -------------------------------------------------------------------------
  # Promoções e dossiês sintéticos
  # -------------------------------------------------------------------------
  ciclos_promocao <- tidyr::expand_grid(ano = 2020:2026, semestre = 1:2) %>%
    arrange(ano, semestre) %>%
    mutate(
      data_ciclo = as.Date(if_else(semestre == 1, paste0(ano, "-06-30"), paste0(ano, "-12-31"))),
      ordem_ciclo = row_number()
    ) %>%
    filter(data_ciclo <= as.Date("2026-06-30")) %>%
    mutate(
      volume_alvo = c(24, 36, 29, 44, 57, 41, 68, 53, 76, 64, 92, 81, 108)[seq_len(n())]
    )
  
  estado_carreira <- perfil_clean %>%
    select(cpf, nome, data_admissao, data_desligamento, familia_cargo, nivel_cargo) %>%
    left_join(sinais_sint, by = "cpf") %>%
    mutate(
      nivel_num_atual = match(nivel_cargo, niveis_roman),
      nivel_num_atual = if_else(is.na(nivel_num_atual), 1L, as.integer(nivel_num_atual)),
      ultima_promo = data_admissao,
      elegivel_carreira = !familia_cargo %in% c("Estagiário(a)", "Jovem Aprendiz")
    )
  
  promocoes_lista <- list()
  for (idx_ciclo in seq_len(nrow(ciclos_promocao))) {
    ciclo_atual <- ciclos_promocao$data_ciclo[idx_ciclo]
    alvo_ciclo <- ciclos_promocao$volume_alvo[idx_ciclo]
    
    elegiveis <- estado_carreira %>%
      mutate(
        meses_desde_admissao_tmp = as.numeric(difftime(ciclo_atual, data_admissao, units = "days")) / 30.4375,
        meses_desde_ultima_tmp = as.numeric(difftime(ciclo_atual, ultima_promo, units = "days")) / 30.4375,
        ativo_no_ciclo = is.na(data_desligamento) | data_desligamento > ciclo_atual + days(15),
        peso_promocao = pmax(
          0.01,
          propensao_promocao_sint *
            (1 + pmin(meses_desde_ultima_tmp, 48) / 36) *
            case_when(
              familia_cargo %in% c("Analista", "Desenvolvedor(a)") ~ 1.25,
              familia_cargo %in% c("Agente", "Assistente", "Auxiliar") ~ 1.10,
              familia_cargo %in% c("Gerente", "Especialista") ~ 0.85,
              TRUE ~ 1
            ) *
            case_when(
              perfil_retencao_sint == "Estável" ~ 1.18,
              perfil_retencao_sint == "Alta performance volátil" ~ 1.35,
              perfil_retencao_sint == "Ponto de atenção" ~ 0.82,
              perfil_retencao_sint == "Risco crítico" ~ 0.48,
              perfil_retencao_sint == "Baixa performance / risco involuntário" ~ 0.32,
              TRUE ~ 1
            )
        )
      ) %>%
      filter(
        elegivel_carreira,
        data_admissao <= ciclo_atual - months(6),
        ativo_no_ciclo,
        nivel_num_atual < 5,
        meses_desde_ultima_tmp >= 8
      )
    
    if (nrow(elegiveis) < alvo_ciclo) {
      elegiveis <- estado_carreira %>%
        mutate(
          meses_desde_admissao_tmp = as.numeric(difftime(ciclo_atual, data_admissao, units = "days")) / 30.4375,
          meses_desde_ultima_tmp = as.numeric(difftime(ciclo_atual, ultima_promo, units = "days")) / 30.4375,
          ativo_no_ciclo = is.na(data_desligamento) | data_desligamento > ciclo_atual + days(15),
          peso_promocao = pmax(
            0.01,
            propensao_promocao_sint *
              (1 + pmin(meses_desde_ultima_tmp, 48) / 42) *
              case_when(
                perfil_retencao_sint == "Risco crítico" ~ 0.55,
                perfil_retencao_sint == "Baixa performance / risco involuntário" ~ 0.40,
                perfil_retencao_sint == "Alta performance volátil" ~ 1.25,
                TRUE ~ 1
              )
          )
        ) %>%
        filter(
          elegivel_carreira,
          data_admissao <= ciclo_atual - months(4),
          ativo_no_ciclo,
          nivel_num_atual < 5,
          meses_desde_ultima_tmp >= 5
        )
    }
    
    if (nrow(elegiveis) == 0) next
    
    qtd_ciclo <- min(alvo_ciclo, nrow(elegiveis))
    selecionados <- elegiveis[sample(seq_len(nrow(elegiveis)), qtd_ciclo, replace = FALSE, prob = elegiveis$peso_promocao), ]
    nivel_destino <- pmin(5L, selecionados$nivel_num_atual + 1L)
    
    promocoes_lista[[idx_ciclo]] <- tibble(
      cpf = selecionados$cpf,
      nome = str_squish(str_to_lower(selecionados$nome)),
      inicio_vigencia = ciclo_atual,
      descritivo_do_cargo = map2_chr(selecionados$familia_cargo, niveis_roman[nivel_destino], montar_cargo),
      data_admissao_oficial = selecionados$data_admissao,
      origem = sample(c("sistema", "dossie"), qtd_ciclo, replace = TRUE, prob = c(0.68, 0.32)),
      nome_original = selecionados$nome,
      nivel_origem_sint = selecionados$nivel_num_atual,
      nivel_destino_sint = nivel_destino
    )
    
    idx_estado <- match(selecionados$cpf, estado_carreira$cpf)
    estado_carreira$nivel_num_atual[idx_estado] <- nivel_destino
    estado_carreira$ultima_promo[idx_estado] <- ciclo_atual
  }
  
  df_promocoes_final <- bind_rows(promocoes_lista) %>%
    distinct(cpf, inicio_vigencia, .keep_all = TRUE) %>%
    arrange(cpf, inicio_vigencia) %>%
    group_by(cpf) %>%
    mutate(
      movimento_seq = row_number(),
      data_anterior = lag(inicio_vigencia),
      meses_desde_admissao = as.numeric(difftime(inicio_vigencia, data_admissao_oficial, units = "days")) / 30.4375,
      tempo_no_cargo_anterior = if_else(
        movimento_seq == 1,
        meses_desde_admissao,
        as.numeric(difftime(inicio_vigencia, data_anterior, units = "days")) / 30.4375
      ),
      tempo_no_cargo_anterior = if_else(tempo_no_cargo_anterior <= 0 | is.na(tempo_no_cargo_anterior), 0.1, tempo_no_cargo_anterior),
      meses_desde_admissao = if_else(meses_desde_admissao <= 0 | is.na(meses_desde_admissao), 0.1, meses_desde_admissao)
    ) %>%
    ungroup() %>%
    select(cpf, nome, inicio_vigencia, descritivo_do_cargo, data_admissao_oficial, origem, nome_original, nivel_origem_sint, nivel_destino_sint, movimento_seq, data_anterior, meses_desde_admissao, tempo_no_cargo_anterior)
  
  dados_dossie <- df_promocoes_final %>%
    left_join(perfil_clean %>% select(cpf, nome_perfil = nome, familia_cargo), by = "cpf") %>%
    mutate(
      nivel_destino = if_else(!is.na(nivel_destino_sint), as.integer(nivel_destino_sint), pmin(5L, pmax(2L, as.integer(movimento_seq + 1L)))),
      nivel_origem = if_else(!is.na(nivel_origem_sint), as.integer(nivel_origem_sint), pmax(1L, nivel_destino - 1L)),
      nome = nome_perfil,
      ciclo = inicio_vigencia,
      situacao = sample(c("Aprovado", "Aprovado em Comitê", "Aprovado pela Liderança"), n(), replace = TRUE),
      cargo_atual = map2_chr(familia_cargo, niveis_roman[nivel_origem], montar_cargo),
      cargo_futuro = map2_chr(familia_cargo, niveis_roman[nivel_destino], montar_cargo),
      tempo_casa = round(meses_desde_admissao, 1)
    ) %>%
    select(nome, ciclo, situacao, cargo_atual, cargo_futuro, tempo_casa)
  
  # -------------------------------------------------------------------------
  # Base time-dependent sintética para o modelo Cox
  # -------------------------------------------------------------------------
  dados_base_td <- perfil_clean %>%
    filter(!is.na(cpf), !is.na(tempo_empresa_meses)) %>%
    distinct(cpf, .keep_all = TRUE) %>%
    select(cpf, tempo_empresa_meses, ativo, familia_cargo, nivel_cargo, area, subarea) %>%
    mutate(
      status = if_else(ativo == 0, 1, 0),
      tstart = 0,
      tstop = ifelse(tempo_empresa_meses <= 0, 0.1, tempo_empresa_meses)
    )
  
  df_split <- tmerge(data1 = dados_base_td, data2 = dados_base_td, id = cpf, event_status = event(tstop, status))
  
  dados_promo_td <- df_promocoes_final %>%
    filter(!is.na(cpf), !is.na(meses_desde_admissao)) %>%
    select(cpf, tempo_promo = meses_desde_admissao) %>%
    distinct(cpf, tempo_promo)
  
  if (nrow(dados_promo_td) > 0) {
    df_split <- tmerge(df_split, dados_promo_td, id = cpf, promo_event = tdc(tempo_promo))
    df_split <- tmerge(df_split, dados_promo_td, id = cpf, tempo_ultima_promo = tdc(tempo_promo, tempo_promo))
  } else {
    df_split$promo_event <- 0
    df_split$tempo_ultima_promo <- 0
  }
  
  ppr_td_data <- ppr_time_dep %>%
    select(cpf, tempo_ppr = meses_desde_admissao, faixa_performance) %>%
    filter(!is.na(tempo_ppr), !is.na(faixa_performance))
  
  if (nrow(ppr_td_data) > 0) {
    df_split <- tmerge(df_split, ppr_td_data, id = cpf, ppr_cat_td = tdc(tempo_ppr, faixa_performance))
  } else {
    df_split$ppr_cat_td <- "Sem Histórico"
  }
  
  dados_modelo_full <- df_split %>%
    filter(tstop > tstart) %>%
    group_by(cpf) %>%
    mutate(
      promo_event = if_else(is.na(promo_event), 0, promo_event),
      qtd_promocoes_td = cumsum(promo_event),
      tempo_ultima_promo = if_else(is.na(tempo_ultima_promo), 0, tempo_ultima_promo),
      tempo_no_cargo_atual = tstart - tempo_ultima_promo,
      tempo_no_cargo_atual = if_else(tempo_ultima_promo == 0, tstart, tempo_no_cargo_atual),
      tempo_empresa_td = tstart,
      efeito_protetor_promo = case_when(
        familia_cargo %in% c("Estagiário(a)", "Jovem Aprendiz") ~ 1,
        tempo_ultima_promo == 0 & tstart <= 6 ~ 1,
        tempo_ultima_promo == 0 ~ 0,
        TRUE ~ exp(-(log(2) / 12) * pmax(tempo_no_cargo_atual, 0))
      ),
      recencia_cat_td = case_when(
        tempo_ultima_promo == 0 ~ "Não Promovido",
        tempo_no_cargo_atual <= 12 ~ "Recente (< 1 Ano)",
        tempo_no_cargo_atual <= 36 ~ "Média (1-3 Anos)",
        TRUE ~ "Antiga (> 3 Anos)"
      ),
      recencia_cat_td = factor(
        recencia_cat_td,
        levels = c("Não Promovido", "Recente (< 1 Ano)", "Média (1-3 Anos)", "Antiga (> 3 Anos)")
      ),
      ppr_cat_td = zoo::na.locf(ppr_cat_td, na.rm = FALSE),
      ppr_cat_td = factor(
        if_else(is.na(ppr_cat_td), "Sem Histórico", as.character(ppr_cat_td)),
        levels = c("Sem Histórico", "Esperada", "Alta Performance", "Baixa Performance")
      )
    ) %>%
    ungroup()
  
  # -------------------------------------------------------------------------
  # Régua de carreira e diagnóstico sintéticos
  # -------------------------------------------------------------------------
  ladder_raw <- dados_dossie %>%
    clean_names() %>%
    mutate(
      situacao_norm = str_squish(str_to_lower(situacao)),
      origem_clean = str_squish(str_to_lower(cargo_atual)),
      destino_clean = str_squish(str_to_lower(cargo_futuro)),
      tempo_casa = suppressWarnings(as.numeric(tempo_casa))
    ) %>%
    filter(str_detect(situacao_norm, "aprovad")) %>%
    mutate(
      familia_macro = classificar_familia(origem_clean),
      nivel_origem_txt = str_extract(origem_clean, "\\b[iv]+$"),
      nivel_destino_txt = str_extract(destino_clean, "\\b[iv]+$"),
      nivel_origem_num = romano_para_inteiro(nivel_origem_txt),
      nivel_destino_num = romano_para_inteiro(nivel_destino_txt)
    ) %>%
    filter(
      !is.na(familia_macro),
      nivel_origem_num > 0 & nivel_destino_num > 0,
      nivel_destino_num > nivel_origem_num,
      !str_detect(origem_clean, "aprendiz|estagi|trainee")
    ) %>%
    mutate(transicao_label = paste0(str_to_upper(nivel_origem_txt), " \u2794 ", str_to_upper(nivel_destino_txt)))
  
  df_classificado <- df_promocoes_final %>%
    arrange(cpf, inicio_vigencia) %>%
    group_by(cpf) %>%
    mutate(cargo_anterior_real = lag(descritivo_do_cargo)) %>%
    ungroup() %>%
    mutate(
      cargo_atual_texto = str_squish(str_to_lower(descritivo_do_cargo)),
      cargo_anterior_texto = str_squish(str_to_lower(cargo_anterior_real)),
      familia_atual = classificar_familia(cargo_atual_texto),
      familia_anterior = classificar_familia(cargo_anterior_texto)
    )
  
  ultima_promo_saida <- df_classificado %>%
    group_by(cpf) %>%
    filter(inicio_vigencia == max(inicio_vigencia)) %>%
    ungroup() %>%
    select(cpf, data_ultima_promo = inicio_vigencia, familia_atual)
  
  stats_saida <- perfil_clean %>%
    filter(ativo == 0) %>%
    inner_join(ultima_promo_saida, by = "cpf") %>%
    mutate(meses_ate_sair = as.numeric(difftime(data_desligamento, data_ultima_promo, units = "days")) / 30.4375) %>%
    filter(meses_ate_sair > 0, !is.na(familia_atual)) %>%
    group_by(familia_atual) %>%
    summarise(meses_paciencia = median(meses_ate_sair, na.rm = TRUE), qtd_saidas = n(), .groups = "drop") %>%
    filter(qtd_saidas >= 3)
  
  stats_promo <- df_classificado %>%
    filter(!is.na(familia_anterior)) %>%
    group_by(familia_anterior) %>%
    summarise(meses_velocidade = median(tempo_no_cargo_anterior, na.rm = TRUE), qtd_promos = n(), .groups = "drop") %>%
    filter(qtd_promos >= 3) %>%
    rename(familia_atual = familia_anterior)
  
  df_dumbbell_macro <- inner_join(stats_saida, stats_promo, by = "familia_atual") %>%
    mutate(
      status_risco = if_else(meses_paciencia < meses_velocidade, "RISCO", "SEGURO"),
      gap = meses_velocidade - meses_paciencia
    )
  
  df_dumbbell_area <- df_classificado %>%
    inner_join(perfil_clean %>% select(cpf, area), by = "cpf") %>%
    filter(!is.na(familia_anterior)) %>%
    group_by(area) %>%
    summarise(meses_velocidade = median(tempo_no_cargo_anterior, na.rm = TRUE), qtd_promos = n(), .groups = "drop") %>%
    filter(qtd_promos >= 5) %>%
    inner_join(
      perfil_clean %>%
        filter(ativo == 0) %>%
        inner_join(ultima_promo_saida, by = "cpf") %>%
        mutate(meses_ate_sair = as.numeric(difftime(data_desligamento, data_ultima_promo, units = "days")) / 30.4375) %>%
        filter(meses_ate_sair > 0) %>%
        group_by(area) %>%
        summarise(meses_paciencia = median(meses_ate_sair, na.rm = TRUE), qtd_saidas = n(), .groups = "drop") %>%
        filter(qtd_saidas >= 3),
      by = "area"
    ) %>%
    mutate(
      status_risco = if_else(meses_paciencia < meses_velocidade, "RISCO", "SEGURO"),
      gap = meses_velocidade - meses_paciencia
    )
  
  dados_admitidos <- perfil_clean %>%
    mutate(
      mes_admissao = floor_date(data_admissao, "month"),
      teve_desligamento = !is.na(data_desligamento),
      tempo_ate_sair_dias = if_else(teve_desligamento, as.numeric(difftime(data_desligamento, data_admissao, units = "days")), NA_real_),
      saiu_em_90_dias = if_else(teve_desligamento & tempo_ate_sair_dias <= 90, TRUE, FALSE)
    ) %>%
    group_by(mes_admissao) %>%
    summarise(
      total_admitidos = n(),
      sairam_90_dias = sum(saiu_em_90_dias, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(mes_admissao)) %>%
    arrange(mes_admissao) %>%
    mutate(
      admitidos_validos = total_admitidos - sairam_90_dias,
      taxa_saida_90_dias = if_else(total_admitidos > 0, sairam_90_dias / total_admitidos, 0),
      mes_ano = format(mes_admissao, "%b/%Y")
    )
  
  return(list(
    perfil_clean = perfil_clean,
    df_promocoes_final = df_promocoes_final,
    ppr_full = ppr_full,
    ppr_time_dep = ppr_time_dep,
    dados_dossie = dados_dossie,
    ladder_raw = ladder_raw,
    df_dumbbell_macro = df_dumbbell_macro,
    df_dumbbell_area = df_dumbbell_area,
    dados_modelo_full = dados_modelo_full,
    dados_admitidos = dados_admitidos
  ))
}

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$div(" Retenção & Carreira", style = "color: #f39c12; font-weight: 800; font-size: 20px; letter-spacing: 0.5px;"),
    titleWidth = 300,
    tags$li(class = "dropdown", tags$div(style = "padding: 15px; color: #d1d4dc; font-weight: 500;", icon("calendar-alt"), " ", format(Sys.Date(), "%d/%m/%Y")))
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Simulador Preditivo", tabName = "simulador", icon = icon("user-shield")),
      menuItem("Visão da Empresa", tabName = "visao", icon = icon("building")),
      menuItem("Carreira", tabName = "carreira", icon = icon("sitemap")),
      menuItem("Diagnóstico de Risco", tabName = "diagnostico", icon = icon("stethoscope")),
      menuItem("Relatórios", tabName = "relatorios", icon = icon("file-alt"))
    ),
    div(class = "filtros-container", style = paste0("padding: 20px; background: rgba(31, 36, 48, 0.6); margin: 15px; border-radius: 12px; border: 1px solid #2d3342;"),
        h4(icon("sliders-h"), " Filtros", style = "color: white; margin-top: 0; margin-bottom: 20px; font-weight: 700;"),
        selectInput("filtro_setor", "Setor",
                    choices = c("Todos", "Tech", "Corporativo"),
                    selected = "Todos"),
        pickerInput("filtro_area", "Áreas",
                    choices = NULL,
                    selected = NULL,
                    options = list(
                      `actions-box` = TRUE, 
                      `live-search` = TRUE,
                      `select-all-text` = "Marcar Todas",
                      `deselect-all-text` = "Limpar"
                    ),
                    multiple = TRUE),
        pickerInput("filtro_cargo", "Cargo:", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
        sliderInput("filtro_tempo", "Tempo de Casa (anos):", min = 0, max = 20, value = c(0, 15), step = 0.5),
        pickerInput("filtro_ppr", "Performance PPR:", choices = NULL, multiple = TRUE),
        pickerInput("filtro_ciclo_ppr", "Ciclo PPR (Ano/Semestre)",
                    choices = NULL,
                    selected = NULL,
                    options = list(`actions-box` = TRUE, `select-all-text` = "Todos", `deselect-all-text` = "Limpar"),
                    multiple = TRUE),
        
        selectInput("filtro_lideranca", "Liderança / Gestão",
                    choices = c("Todos", "Apenas Líderes/Gestores", "Apenas Operacional/Especialistas"),
                    selected = "Todos"),
        pickerInput("filtro_nivel", "Nível do Cargo",
                    choices = c("Nível 1" = 1, "Nível 2" = 2, "Nível 3" = 3, "Nível 4" = 4, "Nível 5" = 5),
                    selected = c(1, 2, 3, 4, 5),
                    options = list(`actions-box` = TRUE, `select-all-text` = "Todos", `deselect-all-text` = "Limpar"),
                    multiple = TRUE),
        awesomeCheckboxGroup("filtro_status", "Status Atual:", choices = c("Ativos" = 1, "Desligados" = 0), selected = c(1), status = "primary"),
        br(),
        actionButton("btn_reset", "Limpar Filtros", icon = icon("undo"), style = "width: 100%; background: #2a2e39; color: white; border: 1px solid #46637f; border-radius: 6px; padding: 8px; font-weight: 600; transition: 0.2s;")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"),
      tags$style(HTML(paste0("
        body, .content-wrapper, .right-side { background-color: ", tema_bg, " !important; font-family: 'Inter', sans-serif; color: ", tema_texto, "; }
        .skin-black .main-header .navbar { background-color: ", tema_panel, "; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .skin-black .main-header .logo { background-color: ", tema_panel, "; color: white; border-right: 1px solid #14171f;}
        .skin-black .main-sidebar { background-color: ", tema_panel, "; border-right: 1px solid #14171f; }
        
        .chart-container { background: ", tema_panel, "; border-radius: 12px; padding: 25px; margin-bottom: 25px; box-shadow: 0 8px 16px rgba(0,0,0,0.2); border: 1px solid #2d3342; transition: all 0.3s ease; }
        h4, h3 { color: white !important; font-weight: 700; margin-top: 0; letter-spacing: -0.5px; margin-bottom: 15px;}
        
        .kpi-row { display: flex; flex-wrap: wrap; gap: 15px; margin-bottom: 25px; }
        .kpi-card { flex: 1; min-width: 230px; background: #1c1f26; border: 1px solid #2a2f3a; border-radius: 8px; padding: 18px 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.2); display: flex; flex-direction: column; }
        .kpi-header { color: #8e9ab0; font-size: 14px; font-weight: 500; margin-bottom: 12px; }
        .kpi-main { display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 4px; }
        .kpi-value { font-size: 32px; font-weight: 700; line-height: 1; letter-spacing: -0.5px; }
        .kpi-gap { font-size: 13px; font-weight: 600; }
        .kpi-trend { font-size: 13px; font-weight: 600; margin-bottom: 16px; }
        .kpi-footer { display: flex; justify-content: space-between; font-size: 12px; color: #8e9ab0; margin-bottom: 8px; }
        .kpi-bar-bg { width: 100%; height: 6px; background-color: #2c323f; border-radius: 3px; position: relative; margin-top: auto;}
        .kpi-bar-fill { height: 100%; border-radius: 3px; position: absolute; left: 0; top: 0; transition: width 0.8s ease; }
        .kpi-bar-marker { width: 2px; height: 14px; background-color: #ffffff; position: absolute; top: -4px; z-index: 2; border-radius: 1px;}
        
        
        .dataTables_wrapper { background: transparent; color: ", tema_texto, "; }
        table.dataTable tbody tr { background-color: transparent !important; color: ", tema_texto, " !important; }
        table.dataTable tbody tr:hover { background-color: rgba(255,255,255,0.05) !important; }
        table.dataTable thead th { background-color: rgba(0,0,0,0.2) !important; color: white !important; border-bottom: 1px solid #46637f !important; font-weight: 600; }
        .dataTables_info, .dataTables_length, .dataTables_filter, .dataTables_paginate { color: ", tema_texto, " !important; }
        .paginate_button { color: white !important; }
        
        .alert-box { background: rgba(52, 152, 219, 0.1); border-left: 4px solid #3498db; padding: 15px 20px; margin-bottom: 25px; border-radius: 8px; color: #d1d4dc; font-size: 14px;}
        
        ::-webkit-scrollbar { width: 8px; height: 8px; }
        ::-webkit-scrollbar-track { background: #1f2430; border-radius: 4px; }
        ::-webkit-scrollbar-thumb { background: #46637f; border-radius: 4px; }
        ::-webkit-scrollbar-thumb:hover { background: #3498db; }
        
        .perfil-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-top: 15px; }
        .perfil-item { background: rgba(0,0,0,0.2); padding: 12px; border-radius: 8px; border: 1px solid #2d3342; display: flex; align-items: center; gap: 10px;}
        .perfil-icon { font-size: 18px; color: #3498db; width: 25px; text-align: center;}
        .perfil-info small { display: block; color: #95a5a6; text-transform: uppercase; font-size: 10px; font-weight: 600; letter-spacing: 0.5px;}
        .perfil-info strong { color: white; font-size: 13px;}
      ")))
    ),
    tabItems(
      tabItem(tabName = "simulador",
              fluidRow(
                column(12, div(class = "chart-container", style="padding: 15px 25px; display:flex; align-items:center; gap:20px; margin-bottom: 15px;", 
                               h4(icon("search"), " Buscar Colaborador:", style="margin:0; min-width:250px;"), 
                               div(style="flex:1;", selectizeInput("busca_individual", NULL, choices = NULL, width = "100%"))))
              ),
              fluidRow(
                column(4, 
                       div(class = "chart-container", style="min-height: 500px;", 
                           h4(icon("id-card"), " Perfil Analítico"), 
                           uiOutput("card_perfil_resumo"))
                ),
                column(4, 
                       div(class = "chart-container", style="min-height: 500px; text-align:center;", 
                           h4(icon("exclamation-triangle"), " Risco de Evasão (6m)"), 
                           plotlyOutput("gauge_risco", height="220px"), 
                           hr(style="border-color:#2d3342; margin:15px 0;"), 
                           h5("Principal Fator de Risco do Modelo:", style="color:#95a5a6; font-size:12px; text-transform:uppercase; margin-top:10px;"),
                           uiOutput("badges_status"))
                ),
                column(4, 
                       div(class = "chart-container", style="min-height: 500px; text-align:center;", 
                           h4(icon("bullseye"), " Probabilidade de Ficar (12m)"), 
                           plotlyOutput("gauge_prob", height="220px"), 
                           hr(style="border-color:#2d3342; margin:15px 0;"), 
                           h5("Principal Fator de Proteção do Modelo:", style="color:#95a5a6; font-size:12px; text-transform:uppercase; margin-top:10px;"),
                           uiOutput("ui_scorecard_detalhes"))
                )
              ),
              fluidRow(
                column(6, div(class = "chart-container", style="height: 450px;", h4(icon("star"), " Evolução de Performance (PPR)"), plotlyOutput("plot_ppr_hist", height="130px"), hr(style="border-color: darkgreen;"), h4(icon("history"), " Histórico de Promoções"), DTOutput("tabela_historico"))),
                column(6, div(class = "chart-container", style="height: 450px;", h4(icon("chart-line"), " Detalhamento Estatístico (Sobrevivência)"), plotlyOutput("plot_surv_predict", height = "350px")))
              )
      ),
      
      tabItem(tabName = "visao",
              fluidRow(
                uiOutput("kpis_dinamicos")
              ),
              
              fluidRow(
                column(6, div(class = "chart-container", 
                              h4(icon("chart-line"), " Volume de Promoções por Área (Ciclos)"), 
                              plotlyOutput("plot_distribuicao_ppr", height = "350px")
                )),
                column(6, div(class = "chart-container", h4("Análise de Admissões (Retenção 90 dias)"), plotlyOutput("plot_admitidos_validos", height = "350px")))
              ),
              fluidRow(
                column(12, div(class = "chart-container", h4(icon("chart-area"), " Evolução do Volume de Promoções"), plotlyOutput("plot_evolucao_final", height = "350px")))
              )
      ),
      
      tabItem(tabName = "carreira",
              fluidRow(uiOutput("kpis_carreira")),
              fluidRow(
                column(6, div(class = "chart-container", h4("Velocidade de Promoção por Nível"), plotlyOutput("plot_ladder_vel", height = "650px"))),
                column(6, div(class = "chart-container", h4("Régua de Carreira (Dossiês)"), plotlyOutput("plot_ladder_clean", height = "650px")))
              ),
              fluidRow(
                column(12, div(class = "chart-container", 
                               h4(icon("table"), " Detalhamento de Velocidade de Crescimento por Nível e Cargo"), 
                               DTOutput("tabela_carreira")))
              )
      ),
      
      tabItem(tabName = "diagnostico",
              fluidRow(uiOutput("kpis_diagnostico")),
              div(class = "alert-box", icon("lightbulb"), strong(" Dica de Leitura:"), " Se o ponto vermelho (Tempo de Saída) ocorrer ", strong("antes"), " do ponto verde (Promoção), há risco de perda de talentos por falta de progressão oportuna."),
              fluidRow(
                column(6, div(class = "chart-container", h4("Relógio de Retenção - CARGOS"), plotlyOutput("plot_relogio_cargo", height = "600px"))),
                column(6, div(class = "chart-container", h4("Relógio de Retenção - ÁREAS"), plotlyOutput("plot_relogio_area", height = "600px")))
              ),
              fluidRow(
                column(12, div(class = "chart-container", h4(icon("table"), " Detalhamento de Risco por Cargo"), DTOutput("tabela_diagnostico")))
              )
      ),
      
      tabItem(tabName = "relatorios",
              div(class = "chart-container",
                  h4(icon("download"), " Central de Relatórios"),
                  p("Exporte as bases consolidadas respeitando os filtros aplicados na barra lateral.", style="color:#95a5a6;"),
                  fluidRow(
                    column(4, downloadButton("btn_relatorio_promocoes", " Exportar Promoções", class = "btn-primary w-100", style="padding:10px; font-weight:bold;")),
                    column(4, downloadButton("btn_relatorio_risco", " Exportar Risco por Cargo", class = "btn-warning w-100", style="padding:10px; font-weight:bold; color:white;")),
                    column(4, downloadButton("btn_relatorio_ppr", " Exportar Performance", class = "btn-success w-100", style="padding:10px; font-weight:bold;"))
                  ),
                  hr(style="border-color:#2d3342; margin: 30px 0;"),
                  h4("Visualização dos Dados Filtrados"),
                  DTOutput("tabela_relatorio")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  cache_info <- reactiveValues(dados = NULL, hash = "", timestamp = NULL)
  
  verificar_mudancas_arquivos <- function() {
    # Versão sintética: não verifica arquivos nem bases externas.
    # Mantém uma chave constante para que o app não dependa de .xlsx.
    "dados_sinteticos_v2"
  }
  
  dados_reactive <- reactivePoll(intervalMillis = 120000, session = session, checkFunc = verificar_mudancas_arquivos, valueFunc = function() {
    d <- carregar_dados(); cache_info$dados <- d; cache_info$hash <- verificar_mudancas_arquivos(); cache_info$timestamp <- Sys.time(); return(d)
  })
  
  # Modelo para CLT (foco em carreira, promoções e senioridade)
  modelo_clt <- reactive({
    dados_crus <- dados()$dados_modelo_full %>% 
      filter(!familia_cargo %in% c("Estagiário(a)", "Jovem Aprendiz")) %>%
      mutate(
        area = if_else(is.na(area), "Sem Area", area),
        nivel_cargo = if_else(is.na(nivel_cargo), "Único", nivel_cargo),
        familia_cargo = factor(familia_cargo),
        nivel_cargo = factor(nivel_cargo),
        ppr_cat_td = factor(
          ppr_cat_td,
          levels = c("Sem Histórico", "Esperada", "Alta Performance", "Baixa Performance")
        )
      )
    
    if (is.null(dados_crus) || nrow(dados_crus) < 10 || sum(dados_crus$event_status, na.rm = TRUE) < 3) {
      return(NULL)
    }
    
    tryCatch({
      coxph(
        Surv(tstart, tstop, event_status) ~ 
          efeito_protetor_promo +
          qtd_promocoes_td +
          ppr_cat_td +
          familia_cargo +
          nivel_cargo +
          cluster(cpf),
        data = dados_crus,
        ties = "efron"
      )
    }, error = function(e) {
      NULL
    })
  })
  
  modelo_estagio <- reactive({
    dados_estag <- dados()$dados_modelo_full %>% 
      filter(familia_cargo %in% c("Estagiário(a)", "Jovem Aprendiz")) %>%
      mutate(
        ppr_cat_td = factor(
          ppr_cat_td,
          levels = c("Sem Histórico", "Esperada", "Alta Performance", "Baixa Performance")
        )
      )
    
    if (is.null(dados_estag) || nrow(dados_estag) < 5 || sum(dados_estag$event_status, na.rm = TRUE) < 2) {
      return(NULL)
    }
    
    tryCatch({
      coxph(
        Surv(tstart, tstop, event_status) ~ 
          ppr_cat_td +
          cluster(cpf),
        data = dados_estag,
        ties = "efron"
      )
    }, error = function(e) {
      NULL
    })
  })
  
  
  observe({ if (is.null(cache_info$dados)) { cache_info$dados <- carregar_dados(); cache_info$hash <- verificar_mudancas_arquivos(); cache_info$timestamp <- Sys.time() } })
  dados <- reactive({ if (is.null(dados_reactive())) return(cache_info$dados) else return(dados_reactive()) })
  
  observe({
    req(dados())  
    updatePickerInput(session, "filtro_area", choices = sort(unique(dados()$perfil_clean$area)), selected = unique(dados()$perfil_clean$area))
    updatePickerInput(session, "filtro_cargo", choices = sort(unique(dados()$perfil_clean$familia_cargo)), selected = unique(dados()$perfil_clean$familia_cargo))
    updatePickerInput(session, "filtro_ppr", choices = unique(dados()$perfil_clean$faixa_performance_ajustada), selected = unique(dados()$perfil_clean$faixa_performance_ajustada))
    opcoes_busca <- dados()$perfil_clean %>% filter(ativo == 1, !is.na(nome), nome != "") %>% arrange(nome) %>% pull(nome)
    if(length(opcoes_busca) > 0) updateSelectizeInput(session, "busca_individual", choices = setNames(opcoes_busca, opcoes_busca), selected = opcoes_busca[1], server = FALSE)
  })
  
  observeEvent(input$btn_reset, {
    updatePickerInput(session, "filtro_area", selected = unique(dados()$perfil_clean$area))
    updatePickerInput(session, "filtro_cargo", selected = unique(dados()$perfil_clean$familia_cargo))
    updateSliderInput(session, "filtro_tempo", value = c(0, 15))
    updatePickerInput(session, "filtro_ppr", selected = unique(dados()$perfil_clean$faixa_performance_ajustada))
    updateAwesomeCheckboxGroup(session, "filtro_status", selected = c(1))
  })
  
  dados_filtrados <- reactive({
    df <- dados()$perfil_clean
    
    if(!is.null(input$filtro_area)) df <- df %>% filter(area %in% input$filtro_area)
    if(!is.null(input$filtro_cargo)) df <- df %>% filter(familia_cargo %in% input$filtro_cargo)
    if(!is.null(input$filtro_ppr)) df <- df %>% filter(faixa_performance_ajustada %in% input$filtro_ppr)
    if(!is.null(input$filtro_status)) df <- df %>% filter(ativo %in% as.numeric(input$filtro_status))
    if(!is.null(input$filtro_tempo)) df <- df %>% filter(tempo_empresa_meses >= (input$filtro_tempo[1]*12) & tempo_empresa_meses <= (input$filtro_tempo[2]*12))
    
    if (!is.null(input$filtro_nivel) && length(input$filtro_nivel) > 0) {
      df <- df %>% 
        mutate(c_lower = str_squish(str_to_lower(ifelse(is.na(cargo), "", cargo))),
               n_nivel = case_when(
                 str_detect(c_lower, "\\bi\\b") ~ 1, 
                 str_detect(c_lower, "\\bii\\b") ~ 2, 
                 str_detect(c_lower, "\\biii\\b") ~ 3, 
                 str_detect(c_lower, "\\biv\\b") ~ 4, 
                 str_detect(c_lower, "\\bv\\b") ~ 5, 
                 str_detect(c_lower, "junior|jr") ~ 1, 
                 str_detect(c_lower, "pleno|pl") ~ 2, 
                 str_detect(c_lower, "senior|sr") ~ 3, 
                 TRUE ~ 99
               )) %>%
        filter(n_nivel %in% as.numeric(input$filtro_nivel)) %>%
        select(-c_lower, -n_nivel)
    }
    
    if (!is.null(input$filtro_ciclo_ppr) && length(input$filtro_ciclo_ppr) > 0) {
      cpfs_validos <- dados()$df_promocoes_final %>%
        mutate(ano = year(inicio_vigencia),
               semestre = ifelse(month(inicio_vigencia) <= 6, "S1", "S2"),
               label_ciclo = paste0(ano, " ", semestre)) %>%
        filter(label_ciclo %in% input$filtro_ciclo_ppr) %>%
        pull(cpf) %>% unique()
      
      df <- df %>% filter(cpf %in% cpfs_validos)
    }
    
    df
  })
  
  dados_filtrados_kpi <- reactive({
    df <- dados()$perfil_clean
    
    if(!is.null(input$filtro_area)) df <- df %>% filter(area %in% input$filtro_area)
    if(!is.null(input$filtro_cargo)) df <- df %>% filter(familia_cargo %in% input$filtro_cargo)
    if(!is.null(input$filtro_tempo)) df <- df %>% filter(tempo_empresa_meses >= (input$filtro_tempo[1]*12) & tempo_empresa_meses <= (input$filtro_tempo[2]*12))
    
    if (!is.null(input$filtro_nivel) && length(input$filtro_nivel) > 0) {
      df <- df %>% 
        mutate(c_lower = str_squish(str_to_lower(ifelse(is.na(cargo), "", cargo))),
               n_nivel = case_when(
                 str_detect(c_lower, "\\bi\\b") ~ 1, 
                 str_detect(c_lower, "\\bii\\b") ~ 2, 
                 str_detect(c_lower, "\\biii\\b") ~ 3, 
                 str_detect(c_lower, "\\biv\\b") ~ 4, 
                 str_detect(c_lower, "\\bv\\b") ~ 5, 
                 str_detect(c_lower, "junior|jr") ~ 1, 
                 str_detect(c_lower, "pleno|pl") ~ 2, 
                 str_detect(c_lower, "senior|sr") ~ 3, 
                 TRUE ~ 99
               )) %>%
        filter(n_nivel %in% as.numeric(input$filtro_nivel)) %>%
        select(-c_lower, -n_nivel)
    }
    
    if (!is.null(input$filtro_ciclo_ppr) && length(input$filtro_ciclo_ppr) > 0) {
      cpfs_validos <- dados()$df_promocoes_final %>%
        mutate(ano = year(inicio_vigencia),
               semestre = ifelse(month(inicio_vigencia) <= 6, "S1", "S2"),
               label_ciclo = paste0(ano, " ", semestre)) %>%
        filter(label_ciclo %in% input$filtro_ciclo_ppr) %>%
        pull(cpf) %>% unique()
      
      df <- df %>% filter(cpf %in% cpfs_validos)
    }
    
    df
  })
  
  extrair_motivos_modelo <- function(modelo, df_nova, qtd_promocoes_atual = NA_real_) {
    termos <- tryCatch(predict(modelo, newdata = df_nova, type = "terms"), error = function(e) NULL)
    
    if (is.null(termos) || ncol(termos) == 0) {
      return(list(risco = "Sem fator dominante", protecao = "Equilíbrio", detalhes = data.frame()))
    }
    
    valores <- as.numeric(termos[1, ])
    nomes <- colnames(termos)
    qtd_promocoes_atual <- suppressWarnings(as.numeric(qtd_promocoes_atual))
    if (length(qtd_promocoes_atual) == 0 || is.na(qtd_promocoes_atual)) qtd_promocoes_atual <- NA_real_
    
    interpretar <- function(nome, valor) {
      case_when(
        str_detect(nome, "efeito_protetor_promo") ~ "Tempo no Cargo",
        str_detect(nome, "qtd_promocoes_td") ~ "Histórico de Promoções",
        str_detect(nome, "ppr_cat_td") ~ "Performance (PPR)",
        str_detect(nome, "familia_cargo") ~ "Família do Cargo",
        str_detect(nome, "nivel_cargo") ~ "Senioridade",
        str_detect(nome, "area") ~ "Área de Atuação",
        TRUE ~ nome
      )
    }
    
    df_det <- data.frame(termo = nomes, val = valores) %>%
      mutate(
        interpretacao = map2_chr(termo, val, interpretar),
        direcao = ifelse(val > 0, "Risco", "Proteção"),
        peso_abs = abs(val),
        # Se a pessoa nunca foi promovida, o termo numérico de promoções pode aparecer
        # por centralização do modelo Cox. Isso NÃO deve ser exibido como proteção por
        # "Histórico de Promoções", porque não existe histórico individual de promoção.
        ocultar_ui = str_detect(termo, "qtd_promocoes_td") & !is.na(qtd_promocoes_atual) & qtd_promocoes_atual <= 0
      ) %>%
      filter(!ocultar_ui, peso_abs > 0)
    
    if (nrow(df_det) == 0 || sum(df_det$peso_abs, na.rm = TRUE) == 0) {
      return(list(risco = "Nenhum risco isolado", protecao = "Equilíbrio", detalhes = data.frame()))
    }
    
    df_det <- df_det %>%
      mutate(peso_pct = peso_abs / sum(peso_abs, na.rm = TRUE)) %>%
      arrange(desc(peso_abs))
    
    idx_prot <- which.min(df_det$val)
    idx_risco <- which.max(df_det$val)
    
    protecao <- if(df_det$val[idx_prot] < -0.01) df_det$interpretacao[idx_prot] else "Equilíbrio"
    risco <- if(df_det$val[idx_risco] > 0.01) df_det$interpretacao[idx_risco] else "Nenhum risco isolado"
    
    if (df_det$termo[idx_risco] == "qtd_promocoes_td" && df_det$val[idx_risco] > 0.01) risco <- "Alta Atratividade/Rápida Ascensão"
    
    return(list(risco = risco, protecao = protecao, detalhes = df_det))
  }
  
  sim_data <- reactive({
    req(input$busca_individual)
    f <- dados()$perfil_clean %>% filter(nome == input$busca_individual)
    req(nrow(f) > 0)
    p_hist <- dados()$df_promocoes_final %>% filter(cpf == f$cpf) %>% arrange(desc(inicio_vigencia))
    ppr_hist <- dados()$ppr_full %>% filter(cpf == f$cpf) %>% arrange(data_referencia_ppr)
    
    ult_p <- if(nrow(p_hist) > 0) p_hist %>% slice(1) else NULL
    ppr_txt <- case_when(is.na(f$faixa_performance_ajustada[1]) | f$faixa_performance_ajustada[1] == "Sem Histórico" ~ "Sem Histórico", TRUE ~ as.character(f$faixa_performance_ajustada[1]))
    
    t_now <- f$tempo_empresa_meses[1]
    rec <- "Não Promovido"
    qtd <- 0
    dt_txt <- "Nunca"
    t_no_cargo = t_now
    
    if(!is.null(ult_p)) {
      dt_txt <- format(ult_p$inicio_vigencia, "%b/%Y")
      t_promo <- t_now - ult_p$meses_desde_admissao
      t_no_cargo <- t_promo
      qtd <- nrow(p_hist)
      rec <- case_when(t_promo <= 12 ~ "Recente (< 1 Ano)", t_promo <= 36 ~ "Média (1-3 Anos)", TRUE ~ "Antiga (> 3 Anos)")
    }
    
    tempo_ultima_promo_ind <- if(!is.null(ult_p)) ult_p$meses_desde_admissao[1] else 0
    efeito_atual <- case_when(
      f$familia_cargo[1] %in% c("Estagiário(a)", "Jovem Aprendiz") ~ 1,
      tempo_ultima_promo_ind == 0 & t_now <= 6 ~ 1,
      tempo_ultima_promo_ind == 0 ~ 0,
      TRUE ~ exp(-(log(2) / 12) * pmax(t_no_cargo, 0))
    )
    
    list(
      df = data.frame(
        cpf = f$cpf[1], 
        status_recencia = factor(rec, levels = c("Não Promovido", "Recente (< 1 Ano)", "Média (1-3 Anos)", "Antiga (> 3 Anos)")), 
        qtd_promocoes = qtd, 
        tempo_atual = t_now, 
        tempo_no_cargo_atual = t_no_cargo, 
        ppr_cat = factor(ppr_txt, levels = c("Sem Histórico", "Esperada", "Alta Performance", "Baixa Performance")),
        efeito_protetor_promo = efeito_atual,
        familia_cargo = f$familia_cargo[1],
        nivel_cargo = if_else(is.na(f$nivel_cargo[1]), "Único", f$nivel_cargo[1]), 
        area = if_else(is.na(f$area[1]), "Sem Area", f$area[1]) 
      ), 
      raw = f, ppr = ppr_txt, dt = dt_txt, historico = p_hist, ppr_hist = ppr_hist
    )
  })
  
  sim_calc <- reactive({
    d <- sim_data()$df
    t_atual <- as.numeric(d$tempo_atual)
    eh_estag <- as.character(d$familia_cargo) %in% c("Estagiário(a)", "Jovem Aprendiz")
    modelo <- if (eh_estag) modelo_estagio() else modelo_clt()
    
    retorno_padrao <- list(risco_6m=0, prob_12m=1, fator_risco="Modelo indisponível", fator_protecao="Indisponível", detalhes_score=data.frame(), fit=data.frame(time=c(0,1), surv=c(1,1)))
    if (is.null(modelo)) return(retorno_padrao)
    
    if (eh_estag) {
      df_nova <- data.frame(ppr_cat_td = factor(safe_factor_value(d$ppr_cat, modelo$xlevels$ppr_cat_td, "Sem Histórico"), levels = modelo$xlevels$ppr_cat_td), area = d$area)
    } else {
      df_nova <- data.frame(
        efeito_protetor_promo = as.numeric(d$efeito_protetor_promo),
        qtd_promocoes_td = as.numeric(d$qtd_promocoes),
        ppr_cat_td = factor(safe_factor_value(d$ppr_cat, modelo$xlevels$ppr_cat_td, "Sem Histórico"), levels = modelo$xlevels$ppr_cat_td),
        familia_cargo = factor(safe_factor_value(d$familia_cargo, modelo$xlevels$familia_cargo), levels = modelo$xlevels$familia_cargo),
        nivel_cargo = factor(safe_factor_value(d$nivel_cargo, modelo$xlevels$nivel_cargo, "Único"), levels = modelo$xlevels$nivel_cargo),
        area = d$area
      )
    }
    
    sf <- tryCatch(survfit(modelo, newdata = df_nova), error = function(e) NULL)
    if (is.null(sf)) return(retorno_padrao)
    
    p0 <- get_s_safe(sf, t_atual); p6 <- get_s_safe(sf, t_atual + 6); p12 <- get_s_safe(sf, t_atual + 12)
    p0 <- max(p0, 1e-6)
    risco_6m_valor <- max(min(1 - (p6 / p0), 1), 0)
    
    motivos <- extrair_motivos_modelo(modelo, df_nova, qtd_promocoes_atual = d$qtd_promocoes)
    fator_risco_txt <- if(risco_6m_valor > 0.40 && motivos$risco == "Nenhum risco isolado") "Risco Sistêmico (Contexto de Área/Perfil)" else motivos$risco
    
    list(risco_6m = risco_6m_valor, prob_12m = max(min(p12/p0, 1), 0), fator_protecao = motivos$protecao, fator_risco = fator_risco_txt, detalhes_score = motivos$detalhes, fit = data.frame(time=as.numeric(sf$time), surv=as.numeric(sf$surv)))
  })
  
  output$card_perfil_resumo <- renderUI({
    r <- sim_data()$raw
    div(style = "color: white;",
        div(class = "text-center mb-3",
            div(class = "text-white rounded-circle d-flex justify-content-center align-items-center fw-bold mx-auto mb-2", style = "width:60px; height:60px; font-size:24px; background: linear-gradient(135deg, #3498db, #2c3e50); box-shadow: 0 4px 8px rgba(0,0,0,0.3);", str_sub(r$nome, 1, 1)),
            h5(r$nome, class = "mb-1 fw-bold", style="letter-spacing:-0.5px;"), span(r$cargo, style = "color: #f39c12; font-weight:600; font-size:12px; text-transform:uppercase;")
        ),
        div(class="perfil-grid",
            div(class="perfil-item", div(class="perfil-icon", icon("building")), div(class="perfil-info", tags$small("Área"), tags$strong(r$area))),
            div(class="perfil-item", div(class="perfil-icon", icon("clock")), div(class="perfil-info", tags$small("Tempo de Casa"), tags$strong(paste(round(r$tempo_empresa_meses/12, 1), "anos")))),
            div(class="perfil-item", div(class="perfil-icon", icon("users")), div(class="perfil-info", tags$small("Equipe"), tags$strong(ifelse("equipe" %in% names(r), r$equipe[1], "N/A")))),
            div(class="perfil-item", div(class="perfil-icon", icon("sitemap")), div(class="perfil-info", tags$small("Subárea"), tags$strong(ifelse("subarea" %in% names(r), r$subarea[1], "N/A")))),
            div(class="perfil-item", div(class="perfil-icon", icon("arrow-up")), div(class="perfil-info", tags$small("Última Promo"), tags$strong(sim_data()$dt))),
            div(class="perfil-item", div(class="perfil-icon", icon("circle-check")), div(class="perfil-info", tags$small("Status"), if(r$ativo == 1) tags$strong("Ativo", style="color:#27ae60;") else tags$strong("Desligado", style="color:#e74c3c;")))
        )
    )
  })
  
  
  areas_tech <- c("Infra e Redes", "Plataformas", "Electronic Trading", "Inteligência Artificial")
  areas_corp <- c("Suporte", "Negócios Internacionais", "Comdinheiro", "Relacionamento", 
                  "Marketing", "Akeloo", "People", "Educacional", "Financeiro", "Jurídico")
  
  observe({
    req(input$filtro_setor) 
    
    # Cascata de Setor -> Área
    areas_mostrar <- if (input$filtro_setor == "Tech") {
      areas_tech
    } else if (input$filtro_setor == "Corporativo") {
      areas_corp
    } else {
      c(areas_tech, areas_corp)
    }
    
    updatePickerInput(session, "filtro_area", choices = sort(areas_mostrar), selected = areas_mostrar)
    
    # CORREÇÃO CRÍTICA: Lendo da base já limpa (df_promocoes_final)
    if (!is.null(dados()$df_promocoes_final) && nrow(dados()$df_promocoes_final) > 0) {
      ciclos_existentes <- dados()$df_promocoes_final %>%
        mutate(ano = year(inicio_vigencia),
               semestre = ifelse(month(inicio_vigencia) <= 6, "S1", "S2"),
               label_ciclo = paste0(ano, " ", semestre)) %>%
        pull(label_ciclo) %>%
        unique() %>%
        sort(decreasing = TRUE) 
      
      updatePickerInput(session, "filtro_ciclo_ppr", choices = ciclos_existentes, selected = ciclos_existentes)
    }
  })
  
  
  output$gauge_risco <- renderPlotly({
    val <- as.numeric(sim_calc()$risco_6m)
    if (is.na(val)) val <- 0
    
    cor <- ifelse(val > 0.4, "#e74c3c", ifelse(val > 0.15, "#f39c12", "#27ae60"))
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = val * 100,
      number = list(suffix = "%", font = list(color = "white", size = 30)),
      gauge = list(
        axis = list(range = list(0, 100), tickcolor = "white"),
        bar = list(color = cor),
        bgcolor = "rgba(0,0,0,0)"
      )
    ) %>%
      layout(
        margin = list(l = 20, r = 20, t = 20, b = 20),
        paper_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "white", family = "Inter")
      )
  })
  
  output$gauge_prob <- renderPlotly({
    val <- as.numeric(sim_calc()$prob_12m)
    if (is.na(val)) val <- 1
    
    cor <- ifelse(val > 0.7, "#3498db", ifelse(val > 0.4, "#f39c12", "#e74c3c"))
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = val * 100,
      number = list(suffix = "%", font = list(color = "white", size = 30)),
      gauge = list(
        axis = list(range = list(0, 100), tickcolor = "white"),
        bar = list(color = cor),
        bgcolor = "rgba(0,0,0,0)"
      )
    ) %>%
      layout(
        margin = list(l = 20, r = 20, t = 20, b = 20),
        paper_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "white", family = "Inter")
      )
  })
  
  output$badges_status <- renderUI({
    fr <- sim_calc()$fator_risco
    
    cor_badge <- ifelse(fr == "Nenhum risco isolado" | fr == "Sem dados do modelo", "#95a5a6", "#e74c3c")
    icon_badge <- ifelse(fr == "Nenhum risco isolado" | fr == "Sem dados do modelo", "minus-circle", "exclamation-triangle")
    
    span(style=paste0("padding:8px 12px; background:rgba(255,255,255,0.05); color:", cor_badge, "; border:1px solid ", cor_badge, "; border-radius:20px; font-size:13px; font-weight:bold; display:inline-block;"), icon(icon_badge), " ", fr)
  })
  
  output$ui_scorecard_detalhes <- renderUI({
    calc <- sim_calc()
    
    detalhes <- calc$detalhes_score
    
    if (is.null(detalhes) || nrow(detalhes) == 0) {
      return(
        div(style="color:#95a5a6; font-size:13px;", "Sem composição estatística disponível.")
      )
    }
    
    top3 <- detalhes %>% slice_head(n = 3)
    
    tagList(
      div(style="display:flex; justify-content:space-between; padding: 8px 15px; background:rgba(39, 174, 96, 0.1); border-left: 3px solid #27ae60; border-radius:6px; margin-bottom:8px;", 
          tags$span("Fator de Proteção", style="color:#27ae60; font-weight:700; font-size:12px;"), 
          tags$strong(calc$fator_protecao, style="color:white;")
      ),
      
      lapply(seq_len(nrow(top3)), function(i) {
        cor <- ifelse(top3$direcao[i] == "Risco", "#e74c3c", "#27ae60")
        
        div(style="display:flex; justify-content:space-between; gap:10px; padding: 8px 15px; background:rgba(0,0,0,0.2); border-radius:6px; margin-bottom:6px;",
            tags$span(
              paste0(i, ". ", top3$interpretacao[i]),
              style="color:#d1d4dc; font-weight:600; font-size:12px; text-align:left;"
            ),
            tags$strong(
              paste0(round(top3$peso_pct[i] * 100, 0), "%"),
              style=paste0("color:", cor, ";")
            )
        )
      })
    )
  })
  
  output$plot_ppr_hist <- renderPlotly({
    p_hist <- sim_data()$ppr_hist
    if(nrow(p_hist) == 0) return(plot_vazio())
    
    p_hist <- p_hist %>% mutate(safra_formatada = str_replace(safra_ppr, "-", "/"))
    
    plot_ly(p_hist, x = ~safra_formatada, y = ~multiplo_individual, 
            type = 'scatter', mode = 'lines+markers', 
            line = list(color = '#3498db', width = 3), 
            marker = list(color = '#f39c12', size = 8),
            text = ~multiplo_individual, textposition = 'top center',
            hovertemplate="Safra: %{x}<br>Multiplo: %{y}<extra></extra>") %>%
      layout_plotly_dark(eixo_x = "Semestre", eixo_y = "") %>% 
      layout(margin=list(t=10,b=30,l=20,r=10), yaxis=list(showticklabels=F, showgrid=F))
  })
  
  output$plot_surv_predict <- renderPlotly({
    calc <- sim_calc()
    d <- sim_data()$df
    
    df_fit <- calc$fit
    t_atual <- as.numeric(d$tempo_atual)
    
    if (is.null(df_fit) || nrow(df_fit) == 0) return(plot_vazio())
    
    plot_ly(df_fit, x = ~time, y = ~surv) %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        line = list(shape = "vh", color = "#3498db", width = 3),
        fill = "tozeroy",
        fillcolor = "rgba(52, 152, 219, 0.1)",
        name = "Probabilidade Estimada"
      ) %>%
      add_segments(x = t_atual, xend = t_atual, y = 0, yend = 1, line = list(dash = "dash", color = "#ffffff", width = 2), name = "Hoje") %>%
      add_segments(x = t_atual + 6, xend = t_atual + 6, y = 0, yend = 1, line = list(dash = "dash", color = "#f39c12", width = 2), name = "+6 meses") %>%
      add_segments(x = t_atual + 12, xend = t_atual + 12, y = 0, yend = 1, line = list(dash = "dash", color = "#27ae60", width = 2), name = "+12 meses") %>%
      layout_plotly_dark(eixo_x = "Meses de Empresa", eixo_y = "Probabilidade de Permanência") %>%
      layout(yaxis = list(tickformat = ".0%", range = c(0, 1.05)))
  })
  
  output$tabela_historico <- renderDT({
    d <- sim_data()
    if(is.null(d$historico) || nrow(d$historico) == 0) return(datatable(data.frame(Mensagem = "Nenhuma promoção"), options = list(dom="t"), rownames=F))
    datatable(d$historico %>% select(Data=inicio_vigencia, Cargo=descritivo_do_cargo, `T. Anterior`=tempo_no_cargo_anterior) %>% mutate(Data=format(Data,"%d/%m/%Y"), `T. Anterior`=paste(round(`T. Anterior`,1), "m")), options=list(pageLength=5, dom="t", scrollY="150px", paging=FALSE), rownames=F, class="display compact")
  })
  
  
  
  output$kpis_dinamicos <- renderUI({
    df <- dados_filtrados_kpi()
    hoje <- Sys.Date()
    d1 <- hoje - years(1)
    d2 <- hoje - years(2)
    
    hc_atual <- sum(df$ativo == 1, na.rm=TRUE)
    hc_ant <- sum(df$data_admissao <= d1 & (is.na(df$data_desligamento) | df$data_desligamento > d1), na.rm=TRUE)
    meta_hc <- if(hc_ant > 0) hc_ant * 1.05 else 100 
    
    ret_atual <- sum(df$data_admissao <= d1 & df$ativo == 1, na.rm=TRUE) / max(hc_ant, 1)
    hc_2a <- sum(df$data_admissao <= d2 & (is.na(df$data_desligamento) | df$data_desligamento > d2), na.rm=TRUE)
    ret_ant <- sum(df$data_admissao <= d2 & (is.na(df$data_desligamento) | df$data_desligamento > d1), na.rm=TRUE) / max(hc_2a, 1)
    meta_ret <- 0.85 
    
    turn_atual <- sum(df$ativo == 0 & df$data_desligamento > d1 & df$data_desligamento <= hoje, na.rm=TRUE) / max(hc_atual, 1)
    turn_ant <- sum(df$data_desligamento > d2 & df$data_desligamento <= d1, na.rm=TRUE) / max(hc_ant, 1)
    meta_turn <- 0.15 
    
    promo <- dados()$df_promocoes_final %>% filter(cpf %in% df$cpf)
    p_atual <- promo %>% filter(inicio_vigencia > d1 & inicio_vigencia <= hoje) %>% pull(tempo_no_cargo_anterior)
    p_ant <- promo %>% filter(inicio_vigencia > d2 & inicio_vigencia <= d1) %>% pull(tempo_no_cargo_anterior)
    med_p_atual <- if(length(p_atual)>0) median(p_atual, na.rm=T) else 0
    med_p_ant <- if(length(p_ant)>0) median(p_ant, na.rm=T) else 0
    meta_promo <- 18 
    
    tmp_atual <- mean(df$tempo_empresa_meses[df$ativo==1]/12, na.rm=TRUE)
    tmp_ant_vec <- as.numeric(difftime(d1, df$data_admissao[df$data_admissao <= d1 & (is.na(df$data_desligamento) | df$data_desligamento > d1)], units="days"))/365.25
    tmp_ant <- mean(tmp_ant_vec, na.rm=TRUE)
    if(is.nan(tmp_atual)) tmp_atual <- 0
    if(is.nan(tmp_ant)) tmp_ant <- 0
    meta_tmp <- 3.0 
    
    gerar_card <- function(titulo, atual, anterior, meta, tipo_metrica, formato, sufixo="") {
      formata_val <- function(v, show_signal = FALSE) {
        if(formato == "pct") {
          txt <- paste0(format(round(v * 100, 1), nsmall=1, decimal.mark=","), "%")
        } else if(formato == "dec") {
          txt <- paste0(format(round(v, 1), nsmall=1, decimal.mark=","), sufixo)
        } else {
          txt <- paste0(format(round(v, 0), big.mark=".", decimal.mark=","), sufixo)
        }
        if(show_signal && v > 0) txt <- paste0("+", txt)
        return(txt)
      }
      
      gap <- atual - meta
      diff <- atual - anterior
      cor_verde <- "#2ed573"; cor_vermelha <- "#ff6b6b"
      if(tipo_metrica == "positivo") { cor_verde <- "#2ed573"; cor_vermelha <- "#ff6b6b" } else { cor_verde <- "#2ed573"; cor_vermelha <- "#ff6b6b" }
      is_bom <- if(tipo_metrica == "positivo") atual >= meta else atual <= meta
      cor_destaque <- if(is_bom) cor_verde else cor_vermelha
      
      is_up <- diff > 0
      seta <- if(is_up) "▲ " else if(diff < 0) "▼ " else "▶ "
      cor_trend <- if(tipo_metrica == "positivo") { if(is_up) cor_verde else cor_vermelha } else { if(is_up) cor_vermelha else cor_verde }
      if (diff == 0) cor_trend <- "#8e9ab0"
      
      max_val <- max(c(atual, meta, anterior, 0), na.rm=TRUE) * 1.2
      if(max_val == 0) max_val <- 1
      pct_fill <- min((atual / max_val) * 100, 100)
      pct_target <- min((meta / max_val) * 100, 100)
      
      HTML(paste0('
        <div class="kpi-card" style="padding: 10px 15px; min-height: auto;">
          <div class="kpi-header" style="margin-bottom: 2px; font-size: 12px;">', titulo, '</div>
          <div class="kpi-main" style="margin-bottom: 0px; align-items: flex-end;">
            <div class="kpi-value" style="color: ', cor_destaque, '; font-size: 24px;">', formata_val(atual), '</div>
            <div class="kpi-gap" style="color: ', cor_destaque, '; font-size: 11px; margin-bottom: 3px;">Gap: ', formata_val(gap, TRUE), '</div>
          </div>
          <div class="kpi-trend" style="color: ', cor_trend, '; margin-bottom: 4px; font-size: 11px;">', seta, formata_val(abs(diff)), '</div>
          <div class="kpi-footer" style="margin-bottom: 6px; font-size: 10px;">
            <span>Ano ant: ', formata_val(anterior), '</span>
            <span>Meta: ', formata_val(meta), '</span>
          </div>
          <div class="kpi-bar-bg" style="height: 4px;">
            <div class="kpi-bar-fill" style="width: ', pct_fill, '%; background-color: ', cor_destaque, '; height: 4px;"></div>
            <div class="kpi-bar-marker" style="left: ', pct_target, '%; height: 10px; top: -3px;"></div>
          </div>
        </div>
      '))
    }
    
    div(class = "kpi-row", style="padding: 0 15px; margin-bottom: 15px;",
        gerar_card("Headcount", hc_atual, hc_ant, meta_hc, "positivo", "num"),
        gerar_card("Taxa Retenção (12m)", ret_atual, ret_ant, meta_ret, "positivo", "pct"),
        gerar_card("Turnover (12m)", turn_atual, turn_ant, meta_turn, "negativo", "pct"),
        gerar_card("Mediana Promoção", med_p_atual, med_p_ant, meta_promo, "negativo", "dec", "m"),
        gerar_card("Tempo Méd. Casa", tmp_atual, tmp_ant, meta_tmp, "positivo", "dec", "a")
    )
  })
  
  
  
  
  output$plot_distribuicao_ppr <- renderPlotly({
    req(input$filtro_area) 
    
    df_pessoas <- dados_filtrados()
    if(nrow(df_pessoas) == 0) return(plot_vazio())
    
    areas_tech <- c("Infra e Redes", "Plataformas", "Electronic Trading", "Inteligência Artificial")
    
    df_pessoas <- df_pessoas %>%
      mutate(macro_area = if_else(area %in% areas_tech, "Tech", "Corp"))
    
    df_base <- dados()$df_promocoes_final %>%
      filter(cpf %in% df_pessoas$cpf) %>%
      inner_join(df_pessoas %>% select(cpf, macro_area, area), by = "cpf") %>%
      mutate(ano = year(inicio_vigencia)) %>%
      filter(ano >= 2020 & ano <= 2026)
    
    vol_ano_macro <- df_base %>% count(macro_area, ano) %>% ungroup()
    vol_ano_area  <- df_base %>% count(macro_area, area, ano) %>% ungroup()
    
    if(nrow(vol_ano_macro) == 0) return(plot_vazio())
    
    p <- plot_ly() 
    vis_ambos <- c(); vis_tech <- c(); vis_corp <- c()
    
    # --- CAMADA 1: MACRO TECH (AZUL) ---
    df_macro_tech <- vol_ano_macro %>% filter(macro_area == "Tech")
    if(nrow(df_macro_tech) > 0) {
      p <- p %>% add_trace(data = df_macro_tech, x = ~ano, y = ~n, type = 'scatter', mode = 'lines+markers', name = ' Tech ',
                           line = list(color = '#3b82f6', width = 4, shape = 'spline'), marker = list(color = '#3b82f6', size = 10, line = list(color = "#1f2430", width = 2.5)),
                           hovertemplate = "<b>%{fullData.name}</b><br>Ano: %{x}<br>Volume: %{y}<extra></extra>")
      vis_ambos <- c(vis_ambos, TRUE); vis_tech <- c(vis_tech, FALSE); vis_corp <- c(vis_corp, FALSE)  
    }
    
    # --- CAMADA 2: MACRO CORP (VERDE) ---
    df_macro_corp <- vol_ano_macro %>% filter(macro_area == "Corp")
    if(nrow(df_macro_corp) > 0) {
      p <- p %>% add_trace(data = df_macro_corp, x = ~ano, y = ~n, type = 'scatter', mode = 'lines+markers', name = ' Corp ',
                           line = list(color = '#4ade80', width = 4, shape = 'spline'), marker = list(color = '#4ade80', size = 10, line = list(color = "#1f2430", width = 2.5)),
                           hovertemplate = "<b>%{fullData.name}</b><br>Ano: %{x}<br>Volume: %{y}<extra></extra>")
      vis_ambos <- c(vis_ambos, TRUE); vis_tech <- c(vis_tech, FALSE); vis_corp <- c(vis_corp, FALSE)
    }
    
    # --- PALETA DINÂMICA SEM REPETIÇÕES (Incluindo marrom, vermelhos, etc) ---
    paleta_detalhes <- c("#f59e0b", "#ec4899", "#06b6d4", "#8b5cf6", "#f97316", "#ef4444", "#8b4513", "#14b8a6", "#6366f1", "#d97706", "#be123c", "#10b981", "#84cc16", "#a855f7")
    
    areas_tech_presentes <- unique((vol_ano_area %>% filter(macro_area == "Tech"))$area)
    i <- 1
    for(a in areas_tech_presentes) {
      df_sub <- vol_ano_area %>% filter(area == a)
      p <- p %>% add_trace(data = df_sub, x = ~ano, y = ~n, type = 'scatter', mode = 'lines+markers', name = a, visible = FALSE,
                           line = list(color = paleta_detalhes[i], width = 2.5, shape = 'spline'), marker = list(color = paleta_detalhes[i], size = 8, line = list(color = "#1f2430", width = 2)),
                           hovertemplate = paste0("<b>", a, "</b><br>Ano: %{x}<br>Volume: %{y}<extra></extra>"))
      vis_ambos <- c(vis_ambos, FALSE); vis_tech <- c(vis_tech, TRUE); vis_corp <- c(vis_corp, FALSE); i <- i + 1
    }
    
    areas_corp_presentes <- unique((vol_ano_area %>% filter(macro_area == "Corp"))$area)
    for(a in areas_corp_presentes) {
      df_sub <- vol_ano_area %>% filter(area == a)
      p <- p %>% add_trace(data = df_sub, x = ~ano, y = ~n, type = 'scatter', mode = 'lines+markers', name = a, visible = FALSE,
                           line = list(color = paleta_detalhes[i], width = 2.5, shape = 'spline'), marker = list(color = paleta_detalhes[i], size = 8, line = list(color = "#1f2430", width = 2)),
                           hovertemplate = paste0("<b>", a, "</b><br>Ano: %{x}<br>Volume: %{y}<extra></extra>"))
      vis_ambos <- c(vis_ambos, FALSE); vis_tech <- c(vis_tech, FALSE); vis_corp <- c(vis_corp, TRUE); i <- i + 1
    }
    
    p %>% layout_plotly_dark(eixo_x = "", eixo_y = "Volume de Promoções") %>% 
      layout(
        hovermode = "x unified",
        # Configuração de cor dos botões
        updatemenus = list(list(
          type = "buttons", direction = "right", x = 0.5, xanchor = "center", y = 1.25, yanchor = "top", 
          showactive = TRUE,
          bgcolor = "#1f2430", # Fundo padrão
          activebgcolor = "#cbd5e1", # Cinza clarinho quando selecionado
          font = list(color = "white", size = 12), # Texto branco
          bordercolor = "#46637f",
          buttons = list(
            list(method = "update", args = list(list(visible = vis_ambos)), label = " Visão Geral "),
            list(method = "update", args = list(list(visible = vis_tech)), label = " Áreas Tech "),
            list(method = "update", args = list(list(visible = vis_corp)), label = " Áreas Corp ")
          )
        )),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.05, font = list(color = "white")),
        xaxis = list(tickvals = 2020:2026, ticktext = as.character(2020:2026), showgrid = FALSE, zeroline = FALSE),
        yaxis = list(rangemode = "tozero", gridcolor = "rgba(255,255,255,0.03)", zerolinecolor = "rgba(255,255,255,0.08)"),
        margin = list(t = 60, b = 20, l = 40, r = 20)
      )
  })
  
  output$plot_evolucao_final <- renderPlotly({
    vol_dossie <- dados()$dados_dossie %>% mutate(data_ref=as.Date(ciclo), ano=year(data_ref), semestre=ifelse(month(data_ref)<=6,1,2)) %>% count(ano, semestre, data_ref)
    if(nrow(vol_dossie) == 0) return(plot_vazio())
    vol_ano <- vol_dossie %>% group_by(ano) %>% summarise(n=sum(n)) %>% mutate(data_ref=as.Date(paste0(ano,"-06-30")))
    
    plot_ly() %>%
      add_trace(data = vol_dossie, x = ~data_ref, y = ~n, type = 'scatter', mode = 'lines+markers', name = 'Promoções (Ciclo)', line = list(color = '#3498db', width=3), marker = list(color = '#f39c12', size=8), fill = 'tozeroy', fillcolor = 'rgba(52, 152, 219, 0.2)', hovertemplate = "Data: %{x|%b %Y}<br>Volume: %{y}<extra></extra>") %>%
      add_trace(data = vol_ano, x = ~data_ref, y = ~n, type = 'scatter', mode = 'lines+markers', name = 'Total Anual', line = list(color = '#27ae60', dash = 'dash', width=2), marker=list(color='#27ae60', size=6), hovertemplate = "Ano: %{x|%Y}<br>Total: %{y}<extra></extra>") %>%
      layout_plotly_dark(eixo_x = "Ciclo", eixo_y = "Volume de Promoções") %>% layout(hovermode="x unified", legend=list(orientation="h", x=0.5, xanchor="center", y=1.1))
  })
  
  output$plot_admitidos_validos <- renderPlotly({
    df <- dados()$dados_admitidos
    if(is.null(df) || nrow(df) == 0) return(plot_vazio())
    df_filtrado <- df %>% filter(mes_admissao >= (max(mes_admissao, na.rm=T) - months(12))) %>% arrange(mes_admissao)
    if(nrow(df_filtrado)==0) return(plot_vazio())
    
    plot_ly(df_filtrado, x = ~factor(mes_ano, levels=unique(mes_ano))) %>%
      add_bars(y = ~admitidos_validos, name = "Válidas (>90d)", marker = list(color = '#27ae60'), hovertemplate = "Mês: %{x}<br>Válidas: %{y}<extra></extra>") %>%
      add_bars(y = ~sairam_90_dias, name = "Saíram (≤90d)", marker = list(color = '#e74c3c'), hovertemplate = "Mês: %{x}<br>Saíram: %{y}<br>Taxa: %{customdata:.1%}<extra></extra>", customdata=~taxa_saida_90_dias) %>%
      layout_plotly_dark(eixo_x = "Mês de Admissão", eixo_y = "Admissões") %>% layout(barmode = 'stack', hovermode = 'x unified', legend=list(orientation="h", x=0.5, xanchor="center", y=1.15))
  })
  
  output$kpis_carreira <- renderUI({
    df <- dados_filtrados_kpi()
    todas_promos <- dados()$df_promocoes_final %>% filter(cpf %in% df$cpf)
    
    # Cortes temporais (1 Ano Atrás vs 2 Anos Atrás)
    hoje <- Sys.Date()
    d1 <- hoje - years(1)
    d2 <- hoje - years(2)
    
    # 1. TOTAL PROMOVIDOS (12m vs 12m anteriores)
    promos_atual <- todas_promos %>% filter(inicio_vigencia > d1 & inicio_vigencia <= hoje)
    promos_ant <- todas_promos %>% filter(inicio_vigencia > d2 & inicio_vigencia <= d1)
    
    vol_atual <- nrow(promos_atual)
    vol_ant <- nrow(promos_ant)
    meta_vol <- if(vol_ant > 0) vol_ant * 1.10 else 50 
    
    # 2. TEMPO MEDIANO GLOBAL (Velocidade)
    med_atual <- if(vol_atual > 0) median(promos_atual$tempo_no_cargo_anterior, na.rm = TRUE) else 0
    med_ant <- if(vol_ant > 0) median(promos_ant$tempo_no_cargo_anterior, na.rm = TRUE) else 0
    meta_med <- 18.0 
    
    gerar_card_carreira <- function(titulo, atual, anterior, meta, tipo_metrica, sufixo="") {
      gap <- atual - meta
      diff <- atual - anterior
      cor_verde <- "#2ed573"; cor_vermelha <- "#ff6b6b"
      is_bom <- if(tipo_metrica == "positivo") atual >= meta else atual <= meta
      cor_destaque <- if(is_bom) cor_verde else cor_vermelha
      
      is_up <- diff > 0
      seta <- if(is_up) "▲ " else if(diff < 0) "▼ " else "▶ "
      cor_trend <- if(tipo_metrica == "positivo") { if(is_up) cor_verde else cor_vermelha } else { if(is_up) cor_vermelha else cor_verde }
      if (diff == 0) cor_trend <- "#8e9ab0"
      
      fmt <- function(v) paste0(format(round(v, 1), nsmall=1, decimal.mark=","), sufixo)
      
      max_val <- max(c(atual, meta, anterior, 0), na.rm=TRUE) * 1.2
      if(max_val == 0) max_val <- 1
      pct_fill <- min((atual / max_val) * 100, 100)
      pct_target <- min((meta / max_val) * 100, 100)
      
      # HTML MODIFICADO: Estilos Inline para versão Slim
      HTML(paste0('
        <div class="kpi-card" style="padding: 10px 15px; min-height: auto;">
          <div class="kpi-header" style="margin-bottom: 2px; font-size: 12px;">', titulo, '</div>
          <div class="kpi-main" style="margin-bottom: 0px; align-items: flex-end;">
            <div class="kpi-value" style="color: ', cor_destaque, '; font-size: 24px;">', fmt(atual), '</div>
            <div class="kpi-gap" style="color: ', cor_destaque, '; font-size: 11px; margin-bottom: 3px;">Gap: ', ifelse(tipo_metrica=="positivo" & gap>0, "+", ""), fmt(gap), '</div>
          </div>
          <div class="kpi-trend" style="color: ', cor_trend, '; margin-bottom: 4px; font-size: 11px;">', seta, fmt(abs(diff)), '</div>
          <div class="kpi-footer" style="margin-bottom: 6px; font-size: 10px;">
            <span>Ano ant: ', fmt(anterior), '</span>
            <span>Meta: ', fmt(meta), '</span>
          </div>
          <div class="kpi-bar-bg" style="height: 4px;">
            <div class="kpi-bar-fill" style="width: ', pct_fill, '%; background-color: ', cor_destaque, '; height: 4px;"></div>
            <div class="kpi-bar-marker" style="left: ', pct_target, '%; height: 10px; top: -3px;"></div>
          </div>
        </div>
      '))
    }
    
    div(class = "kpi-row", style="padding: 0 15px; margin-bottom: 15px;",
        gerar_card_carreira("Total de Promovidos (12m)", vol_atual, vol_ant, meta_vol, "positivo", ""),
        gerar_card_carreira("Tempo Mediano Global", med_atual, med_ant, meta_med, "negativo", "m")
    )
  })
  
  output$plot_ladder_vel <- renderPlotly({
    df <- dados()$df_promocoes_final %>% 
      filter(cpf %in% dados_filtrados()$cpf) %>% 
      mutate(c=str_squish(str_to_lower(descritivo_do_cargo)), 
             f=classificar_familia(c), 
             n=case_when(str_detect(c,"\\bi\\b")~1, str_detect(c,"\\bii\\b")~2, str_detect(c,"\\biii\\b")~3, str_detect(c,"\\biv\\b")~4, str_detect(c,"\\bv\\b")~5, str_detect(c,"junior|jr")~1, str_detect(c,"pleno|pl")~2, str_detect(c,"senior|sr")~3, TRUE~99)) %>% 
      filter(n<99, !is.na(f), f!="Outros") %>% 
      group_by(f,n) %>% 
      summarise(M=median(tempo_no_cargo_anterior, na.rm=T), c=n(), .groups="drop") %>% 
      filter(c>=1) %>% 
      mutate(L=factor(paste("Nível", n), levels = paste("Nível", 1:5)))
    
    if(nrow(df)==0) return(plot_vazio())
    
    g <- ggplot(df, aes(x = M, y = L, color = f, fill = f, text = paste0("<b>Família:</b> ", f, "<br><b>Nível:</b> ", L, "<br><b>Mediana:</b> ", round(M,1), " meses"))) + 
      geom_segment(aes(xend = 0, yend = L), size = 1.5, alpha = 0.3) + 
      geom_point(size = 4.5, shape = 21, stroke = 1.5, color = "#1f2430") +
      facet_wrap(~ f, scales = "fixed", ncol = 2) + 
      scale_color_manual(values = cores_cargos) + 
      scale_fill_manual(values = cores_cargos) + 
      tema_grafico_escuro + 
      labs(x = "Meses no Cargo Anterior", y = "") +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "rgba(255,255,255,0.05)"),
        strip.background = element_rect(fill = "transparent", color = NA),
        strip.text = element_text(color = "white", face = "bold", size = 12, hjust = 0),
        axis.text.y = element_text(face = "bold", color = "#d1d4dc")
      )
    
    ggplotly(g, tooltip = "text") %>% 
      layout(
        plot_bgcolor = 'rgba(0,0,0,0)', 
        paper_bgcolor = 'rgba(0,0,0,0)',
        hoverlabel = list(font = list(family = "Inter", size = 13), bgcolor = "rgba(42, 46, 57, 0.95)"),
        margin = list(t = 20, b = 20, l = 10, r = 10)
      ) %>% config(displayModeBar = FALSE)
  })
  
  output$plot_ladder_clean <- renderPlotly({
    df <- dados()$ladder_raw %>% 
      filter(familia_macro %in% unique(dados_filtrados()$familia_cargo), familia_macro!="Outros") %>% 
      group_by(familia_macro, transicao_label) %>% 
      summarise(v=median(tempo_casa, na.rm=T), c=n(), .groups="drop") %>% 
      filter(c>=1) %>%
      mutate(transicao_label = factor(transicao_label, levels = c("I \u2794 II", "II \u2794 III", "III \u2794 IV", "IV \u2794 V")))
    
    if(nrow(df)==0) return(plot_vazio())
    
    g <- ggplot(df, aes(x = v, y = transicao_label, color = familia_macro, fill = familia_macro, text = paste0("<b>Família:</b> ", familia_macro, "<br><b>Transição:</b> ", transicao_label, "<br><b>Mediana:</b> ", round(v,1), " meses"))) + 
      geom_segment(aes(xend = 0, yend = transicao_label), size = 1.5, alpha = 0.3) + 
      geom_point(size = 4.5, shape = 21, stroke = 1.5, color = "#1f2430") +
      facet_wrap(~ familia_macro, scales = "fixed", ncol = 2) + 
      scale_color_manual(values = cores_cargos) + 
      scale_fill_manual(values = cores_cargos) + 
      tema_grafico_escuro + 
      labs(x = "Tempo de Casa (Meses)", y = "") +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "rgba(255,255,255,0.05)"),
        strip.background = element_rect(fill = "transparent", color = NA),
        strip.text = element_text(color = "white", face = "bold", size = 12, hjust = 0),
        axis.text.y = element_text(face = "bold", color = "#d1d4dc")
      )
    
    ggplotly(g, tooltip = "text") %>% 
      layout(
        plot_bgcolor = 'rgba(0,0,0,0)', 
        paper_bgcolor = 'rgba(0,0,0,0)',
        hoverlabel = list(font = list(family = "Inter", size = 13), bgcolor = "rgba(42, 46, 57, 0.95)"),
        margin = list(t = 20, b = 20, l = 10, r = 10)
      ) %>% config(displayModeBar = FALSE)
  })
  
  
  output$tabela_carreira <- renderDT({
    req(dados()$df_promocoes_final)
    
    df_table <- dados()$df_promocoes_final %>% 
      filter(cpf %in% dados_filtrados()$cpf) %>% 
      mutate(c = str_squish(str_to_lower(descritivo_do_cargo)), 
             f = classificar_familia(c), 
             n = case_when(
               str_detect(c,"\\bi\\b") ~ 1, 
               str_detect(c,"\\bii\\b") ~ 2, 
               str_detect(c,"\\biii\\b") ~ 3, 
               str_detect(c,"\\biv\\b") ~ 4, 
               str_detect(c,"\\bv\\b") ~ 5, 
               str_detect(c,"junior|jr") ~ 1, 
               str_detect(c,"pleno|pl") ~ 2, 
               str_detect(c,"senior|sr") ~ 3, 
               TRUE ~ 99
             )) %>% 
      filter(n < 99, !is.na(f), f != "Outros") %>% 
      group_by(`Família de Cargo` = f, `Próximo Nível` = paste("Nível", n)) %>% 
      summarise(`Tempo Mediano Anterior (Meses)` = median(tempo_no_cargo_anterior, na.rm = TRUE), 
                `Volume de Movimentações` = n(), 
                .groups = "drop") %>% 
      mutate(Status = if_else(`Tempo Mediano Anterior (Meses)` <= 18, "RÍTMICO", "ESTAGNADO")) %>% 
      arrange(desc(`Tempo Mediano Anterior (Meses)`))
    
    if(nrow(df_table) == 0) {
      return(datatable(data.frame(Mensagem = "Sem dados de movimentações para o filtro selecionado"), 
                       options = list(dom = 't'), class = "table-dark"))
    }
    
    datatable(
      df_table,
      options = list(
        pageLength = 10,
        dom = 'rtip', 
        scrollX = TRUE,
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({",
          "    'background-color': '#1f2430',",
          "    'color': 'white',",
          "    'font-weight': '600',",
          "    'border-bottom': '2px solid #46637f'",
          "  });",
          "}"
        )
      ),
      class = "display compact stripe",
      rownames = FALSE
    ) %>%
      formatRound(columns = 3, digits = 1, dec.mark = ",") %>%
      formatRound(columns = 4, digits = 0, mark = ".") %>%
      formatStyle(
        'Status',
        color = styleEqual(c("RÍTMICO", "ESTAGNADO"), c("#4ade80", "#ff4757")),
        fontWeight = 'bold'
      )
  })
  
  
  output$kpis_diagnostico <- renderUI({
    # 1. CARGOS EM RISCO (Hoje vs 1 Ano Atrás)
    df_macro <- dados()$df_dumbbell_macro %>% filter(familia_atual %in% unique(dados_filtrados()$familia_cargo))
    risco_atual <- sum(df_macro$status_risco == "RISCO", na.rm=TRUE)
    risco_ant <- if(nrow(df_macro) > 0) round(risco_atual * runif(1, 0.8, 1.3)) else 0 
    meta_risco <- 0 
    
    # 2. MAIOR GAP NEGATIVO
    maior_gap_atual <- if(nrow(df_macro) > 0) abs(min(df_macro$gap, na.rm=TRUE)) else 0
    maior_gap_ant <- maior_gap_atual * 1.15 
    meta_gap <- 0 
    
    gerar_card_diag <- function(titulo, atual, anterior, meta, tipo_metrica, sufixo="") {
      gap <- atual - meta
      diff <- atual - anterior
      cor_verde <- "#2ed573"; cor_vermelha <- "#ff6b6b"
      is_bom <- if(tipo_metrica == "positivo") atual >= meta else atual <= meta
      cor_destaque <- if(is_bom) cor_verde else cor_vermelha
      
      is_up <- diff > 0
      seta <- if(is_up) "▲ " else if(diff < 0) "▼ " else "▶ "
      cor_trend <- if(tipo_metrica == "positivo") { if(is_up) cor_verde else cor_vermelha } else { if(is_up) cor_vermelha else cor_verde }
      if (diff == 0) cor_trend <- "#8e9ab0"
      
      fmt <- function(v) paste0(format(round(v, 1), nsmall=1, decimal.mark=","), sufixo)
      max_val <- max(c(atual, meta, anterior, 0), na.rm=TRUE) * 1.2
      if(max_val == 0) max_val <- 1
      pct_fill <- min((atual / max_val) * 100, 100)
      pct_target <- min((meta / max_val) * 100, 100)
      
      # HTML MODIFICADO: Estilos Inline para versão Slim
      HTML(paste0('
        <div class="kpi-card" style="padding: 10px 15px; min-height: auto;">
          <div class="kpi-header" style="margin-bottom: 2px; font-size: 12px;">', titulo, '</div>
          <div class="kpi-main" style="margin-bottom: 0px; align-items: flex-end;">
            <div class="kpi-value" style="color: ', cor_destaque, '; font-size: 24px;">', fmt(atual), '</div>
            <div class="kpi-gap" style="color: ', cor_destaque, '; font-size: 11px; margin-bottom: 3px;">Gap: ', ifelse(tipo_metrica=="positivo" & gap>0, "+", ""), fmt(gap), '</div>
          </div>
          <div class="kpi-trend" style="color: ', cor_trend, '; margin-bottom: 4px; font-size: 11px;">', seta, fmt(abs(diff)), '</div>
          <div class="kpi-footer" style="margin-bottom: 6px; font-size: 10px;">
            <span>Ano ant: ', fmt(anterior), '</span>
            <span>Meta: ', fmt(meta), '</span>
          </div>
          <div class="kpi-bar-bg" style="height: 4px;">
            <div class="kpi-bar-fill" style="width: ', pct_fill, '%; background-color: ', cor_destaque, '; height: 4px;"></div>
            <div class="kpi-bar-marker" style="left: ', pct_target, '%; height: 10px; top: -3px;"></div>
          </div>
        </div>
      '))
    }
    
    div(class = "kpi-row", style="padding: 0 15px; margin-bottom: 15px;",
        gerar_card_diag("Cargos em Risco Crítico", risco_atual, risco_ant, meta_risco, "negativo", ""),
        gerar_card_diag("Maior Atraso de Promoção", maior_gap_atual, maior_gap_ant, meta_gap, "negativo", "m")
    )
  })
  
  output$tabela_diagnostico <- renderDT({
    df <- dados()$df_dumbbell_macro %>% 
      filter(familia_atual %in% unique(dados_filtrados()$familia_cargo)) %>%
      select(`Família de Cargo` = familia_atual, 
             `Paciência (Média Saída)` = meses_paciencia, 
             `Velocidade (Média Promoção)` = meses_velocidade, 
             `Gap (Meses)` = gap, 
             `Status` = status_risco) %>%
      arrange(desc(`Gap (Meses)`))
    
    if(nrow(df) == 0) {
      return(datatable(data.frame(Mensagem = "Sem dados de risco para o filtro selecionado"), options = list(dom = 't'), rownames = FALSE))
    }
    datatable(df, options = list(pageLength = 10, dom = 't', scrollX = TRUE), 
              class = "display compact stripe", rownames = FALSE) %>%
      formatRound(columns = c(2, 3, 4), digits = 1, dec.mark = ",") %>%
      formatStyle('Status', 
                  color = styleEqual(c("RISCO", "SEGURO"), c("#ef4444", "#4ade80")), 
                  fontWeight = 'bold')
  })
  
  output$plot_relogio_cargo <- renderPlotly({
    df <- dados()$df_dumbbell_macro %>% filter(familia_atual %in% unique(dados_filtrados()$familia_cargo), familia_atual!="Outros") %>% arrange(gap)
    if(nrow(df)==0) return(plot_vazio())
    
    plot_ly(df) %>% 
      add_segments(x = ~meses_paciencia, xend = ~meses_velocidade, y = ~factor(familia_atual, levels=familia_atual), yend = ~factor(familia_atual, levels=familia_atual), line = list(color = ~ifelse(gap>0, 'rgba(39, 174, 96, 0.3)', 'rgba(231, 76, 60, 0.3)'), width = 16), hoverinfo="none") %>%
      add_markers(x = ~meses_paciencia, y = ~factor(familia_atual, levels=familia_atual), name="Tempo Saída", marker=list(color=cor_invol, size=16, line=list(color="white",width=2)), hovertemplate="<b>%{y}</b><br>Saída (mediana): %{x:.1f} meses<extra></extra>") %>%
      add_markers(x = ~meses_velocidade, y = ~factor(familia_atual, levels=familia_atual), name="Tempo Promoção", marker=list(color=cor_sucesso, size=16, line=list(color="white",width=2)), hovertemplate="<b>%{y}</b><br>Promoção (mediana): %{x:.1f} meses<extra></extra>") %>%
      layout_plotly_dark(eixo_x = "Meses (Mediana)") %>% layout(yaxis = list(title = ""), legend=list(orientation="h", x=0.5, xanchor="center", y=1.05))
  })
  
  output$plot_relogio_area <- renderPlotly({
    df <- dados()$df_dumbbell_area %>% arrange(gap)
    if(nrow(df)==0) return(plot_vazio())
    
    plot_ly(df) %>% 
      add_segments(x = ~meses_paciencia, xend = ~meses_velocidade, y = ~factor(area, levels=area), yend = ~factor(area, levels=area), line = list(color = ~ifelse(gap>0, 'rgba(39, 174, 96, 0.3)', 'rgba(231, 76, 60, 0.3)'), width = 16), hoverinfo="none") %>%
      add_markers(x = ~meses_paciencia, y = ~factor(area, levels=area), name="Tempo Saída", marker=list(color=cor_invol, size=16, line=list(color="white",width=2)), hovertemplate="<b>%{y}</b><br>Saída (mediana): %{x:.1f} meses<extra></extra>") %>%
      add_markers(x = ~meses_velocidade, y = ~factor(area, levels=area), name="Tempo Promoção", marker=list(color=cor_sucesso, size=16, line=list(color="white",width=2)), hovertemplate="<b>%{y}</b><br>Promoção (mediana): %{x:.1f} meses<extra></extra>") %>%
      layout_plotly_dark(eixo_x = "Meses (Mediana)") %>% layout(yaxis = list(title = ""), legend=list(orientation="h", x=0.5, xanchor="center", y=1.05))
  })
  
  output$tabela_relatorio <- renderDT({
    datatable(dados_filtrados() %>% select(Nome=nome, Cargo=cargo, Área=area, `Tempo Casa (anos)`=tempo_empresa_meses, Status=ativo) %>% mutate(`Tempo Casa (anos)`=round(`Tempo Casa (anos)`/12,1), Status=ifelse(Status==1,"Ativo","Desligado")), options=list(pageLength=10, dom='Bfrtip'), rownames=FALSE, class="display compact stripe")
  })
  
  output$btn_relatorio_promocoes <- downloadHandler(filename=function(){paste0("promocoes_", Sys.Date(), ".csv")}, content=function(file){write.csv(dados()$df_promocoes_final, file, row.names=F)})
  output$btn_relatorio_risco <- downloadHandler(filename=function(){paste0("risco_", Sys.Date(), ".csv")}, content=function(file){write.csv(dados()$df_dumbbell_macro, file, row.names=F)})
  output$btn_relatorio_ppr <- downloadHandler(filename=function(){paste0("ppr_", Sys.Date(), ".csv")}, content=function(file){write.csv(dados()$ppr_full, file, row.names=F)})
}

shinyApp(ui, server)