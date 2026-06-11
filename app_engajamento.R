library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(tidyr)
library(stringr)
library(plotly)
library(shinyWidgets)
library(scales)
library(forecast)
library(DT)
library(tm)
library(RColorBrewer)
library(purrr)
library(zoo)
library(forcats)
library(janitor)

Sys.setlocale("LC_TIME", "Portuguese_Brazil.1252")


# CONFIGURAÇÕES INICIAIS 
BASE_PATH <- tempdir()
LOGO_PATH <- NULL  


# FUNÇÕES AUXILIARES 
parse_pt_date <- function(date_vec) {
  if(all(is.na(date_vec))) return(as.Date(NA))
  date_vec <- as.character(date_vec)
  date_vec <- str_to_lower(date_vec)
  months_pt <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", 
                 "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
  months_en <- month.name
  
  for(i in 1:12) {
    date_vec <- str_replace_all(date_vec, months_pt[i], months_en[i])
  }
  date_vec <- str_replace_all(date_vec, " de ", " ")
  date_vec <- str_replace_all(date_vec, "º", "")
  
  result <- as.Date(parse_date_time(date_vec, orders = c("d m Y", "d B Y", "Y-m-d", "d/m/Y")))
  return(result)
}

calc_nps <- function(satisfacao) {
  if(length(satisfacao) == 0) return(NA)
  satisfacao <- as.numeric(satisfacao)
  satisfacao <- satisfacao[!is.na(satisfacao)]
  if(length(satisfacao) == 0) return(NA)
  promotores <- sum(satisfacao >= 4.5, na.rm = TRUE)
  detratores <- sum(satisfacao <= 4, na.rm = TRUE)
  total <- length(satisfacao)
  if(total == 0) return(NA)
  return(round(((promotores - detratores) / total) * 100, 1))
}

classificar_sentimento <- function(nota) {
  nota <- as.numeric(nota)
  if(is.na(nota)) return(NA)
  if(nota >= 4) return("Positivo")
  if(nota >= 3) return("Neutro")
  return("Negativo")
}

create_kpi_card <- function(titulo, valor_atual, valor_anterior, meta, is_percent = FALSE, invert = FALSE, is_volume = FALSE, label_anterior = "Período anterior: ", label_meta = "Meta: ") {
  if(is.na(valor_atual)) valor_atual <- 0
  if(is.na(valor_anterior)) valor_anterior <- 0
  
  variacao <- valor_atual - valor_anterior
  gap_meta <- valor_atual - meta
  
  if (invert) {
    pct_atingimento <- 2 - (valor_atual / meta)
  } else {
    pct_atingimento <- valor_atual / meta
  }
  
  if (pct_atingimento >= 0.8) {
    cor_valor <- "#4ade80" 
  } else if (pct_atingimento >= 0.5) {
    cor_valor <- "#fbbf24" 
  } else {
    cor_valor <- "#f87171" 
  }
  
  escala_max <- if(is_volume) {
    max(meta * 1.2, valor_atual * 1.1, 10, na.rm = TRUE)
  } else if(is_percent) {
    100
  } else {
    5
  }
  
  largura_barra_css <- max(0, min((valor_atual / escala_max) * 100, 100))
  posicao_meta_css <- max(0, min((meta / escala_max) * 100, 100))
  
  if(invert) {
    cor_variacao <- ifelse(variacao <= 0, "#4ade80", "#f87171")
    cor_gap <- ifelse(gap_meta <= 0, "#4ade80", "#f87171")
    sinal_gap <- ifelse(gap_meta <= 0, "", "+")
    seta <- ifelse(variacao <= 0, "▼", "▲")
  } else {
    cor_variacao <- ifelse(variacao >= 0, "#4ade80", "#f87171")
    cor_gap <- ifelse(gap_meta >= 0, "#4ade80", "#f87171")
    sinal_gap <- ifelse(gap_meta >= 0, "+", "")
    seta <- ifelse(variacao >= 0, "▲", "▼")
  }
  
  if(is_percent) {
    valor_formatado <- paste0(format(round(valor_atual, 1), nsmall = 1, dec = ","), "%")
    variacao_formatada <- paste0(seta, " ", format(round(abs(variacao), 1), nsmall = 1, dec = ","), "%")
    meta_formatada <- paste0(label_meta, format(meta, nsmall = 0, dec = ","), "%")
    anterior_formatado <- paste0(label_anterior, format(round(valor_anterior, 1), nsmall = 1, dec = ","), "%")
    gap_formatado <- paste0("Gap: ", sinal_gap, format(round(gap_meta, 1), nsmall = 1, dec = ","), "%")
  } else if(is_volume) {
    valor_formatado <- format(round(valor_atual, 0), big.mark = ".", decimal.mark = ",")
    variacao_formatada <- paste0(seta, " ", format(round(abs(variacao), 0), big.mark = ".", decimal.mark = ","))
    meta_formatada <- paste0(label_meta, format(round(meta, 0), big.mark = ".", decimal.mark = ","))
    anterior_formatado <- paste0(label_anterior, format(round(valor_anterior, 0), big.mark = ".", decimal.mark = ","))
    gap_formatado <- paste0("Gap: ", sinal_gap, format(round(abs(gap_meta), 0), big.mark = ".", decimal.mark = ","))
  } else {
    valor_formatado <- format(round(valor_atual, 2), nsmall = 2, dec = ",")
    variacao_formatada <- paste0(seta, " ", format(round(abs(variacao), 2), nsmall = 2, dec = ","))
    meta_formatada <- paste0(label_meta, format(meta, nsmall = 1, dec = ","))
    anterior_formatado <- paste0(label_anterior, format(round(valor_anterior, 2), nsmall = 2, dec = ","))
    gap_formatado <- paste0("Gap: ", sinal_gap, format(round(gap_meta, 2), nsmall = 2, dec = ","))
  }
  
  div(
    style = "background: linear-gradient(135deg, #1a1f2e 0%, #0f1219 100%); border-radius: 8px; padding: 6px 8px; margin: 4px; box-shadow: 0 2px 8px rgba(0,0,0,0.2); border: 1px solid #2d3748;",
    
    div(style = "font-size: 14px; color: #94a3b8; font-weight: 600; margin-bottom: 4px;", titulo),
    
    div(style = "display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 2px;",
        div(style = paste0("font-size: 24px; font-weight: bold; color: ", cor_valor, ";"), valor_formatado),
        div(style = paste0("font-size: 12px; font-weight: bold; color: ", cor_gap, ";"), gap_formatado)
    ),
    
    div(style = paste0("font-size: 11px; color: ", cor_variacao, "; margin-bottom: 4px;"), variacao_formatada),
    
    div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
        div(style = "font-size: 12px; color: #94a3b8;", anterior_formatado),
        div(style = "font-size: 12px; color: #94a3b8;", meta_formatada)
    ),
    
    div(style = "position: relative; margin-top: 6px; height: 8px; background: #334155; border-radius: 4px; overflow: visible;",
        div(style = paste0("width: ", largura_barra_css, "%; height: 100%; background: ", cor_valor, "; border-radius: 4px; transition: width 0.5s ease-in-out;")),
        div(style = paste0("position: absolute; top: -2px; left: ", posicao_meta_css, "%; width: 2px; height: 12px; background: #ffffff; border-radius: 1px; box-shadow: 0 0 2px rgba(0,0,0,0.8);"))
    )
  )
}

format_num <- function(x) {
  if(is.na(x)) return("N/A")
  format(round(x, 0), big.mark = ".", decimal.mark = ",")
}

clean_numeric <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_replace_all(x, ",", ".")
  x <- str_replace_all(x, "%", "")
  x <- str_replace_all(x, "—", NA_character_)
  x <- str_replace_all(x, "-", NA_character_)
  x <- str_replace_all(x, "N/A", NA_character_)
  x <- ifelse(x == "" | x == " ", NA, x)
  as.numeric(x)
}

agrupar_demografia <- function(df, tipo = "localidade") {
  if(nrow(df) == 0) return(df)
  
  if(tipo == "localidade") {
    df <- df %>%
      mutate(Regiao = case_when(
        grepl("Rio Grande do Sul|Santa Catarina|Paraná|Porto Alegre|Curitiba|Florianópolis|Viamão|Caxias|Joinville", Localidade, ignore.case=T) ~ "Sul",
        grepl("São Paulo|Rio de Janeiro|Minas Gerais|Espírito Santo|Belo Horizonte|Campinas|Vitória", Localidade, ignore.case=T) ~ "Sudeste",
        grepl("Bahia|Pernambuco|Ceará|Recife|Salvador|Fortaleza|Maceió|Natal", Localidade, ignore.case=T) ~ "Nordeste",
        grepl("Goiás|Mato Grosso|Brasília|Goiânia", Localidade, ignore.case=T) ~ "Centro-Oeste",
        grepl("Amazonas|Pará|Manaus|Belém", Localidade, ignore.case=T) ~ "Norte",
        grepl("Reino Unido|Irlanda|Espanha|EUA|Portugal|Estados Unidos", Localidade, ignore.case=T) ~ "Exterior",
        TRUE ~ "Outros"
      ))
  } else if(tipo == "setor") {
    df <- df %>%
      mutate(Setor_Grupo = case_when(
        grepl("Tecnologia|Software|TI|Dados|Informação", Setor, ignore.case=T) ~ "TI & Software",
        grepl("Financeiros|Bancos|Contabilidade|Investimento", Setor, ignore.case=T) ~ "Finanças",
        grepl("Varejo|Comércio|Vendas|Atacadista", Setor, ignore.case=T) ~ "Varejo & Comércio",
        grepl("Educação|Ensino|Treinamento", Setor, ignore.case=T) ~ "Educação",
        grepl("Saúde|Hospitais|Médico|Odontológico", Setor, ignore.case=T) ~ "Saúde",
        grepl("Engenharia|Construção|Indústria|Fabricação", Setor, ignore.case=T) ~ "Engenharia & Indústria",
        grepl("Marketing|Publicidade|Mídia|Comunicação", Setor, ignore.case=T) ~ "Marketing & Mídia",
        TRUE ~ "Outros Serviços"
      ))
  }
  return(df)
}

create_dual_kpi_card <- function(titulo, label_esq, valor_esq, pct_esq, cor_esq, label_dir, valor_dir, pct_dir, cor_dir) {
  div(
    style = "background: linear-gradient(135deg, #1a1f2e 0%, #0f1219 100%); border-radius: 8px; padding: 12px; margin: 4px; box-shadow: 0 2px 8px rgba(0,0,0,0.2); border: 1px solid #2d3748;",
    div(style = "font-size: 13px; color: #e2e8f0; font-weight: 600; margin-bottom: 12px; text-transform: uppercase; text-align: center;", titulo),
    div(style = "display: flex; justify-content: space-between; align-items: center;",
        div(style = "flex: 1; text-align: center; border-right: 1px solid #374151;",
            div(style = "font-size: 11px; color: #94a3b8; margin-bottom: 4px;", label_esq),
            div(style = paste0("font-size: 22px; font-weight: bold; color: ", cor_esq, "; line-height: 1;"), pct_esq),
            div(style = "font-size: 11px; color: #64748b; margin-top: 4px;", valor_esq)
        ),
        div(style = "flex: 1; text-align: center;",
            div(style = "font-size: 11px; color: #94a3b8; margin-bottom: 4px;", label_dir),
            div(style = paste0("font-size: 22px; font-weight: bold; color: ", cor_dir, "; line-height: 1;"), pct_dir),
            div(style = "font-size: 11px; color: #64748b; margin-top: 4px;", valor_dir)
        )
    )
  )
}


# GERAÇÃO DE DADOS SINTÉTICOS
gerar_dados_glassdoor <- function(n = 500) {
  set.seed(42)
  
  datas <- seq.Date(as.Date("2023-01-01"), as.Date("2026-06-01"), by = "month")
  datas_avaliacao <- sample(datas, n, replace = TRUE)
  
  cargos_base <- c(
    "Desenvolvedor Senior", "Analista de Dados", "Coordenador de TI", 
    "Gerente de Produto", "Tech Lead", "Desenvolvedor Junior",
    "Analista de Suporte", "UX Designer", "DevOps Engineer", 
    "Product Owner", "Estagiário", "Diretor"
  )
  
  cargos <- rep(cargos_base, length.out = n)
  
  status <- sample(c("Atual", "Ex-funcionário"), n, prob = c(0.6, 0.4), replace = TRUE)
  
  satisfacao_base <- rnorm(n, mean = 3.2, sd = 1.2)
  satisfacao_base <- pmax(1, pmin(5, satisfacao_base))
  
  idx_2026 <- year(datas_avaliacao) == 2026
  satisfacao_base[idx_2026] <- satisfacao_base[idx_2026] + 0.5
  satisfacao_base <- pmax(1, pmin(5, satisfacao_base))
  
  df <- data.frame(
    Data_avaliacao = datas_avaliacao,
    Cargo = cargos,
    Status = status,
    Satisfacao_geral = satisfacao_base,
    Oportunidades_carreira = pmax(1, pmin(5, satisfacao_base + rnorm(n, 0, 0.4))),
    Remuneracao_beneficios = pmax(1, pmin(5, satisfacao_base + rnorm(n, -0.2, 0.5))),
    Alta_lideranca = pmax(1, pmin(5, satisfacao_base + rnorm(n, -0.3, 0.6))),
    Qualidade_vida = pmax(1, pmin(5, satisfacao_base + rnorm(n, 0.1, 0.5))),
    Cultura_valores = pmax(1, pmin(5, satisfacao_base + rnorm(n, 0.2, 0.4))),
    Diversidade_inclusao = pmax(1, pmin(5, satisfacao_base + rnorm(n, 0.3, 0.4))),
    Recomendam = sample(c("Sim", "Não"), n, prob = c(0.65, 0.35), replace = TRUE),
    Visao_mercado = sample(c("Melhorando", "Igual", "Piorando"), n, prob = c(0.4, 0.35, 0.25), replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  df <- df %>%
    mutate(
      Data_Fmt = as.Date(Data_avaliacao),
      Ano = year(Data_Fmt),
      Mes = floor_date(Data_Fmt, "month"),
      Trimestre = paste0(Ano, "-Q", quarter(Data_Fmt)),
      Sentimento = sapply(Satisfacao_geral, classificar_sentimento),
      Recomendacao_bin = ifelse(Recomendam == "Sim", 1, 0),
      Aprovacao_CEO_bin = sample(c(1, 0), n, prob = c(0.7, 0.3), replace = TRUE),
      Cargo_Nivel = case_when(
        grepl("Líder|Lider|Gerente|Manager|Coordenador|Diretor|Head|Supervisor|Lead|Tech Lead|Team Leader", Cargo, ignore.case = TRUE) ~ "Liderança/Gestão",
        grepl("Especialista|Specialist", Cargo, ignore.case = TRUE) ~ "Especialista",
        grepl("Analista", Cargo, ignore.case = TRUE) ~ "Analista",
        grepl("Desenvolvedor|Developer|Dev|Engenheiro|Programador", Cargo, ignore.case = TRUE) ~ "Desenvolvedor",
        grepl("Estágio|Estagiário|Trainee|Aprendiz|Intern", Cargo, ignore.case = TRUE) ~ "Estágio/Trainee",
        grepl("Suporte|Support", Cargo, ignore.case = TRUE) ~ "Suporte",
        TRUE ~ "Outros"
      ),
      Cargo_Area = case_when(
        grepl("Desenvolvedor|Developer|Dev", Cargo, ignore.case = TRUE) ~ "Desenvolvimento",
        grepl("Dados|Data|BI", Cargo, ignore.case = TRUE) ~ "Análise de Dados",
        grepl("Infra|DevOps|Cloud", Cargo, ignore.case = TRUE) ~ "Infraestrutura",
        grepl("Produto|Product|PO", Cargo, ignore.case = TRUE) ~ "Produto",
        grepl("UX|Design", Cargo, ignore.case = TRUE) ~ "Design",
        grepl("Suporte|Support", Cargo, ignore.case = TRUE) ~ "Suporte",
        TRUE ~ "Outras"
      )
    )
  
  return(df)
}

gerar_dados_concorrentes <- function() {
  set.seed(42)
  
  aspectos <- c("Classificação geral", "Oportunidades de carreira", "Compensation And Benefits",
                "Cultura e valores", "Alta Liderança", "Qualidade de vida", "Recommend To A Friend")
  
  empresas <- c("EMPRESA 1", "EMPRESA 2", "EMPRESA 3", "EMPRESA 3", "EMPRESA 4", "EMPRESA 5")
  
  notas_empresa1 <- c(3.8, 3.6, 3.4, 3.9, 3.5, 3.7, 68)
  
  df <- data.frame(Aspecto = aspectos, stringsAsFactors = FALSE)
  
  for(emp in empresas) {
    if(emp == "EMPRESA 1") {
      df[[emp]] <- notas_empresa1
    } else {
      base_nota <- notas_empresa1 + rnorm(length(aspectos), mean = -0.2, sd = 0.3)
      base_nota[7] <- notas_empresa1[7] + rnorm(1, -5, 10)
      df[[emp]] <- pmax(1, pmin(5, base_nota))
      df[[emp]][7] <- pmax(0, pmin(100, df[[emp]][7]))
    }
  }
  
  empresas_sem_empresa1 <- empresas[empresas != "EMPRESA 1"]
  
  # Calcular média linha a linha 
  df$Media_Glassdoor <- apply(df[, empresas_sem_empresa1, drop = FALSE], 1, function(row) {
    row_num <- as.numeric(row)
    mean(row_num, na.rm = TRUE)
  })
  
  return(df)
}

gerar_dados_linkedin_visao_geral <- function() {
  set.seed(42)
  
  datas <- seq.Date(as.Date("2024-01-01"), as.Date("2026-05-01"), by = "month")
  
  # Seguidores com crescimento
  seguidores_base <- 5000
  crescimento <- cumsum(rnorm(length(datas), mean = 150, sd = 50))
  seguidores <- seguidores_base + crescimento
  seguidores <- pmax(1000, round(seguidores))
  
  # Novos seguidores (diferença + ruído)
  novos <- c(seguidores[1], diff(seguidores)) + rnorm(length(datas), 0, 30)
  novos <- pmax(0, round(novos))
  
  # Engajamentos (correlacionado com seguidores)
  engajamentos <- round(seguidores * 0.05 + rnorm(length(datas), 0, 100))
  engajamentos <- pmax(0, engajamentos)
  
  data.frame(
    Data_Ref = datas,
    Mes = month(datas),
    Ano = year(datas),
    Total_Seguidores = seguidores,
    Novos_Seguidores = novos,
    Engajamentos = engajamentos,
    Variacao_Seguidores = c(NA, diff(seguidores)),
    Taxa_Crescimento = c(NA, diff(seguidores) / head(seguidores, -1) * 100)
  )
}

gerar_dados_linkedin_visitantes_serie <- function() {
  set.seed(42)
  
  datas <- seq.Date(as.Date("2024-01-01"), as.Date("2026-05-01"), by = "month")
  
  # Tendência de crescimento
  views_base <- 8000
  crescimento <- cumsum(rnorm(length(datas), mean = 200, sd = 100))
  total_views <- views_base + crescimento
  total_views <- pmax(1000, round(total_views))
  
  total_unicos <- round(total_views * (0.6 + runif(1, -0.1, 0.1)))
  unicos_pc <- round(total_unicos * (0.45 + rnorm(length(datas), 0, 0.05)))
  unicos_mobile <- total_unicos - unicos_pc
  
  visualizacoes_corp <- round(total_views * (0.7 + rnorm(length(datas), 0, 0.05)))
  visualizacoes_vagas <- total_views - visualizacoes_corp
  
  media_diaria <- total_views / 30
  
  data.frame(
    Data_Ref = datas,
    Total_Views = total_views,
    Total_Unicos = total_unicos,
    Unicos_PC = unicos_pc,
    Unicos_Mobile = unicos_mobile,
    Visualizacoes_Corp = visualizacoes_corp,
    Visualizacoes_Vagas = visualizacoes_vagas,
    Media_Diaria_Views = round(media_diaria, 1)
  )
}

gerar_demografia_linkedin <- function(tipo = "visitantes") {
  set.seed(42)
  
  datas <- seq.Date(as.Date("2024-01-01"), as.Date("2026-05-01"), by = "month")
  
  # Funções
  funcoes <- c("Engenharia de Software", "Análise de Dados", "Produto", "UX/Design", 
               "Suporte Técnico", "Vendas", "Marketing", "RH", "Financeiro")
  
  dados_funcoes <- expand.grid(Data_Ref = datas, Funcao = funcoes) %>%
    mutate(Valor = round(runif(n(), 50, 500) * (1 + 0.02 * (year(Data_Ref) - 2024))))
  
  # Localidades
  localidades <- c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Porto Alegre", "Curitiba", 
                   "Recife", "Brasília", "Salvador", "Exterior")
  
  dados_localidades <- expand.grid(Data_Ref = datas, Localidade = localidades) %>%
    mutate(Valor = round(runif(n(), 30, 400) * (1 + 0.02 * (year(Data_Ref) - 2024))))
  
  # Tamanhos de empresa
  tamanhos <- c("1", "2-10", "11-50", "51-200", "201-500", "501-1.000", "1.001-5.000", "5.001-10.000", "+ de 10.001")
  
  dados_tamanhos <- expand.grid(Data_Ref = datas, Tamanho = tamanhos) %>%
    mutate(Valor = round(runif(n(), 20, 300) * (1 + 0.02 * (year(Data_Ref) - 2024))))
  
  # Experiência
  experiencias <- c("Iniciante", "Sênior", "Gerente", "Diretor", "Vice-Presidente")
  
  dados_experiencias <- expand.grid(Data_Ref = datas, Experiencia = experiencias) %>%
    mutate(Valor = round(runif(n(), 30, 400) * (1 + 0.02 * (year(Data_Ref) - 2024))))
  
  # Setores
  setores <- c("TI & Software", "Finanças", "Varejo & Comércio", "Educação", "Saúde", "Engenharia & Indústria")
  
  dados_setores <- expand.grid(Data_Ref = datas, Setor = setores) %>%
    mutate(Valor = round(runif(n(), 40, 350) * (1 + 0.02 * (year(Data_Ref) - 2024))))
  
  # Ajustar coluna de valor conforme tipo
  col_valor <- ifelse(tipo == "visitantes", "Total_Visualizacoes", "Total_Seguidores")
  
  list(
    funcoes = dados_funcoes %>% rename(!!col_valor := Valor),
    localidades = dados_localidades %>% rename(!!col_valor := Valor),
    tamanhos = dados_tamanhos %>% rename(!!col_valor := Valor),
    experiencias = dados_experiencias %>% rename(!!col_valor := Valor),
    setores = dados_setores %>% rename(!!col_valor := Valor)
  )
}


# CARREGAMENTO DOS DADOS SINTÉTICOS
load_all_data <- function() {
  message("Carregando dados sintéticos...")
  data_list <- list()

  data_list$main <- gerar_dados_glassdoor(500)
  message("  - Glassdoor: ", nrow(data_list$main), " avaliações")
  
  # Concorrentes
  data_list$comp <- gerar_dados_concorrentes()
  message("  - Benchmark: ", nrow(data_list$comp), " aspectos")
  
  # LinkedIn Visão Geral
  data_list$linkedin_visao_geral <- gerar_dados_linkedin_visao_geral()
  message("  - LinkedIn Visão Geral: ", nrow(data_list$linkedin_visao_geral), " meses")
  
  # LinkedIn Visitantes (série temporal)
  data_list$linkedin_visitantes_serie <- gerar_dados_linkedin_visitantes_serie()
  message("  - LinkedIn Série Visitantes: ", nrow(data_list$linkedin_visitantes_serie), " meses")
  
  # LinkedIn Visitantes (demografia)
  data_list$linkedin_visitantes <- gerar_demografia_linkedin("visitantes")
  message("  - LinkedIn Visitantes (demografia): carregado")
  
  # LinkedIn Seguidores (demografia)
  data_list$linkedin_seguidores <- gerar_demografia_linkedin("seguidores")
  message("  - LinkedIn Seguidores (demografia): carregado")
  
  # Evolução combinada
  if(nrow(data_list$linkedin_visao_geral) > 0 && nrow(data_list$linkedin_visitantes_serie) > 0) {
    data_list$linkedin_evolucao <- data_list$linkedin_visao_geral %>%
      select(Data_Ref, Total_Seguidores, Novos_Seguidores, Engajamentos) %>%
      left_join(data_list$linkedin_visitantes_serie, by = "Data_Ref")
  } else {
    data_list$linkedin_evolucao <- data.frame()
  }
  
  # Aplicar agrupamentos de demografia
  data_list$linkedin_visitantes$localidades <- agrupar_demografia(data_list$linkedin_visitantes$localidades, "localidade")
  data_list$linkedin_visitantes$setores <- agrupar_demografia(data_list$linkedin_visitantes$setores, "setor")
  data_list$linkedin_seguidores$localidades <- agrupar_demografia(data_list$linkedin_seguidores$localidades, "localidade")
  data_list$linkedin_seguidores$setores <- agrupar_demografia(data_list$linkedin_seguidores$setores, "setor")
  
  message("Dados carregados com sucesso!")
  return(data_list)
}

# Carrega todos os dados sintéticos
DATA <- load_all_data()


# UI - INTERFACE DO USUÁRIO

ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center; gap: 12px;",
    span("EMPRESA 1 - People Analytics Dashboard", style = "font-weight: 700; font-size: 20px; color: #f1f5f9;")
  ),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    bg = "#0a0c10",
    fg = "#e2e8f0",
    primary = "#4ade80",
    "navbar-bg" = "#0f1219"
  ),
  
  sidebar = sidebar(
    width = 280,
    title = tags$h5("Filtros", style = "font-weight: 700; margin-bottom: 20px; color: #e2e8f0;"),
    
    selectInput("status", "Status",
                choices = c("Todos", "Atual", "Ex-funcionário")),
    
    selectInput("sentimento", "Sentimento",
                choices = c("Todos", "Positivo", "Neutro", "Negativo")),
    
    selectInput("ano", "Ano",
                choices = if(!is.null(DATA$main) && is.data.frame(DATA$main) && nrow(DATA$main) > 0) {
                  c("Todos", sort(unique(DATA$main$Ano), decreasing = TRUE))
                } else {
                  c("Todos", 2026)
                },
                selected = "Todos"),
    
    hr(),
    actionButton("refresh", "Atualizar Dados", 
                 style = "width: 100%; background: linear-gradient(135deg, #4ade80, #22c55e); color: #000; font-weight: bold; border: none; border-radius: 8px; padding: 10px;"),
    br(), br(),
    div(style = "text-align: center; font-size: 11px; color: #64748b;",
        "EMPRESA 1 - People Analytics", br(),
        "Dashboard v5.0 (Sintético)", br(),
        "Atualizado: ", format(Sys.Date(), "%d/%m/%Y"))
  ),
  
  # PÁGINA 1: ANÁLISE GLASSDOOR 
  nav_panel("Análise Glassdoor",
            tags$head(tags$style(HTML("
    .card { background: linear-gradient(135deg, #111827 0%, #0f1219 100%) !important; border-radius: 20px !important; border: 1px solid #2d3748 !important; box-shadow: 0 8px 32px rgba(0,0,0,0.2) !important; }
    .card-header { background: transparent !important; border-bottom: 1px solid #2d3748 !important; color: #e2e8f0 !important; font-weight: 600 !important; font-size: 16px !important; padding: 15px 20px !important; }
    .navbar .navbar-nav .nav-link { color: #4ade80 !important; font-size: 16px !important; font-weight: 600 !important; }
    .navbar .navbar-nav .nav-link:hover { color: #fbbf24 !important; }
    .navbar .navbar-nav .nav-link.active { color: #ffffff !important; border-bottom: 3px solid #4ade80 !important; }
    .navbar { background-color: #0f1219 !important; }
  "))),
            
            fluidRow(
              column(3, uiOutput("kpi_nps")),
              column(3, uiOutput("kpi_satisfacao")),
              column(3, uiOutput("kpi_lideranca")),
              column(3, uiOutput("kpi_ceo"))
            ),
            
            fluidRow(
              column(3, uiOutput("kpi_recomendacao")),
              column(3, uiOutput("kpi_negativas")),
              column(3, uiOutput("kpi_positivas")),
              column(3, uiOutput("kpi_total_avaliacoes"))
            ),
            
            fluidRow(
              column(12,
                     card(
                       card_header("Projeção da Satisfação Geral"),
                       plotlyOutput("projecao_satisfacao_plot", height = "350px")
                     )
              )
            ),
            
            fluidRow(
              column(6,
                     card(
                       card_header("NPS por Dimensão (e-NPS)"),
                       plotlyOutput("nps_dimensoes_plot", height = "450px")
                     )
              ),
              column(6,
                     card(
                       card_header("Evolução das Avaliações por Dimensão"),
                       plotlyOutput("comparacao_periodos_plot", height = "450px")
                     )
              )
            ),
            
            fluidRow(
              column(6,  
                     card(
                       card_header("Evolução das Dimensões: Últimos 6 Meses"),
                       plotlyOutput("historico_6_dimensoes_plot", height = "450px")
                     )
              ),
              column(6,
                     card(
                       card_header("Comparação: Funcionários Atuais vs Ex-funcionários"),
                       plotlyOutput("plot_comparacao_status", height = "450px")
                     )
              )
            ),
            
            fluidRow(
              column(6,
                     card(
                       card_header("NPS por Área"),
                       plotlyOutput("nps_cargo_plot", height = "500px")
                     )
              ),
              column(6,
                     card(
                       card_header("Perspectiva sobre a Empresa por Área"),
                       plotlyOutput("perspectiva_area_plot", height = "500px")
                     )
              )
            ),
            
            fluidRow(
              column(12,  
                     card(
                       card_header("Tabela de Detalhamento das Dimensões"),
                       DTOutput("dimensoes_status_table", height = "500px")
                     )
              )
            )
  ),
  
  # PÁGINA 2: BENCHMARK
  nav_panel("Benchmark",
            fluidRow(
              column(3, uiOutput("kpi_benchmark_geral")),
              column(3, uiOutput("kpi_benchmark_recomendacao")),
              column(3, uiOutput("kpi_benchmark_ceo")),
              column(3, uiOutput("kpi_benchmark_lideranca"))
            ),
            fluidRow(
              column(6,
                     card(
                       card_header("Radar - EMPRESA 1 vs Concorrentes"),
                       plotlyOutput("comp_radar_plot", height = "500px")
                     )
              ),
              column(6,
                     card(
                       card_header("Comparação por Aspecto"),
                       plotlyOutput("comp_bars_plot", height = "500px")
                     )
              )
            ),
            fluidRow(
              column(12,
                     card(
                       card_header("Tabela de Benchmark - Concorrentes"),
                       DTOutput("comp_data_table", height = "auto")
                     )
              )
            )
  ),
  
  # PÁGINA 3: VISÃO GERAL LINKEDIN
  nav_panel("Visão Geral LinkedIn",
            fluidRow(
              column(3, uiOutput("kpi_views_totais")),
              column(3, uiOutput("kpi_views_media")),
              column(3, uiOutput("kpi_unicos_totais")),
              column(3, uiOutput("kpi_seguidores_totais"))
            ),
            fluidRow(
              column(3, uiOutput("kpi_pc_share")),
              column(3, uiOutput("kpi_mobile_share")),
              column(3, uiOutput("kpi_views_growth")),
              column(3, uiOutput("kpi_followers_growth"))
            ),
            fluidRow(
              column(6, card(
                card_header("Evolução Temporal: Visualizações vs Visitantes Únicos"), 
                plotlyOutput("ln_views_vs_unicos_plot", height = "400px")
              )),
              column(6, card(
                card_header("Composição de Tráfego: Institucional vs Vagas (Média Diária)"), 
                plotlyOutput("ln_corp_vagas_plot", height = "400px")
              ))
            ),
            fluidRow(
              column(12, card(
                card_header("Comparativo: Visualizações vs Seguidores por Tamanho de Empresa"), 
                plotlyOutput("ln_tamanho_bar_plot", height = "400px")
              ))
            ),
            fluidRow(
              column(12, card(
                card_header("Tabela de Desempenho: Visitantes por Dispositivo (MoM)"), 
                DTOutput("ln_disp_table", height = "auto")
              ))
            )
  ),
  
  # PÁGINA 4: DEMOGRAFIA LINKEDIN
  nav_panel("Demografia LinkedIn",
            div(style = "background: #111827; border: 1px solid #2d3748; border-radius: 8px; padding: 15px; margin-bottom: 15px;",
                fluidRow(
                  column(6, selectInput("ln_demo_ano", "Ano de Referência:", choices = c("Todos" = "all"), width = "100%")),
                  column(6, selectInput("ln_demo_mes", "Mês de Referência:", choices = c("Todos" = "all"), width = "100%"))
                )
            ),
            fluidRow(
              column(12, card(
                div(style="display:flex; justify-content:space-between; align-items:center;", 
                    card_header("Nível de Experiência: Comparativo (Ano Atual vs Anterior)"),
                    radioGroupButtons("tgl_exp_bar", choices = c("Visitantes"="vis", "Seguidores"="seg"), size="sm", status="primary")),
                plotlyOutput("ln_demo_exp_yoy_plot", height = "400px")
              ))
            ),
            fluidRow(
              column(12, card(
                div(style="display:flex; justify-content:space-between; align-items:center;", 
                    card_header("Evolução: Nível de Experiência ao Longo do Tempo"),
                    radioGroupButtons("tgl_exp_line", choices = c("Visitantes"="vis", "Seguidores"="seg"), size="sm", status="primary")),
                plotlyOutput("ln_demo_exp_ts_plot", height = "400px")
              ))
            ),
            fluidRow(
              column(12, card(
                card_header("Evolução de Público por Tamanho da Empresa (Pequenas vs Grandes)"),
                plotlyOutput("ln_demo_tam_ts_plot", height = "400px")
              ))
            ),
            fluidRow(
              column(12, card(
                div(style="display:flex; justify-content:space-between; align-items:center;", 
                    card_header("Evolução Mensal: Top Funções"),
                    radioGroupButtons("tgl_tab_func", choices = c("Visitantes"="vis", "Seguidores"="seg"), size="sm", status="primary")),
                DTOutput("ln_demo_func_table", height = "auto")
              ))
            ),
            fluidRow(
            column(12, card(
              div(style="display:flex; justify-content:space-between; align-items:center;", 
                  card_header("Evolução Mensal: Tamanho da Empresa (Detalhado)"),
                  radioGroupButtons("tgl_tab_tam", choices = c("Visitantes"="vis", "Seguidores"="seg"), size="sm", status="primary")),
              DTOutput("ln_demo_tam_table", height = "auto")
              ))
            )
  ),
)


# SERVER - LÓGICA DO SERVIDOR

server <- function(input, output, session) {
  
  agg_funcoes <- function(df) {
    niveis_proibidos <- c("Treinamento", "Iniciante", "Sênior", "Gerente", "Diretor", 
                          "Vice-Presidente", "Proprietário", "Parceiro", "Não remunerado")
    
    df %>% 
      filter(!Funcao %in% niveis_proibidos) %>%
      group_by(Data_Ref, Funcao) %>% 
      summarise(Valor = sum(Valor, na.rm=TRUE), .groups='drop')
  }
  
  filtered_main <- reactive({
    df <- DATA$main
    
    if(is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(data.frame())
    
    if(!is.null(input$ano) && input$ano != "Todos") {
      df <- df %>% filter(Ano == input$ano)
    }
    
    if(input$status != "Todos") df <- df %>% filter(Status == input$status)
    
    if(input$sentimento != "Todos") df <- df %>% filter(Sentimento == input$sentimento)
    
    df
  })
  
  data_2026 <- reactive({
    df <- filtered_main()
    df %>% filter(Ano == 2026)
  })
  
  data_2025_same_period <- reactive({
    df <- filtered_main()
    periodo_2026 <- data_2026()
    if(nrow(periodo_2026) == 0) return(data.frame())
    meses_2026 <- unique(month(periodo_2026$Data_Fmt))
    df %>%
      filter(Ano == 2025, month(Data_Fmt) %in% meses_2026)
  })
  
  last_90_days <- reactive({
    df <- filtered_main()
    if(nrow(df) == 0) return(NULL)
    data_fim <- max(df$Data_Fmt, na.rm = TRUE)
    data_90_ini <- data_fim - days(90)
    data_180_ini <- data_fim - days(180)
    atual <- df %>% filter(Data_Fmt >= data_90_ini)
    anterior <- df %>% filter(Data_Fmt >= data_180_ini & Data_Fmt < data_90_ini)
    list(atual = atual, anterior = anterior, nome_atual = "Últimos 90 dias", nome_anterior = "90 dias anteriores")
  })
  
  last_6_months_data <- reactive({
    df <- filtered_main()
    if(nrow(df) == 0) return(data.frame())
    data_fim <- max(df$Data_Fmt, na.rm = TRUE)
    data_6_ini <- data_fim - months(6)
    df %>% filter(Data_Fmt >= data_6_ini)
  })
  
  filtrar_demografia <- function(categoria, tipo_publico) {
    if(tipo_publico == "vis") {
      df <- DATA$linkedin_visitantes[[categoria]]
      col_val <- "Total_Visualizacoes"
    } else {
      df <- DATA$linkedin_seguidores[[categoria]]
      col_val <- "Total_Seguidores"
    }
    
    if(is.null(df) || nrow(df) == 0) return(data.frame())
    
    df$Valor <- suppressWarnings(as.numeric(df[[col_val]]))
    
    if(isTruthy(input$ln_demo_ano) && input$ln_demo_ano != "all") {
      df <- df %>% filter(year(Data_Ref) == as.numeric(input$ln_demo_ano))
    }
    
    if(categoria == "localidades" && isTruthy(input$ln_demo_regiao) && input$ln_demo_regiao != "all") {
      if("Regiao" %in% names(df)) df <- df %>% filter(Regiao == input$ln_demo_regiao)
    }
    
    if(categoria == "setores" && isTruthy(input$ln_demo_setorgrupo) && input$ln_demo_setorgrupo != "all") {
      if("Setor_Grupo" %in% names(df)) df <- df %>% filter(Setor_Grupo == input$ln_demo_setorgrupo)
    }
    
    return(df)
  }
  
  # VISUAIS ANÁLISE GLASSDOOR 
  
  output$kpi_nps <- renderUI({
    atual <- data_2026()
    anterior <- data_2025_same_period()
    
    if(nrow(atual) == 0) {
      create_kpi_card("NPS Geral", 0, 0, 40, TRUE)
    } else {
      nps_atual <- calc_nps(atual$Satisfacao_geral)
      nps_anterior <- if(nrow(anterior) > 0) calc_nps(anterior$Satisfacao_geral) else 0
      create_kpi_card("NPS Geral", nps_atual, nps_anterior, 40, TRUE)
    }
  })
  
  output$kpi_satisfacao <- renderUI({
    atual <- data_2026()
    anterior <- data_2025_same_period()
    
    if(nrow(atual) == 0) {
      create_kpi_card("Satisfação Geral", 0, 0, 4.0, FALSE)
    } else {
      sat_atual <- mean(atual$Satisfacao_geral, na.rm = TRUE)
      sat_anterior <- if(nrow(anterior) > 0) mean(anterior$Satisfacao_geral, na.rm = TRUE) else 0
      create_kpi_card("Satisfação Geral", sat_atual, sat_anterior, 4.0, FALSE)
    }
  })
  
  output$kpi_lideranca <- renderUI({
    atual <- data_2026()
    anterior <- data_2025_same_period()
    
    if(nrow(atual) == 0) {
      create_kpi_card("Alta Liderança", 0, 0, 3.5, FALSE)
    } else {
      lid_atual <- mean(atual$Alta_lideranca, na.rm = TRUE)
      lid_anterior <- if(nrow(anterior) > 0) mean(anterior$Alta_lideranca, na.rm = TRUE) else 0
      create_kpi_card("Alta Liderança", lid_atual, lid_anterior, 3.5, FALSE)
    }
  })
  
  output$kpi_ceo <- renderUI({
    atual <- data_2026()
    anterior <- data_2025_same_period()
    
    calc_perspectiva <- function(dados) {
      if(is.null(dados) || nrow(dados) == 0) return(0)
      respostas_validas <- dados$Visao_mercado[dados$Visao_mercado %in% c("Melhorando", "Piorando", "Igual")]
      if(length(respostas_validas) == 0) return(0)
      return((sum(respostas_validas == "Melhorando") / length(respostas_validas)) * 100)
    }
    
    perspectiva_atual <- calc_perspectiva(atual)
    perspectiva_anterior <- calc_perspectiva(anterior)
    
    create_kpi_card("Perspectiva Positiva da Empresa", perspectiva_atual, perspectiva_anterior, 80, is_percent = TRUE)
  })
  
  output$kpi_recomendacao <- renderUI({
    atual <- data_2026()
    anterior <- data_2025_same_period()
    
    if(nrow(atual) == 0) {
      create_kpi_card("Recomendação", 0, 0, 70, TRUE)
    } else {
      rec_atual <- mean(atual$Recomendacao_bin, na.rm = TRUE) * 100
      rec_anterior <- if(nrow(anterior) > 0) mean(anterior$Recomendacao_bin, na.rm = TRUE) * 100 else 0
      create_kpi_card("Recomendação", rec_atual, rec_anterior, 70, TRUE)
    }
  })
  
  output$kpi_negativas <- renderUI({
    atual <- data_2026()
    anterior <- data_2025_same_period()
    
    if(nrow(atual) == 0) {
      create_kpi_card("Avaliações Negativas", 0, 0, 25, TRUE, invert = TRUE)
    } else {
      neg_atual <- (sum(atual$Satisfacao_geral <= 2, na.rm = TRUE) / nrow(atual)) * 100
      neg_anterior <- if(nrow(anterior) > 0) (sum(anterior$Satisfacao_geral <= 2, na.rm = TRUE) / nrow(anterior)) * 100 else 0
      create_kpi_card("Avaliações Negativas", neg_atual, neg_anterior, 25, TRUE, invert = TRUE)
    }
  })
  
  output$kpi_positivas <- renderUI({
    atual <- data_2026()
    anterior <- data_2025_same_period()
    
    if(nrow(atual) == 0) {
      create_kpi_card("Avaliações Positivas", 0, 0, 60, TRUE)
    } else {
      pos_atual <- (sum(atual$Satisfacao_geral >= 4, na.rm = TRUE) / nrow(atual)) * 100
      pos_anterior <- if(nrow(anterior) > 0) (sum(anterior$Satisfacao_geral >= 4, na.rm = TRUE) / nrow(anterior)) * 100 else 0
      create_kpi_card("Avaliações Positivas", pos_atual, pos_anterior, 60, TRUE)
    }
  })
  
  output$kpi_total_avaliacoes <- renderUI({
    atual <- data_2026()
    anterior <- data_2025_same_period()
    
    total_atual <- nrow(atual)
    total_anterior <- if(nrow(anterior) > 0) nrow(anterior) else 0
    meta <- 40
    
    create_kpi_card("Total Avaliações", total_atual, total_anterior, meta, is_percent = FALSE, invert = FALSE, is_volume = TRUE)
  })
  
  output$projecao_satisfacao_plot <- renderPlotly({
    df <- filtered_main()
    if(nrow(df) < 6) return(plot_ly() %>% layout(title = "Dados insuficientes", plot_bgcolor = "#111827", paper_bgcolor = "#111827", xaxis = list(color = "#e2e8f0"), yaxis = list(color = "#e2e8f0")))
    
    df_ts <- df %>% group_by(Mes) %>% summarise(Satisfacao = mean(Satisfacao_geral, na.rm = TRUE), .groups = 'drop') %>% filter(!is.na(Mes)) %>% arrange(Mes)
    if(nrow(df_ts) < 4) return(plot_ly() %>% layout(title = "Dados insuficientes", plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
    
    ts_sat <- ts(df_ts$Satisfacao, frequency = min(12, max(1, nrow(df_ts) - 1)))
    
    tryCatch({
      fit <- auto.arima(ts_sat, seasonal = FALSE, approximation = TRUE, stepwise = TRUE)
      fc <- forecast(fit, h = 6)
      
      historico <- data.frame(Periodo = df_ts$Mes, Valor = as.numeric(ts_sat), Tipo = "Histórico")
      proj <- data.frame(Periodo = seq(max(df_ts$Mes), by = "month", length.out = 7)[-1], Valor = as.numeric(fc$mean), Lower = as.numeric(fc$lower[, 2]), Upper = as.numeric(fc$upper[, 2]), Tipo = "Projeção")
      
      plot_ly() %>% 
        add_trace(x = ~historico$Periodo, y = ~historico$Valor, name = "Histórico", type = "scatter", mode = "lines+markers", line = list(color = "#4ade80", width = 2), marker = list(color = "#4ade80", size = 6)) %>%
        add_trace(x = ~proj$Periodo, y = ~proj$Valor, name = "Projeção", type = "scatter", mode = "lines+markers", line = list(color = "#fbbf24", width = 2, dash = "dash"), marker = list(color = "#fbbf24", size = 6)) %>%
        add_ribbons(x = ~proj$Periodo, ymin = ~proj$Lower, ymax = ~proj$Upper, name = "IC 95%", fillcolor = "rgba(251, 191, 36, 0.2)", line = list(color = "transparent")) %>%
        add_lines(x = ~c(historico$Periodo[1], proj$Periodo[nrow(proj)]), y = 4, name = "Meta (4.0)", line = list(color = "#ef4444", width = 2, dash = "dash")) %>%
        layout(xaxis = list(title = "", tickfont = list(color = "#e2e8f0"), gridcolor = "#374151"), yaxis = list(title = "Satisfação Média", range = c(1, 5), tickfont = list(color = "#e2e8f0"), titlefont = list(color = "#e2e8f0"), gridcolor = "#374151"), plot_bgcolor = "#111827", paper_bgcolor = "#111827", hovermode = "x unified", legend = list(orientation = "h", yanchor = "bottom", y = 1.02, font = list(color = "#e2e8f0")))
    }, error = function(e) plot_ly() %>% layout(title = "Erro na projeção", plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
  })
  
  output$nps_dimensoes_plot <- renderPlotly({
    df <- data_2026()
    if(nrow(df) == 0) return(plot_ly() %>% layout(title = "Sem dados", plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
    
    dimensoes <- c("Satisfacao_geral", "Oportunidades_carreira", "Remuneracao_beneficios", "Alta_lideranca", "Qualidade_vida", "Cultura_valores", "Diversidade_inclusao")
    nomes <- c("Satisfação", "Oportunidades", "Remuneração", "Liderança", "Qualidade Vida", "Cultura", "Diversidade")
    nps_vals <- sapply(dimensoes, function(d) calc_nps(df[[d]]))
    df_nps <- data.frame(Dimensao = nomes, NPS = nps_vals)
    cores_barra <- ifelse(df_nps$NPS >= 50, "#22c55e", ifelse(df_nps$NPS >= 0, "#4ade80", ifelse(df_nps$NPS >= -50, "#f97316", "#ef4444")))
    
    plot_ly(df_nps, x = ~reorder(Dimensao, NPS), y = ~NPS, type = "bar", marker = list(color = cores_barra, line = list(color = "#1f2937", width = 1)), text = ~paste0(round(NPS, 1), "%"), textposition = "outside", textfont = list(color = "#e2e8f0")) %>% 
      layout(xaxis = list(title = "", tickfont = list(color = "#e2e8f0"), gridcolor = "#374151"), yaxis = list(title = "NPS (%)", range = c(-100, 100), tickfont = list(color = "#e2e8f0"), titlefont = list(color = "#e2e8f0"), gridcolor = "#374151"), plot_bgcolor = "#111827", paper_bgcolor = "#111827", showlegend = FALSE)
  })
  
  output$comparacao_periodos_plot <- renderPlotly({
    comp <- last_90_days()
    if(is.null(comp) || nrow(comp$atual) == 0) return(plot_ly() %>% layout(title = "Dados insuficientes", plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
    
    dimensoes <- c("Satisfacao_geral", "Oportunidades_carreira", "Remuneracao_beneficios", "Alta_lideranca", "Qualidade_vida", "Cultura_valores", "Diversidade_inclusao")
    nomes <- c("Satisfação", "Oportunidades", "Remuneração", "Liderança", "Qualidade Vida", "Cultura", "Diversidade")
    atual_vals <- sapply(dimensoes, function(d) mean(comp$atual[[d]], na.rm = TRUE))
    anterior_vals <- sapply(dimensoes, function(d) mean(comp$anterior[[d]], na.rm = TRUE))
    
    plot_ly() %>% 
      add_trace(x = nomes, y = atual_vals, name = comp$nome_atual, type = "bar", marker = list(color = "#22c55e", line = list(color = "#1f2937", width = 1)), text = round(atual_vals, 2), textposition = "outside", textfont = list(color = "#e2e8f0")) %>%
      add_trace(x = nomes, y = anterior_vals, name = comp$nome_anterior, type = "bar", marker = list(color = "#64748b", line = list(color = "#1f2937", width = 1)), text = round(anterior_vals, 2), textposition = "outside", textfont = list(color = "#e2e8f0")) %>%
      layout(xaxis = list(title = "", tickangle = -45, tickfont = list(color = "#e2e8f0"), gridcolor = "#374151"), yaxis = list(title = "Nota Média", range = c(1, 5), tickfont = list(color = "#e2e8f0"), titlefont = list(color = "#e2e8f0"), gridcolor = "#374151"), plot_bgcolor = "#111827", paper_bgcolor = "#111827", barmode = "group", legend = list(orientation = "h", yanchor = "bottom", y = 1.02, font = list(color = "#e2e8f0")))
  })
  
  output$historico_6_dimensoes_plot <- renderPlotly({
    df <- last_6_months_data()
    if(nrow(df) == 0) return(plot_ly() %>% layout(title = "Sem dados", plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
    
    df_agg <- df %>% mutate(Mes_ref = floor_date(Data_Fmt, "month")) %>%
      group_by(Mes_ref) %>% summarise(
        Satisfacao = mean(Satisfacao_geral, na.rm = TRUE),
        Oportunidades = mean(Oportunidades_carreira, na.rm = TRUE),
        Remuneracao = mean(Remuneracao_beneficios, na.rm = TRUE),
        Lideranca = mean(Alta_lideranca, na.rm = TRUE),
        Qualidade_Vida = mean(Qualidade_vida, na.rm = TRUE),
        Cultura = mean(Cultura_valores, na.rm = TRUE),
        Diversidade = mean(Diversidade_inclusao, na.rm = TRUE),
        .groups = 'drop') %>% arrange(Mes_ref)
    
    df_agg$Mes_Label <- factor(format(df_agg$Mes_ref, "%b/%Y"), levels = format(df_agg$Mes_ref, "%b/%Y"))
    
    plot_ly() %>% 
      add_trace(x = ~df_agg$Mes_Label, y = ~df_agg$Satisfacao, name = "Satisfação", type = "scatter", mode = "lines+markers", line = list(color = "#4ade80", width = 2, shape = "spline"), marker = list(size = 6)) %>%
      add_trace(x = ~df_agg$Mes_Label, y = ~df_agg$Oportunidades, name = "Oportunidades", type = "scatter", mode = "lines+markers", line = list(color = "#3b82f6", width = 2, shape = "spline"), marker = list(size = 6)) %>%
      add_trace(x = ~df_agg$Mes_Label, y = ~df_agg$Remuneracao, name = "Remuneração", type = "scatter", mode = "lines+markers", line = list(color = "#f59e0b", width = 2, shape = "spline"), marker = list(size = 6)) %>%
      add_trace(x = ~df_agg$Mes_Label, y = ~df_agg$Lideranca, name = "Liderança", type = "scatter", mode = "lines+markers", line = list(color = "#ec4899", width = 2, shape = "spline"), marker = list(size = 6)) %>%
      add_trace(x = ~df_agg$Mes_Label, y = ~df_agg$Qualidade_Vida, name = "Qualidade Vida", type = "scatter", mode = "lines+markers", line = list(color = "#06b6d4", width = 2, shape = "spline"), marker = list(size = 6)) %>%
      add_trace(x = ~df_agg$Mes_Label, y = ~df_agg$Cultura, name = "Cultura", type = "scatter", mode = "lines+markers", line = list(color = "#8b5cf6", width = 2, shape = "spline"), marker = list(size = 6)) %>%
      add_trace(x = ~df_agg$Mes_Label, y = ~df_agg$Diversidade, name = "Diversidade", type = "scatter", mode = "lines+markers", line = list(color = "#f97316", width = 2, shape = "spline"), marker = list(size = 6)) %>%
      add_lines(x = ~df_agg$Mes_Label, y = 4, name = "Meta (4.0)", line = list(color = "#ffffff", width = 2, dash = "dash")) %>%
      layout(xaxis = list(title = "", tickangle = -45, tickfont = list(color = "#e2e8f0"), gridcolor = "#374151"), yaxis = list(title = "Nota Média", range = c(0, 5), tickfont = list(color = "#e2e8f0"), titlefont = list(color = "#e2e8f0"), gridcolor = "#374151"), plot_bgcolor = "#111827", paper_bgcolor = "#111827", hovermode = "x unified", legend = list(orientation = "h", yanchor = "bottom", y = 1.02, font = list(color = "#e2e8f0"), itemclick = "toggle", itemdoubleclick = "toggleothers"))
  })
  
  output$nps_cargo_plot <- renderPlotly({
    df <- filtered_main()
    
    if(nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "Sem dados disponíveis", plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
    }
    
    df_plot <- df %>%
      filter(!is.na(Cargo_Area), Cargo_Area != "Não Informado", Cargo_Nivel != "Outros", Cargo_Area != "Outras Áreas") %>%
      group_by(Cargo = Cargo_Area) %>%
      summarise(
        NPS = calc_nps(Satisfacao_geral),
        Qtd = n(),
        Satisfacao_Media = round(mean(Satisfacao_geral, na.rm = TRUE), 2),
        .groups = 'drop'
      ) %>%
      filter(!is.na(NPS), Qtd >= 3) %>%
      arrange(desc(NPS))
    
    if(nrow(df_plot) == 0) {
      return(plot_ly() %>% layout(title = "Dados insuficientes para NPS por cargo", plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
    }
    
    df_plot$cor <- ifelse(df_plot$NPS >= 70, "#22c55e",
                          ifelse(df_plot$NPS >= 50, "#fbbf24",
                                 ifelse(df_plot$NPS >= 0, "#f97316", "#ef4444")))
    
    df_plot$hover_text <- paste0(
      "<b>Cargo:</b> ", df_plot$Cargo, "<br>",
      "<b>NPS:</b> ", round(df_plot$NPS, 1), "%<br>",
      "<b>Satisfação Média:</b> ", df_plot$Satisfacao_Media, "<br>",
      "<b>Avaliações:</b> ", df_plot$Qtd
    )
    
    plot_ly(df_plot, 
            x = ~NPS, 
            y = ~reorder(Cargo, NPS), 
            type = "bar", 
            orientation = "h",
            marker = list(color = ~cor, line = list(color = "#1f2937", width = 1)),
            text = ~paste0(round(NPS, 1), "%"),
            textposition = "outside",
            textfont = list(color = "#e2e8f0", size = 11),
            hoverinfo = "text",
            hovertext = ~hover_text) %>%
      layout(
        xaxis = list(title = "NPS (%)", range = c(-100, 100), tickfont = list(color = "#e2e8f0", size = 11), titlefont = list(color = "#e2e8f0", size = 12), gridcolor = "#374151", ticksuffix = "%", zeroline = TRUE, zerolinecolor = "#fbbf24", zerolinewidth = 1.5),
        yaxis = list(title = "", tickfont = list(color = "#e2e8f0", size = 11), gridcolor = "#374151"),
        plot_bgcolor = "#111827",
        paper_bgcolor = "#111827",
        showlegend = FALSE,
        margin = list(l = 150, r = 80, t = 40, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$plot_comparacao_status <- renderPlotly({
    df <- filtered_main()
    
    if(nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "Sem dados disponíveis", plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
    }
    
    ultima_data <- max(df$Data_Fmt, na.rm = TRUE)
    data_corte_90 <- ultima_data - days(90)
    data_corte_180 <- ultima_data - days(180)
    
    ultimos_90 <- df %>% filter(Data_Fmt >= data_corte_90)
    anteriores_90 <- df %>% filter(Data_Fmt >= data_corte_180 & Data_Fmt < data_corte_90)
    
    media_atual_current <- mean(ultimos_90$Satisfacao_geral[ultimos_90$Status == "Atual"], na.rm = TRUE)
    media_atual_past <- mean(ultimos_90$Satisfacao_geral[ultimos_90$Status == "Ex-funcionário"], na.rm = TRUE)
    media_anterior_current <- mean(anteriores_90$Satisfacao_geral[anteriores_90$Status == "Atual"], na.rm = TRUE)
    media_anterior_past <- mean(anteriores_90$Satisfacao_geral[anteriores_90$Status == "Ex-funcionário"], na.rm = TRUE)
    
    df_plot <- data.frame(
      Periodo = c(rep("Últimos 90 dias", 2), rep("90 dias anteriores", 2)),
      Status = rep(c("Atuais", "Ex-funcionários"), 2),
      Nota = c(media_atual_current, media_atual_past, media_anterior_current, media_anterior_past)
    )
    
    df_plot <- df_plot %>% filter(!is.na(Nota))
    
    if(nrow(df_plot) == 0) {
      return(plot_ly() %>% layout(title = "Dados insuficientes para comparação", plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
    }
    
    plot_ly(df_plot, 
            x = ~Periodo, 
            y = ~Nota, 
            color = ~Status,
            type = "bar",
            colors = c("Atuais" = "#22c55e", "Ex-funcionários" = "#64748b"),
            text = ~round(Nota, 2),
            textposition = "outside",
            textfont = list(color = "#e2e8f0", size = 11),
            hovertemplate = "Período: %{x}<br>Status: %{color}<br>Nota Média: %{y:.2f}<extra></extra>") %>%
      layout(
        xaxis = list(title = "", tickfont = list(color = "#e2e8f0"), gridcolor = "#374151"),
        yaxis = list(title = "Nota Média", range = c(1, 5), tickfont = list(color = "#e2e8f0"), titlefont = list(color = "#e2e8f0"), gridcolor = "#374151"),
        plot_bgcolor = "#111827",
        paper_bgcolor = "#111827",
        barmode = "group",
        bargap = 0.3,
        legend = list(orientation = "h", yanchor = "bottom", y = 1.02, xanchor = "center", x = 0.5, font = list(color = "#e2e8f0", size = 11)),
        margin = list(t = 60, b = 40, l = 60, r = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$perspectiva_area_plot <- renderPlotly({
    df <- filtered_main()
    
    if(nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "Sem dados disponíveis", plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
    }
    
    df_plot <- df %>%
      filter(
        !is.na(Visao_mercado),
        Visao_mercado %in% c("Melhorando", "Igual", "Piorando"),
        !is.na(Cargo_Area), 
        Cargo_Area != "Não Informado", 
        Cargo_Nivel != "Outros", 
        Cargo_Area != "Outras Áreas"
      ) %>%
      group_by(Cargo = Cargo_Area, Visao = Visao_mercado) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      group_by(Cargo) %>%
      mutate(Total = sum(Count), Percentual = Count / Total * 100) %>%
      filter(Total >= 3)
    
    if(nrow(df_plot) == 0) {
      return(plot_ly() %>% layout(title = "Dados insuficientes", plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
    }
    
    ordem_cargos <- df_plot %>%
      filter(Visao == "Melhorando") %>%
      arrange(Percentual) %>%
      pull(Cargo)
    
    todos_cargos <- unique(df_plot$Cargo)
    faltantes <- setdiff(todos_cargos, ordem_cargos)
    ordem_final <- c(faltantes, ordem_cargos)
    
    df_plot$Cargo <- factor(df_plot$Cargo, levels = ordem_final)
    df_plot$Visao <- factor(df_plot$Visao, levels = c("Piorando", "Igual", "Melhorando"))
    
    cores <- c("Melhorando" = "#22c55e", "Igual" = "#fbbf24", "Piorando" = "#ef4444")
    
    plot_ly(df_plot,
            x = ~Percentual,
            y = ~Cargo,
            color = ~Visao,
            colors = cores,
            type = "bar",
            orientation = "h",
            text = ~ifelse(Percentual >= 8, paste0(round(Percentual, 1), "%"), ""),
            textposition = "inside",
            insidetextanchor = "middle",
            textfont = list(color = "#111827", size = 11, weight = "bold"),
            hoverinfo = "text",
            hovertext = ~paste0("<b>Área:</b> ", Cargo, "<br><b>Visão:</b> ", Visao, "<br><b>%:</b> ", round(Percentual, 1), "% (", Count, " avaliações)")) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Distribuição (%)", tickfont = list(color = "#e2e8f0", size = 11), titlefont = list(color = "#e2e8f0", size = 12), gridcolor = "#374151", ticksuffix = "%", range = c(0, 100)),
        yaxis = list(title = "", tickfont = list(color = "#e2e8f0", size = 11), gridcolor = "#374151"),
        plot_bgcolor = "#111827",
        paper_bgcolor = "#111827",
        legend = list(orientation = "h", yanchor = "bottom", y = 1.02, xanchor = "center", x = 0.5, font = list(color = "#e2e8f0", size = 11)),
        margin = list(l = 150, r = 30, t = 40, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$dimensoes_status_table <- renderDT({
    df <- data_2026()
    if(nrow(df) == 0) return(datatable(data.frame(Mensagem = "Sem dados"), options = list(dom = 't'), class = "table-dark"))
    
    dimensoes <- c("Satisfacao_geral", "Oportunidades_carreira", "Remuneracao_beneficios", "Alta_lideranca", "Qualidade_vida", "Cultura_valores", "Diversidade_inclusao")
    nomes <- c("Satisfação Geral", "Oportunidades de Carreira", "Remuneração e Benefícios", "Alta Liderança", "Qualidade de Vida", "Cultura e Valores", "Diversidade e Inclusão")
    metas <- c(4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0)
    
    atual_vals <- sapply(dimensoes, function(d) mean(df[[d]], na.rm = TRUE))
    nps_vals <- sapply(dimensoes, function(d) calc_nps(df[[d]]))
    
    desempenho <- (atual_vals / metas) * 100
    
    nps_class <- ifelse(nps_vals >= 75, "Excelente", 
                        ifelse(nps_vals >= 50, "Bom",
                               ifelse(nps_vals >= 25, "Regular",
                                      ifelse(nps_vals >= 0, "Ruim", "Crítico"))))
    
    df_status <- data.frame(
      Dimensão = nomes,
      `Média 2026` = atual_vals,
      Meta = metas,
      `% Meta` = desempenho,
      Status = nps_class,
      NPS = nps_vals,
      check.names = FALSE
    )
    
    datatable(
      df_status,
      options = list(
        paging = FALSE,
        dom = 'Brt',
        scrollX = TRUE,
        buttons = list(
          list(extend = 'copy', text = 'Copiar', className = 'btn-dark'),
          list(extend = 'csv', text = 'CSV', title = 'dimensoes_2026', className = 'btn-dark'),
          list(extend = 'excel', text = 'Excel', title = 'dimensoes_2026', className = 'btn-dark')
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({",
          "    'background-color': '#0a0c10',",
          "    'color': '#94a3b8',",
          "    'font-weight': '600',",
          "    'font-size': '12px',",
          "    'text-transform': 'uppercase',",
          "    'letter-spacing': '0.5px',",
          "    'border-bottom': '2px solid #334155'",
          "  });",
          "}"
        ),
        columnDefs = list(
          list(className = 'dt-center', targets = c(1, 2, 3, 4, 5)),
          list(className = 'dt-left', targets = 0)
        )
      ),
      class = "table-dark table-hover compact",
      rownames = FALSE,
      escape = FALSE
    ) %>%
      formatStyle('Dimensão', fontWeight = 'bold', color = '#e2e8f0', borderRight = '1px solid #334155') %>%
      formatStyle('Média 2026', fontWeight = 'bold', color = styleInterval(c(3.0, 3.8), c('#f87171', '#fbbf24', '#4ade80'))) %>%
      formatStyle('% Meta', background = styleColorBar(c(0, 120), "rgba(59, 130, 246, 0.25)"), backgroundSize = "90% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center", color = '#e2e8f0') %>%
      formatStyle('Status', color = styleEqual(c("Excelente", "Bom", "Regular", "Ruim", "Crítico"), c("#4ade80", "#10b981", "#fbbf24", "#f97316", "#ef4444")), fontWeight = "bold") %>%
      formatStyle('NPS', fontWeight = 'bold', color = '#e2e8f0', background = JS("value < 0 ? 'linear-gradient(90deg, transparent ' + (100 + value)/2 + '%, rgba(248, 113, 113, 0.3) ' + (100 + value)/2 + '%, rgba(248, 113, 113, 0.3) 50%, transparent 50%)' : 'linear-gradient(90deg, transparent 50%, rgba(74, 222, 128, 0.3) 50%, rgba(74, 222, 128, 0.3) ' + (50 + value/2) + '%, transparent ' + (50 + value/2) + '%)'"), backgroundSize = "100% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center") %>%
      formatRound(c('Média 2026', 'Meta'), digits = 2, dec.mark = ",") %>%
      formatPercentage('% Meta', 0) %>%
      formatString('NPS', suffix = "%")
  })
  

  # BENCHMARK
  
  output$kpi_benchmark_geral <- renderUI({
    df_atual <- data_2026()
    nota_empresa1 <- if(nrow(df_atual) > 0) mean(df_atual$Satisfacao_geral, na.rm = TRUE) else 0
    
    nota_mercado <- if(nrow(DATA$comp) > 0) {
      DATA$comp %>% filter(Aspecto == "Classificação geral") %>% pull(Media_Glassdoor)
    } else 0
    if(length(nota_mercado) == 0) nota_mercado <- 0
    
    create_kpi_card("Nota Geral (EMPRESA 1 vs Mercado)", nota_empresa1, nota_mercado, 4.0, FALSE, label_anterior = "Média Mercado: ")
  })
  
  output$kpi_benchmark_recomendacao <- renderUI({
    df_atual <- data_2026()
    rec_empresa1 <- if(nrow(df_atual) > 0) mean(df_atual$Recomendacao_bin, na.rm = TRUE) * 100 else 0
    
    rec_mercado <- if(nrow(DATA$comp) > 0) {
      DATA$comp %>% filter(Aspecto == "Recommend To A Friend") %>% pull(Media_Glassdoor)
    } else 0
    if(length(rec_mercado) == 0) rec_mercado <- 0
    
    create_kpi_card("Recomendação (EMPRESA 1 vs Mercado)", rec_empresa1, rec_mercado, 70, TRUE, label_anterior = "Média Mercado: ")
  })
  
  output$kpi_benchmark_ceo <- renderUI({
    df_atual <- data_2026()
    
    calc_perspectiva <- function(dados) {
      if(is.null(dados) || nrow(dados) == 0) return(0)
      respostas_validas <- dados$Visao_mercado[dados$Visao_mercado %in% c("Melhorando", "Piorando", "Igual")]
      if(length(respostas_validas) == 0) return(0)
      return((sum(respostas_validas == "Melhorando") / length(respostas_validas)) * 100)
    }
    
    persp_empresa1 <- calc_perspectiva(df_atual)
    
    persp_mercado <- if(nrow(DATA$comp) > 0) {
      DATA$comp %>% filter(grepl("Perspectiva positiva", Aspecto, ignore.case = TRUE)) %>% pull(Media_Glassdoor)
    } else 0
    
    if(length(persp_mercado) == 0) persp_mercado <- 0
    
    create_kpi_card("Perspectiva Positiva da Empresa", persp_empresa1, persp_mercado, 80, is_percent = TRUE, label_anterior = "Média Mercado: ")
  })
  
  output$kpi_benchmark_lideranca <- renderUI({
    df_atual <- data_2026()
    lid_empresa1 <- if(nrow(df_atual) > 0) mean(df_atual$Alta_lideranca, na.rm = TRUE) else 0
    
    lid_mercado <- if(nrow(DATA$comp) > 0) {
      DATA$comp %>% filter(grepl("Liderança", Aspecto, ignore.case = TRUE)) %>% pull(Media_Glassdoor)
    } else 0
    if(length(lid_mercado) == 0) lid_mercado <- 0
    
    create_kpi_card("Alta Liderança (EMPRESA 1 vs Mercado)", lid_empresa1, lid_mercado, 3.5, FALSE, label_anterior = "Média Mercado: ")
  })
  
  output$comp_radar_plot <- renderPlotly({
    if(nrow(DATA$comp) == 0) {
      return(plot_ly() %>% layout(title = list(text = "Sem dados de concorrentes", font = list(color = "#e2e8f0")), plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
    }
    
    aspectos_radar <- c("Classificação geral", "Oportunidades de carreira", "Compensation And Benefits", 
                        "Cultura e valores", "Alta Liderança", "Qualidade de vida")
    
    df_radar <- DATA$comp %>% filter(Aspecto %in% aspectos_radar)
    
    if(nrow(df_radar) == 0) {
      return(plot_ly() %>% layout(title = list(text = "Dados insuficientes para radar", font = list(color = "#e2e8f0")), plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
    }
    
    df_radar <- rbind(df_radar, df_radar[1, ])
    
    plot_ly(type = 'scatterpolar', mode = "lines+markers") %>%
      add_trace(r = df_radar$Empresa1, theta = df_radar$Aspecto, customdata = df_radar$Media_Glassdoor, name = "EMPRESA 1", line = list(color = "#4ade80", width = 3), fill = 'toself', fillcolor = "rgba(74, 222, 128, 0.25)", marker = list(color = "#4ade80", size = 8, symbol = "circle", line = list(color = "#0f1219", width = 1.5)), hovertemplate = paste0("<b>%{theta}</b><br><br><span style='color:#4ade80'>EMPRESA 1: <b>%{r:.1f}</b></span><br><span style='color:#fbbf24'>Média Mercado: %{customdata:.1f}</span><extra></extra>"), hoverlabel = list(bgcolor = "#1f2937", bordercolor = "#4ade80", font = list(size = 13, color = "#e2e8f0"))) %>%
      add_trace(r = df_radar$Media_Glassdoor, theta = df_radar$Aspecto, customdata = df_radar$Empresa1, name = "Média Glassdoor", line = list(color = "#fbbf24", width = 3), fill = 'toself', fillcolor = "rgba(251, 191, 36, 0.15)", marker = list(color = "#fbbf24", size = 8, symbol = "square", line = list(color = "#0f1219", width = 1.5)), hovertemplate = paste0("<b>%{theta}</b><br><br><span style='color:#fbbf24'>Média Mercado: <b>%{r:.1f}</b></span><br><span style='color:#4ade80'>EMPRESA 1: %{customdata:.1f}</span><extra></extra>"), hoverlabel = list(bgcolor = "#1f2937", bordercolor = "#fbbf24", font = list(size = 13, color = "#e2e8f0"))) %>%
      layout(polar = list(bgcolor = "#0a0c10", radialaxis = list(visible = TRUE, range = c(0, 5), tickfont = list(color = "#64748b", size = 11), gridcolor = "#334155", dtick = 1), angularaxis = list(tickfont = list(color = "#e2e8f0", size = 12, weight = "bold"), gridcolor = "#334155", linecolor = "#334155")), plot_bgcolor = "#111827", paper_bgcolor = "#111827", legend = list(orientation = "h", yanchor = "bottom", y = 1.08, xanchor = "center", x = 0.5, font = list(color = "#e2e8f0", size = 12)), margin = list(t = 60, b = 40, l = 40, r = 40)) %>%
      config(displayModeBar = FALSE)
  })
  
  output$comp_bars_plot <- renderPlotly({
    if(nrow(DATA$comp) == 0) {
      return(plot_ly() %>% layout(title = list(text = "Sem dados disponíveis", font = list(color = "#e2e8f0")), plot_bgcolor = "#111827", paper_bgcolor = "#111827"))
    }
    
    empresas_todas <- names(DATA$comp)[names(DATA$comp) != "Aspecto"]
    empresas_reais <- empresas_todas[empresas_todas != "Media_Glassdoor"]
    
    aspectos <- c("Classificação geral", "Oportunidades de carreira", "Compensation And Benefits", 
                  "Cultura e valores", "Alta Liderança", "Qualidade de vida")
    
    aspectos_existentes <- intersect(aspectos, DATA$comp$Aspecto)
    if(length(aspectos_existentes) == 0) {
      aspectos_existentes <- head(DATA$comp$Aspecto, 6)
    }
    
    df_long <- DATA$comp %>% 
      filter(Aspecto %in% aspectos_existentes) %>% 
      pivot_longer(cols = all_of(empresas_reais), names_to = "Empresa", values_to = "Nota") %>%
      filter(!is.na(Nota))
    
    df_media <- DATA$comp %>%
      filter(Aspecto %in% aspectos_existentes) %>%
      select(Aspecto, Media_Glassdoor)
    
    df_long$Aspecto <- factor(df_long$Aspecto, levels = aspectos_existentes)
    df_media$Aspecto <- factor(df_media$Aspecto, levels = aspectos_existentes)
    
    cores_disponiveis <- c("#4ade80", "#3b82f6", "#ec4899", "#06b6d4", "#8b5cf6", "#f97316", "#ef4444", "#10b981", "#6366f1")
    paleta_cores <- head(cores_disponiveis, length(empresas_reais))
    names(paleta_cores) <- empresas_reais
    
    plot_ly() %>%
      add_trace(data = df_long, x = ~Aspecto, y = ~Nota, color = ~Empresa, type = "bar", colors = paleta_cores, text = ~round(Nota, 1), textposition = "outside", textfont = list(color = "#e2e8f0", size = 11, weight = "bold"), marker = list(line = list(color = "#0a0c10", width = 1)), hovertemplate = paste0("<b>%{x}</b><br><span style='font-size:13px;'>%{data.name}: <b>%{y:.1f}</b></span><extra></extra>"), hoverlabel = list(bgcolor = "#1f2937", font = list(size = 13, color = "#e2e8f0"))) %>%
      add_trace(data = df_media, x = ~Aspecto, y = ~Media_Glassdoor, type = "scatter", mode = "lines+markers", name = "Média Mercado", line = list(color = "#ffffff", width = 2, dash = "dash"), marker = list(color = "#ffffff", size = 9, symbol = "diamond", line = list(color = "#0a0c10", width = 1)), hovertemplate = paste0("<b>%{x}</b><br><span style='font-size:13px; color:#ffffff;'>Média Mercado: <b>%{y:.1f}</b></span><extra></extra>"), hoverlabel = list(bgcolor = "#1f2937", bordercolor = "#ffffff", font = list(size = 13, color = "#ffffff"))) %>%
      layout(xaxis = list(title = "", tickangle = -30, tickfont = list(color = "#e2e8f0", size = 13, weight = "bold"), gridcolor = "transparent"), yaxis = list(title = "Nota Média", range = c(0, 5.2), tickfont = list(color = "#94a3b8", size = 11), titlefont = list(color = "#e2e8f0", size = 12), gridcolor = "#334155", zeroline = TRUE, zerolinecolor = "#334155"), plot_bgcolor = "#111827", paper_bgcolor = "#111827", barmode = "group", bargap = 0.15, bargroupgap = 0.05, legend = list(orientation = "h", yanchor = "bottom", y = 1.05, xanchor = "center", x = 0.5, font = list(color = "#e2e8f0", size = 12)), margin = list(t = 20, b = 80, l = 40, r = 20)) %>%
      config(displayModeBar = FALSE)
  })
  
  output$comp_data_table <- renderDT({
    if(nrow(DATA$comp) == 0) {
      return(datatable(data.frame(Mensagem = "Sem dados disponíveis"), options = list(dom = 't'), class = "table-dark"))
    }
    
    df_table <- DATA$comp
    names(df_table)[names(df_table) == "Media_Glassdoor"] <- "Média Glassdoor (mercado)"
    media <- "Média Glassdoor (mercado)"
    todas_empresas <- names(df_table)[names(df_table) != "Aspecto"]
    concorrentes <- setdiff(todas_empresas, media)
    alvo <- "EMPRESA 1"
    
    for(emp in concorrentes) {
      df_table[[paste0("diff_", emp)]] <- df_table[[emp]] - df_table[[media]]
    }
    
    colunas_originais <- ncol(DATA$comp)
    colunas_totais <- ncol(df_table)
    idx_ocultas <- colunas_originais:(colunas_totais - 1)
    
    dt <- datatable(df_table, 
                    options = list(
                      paging = FALSE,
                      info = FALSE,
                      scrollX = TRUE,
                      dom = 'Brt',
                      buttons = list(
                        list(extend = 'copy', text = 'Copiar', className = 'btn-dark'),
                        list(extend = 'csv', text = 'CSV', title = 'benchmark_concorrentes', className = 'btn-dark'),
                        list(extend = 'excel', text = 'Excel', title = 'benchmark_concorrentes', className = 'btn-dark')
                      ),
                      initComplete = JS(
                        "function(settings, json) {",
                        "  $(this.api().table().header()).css({",
                        "    'background-color': '#0a0c10',",
                        "    'color': '#94a3b8',",
                        "    'font-weight': '600',",
                        "    'font-size': '12px',",
                        "    'text-transform': 'uppercase',",
                        "    'letter-spacing': '0.5px',",
                        "    'border-bottom': '2px solid #334155'",
                        "  });",
                        "}"
                      ),
                      columnDefs = list(
                        list(visible = FALSE, targets = idx_ocultas),
                        list(className = 'dt-center', targets = 1:(colunas_originais - 1)),
                        list(className = 'dt-left', targets = 0)
                      )
                    ),
                    class = "table-dark table-hover compact",
                    rownames = FALSE) %>%
      formatStyle('Aspecto', fontWeight = 'bold', color = '#e2e8f0', borderRight = '1px solid #334155') %>%
      formatStyle(media, fontStyle = 'italic', color = '#cbd5e1', background = styleColorBar(c(0, 5), "rgba(148, 163, 184, 0.2)"), backgroundSize = "95% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
    
    for(emp in concorrentes) {
      peso_fonte <- ifelse(emp == alvo, 'bold', 'normal')
      cor_fundo <- ifelse(emp == alvo, "rgba(74, 222, 128, 0.1)", "rgba(51, 65, 85, 0.4)")
      
      dt <- dt %>% formatStyle(
        emp,
        valueColumns = paste0("diff_", emp),
        fontWeight = peso_fonte,
        color = styleInterval(c(-0.001, 0.001), c('#f87171', '#94a3b8', '#4ade80')),
        background = styleColorBar(c(0, 5), cor_fundo),
        backgroundSize = "95% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
    }
    
    dt <- dt %>% formatRound(columns = todas_empresas, digits = 1, dec.mark = ",")
    
    return(dt)
  })
  

  # FUNÇÃO DE SEGURANÇA PARA EVITAR ERRO
  empty_plot <- function(msg = "Sem dados disponíveis no período") {
    plot_ly() %>% layout(
      title = list(text = msg, font = list(color = "#64748b", size = 14)),
      plot_bgcolor = "#111827", paper_bgcolor = "#111827",
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
    ) %>% config(displayModeBar = FALSE)
  }
  

  # VISÃO GERAL LINKEDIN
  
  output$kpi_views_totais <- renderUI({
    df <- DATA$linkedin_visitantes_serie
    if(is.null(df) || nrow(df) < 1) return(create_kpi_card("Visualizações Totais", 0, 0, 10000, is_volume=TRUE))
    atual <- tail(df, 1)
    ant <- if(nrow(df) >= 2) tail(df, 2)[1,] else atual
    create_kpi_card("Visualizações Totais", atual$Total_Views, ant$Total_Views, max(atual$Total_Views, ant$Total_Views, 1)*1.1, is_volume=TRUE)
  })
  
  output$kpi_views_media <- renderUI({
    df <- DATA$linkedin_visitantes_serie
    if(is.null(df) || nrow(df) < 1) return(create_kpi_card("Média Diária de Views", 0, 0, 500, is_volume=TRUE))
    atual <- tail(df, 1)
    ant <- if(nrow(df) >= 2) tail(df, 2)[1,] else atual
    create_kpi_card("Média Diária de Views", atual$Media_Diaria_Views, ant$Media_Diaria_Views, max(atual$Media_Diaria_Views, ant$Media_Diaria_Views, 1)*1.1, is_volume=TRUE)
  })
  
  output$kpi_unicos_totais <- renderUI({
    df <- DATA$linkedin_visitantes_serie
    if(is.null(df) || nrow(df) < 1) return(create_kpi_card("Visitantes Únicos", 0, 0, 5000, is_volume=TRUE))
    atual <- tail(df, 1)
    ant <- if(nrow(df) >= 2) tail(df, 2)[1,] else atual
    create_kpi_card("Visitantes Únicos", atual$Total_Unicos, ant$Total_Unicos, max(atual$Total_Unicos, ant$Total_Unicos, 1)*1.1, is_volume=TRUE)
  })
  
  output$kpi_seguidores_totais <- renderUI({
    df <- DATA$linkedin_visao_geral
    if(is.null(df) || nrow(df) < 1) return(create_kpi_card("Novos Seguidores", 0, 0, 1000, is_volume=TRUE))
    atual <- tail(df, 1)
    ant <- if(nrow(df) >= 2) tail(df, 2)[1,] else atual
    create_kpi_card("Novos Seguidores", atual$Novos_Seguidores, ant$Novos_Seguidores, max(atual$Novos_Seguidores, ant$Novos_Seguidores, 1)*1.1, is_volume=TRUE)
  })
  
  output$kpi_pc_share <- renderUI({
    df <- DATA$linkedin_visitantes_serie
    if(is.null(df) || nrow(df) == 0) return(create_kpi_card("Share Computador", 0, 0, 50, is_percent=TRUE))
    atual <- tail(df, 1)
    pct <- ifelse(atual$Total_Unicos > 0, (atual$Unicos_PC / atual$Total_Unicos) * 100, 0)
    create_kpi_card("Share Computador", pct, 0, 50, is_percent=TRUE)
  })
  
  output$kpi_mobile_share <- renderUI({
    df <- DATA$linkedin_visitantes_serie
    if(is.null(df) || nrow(df) == 0) return(create_kpi_card("Share Mobile", 0, 0, 50, is_percent=TRUE))
    atual <- tail(df, 1)
    pct <- ifelse(atual$Total_Unicos > 0, (atual$Unicos_Mobile / atual$Total_Unicos) * 100, 0)
    create_kpi_card("Share Mobile", pct, 0, 50, is_percent=TRUE)
  })
  
  output$kpi_views_growth <- renderUI({
    df <- DATA$linkedin_visitantes_serie
    if(is.null(df) || nrow(df) < 2) return(create_kpi_card("Crescimento Views", 0, 0, 10, is_percent=TRUE))
    atual <- tail(df, 1)
    ant <- tail(df, 2)[1,]
    growth <- ifelse(ant$Total_Views > 0, ((atual$Total_Views - ant$Total_Views) / ant$Total_Views) * 100, 0)
    create_kpi_card("Crescimento Views", growth, 0, 10, is_percent=TRUE)
  })
  
  output$kpi_followers_growth <- renderUI({
    df <- DATA$linkedin_visao_geral
    if(is.null(df) || nrow(df) < 2) return(create_kpi_card("Crescimento Seguidores", 0, 0, 10, is_percent=TRUE))
    atual <- tail(df, 1)
    ant <- tail(df, 2)[1,]
    growth <- ifelse(ant$Novos_Seguidores > 0, ((atual$Novos_Seguidores - ant$Novos_Seguidores) / ant$Novos_Seguidores) * 100, 0)
    create_kpi_card("Crescimento Seguidores", growth, 0, 10, is_percent=TRUE)
  })
  
  output$ln_views_vs_unicos_plot <- renderPlotly({
    df <- DATA$linkedin_visitantes_serie
    if(is.null(df) || nrow(df) == 0) return(empty_plot())
    
    p <- plot_ly()
    
    if("Total_Views" %in% names(df) && any(!is.na(df$Total_Views))) {
      p <- p %>% add_trace(x = ~df$Data_Ref, y = ~df$Total_Views, name = "Visualizações", type = "scatter", mode = "lines+markers", line = list(color = "#4ade80", width = 3), marker = list(size = 8, color = "#4ade80"))
    }
    
    if("Total_Unicos" %in% names(df) && any(!is.na(df$Total_Unicos))) {
      p <- p %>% add_trace(x = ~df$Data_Ref, y = ~df$Total_Unicos, name = "Visitantes Únicos", type = "scatter", mode = "lines+markers", line = list(color = "#3b82f6", width = 3), marker = list(size = 8, color = "#3b82f6"))
    }
    
    if(length(p$x$attrs) == 0) return(empty_plot())
    
    p %>% layout(
      plot_bgcolor = "#111827", paper_bgcolor = "#111827", hovermode = "x unified",
      xaxis = list(title = "", gridcolor = "#374151", tickformat = "%b %Y", tickfont = list(color = "#e2e8f0")),
      yaxis = list(title = "Volume", gridcolor = "#374151", tickfont = list(color = "#e2e8f0")),
      legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center", font = list(color = "#e2e8f0")),
      margin = list(t = 50)
    ) %>% config(displayModeBar = FALSE)
  })
  
  output$ln_corp_vagas_plot <- renderPlotly({
    df <- DATA$linkedin_visitantes_serie
    if(is.null(df) || nrow(df) == 0) return(empty_plot())
    
    tem_corp <- "Visualizacoes_Corp" %in% names(df) && any(!is.na(df$Visualizacoes_Corp) & df$Visualizacoes_Corp > 0)
    tem_vagas <- "Visualizacoes_Vagas" %in% names(df) && any(!is.na(df$Visualizacoes_Vagas) & df$Visualizacoes_Vagas > 0)
    tem_media <- "Media_Diaria_Views" %in% names(df) && any(!is.na(df$Media_Diaria_Views) & df$Media_Diaria_Views > 0)
    
    if(!tem_corp && !tem_vagas && !tem_media) return(empty_plot())
    
    p <- plot_ly(df, x = ~Data_Ref)
    
    if(tem_corp) {
      p <- p %>% add_bars(y = ~Visualizacoes_Corp, name = "Views Institucional", marker = list(color = "#8b5cf6", line = list(width = 1, color = "#0a0c10")))
    }
    
    if(tem_vagas) {
      p <- p %>% add_bars(y = ~Visualizacoes_Vagas, name = "Views Vagas", marker = list(color = "#f59e0b", line = list(width = 1, color = "#0a0c10")))
    }
    
    if(tem_media) {
      p <- p %>% add_lines(y = ~Media_Diaria_Views, name = "Média Diária (Total)", yaxis = "y2", line = list(color = "#ffffff", width = 3), marker = list(size = 6, color = "#ffffff"))
    }
    
    p %>% layout(
      barmode = 'group', plot_bgcolor = "#111827", paper_bgcolor = "#111827", hovermode = "x unified",
      xaxis = list(title = "", gridcolor = "transparent", tickformat = "%b %Y", tickfont = list(color = "#e2e8f0")),
      yaxis = list(title = "Volume (Barras)", gridcolor = "#374151", tickfont = list(color = "#e2e8f0")),
      yaxis2 = list(title = "Média (Linha)", overlaying = "y", side = "right", gridcolor = "transparent", tickfont = list(color = "#ffffff")),
      legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center", font = list(color = "#e2e8f0")),
      margin = list(t = 50)
    ) %>% config(displayModeBar = FALSE)
  })
  
  output$ln_tamanho_bar_plot <- renderPlotly({
    vis <- DATA$linkedin_visitantes$tamanhos
    seg <- DATA$linkedin_seguidores$tamanhos
    
    vis_agg <- if(!is.null(vis) && nrow(vis) > 0) {
      vis %>% group_by(Tamanho) %>% summarise(Visitas = sum(Total_Visualizacoes, na.rm = TRUE), .groups = 'drop')
    } else { data.frame() }
    
    seg_agg <- if(!is.null(seg) && nrow(seg) > 0) {
      seg %>% group_by(Tamanho) %>% summarise(Seguidores = sum(Total_Seguidores, na.rm = TRUE), .groups = 'drop')
    } else { data.frame() }
    
    df_plot <- full_join(vis_agg, seg_agg, by = "Tamanho") %>%
      filter(!is.na(Tamanho), Tamanho != "", Tamanho != "NA")
    
    if(nrow(df_plot) == 0) return(empty_plot())
    
    ordem <- c("1", "2-10", "11-50", "51-200", "201-500", "501-1.000", "1.001-5.000", "5.001-10.000", "+ de 10.001")
    df_plot <- df_plot %>% mutate(Tamanho = factor(Tamanho, levels = ordem)) %>% arrange(Tamanho)
    
    p <- plot_ly(df_plot, x = ~Tamanho)
    
    if("Visitas" %in% names(df_plot) && any(!is.na(df_plot$Visitas))) {
      p <- p %>% add_bars(y = ~Visitas, name = "Visualizações", marker = list(color = "#4ade80"))
    }
    
    if("Seguidores" %in% names(df_plot) && any(!is.na(df_plot$Seguidores))) {
      p <- p %>% add_bars(y = ~Seguidores, name = "Seguidores", marker = list(color = "#3b82f6"))
    }
    
    p %>% layout(
      barmode = 'group', plot_bgcolor = "#111827", paper_bgcolor = "#111827", hovermode = "x unified",
      xaxis = list(title = "Porte da Empresa", gridcolor = "transparent", tickangle = -45, tickfont = list(color = "#e2e8f0")),
      yaxis = list(title = "Volume Total", gridcolor = "#374151", tickfont = list(color = "#e2e8f0")),
      legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center", font = list(color = "#e2e8f0")),
      margin = list(b = 80, t = 50)
    ) %>% config(displayModeBar = FALSE)
  })
  
  output$ln_disp_table <- renderDT({
    df <- DATA$linkedin_visitantes_serie
    if(is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Mensagem = "Sem dados"), options = list(dom = 't'), class = "table-dark"))
    }
    
    if(!all(c("Total_Unicos", "Unicos_PC", "Unicos_Mobile") %in% names(df))) {
      return(datatable(data.frame(Mensagem = "Colunas necessárias não encontradas"), options = list(dom = 't'), class = "table-dark"))
    }
    
    df_table <- df %>% arrange(desc(Data_Ref)) %>%
      mutate(
        Mes = format(Data_Ref, "%b/%Y"),
        Var_Unicos = ifelse(!is.na(lead(Total_Unicos)) & lead(Total_Unicos) > 0, (Total_Unicos - lead(Total_Unicos)) / lead(Total_Unicos), NA),
        PC_Share = ifelse(Total_Unicos > 0, Unicos_PC / Total_Unicos, 0),
        Mob_Share = ifelse(Total_Unicos > 0, Unicos_Mobile / Total_Unicos, 0)
      ) %>%
      select(Mes, Total_Unicos, Var_Unicos, Unicos_PC, PC_Share, Unicos_Mobile, Mob_Share) %>%
      head(12)
    
    datatable(df_table, 
              colnames = c("Mês", "Visitantes Únicos", "Crescimento (MoM)", "Via Computador", "% Computador", "Via Celular", "% Celular"),
              options = list(dom = 't', paging = FALSE, scrollX = TRUE),
              class = "table-dark table-hover compact", 
              rownames = FALSE
    ) %>%
      formatRound(columns = c("Total_Unicos", "Unicos_PC", "Unicos_Mobile"), digits = 0) %>%
      formatPercentage(columns = c("PC_Share", "Mob_Share"), digits = 1) %>%
      formatPercentage(columns = "Var_Unicos", digits = 1) %>%
      formatStyle('Var_Unicos', color = styleInterval(0, c('#f87171', '#4ade80')), fontWeight = 'bold')
  })
  

  # DEMOGRAFIA LINKEDIN
  
  observe({
    req(DATA$linkedin_visitantes$funcoes)
    anos <- sort(unique(year(DATA$linkedin_visitantes$funcoes$Data_Ref)), decreasing = TRUE)
    meses <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
    updateSelectInput(session, "ln_demo_ano", choices = c("Todos" = "all", anos))
    updateSelectInput(session, "ln_demo_mes", choices = c("Todos" = "all", meses))
  })
  
  filtrar_demo <- function(categoria, is_vis) {
    df <- if(is_vis == "vis") DATA$linkedin_visitantes[[categoria]] else DATA$linkedin_seguidores[[categoria]]
    if(is.null(df) || nrow(df) == 0) return(data.frame())
    
    if("Total_Visualizacoes" %in% names(df)) {
      df$Valor <- df$Total_Visualizacoes
    } else if("Total_Seguidores" %in% names(df)) {
      df$Valor <- df$Total_Seguidores
    }
    
    if(input$ln_demo_ano != "all") df <- df %>% filter(year(Data_Ref) == as.numeric(input$ln_demo_ano))
    if(input$ln_demo_mes != "all") df <- df %>% filter(format(Data_Ref, "%m") == input$ln_demo_mes)
    return(df)
  }
  
  output$ln_demo_exp_yoy_plot <- renderPlotly({
    req(input$tgl_exp_bar)
    df <- filtrar_demo("experiencias", input$tgl_exp_bar)
    if(nrow(df) == 0) return(empty_plot())
    
    ano_atual <- max(year(df$Data_Ref), na.rm = TRUE)
    df_plot <- df %>% 
      filter(year(Data_Ref) %in% c(ano_atual, ano_atual - 1)) %>%
      mutate(Ano = as.character(year(Data_Ref))) %>%
      group_by(Ano, Experiencia) %>% summarise(Valor = sum(Valor, na.rm = TRUE), .groups = 'drop')
    
    niveis <- c("Iniciante", "Sênior", "Gerente", "Diretor", "Vice-Presidente")
    df_plot <- df_plot %>% filter(Experiencia %in% niveis) %>% 
      mutate(Experiencia = factor(Experiencia, levels = niveis)) %>% arrange(Experiencia)
    
    if(nrow(df_plot) == 0) return(empty_plot())
    
    plot_ly(df_plot, x = ~Experiencia, y = ~Valor, color = ~Ano, 
            colors = c("#64748b", "#4ade80"), type = 'bar') %>%
      layout(
        barmode = 'group', plot_bgcolor = "#111827", paper_bgcolor = "#111827", hovermode = "x unified",
        xaxis = list(title = "", gridcolor = "transparent", tickangle = -30, tickfont = list(color = "#e2e8f0")),
        yaxis = list(title = "Total", gridcolor = "#374151", tickfont = list(color = "#e2e8f0")),
        legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center", font = list(color = "#e2e8f0")),
        margin = list(t = 50, b = 80)
      ) %>% config(displayModeBar = FALSE)
  })
  
  output$ln_demo_exp_ts_plot <- renderPlotly({
    req(input$tgl_exp_line)
    df <- filtrar_demo("experiencias", input$tgl_exp_line)
    if(nrow(df) == 0) return(empty_plot())
    
    niveis_principais <- c("Iniciante", "Sênior", "Gerente", "Diretor")
    df_plot <- df %>% 
      filter(Experiencia %in% niveis_principais) %>% 
      group_by(Data_Ref, Experiencia) %>% summarise(Valor = sum(Valor, na.rm = TRUE), .groups = 'drop')
    
    if(nrow(df_plot) == 0) return(empty_plot())
    
    plot_ly(df_plot, x = ~Data_Ref, y = ~Valor, color = ~Experiencia, type = 'scatter', mode = 'lines+markers', line = list(width = 2)) %>%
      layout(
        plot_bgcolor = "#111827", paper_bgcolor = "#111827", hovermode = "x unified",
        xaxis = list(title = "", gridcolor = "#374151", tickformat = "%b %Y", tickfont = list(color = "#e2e8f0")),
        yaxis = list(title = "Total", gridcolor = "#374151", tickfont = list(color = "#e2e8f0")),
        legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center", font = list(color = "#e2e8f0")),
        margin = list(t = 50)
      ) %>% config(displayModeBar = FALSE)
  })
  
  output$ln_demo_tam_ts_plot <- renderPlotly({
    vis <- filtrar_demo("tamanhos", "vis") %>% mutate(Tipo = "Visitantes")
    seg <- filtrar_demo("tamanhos", "seg") %>% mutate(Tipo = "Seguidores")
    df <- bind_rows(vis, seg)
    if(nrow(df) == 0) return(empty_plot())
    
    df_plot <- df %>%
      mutate(Grupo = ifelse(grepl("1|2-10|11-50|51-200|201-500", Tamanho), "Pequenas", "Grandes"),
             Serie = paste(Grupo, Tipo, sep = " - ")) %>%
      group_by(Data_Ref, Serie) %>% summarise(Valor = sum(Valor, na.rm = TRUE), .groups = 'drop')
    
    paleta <- c("Pequenas - Visitantes" = "#60a5fa", "Pequenas - Seguidores" = "#2563eb", 
                "Grandes - Visitantes" = "#fcd34d", "Grandes - Seguidores" = "#d97706")
    
    plot_ly(df_plot, x = ~Data_Ref, y = ~Valor, color = ~Serie, colors = paleta, 
            type = 'scatter', mode = 'lines+markers', line = list(width = 3)) %>%
      layout(
        plot_bgcolor = "#111827", paper_bgcolor = "#111827", hovermode = "x unified",
        xaxis = list(title = "", gridcolor = "#374151", tickformat = "%b %Y", tickfont = list(color = "#e2e8f0")),
        yaxis = list(title = "Total", gridcolor = "#374151", tickfont = list(color = "#e2e8f0")),
        legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center", font = list(color = "#e2e8f0")),
        margin = list(t = 50)
      ) %>% config(displayModeBar = FALSE)
  })
  
  output$ln_demo_func_table <- renderDT({
    req(input$tgl_tab_func)
    df <- filtrar_demo("funcoes", input$tgl_tab_func)
    if(nrow(df) == 0) return(datatable(data.frame(Mensagem = "Sem dados"), options = list(dom = 't'), class = "table-dark"))
    
    datas <- sort(unique(df$Data_Ref), decreasing = TRUE)
    if(length(datas) < 2) return(datatable(data.frame(Mensagem = "Mínimo 2 meses"), options = list(dom = 't'), class = "table-dark"))
    
    m1 <- as.character(datas[1]); m2 <- as.character(datas[2])
    
    df_res <- df %>% filter(Data_Ref %in% c(datas[1], datas[2])) %>%
      filter(!Funcao %in% c("Treinamento", "Iniciante", "Sênior", "Gerente", "Diretor", "Vice-Presidente", "Proprietário", "Parceiro", "Não remunerado")) %>%
      group_by(Funcao, Data_Ref) %>% summarise(Val = sum(Valor, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Data_Ref = as.character(Data_Ref)) %>% 
      pivot_wider(names_from = Data_Ref, values_from = Val, values_fill = 0)
    
    if(!m1 %in% names(df_res)) df_res[[m1]] <- 0
    if(!m2 %in% names(df_res)) df_res[[m2]] <- 0
    
    df_res <- df_res %>% rename(Mes_Atual = !!sym(m1), Mes_Anterior = !!sym(m2)) %>%
      mutate(Evolucao_Vol = Mes_Atual - Mes_Anterior, 
             Evolucao_Pct = ifelse(Mes_Anterior > 0, Evolucao_Vol / Mes_Anterior, ifelse(Mes_Atual > 0, 1, 0))) %>% 
      arrange(desc(Mes_Atual)) %>% head(15)
    
    datatable(df_res, 
              colnames = c("Cargo/Função", "Mês Anterior", "Mês Atual", "Evolução (Vol)", "Evolução (%)"), 
              options = list(dom = 't', paging = FALSE, scrollX = TRUE), 
              class = "table-dark table-hover compact", 
              rownames = FALSE
    ) %>%
      formatRound(c('Mes_Anterior', 'Mes_Atual', 'Evolucao_Vol'), digits = 0) %>%
      formatPercentage('Evolucao_Pct', 1) %>% 
      formatStyle('Evolucao_Pct', color = styleInterval(0, c('#f87171', '#4ade80')), fontWeight = 'bold')
  })
  
  output$ln_demo_tam_table <- renderDT({
    req(input$tgl_tab_tam)
    df <- filtrar_demo("tamanhos", input$tgl_tab_tam)
    if(nrow(df) == 0) return(datatable(data.frame(Mensagem = "Sem dados"), options = list(dom = 't'), class = "table-dark"))
    
    datas <- sort(unique(df$Data_Ref), decreasing = TRUE)
    if(length(datas) < 2) return(datatable(data.frame(Mensagem = "Mínimo 2 meses"), options = list(dom = 't'), class = "table-dark"))
    
    m1 <- as.character(datas[1]); m2 <- as.character(datas[2])
    
    df_res <- df %>% filter(Data_Ref %in% c(datas[1], datas[2])) %>%
      group_by(Tamanho, Data_Ref) %>% summarise(Val = sum(Valor, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Data_Ref = as.character(Data_Ref)) %>% 
      pivot_wider(names_from = Data_Ref, values_from = Val, values_fill = 0)
    
    if(!m1 %in% names(df_res)) df_res[[m1]] <- 0
    if(!m2 %in% names(df_res)) df_res[[m2]] <- 0
    
    ordem <- c("1", "2-10", "11-50", "51-200", "201-500", "501-1.000", "1.001-5.000", "5.001-10.000", "+ de 10.001")
    df_res <- df_res %>% rename(Mes_Atual = !!sym(m1), Mes_Anterior = !!sym(m2)) %>%
      mutate(Tamanho = factor(Tamanho, levels = ordem), 
             Evolucao_Vol = Mes_Atual - Mes_Anterior, 
             Evolucao_Pct = ifelse(Mes_Anterior > 0, Evolucao_Vol / Mes_Anterior, ifelse(Mes_Atual > 0, 1, 0))) %>% 
      arrange(Tamanho)
    
    datatable(df_res, 
              colnames = c("Tamanho da Empresa", "Mês Anterior", "Mês Atual", "Evolução (Vol)", "Evolução (%)"), 
              options = list(dom = 't', paging = FALSE, scrollX = TRUE), 
              class = "table-dark table-hover compact", 
              rownames = FALSE
    ) %>%
      formatRound(c('Mes_Anterior', 'Mes_Atual', 'Evolucao_Vol'), digits = 0) %>%
      formatPercentage('Evolucao_Pct', 1) %>% 
      formatStyle('Evolucao_Pct', color = styleInterval(0, c('#f87171', '#4ade80')), fontWeight = 'bold')
  })
  

  # REFRESH DOS DADOS
  observeEvent(input$refresh, {
    showNotification("Atualizando dados sintéticos...", type = "info", duration = 2)
    new_data <- load_all_data()
    DATA <<- new_data
    
    if(nrow(DATA$main) > 0) {
      anos_disponiveis <- sort(unique(DATA$main$Ano), decreasing = TRUE)
      updateSelectInput(session, "ano", choices = c("Todos", anos_disponiveis), selected = "Todos")
    }
    
    updateSelectInput(session, "ln_demo_ano", choices = c("Todos" = "all", sort(unique(year(DATA$linkedin_visitantes$funcoes$Data_Ref)), decreasing = TRUE)))
    
    showNotification("Dados atualizados!", type = "success", duration = 2)
  })
}


# EXECUTA O APP
shinyApp(ui = ui, server = server)