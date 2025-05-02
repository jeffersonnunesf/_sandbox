# Pacotes necessários

library(dplyr) # Manipulação de dados
library(ggplot2) # Visualização de dados
library(janitor) # Limpeza e resumo dos dados
library(lubridate) # Manipulação de datas
library(naniar) # Visualização de dados faltantes
library(stringr) # Manipulação de textos
library(tidyr) # Para transformar os dados


# Lendo o conjunto de dados e LIMPANDO E TRANSFORMANDO VALORES DAS COLUNAS 'Salary' e 'HireDate'

df_base_employees <- readr::read_delim(
  file = "D:/Projetos Data Science/_sandbox/rh_analysis/base_funcionarios.csv",
  delim = ",") %>%
  dplyr::mutate(
    Salary = Salary |>
      gsub('"', "", x = _) |>            # Remove aspas
      trimws() |>                        # Tira espaços extras
      gsub("R\\$\\s*", "", x = _) |>     # Remove R$ e espaço
      gsub(",", "", x = _) |>            # Remove vírgula do milhar
      as.double()                        
    )
# Transformando a coluna 'HireDate' em Date

df_base_employees <- df_base_employees |>
  dplyr::mutate(
    HireDate = HireDate |> as.Date()
  )

# ANÁLISE DE QUALIDADE DE DADOS CATEGÓRICOS #

# Análise de balanceamento de classes com 'tabyl' do 'janitor'
# Analisando 'Departament' e 'Position'

df_employees_tabyl_department <- df_base_employees |>
  janitor::tabyl(Department)

df_employees_tabyl_position <- df_base_employees |>
  janitor::tabyl(Position)

# Exibindo análise 'Departamento' e 'Cargo'
df_employees_tabyl_department
df_employees_tabyl_position

# ANÁLISE DE QUALIDADE DE DADOS NUMÉRICOS #

df_stats <- df_base_employees |>
  dplyr::select(-c("EmployeeID")) |> # A coluna EmployeeID apesar de numérica é uma coluna categórica
  dplyr::summarise(
    dplyr::across(
      dplyr::where(is.numeric),
      list(
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        na_percentage = ~sum(is.na(.)) / n() * 100
      ),
      .names = "{.col}-{fn}" # usando hífen como separador
      )
    ) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".Value"),
    names_sep = "-" # Alinhando o separador como usado acima
  )

# Exibindo estatísticas descritivas
df_stats

# ANÁLISE DE QUALIDADE DE DADOS TEMPORAIS #

# Verificando se a 'HireDate' é do tipo Data

str(df_base_employees$HireDate)

# Análise de variáveis data

df_date_analysis <- df_base_employees %>% 
  dplyr::summarise(
    min_data = min(HireDate, na.rm = TRUE),
    max_date = max(HireDate, na.rm = TRUE),
    na_percentage = sum(is.na(HireDate)) / n() * 100
  )

# Exibindo todos os resultados

df_date_analysis
df_employees_tabyl_department
df_employees_tabyl_position
df_stats

# CONVERSANDO COM A ÁREA DE NEGÓCIO SOBRE OS NA's FOI SUGERIDO POR ELES QUE OS DADOS
# FALTANTES FOSSEM REMOVIDOS DA ANÁLISE. MAS QUE FOSSE DEIXANDO UMA VERSÃO 'ORIGINAL' DE BACKUP

# Verificando quais colunas estão com NA's
# 1.
colSums(is.na(df_base_employees))

# 2.
df_base_employees |> filter(if_any(everything(), is.na))


# Excluindo dados faltantes do DataFrame

df_base_employees_clean <- df_base_employees |>
  dplyr::filter(if_all(everything(), ~ !is.na(.)))

# Romovendo os espaços vazios das colunas 'vazias' que não eram NA's de verdade

df_base_employees_clean <- df_base_employees_clean |>
  dplyr::mutate(across(where(is.character), ~ na_if(trimws(.), ""))) |>
  dplyr::filter(if_all(everything(), ~ !is.na(.)))

# Verificando se está tudo vazio mesmo

sapply(df_base_employees_clean, function(x) sum(x == "", na.rm = TRUE))

sapply(df_base_employees_clean, function(x) sum(trimws(x) == "", na.rm = TRUE))

#################

# Medidas de tedência central 'Média' e 'Mediana'

mean_salary <- mean(df_base_employees_clean$Salary)
mean_age <- mean(df_base_employees_clean$Age)

df_stats_central <- df_base_employees_clean |>
  dplyr::group_by(Department, Position) |>
  dplyr::summarise(
    mean_age = mean(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE),
    mean_salary = mean(Salary, na.rm = TRUE),
    median_salary = median(Salary, na.rm = TRUE),
    .groups = "drop"
  ) 
# Exibindo resultado

df_stats_central

# Medidas de dispersão 'Variância' e 'Desvio Padrão'

variancia_salary <- var(df_base_employees_clean$Salary)
desvio_padrao_salary <- sd(df_base_employees_clean$Salary)
variancia_age <- var(df_base_employees_clean$Age)
desvio_padrao_age <- sd(df_base_employees_clean$Age)

# Exibindo resultados

variancia_salary
desvio_padrao_salary
variancia_age
desvio_padrao_age

# Comparando DP de 'Salary' com Média 'Salary'

desvio_padrao_salary
mean_salary

# Comparando DP de 'Age' com Média 'Age'

desvio_padrao_age
mean_age

# Como as médias e os DP's ficaram muito distantes, vou usar a amplitude pra ver possíveis distorcões

# Salary

amplitude_salary <- range(df_base_employees_clean$Salary)
amplitude_salary_diff <- diff(amplitude_salary)
amplitude_salary

amplitude_salary_diff

# Age

amplitude_age <- range(df_base_employees_clean$Age)
amplitude_age_diff <- diff(amplitude_age)
amplitude_age

amplitude_age_diff


# Calculando o IQR dos salários, para descobrir aonde está a maior concentaração de salários na empresa

# Antes, calcular os QUARTIS

#atribuindo coluna salário à "x"

x <- df_base_employees_clean$Salary

# Quartis
quantile(x)

# IQR
IQR(x)

# Plotando os valores no gráfico boxplot

df <- data.frame("x" = x)

# Desconsiderando os outliers

library(highcharter)

dat <- data_to_boxplot(df, x)
highchart() %>%
  hc_xAxis(type = "category") %>%
  hc_add_series_list(dat)

# Com outliers

df <- data.frame("x" = x)

dat <- data_to_boxplot(df, x, add_outliers = TRUE)
highchart() %>%
  hc_xAxis(type = "category") %>%
  hc_add_series_list(dat)

### Analisando por 'Department'

library(echarts4r)
library(dplyr)

median_income_df <- df_base_employees_clean %>%
  group_by(Department) %>%
  summarize(median_income = median(Salary, na.rm = TRUE))

#Arrange 'Department' by median income and create a factor
sorted_data <- df_base_employees_clean %>%
  left_join(median_income_df, by = "Department") %>%
  mutate(Department = factor(Department, levels = rev(median_income_df$Department[order(median_income_df$median_income)]))) %>%
  arrange(Department)

sorted_data %>%
  group_by(Department) %>%
  e_chart() %>%
  e_boxplot(Salary) %>%
  e_color(color = "#4292b5") %>%
  e_tooltip() %>%
  e_title(text = "Renda mensal por Department", subtext = "Em milhares de USD.")

########

# CORRELAÇÃO DE IDADE E SALÁRIO #

df_dados_rh_num <- df_base_employees_clean |>
  dplyr::select_if(is.numeric) |>
  dplyr::select(
    -c(EmployeeID)
  )
correl_dados_rh_num <- df_dados_rh_num |> cor(method = "spearman")

cor_chart_rh <- cor(correl_dados_rh_num) |>
  e_charts() |>
  e_correlations(
    order = "hclust",
    visual_map = FALSE
  ) |>
  e_visual_map(
    min = -1,
    max = 1
  ) |>
  e_tooltip()

correl_dados_rh_num |> as_tibble()

# Exibindo a matriz de correlação
cor_chart_rh


# ANÁLISE EXTRA -> ANALISANDO 'POSITION' COM 'GENDER' #
# Usando o coeficiente de Associação para variáveis categóricas -> Cramer's V

df_dados_rh_cat <- df_base_employees_clean |>
  dplyr::select_if(is.character)

library(rcompanion)

# Relacionando 'Position' e 'Gender'

rcompanion::cramerV(
  df_dados_rh_cat$Position,
  df_dados_rh_cat$Gender
)

# Calcula o coeficiente Cramer V, uma espécie de coeficiente de correlação entre variáveis categóricas. 
# A função deve ser aplicada sobre um df com todas variáveis do tipo character, que irá construir uma matriz com todos coeficientes, 
# de forma análoga à matriz de correlação.

cramer_mat <-
  function(df){
    lapply(names(df), function(x){
      df_aux <- lapply(
        names(df), 
        function(y){
          rcompanion::cramerV(df[[paste0(x)]], df[[paste0(y)]])[[1]]
        }
      ) 
      names(df_aux) <- names(df)
      df_aux %>% dplyr::bind_rows()
    }) %>%
      dplyr::bind_rows() %>% 
      dplyr::mutate( var = names(df)) %>% 
      dplyr::select(var, everything())
  }

rh_cramer_matrix <- cramer_mat(df_dados_rh_cat)
rh_cramer_matrix











