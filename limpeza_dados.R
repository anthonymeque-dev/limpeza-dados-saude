# 1 Passo

library(tidyverse)
library(lubridate)

# 2. Selecionar os ficheiros CSV 
ficheiro_doencas <- file.choose()
ficheiro_populacao <- file.choose()

# 3. Importar os dados
doencas <- readr::read_csv(
  ficheiro_doencas,
  na = c("", "NA", "N/A", "null", "NULL"),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

populacao <- readr::read_csv(
  ficheiro_populacao,
  na = c("", "NA", "N/A", "null", "NULL"),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

# 4. Diagnóstico inicial
cat("\n--- DIMENSÕES INICIAIS ---\n")
print(dim(doencas))
print(dim(populacao))

cat("\n--- VALORES AUSENTES EM DOENCAS ---\n")
print(colSums(is.na(doencas)))

cat("\n--- LINHAS DUPLICADAS EM DOENCAS ---\n")
print(sum(duplicated(doencas)))

cat("\n--- IDs REPETIDOS EM DOENCAS ---\n")
print(doencas %>% count(id_caso, name = "quantidade") %>% filter(quantidade > 1))

# 5. Limpar a tabela de doenças
doencas_limpos <- doencas %>%
  # Remover espaços no início/fim e espaços repetidos nos campos de texto
  mutate(across(where(is.character), ~ stringr::str_squish(.x))) %>%
  
  # Remover linhas totalmente duplicadas
  distinct() %>%
  
  # Padronizar províncias
  mutate(
    provincia = case_when(
      str_to_lower(provincia) %in% c("zambezía", "zambezia") ~ "Zambézia",
      TRUE ~ provincia
    ),
    
    # Padronizar nomes das doenças, ignorando maiúsculas/minúsculas
    doenca = case_when(
      str_to_lower(doenca) %in% c("malária", "malaria") ~ "Malária",
      str_to_lower(doenca) %in% c("diarreia aguda", "diarreia") ~ "Diarreia aguda",
      str_to_lower(doenca) == "pneumonia" ~ "Pneumonia",
      str_to_lower(doenca) == "tuberculose" ~ "Tuberculose",
      str_to_lower(doenca) %in% c("cólera", "colera") ~ "Cólera",
      str_to_lower(doenca) == "sarampo" ~ "Sarampo",
      TRUE ~ doenca
    ),
    
    # Padronizar sexo
    sexo = case_when(
      str_to_lower(sexo) %in% c("m", "masculino") ~ "Masculino",
      str_to_lower(sexo) %in% c("f", "feminino") ~ "Feminino",
      TRUE ~ NA_character_
    ),
    
    # Padronizar resposta de hospitalização
    hospitalizado = case_when(
      str_to_lower(hospitalizado) %in% c("sim", "s", "yes", "yes") ~ "Sim",
      str_to_lower(hospitalizado) %in% c("não", "nao", "n", "no") ~ "Não",
      TRUE ~ NA_character_
    ),
    
    # Padronizar desfecho sem preencher valores desconhecidos
    desfecho = case_when(
      str_to_lower(desfecho) == "recuperado" ~ "Recuperado",
      str_to_lower(desfecho) == "em tratamento" ~ "Em tratamento",
      str_to_lower(desfecho) %in% c("óbito", "obito") ~ "Óbito",
      TRUE ~ NA_character_
    ),
    
    # Converter data para formato de data
    data_notificacao = lubridate::ymd(data_notificacao, quiet = TRUE),
    
    # Converter variáveis numéricas com segurança
    idade = readr::parse_number(as.character(idade)),
    dias_internamento = readr::parse_number(as.character(dias_internamento)),
    
    # Idades negativas ou acima de 120 são tratadas como inválidas e ficam NA.
    # Não são eliminadas as linhas; apenas o valor inválido é marcado como ausente.
    idade = if_else(idade < 0 | idade > 120, NA_real_, idade),
    
    # Duração de internamento deve ser positiva; valores inválidos ficam NA.
    dias_internamento = if_else(dias_internamento <= 0, NA_real_, dias_internamento)
  )

# Se um ID continuar repetido depois de remover duplicados exatos,
# interromper para investigar em vez de eliminar registos potencialmente diferentes.
ids_repetidos <- doencas_limpos %>%
  count(id_caso, name = "quantidade") %>%
  filter(quantidade > 1)

if (nrow(ids_repetidos) > 0) {
  print(ids_repetidos)
  stop("Há IDs repetidos com registos potencialmente diferentes. Investigue antes de continuar.")
}

# 6. Limpar a tabela de população
populacao_limpa <- populacao %>%
  mutate(across(where(is.character), ~ stringr::str_squish(.x))) %>%
  mutate(
    provincia = case_when(
      str_to_lower(provincia) %in% c("zambezía", "zambezia") ~ "Zambézia",
      TRUE ~ provincia
    ),
    regiao = str_to_title(regiao),
    populacao = readr::parse_number(as.character(populacao))
  ) %>%
  distinct()

# Verificar províncias sem correspondência na tabela de população
provincias_sem_populacao <- doencas_limpos %>%
  distinct(provincia) %>%
  anti_join(populacao_limpa %>% distinct(provincia), by = "provincia")

cat("\n--- PROVÍNCIAS SEM CORRESPONDÊNCIA NA TABELA DE POPULAÇÃO ---\n")
print(provincias_sem_populacao)

# 7. Juntar os casos à população provincial
# A população é acrescentada a cada caso com base na província.
doencas_com_populacao <- doencas_limpos %>%
  left_join(populacao_limpa, by = "provincia")

# 8. Relatório final de qualidade
cat("\n--- RESUMO APÓS LIMPEZA ---\n")
cat("Linhas originais de doenças:", nrow(doencas), "\n")
cat("Linhas após limpeza:", nrow(doencas_limpos), "\n")
cat("Duplicados exactos removidos:", nrow(doencas) - nrow(doencas %>% distinct()), "\n")
cat("\nValores ausentes após limpeza:\n")
print(colSums(is.na(doencas_limpos)))

cat("\nCategorias de doença:\n")
print(sort(unique(doencas_limpos$doenca)))

cat("\nCategorias de sexo:\n")
print(table(doencas_limpos$sexo, useNA = "ifany"))

cat("\nCasos por província:\n")
print(doencas_limpos %>% count(provincia, sort = TRUE))

# 9. Exportar os ficheiros limpos para a pasta de trabalho atual
readr::write_csv(doencas_limpos, "doencas_limpos.csv", na = "")
readr::write_csv(populacao_limpa, "populacao_limpa.csv", na = "")
readr::write_csv(doencas_com_populacao, "doencas_com_populacao.csv", na = "")

cat("\nFicheiros criados na pasta:\n")
print(getwd())
cat("- doencas_limpos.csv\n")
cat("- populacao_limpa.csv\n")
cat("- doencas_com_populacao.csv\n")

# 10. Gráfico simples para validar a limpeza
grafico_casos <- ggplot(doencas_limpos, aes(x = provincia)) +
  geom_bar() +
  labs(
    title = "Número de casos por província",
    x = "Província",
    y = "Número de casos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_casos)

