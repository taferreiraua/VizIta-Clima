# packages
pacman::p_load(tidyverse)


# dados
ita_quandu = read.csv("~/2023.2/Projeto Integrador 2/Data/Raw/ita_quandu_3@2.csv")


# Manipulação de dados

## Tipagem

ita_quandu = ita_quandu |>
  mutate_at(c('direcao_do_vento_10m_graus', 'temperatura_do_ar_a_2m_c', 'umidade_relativa_do_ar_a_2m'), as.double)

## Criando a variável data_hora e retirando o fuso-horário

ita_quandu = ita_quandu |>
  mutate(data_hora = paste0(data, ' ', hora_utc), # juntando a coluna data e hora_utc
         data_hora = dmy_hms(data_hora), # transformando em um objeto de datetime
         data_hora = data_hora - hours(3)) |> # retirando 3 horas do fuso horário
  select(-c(data, hora_utc))

## Filtros

### Foram retiradas observações no período entre 04/04/2005 e 22/06/2006 (dados duplicados)
### Foram retiradas observações no período entre 31/10/2011 e 26/10/2014 (comportamento atípico)
### Foram estabelecidos pontos de corte para as variáveis: (comportamento atípico)
    # precipitacao_pluviometrica_mm -> [0, 100]
    # temperatura_do_ar_a_2m_c ->  [21, 39]
    # pressao_atmosferica_h_pa -> [900, 1030]

ita_quandu = ita_quandu |>
  filter(data_hora > '2006-06-22') |>
  filter(data_hora < '2011-10-31') |>
  filter(precipitacao_pluviometrica_mm >=0 & precipitacao_pluviometrica_mm <= 100,
         temperatura_do_ar_a_2m_c >= 21 & temperatura_do_ar_a_2m_c <= 39,
         pressao_atmosferica_h_pa >= 900 & pressao_atmosferica_h_pa <= 1030)


# Exportando os dados
write.table(ita_quandu, file = "~/2023.2/Projeto Integrador 2/Data/Clean/clean_ita_quandu.csv", 
            sep = ",", na = "", quote = T, row.names = F)
