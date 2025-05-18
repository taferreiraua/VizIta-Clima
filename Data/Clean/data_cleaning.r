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


# Salvando os dados
write.table(ita_quandu, file = "~/2023.2/Projeto Integrador 2/Data/Clean/clean_ita_quandu.csv", 
            sep = ",", na = "", quote = T, row.names = F)

## renomeando as variaveis

colnames(df_raw_INMET) = c('data_medicao', 
                           'hora_medicao', 
                           'precipitacao_total_horario_mm',
                           'pressao_atmosferica_nivel_estacao_horaria_mB',
                           'pressao_atmosferica_nivel_mar_mB',
                           'pressao_atmosferica_max_mB',
                           'pressao_atmosferica_min_mB',
                           'radiacao_Kjm',
                           'temperatura_cpu_estacao_C',
                           'temperatura_do_ar_C',
                           'temperatura_ponto_de_orvalho_C',
                           'temperatura_do_ar_max_na_hora_anterior_C',
                           'temperatura_do_ar_min_na_hora_anterior_C',
                           'temperatura_max_ponto_de_orvalho_na_hora_anterior_C',
                           'temperatura_min_ponto_de_orvalho_na_hora_anterior_C',
                           'umidade_relativa_do_ar_max_na_hora_anterior_percentual',
                           'umidade_relativa_do_ar_min_na_hora_anterior_percentual',
                           'umidade_relativa_do_ar_horaria_percentual',
                           'direcao_do_vento_horaria_(gr)',
                           'rajada_max_do_vento_ms',
                           'velocidade_do_vento_horaria_ms')


df_INMET = df_raw_INMET |>
  mutate(data_medicao = as.Date(data_medicao, format='%d/%m/%Y'),
         hora_medicao = sapply(hora_medicao, get_hora_formatada)) |>
  mutate_at(vars(-one_of("hora_medicao")), ~ if(is.character(.)) get_valor_float_formatado(.) else .)


## Retira registros que não possuem data de medição

df_INMET = df_INMET |>
  filter(rowSums(!is.na(select(df_INMET, -one_of(c('data_medicao', 'hora_medicao'))))) > 0)


# Exportando os dados

write.csv(df_INMET, file = 'C:/Users/Thays Ferreira/Documents/UFC/Semestres/2024.1/Projeto integrador III/Data/Clean/df_clean_INMET_2008-09-04_2024-04-12.csv', row.names=F)
