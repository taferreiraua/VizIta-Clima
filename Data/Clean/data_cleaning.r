# packages
pacman::p_load(tidyverse)


# dados
df_raw_INMET = read.csv('C:/Users/Thays Ferreira/Documents/UFC/Semestres/2024.1/Projeto integrador III/Data/Raw/dados_A359_H_2008-09-04_2024-04-12.csv', sep=';')


# manipulação

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


# Salvando os dados

write.csv(df_INMET, file = 'C:/Users/Thays Ferreira/Documents/UFC/Semestres/2024.1/Projeto integrador III/Data/Clean/df_clean_INMET_2008-09-04_2024-04-12.csv', row.names=F)
