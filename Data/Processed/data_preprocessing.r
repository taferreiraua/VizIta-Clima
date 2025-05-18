# Packages
pacman::p_load(tidyverse)


# Dados
ita_quandu = read.csv("~/2023.2/Projeto Integrador 2/Data/Clean/clean_ita_quandu.csv")

ita_quandu$data_hora = as.POSIXct(ita_quandu$data_hora, format='%Y-%m-%d %H:%M:%S')


# Criando datasets

## Wind-Rose

### velocidade: intervalos numéricos para velocidade media do vento (categórica)
### ponto_cardeal: ponto cardeal para direção do vento (categórica)
### frequencia: frequencia da velocidade de direcao do vento por ponto cardeal (numérica)

wind_rose_ita_quandu = ita_quandu |>
  filter(!is.na(direcao_do_vento_10m_graus),
         !is.na(velocidade_media_do_vento_10m_m_s)) |>
  mutate(
    velocidade = case_when(
      (velocidade_media_do_vento_10m_m_s >= 0 & velocidade_media_do_vento_10m_m_s <= 2) ~ '< 2 m/s',
      (velocidade_media_do_vento_10m_m_s > 2 & velocidade_media_do_vento_10m_m_s <= 4) ~ '2-4 m/s',
      (velocidade_media_do_vento_10m_m_s > 4 & velocidade_media_do_vento_10m_m_s <= 6) ~ '4-6 m/s',
      (velocidade_media_do_vento_10m_m_s > 6 & velocidade_media_do_vento_10m_m_s <= 8) ~ '6-8 m/s',
      (velocidade_media_do_vento_10m_m_s > 8 & velocidade_media_do_vento_10m_m_s <= 52) ~ '> 8 m/s')) |>
  mutate(
    ponto_cardeal = case_when(
      (direcao_do_vento_10m_graus > 348.75 & direcao_do_vento_10m_graus <= 360) ~ 'N',
      (direcao_do_vento_10m_graus >= 0 & direcao_do_vento_10m_graus <= 11.25) ~ 'N',
      (direcao_do_vento_10m_graus > 11.25 & direcao_do_vento_10m_graus <= 33.75) ~ 'NNE',
      (direcao_do_vento_10m_graus > 33.75 & direcao_do_vento_10m_graus <= 56.25) ~ 'NE',
      (direcao_do_vento_10m_graus > 56.25 & direcao_do_vento_10m_graus <= 78.75) ~ 'LNE',
      (direcao_do_vento_10m_graus > 78.75 & direcao_do_vento_10m_graus <= 101.25) ~ 'L',
      (direcao_do_vento_10m_graus > 101.25 & direcao_do_vento_10m_graus <= 123.75) ~ 'LSE',
      (direcao_do_vento_10m_graus > 123.75 & direcao_do_vento_10m_graus <= 146.25) ~ 'SE',
      (direcao_do_vento_10m_graus > 146.25 & direcao_do_vento_10m_graus <= 168.75) ~ 'SSE',
      (direcao_do_vento_10m_graus > 168.75 & direcao_do_vento_10m_graus <= 191.25) ~ 'S',
      (direcao_do_vento_10m_graus > 191.25 & direcao_do_vento_10m_graus <= 213.75) ~ 'SSO',
      (direcao_do_vento_10m_graus > 213.75 & direcao_do_vento_10m_graus <= 236.25) ~ 'SO',
      (direcao_do_vento_10m_graus > 236.25 & direcao_do_vento_10m_graus <= 258.75) ~ 'OSO',
      (direcao_do_vento_10m_graus > 258.75 & direcao_do_vento_10m_graus <= 281.25) ~ 'O',
      (direcao_do_vento_10m_graus > 281.25 & direcao_do_vento_10m_graus <= 303.75) ~ 'ONO',
      (direcao_do_vento_10m_graus > 303.75 & direcao_do_vento_10m_graus <= 326.25) ~ 'NO',
      (direcao_do_vento_10m_graus > 326.25 & direcao_do_vento_10m_graus <= 348.75) ~ 'NNO')) |>
  mutate(label_ponto_cardeal = case_when(
    ponto_cardeal == 'N' ~ 'Norte',
    ponto_cardeal == 'NNE' ~ 'Norte-Nordeste',
    ponto_cardeal == 'NE' ~ 'Nordeste',
    ponto_cardeal == 'LNE' ~ 'Leste-Nordeste',
    ponto_cardeal == 'L' ~ 'Leste',
    ponto_cardeal == 'LSE' ~ 'Leste-Sudeste',
    ponto_cardeal == 'SE' ~ 'Sudeste',
    ponto_cardeal == 'SSE' ~ 'Sul-Sudeste',
    ponto_cardeal == 'S' ~ 'Sul',
    ponto_cardeal == 'SSO' ~ 'Sul-Sudoeste',
    ponto_cardeal == 'SO' ~ 'Sudoeste',
    ponto_cardeal == 'OSO' ~ 'Oeste-Sudoeste',
    ponto_cardeal == 'O' ~ 'Oeste',
    ponto_cardeal == 'ONO' ~ 'Oeste-Noroeste',
    ponto_cardeal == 'NO' ~ 'Noroeste',
    ponto_cardeal == 'NNO' ~ 'Norte-Noroeste'
  )) |>
  distinct(ponto_cardeal, label_ponto_cardeal, velocidade, data_hora)


## Stock Line

### data_hora_timestamp: data_hora em formato de timestamp, para o highstock

stock_line_ita_quandu = ita_quandu |>
  mutate(data_hora_timestamp = as.numeric(data_hora)*1000)


## Range Bar

### mes
### max_temp: temperatura máxima no mês
### min_temp: temperatura mínima no mês

range_temp_ita_quandu = ita_quandu |>
  filter(!is.na(data_hora), !is.na(temperatura_do_ar_a_2m_c))


# Exportando os dados

write.table(wind_rose_ita_quandu, file = "~/2023.2/Projeto Integrador 2/Data/Processed/wind_rose_ita_quandu.csv", sep = ",", na = "", quote = T, row.names = F)
write.table(stock_line_ita_quandu, file = "~/2023.2/Projeto Integrador 2/Data/Processed/stock_line_ita_quandu.csv", sep = ",", na = "", quote = T, row.names = F)
write.table(range_temp_ita_quandu, file = "~/2023.2/Projeto Integrador 2/Data/Processed/range_temp_ita_quandu.csv", sep = ",", na = "", quote = T, row.names = F)
