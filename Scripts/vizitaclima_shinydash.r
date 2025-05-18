pacman::p_load(
  shiny,
  shinydashboard,
  highcharter,
  tidyverse
)


ita_quandu = ita_quandu
stock_line_dt = read.csv('~/UFC/Semestres/2023.2/Projeto Integrador 2/Data/Processed/stock_line_ita_quandu.csv')
wind_rose_dt = read.csv('~/UFC/Semestres/2023.2/Projeto Integrador 2/Data/Processed/wind_rose_ita_quandu.csv')
range_temp_dt = read.csv('~/UFC/Semestres/2023.2/Projeto Integrador 2/Data/Processed/range_temp_ita_quandu.csv')


ui = fluidPage(
  
  shinyWidgets::useShinydashboard(),
  
  titlePanel(HTML('<span style="font-size:50pt;"><b>VizIta Clima'), windowTitle = 'VizIta Clima'),
  
  mainPanel(
    width = 14,
    fluidRow(
      column(width=10),
      column(
        width=1,
        div(
          style = "text-align: right; margin-top: 7px;",
          "Período"
        )
      ),
      column(
        width = 1,
        selectInput(
          inputId = 'ano',
          label =  NULL,
          choices = stock_line_dt |>
            drop_na() |>
            mutate(data_hora = as.Date(data_hora),
                   ano = year(data_hora)) |>
            select(ano) |>
            distinct(ano) |>
            pull(ano) |>
            as.character() |>
            c('todos'),
          selected = 'todos'
        )
      )
    ),
    column(width = 8,
           # value boxes
           fluidRow(
             # Value Vento
             valueBoxOutput(outputId = "box_1", width = 3),
             
             # Box Dias Secos
             valueBoxOutput(outputId = "box_2", width = 3),
             
             # Value UV INDEX
             valueBoxOutput(outputId = "box_3", width = 3),
             
             # Value Box 4
             valueBoxOutput(outputId = "box_4", width = 3)),
           
           fluidRow(
             column(width=4, 
                    selectInput(
                      inputId = "var_stock_line",
                      label = NULL,
                      choices = list("Umidade relativa do ar" = "umidade_relativa_do_ar_a_2m",
                                     "Precipitação pluviométrica" = "precipitacao_pluviometrica_mm",
                                     "Pressão atmosférica" = "pressao_atmosferica_h_pa",
                                     "Radiação" = "radiacao_w_m",
                                     "Temperatura do ar" = "temperatura_do_ar_a_2m_c",
                                     "Velocidade média do vento" = "velocidade_media_do_vento_10m_m_s"),
                      selected = "Umidade relativa do ar"
                    )),
             column(width = 8,
                    align='center',
                    valueBoxOutput(outputId = "box_max", width = 3),
                    valueBoxOutput(outputId = "box_min", width = 3),
                    valueBoxOutput(outputId = "box_mean", width = 3),
                    valueBoxOutput(outputId = "box_mode", width = 3))
           ),
             
             # stock chart
             fluidRow(highchartOutput(outputId = "plot_stock_line", height = '500px')
           )),
    
    column(width = 4,
           # wind_rose
           fluidRow(
             highchartOutput(outputId = "plot_wind_rose",
                             height = '350px'),
             br(),
             highchartOutput(outputId = "plot_temp_range",
                             height = '350px')
           ))
  )
)

server = function(input, output, session) {
  get_mode = function(x) {
    tbl = table(x)
    mode_value = as.numeric(names(tbl[tbl == max(tbl)]))
    if (length(mode_value) == length(x)) {
      return("No mode")
    } else {
      return(mode_value)
    }
  }
  
  observe({
    # Calculate and render max, min, mean, and mode for the selected variable
    selected_var = input$var_stock_line
    data = stock_line_dt |>
      filter(if (input$ano != 'todos') grepl(input$ano, data_hora) else TRUE) |>
      filter(!is.na(data_hora), !is.na(!!sym(selected_var))) |>
      mutate(measure = case_when(input$var_stock_line == 'precipitacao_pluviometrica_mm' ~ 'mm',
                                 input$var_stock_line == 'pressao_atmosferica_h_pa' ~ 'h/Pa',
                                 input$var_stock_line == 'radiacao_w_m' ~ 'W/m²',
                                 input$var_stock_line == 'temperatura_do_ar_a_2m_c' ~ 'ºC',
                                 input$var_stock_line == 'umidade_relativa_do_ar_a_2m' ~ '%',
                                 input$var_stock_line == 'velocidade_media_do_vento_10m_m_s' ~ 'm/s'))
    
    # Display mean, max, min, and mode with colored background
    output$box_mean = renderText({
      paste0("<div style='background-color: #F4F5F6; font-size:10pt; padding: 5px; color: black;'>Média: ", round(mean(data[[selected_var]]), 1), unique(data$measure), "</div>")
    })
    
    output$box_max = renderText({
      paste0("<div style='background-color: #F4F5F6; font-size:10pt; padding: 5px; color: black;'>Max: ", round(max(data[[selected_var]]), 1), unique(data$measure), "</div>")
    })
    
    output$box_min = renderText({
      paste0("<div style='background-color: #F4F5F6; font-size:10pt; padding: 5px; color: black;'>Min: ", round(min(data[[selected_var]]), 1), unique(data$measure), "</div>")
    })
    
    output$box_mode = renderText({
      mode = get_mode(data[[selected_var]])
      paste0("<div style='background-color: #F4F5F6; font-size:10pt; padding: 5px; color: black;'>Moda: ", mode, unique(data$measure), "</div>")
    })
    
    output$box_std = renderText({
      paste0("<div style='background-color: #F4F5F6; font-size:10pt; padding: 5px; color: black;'>Dp: ", round(sd(data[[selected_var]]), 1), unique(data$measure), "</div>")
    })
  })


  dias_secos = reactive({
    ita_quandu %>%
      filter(if (input$ano != 'todos') grepl(input$ano, data_hora) else TRUE) %>%
      mutate(data = as.Date(data_hora)) %>%
      group_by(data = lubridate::floor_date(data, "day")) %>%
      summarise(dia_seco = ifelse(all(precipitacao_pluviometrica_mm == 0), 1, 0)) %>% 
      summarise(total_dias_secos = sum(dia_seco))
  })
  
  velocidade_maxima = reactive({
    ita_quandu %>%
      filter(if (input$ano != 'todos') grepl(input$ano, data_hora) else TRUE) |>
      mutate(data_hora = as.Date(data_hora),
             mes = month(data_hora)) %>%
      group_by(mes) %>%
      summarise(velocidade_max_mensal = max(velocidade_media_do_vento_10m_m_s, na.rm = TRUE)) %>%
      summarise(velocidade_max_ano = max(velocidade_max_mensal))
  })
  
  uv_index = reactive({
    ita_quandu %>%
      filter(if (input$ano != 'todos') grepl(input$ano, data_hora) else TRUE) %>%
      mutate(data_hora = as.Date(data_hora),
             mes = month(data_hora),
             uv_index = case_when(
               radiacao_w_m < 200 ~ 1,
               radiacao_w_m < 300 ~ 2,
               radiacao_w_m < 400 ~ 3,
               radiacao_w_m < 500 ~ 4,
               radiacao_w_m < 600 ~ 5,
               radiacao_w_m < 700 ~ 6,
               radiacao_w_m < 800 ~ 7,
               radiacao_w_m < 900 ~ 8,
               radiacao_w_m < 1000 ~ 9,
               radiacao_w_m < 1138 ~ 10
             )) %>%
      select(mes, uv_index) %>%
      distinct() %>%
      summarise(uv_index = max(uv_index, na.rm = TRUE)) %>% 
      summarise(uv_index_ano = max(uv_index))
  })
  
  umidade_media = reactive({
    ita_quandu %>%
      filter(if (input$ano != 'todos') grepl(input$ano, data_hora) else TRUE) %>%
      summarise(media_umidade = mean(umidade_relativa_do_ar_a_2m, na.rm = TRUE))
  })
  
  # Box 1   
  output$box_1 = shinydashboard::renderValueBox({
    valueBox("box1", color = "olive",
             value = paste(velocidade_maxima()$velocidade_max_ano,"m/s"),
             subtitle = "Velocidade Máx. do Vento",
             icon = icon("wind")
    )
  })
  
  # Box 2
  output$box_2 = renderValueBox({
    valueBox("box2", color = "red",
             value = dias_secos()$total_dias_secos,
             subtitle = "Dias Secos",
             icon = icon("cloud")
    )
  })
  
  # Box 3
  output$box_3 = renderValueBox({
    valueBox("box3", color = 'yellow',
             value = uv_index()$uv_index_ano,
             subtitle = "Índice UV",
             icon = icon("sun")
    )
  })
  
  # Box 4
  output$box_4 = renderValueBox({
    valueBox(20, "box4", color = "light-blue",
             value = paste0(round(umidade_media()$media_umidade), "%"),
             subtitle = "Umidade Relativa Média",
             icon = icon("tint")
    )
  })
  
  
  # stock line
  output$plot_stock_line = renderHighchart({
    stock_line_dt = stock_line_dt |> 
      filter(if (input$ano != 'todos') grepl(input$ano, data_hora) else TRUE) |>
      filter(!is.na(data_hora), !is.na(!!sym(input$var_stock_line))) |>
      mutate(data_hora = format(as.Date(data_hora), '%d/%m/%Y')) |>
      mutate(measure = case_when(input$var_stock_line == 'precipitacao_pluviometrica_mm' ~ 'mm',
                                 input$var_stock_line == 'pressao_atmosferica_h_pa' ~ 'h/Pa',
                                 input$var_stock_line == 'radiacao_w_m' ~ 'W/m²',
                                 input$var_stock_line == 'temperatura_do_ar_a_2m_c' ~ 'ºC',
                                 input$var_stock_line == 'umidade_relativa_do_ar_a_2m' ~ '%',
                                 input$var_stock_line == 'velocidade_media_do_vento_10m_m_s' ~ 'm/s'))
    
    mean_y = mean(stock_line_dt |>
                     pull(!!sym(input$var_stock_line)))
    
    highchart(type = "stock") |>
      hc_xAxis(type = "datetime") |>
      hc_rangeSelector(inputDateFormat = '%d/%m/%Y') |>
      hc_add_series(data = stock_line_dt, 
                    hcaes(x = data_hora_timestamp,
                          y = !!sym(input$var_stock_line)),
                    type='line',
                    tooltip = list(
                      headerFormat = NULL,
                      pointFormat = '<b>{point.options.data_hora}<br><i>{point.y: .2f}{point.options.measure}')
      ) |>
      hc_colors('#3C8DBC')
  })
  
  # wind rose
  output$plot_wind_rose = renderHighchart({
    wind_rose_dt |>
      filter(if (input$ano != 'todos') grepl(input$ano, data_hora) else TRUE) |>
      group_by(ponto_cardeal) |>
      mutate(frequencia = n()) |>
      distinct(ponto_cardeal, frequencia) |>
      hchart(type = 'column', hcaes(x = ponto_cardeal, y = frequencia)) |>
      hc_chart(polar = TRUE) |>
      hc_plotOptions(column = list(groupPadding = 0,
                                   pointPlacement = 'on')) |>
      hc_xAxis(title=list(text=''),
               categories = c('N', 'NNE', 'NE', 'LNE', 'L', 'LSE', 'SE', 'SSE', 'S', 'SSO', 'SO', 'OSO', 'O', 'ONO', 'NO', 'NNO'),
               tickmarkPlacement='on') |>
      hc_yAxis(title=list(text=''), endOnTick=F) |>
      hc_colors(c('#3D9970')) |>
      hc_add_theme(hc_theme_gridlight()) |>
      hc_tooltip(
        headerFormat = NULL,
        pointFormat = '</b><br>Freq: {point.y}',
        shared = TRUE
      )
  })
  
  # temperature range
  output$plot_temp_range = renderHighchart({
    range_temp_dt = range_temp_dt |>
      filter(if (input$ano != 'todos') grepl(input$ano, data_hora) else TRUE) |>
      mutate(mes = month(data_hora)) |>
      group_by(mes) |>
      mutate(max_temp = round(max(temperatura_do_ar_a_2m_c)),
             min_temp = round(min(temperatura_do_ar_a_2m_c)),
             media_temp = round(mean(temperatura_do_ar_a_2m_c))) |>
      distinct(mes, max_temp, min_temp, media_temp) |>
      arrange(mes)
    
    highchart() |>
      hc_chart(type = "columnrange", inverted = TRUE) |>
      hc_xAxis(categories = c('Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun',
                              'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez')) |>
      hc_yAxis(title = list(text = "Temperatura (°C)")) |>
      hc_tooltip(valueSuffix = "°C") |>
      hc_legend(enabled = FALSE) |>
      hc_plotOptions(columnrange = list(
        borderRadius = "2%", 
        dataLabels = list(enabled = TRUE, format = "{y}°C")
      )) |>
      hc_series(list(
        name = "Temperatura",
        data = lapply(1:12, function(i) list(range_temp_dt$min_temp[i], range_temp_dt$max_temp[i]))
      )) |>
      hc_colors('#DD4B39') |>
      hc_add_series(
        name = "Média",
        type = "scatter",
        data = range_temp_dt$media_temp,
        marker = list(
          symbol = "circle",
          radius = 6,
          fillColor = "white",
          lineWidth = .9,
          lineColor = "#DD4B39"
        ),
        tooltip = list(
          headerFormat = "",
          pointFormat = "<b>Média:</b> {point.y}"
        )
      )
  })
}

shinyApp(ui, server)
