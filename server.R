# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  ## reações auxiliares----
  
  # reactive que retorna lista de municípios da UF escolhida
  minInput <- reactive({
    switch(input$uf,subset(dtb_viz,sigla_uf == input$uf)$nome_mun)
  })
  
  # 'observe' que atualiza lista de input municípios de com base na secolha da UF
  observe({
    updateSelectInput(session,"mun",
                      label = "Municipio",
                      choices = unique(minInput()))
  })
  
  # reactive que retorna o código do município selecionado
  mun_sel <- reactive({
    ## código do município selecionado
    codmun_sel  <- dtb_viz[sigla_uf == input$uf
                             & nome_mun == input$mun,unique(codigo_mun7)]
    
    switch(input$mun,codmun_sel)
  })
  
  # reactive que retorna nomes e vizinhan
  
  # # reactive que: retorna objeto leaflet de shape filtrado nos vizinhos
  # filtrar_leaflet <- reactive({
  #   
  # })
  
  ## outputs ----
  
  output$printar <- renderPrint({
    mun_sel()
    
  })
  
  output$file_rmd <- renderUI({
    codmun_sel <- mun_sel() %>% as.integer()
    HTML(markdownToHTML(render("relatorio_municipio.Rmd", 
                               params = list(
                                 cod_munsel = codmun_sel
                                 ),
                               encoding = "utf8"
                               ),
                        encoding = "utf8"
                        )
         )
    })
  })
