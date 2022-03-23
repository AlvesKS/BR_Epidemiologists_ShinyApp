library(shiny)
library(tidyr)
library(magrittr)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(leaflet)
library(googlesheets4)
library(gsheet)
library(dplyr)
library(DT)


options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "kaiquedsalves@gmail.com"
)
# run sheets auth
gs4_auth(use_oob = TRUE)




# estados = read.csv("estados.csv", encoding = "UTF-8")
estados =  gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1XXrplXvzRhu_l6sNTerGUV7VtzcQyyDh81k03cvj1EE/edit#gid=270297697")
# municipios = read.csv("municipios.csv", encoding = "UTF-8")
municipios = gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1XXrplXvzRhu_l6sNTerGUV7VtzcQyyDh81k03cvj1EE/edit#gid=1781018116")

lista_cargos = c("Estudante graduação",
                 "Estudante pós-graduação: Mestrado",
                 "Estudante pós-graduação: Doutorado",
                 "Post-doc",
                 "Docente em universidade",
                 "Pesquisador empresa publica",
                 "Pesquisador empresa privada", "Autônomo")


# loadData <- function() {
#   # files <- list.files(file.path(responsesDir), full.names = TRUE)
#   # data <- lapply(files, read.csv, stringsAsFactors = FALSE)
#   # data <- do.call(rbind, data)
#   
#   data = read_sheet('https://docs.google.com/spreadsheets/d/1XXrplXvzRhu_l6sNTerGUV7VtzcQyyDh81k03cvj1EE/edit#gid=0')
#   
#   data
# }


fieldsMandatory <- c("name", "org","email","tema","org", "cargo","state", "municip")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }
   #error { color: red; }"


fieldsAll <- c("name", "org", "email", "cargo","state", "municip")
responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

  ui = dashboardPage(skin = "green",
                     
                     
                     dashboardHeader(title = "EpidemioBR",
                                     titleWidth = 250),
                     
                     dashboardSidebar(width = 250,
                                      sidebarMenu(
                                        menuItem("Onde estamos?" , tabName = "map_view", icon = icon("map")),
                                        menuItem("Faça parte", tabName = "form1", icon = icon("file-signature"))
                                        
                                        )
                                      ),
                     
                     dashboardBody(shinyjs::useShinyjs(),
                                   shinyjs::inlineCSS(appCSS),
                        tabItems(
                          
                          tabItem(tabName = "map_view",
                                  div(id ="map_table",
                                  tabBox(title = "",
                                         width = 12,
                                         height = 700,
                                         tabPanel("Mapa",
                                                  leafletOutput("map_people",
                                                                width = "100%",
                                                                height = 700)
                                                  ),
                                         
                                         tabPanel("Lista completa",
                                                  DT::dataTableOutput("responsesTable")
                                                  )
                                         )
                                    )
                                  ),
                          
                          
                          
                          tabItem(tabName = "form1",
                                  div(id = "form",
                                      
                                  box(title = "Formulário de cadastro",
                                      p("Caso ainda não tenha cadastro no diretório Epidemio BR, preecha o formulario
                                        abaixo. Caso deseje alterar seu cadastro, entre em contato por email em",
                                        a("kaique.alves@ufv.br", href = "mailto:kaique.alves@ufv.br"), 
                                        ". Caso esteja atuando no exterior, nos encaminhe as informações abaixo com país estado e local de atuação"),
                                      textInput(inputId = "name",
                                                label = labelMandatory("Nome completo"),
                                                value = ""),
                                      
                                      textInput(inputId = "email",
                                                label = labelMandatory("Email"),
                                                value = ""),
                                      
                                      textInput(inputId = "org",
                                                label = labelMandatory("Sua instituição ou empresa (evite siglas)"),
                                                value = ""),
                                      
                                      textInput(inputId = "tema",
                                                label = labelMandatory("Área(s) de atuação"),
                                                value = ""),
                                      
                                      pickerInput(inputId = "cargo",
                                                  label = labelMandatory("Cargo/função"),
                                                  choices = c("",lista_cargos),
                                                  options = list(
                                                    `actions-box` = TRUE, 
                                                    size = 10,
                                                    `selected-text-format` = "count > 3"
                                                  ),
                                                  multiple = F),
                                      
                                      pickerInput(inputId = "state",
                                                  label = labelMandatory("Estado (trabalho)"),
                                                  choices = c("",sort(estados$nome)),
                                                  multiple = F),
                                      
                                      selectInput(inputId = "municip",
                                                  label = labelMandatory("Município (trabalho)"),
                                                  choices = "",
                                                  multiple = F),
                                      
                                      actionButton(inputId = "submit",
                                                   label = "Enviar",
                                                   class = "btn-primary"),
                                      
                                      shinyjs::hidden(
                                        span(id = "submit_msg", "Submitting..."),
                                        div(id = "error",
                                            div(br(), tags$b("Error: "), span(id = "error_msg"))
                                        )
                                      )
                                      
                                      
                                      )),
                                  # div(id = "form"),
                                  
                                  shinyjs::hidden(
                                    div(
                                      id = "thankyou_msg",
                                      h3("Seu cadastro foi realizado com sucesso"),
                                      p("Atualize a página para atualizar o mapa e a lista completa de membros"),
                                      p("Se desejar informar a localização mais precisa da sua instituição, nos encaminhe  as coordenadas aqui ",
                                        a("kaique.alves@ufv.br", href = "mailto:kaique.alves@ufv.br"),", que adicionaremos ao seu cadastro"),
                                      actionLink("submit_another", "Fazer outro cadastro")
                                    )
                                  )
                                  
                                  
                                  
                                  
                                  )
                        )
                       
                     )
                     
                     
                     
                     )
  
 
  
  server = function(input, output, session) {
    

    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)

      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })




    observe({
      x = input$state

      if(x ==""){
        mlist = data.frame(nome = "", nada = "")
      }else{


      estado_selected= estados %>%
        filter(nome == x)

      mlist = municipios %>%
        filter(codigo_uf == estado_selected$codigo_uf)
      }
      updateSelectInput(session,
                        inputId = "municip",
                        # label = labelMandatory("Município"),
                        choices = c("",sort(mlist$nome)))
    })









    saveData <- function(data) {
      # fileName <- sprintf("%s_%s.csv",
      #                     humanTime(),
      #                     digest::digest(data))

      # write.csv(x = data, file = file.path(responsesDir, fileName),
      #           row.names = FALSE, quote = TRUE)
      sheet_append("https://docs.google.com/spreadsheets/d/1XXrplXvzRhu_l6sNTerGUV7VtzcQyyDh81k03cvj1EE/edit#gid=0",
                   data = data)

    }

    # action to take when submit button is pressed
    observeEvent(input$submit, {
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")


      x = input$state
      estado_selected= estados %>%
        filter(nome == x)

      data_muni = municipios %>%
        filter(codigo_uf == estado_selected$codigo_uf) %>%
        filter(nome == input$municip)

      dataa = data.frame(name = input$name,
                        org = input$org,
                        email= input$email,
                        cargo= input$cargo,
                        tema = input$tema,
                        state= input$state,
                        municip= input$municip,
                        timestamp= epochTime(),
                        latitude = data_muni$latitude,
                        longitude = data_muni$longitude)

      tryCatch({
      # saveData(formData())
      sheet_append("https://docs.google.com/spreadsheets/d/1XXrplXvzRhu_l6sNTerGUV7VtzcQyyDh81k03cvj1EE/edit#gid=0",
                   data = dataa,
                   sheet  = "people")

      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })

    })


    
    
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    
    
    

output$responsesTable <- DT::renderDataTable({
  data = gsheet2tbl('https://docs.google.com/spreadsheets/d/1XXrplXvzRhu_l6sNTerGUV7VtzcQyyDh81k03cvj1EE/edit#gid=0')

  data %>%
    mutate(Nome = name,
           Instituicao = org,
           Estado = state,
           Tema = tema,
           Local = municip,
           `Cargo` = cargo,
           Email =email) %>%
    dplyr:: select(Nome, Instituicao, Estado, Local, Tema, `Cargo`, Email)},
  rownames = FALSE,
  options = list(searching = FALSE, lengthChange = FALSE)
)





output$map_people = renderLeaflet({

  data = gsheet2tbl('https://docs.google.com/spreadsheets/d/1XXrplXvzRhu_l6sNTerGUV7VtzcQyyDh81k03cvj1EE/edit#gid=0')

  data_base = data %>%
    mutate(Nome = name,
           Instituicao = org,
           # tema = tema
           Estado = state,
           Local = municip,
           `Cargo` = cargo) %>%
    
    unite("latlong", latitude, longitude, sep ="/", remove = F ) %>%
    group_by(latlong) %>%
    mutate(n = n(),
           latitude = as.numeric(latitude),
           longitude = as.numeric(longitude),
           latitude = case_when(n > 1 ~ rnorm(n, latitude, 0.01),
                                n == 1 ~ latitude),
           longitude = case_when(n > 1 ~ rnorm(n, longitude, 0.01),
                                 n == 1 ~ longitude))

  leaflet(data = data_base, width = "100%") %>%
         setView(-50, -15, zoom = 4) %>%
         addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
         addProviderTiles("OpenTopoMap", group = "Terrain") %>%
         addScaleBar("bottomright") %>%
         addProviderTiles(providers$CartoDB.Voyager, group = "Default") %>%
         addLayersControl(
               baseGroups = c("Aerial","Default","Terrain"),
               overlayGroups = "EpidemioBR",
               options = layersControlOptions(collapsed = T)) %>%
    addCircleMarkers(radius = 6,
                     fillOpacity = 1,
                     weight = 0.5,
                     lat = ~latitude,
                     lng = ~longitude,
                     label = paste(data_base$name),
                     popup = paste(data_base$name,"<br>",
                                   "Cargo:", data_base$cargo,"<br>",
                                   "Instituição:", data_base$org,"<br>",
                                   "Tema:", data_base$tema,"<br>",
                                   "Contato:",data_base$email ))
})
    
  }
 
  
   shinyApp(ui, server)


  # rsconnect::deployApp(
  #  appFiles = c("app.R", ".secrets;bb8565617b5e9996ff93e6d214ba7218_kaiquedsalves@gmail"))
   
   
   
