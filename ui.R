title <- "Probabilidade Básica I"

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = "tabName"
  mi
}

ui <- fluidPage(
  dashboardPage(
    # Header ####
    dashboardHeader(title = title, titleWidth = 350),
    # Side Bar ####
    dashboardSidebar(width = 350,
                     sidebarMenu(id = "sidebar_menu",
                                 menuItem("Ajuda", tabName = "esc"),
                                 convertMenuItem(
                                   menuItem("Visualização de variáveis aleatórias",tabName="viz",
                                            startExpanded = F, 
                                            selectInput("var", "Variável Aleatória",
                                                        choices = distr, 
                                                        selected = distr[1], multiple = FALSE,
                                                        selectize=FALSE, width = '98%'),
                                            submitButton("Atualizar", icon("refresh")),
                                            br()),"viz"),
                                 convertMenuItem(
                                   menuItem("Transformações de variáveis aleatórias",tabName="tran",
                                            startExpanded = F,
                                            selectInput("var1", "Variável Aleatória",
                                                        choices = distr1, 
                                                        selected = distr1[1], multiple = FALSE,
                                                        selectize=FALSE, width = '98%'),
                                            selectInput("tran1", "Transformação",
                                                        choices = "", 
                                                        selected = NULL, multiple = FALSE,
                                                        selectize=FALSE, width = '98%'),
                                            submitButton("Atualizar", icon("refresh")),
                                            br()),"tran"),
                                 convertMenuItem(
                                   menuItem("Calculadora",tabName="prob",
                                            startExpanded = F,
                                            selectInput("prob1", "Variável Aleatória",
                                                        choices = c("Normal","t-Student","F-Snedecor"), 
                                                        selected = NULL, multiple = FALSE,
                                                        selectize=FALSE, width = '98%'),
                                            selectInput("escol","cauda",
                                                        choices = c("Direita","Esquerda","Bilateral","Entre os quantis"), selected = "dir"),
                                            submitButton("Atualizar", icon("refresh")),
                                            br()),"prob"),
                                 menuItem("Aluno: Wesley Almeida Cruz", icon = icon("glasses"), tabName = "about"),
                                 menuItem("Orientador: Francisco Moisés Cândido de Medeiros", icon = icon("chalkboard-teacher"), tabName = "about1")
                                 
                     )
    ),
    # Body ####
    dashboardBody(
      tags$head(
        tags$style(HTML(
          '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    ')),
        
        tags$link(rel = "stylesheet", type = "text/css", href = "custom123.css")),
      tags$script(HTML('
        $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Aplicativo para o ensino de Probabilidade Básica I </span>\');
        })
        
        ')),
      tabItems(
        tabItem(tabName = "esc",
                box(
                  helpText("●", HTML("<b>Visualização de variáveis aleatórias:</b>"), "Nessa seção o usuário 
                      poderá visualizar o histograma, gráfico de
                       densidade de probabilidade (ou função de probabilidade)
                       e gráfico da função de distribuição acumulada das variáveis aleatórias
                       mais estudas na disciplina de probabilidade básica I
                       ministrada na UFRN."),
                  helpText("●", HTML("<b>Transformação de variáveis aleatórias:</b>"), "Nessa seção o usuário
                       podera visualizar o comportamento de algumas variáveis aleatórias
                       quando são aplicadas algumas transformações na variável original, será
                       apresentado um breve resumo das possíveis transformações consideradas para cada
                       variável e dois gráficos de histograma de uma amostra de tamanho 10.000 da 
                       v.a. de referência e transformada e a densidade da variável
                       aleatória escolhida."),
                  helpText("●", HTML("<b>Calculadora de probabilidade:</b>"), "Nessa seção
                       o usuário poderá visualizar a área abaixo da curva da função de
                       densidade de probabilidade das distribuições Normal, t-Student
                       e F-Snedecor, bem como o cálculo da probabilidade dado uma certa
                       combinação de quantis para o caso unilateral, bilateral ou entre
                       os quantis."),
                  helpText("Para uma melhor uma melhor visualização clique no ícone   ", HTML('<i class="fas fa-bars" style = "color:#242323;"></i>')), width = 20)),
        tabItem(tabName = "tran",
                column(12,
                       splitLayout(uiOutput("vizu1"),
                                   htmlOutput("txt1"),
                                   cellWidths = c("50%","50%"))
                ),
                column(12,
                       plotOutput("plot3")
                       
                )
        ),
        tabItem(tabName = "viz",
                column(6,
                       uiOutput("escl"),
                       uiOutput("vizu"),
                       uiOutput("aaa"),
                       tabBox(tabPanel("Histograma",plotOutput("plot"), status = "primary"),
                              tabPanel("Densidade",plotOutput("plot1"), status = "primary"),
                              tabPanel("Acumulada",plotOutput("plot2"), status = "primary"),
                              width = 20)
                ),
                column(6,
                       splitLayout(
                         box("Medida", background = "black", width = 20),
                         box("Amostral", background = "black", width = 20),
                         box("Teórica", background = "black", width = 20),
                         cellWidths = c("40%","30%","30%"))),
                br(),
                column(6,
                       splitLayout(
                         box("Média", background = "red", width = 20),
                         box(textOutput("tex"), background = "red", width = 20),
                         box(textOutput("texa"), background = "red", width = 20),
                         cellWidths = c("40%","30%","30%"))),
                column(6,
                       splitLayout(
                         box("Mediana", background = "blue", width = 20),
                         box(textOutput("tex2"), background = "blue", width = 20),
                         box(textOutput("texb"), background = "blue", width = 20),
                         cellWidths = c("40%","30%","30%"))),
                column(6,
                       splitLayout(
                         box("Moda", background = "green", width = 20),
                         box(textOutput("tex4"), background = "green", width = 20),
                         box(textOutput("texe"), background = "green", width = 20),
                         cellWidths = c("40%","30%","30%"))),
                column(6,
                       splitLayout(
                         box("Variância", background = "navy", width = 20),
                         box(textOutput("tex1"), background = "navy", width = 20),
                         box(textOutput("texc"), background = "navy", width = 20),
                         cellWidths = c("40%","30%","30%"))),
                column(6,
                       splitLayout(
                         box("Curtose", background = "navy", width = 20),
                         box(textOutput("tex3"), background = "navy", width = 20),
                         box(textOutput("texd"), background = "navy", width = 20),
                         cellWidths = c("40%","30%","30%"))),
                column(6,
                       splitLayout(
                         box("Assímetria", background = "navy", width = 20),
                         box(textOutput("tex5"), background = "navy", width = 20),
                         box(textOutput("texf"), background = "navy", width = 20),
                         cellWidths = c("40%","30%","30%")))
        ),
        tabItem(tabName = "prob",
                column(12,
                       splitLayout(plotOutput("plot4"),
                                   uiOutput("vizu2"),
                                   cellWidths = c("60%","40%"))
                )
        ))
    )
  )
)