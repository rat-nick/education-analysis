## app.R ##
library(shinydashboard)
library(shiny)
library(ggridges)
library(plotly)
source("../scripts/r/data_import.R")
source("../scripts/r/wrangling.R")
# Grade data ui ----------
ui <- dashboardPage(
    dashboardHeader(title = "EduBoard"),
    dashboardSidebar(sidebarMenu(
        menuItem(
            text = "Uspeh ucenika",
            tabName = "uspeh",
            icon = icon("clipboard")
        ),
        menuItem(
            text = "Izostanci ucenika",
            tabName = "izostanci",
            icon = icon("clipboard")
        )
    )),
    
    dashboardBody(tabItems(
        tabItem(tabName = "uspeh",
                fluidRow(box(
                    selectInput(
                        inputId = "uspeh_godine",
                        choices = NULL,
                        multiple = T,
                        label = "Izaberite razrede"
                    )
                ),
                box(
                    selectInput(
                        inputId = "uspeh_predmeti",
                        choices = NULL,
                        multiple = T,
                        label = "Izaberite predmete"
                    )
                )),
                fluidRow(box(
                    plotlyOutput("uspehCompHist")
                ),
                box(
                    plotOutput("uspehPoPredmetima")
                ))),
        tabItem(tabName = 'izostanci',
                fluidRow(box(
                    selectInput(
                        inputId = "izostanci_godine",
                        choices = NULL,
                        multiple = T,
                        label = "Izaberite razrede"
                    )
                ),
                box(
                    selectInput(
                        inputId = "izostanci_predmeti",
                        choices = NULL,
                        multiple = T,
                        label = "Izaberite predmete"
                    )
                )))
    ))
)


server <- function(input, output, session) {
    data <- read.csv("../data/all_grades.csv")
    rv <- reactiveValues()
    rv$data <- data
    
    filtered <- reactive({
        df <- rv$data
        print(input$uspeh_godine)
        print(input$uspeh_predmeti)
        if (!is.null(input$uspeh_godine)) {
            df <- df %>% filter(godina %in% input$uspeh_godine)
        }
        if (!is.null(input$uspeh_predmeti)) {
            df <- df %>% filter(predmet %in% input$uspeh_predmeti)
        }
        return(df)
        
    })
    
    observe({
        print("data change")
        ch <- unique(isolate(filtered())['godina'])
        updateSelectInput(session, "uspeh_godine", choices = ch)
    })
    
    observe({
        print("data change")
        ch <- unique(isolate(filtered())['predmet'])
        updateSelectInput(session, "uspeh_predmeti", choices = ch)
    })
    
    output$uspehCompHist <- renderPlotly({
        df <- filtered() %>% mutate(pandemija = if_else(pandemija, "tokom pandemije", "pre pandemije"))
        
        df %>% ggplot(aes(x = ocena, fill=pandemija)) + ggtitle("Raspodela ocena") +
            geom_histogram(
                data = ~ subset(., pandemija == "tokom pandemije"),
                aes(y=1*..count..),
                alpha = 0.6,
                bins = 5
            ) +
            geom_histogram(
                data = ~ subset(., pandemija == "pre pandemije"),
                aes(y=-1*..count..),
                alpha = 0.8,
                bins = 5
            ) +
            theme_minimal() +
            theme(legend.position = "bottom")+
            xlab("ocena") + ylab("broj ocena")
    })
    
    output$uspehPoPredmetima <- renderPlotly({
        df <- filtered() %>%
            group_by(godina, predmet) %>%
            summarise(avg = mean(ocena))
        
        df %>%
            ggplot(aes(x = predmet, y = avg)) +
            geom_bar(aes(color = godina), alpha = .3) + coord_polar()
    })
    
}
shinyApp(ui, server)