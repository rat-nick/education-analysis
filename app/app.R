## app.R ##
library(shinydashboard)
library(shiny)
library(ggridges)
library(plotly)
source("../scripts/r/data_import.R")
source("../scripts/r/wrangling.R")

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
                    sliderInput("numSubjects", "Broj predmeta", min = 1, max = 10, value = 3),
                    checkboxInput("poslednji", "Najmanja promena"),
                    plotlyOutput("najpogodjenijiPredmeti")
                ))),
        tabItem(
            tabName = 'izostanci',
            fluidRow(dataTableOutput("studentAttendanceDataTable")),
            fluidRow(dataTableOutput("classAttendanceDataTable"))
        )
    ))
)


server <- function(input, output, session) {
    data <- read.csv("../data/all_grades.csv")
    class_attendance_data <-
        read.csv("../data/summary_attendance.csv")
    
    rv <- reactiveValues()
    rv$data <- data
    rv$ca_data <- class_attendance_data
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
        df <-
            filtered() %>% mutate(pandemija = if_else(pandemija, "tokom pandemije", "pre pandemije"))
        
        df %>% ggplot(aes(x = ocena, fill = pandemija)) + ggtitle("Raspodela ocena") +
            geom_histogram(
                data = ~ subset(., pandemija == "tokom pandemije"),
                aes(y = 1 * ..count..),
                alpha = 0.6,
                bins = 5
            ) +
            geom_histogram(
                data = ~ subset(., pandemija == "pre pandemije"),
                aes(y = -1 * ..count..),
                alpha = 0.8,
                bins = 5
            ) +
            theme_minimal() +
            theme(legend.position = "bottom") +
            xlab("ocena") + ylab("broj ocena")
    })
    
    output$najpogodjenijiPredmeti <- renderPlotly({
        no_fac <- rv$data %>% filter(!grepl("факулта", predmet))
        dT <- no_fac %>% filter(pandemija == T) %>% group_by(predmet) %>% summarise(prosek = mean(ocena))
        dF <- no_fac %>% filter(pandemija == F) %>% group_by(predmet) %>% summarise(prosek = mean(ocena))
        
        multi <- if_else(input$poslednji, 1, -1)
        
        df <- merge(x = dF, y = dT, by = "predmet") %>% 
            mutate(promena = prosek.y - prosek.x)
        df <- head(df[order(multi*df$promena), ], input$numSubjects)
        
        df %>% ggplot(aes(x=reorder(predmet, promena), y=promena)) + 
            geom_col(aes(stat = , fill=predmet)) + coord_flip()
        
        
    })
    
    output$studentAttendanceDataTable <- renderDataTable({
        cols <- c(1, 2, 3, 4, 6, 8, 12, 13, 14)
        
        rv$ca_data[, cols]
    })
    
    output$classAttendanceDataTable <- renderDataTable({
        cols <- c(1, 5, 7, 9, 10, 11, 12, 13, 14)
        
        rv$ca_data[, cols]
    })
}
shinyApp(ui, server)