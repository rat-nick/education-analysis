## app.R ##
library(shinydashboard)
library(shiny)
library(ggridges)
library(plotly)
library(ggcorrplot)
library(stringr)
library(ggthemes)
source("scripts/data_import.R")
source("scripts/wrangling.R")
# UI definition ----
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "EduBoard"),
    # Sidebar definitions -------------------------
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
    # Dashboard body definition ----------------
    dashboardBody(tabItems(
        # Uspeh tab ----------------------
        tabItem(tabName = "uspeh",
                fluidRow(
                    box(
                        width = 12,
                        collapsible = T,
                        title = "Podaci po predmetima",
                        fluidRow(
                            box(
                                collapsible = T,
                                selectInput(
                                    inputId = "uspeh_godine",
                                    choices = NULL,
                                    multiple = T,
                                    label = "Izaberite razrede"
                                )
                            ),
                            box(
                                collapsible = T,
                                selectInput(
                                    inputId = "uspeh_predmeti",
                                    choices = NULL,
                                    multiple = T,
                                    label = "Izaberite predmete"
                                )
                            )
                        ),
                        fluidRow(
                            box(
                                collapsible = T,
                                width = 6,
                                plotlyOutput("uspehCompHist")
                            ),
                            box(
                                collapsible = T,
                                width = 6,
                                plotlyOutput("corrPlot")
                            )
                        ),
                        box(
                            collapsible = T,
                            checkboxInput("poslednji", "Najmanja promena"),
                            plotlyOutput("najpogodjenijiPredmeti"),
                            sliderInput(
                                "numSubjects",
                                "Broj predmeta",
                                min = 1,
                                max = 10,
                                value = 3
                            )
                        )
                    )
                )),
        # Izostanci tab -------------------------------
        tabItem(tabName = 'izostanci',
                fluidRow(
                    box(
                        width = 12,
                        dataTableOutput("studentAttendanceDataTable"),
                        collapsible = T
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        column(
                            width = 4,
                            selectInput("attendanceColSelect", "Izaberite statistiku", choices = NULL)
                        ),
                        column(
                            width = 4,
                            selectInput(
                                "period",
                                "Izaberite period",
                                choices = c("I polugodište", "II polugodište", "ukupno"),
                                selected = "ukupno"
                            )
                        ),
                        fluidRow(column(
                            width = 10,
                            plotlyOutput("saDensity", height = 700),
                        ), column(width = 2, textOutput("avgDiff"))),
                        sliderInput(
                            'saNumBins',
                            min = 5,
                            max = 30,
                            label = "Broj grupa",
                            value = 10
                        )
                        
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        dataTableOutput("classAttendanceDataTable"),
                        collapsible = T
                    )
                ))
    ))
)



# server definition ------
server <- function(input, output, session) {
    data <- read.csv("data/all_grades.csv")
    class_attendance_data <-
        read.csv("data/summary_attendance.csv")
    
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
        dT <-
            no_fac %>% filter(pandemija == T) %>% group_by(predmet) %>% summarise(prosek = mean(ocena))
        dF <-
            no_fac %>% filter(pandemija == F) %>% group_by(predmet) %>% summarise(prosek = mean(ocena))
        
        multi <- if_else(input$poslednji, 1, -1)
        
        df <- merge(x = dF, y = dT, by = "predmet") %>%
            mutate(promena = prosek.y - prosek.x)
        df <-
            head(df[order(multi * df$promena),], input$numSubjects)
        
        df %>% ggplot(aes(x = reorder(predmet, promena), y = promena)) +
            ggtitle("Promena proseka od pocetka pandemije") +
            geom_col(aes(stat = , fill = predmet), alpha = .7) +
            coord_flip() + theme_minimal() +
            xlab("predmet") + theme(legend.position = "bottom")
        
        
    })
    
    output$studentAttendanceDataTable <- renderDataTable({
        cols <- c(1, 2, 3, 4, 6, 8, 12, 13, 14)
        
        rv$ca_data[, cols]
    })
    
    output$classAttendanceDataTable <- renderDataTable({
        cols <- c(1, 5, 7, 9, 10, 11, 12, 13, 14)
        
        rv$ca_data[, cols]
    })
    
    output$corrPlot <- renderPlotly({
        if (length(input$uspeh_predmeti) == 1) {
            ggplot() + ggtitle("Izaberite vise od jednog predemet da biste videli grafik korelacije")
            
        }
        else{
            df <- filtered() %>%
                group_by(ucenik, predmet) %>%
                summarise(prosek = mean(ocena)) %>%
                pivot_wider(names_from = "predmet", values_from = "prosek") %>%
                data.frame() %>% select(-c("ucenik"))
            
            
            corr <- cor(df, use = "pairwise.complete.obs")
            ggcorrplot(
                corr,
                type = "lower",
                outline.color = 'white',
                show.legend = T
            ) +
                ggtitle("Korelacija proseka ocena") +
                theme(axis.text.x = element_blank(),
                      axis.ticks = element_blank())
        }
    })
    
    observe({
        updateSelectInput(
            session = session,
            inputId = "attendanceColSelect",
            choices = names(isolate(rv$ca_data))[2:11]
        )
    })
    
    selected <- reactive({
        df <- rv$ca_data %>% select(c(input$attendanceColSelect, 1, 12, 14))
        df <- as.tibble(df)
        print(head(df))
    })
    
    output$saDensity <- renderPlotly({
        selectedAxis <- input$attendanceColSelect
        df <-
            rv$ca_data %>% mutate(
                polugodiste = if_else(
                    polugodiste == 1,
                    "I polugodište",
                    if_else(polugodiste == 2, "II polugodište", "ukupno")
                ),
                pandemija = if_else(pandemija == T, "Tokom pandemije", "Pre pandemije")
            ) %>%
            filter(polugodiste %in% input$period)
        
        srednja_vrednost_tokom_pandemije <-
            mean(df[df$pandemija == "Tokom pandemije", selectedAxis])
        srednja_vrednost_pre_pandemije <-
            mean(df[df$pandemija == "Pre pandemije", selectedAxis])
        
        promena_srednje_vrednosti <-
            srednja_vrednost_tokom_pandemije - srednja_vrednost_pre_pandemije
        
        
        base <-
            data.frame(df) %>% ggplot(aes_string(x = selectedAxis))
        base +
            geom_vline(
                aes(xintercept = srednja_vrednost_tokom_pandemije),
                color = "blue",
                linetype = "dashed",
                size = 1
            ) +
            geom_histogram(
                data = subset(df, pandemija == "Pre pandemije"),
                aes(y = ..count.., fill = pandemija),
                alpha = .7,
                bins = input$saNumBins,
                position = "identity"
            ) +
            #geom_density(data = subset(df, pandemija == "Pre pandemije"),alpha=.4, aes(y=-1*..count..)) +
            geom_vline(
                aes(xintercept = srednja_vrednost_pre_pandemije),
                color = "red",
                linetype = "dashed",
                size = 1
            ) +
            geom_histogram(
                data = subset(df, pandemija == "Tokom pandemije"),
                aes(y = -1 * ..count.., fill = pandemija) ,
                alpha = .7,
                bins = input$saNumBins,
                position = "identity"
            ) + ylab("Broj odeljenja") +
            ggtitle(label = "Statistika o izostancima po odeljenjima", subtitle = "Koliko odeljenja pripada određenoj grupi?")
    })
}
shinyApp(ui, server)