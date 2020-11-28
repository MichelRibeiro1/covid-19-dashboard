#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library("ggplot2")
library("shiny")
library("plyr")
library("shinydashboard")

options(shiny.port = 8000)

#laod datasets
casos_por_cidade <- read.csv2(
    file = "covid_bahia_casos.csv",
    header = TRUE,
    sep = ";",
    na.strings = NA,
    dec = ",",
)

casos_obitos_br <- read.csv2(
    file = "covid_brasil_casos_obitos.csv",
    header = TRUE,
    sep = ",",
    na.strings = NA,
    dec = ",",
)

# Data manipulation - by cidade
counts <- count(casos_por_cidade, "MUNICIPIO.DE.RESIDENCIA")
sorted_10 <- head(counts[order(counts$freq, decreasing = TRUE), ], 15)

# Data manipulation - by UF
casos_obitos_br <- na.omit(casos_obitos_br)
dataset_br_summarized <- ddply(
    covid_brasil_casos_obitos,
    "estado",
    summarise,
    total_casos = max(casosAcumulado, na.rm = TRUE),
    total_obitos = max(obitosAcumulado, na.rm = TRUE)
)
total_casos_br <- sum(dataset_br_summarized["total_casos"])
total_obitos_br <- sum(dataset_br_summarized["total_obitos"])
taxa_mortalidade <- total_obitos_br / total_casos_br

sorted_casos_br <- dataset_br_summarized[order(dataset_br_summarized$total_casos, decreasing = TRUE),]
sorted_obitos_br <- dataset_br_summarized[order(dataset_br_summarized$total_obitos, decreasing = TRUE),]

ui <- dashboardPage(
    dashboardHeader(title = "COVID-19"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dados Nacionais", tabName = "dados_nacionais", icon = icon("chart-bar")),
            menuItem("Dados Estaduais da Bahia", tabName = "dados_estaduais", icon = icon("chart-bar"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "dados_nacionais",
                fluidRow(
                    infoBox("Total de casos", format(total_casos_br, big.mark = ".", decimal.mark = ","), icon = icon("viruses")),
                    infoBox("Total de óbitos", format(total_obitos_br, big.mark = ".", decimal.mark = ","), icon = icon("heart")),
                    infoBox(
                        "Taxa de Mortalidade",
                        paste(
                            format(taxa_mortalidade * 100, digits = 2, nsmall = 2),
                            "%"
                        ),
                        icon = icon("percent")),
                ),
                fluidRow(
                    box(plotOutput("casosBrasil"), width = 12)
                ),
                fluidRow(
                    box(plotOutput("obitosBrasil"), width = 12)
                )
            ),
            tabItem(
                tabName = "dados_estaduais",
                fluidRow(
                    box(plotOutput("casosBahia"), width = 12)
                ),
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$casosBrasil <- renderPlot({
        bars_br <- barplot(
            sorted_casos_br$total_casos,
            names.arg = sorted_casos_br$estado,
            main = "QTD de casos x UF",
            ylim = c(0, 1.45*max(sorted_casos_br$total_casos)),
            width = 0.85
        )
        text(
            bars_br,
            sorted_casos_br$total_casos,
            format(sorted_casos_br$total_casos, decimal.mark = ",", big.mark = "."),
            pos = 3,
            cex = 1
        )
    })
    
    output$obitosBrasil <- renderPlot({
        bars_obitos_br <- barplot(
            sorted_obitos_br$total_obitos,
            names.arg = sorted_obitos_br$estado,
            main = "QTD de obitos x UF",
            ylim = c(0, 1.45*max(sorted_obitos_br$total_obitos)),
            width = 0.85,
        )
        text(
            bars_obitos_br,
            sorted_obitos_br$total_obitos,
            format(sorted_obitos_br$total_obitos, decimal.mark = ",", big.mark = "."),
            pos = 3,
            cex = 1
        )
    })

    output$casosBahia <- renderPlot({
        bars <- barplot(
            sorted_10$freq,
            names.arg = sorted_10$MUNICIPIO.DE.RESIDENCIA,
            main = "QTD de casos x Municipio",
            width = 0.85,
            ylim = c(0, 1.1*max(sorted_10$freq)),
            cex.names = 0.8
        )
        text(
            bars,
            sorted_10$freq,
            format(sorted_10$freq, decimal.mark = ",", big.mark = "."),
            pos = 3,
            cex = 1
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
