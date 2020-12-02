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
library("dplyr")
library("shinydashboard")
library("reshape2")
library("stringr")

options(shiny.port = 8000)

#DataFrame UF - label

ufs <- data.frame(
    uf = c("RONDÔNIA","ACRE","AMAZONAS","RORAIMA","PARÁ","AMAPÁ","TOCANTINS","MARANHÃO","PIAUÍ","CEARÁ","RIO GRANDE DO NORTE","PARAÍBA","PERNAMBUCO","ALAGOAS","SERGIPE","BAHIA","MINAS GERAIS","ESPÍRITO SANTO","RIO DE JANEIRO","SÃO PAULO","PARANÁ","SANTA CATARINA","RIO GRANDE DO SUL","MATO GROSSO DO SUL","MATO GROSSO","GOIÁS","DISTRITO FEDERAL"),
    sigla = c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE","AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF")
)
ufs2 <- ufs
row.names(ufs) <- ufs$uf
row.names(ufs2) <- ufs$sigla

#load datasets
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
    sep = ";",
    na.strings = NA,
    dec = ",",
)

dataset_salvador <- read.csv2(
    file = "salvador.csv",
    header = TRUE,
    sep = ",",
    na.strings = NA,
    dec = ",",
)

casos_obitos_br <- na.omit(casos_obitos_br)
casos_obitos_br$uf <- with(casos_obitos_br, ufs[estado, "sigla"])
casos_obitos_br$data <- as.Date(casos_obitos_br$data, format = "%d/%m/%Y")

# Data manipulation - by cidade
counts <- count(casos_por_cidade, MUNICIPIO.DE.RESIDENCIA)
sorted_10 <- head(counts[order(counts$n, decreasing = TRUE), ], 15)

# Data manipulation - by UF

dataset_br_summarized <- summarise(
    group_by(casos_obitos_br, estado, uf),
    total_casos = sum(casosNovos, na.rm = TRUE),
    total_obitos = sum(obitosNovos, na.rm = TRUE)
)

dataset_br_summarized$taxa_mortalidade <- with(dataset_br_summarized, total_obitos / total_casos)

dataset_br_summarized_by_data <- summarise(
    group_by(casos_obitos_br, data),
    total_casos = sum(casosNovos, na.rm = TRUE),
    total_obitos = sum(obitosNovos, na.rm = TRUE)
)

dataset_br_summarized_by_data <- dataset_br_summarized_by_data[order(dataset_br_summarized_by_data$data, decreasing = FALSE), ]
dataset_br_summarized_by_data$cumsum <- cumsum(dataset_br_summarized_by_data$total_casos)

dataset_br_summarized_by_uf_data <- summarise(
    group_by(casos_obitos_br, estado, uf, data),
    total_casos = sum(casosNovos, na.rm = TRUE),
    total_obitos = sum(obitosNovos, na.rm = TRUE)
)

estados <- dataset_br_summarized$uf
total_casos_br <- sum(dataset_br_summarized["total_casos"])
total_obitos_br <- sum(dataset_br_summarized["total_obitos"])
taxa_mortalidade <- total_obitos_br / total_casos_br

sorted_casos_br <- dataset_br_summarized[order(dataset_br_summarized$total_casos, decreasing = TRUE),]
sorted_obitos_br <- dataset_br_summarized[order(dataset_br_summarized$total_obitos, decreasing = TRUE),]

# SALVADOR

na.omit(dataset_salvador)
salvador_pop <- dataset_salvador[, c(1,2)]
row.names(salvador_pop) <- salvador_pop$BAIRRO

salvador_pop$POPULAÇÃO <- as.numeric(salvador_pop$POPULAÇÃO)
na.omit(salvador_pop)
dataset_salvador$POPULAÇÃO <- NULL
melted <- melt(dataset_salvador)
melted$variable <- lapply(melted$variable, function (v) {
    str_replace(v, "X", "")
})
melted$data <- as.Date(as.character(melted$variable), format = "%d.%m")
melted$variable <- NULL
melted$inc <- with(melted, (1000 / salvador_pop[BAIRRO, "POPULAÇÃO"]) * value)
na.omit(melted)
pincipais_bairros <- c("PITUBA", "CAMINHO DAS ÁRVORES", "GRAÇA", "ITAIGARA", "STELLA MARIS", "VALERIA", "PERNAMBUÉS", "LIBERDADE", "FAZENDA GRANDE DO RETIRO", "LOBATO")
pincipais_bairros_dados <- melted[melted$BAIRRO %in% pincipais_bairros, ]

# SALVADOR - FIM

estadosSubItemList <- lapply(
    estados,
    function (estado) {
        menuSubItem(ufs2[estado, "uf"], tabName = estado)
    }
)

ui <- dashboardPage(
    dashboardHeader(title = "COVID-19"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("dashboard")),
            menuItem(
                "Dados Nacionais",
                tabName = "dados_nacionais",
                icon = icon("chart-bar"),
                do.call(tagList, estadosSubItemList)
            ),
            menuItem(
                "Dados Estaduais da Bahia",
                tabName = "dados_estaduais",
                icon = icon("chart-bar"),
                menuSubItem("geral", tabName = "geral"),
                menuSubItem("Salvador", tabName = "salvador")
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "home",
                fluidRow(
                    infoBox("Total de casos", format(total_casos_br, big.mark = ".", decimal.mark = ","), icon = icon("viruses")),
                    infoBox("Total de óbitos", format(total_obitos_br, big.mark = ".", decimal.mark = ","), icon = icon("heart")),
                    infoBox(
                        "Taxa de Mortalidade",
                        paste(
                            format(taxa_mortalidade * 100, digits = 2, nsmall = 2),
                            "%"
                        ),
                        icon = icon("percent")
                    )
                ),
                fluidRow(
                    box(plotOutput("casosBrasil"), width = 12)
                ),
                fluidRow(
                    box(plotOutput("obitosBrasil"), width = 12)
                )
            ),
            tabItem(
                tabName = "geral",
                fluidRow(
                    box(plotOutput("casosBahia"), width = 12)
                )
            ),
            tabItem(tabName = "RO", uiOutput("RO")),
            tabItem(tabName = "AC", uiOutput("AC")),
            tabItem(tabName = "AM", uiOutput("AM")),
            tabItem(tabName = "RR", uiOutput("RR")),
            tabItem(tabName = "PA", uiOutput("PA")),
            tabItem(tabName = "AP", uiOutput("AP")),
            tabItem(tabName = "TO", uiOutput("TO")),
            tabItem(tabName = "MA", uiOutput("MA")),
            tabItem(tabName = "PI", uiOutput("PI")),
            tabItem(tabName = "CE", uiOutput("CE")),
            tabItem(tabName = "RN", uiOutput("RN")),
            tabItem(tabName = "PB", uiOutput("PB")),
            tabItem(tabName = "PE", uiOutput("PE")),
            tabItem(tabName = "AL", uiOutput("AL")),
            tabItem(tabName = "SE", uiOutput("SE")),
            tabItem(tabName = "BA", uiOutput("BA")),
            tabItem(tabName = "MG", uiOutput("MG")),
            tabItem(tabName = "ES", uiOutput("ES")),
            tabItem(tabName = "RJ", uiOutput("RJ")),
            tabItem(tabName = "SP", uiOutput("SP")),
            tabItem(tabName = "PR", uiOutput("PR")),
            tabItem(tabName = "SC", uiOutput("SC")),
            tabItem(tabName = "RS", uiOutput("RS")),
            tabItem(tabName = "MS", uiOutput("MS")),
            tabItem(tabName = "MT", uiOutput("MT")),
            tabItem(tabName = "GO", uiOutput("GO")),
            tabItem(tabName = "DF", uiOutput("DF")),
            tabItem(
                tabName = "salvador",
                fluidRow(
                    infoBox("Total de casos", format(0, big.mark = ".", decimal.mark = ","), icon = icon("viruses")),
                    infoBox("Total de óbitos", format(0, big.mark = ".", decimal.mark = ","), icon = icon("heart")),
                    infoBox(
                        "Taxa de Mortalidade",
                        paste(
                            format(0 * 100, digits = 2, nsmall = 2),
                            "%"
                        ),
                        icon = icon("percent")
                    ),
                    box(tableOutput("bairrosCasos"), width = 4),
                    box(tableOutput("bairrosPopulosos"), width = 4),
                    box(tableOutput("bairrosRecuperados"), width = 4),
                    box(plotOutput("salvador_principais_bairros2"), width = 12),
                    box(plotOutput("salvador_principais_bairros"), width = 12)
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    lapply(estados, function (estado) {
        estadoInfo <- dataset_br_summarized[dataset_br_summarized$uf == estado, ]
        estadoInfoAux <- dataset_br_summarized_by_uf_data[dataset_br_summarized_by_uf_data$uf == estado, ]
        piorDiaCasos <- estadoInfoAux[which.max(estadoInfoAux$total_casos),]
        piorDiaObitos <- estadoInfoAux[which.max(estadoInfoAux$total_obitos),]
        output[[estado]] <- renderUI({
            fluidRow(
                infoBox("Total de casos",
                        subtitle = paste("Recorde: ", piorDiaCasos$total_casos, "casos em ", format(piorDiaCasos$data, "%d/%m/%y")),
                        format(
                            estadoInfo$total_casos,
                            big.mark = ".",
                            decimal.mark = ","
                        ),
                        icon = icon("viruses"),
                ),
                infoBox("Total de óbitos",
                        subtitle = paste("Recorde: ", piorDiaObitos$total_obitos, "casos em ", format(piorDiaObitos$data, "%d/%m/%y")),
                        format(estadoInfo$total_obitos, big.mark = ".", decimal.mark = ","),
                        icon = icon("heart")),
                infoBox(
                    "Taxa de Mortalidade",
                    paste(
                        format(estadoInfo$taxa_mortalidade * 100, digits = 2, nsmall = 2),
                        "%"
                    ),
                    icon = icon("percent")
                ),
                box(
                    renderPlot({
                        dadosBahia <- dataset_br_summarized_by_uf_data[dataset_br_summarized_by_uf_data$uf == estado, ]
                        dadosBahia <- dadosBahia[order(dadosBahia$data, decreasing = FALSE), ]
                        dadosBahia$cumsum <- cumsum(dadosBahia$total_casos)
                        
                        ggplot(dadosBahia, aes(x = data, y = cumsum)) + geom_line()
                    }),
                    width = 12
                )
            )
        })
    })
    
    output$bairrosCasos <- renderTable({
        ranking <- head(dataset_salvador[order(dataset_salvador$X06.09, decreasing = TRUE), c("BAIRRO", "X06.09")], 10)
        ranking$CASOS <- format(ranking$X06.09, decimal.mark = ",", big.mark = ".")
        ranking$X06.09 <- NULL
        ranking
    })
    
    output$bairrosPopulosos <- renderTable({
        df <- head(salvador_pop[order(salvador_pop$POPULAÇÃO, decreasing = TRUE), ], 10)
        
        df <- data.frame(BAIRRO = row.names(df), POPULAÇÃO = format(df$POPULAÇÃO, decimal.mark = ",", big.mark = "."))
        
        df
    })

    output$salvador_principais_bairros2 <- renderPlot({
        ggplot(pincipais_bairros_dados, aes(x = data, y = value, group = factor(BAIRRO))) + geom_line(aes(color = factor(BAIRRO)), size = 2)
    })
    
    output$salvador_principais_bairros2 <- renderPlot({
        ggplot(pincipais_bairros_dados, aes(x = data, y = value, group = factor(BAIRRO))) + geom_line(aes(color = factor(BAIRRO)), size = 2)
    })
    
    output$salvador_principais_bairros <- renderPlot({
        ggplot(pincipais_bairros_dados, aes(x = data, y = inc, group = factor(BAIRRO))) + geom_line(aes(color = factor(BAIRRO)), size = 2)
    })
    
    output$casosBrasil <- renderPlot({
        bars_br <- barplot(
            sorted_casos_br$total_casos,
            names.arg = sorted_casos_br$uf,
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
            names.arg = sorted_obitos_br$uf,
            main = "QTD de obitos x UF",
            ylim = c(0, 1.45*max(sorted_obitos_br$total_obitos)),
            width = 0.85
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
            sorted_10$n,
            names.arg = sorted_10$MUNICIPIO.DE.RESIDENCIA,
            main = "QTD de casos x Municipio",
            width = 0.85,
            ylim = c(0, 1.1*max(sorted_10$n)),
            cex.names = 0.8
        )
        text(
            bars,
            sorted_10$n,
            format(sorted_10$n, decimal.mark = ",", big.mark = "."),
            pos = 3,
            cex = 1
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
