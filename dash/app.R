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
dataset_casos_ba <- read.csv2(
    file = "dataset_casos_ba.csv",
    header = TRUE,
    sep = ";",
    na.strings = NA,
    dec = ",",
)

dataset_obitos_ba <- read.csv2(
    file = "dataset_obitos_ba.csv",
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

dataset_salvador_geral <- read.csv2(
    file = "./dados_gerais_salvador.csv",
    header = TRUE,
    sep = ";",
    na.strings = NA,
    dec = ",",
)

casos_obitos_br <- na.omit(casos_obitos_br)
casos_obitos_br$uf <- with(casos_obitos_br, ufs[estado, "sigla"])
casos_obitos_br$data <- as.Date(casos_obitos_br$data, format = "%d/%m/%Y")

# Data manipulation - BAHIA
dataset_casos_ba <- na.omit(dataset_casos_ba)
dataset_obitos_ba <- na.omit(dataset_obitos_ba)
dataset_casos_ba$RACA.COR <- with(dataset_casos_ba, ifelse(RACA.COR %in% c("IGNORADA", "IGNORADO"), "NÃO INFORMADA", RACA.COR))

counts <- count(dataset_casos_ba, MUNICIPIO.DE.RESIDENCIA)
counts_obitos <- count(dataset_obitos_ba, MUNICIPIO)
sorted_10 <- head(counts[order(counts$n, decreasing = TRUE), ], 12)
sorted_10_obitos <- head(counts_obitos[order(counts_obitos$n, decreasing = TRUE), ], 12)

# fim - BAHIA

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

# UF - FIM

# SALVADOR

dataset_salvador <- na.omit(dataset_salvador)
salvador_pop <- dataset_salvador[, c(1,2)]
row.names(salvador_pop) <- salvador_pop$BAIRRO

total_casos_ssa <- sum(dataset_salvador$X06.09)
total_obitos_ssa <- 2399
tx_mort_ssa <- total_obitos_ssa / total_casos_ssa

salvador_pop$POPULAÇÃO <- as.numeric(salvador_pop$POPULAÇÃO)
salvador_pop <- na.omit(salvador_pop)
dataset_salvador$POPULAÇÃO <- NULL
melted <- melt(dataset_salvador)
melted$variable <- lapply(melted$variable, function (v) {
    str_replace(v, "X", "")
})
melted$data <- as.Date(as.character(melted$variable), format = "%d.%m")
melted$variable <- NULL
melted$inc <- with(melted, (1000 / salvador_pop[BAIRRO, "POPULAÇÃO"]) * value)
melted <- na.omit(melted)
pincipais_bairros <- c("PITUBA", "CAMINHO DAS ÁRVORES", "GRAÇA", "ITAIGARA", "STELLA MARIS", "VALERIA", "PERNAMBUÉS", "LIBERDADE", "FAZENDA GRANDE DO RETIRO", "LOBATO")
pincipais_bairros_dados <- melted[melted$BAIRRO %in% pincipais_bairros, ]
BAIRRO <- factor(pincipais_bairros_dados$BAIRRO)

# SALVADOR - FIM

estadosSubItemList <- lapply(
    estados,
    function (estado) {
        menuSubItem(ufs2[estado, "uf"], tabName = estado)
    }
)

ui <- dashboardPage(
    dashboardHeader(
        title = "COVID-19"
    ),
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
                menuSubItem("Informações Gerais", tabName = "geral"),
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
                        icon = icon("percent"),
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
                    infoBox("Total de casos", format(length(dataset_casos_ba$SEXO), big.mark = ".", decimal.mark = ","), icon = icon("viruses")),
                    infoBox("Total de óbitos", format(length(dataset_obitos_ba$SEXO), big.mark = ".", decimal.mark = ","), icon = icon("heart")),
                    infoBox(
                        "Taxa de Mortalidade",
                        paste(
                            format((length(dataset_obitos_ba$SEXO) / length(dataset_casos_ba$SEXO)) * 100, digits = 2, nsmall = 2),
                            "%"
                        ),
                        icon = icon("percent")
                    ),
                    
                    box(plotOutput("pizzaCasosSexo"), width = 6, title = "CASOS CONFIRMADOS POR SEXO"),
                    box(plotOutput("pizzaObitosSexo"), width = 6, title = "ÓBITOS POR SEXO"),
                    box(plotOutput("pizzaObitosRacaCor"), width = 6, title = "CASOS POR RAÇA/COR AUTODECLARADA"),
                    box(plotOutput("pizzaBaRacaCor"), width = 6, title = "DISTRIBUIÇÃO PERCENTUAL POR COR OU RAÇA - IBGE 2008"),
                    box(plotOutput("casosBahia"), width = 12),
                    box(plotOutput("obitosBahia"), width = 12)
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
                    infoBox("Total de casos", format(total_casos_ssa, big.mark = ".", decimal.mark = ","), icon = icon("viruses")),
                    infoBox("Total de óbitos", format(total_obitos_ssa, big.mark = ".", decimal.mark = ","), icon = icon("heart")),
                    infoBox(
                        "Taxa de Mortalidade",
                        paste(
                            format((total_obitos_ssa / total_casos_ssa) * 100, digits = 2, nsmall = 2),
                            "%"
                        ),
                        icon = icon("percent")
                    ),
                    box(plotOutput("salvadorCasosSexo"), width = 6, align="center", title = "CASOS CONFIRMADOS POR SEXO"),
                    box(plotOutput("salvadorCasosRaca"), width = 6, align="center", title = "CASOS CONFIRMADOS POR RAÇA OU COR"),
                    box(tableOutput("bairrosCasos") , width = 4, align = "center"),
                    box(tableOutput("bairrosCasos2") , width = 4, align = "center"),
                    box(tableOutput("bairrosCasos3") , width = 4, align = "center"),
                    box(plotOutput("salvador_principais_bairros_casos_totais"), width = 12),
                    box(plotOutput("salvador_principais_bairros_inc"), width = 12)
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
                        
                        ggplot(dadosBahia, aes(x = data, y = cumsum)) +
                            geom_line() +
                            ggtitle("NÚMERO DE CASOS CONFIRMADOS AO LONDO DO TEMPO") +
                            labs(y = "NÚMERO DE CASOS", x = "DATA")
                    }),
                    width = 12
                )
            )
        })
    })
    
    aux_casos_ssa <- function (page) {
        ranking <- head(dataset_salvador[order(dataset_salvador$X06.09, decreasing = TRUE), c("BAIRRO", "X06.09")], 29)
        ranking[30, ] <- c("OUTROS", total_casos_ssa - sum(ranking$X06.09))
        ranking[["%"]] <- format((as.numeric(ranking$X06.09) / total_casos_ssa) * 100, digits = 2, nsmall = 2)
        
        ranking$CASOS <- format(ranking$X06.09, decimal.mark = ",", big.mark = ".")
        ranking$X06.09 <- NULL
        ranking$POSIÇÃO <- 1:30
        ranking <- ranking[, c("POSIÇÃO", "BAIRRO", "CASOS", "%")]
        
        limit <- page * 10
        start <- limit - 9
        ranking[start:limit, ]
    }
    
    output$bairrosCasos <- renderTable({
        aux_casos_ssa(1)
    })
    
    output$bairrosCasos2 <- renderTable({
        aux_casos_ssa(2)
    })
    
    output$bairrosCasos3 <- renderTable({
        aux_casos_ssa(3)
    })

    output$salvador_principais_bairros_casos_totais <- renderPlot({
        ggplot(pincipais_bairros_dados, aes(x = data, y = value, group = BAIRRO)) +
            geom_line(aes(color = BAIRRO), size = 2) +
            ggtitle("NÚMERO DE CASOS AO LONGO DO TEMPO POR BAIRRO") +
            labs(y = "NÚMERO DE CASOS", x = "DATA")
    })
    
    output$salvador_principais_bairros_inc <- renderPlot({
        ggplot(pincipais_bairros_dados, aes(x = data, y = inc, group = BAIRRO)) +
            geom_line(aes(color = BAIRRO), size = 2) +
            ggtitle("COEFICIENTE DE INCIDÊNCIA POR MIL HABITANTES AO LONGO DO TEMPO POR BAIRRO") +
            labs(y = "COEFICIENTE DE INCIDÊNCIA", x = "DATA")
    })
    
    output$salvadorCasosSexo <- renderPlot({
        casos_sexo_ssa <- count(dataset_salvador_geral, SEXO)
        casos_sexo_ssa <- casos_sexo_ssa[casos_sexo_ssa$n > 1000, ]
        casos_sexo_ssa$SEXO <- with(casos_sexo_ssa, ifelse(SEXO == "F", "FEMININO", "MASCULINO"))
        casos_sexo_ssa <- casos_sexo_ssa %>% 
            arrange(desc(SEXO)) %>%
            mutate(prop = n / sum(casos_sexo_ssa$n) * 100) %>%
            mutate(ypos = cumsum(prop) - 0.5 * prop )
        
        ggplot(casos_sexo_ssa, aes(x = "", y = prop, fill = SEXO)) +
            geom_bar(stat = "identity", width = 1, color = "white") +
            coord_polar("y", start = 0) +
            theme_void() +
            geom_text(aes(y = ypos, label = str_c(format(prop, digits = 2),"%")), color = "white", size = 6) +
            scale_fill_brewer(palette = "Set1")
    })
    
    output$salvadorCasosRaca <- renderPlot({
        casos_raca_ssa <- count(dataset_salvador_geral, RACA_COR)
        casos_raca_ssa <- casos_raca_ssa %>% 
            arrange(desc(RACA_COR)) %>%
            mutate(prop = n / sum(casos_raca_ssa$n) * 100) %>%
            mutate(ypos = cumsum(prop) - 0.5 * prop )
        
        ggplot(casos_raca_ssa, aes(x = "", y = prop, fill = RACA_COR)) +
            geom_bar(stat = "identity", width = 1, color = "white") +
            coord_polar("y", start = 0) +
            theme_void() +
            geom_text(aes(y = ypos, label = ifelse(prop > 1, str_c(format(prop, digits = 2),"%"), "")), color = "black", size = 6) +
            scale_fill_brewer(palette = "Set1")
    })
    
    output$casosBrasil <- renderPlot({
        bars_br <- barplot(
            sorted_casos_br$total_casos,
            names.arg = sorted_casos_br$uf,
            main = "CASOS CONFIRMADOS POR UF",
            ylim = c(0, 1.45*max(sorted_casos_br$total_casos)),
            width = 0.85,
            ylab = "TOTAL DE CASOS",
            xlab = "UF"
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
            main = "ÓBITOS POR UF",
            ylim = c(0, 1.45*max(sorted_obitos_br$total_obitos)),
            width = 0.85,
            ylab = "TOTAL DE ÓBITOS",
            xlab = "UF"
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
            main = "CASOS CONFIRMADOS POR MUNICÍPIO",
            width = 0.85,
            ylim = c(0, 1.1*max(sorted_10$n)),
            cex.names = 0.8,
            ylab = "TOTAL DE CASOS",
            xlab = "MUNICÍPIO"
        )
        text(
            bars,
            sorted_10$n,
            format(sorted_10$n, decimal.mark = ",", big.mark = "."),
            pos = 3,
            cex = 1
        )
    })
    
    output$obitosBahia <- renderPlot({
        bars <- barplot(
            sorted_10_obitos$n,
            names.arg = sorted_10_obitos$MUNICIPIO,
            main = "ÓBITOS POR MUNICÍPIO",
            width = 0.85,
            ylim = c(0, 1.1*max(sorted_10_obitos$n)),
            cex.names = 0.8,
            ylab = "TOTAL DE ÓBITOS",
            xlab = "MUNICÍPIO"
        )
        text(
            bars,
            sorted_10_obitos$n,
            format(sorted_10_obitos$n, decimal.mark = ",", big.mark = "."),
            pos = 3,
            cex = 1
        )
    })
    
    output$pizzaCasosSexo <- renderPlot({
        ba_casos_sexo <- count(dataset_casos_ba, SEXO)
        ba_casos_sexo <- ba_casos_sexo[ba_casos_sexo$n > 1000, ]
        ba_casos_sexo <- ba_casos_sexo %>% 
            arrange(desc(SEXO)) %>%
            mutate(prop = n / sum(ba_casos_sexo$n) *100) %>%
            mutate(ypos = cumsum(prop)- 0.5*prop )
        
        ggplot(ba_casos_sexo, aes(x = "", y = prop, fill = SEXO), title("Casos")) +
            geom_bar(stat = "identity", width = 1, color = "white") +
            coord_polar("y", start = 0) +
            theme_void() +
            geom_text(aes(y = ypos, label = str_c(format(prop, digits = 2),"%")), color = "white", size = 6) +
            scale_fill_brewer(palette = "Set1")
    })
    
    output$pizzaObitosSexo <- renderPlot({
        ba_obitos_sexo <- count(dataset_obitos_ba, SEXO)
        ba_obitos_sexo$SEXO <- with(ba_obitos_sexo, ifelse(SEXO == "F", "FEMININO", "MASCULINO"))
        ba_obitos_sexo <- ba_obitos_sexo %>%
            arrange(desc(SEXO)) %>%
            mutate(prop = n / sum(ba_obitos_sexo$n) *100) %>%
            mutate(ypos = cumsum(prop)- 0.5*prop )
        
        ggplot(ba_obitos_sexo, aes(x = "", y = prop, fill = SEXO)) +
            geom_bar(stat = "identity", width = 1, color = "white") +
            coord_polar("y", start = 0) +
            theme_void() +
            geom_text(aes(y = ypos, label = str_c(format(prop, digits = 2),"%")), color = "white", size = 6) +
            scale_fill_brewer(palette = "Set1")
    })
    
    output$pizzaObitosRacaCor <- renderPlot({
        casos_raca_ba <- count(dataset_casos_ba, RACA.COR)
        
        casos_raca_ba <- casos_raca_ba %>%
            arrange(desc(RACA.COR)) %>%
            mutate(prop = n / sum(casos_raca_ba$n) * 100) %>%
            mutate(ypos = cumsum(prop)- 0.5*prop )
        
        ggplot(casos_raca_ba, aes(x = "", y = prop, fill = RACA.COR),) +
            geom_bar(stat = "identity", width = 1, color = "white") +
            coord_polar("y", start = 0) +
            theme_void() +
            geom_text(aes(y = ypos, label = ifelse(prop > 3, str_c(format(prop, digits = 2),"%"), paste(""))), color = "black", size = 6) +
            scale_fill_brewer(palette = "Set1")
    })
    
    output$pizzaBaRacaCor <- renderPlot({
        df_bahia_raca <- data.frame( RAÇA.COR = c("AMARELA OU INDIGENA", "BRANCA", "INDIGENA", "NÃO INFORMADO", "PARDA", "PRETA"), prop = c(0.6, 20.3, 0, 0, 63.4, 15.7))
        df_bahia_raca <- df_bahia_raca %>%
            mutate(ypos = cumsum(prop)- 0.5*prop)
        
        ggplot(df_bahia_raca, aes(x = "", y = prop, fill = RAÇA.COR),) +
            geom_bar(stat = "identity", width = 1, color = "white") +
            coord_polar("y", start = 0) +
            theme_void() +
            geom_text(aes(y = ypos, label = ifelse(prop > 0, str_c(format(prop, digits = 2),"%"), "")), color = "black", size = 6) +
            scale_fill_brewer(palette = "Set1")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
