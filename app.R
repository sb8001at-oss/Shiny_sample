library(shiny)
library(tidyverse)

fruits <- c("banana", "apple", "orange")

ui <- fluidPage(
    tabsetPanel(
        tabPanel("textOutput",
            fluidPage(
                fluidRow(
                    column(width = 4,                   
                           h3("SliderInput"),
                           p("ui"),
                           p(code("sliderInput(\"i_slider\", \"sliderInput\", min = 1, max = 50, value = 30)")),
                           sliderInput("i_slider", "sliderInput", min = 1, max = 50, value = 30),
                           
                           p("server"),
                           p(code("output$o_slider <- renderText({paste(\"slider Inputの値は\", input$i_slider, \"です。\")})")),
                           textOutput("o_slider"),
                           
                           hr(),                 
                           h3("numericInput"),
                           p("ui"),
                           p(code("numericInput(\"i_numeric\", \"numericInput\", value = 10, min = 0, max = 100)")),
                           numericInput("i_numeric", "numericInput", value = 10, min = 0, max = 100), 
                           
                           p("server"),
                           p(code("output$o_num <- renderText({paste(\"入力値の5乗は\", input$i_numeric ^ 5, \"です。\")})")),
                           verbatimTextOutput("o_num"),
                           
                           hr(),
                           h3("textInput"),
                           p("ui"),
                           p(code("textInput(\"i_text\", \"textInput\", value = \"Joe\")")),
                           textInput("i_text", "textInput", value = "Joe"),
                           
                           p("server"),
                           p(code("output$o_tex <- renderText({paste0(\"Hello World, \", input$i_text, \"!\")})")),
                           textOutput("o_tex"),
                           
                           hr(),
                           h3("passwordInput"),
                           p("ui"),
                           p(code("numericInput(\"i_numeric\", \"numericInput\", value = 10, min = 0, max = 100)")),
                           passwordInput("i_pass", "passwordInput", value = "himitsuno_password"),
                           
                           p("server"),
                           p(code("output$o_pass <- renderText({paste0(input$i_pass)})")),
                           textOutput("o_pass")                
                    ),
                    
                    column(width = 4,
                           h3("checkboxInput"),
                           p("ui"),
                           p(code("checkboxInput(\"i_cbi\", \"checkboxInput\", choices = c(\"banana\", \"apple\", \"orange\", selected = \"banana\"))")),
                           checkboxInput("i_cbi", "checkboxInput"),
                           
                           p("server"),
                           p(code("output$o_cbi <- renderText({if_else(input$i_cbi, \"Checked\", \"Check removed.\")})")),
                           textOutput("o_cbi"),
                           
                           hr(),
                           h3("checkboxGroupInput"),
                           p("ui"),
                           p(code("checkboxGroupInput(\"i_cbg\", \"checkboxGroupInput\", choices = c(\"banana\", \"apple\", \"orange\", selected = \"banana\"))")),
                           checkboxGroupInput("i_cbg", "checkboxGroupInput", choices = c("banana", "apple", "orange"), selected = "banana"),
                           
                           p("server"),
                           p(code("output$o_cbg <- renderText({input$i_cbg})")),
                           verbatimTextOutput("o_cbg"),                   
                           
                           hr(),
                           h3("selectInput"),
                           p("ui"),
                           p(code("selectInput(\"i_cbg\", \"selectInput\", choices = c(\"banana\", \"apple\", \"orange\", selected = \"banana\"))")),
                           selectInput("i_sel", "checkboxGroupInput", choices = c("banana", "apple", "orange"), selected = "banana"),
                           
                           p("server"),
                           p(code("output$o_sel <- renderText({input$i_sel})")),
                           verbatimTextOutput("o_sel"),                   
                           
                           hr(),
                           h3("radioButtons"),
                           p("ui"),
                           p(code("radioButtons(\"i_cbg\", \"selectInput\", choices = c(\"banana\", \"apple\", \"orange\", selected = \"banana\"))")),
                           radioButtons("i_rad", "radioButtons", choices = c("banana", "apple", "orange"), selected = "banana"),
                           
                           p("server"),
                           p(code("output$o_rad <- renderText({input$i_rad})")),
                           verbatimTextOutput("o_rad")
                    ),
                    
                    column(width = 4,
                           h3("dateInput"),
                           p("ui"),
                           p(code("dateInput(\"i_dat\", \"dateInput\", value = Sys.Date(), min = \"1980-01-01\", max = \"2025-12-31\")")),
                           dateInput("i_dat", "dateInput", value = Sys.Date(), min = "1980-01-01", max = "2025-12-31"),
                           
                           p("server"),
                           p(code("output$o_dat <- renderText({input$i_dat |> as.Date() |> as.character()})")),
                           verbatimTextOutput("o_dat"),
                           
                           hr(),
                           h3("dateRangeInput"),
                           p("ui"),
                           p(code("dateRangeInput(\"i_datr\", \"dateInput\", start = Sys.Date(), end = Sys.Date + 5, min = \"1980-01-01\", max = \"2025-12-31\")")),
                           dateRangeInput("i_datr", "dateRangeInput", start = Sys.Date(), end = Sys.Date() + 5, min = "1980-01-01", max = "2025-12-31"),
                           
                           p("server"),
                           p(code("output$o_datr <- renderText({input$i_datr})")),
                           verbatimTextOutput("o_datr"),
                           
                           hr(),
                           h3("fileInput"),
                           p("ui"),
                           p(code("fileInput(\"i_file\", \"fileInput\")")),
                           fileInput("i_file", "fileInput"),
                           
                           p("server"),
                           p(code("output$o_file <- renderText({input$i_file$datapath})")),
                           verbatimTextOutput("o_file"),
                           
                           hr(),
                           h3("actionButton"),
                           p("ui"),
                           p(code("actionButton(\"i_act\", \"actionButton\")")),
                           actionButton("i_act", "actionButton"),
                           
                           p("server"),
                           p(code("x = reactiveVal(0)")),
                           p(code("observeEvent(input$i_act,{x(x()+1)})")),
                           p(code("output$o_act = renderText(x())")),
                           textOutput("o_act"),
                           p(em("＊verbatimTextOutputではボタンを押しても数値が増えない"))
                    )
                )
            )
        ),
        
        tabPanel("Plot・Table Output",
            fluidPage(
                fluidRow(
                    column(width = 4,
                           h3("sliderInput → plotOutput"),
                           sliderInput("i_bins", "ヒストグラムのBinsの数", value = 10, min = 5, max = 30),
                           plotOutput("o_bins"),
                           p("ui"),
                           verbatimTextOutput("ui_1"),
                           p("server"),
                           verbatimTextOutput("server_1")
                    ),
                    column(width = 4,
                           h3("selectInput → plotOutput"),
                           selectInput("i_species", "列名", choices = iris$Species |> levels(), selected = "setosa"),
                           plotOutput("o_species"),
                           p("ui"),
                           verbatimTextOutput("ui_2"),
                           p("server"),
                           verbatimTextOutput("server_2")
                    ),
                    column(width = 4,
                           h3("sliderInput → tableOutput"),
                           sliderInput("i_rows", "irisの行の数", value = 10, min = 5, max = 30),
                           tableOutput("o_rows"),
                           p("ui"),
                           verbatimTextOutput("ui_3"),
                           p("server"),
                           verbatimTextOutput("server_3")
                    )
                )
            )
        ),
        tabPanel("HTML elements",
            fluidPage(
                fluidRow(
                    column(width = 4,
                           h3("ヘッダー"),
                           p(code("h1(\"Header 1\")")),
                           h1("Header 1"),
                           
                           hr(),
                           p(code("h2(\"Header 2\")")),
                           h2("Header 2"),
                           
                           hr(),
                           p(code("h3(\"Header 3\")")),
                           h3("Header 3"),
                           
                           hr(),
                           p(code("h4(\"Header 4\")")),
                           h4("Header 4"),
                           
                           hr(),
                           p(code("h5(\"Header 5\")")),
                           h5("Header 5"),
                           
                           hr(),
                           p(code("h6(\"Header 6\")")),
                           h6("Header 6")
                           
                    ),
                    column(width = 4,
                           h3("水平線、パラグラフ、コード等"),
                           p("水平線"),
                           p(code("hr()")),
                           hr(),
                           
                           hr(),
                           p("改行"),
                           p(code("br()")),
                           br(),
                           
                           hr(),
                           p(code("p(\"パラグラフ\")")),
                           p("パラグラフ"),
                           
                           hr(),
                           p(code("p(strong(\"ボールド\"))")),
                           p(strong("ボールド")),
                           
                           hr(),
                           p(code("p(em(\"Italic\"))")),
                           p(em("Italic")),
                           
                           hr(),
                           p(code("p(code(\"コード\"))")),
                           p(code("コード"))
                    ),
                    column(width = 4,
                           h3("画像、リンク、Javascript等"),
                           
                           p("リンク"),
                           p(code("a(href = \"https://posit.co/\", \"link to Posit.co\")")),
                           a(href = "https://posit.co/", "link to Posit.co"),
                           
                           hr(),
                           p("画像"),
                           p(code("img(src=\"https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png\", width = \"100\")")),
                           img(src="https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png", width = "100"),
                           
                           hr(),
                           p("リンク付き画像"),
                           p(code("a(href = \"https://posit.co/\", img(src=\"https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png\", width = \"100\"))")),
                           a(href = "https://posit.co/", img(src="https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png", width = "100")),
                           hr(),
                           
                           p(code("HTML(\"<p>HTML直打ち</p>\")")),
                           HTML("<p>HTML直打ち</p>"),
                           hr(),
                           
                           p("Javascriptの実行（wwwフォルダに保存したJSを実行）"),
                           p(code("tags$head(tags$script(src = \"Hello_world.js\"))")),
                           tags$head(tags$script(src = "Hello_world.js")),
                           p("F12→ConsoleにHello World!が表示されている")
                    )
                )
            )
        )
    )
)

server <- function(input, output) {

    output$o_slider <- renderText({paste("slider Inputの値は", input$i_slider, "です。")})
    output$o_num <- renderText({paste("入力値の5乗は", input$i_numeric ^ 5, "です。")})
    output$o_tex <- renderText({paste0("Hello World, ", input$i_text, "!")})
    output$o_pass <- renderText({paste0(input$i_pass)})
    output$o_cbi <- renderText({if_else(input$i_cbi, "Checked", "Check removed.")})
    output$o_cbg <- renderText({input$i_cbg})
    output$o_sel <- renderText({input$i_sel})
    output$o_rad <- renderText({input$i_rad})
    output$o_dat <- renderText({input$i_dat |> as.Date() |> as.character()})
    output$o_datr <- renderText({input$i_datr |> as.Date() |> as.character()})
    output$o_file <- renderText({input$i_file$datapath})
    
    x = reactiveVal(0)
    observeEvent(input$i_act,{x(x()+1)})
    output$o_act = renderText(x())
    
    output$o_bins <- renderPlot({
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$i_bins + 1)
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = '間欠泉の吹き出し間の時間',
             ylab = '間欠泉の吹き出しの発生頻度',
             main = "")
    })
    
    output$o_species <- renderPlot({
        iris |> 
            filter(Species == input$i_species) |> 
            ggplot(aes(x = Petal.Length, y = Petal.Width)) +
            geom_point(size=5, alpha = 0.5)
    })
    
    output$o_rows <- renderTable({
        rows <- iris[sample(1:nrow(iris), input$i_rows), c(1, 5)]
    })
    
    output$ui_1 <- renderText({
        "h3(\"sliderInput → plotOutput\"),\n
        sliderInput(\"i_bins\", \"ヒストグラムのBinsの数\", value = 10, min = 5, max = 30),\n
        plotOutput(\"o_bins\")"
    })
    
    output$ui_2 <- renderText({
        "h3(\"selectInput → plotOutput\"),\n
        selectInput(\"i_species\", \"列名\", choices = iris$Species |> levels(), selected = \"setosa\"),\n
        plotOutput(\"o_species\")"
    })
    
    output$ui_3 <- renderText({
        "h3(\"sliderInput → tableOutput\"),\n
        sliderInput(\"i_rows\", \"irisの行の数\", value = 10, min = 5, max = 30),\n
        tableOutput(\"o_rows\")"
    })
    
    output$server_1 <- renderText({
        "output$o_bins <- renderPlot({\n
        x    <- faithful[, 2]\n
        bins <- seq(min(x), max(x), length.out = input$i_bins + 1)\n
        hist(x, breaks = bins, col = \'darkgray\', border = \'white\',\n
        xlab = \'間欠泉の吹き出し間の時間\',\n
        ylab = \'間欠泉の吹き出しの発生頻度\',\n
        main = \"\")
        })"
    })
    
    output$server_2 <- renderText({
        "output$o_species <- renderPlot({\n
        iris |> \n
        filter(Species == input$i_species) |> \n
        ggplot(aes(x = Petal.Length, y = Petal.Width)) +\n
        geom_point(size=5, alpha = 0.5)\n
        })"
    })
    
    output$server_3 <- renderText({
        "output$o_rows <- renderTable({\n
        rows <- iris[sample(1:nrow(iris), input$i_rows), c(1, 5)]\n
        })"
    })
}

shinyApp(ui = ui, server = server)
