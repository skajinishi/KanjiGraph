library(shiny)
library(shinycssloaders)
library(fmsb)
library(stringr)
library(showtext)
library(png) 


showtext_auto()


shinyUI(fluidPage(
  titlePanel("Kanji Graph"),
  sidebarLayout(
    sidebarPanel(width=3,
                 fileInput('data1', 'データのアップロード', accept = c(".csv")),
                 h6("データは「.csv」形式で用意してください"),
                 actionButton("go1", "RUN"),
                 uiOutput("choose_columns"),
                 
                 hr(),
                 withMathJax(helpText("標準化のデータ変換： $$x_{ij}^{'}=a\\left(\\frac{x_{ij}-\\bar x_j}{S_{j}}\\right)+b$$")),
                 withMathJax(helpText("文字の大きさは$$l\\left(\\frac{x_{ij}^{'}}{b}\\right)$$ で決めています")),
                 
                 numericInput("kakeru", "a:", value = 1, min = 0.1, step = 0.1),
                 numericInput("tasu", "b:", value = 3, min = 1, step = 1),
                 numericInput("bai", "l:", value = 1, min = 1, step = 1)
    ),
    
    mainPanel(width=9,
              tabsetPanel(id="tabs",
                          tabPanel("Data", 
                                   uiOutput("duplicate_warning"),
                                   tableOutput("pca_result")
                          ),
                          
                          tabPanel("漢字グラフ",
                                   br(),
                                   uiOutput("title_plot1"),
                                   plotOutput("plot2") %>% withSpinner(type = 4, color = "#0dc5c1"),
                                   radioButtons("button2", "Download figure", choices = list("png", "pdf", "eps"), inline = TRUE),
                                   downloadButton("out_down2", "download")
                          ),
                          
                          tabPanel("拡張型漢字グラフ",
                                   fluidRow(
                                     column(4, checkboxInput('header', '標準化する', TRUE)),
                                     column(4, uiOutput("max"))
                                   ),
                                   uiOutput("title_plot2"),
                                   div(
                                     plotOutput("plot", height = "100%") %>% withSpinner(type = 4, color = "#0dc5c1"),
                                     style = "height: 800px"
                                   ),
                                   radioButtons("button1", "Download figure", choices = list("png", "pdf", "eps"), inline = TRUE),
                                   downloadButton("out_down1", "download")
                          ),
                          
                          tabPanel("データ作成",
                                   fluidRow(
                                     column(4, fileInput('data2', 'データアップロード')),
                                     column(4, radioButtons("group", "データ形式", choices = list("グループ情報あり"=1, "なし"=2))),
                                     column(4, uiOutput("clust"))
                                   ),
                                   tableOutput("face2"),
                                   downloadButton('downloadData', 'DataDownload'),
                                   downloadButton('downloadData2', 'Cluster Info')
                          ),
                          
                          tabPanel("Sample Data",
                                   br(),
                                   HTML("<h4><b>Sample data</b></h4>"),
                                   HTML("<td align=\"center\"><a href=\"sample1.csv\" download>sample1.csv(漢字グラフ用)</a></td>"),
                                   br(),
                                   HTML("<td align=\"center\"><a href=\"sample2.csv\" download>sample2.csv(拡張型漢字グラフ用)</a></td>"),
                                   br(), br(),
                                   HTML("<h4><b>Version history</b></h4>"),
                                   HTML("<style>div.ScrollBox {overflow:auto;width:500px;height:120px;border:1px black solid;margin:0px}</style>
                                         <body><div class=\"ScrollBox\"><p>
                                         <b>0.1.0</b> - Aug. 2025<br>&nbsp;*Released.<br>
                                         <br>
                                         <b>0.1.1</b> - Feb. 2026<br>
                                         &nbsp;* Maintenance update.<br>
                                         </p></div></body>"),
                                   br(), br()
                          )
                          ,
                          tabPanel("確認",
                                   verbatimTextOutput("face")
                          )
              )
    )
  )
))