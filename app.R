#last update 01.07.2024
#preregistration number
#line between the items
#https://ntp.niehs.nih.gov/sites/default/files/ntp/ohat/pubs/handbookmarch2019_508.pdf
#https://ntp.niehs.nih.gov/sites/default/files/ntp/ohat/pubs/riskofbiastool_508.pdf
#https://ntp.niehs.nih.gov/whatwestudy/assessments/noncancer/riskbias
#,img(src = 'EFSA-logo3.png', style= "vertical-align:bottom; float : right", height = "60%", width = "60%")

library(shiny)
library(shinydashboard)
library(shinyjs)
library(R6)
library(DT)
library(ggplot2)
library(dplyr)
library(stringr)

addResourcePath("static", "www")
choices <- list("high risk of bias", "some concerns", "undecided/no judgement", 
                "low risk of bias", "no information", "not applicable")


ui <- function(request){
  dashboardPage(
  dashboardHeader(title = "raRoB-vet"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem(text    = "About raRoB-vet",
               tabName = "about"),
      menuItem(text    = "General Information", 
               tabName = "summary" 
               ),
      menuItem(text    = "1. Domain Selection", 
               tabName = "selection" 
               ),
      menuItem(text    = "2. Domain Exposure",
               tabName = "exposure"
               ),
      menuItem(text    = "3. Domain Outcome",
               tabName = "outcome"
               ),
      menuItem(text    = "4. Domain Confounding",
               tabName = "confounding"
               ),
      menuItem(text    = "5. Domain Censoring",
               tabName = "censoring"
               ),
      menuItem(text    = "6. Domain Analysis",
               tabName = "analysis"
               ),
      menuItem(text    = "7. Domain Selective Reporting",
               tabName = "reporting"
               ),
      menuItem(text = "Reviewer Information",
               tabName = "rev_info"),
      menuItem(text = "Assessment results", 
               tabName = "result"),
      menuItem(text = "Weights",
               tabName = "weights")
      
    )),
  #"display: block; margin-left: auto; margin-right: auto;"
  #
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              fluidPage(
                column(8,
                textOutput("rarob_manual1"),
                div(style = "height: 10px"),
                textOutput("rarob_manual2"),
                div(style = "height: 10px"),
                tags$footer(
                  "For detailed information about the tool, please visit",
                tags$a(
                  "the raRoB-vet tool manual",
                  target = "_blank",
                  href = "static/Manual-raRoB-vet.pdf"
                ),
                style = " width: 100%; color: black; text-align: left;"
                ),
                div(style = "height: 10px"),
                tags$footer(
                  "For more information about appraising and integrating evidence from epidemiological studies see",
                  tags$a("Scientific Committee guidance of EFSA",
                         target = "_blank",
                        href = "https://www.efsa.europa.eu/en/efsajournal/pub/8866/"),
                  style = " width: 100%; color: black; text-align: left;"
                  ),
                div(style = "height: 10px"),
              tags$footer(
                "For any questions or problems when using the tool please contact us at",
                tags$a(
                  "rarob@bfr.bund.de",
                  target = "_blank",
                  href = "mailto:rarob@bfr.bund.de"
                ),
                style = " width: 100%; color: black; text-align: left;"
              ),
              div(style = "height: 50px"),
              tags$footer(em("raRoB-vet prototype is still a draft version.")),
              div(style = "height: 10px"),
              tags$footer(em("The development project is funded by the European Union."))),
              column(3,img(src = 'logo.png', style= "float : right",  height = "50%", width = "50%")),
              div(style = "height: 10px")
      )),
      tabItem(tabName = "summary",
              fluidPage(
                fluidRow(
                  column(6,
                         textAreaInput(
                           inputId = "title", 
                           label   = "Title", 
                           value = "", 
                           width = NULL, 
                           placeholder = NULL
                         ),
                         textAreaInput(
                           inputId = "qual",
                           label = "Is primary or secondary data used in the study?",
                           value = "",
                           width = NULL,
                           placeholder = NULL
                         )),
                  column(6,
                         textInput(
                           inputId = "doi", 
                           label   = "DOI/URL", 
                           value = "", 
                           width = NULL, 
                           placeholder = NULL
                         ),
                         checkboxInput("prereg", "Is the study pre-registered?"),
                         htmlOutput("note_prereg")
                         
                  )),
                fluidRow(
                  textAreaInput(
                    inputId = "abs",
                    label = "Abstract",
                    value = "",
                    width = "800px",
                    height = "200px",
                    cols = NULL,
                    rows = NULL,
                    placeholder = NULL,
                    resize = NULL
                  )),
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "stud_des",
                           label = "Study Design",
                           selected = "character(0)",
                           inline = TRUE,
                           width = NULL,
                           choices = list("cohort", "case-control", "cross-sectional", "ecological", "nested case-control", "case cohort")
                      ),
                      radioButtons(
                        inputId = "item01",
                        label = "Item 0.1: Is the selected study design appropriate for the research question?",
                        selected = "character(0)",
                        inline = TRUE,
                        width = NULL,
                        choices = choices)),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_stud_des")),
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item01")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item01"))
                         ))
                )),
      
      tabItem(tabName = "selection",
              fluidPage(
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "item11",
                           label = "Item 1.1: Eligibility criteria",
                           selected = "character(0)",
                           inline = FALSE,
                           width = NULL,
                           choices = choices
                    ),
                         textAreaInput(
                           inputId = "item11_comment",
                           label = "Comment",
                           value = "",
                           width = NULL,
                           height = "100px",
                           cols = NULL,
                           rows = NULL,
                           placeholder = "Elaborate your choice/Reference the text (line and page number)",
                           resize = NULL
                         )),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item11")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item11"))
                  )),
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "item12",
                           label = "Item 1.2: Comparability of groups",
                           selected = "character(0)",
                           inline = FALSE,
                           width = NULL,
                           choices = choices
                     ),
                         textAreaInput(
                           inputId = "item12_comment",
                           label = "Comment",
                           value = "",
                           width = NULL,
                           height = "100px",
                           cols = NULL,
                           rows = NULL,
                           placeholder = "Elaborate your choice/Reference the text (line and page number)",
                           resize = NULL
                         )),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item12")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item12"))
                         )
                ),
                
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "item13",
                           label = "Item 1.3: Non-response rate",
                           selected = "character(0)",
                           inline = FALSE,
                           width = NULL,
                           choices = choices
                      ),
                         textAreaInput(
                           inputId = "item13_comment",
                           label = "Comment",
                           value = "",
                           width = NULL,
                           height = "100px",
                           cols = NULL,
                           rows = NULL,
                           placeholder = "Elaborate your choice/Reference the text (line and page number)",
                           resize = NULL
                         )),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item13")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item13"))
                         )
                ),
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "item14",
                           label = "Item 1.4: Time frames",
                           selected = "character(0)",
                           inline = FALSE,
                           width = NULL,
                           choices = choices
                       ),
                         textAreaInput(
                           inputId = "item14_comment",
                           label = "Comment",
                           value = "",
                           width = NULL,
                           height = "100px",
                           cols = NULL,
                           rows = NULL,
                           placeholder = "Elaborate your choice/Reference the text (line and page number)",
                           resize = NULL
                         )),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item14")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item14"))
                         )
                )
                )),

      tabItem(tabName = "exposure",
              fluidPage(
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "item21",
                           label = "Item 2.1: Methodology of exposure measurements",
                           selected = "character(0)",
                           inline = FALSE,
                           width = NULL,
                           choices = choices
                       ),
                         textAreaInput(
                           inputId = "item21_comment",
                           label = "Comment",
                           value = "",
                           width = NULL,
                           height = "100px",
                           cols = NULL,
                           rows = NULL,
                           placeholder = "Elaborate your choice/Reference the text (line and page number)",
                           resize = NULL
                         )),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item21")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item21"))
                         ))
              )),

      tabItem(tabName = "outcome",
              fluidPage(
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "item31",
                           label = "Item 3.1: Methodology of outcome measurements",
                           selected = "character(0)",
                           inline = FALSE,
                           width = NULL,
                           choices = choices
                       ),
                         textAreaInput(
                           inputId = "item31_comment",
                           label = "Comment",
                           value = "",
                           width = NULL,
                           height = "100px",
                           cols = NULL,
                           rows = NULL,
                           placeholder = "Elaborate your choice/Reference the text (line and page number)",
                           resize = NULL
                         )),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item31")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item31"))
                         )
                )
              )),

      tabItem(tabName = "confounding",
              fluidPage(
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "item41",
                           label = "Item 4.1: Accounting for confounding",
                           selected = "character(0)",
                           inline = FALSE,
                           width = NULL,
                           choices = choices
                           # choiceNames = choices,
                           # choiceValues = choice_values
                         ),
                         textAreaInput(
                           inputId = "item41_comment",
                           label = "Comment",
                           value = "",
                           width = NULL,
                           height = "100px",
                           cols = NULL,
                           rows = NULL,
                           placeholder = "Elaborate your choice/Reference the text (line and page number)",
                           resize = NULL
                         )),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item41")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item41"))
                         )
                ),
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "item42",
                           label = "Item 4.2: Confounding assessments",
                           selected = "character(0)",
                           inline = FALSE,
                           width = NULL,
                           choices = choices
                         ),
                         textAreaInput(
                           inputId = "item42_comment",
                           label = "Comment",
                           value = "",
                           width = NULL,
                           height = "100px",
                           cols = NULL,
                           rows = NULL,
                           placeholder = "Elaborate your choice/Reference the text (line and page number)",
                           resize = NULL
                         )),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item42")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item42"))
                         )
                )
                )),

      tabItem(tabName = "censoring",
              fluidPage(
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "item51",
                           label = "Item 5.1: Adequacy of length of observation periods",
                           selected = "character(0)",
                           inline = FALSE,
                           width = NULL,
                           choices = choices
                        ),
                         textAreaInput(
                           inputId = "item51_comment",
                           label = "Comment",
                           value = "",
                           width = NULL,
                           height = "100px",
                           cols = NULL,
                           rows = NULL,
                           placeholder = "Elaborate your choice/Reference the text (line and page number)",
                           resize = NULL
                         )),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item51")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item51"))
                         )
                ),
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "item52",
                           label = "Item 5.2: Relevance and handling of dropouts",
                           selected = "character(0)",
                           inline = FALSE,
                           width = NULL,
                           choices = choices
                        ),
                         textAreaInput(
                           inputId = "item52_comment",
                           label = "Comment",
                           value = "",
                           width = NULL,
                           height = "100px",
                           cols = NULL,
                           rows = NULL,
                           placeholder = "Elaborate your choice/Reference the text (line and page number)",
                           resize = NULL
                         )),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item52")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item52"))
                         )
                )
                

              )),

      tabItem(tabName = "analysis",
              fluidPage(
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "item61",
                           label = "Item 6.1: Statistical methods",
                           selected = "character(0)",
                           inline = FALSE,
                           width = NULL,
                           choices = choices
                           # choiceNames = choices,
                           # choiceValues = choice_values
                         ),
                         textAreaInput(
                           inputId = "item61_comment",
                           label = "Comment",
                           value = "",
                           width = NULL,
                           height = "100px",
                           cols = NULL,
                           rows = NULL,
                           placeholder = "Elaborate your choice/Reference the text (line and page number)",
                           resize = NULL
                         )),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item61")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item61"))
                         )
                ),
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "item62",
                           label = "Item 6.2: Handling of missing values",
                           selected = "character(0)",
                           inline = FALSE,
                           width = NULL,
                           choices = choices
                       ),
                         textAreaInput(
                           inputId = "item62_comment",
                           label = "Comment",
                           value = "",
                           width = NULL,
                           height = "100px",
                           cols = NULL,
                           rows = NULL,
                           placeholder = "Elaborate your choice/Reference the text (line and page number)",
                           resize = NULL
                         )),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item62")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item62"))
                         )
                )
                )),

      tabItem(tabName = "reporting",
              fluidPage(
                fluidRow(
                  column(6,
                         radioButtons(
                           inputId = "item71",
                           label = "Item 7.1: Selective reporting",
                           selected = "character(0)",
                           inline = FALSE,
                           width = NULL,
                           choices = choices
                         ),
                        textAreaInput(
                           inputId = "item71_comment",
                           label = "Comment",
                           value = "",
                           width = NULL,
                           height = "100px",
                           cols = NULL,
                           rows = NULL,
                           placeholder = "Elaborate your choice/Reference the text (line and page number)",
                           resize = NULL
                         )),
                  column(6,
                         box(title = span("Help", style = 'font-size:14px;font-weight:bold'), 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("help_item71")),
                         box(title = span("Signalling Questions", style = 'font-size:14px;font-weight:bold'),
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             width = 12,
                             htmlOutput("signal_item71"))
                         )
                )
              )),
      tabItem(tabName = "rev_info",
              fluidPage(
                textInput(
                  inputId = "name", 
                  label   = "Name of the assessor", 
                  value = "", 
                  width = NULL, 
                  placeholder = "Name"
                ),
                textAreaInput(
                  inputId = "assessor_comment",
                  label = "Assessment of the reviewer",
                  value = "",
                  height = "300px",
                  placeholder = "Please give your assessment"
                )
              )),
      tabItem(tabName = "result",
              fluidPage(
                h4("After you answered all items, click submit to see the results"),
                actionButton("submit", "Submit"),
                plotOutput("circularBarPlot", width = "100%", click = "plot_click"),
                tableOutput("answersTable"),
                div(style = "height: 10px"),
                textOutput("sum_score"),
                div(style = "height: 10px"),
                downloadButton("report", "Print report as a word file"),
                downloadButton("downloadData", "Download table as csv")
              )),
      tabItem(tabName = "weights",
              fluidPage(
                h4("In this section you can manually change the weighting of the items."),
                h3("Important note: The user-defined weights are required to calculate an overall numerical score. This feature was implemented in response to user requests. The default weights ensure that equal weights are placed on each of the domains, irrespective of the number of items assessed. The developers of this tool do not explicitly endorse the use of a sum score and advise users to use the results with caution. In particular, the sum score should not be interpreted in terms of the magnitude of risk of bias. Studies with identical summary scores should not be assumed to have the same magnitude of risk of bias. Differences or rations of sum scores for two studies are not necessarily proportional to differences or ratios of risk of bias."),
                column(4,
                       numericInput("weight01", "Weights for item 0.1", value = 0.25),                     
                       numericInput("weight11", "Weights for item 1.1", value = 0.25),
                       numericInput("weight12", "Weights for item 1.2", value = 0.25),
                       numericInput("weight13", "Weights for item 1.3", value = 0.25),
                       numericInput("weight14", "Weights for item 1.4", value = 0.25),
                       numericInput("weight21", "Weights for item 2.1", value = 1),
                       numericInput("weight31", "Weights for item 3.1", value = 1)),
                column(4,
                       numericInput("weight41", "Weights for item 4.1", value = 0.5),
                       numericInput("weight42", "Weights for item 4.2", value = 0.5),
                       numericInput("weight51", "Weights for item 5.1", value = 0.5),
                       numericInput("weight52", "Weights for item 5.2", value = 0.5),
                       numericInput("weight61", "Weights for item 6.1", value = 0.5),
                       numericInput("weight62", "Weights for item 6.2", value = 0.5),
                       numericInput("weight71", "Weights for item 7.1", value = 1))
              ))
      
    ),
    headerPanel(""),
    actionButton(inputId = "Previous", label = "Previous"),
    actionButton(inputId = "Next", label = "Next")
  )
)
}


server <- function(input, output, session) {
  
  tab_id <- c("about", "summary", "selection", "exposure", "outcome",
              "confounding", "censoring", "analysis", "reporting", "rev_info", "result", "weights")
  
  observe({
    lapply(c("Next", "Previous"),
           toggle,
           condition = input[["tabs"]] != "about")
  })
  
  Current <- reactiveValues(
    Tab = "about"
  )
  
  observeEvent(
    input[["tabs"]],
    {
      Current$Tab <- input[["tabs"]]
    }
  )
  
  observeEvent(
    input[["Previous"]],
    {
      tab_id_position <- match(Current$Tab, tab_id) - 1
      if (tab_id_position == 0) tab_id_position <- length(tab_id)
      Current$Tab <- tab_id[tab_id_position]
      updateTabItems(session, "tabs", tab_id[tab_id_position]) 
    }
  )
  
  observeEvent(
    input[["Next"]],
    {
      tab_id_position <- match(Current$Tab, tab_id) + 1
      if (tab_id_position > length(tab_id)) tab_id_position <- 1
      Current$Tab <- tab_id[tab_id_position]
      updateTabItems(session, "tabs", tab_id[tab_id_position]) 
    }
  )
  
    # Reactive values to store the answers
    answers <- reactiveValues(
      q01 = NULL,
      w01 = NULL,
      q11 = NULL,
      w11 = NULL,
      q12 = NULL,
      w12 = NULL,
      q13 = NULL,
      w13 = NULL,
      q14 = NULL,
      w14 = NULL,
      q21 = NULL,
      w21 = NULL,
      q31 = NULL,
      w31 = NULL,
      q41 = NULL,
      w41 = NULL,
      q42 = NULL,
      w42 = NULL,
      q51 = NULL,
      w51 = NULL,
      q52 = NULL,
      w52 = NULL,
      q61 = NULL,
      w61 = NULL,
      q62 = NULL,
      w62 = NULL,
      q71 = NULL,
      w71 = NULL
    )
    
    # Observe event for the submit button
    observeEvent(input$submit, {
      answers$q01 <- input$item01
      answers$w01 <- input$weight01
      answers$q11 <- input$item11
      answers$w11 <- input$weight11
      answers$q12 <- input$item12
      answers$w12 <- input$weight12
      answers$q13 <- input$item13
      answers$w13 <- input$weight13
      answers$q14 <- input$item14
      answers$w14 <- input$weight14
      answers$q21 <- input$item21
      answers$w21 <- input$weight21
      answers$q31 <- input$item31
      answers$w31 <- input$weight31
      answers$q41 <- input$item41
      answers$w41 <- input$weight41
      answers$q42 <- input$item42
      answers$w42 <- input$weight42
      answers$q51 <- input$item51
      answers$w51 <- input$weight51
      answers$q52 <- input$item52
      answers$w52 <- input$weight52
      answers$q61 <- input$item61
      answers$w61 <- input$weight61
      answers$q62 <- input$item62
      answers$w62 <- input$weight62
      answers$q71 <- input$item71
      answers$w71 <- input$weight71

    })
    
    # Output the answers as a table
    answersTable <- reactive({
      if (is.null(answers$q01) || is.null(answers$q11) || is.null(answers$q12) || is.null(answers$q13) ||
          is.null(answers$q14) || is.null(answers$q21) || is.null(answers$q31) ||
          is.null(answers$q41) || is.null(answers$q42) || is.null(answers$q51) ||
          is.null(answers$q52) || is.null(answers$q61) || is.null(answers$q62) ||
          is.null(answers$q71) 
          ) {data.frame(Item = character(), Rating = character(), Weight = numeric(), stringsAsFactors = FALSE)
      } else {
        data.frame(
          Weight = c(answers$w01, answers$w11, answers$w12, answers$w13, answers$w14, answers$w21, answers$w31, answers$w41, answers$w42, answers$w51, answers$w52, answers$w61, answers$w62, answers$w71),
          Domain = c("Design", "1) Selection", "1) Selection", "1) Selection", "1) Selection", "2) Exposure", "3) Outcome", 
                     "4) Confounding", "4) Confounding", "5) Censoring", "5) Censoring", "6) Analysis", "6) Analysis", "7) Reporting"),
          Number = c("Item 0.1", "Item 1.1", "Item 1.2", "Item 1.3", "Item 1.4", "Item 2.1", "Item 3.1", "Item 4.1", "Item 4.2",
                     "Item 5.1", "Item 5.2", "Item 6.1", "Item 6.2", "Item 7.1"),
          Item = c("Appropriateness of study design", "Eligibility criteria", "Comparability of groups", 
                   "Non-response rate", "Time frames", "Methodology of exposure measurements", 
                   "Methodology of outcome measurements", "Accounting for confounding", "Confounding assessments", 
                   "Adequacy of length of observation periods", "Relevance and handling of dropouts",
                   "Statistical methods", "Handling of missing values", "Selective reporting"),
          Rating = c(answers$q01, answers$q11, answers$q12, answers$q13, answers$q14, answers$q21, answers$q31, answers$q41, answers$q42, answers$q51, answers$q52, answers$q61, answers$q62, answers$q71),
          stringsAsFactors = FALSE
        )
      }
    })

    # Output the answers as a table
    output$answersTable <- renderTable({
      answersTable()
    })
## | answersTable$Rating == "no information"
    output$sum_score <- renderText({
    sum_data <- answersTable() %>% 
      mutate(ratenum = case_when(
        Rating == "high risk of bias" ~ 3,
        Rating == "some concerns" ~ 2,
        Rating == "undecided/no judgement" ~ 0,
        Rating == "low risk of bias" ~ 1,
        Rating == "no information" ~ 3,
        Rating == "not applicable" ~ 0))
    sum_score <- sum(sum_data$Weight * sum_data$ratenum)/7
    paste("Summary Score:", sum_score)
    })
    
    output$circularBarPlot <- renderPlot({
      data <- answersTable() %>% 
        mutate(score = case_when(
          Rating == "high risk of bias" ~ 3,
          Rating == "some concerns" ~ 2,
          Rating == "undecided/no judgement" ~ 4,
          Rating == "low risk of bias" ~ 1,
          Rating == "no information" ~ 5,
          Rating == "not applicable" ~ 0))
      if (nrow(data) > 0) {
      p <- data %>% 
        ggplot() +
        geom_hline(aes(yintercept = y), data.frame(y = c(0:3)), color = "lightgrey") +
        geom_col(aes(x = factor(str_wrap(Number, 5)), y = score, fill = factor(Rating)), position = "dodge", alpha = .9) +
#        geom_text(aes(x = factor(Domain), y = 5, label = factor(Domain)), alpha = .7) +
        scale_y_continuous(limits = c(-3, 7), expand = c(0,0), breaks = c(0, 1, 2, 3)) +
#        geom_bar(stat = "identity") +
        coord_polar() + 
        theme_minimal() +
        scale_color_manual(values = c("high risk of bias" = "red", "not applicable" = "white", "low risk of bias" = "green", "some concerns" = "yellow", "undecided/no judgement" = "gray", "no information" =  "orange"), aesthetics = "fill") +
        theme(legend.position = "bottom",
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = 12),
              panel.background = element_rect(fill = "white", color = "white"),
              panel.grid = element_blank(),
              panel.grid.major.x = element_blank(),
              text = element_text(color = "gray12", family = "Bell MT")) +
        labs(fill = "Rating")
      print(p)
      }
    })
    
    # Download handler for CSV
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("raRoB-vet-assessment-results-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(answersTable(), file, row.names = FALSE)
      }
    )


  output$report <- downloadHandler(
    filename = function() {
      paste("raRoB-vet-assessment-results-", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
    #   #making temporary report file
     tempReport <- file.path(tempdir(), "report.rmd")
     file.copy("report.rmd", tempReport, overwrite = TRUE)
    # 
    #   #set up parameters to pass to report
     params <- list(n = c(
       #1
       input$name, 
       #2
       input$title,
       #3
       input$abs,
       #4
       input$prereg,
       #5
       input$assessor_comment,
       #6
       input$stud_des,
       #7
       input$item01,
       #8
       input$qual,
       #9
       input$item11,
       #10
       input$item11_comment,
       #11
       input$item12,
       #12
       input$item12_comment,
       #13
       input$item13,
       #14
       input$item13_comment,
       #15
       input$item14,
       #16
       input$item14_comment,
       #17
       input$item21,
       #18
       input$item21_comment,
       #19
       input$item31,
       #20
       input$item31_comment,
       #21
       input$item41,
       #22
       input$item41_comment,
       #23
       input$item42,
       #24
       input$item42_comment,
       #25
       input$item51,
       #26
       input$item51_comment,
       #27
       input$item52,
       #28
       input$item52_comment,
       #29
       input$item61,
       #30
       input$item61_comment,
       #31
       input$item62,
       #32
       input$item62_comment,
       #33
       input$item71, 
       #34
       input$item71_comment
       ))
     
     rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
                        )
    }
  )
              
  output$rarob_manual1  <- renderText({
    "The appraisal of evidence from observational studies on animal populations, e.g. in the framework of meta-analysis, weight of evidence assessment or risk assessment, requires  judgments on the reliability of results from each individual primary study. Available assessment tools for such evaluations of quality of observational studies in human populations lack important aspects specific for studies on animal populations."
  })
  
  output$rarob_manual2  <- renderText({
    "Against this background, at the German Federal Institute for Risk Assessment (BfR) in cooperation with the European Food Safety Authority (EFSA) and experts in the area, we have developed a tool for rapid assessment of Risk of Bias in observational epidemiological veterinary studies (raRoB-vet). The tool will be freely available and is designed for the scientific community to facilitate a transparent and valid critical appraisal of observational animal studies."
  })

  output$note_prereg <- renderUI({
    HTML(paste("Example of registery for animal studies:", 
    "https://www.animalstudyregistry.org/asr_web/index.action,", 
    "https://preclinicaltrials.eu/database", sep = "<br/>"))
    })
  
  output$note1   <- renderText("In this section we included the items that we are considering to add to the tool, improve the quality asessment of the epidemiological animal studies. Please give your assessment if you think this item clarifies aspects of the quality assessment that are not covered elsewhere in the tool.")
  
  
  output$signal_item01 <- renderUI({
    HTML(paste("Does the study design (cohort, case-control, cross-sectional, etc.) match the type of research question being asked?",
               sep = "<br/>"))
  })
  
  output$signal_item11 <- renderUI({
  HTML(paste("Are the eligibility criteria (inclusion and exclusion criteria) for enrolment in the study adequately and appropriately specified?",
             "Are inclusion/exclusion criteria based on characteristics of study subjects and/or study units?",
             "Are inclusion/exclusion criteria applied equally to all study groups (by exposure or outcome)?",
             "Are the target and study populations and their characteristics clearly described?",
             sep = "<br/>"))
  })
  
  output$signal_item12 <- renderUI({
    HTML(paste("Were the inclusion and exclusion criteria applied to all groups to be studied?",
               "If several groups are under investigation (control group): The groups being studied are selected from the same source population",
               "Were subjects and groups of subjects housed under identical or comparable conditions, including:",
               "1) Housing conditions (e.g. indoor/outdoor, barn types, stocking density)",
               "2) Feeding strategy",
               "3) Sex ratio",
               "4) Climatic conditions (e.g. temperature, humidity)",
               sep = "<br/>"))
    })
  
  # output$signal_item13 <- renderUI({HTML(paste("Were subjects and groups of subjects housed under identical or comparable conditions, 
  # including:", "1) Stocking density", "2) Diet", "3) Sex ratio", "4) Temperature", sep = "<br/>"))})
 
  output$signal_item13 <- renderUI({
  HTML(paste("Is there a low rate of non-response?",
             "Are non response rates specified for each study group?",
             "Is the non-response rate comparable among considered groups?",
             "Is the participation rate stated?",
             "How high was the non-response rate and was it comparable among considered groups?",
             "Were there any meaningful difference between responders and non-responders?",
             "Were the reasons provided for non-response related to the exposure or outcome?",
             sep = "<br/>"))})
  
  output$signal_item14 <- renderUI({
    HTML(paste("Are relevant dates, including period of recruitment and exposure, clearly described?", sep = "<br/>"))})
  
  output$signal_item21 <- renderUI({
    HTML(paste("Were objective, suitable, and validated criteria used for the measurement of exposure?",
               "If subjective judgement for the measurement of exposure was required: Were independent adjudicators used?",
               "Was exposure status determined without knowledge of the results of the outcome (blinded)?",
               sep = "<br/>"))})
  
  output$signal_item31 <- renderUI({
    HTML(paste("Were objective, suitable and validated criteria used for the measurement of outcome?", 
               "Was the outcome assessed without the knowledge about the exposure (blinded)?",
               "If subjective judgement for the measurement of outcome was required: Were independent adjudicators used?",
               "Could knowledge of exposure status have influenced outcome measurements?",
               sep = "<br/>"))})
  
  output$signal_item41 <- renderUI({
    HTML(paste("Were all confounding variables taken into account in the design and/or analysis, using techniques such as restriction, stratification, multivariable analysis, propensity score matching, instrumental variables or other approaches?",
               sep = "<br/>"))})
  
  output$signal_item42 <- renderUI({
    HTML(paste("Are the distributions of the principal confounders clearly described for each study group?",
               "Were confounding factors that were controlled for (and for which control was necessary) measured validly and reliably by the variables available in this study?",
               sep = "<br/>"))})
  
  output$signal_item51 <- renderUI({
    HTML(paste("Was the length of follow up reported?",
               "Is the follow-up period, following exposure, adequate to allow for the development of the outcome of interest?",
               sep = "<br/>"))})
  
  output$signal_item52 <- renderUI({HTML(paste("Were withdrawals and dropouts reported in terms of numbers and/or reasons per group?",
                                               "Are the proportion of drop outs comparable between the study groups?",
                                               sep = "<br/>"))})
  
  output$signal_item61 <- renderUI({HTML(paste("Are the statistical methods appropriate for the study type, design and/or the question?",
                                               "Were balancing techniques used, such as matching or stratification, if there was unequal distribution of base line characteristics or different group sizes between groups?",
                                               sep = "<br/>"))})
  output$signal_item62 <- renderUI({HTML(paste("Was the number and distribution of missing values reported and taken into account for analysis and subsequent interpretation?",
                                               "Were appropriate statistical methods used to account for missing values and/or impute data, such as mean imputation, multiple imputation, maximum likelihood estimation?",
                                               "Are the proportion of study subjects and/or study units with missing data and reasons for missing data similar across exposures/outcomes?",
                                               sep = "<br/>"))})
  output$signal_item71 <- renderUI({HTML(paste("Are results reported for all research questions /objectives, and are all results associated with a research question /objective?",
                                               "Are any important primary/measured outcomes or exposures missing from results?", 
                                               "Do the reported results align with the outcomes and analyses specified in the preregistered study plan?",
                                               sep = "<br/>"))})
  
  output$help_stud_des <- renderUI({HTML(paste("Cohort study: A cohort study tracks two or more exposure groups forward from exposure to outcome. This type of study can be done by going ahead in time from the present (prospective cohort study) or by going back in time to identify the cohorts and following them up to the present or past or future (retrospective cohort study).",
                                               "Case-control study: Study groups are defined by outcome, based on whether the outcome of interest has occurred(cases) or not(controls). Historical and present data on exposures and characteristics of cases and controls are compared to assess potential associations and identify risk factors.",
                                               "Cross-sectional studies: A study conducted in a specific population at a specific point in time or over a short period. Exposure and outcome are assessed at the same time.",
                                               "Nested case-control study: Variation of a case-control study in which cases and controls are drawn from the population in a cohort study. The exposure of interest is measured in the cases group, and then the investigator chooses a random sample of all participants who did not develop the outcome, to be used as controls.",
                                               "Case-cohort study: A variant of the case-control study conducted within a cohort study in which controls are drawn from the same initial cohort as are cases, regardless of their outcome (case, disease) status.",
                                               sep = "<br/>"))})
  
  output$help_item01 <- renderUI({HTML(paste("The suitability of the chosen study design for addressing the research question depends on several factors, including the nature of the research question and the goals of the study. If the study design does not align well with the research question, various types of bias can be introduced. Different study designs have unique strengths and limitations, and researchers must carefully select the one that aligns with their objectives.",
                                             sep = "<br/>"))})
  
  output$help_item11 <- renderUI({HTML(paste("Inappropriate selection criteria could lead to selection bias or even confounding. The selection domain deals with the process of selecting the study subjects into the study or analysis. This includes the period and course of recruitment. ",
                                             "The item eligibility obtains information on whether:",
                                             "•	the inclusion and exclusion criteria for the study subjects were clearly defined prior to the start of the study",
                                             "•	the inclusion and exclusion criteria were appropriate for the study question and appropriately applied to all study subjects",
                                             "For the specific and guided assessment of the risk of selection bias, use the signalling questions provided. If you are able to answer the question with a positive response, this indicates a low likelihood of bias; if you're unable to answer the question with a yes, this indicates a moderate to high likelihood of bias.",
                                             sep = "<br/>"))})
  
  output$help_item12 <- renderUI({HTML(paste("The item considers the comparability of the groups in the equal distribution of their inherent characteristics, treatment/exposure and other features, such as housing density, climatic conditions, diet and/or feeding and management practices, that may be correlated with the main study variables. Differences in diet composition and/or acces to food as well as temperature, humidity, and light cycles or even routine procedures (like sampling or medication) can significantly affect animals' physiological parameters and behavior, hence the housing conditions should be identical or at least comparable between the groups under investigation.",
                                             "This item is used to obtain information on whether",
                                             "•	the subject selection process leads to bias",
                                             "•	whether the response rate of selected subjects is related to the exposure and outcome",
                                             "Under ceratin circumstances, correction methods for selection bias could be applied. This exploration within the study should be considered when assessing the risk of bias.",
                                             sep = "<br/>"))})
  
  output$help_item13 <- renderUI({HTML(paste("Non-responders are study units or subjects who do not participate in the study at all, thereby not providing any data. In contrast, dropouts are participants who are included in the study at baseline measurements but discontinue participation during the course of the study, resulting in incomplete data and potential selection bias. Non-response does not necessarily indicate a high risk of bias provided that the non-response rate is low and similar between the different exposure/outcome groups (“non-differential”). However, a high non-response rate might raise concerns that there is a systematic difference between responders and non-responders in the outcomes, exposures, potential confounders or other important characteristics. Any indication of correlation between non-response and any of the main study variables (e.g. exposure status, baseline risk factors, etc.) raises concern as possible source of bias.", sep = "<br/>"))})
  
  output$help_item14 <- renderUI({HTML(paste("It is essential to consider the temporal relationship between inclusion of study subjects and/or study units and certain time points during the course of the study, such as beginning of the follow-up, beginning of the exposure, or stage of the outcome.", 
                                             "In prospective studies, recruitment time period ideally lies before the start of the follow-up and the exposure. Nevertheless, it should be ensured that subjects are at a similar exposure stage if possible.", 
                                             "In retrospective studies, it is equally advisable that study subjects are at a similar stage of outcome by recruitment. Recruitment time frame is a technical term describing a concept which is applicable for all types of studies regardless of the actual procedure to enroll or select study subjects.",
                                             sep = "<br/>"))})
  
  output$help_item21 <- renderUI({HTML(paste("Methodology of exposure measurements addresses the validity and reliability of exposure measurements as well as the potential misclassification of exposure, e.g. due to the lack of blinding.", 
                                             "The term measurement error refers to continuous variables and misclassification to categorical variables. For exposure misclassification, the misclassification is nondifferential if it is unrelated to the occurence or presence of outcome; if the misclassification of exposure is different for those with and without the outcome, it is considered differential.",
                                             "Blinding the investigating personnel serves to reduce misclassification due to preconception. Depending on the measurement method, blinding is recommended since information on outcome status may influence the assessment of exposure in the data-collecting personnel.",
                                             "The use of established exposure measurement methods is generally recommended to reduce bias, hence this item is aimed to check if the exposure status was assessed correctly using instruments/measurements that had been validated and published before and if assessment methods/tools were used or implemented consistently among all study subjects. For binary exposure status, the classification performance can be expressed in terms of its sensitivity and specificity relative to the true exposure status.", 
                                             sep = "<br/>"))})
  
  output$help_item31 <- renderUI({HTML(paste("The methodology of outcome measurements addresses the validity and reliability of the outcome measurements, as well as the potential misclassification of outcomes, e.g. due to the lack of blinding. The use of established outcome measurement methods is generally recommended to reduce bias, hence this item is aimed to check if the outcome variables were measured correctly using instruments/measurements that had been validated and published or piloted and published before and if assessment methods/tools were used or implemented consistently among all Study subjects. For binary outcome status, the classification performance can be expressed in terms of its sensitivity and specificity relative to the true outcome status. Similar to the misclassification of exposure, misclassification of outcomes is nondifferential when it is unrelated to the exposure status. ",
                                             "It is also important to consider if a risk of subjectivity (preconception by staff) has been taken into account in the data collection and, if necessary, in the analysis and interpretation. Blinding the investigating personnel serves to reduce misclassification bias due to preconception. Staff which knows about the exposure status of study subjects might report the severe symptoms/endpoints more frequently. At the same time, staff may detect the endpoints in exposed study subjects quicker (or with higher sensitivity) because they would suspect them. Depending on the measurement method, blinding is recommended since information on outcome status may lead to altered perception and assessment of exposure. hence this item is also used to obtain information on whether the outcome assessors were blinded to the exposure status of participants or the measurement of outcome was done with knowledge of the exposure status.",
                                             sep = "<br/>"))})
  
  output$help_item41 <- renderUI({HTML(paste("Confounding refers to a situation where the association between an exposure (independent variable) and an outcome (dependent variable) is distorted or confounded by the presence of a third variable, known as a confounder. Confounders can either mask or distort a real association or create a false appearance of an association that does not exist. Failing to account for confounding can lead to invalid assessments of the true relationships between exposures and outcomes.", 
                                             "With this item, it is checked whether the investigators took into account all possible factors that may potentially act as confounder during their analysis, in the sense of either preventing their confounding effect or adjusting the effect measures for them (e.g. with an appropriate statistical analysis). It is to be assumed that every study has some degree of unknown residual confounding that has not been adjusted for. However, the focus here is mainly on obvious and known factors, which could cause an important distortion to the exposure/outcome relationship. If the major sources of confounding have been accounted for, some small amount of residual confounding might bias the results minimally.",
                                             sep = "<br/>"))})
  
  output$help_item42 <- renderUI({HTML(paste("In this item, the methods and their corresponding validity and reliability of the measurement for the potential confounding variables is assessed.",
                                             "This item is used to obtain information on whether",
                                             "•	some of the measurements of confounding factors were not clearly defined, valid, reliable or consistent for all study subjects.",
                                             "•	whether the confounders were measured with errors or were misclassified.", 
                                             sep = "<br/>"))})
  
  output$help_item51 <- renderUI({HTML(paste("The length of observation periods in a study can introduce bias, especially if it is not appropriately considered in the study design and analysis. If the length of the follow-up is not sufficient for the outcome (e.g., a disease) to occur, the study shall be assessed with a high risk of bias. The observation period refers to the whole duration of the study, including measurements of exposure and outcome as well as duration over which study subjects are followed. The timing and duration of this period can impact the internal validity of the study. If the observation period is too short, important temporal patterns may be missed, and the true relationship between variables cannot be assessed.",
                                             "This item is used to obtain information on whether",
                                             "•	the length of follow-up was long enough to detect a possible association between exposure and outcome",
                                             sep = "<br/>"))})
  
  output$help_item52 <- renderUI({HTML(paste("Dropout in animal observational studies refers to the situation where some animals initially included in the study, for various reasons, are no longer available for observation or data collection during the course of the study. This can occur due to a variety of factors such as illness, death, relocation, or any other circumstance that leads to the removal of animals from the study before its completion. The proportion of these subjects should be calculated for each study group. Any indication of correlation between dropout and any of the main study variables (e.g. exposure status, risk factors, etc.) raises concern as possible source of bias. Dropout can introduce bias and affect the validity of the study results in case the animals that dropout differ systematically from those that remain in the study.",
                                             sep = "<br/>"))})
  
  output$help_item61 <- renderUI({HTML(paste("Statistical analysis itself is not a source of bias; rather statistical analysis can address and potentially adjust for bias. The choice of the statistical methods should be appropriate to the study question and study design, data collection, and limitations.", sep = "<br/>"))})
  
  output$help_item62 <- renderUI({HTML(paste("Handling missing values is a critical aspect of data analysis, and how it is done can introduce bias into the study results if not handled appropriately. While the items Non-response rate and Relevance and handling of drop outs primarily focus on the extent, distribution and possible underlying reasons for lack of response and drop outs, the item handling of missing values concentrates more on the statistical consideration of missing data, especially if data is imputed.",
                                             "To minimize bias associated with missing values, researchers should carefully consider the reasons for missingness, choose appropriate imputation methods, perform sensitivity analyses, and transparently report their handling of missing data in research publications.", 
                                             "If outcome is missing by some study subjects it should be reported in the paper.",
                                             sep = "<br/>"))})
  
  output$help_item71 <- renderUI({HTML(paste("The consistency between the pre-defined outcomes in the study questions and the reported results is assessed. If certain results are missing, this might lead to biased assessments/conclusions. The lack of results for subgroups or an explanation of the reasons for the discrepancy in the number of study subjects enrolled and those included in the analysis (sometimes a study flow chart is used) also raises concerns about this risk of bias.",
                                             "This item is used to obtain information on whether:",
                                             "•	important variables (e.g. primary/measured outcomes or exposures were missing from the results.",
                                             "•	the study results that were reported were likely selected based on the direction and magnitude of the estimated effects or statistical significance.",
                                             sep = "<br/>"))})
  
  session$onSessionEnded(
    function() {
      stopApp()
    }
  )
  }

enableBookmarking(store = "server")
shinyApp(ui = ui, server = server)

#runApp(list(ui=ui,server=server), launch.browser=TRUE) #now runs by default in the external browser.