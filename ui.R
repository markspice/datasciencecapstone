
# Define UI for word prediction application
shinyUI(fluidPage(
        
        style="background-color: #00878c",
  
        tags$head(tags$script(
                'Shiny.addCustomMessageHandler("refocus",
                function(NULL) {
                document.getElementById("text").focus();
                });'
    )),
        
  # Application title
  tags$div(style="background-color: #691941",
  titlePanel(h1("WORD PREDICTION APPLICATION", align = "center", style="color: #ebebeb"))),
  
  sidebarLayout(
          
          sidebarPanel(
                  style="height:562px;background-color: #ebebeb",
                  tags$b("INSTRUCTIONS:"),
                  tags$br(),
                  tags$br(),
                  tags$p("Start entering text by typing into the text box near the top of the screen. 
                         The three large buttons below the text box will give suggestions for your next 
                         word. Selecting one of these words will add the complete section of text to the 
                         main panel."),
                  tags$p("After you have added your first section, further text can be added by 
                         repeating the process. After completing your passage, a final punctuation mark 
                         can be added using one of the period, question, or exclamation mark buttons."),
                  tags$p("If you make a mistake, the 'Clear Last' button removes the last chunk of text 
                         added to the panel, whereas 'Clear All' removes all added text."),
                  tags$p("After you have completed writing your passage, clicking on 'Submit' will update 
                         the word prediction algorithm to give higher prominence to the actual phrases 
                         entered by the user.")
          ),
          
  
          mainPanel(
                  shinyjs::useShinyjs(),
                  br(),
                  textInput("text","Start typing...", width = "100%"),
                  div(style="display:inline-block",uiOutput('Button1')),
                  div(style="display:inline-block",uiOutput('Button2')),
                  div(style="display:inline-block",uiOutput('Button3')),
                  div(style="display:inline-block",actionButton("stop", ".")),
                  div(style="display:inline-block",actionButton("question", "?")),
                  div(style="display:inline-block",actionButton("exclamation", "!")),
                  br(),
                  div(class="container-fluid",
                      style="height:40px",
                      span(h3(textOutput("warning")), style = "color:red")),
                  br(),
                  div(class="container-fluid",
                      style="height:320px;background-color: #ebebeb",
                      h3(textOutput("message"))),
                  br(),
                  div(style="display:inline-block",actionButton("deleteLast", "CLEAR LAST", width = "285px", style = "color: #ebebeb; background-color: #647D8C")),
                  div(style="display:inline-block",actionButton("deleteAll", "CLEAR ALL", width = "285px", style = "color: #ebebeb; background-color: #647D8C")),
                  div(style="display:inline-block",actionButton("submit", "SUBMIT", width = "285px", style = "color: #ebebeb; background-color: #647D8C"))
          )
                    
         )
)
)
