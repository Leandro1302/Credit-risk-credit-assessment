library(shiny)

# Definisci l'interfaccia utente della web app
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .title-panel {
        text-align: center;
        padding: 10px;
        background-color: #4CAF50;
        color: white;
        font-size: 24px;
        font-weight: bold;
        border-radius: 10px;
      }
      .sidebar {
        background-color: #f1f1f1;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
      }
      .main-panel {
        padding: 20px;
      }
      .well {
        background-color: #ffffff;
        border: 1px solid #e3e3e3;
        box-shadow: 1px 1px 5px rgba(0, 0, 0, 0.1);
        border-radius: 10px;
        padding: 20px;
      }
      .help-text {
        font-size: 12px;
        color: #666666;
        margin-top: -10px;
      }
      .btn-custom {
        background-color: #007bff;
        color: white;
        border: none;
        padding: 10px 20px;
        font-size: 16px;
        margin-top: 10px;
        border-radius: 5px;
        width: 100%;
      }
      .input-group {
        margin-bottom: 15px;
      }
      .form-group label {
        font-weight: bold;
        color: #333333;
      }
    "))
  ),
  
  # Titolo della web app
  titlePanel(
    div(class = "title-panel", "Loan Approval Predictor")
  ),
  
  # Form per l'inserimento dei valori delle variabili
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar",
          # Input per le variabili
          fluidRow(
            div(class = "input-group",
                column(6,
                       numericInput("Gender", "Gender", value = 0, min = 0, max = 1),
                       helpText('0 = Woman, 1 = Man', class = 'help-text')
                ),
                column(6,
                       numericInput("Married", "Married", value = 0, min = 0, max = 1),
                       helpText('0 = Not married, 1 = Married', class = 'help-text')
                )
            )
          ),
          fluidRow(
            div(class = "input-group",
                column(6,
                       numericInput("Dependents", "Dependents", value = 0, min = 0, max = 3),
                       helpText('If the borrower has more than 3 dependents, enter 3', class = 'help-text')
                ),
                column(6,
                       numericInput("Education", "Education", value = 0, min = 0, max = 1),
                       helpText('0 = Not graduate, 1 = Graduate', class = 'help-text')
                )
            )
          ),
          fluidRow(
            div(class = "input-group",
                column(6,
                       numericInput("Self_Employed", "Self Employed", value = 0, min = 0, max = 1),
                       helpText('0 = No, 1 = Yes', class = 'help-text')
                ),
                column(6,
                       numericInput("Credit_History", "Credit History", value = 0, min = 0, max = 1),
                       helpText('0 = Not good credit history, 1 = Good credit history', class = 'help-text')
                )
            )
          ),
          fluidRow(
            div(class = "input-group",
                column(12,
                       numericInput("Property_Area", "Property Area", value = 0, min = 0, max = 2),
                       helpText('0 = Rural, 1 = Semiurban, 2 = Urban', class = 'help-text')
                )
            )
          ),
          fluidRow(
            div(class = "input-group",
                column(12,
                       numericInput("LoanAmountlog", "Loan Amount", value = 0, min = 0),
                       helpText('Enter the loan amount', class = 'help-text')
                )
            )
          ),
          fluidRow(
            div(class = "input-group",
                column(12,
                       numericInput("Loan_Amount_Term_log", "Loan Amount Term", value = 0, min = 0),
                       helpText('Enter the loan amount term', class = 'help-text')
                )
            )
          ),
          fluidRow(
            div(class = "input-group",
                column(12,
                       numericInput("Total_Income_log", "Total Income", value = 0, min = 0),
                       helpText('Enter the total income', class = 'help-text')
                )
            )
          ),
          actionButton('submit', 'Submit', class = 'btn-custom')
      )
    ),
    # Output per il risultato della previsione
    mainPanel(
      div(class = "main-panel",
          h3("Prediction Result"),
          wellPanel(
            textOutput("prediction"),
            br(),
            textOutput("repay_text")
          )
      )
    )
  )
)

# Definisci il server della web app
server <- function(input, output) {
  # Funzione per effettuare la previsione utilizzando il modello di regressione logistica
  predict_loan_approval <- function(Gender, Married, Dependents, Education, Self_Employed,
                                    Total_Income_log, LoanAmountlog,
                                    Loan_Amount_Term_log, Credit_History, Property_Area) {
    # Carica il modello di regressione logistica
    # Assicurati di avere già caricato il modello e di sostituire "model.RData" con il nome del tuo file
    load("model.RData")
    
    # Creazione del dataset di input per la previsione
    new_data <- data.frame(
      Gender = Gender,
      Married = Married,
      Dependents = Dependents,
      Education = Education,
      Self_Employed = Self_Employed,
      Total_Income_log = Total_Income_log,
      LoanAmountlog = LoanAmountlog,
      Loan_Amount_Term_log = Loan_Amount_Term_log,
      Credit_History = Credit_History,
      Property_Area = Property_Area
    )
    
    # Effettua la previsione utilizzando il modello
    prediction <- predict(model, new_data, type = 'response')
    
    # Ritorna il risultato della previsione
    return(prediction)
  }
  
  # Funzione per gestire l'evento di submit del form e visualizzare la previsione
  observeEvent(input$submit, {
    
    prediction <- predict_loan_approval(input$Gender, input$Married, input$Dependents,
                                        input$Education, input$Self_Employed, input$Total_Income_log,
                                        input$LoanAmountlog, input$Loan_Amount_Term_log,
                                        input$Credit_History, input$Property_Area)
    
    output$prediction <- renderText({
      paste("Loan Approval Prediction:", round(prediction))
    })
    
    output$repay_text <- renderText({
      if (round(prediction) == 1) {
        "The customer is able to repay the loan within the established Loan amount term."
      } else {
        "The customer is NOT able to repay the loan within the established loan amount term."
      }
    })
  })
}

# Crea la web app
shinyApp(ui = ui, server = server)
