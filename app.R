library(shiny)

ui <- fluidPage(
  titlePanel("Simulation Wachstum & Cash"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("years", "Anzahl Jahre:",
                  min = 1, max = 10, value = 3),
      sliderInput("base_tax", "Grundtaxe (Jahr 0 & Folgejahre):",
                  min = 0, max = 2000, value = 1000, step = 50),
      sliderInput("start_people", "Startpersonen:",
                  min = 1, max = 30, value = 6),
      sliderInput("salary_cost", "Kosten pro Neueinstellung (CHF):",
                  min = 100, max = 1000, value = 418, step = 10),
      actionButton("runSim", "Simulation starten")
    ),

    mainPanel(
      h4("Initialinvestment"),
      verbatimTextOutput("initialInv"),
      h4("Jahresübersicht"),
      tableOutput("yearTable"),
      h4("Personen & Cash Verlauf"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$runSim, {
    salary_cost <- input$salary_cost
    growth_rate <- 1.18
    period_days <- 90
    daily_return_per_person <- (salary_cost * growth_rate) / period_days
    total_days <- input$years * 365
    recruitment_delay <- 3
    max_people <- 30

    # Initialinvestment berechnen
    initial_investment <- input$base_tax + (input$start_people * salary_cost)

    days <- 0
    cash <- 0
    cooldown <- 0
    active_contracts <- rep(period_days, input$start_people)

    history_days <- c()
    history_people <- c()
    history_cash <- c()
    year_end_records <- data.frame(Jahr=integer(), Personen=integer(), Cash=numeric())

    while (days < total_days) {
      days <- days + 1
      cash <- cash + length(active_contracts) * daily_return_per_person

      if (days %% 365 == 1 && days > 365) {
        cash <- cash - input$base_tax
      }

      active_contracts <- active_contracts - 1
      active_contracts <- active_contracts[active_contracts > 0]

      if (cooldown == 0 && cash >= salary_cost && length(active_contracts) < max_people) {
        new_people <- floor(cash / salary_cost)
        possible_new <- min(new_people, max_people - length(active_contracts))
        for (i in 1:possible_new) {
          active_contracts <- c(active_contracts, period_days)
          cash <- cash - salary_cost
        }
        cooldown <- recruitment_delay
      }

      if (cooldown > 0) {
        cooldown <- cooldown - 1
      }

      history_days <- c(history_days, days)
      history_people <- c(history_people, length(active_contracts))
      history_cash <- c(history_cash, cash)

      if (days %% 365 == 0) {
        year_end_records <- rbind(
          year_end_records,
          data.frame(Jahr=days/365,
                     Personen=length(active_contracts),
                     Cash=round(cash,2))
        )
      }
    }

    # Outputs
    output$initialInv <- renderText({
      paste(initial_investment, "CHF")
    })

    output$yearTable <- renderTable({
      year_end_records
    })

    output$plot <- renderPlot({
      plot(history_days, history_people, type="l", col="blue", lwd=2,
           xlab="Tage", ylab="Anzahl Personen / Cash/100",
           main="Verlauf Personen und Cash")
      lines(history_days, history_cash/100, col="red", lwd=2)
      legend("topleft", legend=c("Personen", "Cash / 100"),
             col=c("blue", "red"), lwd=2)
    })
  })
}

shinyApp(ui = ui, server = server)
