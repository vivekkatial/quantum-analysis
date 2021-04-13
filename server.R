#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    d_raw <- reactive({
        read_rds("data/d_comparable.rds")
    })
    
    
    d_runs <- reactive({
        d_raw() %>%
            filter(params_n_qubits.qaoa <= input$qubits)
        
    })
    
    output$distPlot <- renderPlot({
        validate(need(input$color, "Select Option"))
        
        if (input$color == "none") {
            p <- d_runs() %>%
                ggplot(aes(
                    x = as.factor(params_n_qubits.qaoa),
                    y = metrics_energy
                ))
        } else if (input$color == "params_instance_type") {
           p <- d_runs() %>%
                ggplot(
                    aes(
                        x = as.factor(params_n_qubits.qaoa),
                        y = metrics_energy,
                        col = params_instance_type
                    )
                ) + 
               facet_wrap(~classical_optimiser)
        } else if (input$color == "classical_optimiser") {
            p <-  d_runs() %>%
                ggplot(aes(
                    x = as.factor(params_n_qubits.qaoa),
                    y = metrics_energy,
                    col = classical_optimiser
                )) 
        }
        
        if (input$plot_type == "box" ) {
            p <- p + geom_boxplot()
        } else if (input$plot_type == "violin") {
            p <- p + geom_violin()
        }
        
        p +
            theme_minimal() +
            labs(x = "Qubit",
                 y = "Energy")
        
        
    })
    
})
