#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "qubits",
                "Number of Qubits:",
                min = 5,
                max = 8,
                value = 5
            ),
            radioButtons(
                "color",
                "View type:",
                c(
                    "None" = "none",
                    "Classical Optimiser" = "classical_optimiser",
                    "Source" = "params_instance_type"
                )
            ),
            radioButtons(
                "plot_type",
                "Plot type:",
                c(
                    "Box Plot" = "box",
                    "Violin Plot" = "violin"
                )
            ),
            checkboxGroupInput("variable", "Variables to show:",
                               c("Cylinders" = "cyl",
                                 "Transmission" = "am",
                                 "Gears" = "gear"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(plotOutput("distPlot"))
    )
))
