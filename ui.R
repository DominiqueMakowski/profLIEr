#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

custom_slider <- function(tag, label){
    sliderInput(tag,
                label = label,
                min = 0,
                max = 10,
                ticks = FALSE,
                value = 5,
                round = 5)
}



# Define UI for application that draws a histogram
shinyUI(fluidPage(





    # Application title
    titlePanel("PROFLIER: Lying and Deception Profile App"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            custom_slider("LIE_10", "1. I can lie well"),
            custom_slider("LIE_9", "2. I am a good liar"),
            custom_slider("LIE_14", "3. It is hard for others to detect my lies"),
            custom_slider("LIE_18", "4. It is easy for me to make up clever lies"),

            custom_slider("LIE_4", "5. I have a tendency to lie"),
            custom_slider("LIE_5", "6. I lie more often than most people do"),
            custom_slider("LIE_23", "7. I find it difficult to refrain myself from lying"),
            custom_slider("LIE_1", "8. I lie frequently"),

            custom_slider("LIE_41", "10. Lying is against my principles"),
            custom_slider("LIE_25", "11. I feel guilty after lying"),
            custom_slider("LIE_44", "12. It is bad to lie"),
            custom_slider("LIE_34", "13. I always avoid lying if I can"),

            custom_slider("LIE_43", "14. It is okay to lie sometimes"),
            custom_slider("LIE_33", "15. I lie when necessary"),
            custom_slider("LIE_42", "16. It is acceptable to lie depending on the context"),
            custom_slider("LIE_39", "17. I would lie if something important was at stake")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            div(h1("About"),
                HTML("This is the companion app for the <em>LIE Profiler</em> (Makowski et al., in preparation)."),
                strong("DISCLAIMER: This tool is made available for scientific exploration and should NOT be used for any other purpose, including clinical or forensic contexts. It is purely a research tool alongside the Deception Scale Validation study.")
            ),
            div(hr(), h1("Dimensions")),
            plotOutput("distributions"),
            div(hr()),
            tableOutput("scores"),
            div(hr(), h1("Profile")),
            plotOutput("radar")
        )
    )
))
