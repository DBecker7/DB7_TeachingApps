# ScatterCorr
# A tool for students to explore correlation.

library(shiny)
library(ggplot2)

myseed <- 2112
set.seed(myseed)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$style(
        ".irs-bar {",
        "  border-color: transparent;",
        "  background-color: transparent;",
        "}",
        ".irs-bar-edge {",
        "  border-color: transparent;",
        "  background-color: transparent;",
        "}"
    ),

    # Application title
    titlePanel("Where are Points Most Influential?"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                "Number of samples",
                min = 5,
                max = 500,
                value = 50),
            sliderInput("x",
                "x-value of point",
                min = 0,
                max = 10,
                value = 5,
                step = 0.1),
            sliderInput("y",
                "y-value of point",
                min = -5,
                max = 70,
                value = 45,
                step = 0.1),
            actionButton(inputId = "doit", label = "Click me for new data")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            tableOutput("infMeasures"),
            tags$div(HTML("Some questions:<br><ul>
	<li>Where does an outlier have the largest effect on the slope?</li>
	<li>Why is it easier to reduce the slope, but harder to increase it?</li>
	<li>How does your answer to the previous question relate to correlation?</li>
	<li>In the formula for the slope, what values is the outlier affecting?</li>
</ul>"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    newseed <- reactive({
        input$doit
        myseed <<- myseed + 1
    })

    makedata <- reactive({
        set.seed(myseed)
        input$doit
        n <- input$n
        x <- runif(n - 1, 0, 10)
        y <- 2 + 5 * x + rnorm(n - 1, 0, 4)
        data.frame(x = c(input$x, x), y = c(input$y, y))
    })

    output$infMeasures <- renderTable({
        newseed()
        xy <- makedata()

        x <- xy[, 1]
        y <- xy[, 2]
        outlier <- c("Outlier", rep("Data", input$n - 1))

        lmwith <- lm(y ~ x, data = xy)
        lmwout <- lm(y ~ x, data = xy[-1, ])

        data.frame(
            variable = c("intercept", "slope", 
                "sigma", "h_{ii}"),
            without_outlier = round(c(lmwout$coef[1], lmwout$coef[2],
                summary(lmwout)$sigma, NA), 2),
            with_outlier = round(c(lmwith$coef[1], lmwith$coef[2],
                summary(lmwith)$sigma, hatvalues(lmwith)[1]), 2)
        )
    })

    output$distPlot <- renderPlot({
        newseed()
        xy <- makedata()

        x <- xy[, 1]
        y <- xy[, 2]
        outlier <- c("Outlier", rep("Data", input$n - 1))

        bwith <- lm(y ~ x, data = xy)$coef[2]
        bwout <- lm(y ~ x, data = xy[-1, ])$coef[2]

        ggplot(mapping = aes(x = x, y = y)) +
            geom_point(mapping = aes(colour = outlier, size = outlier)) +
            scale_colour_manual(values = c(1, 2)) +
            scale_size_manual(values = c(2, 4)) +
            geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                colour = 2) +
            geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                mapping = aes(x = x[-1], y = y[-1]), colour = 1) +
            theme_bw() +
            labs(title = paste0("Slope with outlier: ", round(bwith, 2),
                    ", slope without: ", round(bwout, 2)),
                caption = paste0("Created by Devan Becker\n",
                    "Github: DBecker7/DB7_TeachingApps"),
                colour = NULL, size = NULL) +
            theme(title = element_text(size = 16),
                axis.text = element_text(size = 14)) +
            coord_cartesian(xlim = c(0, 10), ylim = c(-10, 70))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
