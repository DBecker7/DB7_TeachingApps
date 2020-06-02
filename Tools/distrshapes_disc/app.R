library(shiny)

parameter_tabs <- tagList(
    tags$style("#params { display:none; }"),
    tabsetPanel(id = "params",
        tabPanel("binomial",
            sliderInput("bsize", "size n", min = 1, max = 100,
                value = 4, step = 1,
                animate = list(interval = 600)),
            sliderInput("bprob", "prob p", min = 0, max = 1,
                value = 0.5, step = 0.01,
                animate = list(interval = 600))
        ),
        tabPanel("negbinomial",
            sliderInput("nbsize", "size n", min = 1, max = 100,
                value = 4, step = 1,
                animate = list(interval = 600)),
            sliderInput("nbprob", "prob p", min = 0, max = 1,
                value = 0.5, step = 0.01,
                animate = list(interval = 600))
        ),
        tabPanel("geometric",
            sliderInput("gprob", "prob p", min = 0, max = 1,
                value = 0.5, step = 0.01,
                animate = list(interval = 600))
        ),
        tabPanel("hypergeometric",
            "X is the number of white marbles",
            sliderInput("hypm", "white marbles m", min = 0, max = 50,
                value = 1, step = 1,
                animate = list(interval = 600)),
            sliderInput("hypn", "black marbles n", min = 0, max = 50,
                value = 1, step = 1,
                animate = list(interval = 600)),
            "Nothing is plotted if k > m + n",
            sliderInput("hypk", "marbles drawn k", min = 0, max = 50,
                value = 1, step = 1,
                animate = list(interval = 600))
        ),
        tabPanel("poisson",
            sliderInput("lambda", "rate <U+03BB>", min = 0, max = 50,
                value = 1, step = 1,
                animate = list(interval = 600))
        )
    )
)

geese <- 1
myseed <- 1
setlist <- list(data.frame(x = NA, y = NA, g = 0))

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
    sidebarLayout(
        sidebarPanel(
            selectInput("dist", "Distribution",
                choices = c("binomial", "poisson", "geometric", "negbinomial", "hypergeometric")
            ),
            numericInput("n", "Number of samples", value = 100),
            parameter_tabs,
            actionButton("doit", "Click me for new data"),
            actionButton("axes", "Reset axes and ghosts")
        ),
        mainPanel(
            plotOutput("hist")
        )
    )
)

server <- function(input, output, session) {
    observeEvent(input$dist, {
        updateTabsetPanel(session, "params", selected = input$dist)
    })

    axes <- reactive({
        input$axes
        input$dist

        xmin <<- 0
        xmax <<- 0
        ymax <<- 0

        setlist <<- list(data.frame(x = NA, y = NA, g = 0))
    })

    sample <- reactive({
        set.seed(myseed)
        input$doit
        switch(input$dist,
            binomial = rbinom(input$n, input$bsize, input$bprob),
            negbinomial = rnbinom(input$n, input$nbsize, input$nbprob),
            geometric = rgeom(input$n, input$gprob),
            poisson = rpois(input$n, input$lambda),
            hypergeometric = rhyper(input$n, input$hypm,
                input$hypn, input$hypk)
        )
    })

    distfun <- reactive({


        lohi <- switch(input$dist,
            binomial = c(0, input$bsize),
            negbinomial = c(0, qnbinom(0.999, input$nbsize, input$nbprob)),
            geometric = c(0, qgeom(0.999, input$gprob)),
            poisson = c(0, qpois(0.999, input$lambda)),
            hypergeometric = c(0, min(input$hypn, input$hypk))
        )

        xseq <- seq(lohi[1], lohi[2] + 1, by = 1)


        yseq <- switch(input$dist,
            binomial = dbinom(xseq, input$bsize, input$bprob),
            negbinomial = dnbinom(xseq, input$nbsize, input$nbprob),
            geometric = dgeom(xseq, input$gprob),
            poisson = dpois(xseq, input$lambda),
            hypergeometric = dhyper(xseq, input$hypm,
                input$hypn, input$hypk)
        )


        setlist <<- c(setlist, list(data.frame(x = xseq, y = yseq, g = geese)))
        geese <<- geese + 1

        data.frame(x = xseq, y = yseq, g = geese)
    })
    newseed <- reactive({
        input$doit
        myseed <<- myseed + 1
    })

    output$hist <- renderPlot({
        axes()

        x <- sample()
        d <- distfun()

        d2 <- dplyr::bind_rows(setlist)
        d2 <- subset(d2, g >= geese - 10)

        newseed()


        xmin <<- min(xmin, x - 0.5, d$x - 0.5)
        xmax <<- max(xmax, x + 0.5, d$x + 0.5)
        ymax <<- max(ymax, d$y * input$n, table(x))


        mytitle <- paste0("Bar plot and PDF of ", input$dist)

        ggplot() +
            geom_bar(aes(x = x), fill = "lightgrey", colour = 1) +
            {if (nrow(d2) > nrow(d)) geom_step(data = d2,
                mapping = aes(x = x - 0.5, y = y * input$n,
                    group = g, colour = geese - g, alpha = g),
                size = 0.6, na.rm = TRUE)} +
            geom_step(data = d, mapping = aes(x = x - 0.5, y = y * input$n),
                colour = 2, size = 1) +
            {if (nrow(d2) > nrow(d)) geom_segment(data = d2,
                mapping = aes(x = x - 0.5, xend = x - 0.5,
                    y = 0, yend = y * input$n,
                    group = g, colour = geese - g, alpha = g),
                size = 0.6, na.rm = TRUE)} +
            geom_segment(data = d,
                mapping = aes(x = x - 0.5, xend = x - 0.5,
                    y = 0, yend = y * input$n),
                colour = 2, size = 1) +
            theme_bw() +
            theme(legend.position = "none") +
            coord_cartesian(xlim = c(xmin, xmax) + c(-0.5, 0.5),
                ylim = c(0, ymax)) +
            labs(title = mytitle,
                caption = "Created by Devan Becker\nGithub: DBecker7/DB7_TeachingApps") +
            theme(title = element_text(size = 16),
              axis.text = element_text(size = 14))


        #   for(i in 1:length(setlist)){
        #       lines(setlist[[i]], col = rgb(0,0,0,0.1))
        #   }

        #  lines(d, col = 4,lwd = 2)
    }, res = 96)
}

# Run the application
shinyApp(ui = ui, server = server)
