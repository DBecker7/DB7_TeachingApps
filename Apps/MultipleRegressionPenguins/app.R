library(shiny)
library(palmerpenguins)
library(broom)


peng <- penguins[complete.cases(penguins), ]
# For debugging
input <- list("species" = FALSE, "island" = FALSE,
    "bill_length_mm" = FALSE, "bill_depth_mm" = TRUE,
    "flipper_length_mm" = TRUE, "sex" = FALSE, col = "species",
    "restype" = "Residuals versus Fitted",
    "partial_residuals" = FALSE)

ui <- fluidPage(
    sidebarPanel(
        h2("Predictors"),
        checkboxInput("species", "species", FALSE),
        checkboxInput("island", "island", FALSE),
        checkboxInput("bill_length_mm", "bill_length_mm", TRUE),
        checkboxInput("bill_depth_mm", "bill_depth_mm", TRUE),
        checkboxInput("flipper_length_mm", "flipper_length_mm", TRUE),
        checkboxInput("sex", "sex", FALSE),
        h2("Colour"),
        selectInput("col", "Colouring variable",
            choices = c("species", "island", "sex"),
            selected = "species")
    ),
    mainPanel(
        tabsetPanel(type = "tabs",
            tabPanel("plot.lm(mymodel)",
                selectInput("restype", NULL,
                    c("R Default", "Residuals versus Fitted", "All"),
                    selected = "Residuals versus Fitted"),
                plotOutput("plot")),
            tabPanel("Partials",
                checkboxInput("partial_residuals", "Residuals?",
                    value = FALSE),
                plotOutput("partials")),
            tabPanel("Predictor Matrix",
                verbatimTextOutput("predictors")),
            tabPanel("summary(lm(y ~ .))",
                verbatimTextOutput("lm")),
            tabPanel("ANOVA (Type I)",
                verbatimTextOutput("anovaI")),
            tabPanel("ANOVA (Type II)",
                verbatimTextOutput("anovaII"))
        )
    )
)

server <- function(input, output) {
    output$plot <- renderPlot({
        pengdata <- peng[, c(input$species, input$island,
            input$bill_length_mm, input$bill_depth_mm,
            input$flipper_length_mm, TRUE, input$sex, FALSE)]
        mylm <- lm(body_mass_g ~ ., data = pengdata)
        if (input$restype == "R Default") {
            par(mfrow = c(2, 2))
            plot(mylm, col = peng[, input$col][[1]])
        } else if (input$restype == "Residuals versus Fitted") {
            par(mfrow = c(1, 1))
            plot(mylm, which = 1, col = peng[, input$col][[1]])
        } else {
            par(mfrow = c(2, 3))
            plot(mylm, which = 1:6, col = peng[, input$col][[1]])
        }
    }, width = 900, height = 700)

    output$partials <- renderPlot({
        pengdata <- peng[, c(input$species, input$island,
            input$bill_length_mm, input$bill_depth_mm,
            input$flipper_length_mm, TRUE, input$sex, FALSE)]
        mylm <- lm(body_mass_g ~ ., data = pengdata)
        mydf <- augment(mylm)
        mycoef <- coef(mylm)

        continuous_vars <- c("flipper_length_mm",
            "bill_length_mm",
            "bill_depth_mm")

        par(mfrow = c(1, 3))
        for (i in 1:3) {
            if (continuous_vars[i] %in% names(mycoef)) {
                thisvar <- continuous_vars[i]

                y <- peng$body_mass_g
                if (input$partial_residuals) {
                    y <- mydf$.resid
                }

                plot(peng[, thisvar, drop = TRUE], y,
                    col = peng[, input$col, drop = TRUE],
                    xlab = thisvar,
                    main = thisvar,
                    ylab = "body mass (g)")

                if (input$partial_residuals) {
                    abline(h = 0)
                } else {
                    abline(mycoef[1], mycoef[
                        which(names(mycoef) == continuous_vars[i])])
                }
            } else {
                plot(1, type = "n", xaxt = "n",
                    yaxt = "n", xlab = continuous_vars[i],
                    ylab = NULL,
                    bty = "n")
            }
        }
    }, width = 900, height = 400)

    output$predictors <- renderPrint({
        pengdata <- peng[, c(input$species, input$island,
            input$bill_length_mm, input$bill_depth_mm,
            input$flipper_length_mm, TRUE, input$sex, FALSE)]
        model.matrix(body_mass_g ~ ., data = pengdata)
    })

    output$lm <- renderPrint({
        pengdata <- peng[, c(input$species, input$island,
            input$bill_length_mm, input$bill_depth_mm,
            input$flipper_length_mm, TRUE, input$sex, FALSE)]
        summary(lm(body_mass_g ~ ., data = pengdata))
    })

    output$anovaI <- renderPrint({
        pengdata <- peng[, c(input$species, input$island,
            input$bill_length_mm, input$bill_depth_mm,
            input$flipper_length_mm, TRUE, input$sex, FALSE)]
        anova(lm(body_mass_g ~ ., data = pengdata))
    })

    output$anovaII <- renderPrint({
        opar <- options()
        options(contrasts = c("contr.sum", "contr.poly"))
        pengdata <- peng[, c(input$species, input$island,
            input$bill_length_mm, input$bill_depth_mm,
            input$flipper_length_mm, TRUE, input$sex, FALSE)]
        a2 <- drop1(lm(body_mass_g ~ ., data = pengdata), . ~ ., test = "F")
        options(opar)
        a2
    })
}

shinyApp(ui = ui, server = server)
