# gettysburg

library(shiny)
library(tidytext)
library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)

p1 <- "Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal."
p2 <- "Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this."
p3 <- "But, in a larger sense, we can not dedicate -- we can not consecrate -- we can not hallow -- this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us -- that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion -- that we here highly resolve that these dead shall not have died in vain -- that this nation, under God, shall have a new birth of freedom -- and that government of the people, by the people, for the people, shall not perish from the earth."

getty <- tibble(txt = c(p1, p2, p3), para = c(1,2,3)) %>% 
    unnest_tokens(word, txt) %>% 
    mutate(nch = nchar(word))
pop_mean <- mean(getty$nch)
sample_ind <<- sample(1:nrow(getty), 10, TRUE)
sample_ind1 <<- sample(1:30, 10, TRUE)
sample_ind2 <<- sample(1:73, 10, TRUE)
sample_ind3 <<- sample(1:100, 10, TRUE)
sample_clust <<- 1

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("Length of Words in the Gettysburg Address"),
    
    tabsetPanel(
        
        tabPanel("Full Text", fluid = TRUE,
            "Paragraph 1: Mean letter count = ", 
            round(mean(getty$nch[getty$para == 1]), 3), tags$br(), 
            p1, 
            tags$br(), tags$br(), 
            "Paragraph 2: Mean letter count = ", 
            round(mean(getty$nch[getty$para == 2]), 3), tags$br(), 
            p2, 
            tags$br(), tags$br(), 
            "Paragraph 3: Mean letter count = ", 
            round(mean(getty$nch[getty$para == 3]), 3), tags$br(), 
            p3,
            tags$br(), tags$br(), 
            "Overall mean letter count = ", 
            round(pop_mean, 3),
        ),
        tabPanel("SRS", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    sliderInput("nsrs", "Sample Size", 1, 50, 9, 1),
                    actionButton("doitsrs", "New Data")
                ),
                mainPanel(
                    "Sampled words:",
                    textOutput("srstext"), tags$br(),
                    plotOutput("srs")
                )
            )
        ),
        tabPanel("Stratified", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    sliderInput("nstrat1", "n from paragraph 1", min = 1, 
                        max = 30, value = 3),
                    sliderInput("nstrat2", "n from paragraph 2", min = 1, 
                        max = 73, value = 3),
                    sliderInput("nstrat3", "n from paragraph 3", min = 1, 
                        max = 169, value = 3),
                    actionButton("doitstrat", "New Data"),
                    tags$br(),
                    "For (approx) random, try n = (16, 39, 90)"
                ),
                mainPanel(
                    "Sampled words from paragraph 1:",
                    textOutput("strattext1"), tags$br(),
                    "Sampled words from paragraph 2:",
                    textOutput("strattext2"), tags$br(),
                    "Sampled words from paragraph 3:",
                    textOutput("strattext3"), tags$br(),
                    plotOutput("strat")
                )
            )
        ),
        tabPanel("Cluster", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    sliderInput("nclust", "Number of clusters", 
                        min = 1, 
                        max = 3, value = 1),
                    "Note: Clusters are chosen without replacement; this is not an SRS of clusters.",
                    actionButton("doitclust", "New Data")
                ),
                mainPanel(
                    htmlOutput("clusttext"),
                    plotOutput("clust")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    newdata <- reactive({
        input$doitsrs
        sample_ind <<- sort(sample(1:nrow(getty), input$nsrs, TRUE))
    })
    
    newdataclust <- reactive({
        input$doitclust
        sample_clust <<- sort(sample(1:3, input$nclust, FALSE))
    })
    
    newdatastrat <- reactive({
        input$doitstrat
        sample_ind1 <<- sort(sample(1:30, input$nstrat1, TRUE))
        sample_ind2 <<- sort(sample(1:73, input$nstrat2, TRUE))
        sample_ind3 <<- sort(sample(1:169, input$nstrat3, TRUE))
    })
    
    output$srstext <- renderText({
        newdata()
        input$doitsrs
        input$nsrs
        mysampwords <- getty$word[sample_ind]
        mysampwords
    })
    
    output$clusttext <- renderUI({
        newdataclust()
        input$doitclust
        input$nclust
        clust1 <- c()
        for(i in sample_clust){
            clust1[i] <- paste(getty$word[getty$para == i], collapse = " ")
        }
        clust1 <- clust1[!is.na(clust1)]
        
        HTML(paste(clust1, collapse = "</br></br>"))
    })
    
    output$strattext1 <- renderText({
        newdatastrat()
        input$doitstrat
        input$nstrat1; input$nstrat2; input$nstrat3
        mysampwords1 <- getty$word[getty$para == 1][sample_ind1]
        mysampwords1
    })
    
    output$strattext2 <- renderText({
        newdatastrat()
        input$doitstrat
        input$nstrat1; input$nstrat2; input$nstrat3
        mysampwords2 <- getty$word[getty$para == 2][sample_ind2]
        mysampwords2
    })
    
    output$strattext3 <- renderText({
        newdatastrat()
        input$doitstrat
        input$nstrat1; input$nstrat2; input$nstrat3
        mysampwords3 <- getty$word[getty$para == 3][sample_ind3]
        mysampwords3
    })
    
    output$srs <- renderPlot({
        input$doitsrs
        input$nsrs
        mysampchars <- getty$nch[sample_ind]
        
        ggplot() + 
            geom_bar(aes(x = mysampchars)) +
            scale_x_continuous(breaks = 0:20) +
            theme_bw() +
            geom_vline(xintercept = pop_mean, colour = 2, size = 1) +
            geom_vline(xintercept = mean(mysampchars), colour = 4, size = 1) +
            labs(title = "SRS from all words",
                subtitle = paste0("Sample mean = ", 
                    round(mean(mysampchars), 2), " (blue line), ",
                    "Pop. Mean = ", round(pop_mean, 2), " (red line)"),
                x = "Number of Letters", y = "Count")
        
    })
    
    output$strat <- renderPlot({
        input$doitstrat
        input$nstrat1; input$nstrat2; input$nstrat3
        mysampchars1 <- getty$nch[getty$para == 1][sample_ind1]
        mysampchars2 <- getty$nch[getty$para == 2][sample_ind2]
        mysampchars3 <- getty$nch[getty$para == 3][sample_ind3]
        
        p1 <- ggplot() + 
            geom_bar(aes(x = mysampchars1)) +
            scale_x_continuous(breaks = 0:20) +
            theme_bw() +
            geom_vline(xintercept = pop_mean, colour = 2, size = 1) +
            geom_vline(xintercept = mean(mysampchars1), colour = 4, size = 1) +
            labs(title = "SRS from p1",
                x = "Number of Letters", y = "Count")
        p2 <- ggplot() + 
            geom_bar(aes(x = mysampchars2)) +
            scale_x_continuous(breaks = 0:20) +
            theme_bw() +
            geom_vline(xintercept = pop_mean, colour = 2, size = 1) +
            geom_vline(xintercept = mean(mysampchars2), colour = 4, size = 1) +
            labs(title = "SRS from p2",
                x = "Number of Letters", y = "Count")
        p3 <- ggplot() + 
            geom_bar(aes(x = mysampchars3)) +
            scale_x_continuous(breaks = 0:20) +
            theme_bw() +
            geom_vline(xintercept = pop_mean, colour = 2, size = 1) +
            geom_vline(xintercept = mean(mysampchars3), colour = 4, size = 1) +
            labs(title = "SRS from p3",
                x = "Number of Letters", y = "Count")
        p123 <- ggplot() + 
            geom_bar(aes(x = c(mysampchars1, mysampchars2, mysampchars3))) +
            scale_x_continuous(breaks = 0:20) +
            theme_bw() +
            geom_vline(xintercept = pop_mean, colour = 2, size = 1) +
            geom_vline(xintercept = mean(c(mysampchars1, mysampchars2, 
                    mysampchars3)), 
                colour = 4, size = 1) +
            labs(title = "Stratified Random Sample",
                subtitle = paste0("Sample mean = ", 
                    round(mean(c(mysampchars1, mysampchars2, mysampchars3)), 2), 
                    " (blue line), ",
                    "Pop. Mean = ", round(pop_mean, 2), " (red line)"),
                x = "Number of Letters", y = "Count")
        
        (p1 + p2 + p3) / p123
    })
    
    output$clust <- renderPlot({
        input$doitclust
        input$nclust
        mywords <- getty$nch[getty$para %in% sample_clust]
        
        if(length(sample_clust) == 1){
            mytitle <- paste0("Cluster ", sample_clust)
        } else if(length(sample_clust) == 2){
            mytitle <- paste0("Cluster ", sample_clust[1], 
                " and ", sample_clust[2])
        } else {
            mytitle <- "All clusters"
        }
        
        ggplot() + 
            geom_bar(aes(x = mywords)) +
            theme_bw() +
            geom_vline(xintercept = pop_mean, colour = 2, size = 1) +
            geom_vline(xintercept = mean(mywords), colour = 4, size = 1) +
            labs(title = mytitle,
                subtitle = paste0("Sample mean = ", 
                    round(mean(mywords), 2), 
                    " (blue line), ",
                    "Pop. Mean = ", round(pop_mean, 2), " (red line)"),
                x = "Number of Letters", y = "Count")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
