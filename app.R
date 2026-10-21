# load Shiny
library(shiny)

# load clinical trial dataset(trial_data.rds)
trial <- readRDS("trial_data.rds")

"Step 1: Add the User Interface(UI)
UI - what the users sees
"

ui <- fluidPage(
	titlePanel("Clinical Trial Dashboard"),
	selectInput(
		"group",
		"Select Treatment group:",
		choices = c("New", "Placebo")
	),
	h3("Blood Pressure Data"),
	tableOutput("results"),

	h3("Average change from Baseline"),
	textOutput("average")
)


"Step 2: Add the Server
SERVER - Performs calculations
"

server <- function (input, output)
	{
		filtered <- reactive({
			trial[trial$group == input$group, ]
			
				})
		output$results <- renderTable({
			filtered()
				})
		output$average <- renderText({
			mean(filtered()$CHG)
				})
}

shinyApp(ui = ui, server = server) 
