library(shiny)

ui <- fluidPage(tags$header(tags$div(class='titlebox',tags$h1('Real estate data'))),checkboxGroupInput('stateList', 'States',
                     c('Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware','District of Columbia','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming'), selected=c('California','Florida','Massachusetts','Pennsylvania'),inline=TRUE),
	tabsetPanel(
		tabPanel('Median List Price',imageOutput('mlp', inline=TRUE)),
		tabPanel('Median List Price Per Sq Ft',imageOutput('mlppsf', inline=TRUE)),
		tabPanel('Median Sale Price - Seasonally Adjusted',imageOutput('mspsa', inline=TRUE)),
		tabPanel('Monthly For-Sale Inventory (Normalized)',imageOutput('mfsir', inline=TRUE)),
		tabPanel('Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted',imageOutput('mfsisa', inline=TRUE)),
		tabPanel('Monthly Home Sales (Normalized)',imageOutput('mhsr', inline=TRUE)),
		tabPanel('Monthly Home Sales (Normalized) - Seasonally Adjusted',imageOutput('mhssa', inline=TRUE)),
		tabPanel('New Monthly For-Sale Inventory (Normalized)',imageOutput('nmfsir', inline=TRUE)),
		tabPanel('New Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted',imageOutput('nmfsisa', inline=TRUE)),
		tabPanel('Sale-to-list ratio',imageOutput('stlr', inline=TRUE)),
		tabPanel('Notes','Selected Home Listings and Sales data downloaded from Zillow at https://www.zillow.com/research/data/',tags$br(),tags$br(),'2010 Census Data used for normalization by population retrieved from: https://www.census.gov/prod/cen2010/briefs/c2010br-08.pdf and https://web.archive.org/web/20101225031104/http://2010.census.gov/2010census/data/apportionment-pop-text.php',tags$br(),tags$br(),'Folder of Zillow data files and Python 3 file zillow_states.py used for preprocessing at https://github.com/steverbates/realestate')
	)
)

server <- function(input, output, session) {
	data_categories <- c('Median List Price ($)','Median List Price Per Sq Ft ($)','Median Sale Price - Seasonally Adjusted ($)','Monthly For-Sale Inventory (Normalized)','Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted','Monthly Home Sales (Normalized)','Monthly Home Sales (Normalized) - Seasonally Adjusted','New Monthly For-Sale Inventory (Normalized)','New Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted','Sale-to-list ratio')
	units <- c('$1000','$','$1000','Units/1000 people','Units/1000 people','Fraction of Inventory','Fraction of Inventory','Fraction of Inventory','Fraction of Inventory',' ')
	titles <- c('Median List Price','Median List Price Per Sq Ft','Median Sale Price - Seasonally Adjusted','Monthly For-Sale Inventory (Normalized)','Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted','Monthly Home Sales (Normalized)','Monthly Home Sales (Normalized) - Seasonally Adjusted','New Monthly For-Sale Inventory (Normalized)','New Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted','Sale-to-list ratio') #slightly different titles for data categories used for display
	names(units) <- data_categories
	names(titles) <- data_categories

	df <- read.csv('www/All_Data_State.csv')
	df$time <- as.Date(df$time,format='%Y-%m-%d')
	categCols <- c('time','DataCategory')
	observeEvent(input$stateList ,{
		states <- sapply(input$stateList, function(x) gsub(' ','_',x,fixed=TRUE)) #df column names need to be formatted with underscores
		df_slice <- df[c('time','DataCategory',states)] #slice out state selection here
		for (category in data_categories) {
			sf <- df_slice[df_slice$DataCategory == category,c('time',states)] #
			if (units[category] == '$1000') {
				sf[,!(names(sf) %in% categCols)] <- 0.001*sf[,!(names(sf) %in% categCols)] 
			}
			if (units[category] == 'Units/1000 people') {
				sf[,!(names(sf) %in% categCols)] <- 1000*sf[,!(names(sf) %in% categCols)] 
			}
			png(sprintf('%s.png',category),width=1400)
			pal <- rainbow(length(states))
			i <- 1
			m <- min(sf[states],na.rm=TRUE)
			n <- max(sf[states],na.rm=TRUE)
			for (state in states) {
				x <- sf[c('time',state)]
				if (i == 1) {
					plot(x,col=pal[i],type='l',main=titles[category],xlab='Time',ylab=units[category],ylim=c(m,n+0.4*(n-m)))
				} else {
					lines(x,col=pal[i])
				}
				i <- i+1
				}
			legend('top',legend=input$stateList,ncol=((length(states)-1)%/%5+1),col=pal,lty=1) #need input$stateList for formatting with spaces instead of underscores
			#number of columns: 1 for 1-5 states, 2 for 6-10, 3 for 11-15, etc
			dev.off()
		}
		output$mlp <- renderImage(list(src='Median List Price ($).png'))
		output$mlppsf <- renderImage(list(src='Median List Price Per Sq Ft ($).png'))
		output$mspsa <- renderImage(list(src='Median Sale Price - Seasonally Adjusted ($).png'))
		output$mfsir <- renderImage(list(src='Monthly For-Sale Inventory (Normalized).png'))
		output$mfsisa <- renderImage(list(src='Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted.png'))
		output$mhsr <- renderImage(list(src='Monthly Home Sales (Normalized).png'))
		output$mhssa <- renderImage(list(src='Monthly Home Sales (Normalized) - Seasonally Adjusted.png'))
		output$nmfsir <- renderImage(list(src='New Monthly For-Sale Inventory (Normalized).png'))
		output$nmfsisa <- renderImage(list(src='New Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted.png'))
		output$stlr <- renderImage(list(src='Sale-to-list ratio.png'))
	})
}

shinyApp(ui = ui, server = server)