library(shiny)

all_states <- c('Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware','District of Columbia','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming')

data_categories <- c('Median List Price ($)','Median List Price Per Sq Ft ($)','Median Sale Price - Seasonally Adjusted ($)','Monthly For-Sale Inventory (Normalized)','Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted','Monthly Home Sales (Normalized)','Monthly Home Sales (Normalized) - Seasonally Adjusted','New Monthly For-Sale Inventory (Normalized)','New Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted','Sale-to-list ratio')

units <- c('$1000','$','$1000','Units/1000 people','Units/1000 people','Fraction of Inventory','Fraction of Inventory','Fraction of Inventory','Fraction of Inventory',' ')

titles <- c('Median List Price','Median List Price Per Sq Ft','Median Sale Price - Seasonally Adjusted','Monthly For-Sale Inventory (Normalized)','Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted','Monthly Home Sales (Normalized)','Monthly Home Sales (Normalized) - Seasonally Adjusted','New Monthly For-Sale Inventory (Normalized)','New Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted','Sale-to-list ratio') #slightly different titles for data categories used for display

abbrevs <-c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')

names(units) <- data_categories
names(titles) <- data_categories
names(all_states) <- abbrevs

cf <- read.csv('www/All_Data_County.csv') #need to get list of counties from here for ui
cf$time <- as.Date(cf$time,format='%Y-%m-%d')
df <- read.csv('www/All_Data_State.csv')
df$time <- as.Date(df$time,format='%Y-%m-%d')
categCols <- c('time','DataCategory')
df <- merge(df,cf)

county_parse <- function(string) {
	k <- nchar(string)
	county <- substr(string, 1, k-3)
	state <- substr(string, k-1, k)
	return(c(county,state))
}

all_counties <- list()

for (state in all_states) {
	all_counties[[state]] <- vector(mode='character')
}

for (county in names(cf)) {
	county <- county_parse(county)
	all_counties[[all_states[county[2]]]] <- c(all_counties[[all_states[county[2]]]], county[1]) #adds each county to the appropriate vector in the list indexed by state name
}

respace <- function(string) { #helper function to return underscores in county names to spaces for input display
	return(gsub('_',' ',string,fixed=TRUE))
}

ui <- fluidPage(tags$header(tags$div(class='titlebox',tags$h1('Real estate data'))),
	tabsetPanel(
		tabPanel('Time Trend Data',
			checkboxGroupInput('stateList','States',all_states,selected=c('California','Florida','Massachusetts','Pennsylvania'),inline=TRUE),
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
				tabPanel('Notes','Selected Home Listings and Sales data downloaded from Zillow at https://www.zillow.com/research/data/',tags$br(),tags$br(),'2010 Census Data used for normalization by population retrieved from: https://www.census.gov/prod/cen2010/briefs/c2010br-08.pdf, https://web.archive.org/web/20101225031104/http://2010.census.gov/2010census/data/apportionment-pop-text.php, and https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html',tags$br(),tags$br(),'Folder of Zillow data files and Python 3 files zillow_states.py  and zillow_counties.py used for preprocessing at https://github.com/steverbates/realestate')
				)
			),
		tabPanel('County Selector',#do.call solution credit to stack overflow
			do.call(tabsetPanel, c(lapply(abbrevs, function(state) { #'state' referenced here technically being the abbreviation
				tabPanel(state,
					checkboxGroupInput(state,NULL,lapply(all_counties[[all_states[state]]],respace),inline=TRUE))
			}
				) 
				)
			)
			),
		tabPanel('Correlations',tags$p('How closely correlated are these indicators across regions?  For a given data category this tool plots the values at each time in two different states or counties against one another, and calculates the linear regression line and correlation coefficient.'),
			fluidRow(
				column(2,selectInput('st1','First State',abbrevs,selected='AL')),
				column(3,selectInput('co1','First County',c('Entire State',lapply(all_counties['Alabama'],respace)),selected='Entire State'))
				),
			fluidRow(
				column(2,selectInput('st2','Second State',abbrevs,selected='AZ')),
				column(3,selectInput('co2','Second County',c('Entire State',lapply(all_counties['Arizona'],respace)),selected='Entire State'))
				),
			selectInput('categ','Category',data_categories),
			actionButton(inputId = 'calc',label = 'Calculate'),
			imageOutput('corr',inline=TRUE)
			)
		)
	)

server <- function(input, output, session) {
	observeEvent(c(input$stateList,sapply(abbrevs, function(x) input[[x]])),{
		selection <- sapply(input$stateList, function(x) gsub(' ','_',x,fixed=TRUE))
		legend_list <- input$stateList #need input$stateList for formatting with spaces instead of underscores
		for (state in abbrevs) { #'state' referenced here technically being the abbreviation
			county_selection <- input[[state]]
			selection <- c(selection,sapply(county_selection, function(county) paste(gsub(' ','_',county,fixed=TRUE),state,sep='_')))
			legend_list <- c(legend_list, sapply(county_selection, function(county) paste(county,state,sep=', ')))
		}
		selection <- as.vector(selection,mode='character')
		legend_list <- as.vector(legend_list,mode='character')
		df_slice <- df[c('time','DataCategory',selection)] #slice out state and county selection here
		for (category in data_categories) {
			sf <- df_slice[df_slice$DataCategory == category,c('time',as.vector(selection))] #
			if (units[category] == '$1000') {
				sf[,!(names(sf) %in% categCols)] <- 0.001*sf[,!(names(sf) %in% categCols)] 
			}
			if (units[category] == 'Units/1000 people') {
				sf[,!(names(sf) %in% categCols)] <- 1000*sf[,!(names(sf) %in% categCols)] 
			}
			png(sprintf('%s.png',category),width=1400)
			pal <- rainbow(length(selection))
			i <- 1
			m <- min(sf[as.vector(selection,mode='character')],na.rm=TRUE) #find minimum/maximum across selection lists 
			n <- max(sf[as.vector(selection,mode='character')],na.rm=TRUE)
			for (region in selection) {
				x <- sf[c('time',region)]
				if (i == 1) {
					plot(x,col=pal[i],type='l',main=titles[category],xlab='Time',ylab=units[category],ylim=c(m,n+0.4*(n-m)))
				} else {
					lines(x,col=pal[i])
				}
				i <- i+1
				}
			legend('top',legend=legend_list,ncol=((length(selection)-1)%/%5+1),col=pal,lty=1) 
			#number of columns: 1 for 1-5 regions, 2 for 6-10, 3 for 11-15, etc
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
	observeEvent(input$st1,{
		updateSelectizeInput(session, 'co1', label='First County', choices=c('Entire State',lapply(all_counties[all_states[input$st1]],respace)), selected='Entire State')
	})
	observeEvent(input$st2,{
		updateSelectizeInput(session, 'co2', label='Second County', choices=c('Entire State',lapply(all_counties[all_states[input$st2]],respace)), selected='Entire State')
	})
	observeEvent(input$calc,{
		if (input$co1=='Entire State') {
			region1 <- gsub(' ','_',all_states[input$st1],fixed=TRUE)
			xlab <- all_states[input$st1]
			} else {
				region1 <- paste(gsub(' ','_',input$co1,fixed=TRUE),input$st1,sep='_')
				xlab <- paste(input$co1,input$st1,sep=', ')
			}
		if (input$co2=='Entire State') {
			region2 <- gsub(' ','_',all_states[input$st2],fixed=TRUE)
			ylab <- all_states[input$st2]
			} else {
				region2 <- paste(gsub(' ','_',input$co2,fixed=TRUE),input$st2,sep='_')
				ylab <- paste(input$co2,input$st2,sep=', ')
			}
		df_corr <- df[df$DataCategory == input$categ,c(region1,region2)]
		reg_mod <- lm(df_corr[,region2]~df_corr[,region1]) #with this ordering region1 in position of independent variable in linear model
		png('corr.png',width=1400)
		plot(df_corr,main=titles[input$categ],xlab=xlab,ylab=ylab)
		x <- as.vector(c(par('usr')[1],x2 <- par('usr')[2])) #get left and right limits of graph window for plotting regresion line
		b <- reg_mod[1]$coefficients[1]
		m <- reg_mod[1]$coefficients[2]
		r2 <- summary(reg_mod)$r.squared
		y <- as.vector(c(m*x[1] + b,m*x[2] + b))
		lines(x,y)
		if (m<1) {
			spot <- 'topright'
			} else {
				spot <- 'topleft'
			}
		legend(spot,sprintf('R squared: %f',r2))
		dev.off()
		output$corr <- renderImage(list(src='corr.png'))
	})
}

shinyApp(ui = ui, server = server)