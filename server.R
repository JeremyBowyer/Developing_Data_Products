library(shiny)
library(rvest)
library(zoo)
library(ggplot2)
library(reshape2)
library(markdown)


# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

		#### REPUBLICAN SECTION ####
		## load in republican data
		Republican.polls <- read_html("http://www.realclearpolitics.com/epolls/2016/president/us/2016_republican_presidential_nomination-3823.html")
		Republican.names <- html_text(html_nodes(Republican.polls,"th"))
		Republican.poll.data <- html_text(html_nodes(Republican.polls,"td"))
		Republican.poll.sites <- html_text(html_nodes(Republican.polls,".noCenter"))
		Republican.poll.sites <- unique(Republican.poll.sites)
		
		## clean data
		
		# clean candidate list
		Republican.names <- unique(Republican.names)
		Republican.names <- Republican.names[!(Republican.names %in% c("Date","Poll","Spread"))]
		Republican.names <- gsub(" ","",Republican.names)
		
		# remove upper table
		Republican.poll.data <- Republican.poll.data[grep("RCP Average", Republican.poll.data)[2]:length(Republican.poll.data)]
		
		# rename pollsters
		Republican.poll.data <- replace(Republican.poll.data,grep("FOX News",Republican.poll.data),"FOX News")
		Republican.poll.data <- replace(Republican.poll.data,grep("Quinnipiac",Republican.poll.data),"Quinnipiac")
		Republican.poll.data <- replace(Republican.poll.data,grep("CNN/ORC",Republican.poll.data),"CNN/ORC")
		Republican.poll.data <- replace(Republican.poll.data,grep("PPP",Republican.poll.data),"PPP (D)")
		Republican.poll.data <- replace(Republican.poll.data,grep("ABC News/Wash Post",Republican.poll.data),"ABC News/Wash Post")
		Republican.poll.data <- replace(Republican.poll.data,grep("ABC/Wash Post",Republican.poll.data),"ABC News/Wash Post")
		Republican.poll.data <- replace(Republican.poll.data,grep("Monmouth",Republican.poll.data),"Monmouth")
		Republican.poll.data <- replace(Republican.poll.data,grep("NBC/WSJ",Republican.poll.data),"NBC/WSJ")
		Republican.poll.data <- replace(Republican.poll.data,grep("NBC/Marist",Republican.poll.data),"NBC/Marist")
		Republican.poll.data <- replace(Republican.poll.data,grep("CBS/NY Times",Republican.poll.data),"CBS/NY Times")
		Republican.poll.data <- replace(Republican.poll.data,grep("CBS News",Republican.poll.data),"CBS News")
		Republican.poll.data <- replace(Republican.poll.data,grep("USA Today/Suffolk",Republican.poll.data),"USA Today/Suffolk")
		Republican.poll.data <- replace(Republican.poll.data,grep("USAT/Suffolk",Republican.poll.data),"USA Today/Suffolk")
		Republican.poll.data <- replace(Republican.poll.data,grep("IBD/TIPP",Republican.poll.data),"IBD/TIPP")
		Republican.poll.data <- replace(Republican.poll.data,grep("Bloomberg",Republican.poll.data),"Bloomberg")
		Republican.poll.data <- replace(Republican.poll.data,grep("McClatchy/Marist",Republican.poll.data),"McClatchy/Marist")
		Republican.poll.data <- replace(Republican.poll.data,grep("Pew Research",Republican.poll.data),"Pew Research")
		Republican.poll.data <- replace(Republican.poll.data,grep("Rasmussen",Republican.poll.data),"Rasmussen")
		Republican.poll.data <- replace(Republican.poll.data,grep("Reason-Rupe",Republican.poll.data),"Reason-Rupe")
		Republican.poll.data <- replace(Republican.poll.data,grep("WPA",Republican.poll.data),"WPA (R)")
		
		# replace -- with NAs
		Republican.poll.data <- replace(Republican.poll.data,grep("--",Republican.poll.data),NA)
		
		# remove spread
		Republican.poll.data <- Republican.poll.data[-grep("\\+",Republican.poll.data)]
		Republican.poll.data <- Republican.poll.data[-grep("Tie",Republican.poll.data)]
		
		# extract poll names
		pollster.list <- Republican.poll.data[1]
		for (i in 1:((length(Republican.poll.data)/(2+length(Republican.names)))-1)) {
			pollster.list[(length(pollster.list)+1)] <- Republican.poll.data[((i*(2+length(Republican.names)))+1)]
		}
		
		# extract date range
		date.range.list <- Republican.poll.data[2]
		for (i in 1:((length(Republican.poll.data)/(2+length(Republican.names)))-1)) {
			date.range.list[(length(date.range.list)+1)] <- Republican.poll.data[((i*(2+length(Republican.names)))+2)]
		}
		
		# combine poll names, date range then extract and combine poll data
		Republican.polls.df <- cbind(pollster.list,date.range.list)
		for (i in Republican.names) {
			candidatevector <- Republican.poll.data[2+grep(i,Republican.names)]
			for (y in 1:((length(Republican.poll.data)/(2+length(Republican.names)))-1)) {
				candidatevector[(length(candidatevector)+1)] <- Republican.poll.data[((y*(2+length(Republican.names)))+(2+grep(i,Republican.names)))]
			}
			Republican.polls.df <- cbind(Republican.polls.df,candidatevector)
		}
		
		# remove RCP Average because it is not reproducible
		Republican.polls.df <- Republican.polls.df[-grep("RCP Average",Republican.polls.df[,1]),]
		Republican.polls.df <- as.data.frame(Republican.polls.df)
		
		# split and combine date range into start date and end date columns
		Republican.polls.df <- cbind(Republican.polls.df,data.frame(do.call('rbind', strsplit(as.character(Republican.polls.df$date.range.list),' - ',fixed=TRUE))))
		names(Republican.polls.df) <- c("Poll","Date.Range",Republican.names,"Start.Date","End.Date")
		
		
		## Assign correct year to dates
		# start date year
		year <- as.numeric(format(Sys.Date(), format = "%Y"))
		Republican.polls.df$Start.Year <- NA
		Republican.polls.df$Start.Year[1] <- paste0(as.character(Republican.polls.df[1,"Start.Date"]),"/",year)
		for (i in 2:nrow(Republican.polls.df)) {
			if (abs(as.numeric(as.Date(Republican.polls.df[i-1,"Start.Date"], format = "%m/%d") - as.Date(Republican.polls.df[(i),"Start.Date"], format = "%m/%d"))) > 300) {year <- year-1}
			Republican.polls.df$Start.Year[i] <- paste0(as.character(Republican.polls.df[i,"Start.Date"]),"/",year)
		}
		
		# end date year
		year <- as.numeric(format(Sys.Date(), format = "%Y"))-1
		Republican.polls.df$End.Year <- NA
		Republican.polls.df$End.Year[1] <- paste0(as.character(Republican.polls.df[1,"End.Date"]),"/",year)
		for (i in 2:nrow(Republican.polls.df)) {
			if (abs(as.numeric(as.Date(Republican.polls.df[i-1,"End.Date"], format = "%m/%d") - as.Date(Republican.polls.df[(i),"End.Date"], format = "%m/%d"))) > 300) {year <- year-1}
			Republican.polls.df$End.Year[i] <- paste0(as.character(Republican.polls.df[i,"End.Date"]),"/",year)
		}
	
		
		# rearrange columns
		Republican.polls.df <- Republican.polls.df[,c("Poll","Date.Range","Start.Date","End.Date","Start.Year","End.Year",Republican.names)]
		Republican.names <- Republican.names[!Republican.names %in% c("Sample")]
		
		###########################################
		
		## create data frame of average polls
		
		# generate date list
		datelist <- seq.Date(from = as.Date("2015-01-01", format = "%Y-%m-%d"), to = Sys.Date(), by = "day")
		
		# create data frame
		Republican.avg.polls <- data.frame(matrix(NA, ncol = length(c("Date",Republican.names)), nrow = length(datelist)))
		names(Republican.avg.polls) <- c("Date",Republican.names)
		Republican.avg.polls$Date <- datelist
		Republican.avg.polls <- Republican.avg.polls[order(Republican.avg.polls$Date, decreasing = TRUE),]
		rownames(Republican.avg.polls) <- c(1:nrow(Republican.avg.polls))
		
		# calculate average polls for each candidate and populate Republican.avg.polls data frame
		for (i in Republican.names) {
			for (y in 1:nrow(Republican.avg.polls)){
				Republican.avg.polls[y,i] <- mean(as.numeric(as.character(Republican.polls.df[as.Date(Republican.polls.df$Start.Year, format = "%m/%d/%Y") >= Republican.avg.polls[y,"Date"]-14 &
														as.Date(Republican.polls.df$Start.Year, format = "%m/%d/%Y") <= Republican.avg.polls[y,"Date"],i])),
						na.rm = TRUE)
			}
		}
		
		Republican.avg.polls <- as.data.frame(lapply(Republican.avg.polls,FUN = function(x) replace(x,is.nan(x),NA)))
		
		#### DEMOCRAT SECTION ####
		## load in democrat data
		Democrat.polls <- read_html("http://www.realclearpolitics.com/epolls/2016/president/us/2016_democratic_presidential_nomination-3824.html")
		Democrat.names <- html_text(html_nodes(Democrat.polls,"th"))
		Democrat.poll.data <- html_text(html_nodes(Democrat.polls,"td"))
		Democrat.poll.sites <- html_text(html_nodes(Democrat.polls,".noCenter"))
		Democrat.poll.sites <- unique(Democrat.poll.sites)
		
		## clean data
		
		# clean candidate list
		Democrat.names <- unique(Democrat.names)
		Democrat.names <- Democrat.names[!(Democrat.names %in% c("Date","Poll","Spread","Sample","MoE"))]
		Democrat.names <- gsub(" ","",Democrat.names)
		
		# remove upper table
		Democrat.poll.data <- Democrat.poll.data[grep("RCP Average", Democrat.poll.data)[2]:length(Democrat.poll.data)]
		
		# rename pollsters
		Democrat.poll.data <- replace(Democrat.poll.data,grep("FOX News",Democrat.poll.data),"FOX News")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("Quinnipiac",Democrat.poll.data),"Quinnipiac")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("CNN/ORC",Democrat.poll.data),"CNN/ORC")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("PPP",Democrat.poll.data),"PPP (D)")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("ABC News/Wash Post",Democrat.poll.data),"ABC News/Wash Post")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("ABC/Wash Post",Democrat.poll.data),"ABC News/Wash Post")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("Monmouth",Democrat.poll.data),"Monmouth")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("NBC/WSJ",Democrat.poll.data),"NBC/WSJ")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("NBC/Marist",Democrat.poll.data),"NBC/Marist")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("CBS/NY Times",Democrat.poll.data),"CBS/NY Times")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("CBS News",Democrat.poll.data),"CBS News")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("USA Today/Suffolk",Democrat.poll.data),"USA Today/Suffolk")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("USAT/Suffolk",Democrat.poll.data),"USA Today/Suffolk")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("IBD/TIPP",Democrat.poll.data),"IBD/TIPP")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("Bloomberg",Democrat.poll.data),"Bloomberg")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("McClatchy/Marist",Democrat.poll.data),"McClatchy/Marist")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("Pew Research",Democrat.poll.data),"Pew Research")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("Rasmussen",Democrat.poll.data),"Rasmussen")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("Reason-Rupe",Democrat.poll.data),"Reason-Rupe")
		Democrat.poll.data <- replace(Democrat.poll.data,grep("WPA",Democrat.poll.data),"WPA (R)")
		
		# replace -- with NAs
		Democrat.poll.data <- replace(Democrat.poll.data,grep("--",Democrat.poll.data),NA)
		
		# remove spread
		Democrat.poll.data <- Democrat.poll.data[-grep("\\+",Democrat.poll.data)]
		if(length(grep("Tie",Democrat.poll.data))!= 0){
			Democrat.poll.data <- Democrat.poll.data[-grep("Tie",Democrat.poll.data)]
		}
		
		# extract poll names
		pollster.list <- Democrat.poll.data[1]
		for (i in 1:((length(Democrat.poll.data)/(4+length(Democrat.names)))-1)) {
			pollster.list[(length(pollster.list)+1)] <- Democrat.poll.data[((i*(4+length(Democrat.names)))+1)]
		}
		
		# extract date range
		date.range.list <- Democrat.poll.data[2]
		for (i in 1:((length(Democrat.poll.data)/(4+length(Democrat.names)))-1)) {
			date.range.list[(length(date.range.list)+1)] <- Democrat.poll.data[((i*(4+length(Democrat.names)))+2)]
		}
		
		# combine poll names, date range then extract and combine poll data
		Democrat.polls.df <- cbind(pollster.list,date.range.list)
		for (i in Democrat.names) {
			candidatevector <- Democrat.poll.data[2+grep(i,Democrat.names)]
			for (y in 1:((length(Democrat.poll.data)/(4+length(Democrat.names)))-1)) {
				candidatevector[(length(candidatevector)+1)] <- Democrat.poll.data[((y*(4+length(Democrat.names)))+(4+grep(i,Democrat.names)))]
			}
			Democrat.polls.df <- cbind(Democrat.polls.df,candidatevector)
		}
		
		# remove RCP Average because it is not reproducible
		Democrat.polls.df <- Democrat.polls.df[-grep("RCP Average",Democrat.polls.df[,1]),]
		Democrat.polls.df <- as.data.frame(Democrat.polls.df)
		
		# split and combine date range into start date and end date columns
		Democrat.polls.df <- cbind(Democrat.polls.df,data.frame(do.call('rbind', strsplit(as.character(Democrat.polls.df$date.range.list),' - ',fixed=TRUE))))
		names(Democrat.polls.df) <- c("Poll","Date.Range",Democrat.names,"Start.Date","End.Date")
		
		
		## Assign correct year to dates
		
		# start date year
		year <- as.numeric(format(Sys.Date(), format = "%Y"))
		Democrat.polls.df$Start.Year <- NA
		Democrat.polls.df$Start.Year[1] <- paste0(as.character(Democrat.polls.df[1,"Start.Date"]),"/",year)
		for (i in 2:nrow(Democrat.polls.df)) {
			if (abs(as.numeric(as.Date(Democrat.polls.df[i-1,"Start.Date"], format = "%m/%d") - as.Date(Democrat.polls.df[(i),"Start.Date"], format = "%m/%d"))) > 300) {year <- year-1}
			Democrat.polls.df$Start.Year[i] <- paste0(as.character(Democrat.polls.df[i,"Start.Date"]),"/",year)
		}
		
		# end date year
		year <- as.numeric(format(Sys.Date(), format = "%Y"))-1
		Democrat.polls.df$End.Year <- NA
		Democrat.polls.df$End.Year[1] <- paste0(as.character(Democrat.polls.df[1,"End.Date"]),"/",year)
		for (i in 2:nrow(Democrat.polls.df)) {
			if (abs(as.numeric(as.Date(Democrat.polls.df[i-1,"End.Date"], format = "%m/%d") - as.Date(Democrat.polls.df[(i),"End.Date"], format = "%m/%d"))) > 300) {year <- year-1}
			Democrat.polls.df$End.Year[i] <- paste0(as.character(Democrat.polls.df[i,"End.Date"]),"/",year)
		}
		
		# rearrange columns
		Democrat.polls.df <- Democrat.polls.df[,c("Poll","Date.Range","Start.Date","End.Date","Start.Year","End.Year",Democrat.names)]
		
		###########################################
		
		## create data frame of average polls
		
		# generate date list
		datelist <- seq.Date(from = as.Date("2015-01-01", format = "%Y-%m-%d"), to = Sys.Date(), by = "day")
		
		# create data frame
		Democrat.avg.polls <- data.frame(matrix(NA, ncol = length(c("Date",Democrat.names)), nrow = length(datelist)))
		names(Democrat.avg.polls) <- c("Date",Democrat.names)
		Democrat.avg.polls$Date <- datelist
		Democrat.avg.polls <- Democrat.avg.polls[order(Democrat.avg.polls$Date, decreasing = TRUE),]
		rownames(Democrat.avg.polls) <- c(1:nrow(Democrat.avg.polls))
		
		# calculate average polls for each candidate and populate Democrat.avg.polls data frame
		for (i in Democrat.names) {
			for (y in 1:nrow(Democrat.avg.polls)){
				Democrat.avg.polls[y,i] <- mean(as.numeric(as.character(Democrat.polls.df[as.Date(Democrat.polls.df$Start.Year, format = "%m/%d/%Y") >= Democrat.avg.polls[y,"Date"]-14 &
														as.Date(Democrat.polls.df$Start.Year, format = "%m/%d/%Y") <= Democrat.avg.polls[y,"Date"],i])),
						na.rm = TRUE)
			}
		}
		
		Democrat.avg.polls <- as.data.frame(lapply(Democrat.avg.polls,FUN = function(x) replace(x,is.nan(x),NA)))
		names(Democrat.avg.polls) <- c("Date",Democrat.names)
		
		# create dynamic UI candidate list
		output$candidatelist <- renderUI({
					candidates <- eval(as.name(paste0(input$party,".names")))
					checkboxGroupInput("candidates",
							"Candidates:",
							choices = candidates)
				})
		# generate plot based on UI inputs
		reactive.data <- reactive({
					plotdata <- melt(eval(as.name(paste0(input$party,".avg.polls"))), 
									value.name = "Average.Poll",
									variable.name = "candidate",
									id.vars = "Date")
					plotdata <- plotdata[plotdata$candidate %in% input$candidates &
									plotdata$Date > as.Date(input$date[1], format = "%Y-%m-%d") &
									plotdata$Date < as.Date(input$date[2], format = "%Y-%m-%d"),]
				
				})
				
		output$pollPlot <- renderPlot({
					p <- ggplot(reactive.data()) +
							geom_line(aes(x = Date,
											y = Average.Poll,
											color = candidate),
											size=2)
					print(p)
				})		

	

})