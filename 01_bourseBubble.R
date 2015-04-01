library("swiTheme")
library("swiRcharts")
source("~/swissinfo/_helpers/helpers.R")

############################################################################################
###		Get data and translations
############################################################################################

data <- read.csv("bourseTertiaires.csv", check.names = FALSE, stringsAsFactors = F)

# cbind the 2 letters abbreviations
data <- cbind(iso2 = c('CH', canton_namesStrict(data[-1,1])), data)
colnames(data)[2] <- 'canton'
write.csv(data, "bourseTertiaires_01.csv", row.names = F)

data <- read.csv("bourseTertiaires_01.csv", check.names = FALSE, stringsAsFactors = F)

## load translation

trad <- read.csv("01_trad.csv", row.names = 1, stringsAsFactors = F)


############################################################################################
###		Compute
############################################################################################

data$montantBenef <- data$Montant / data$Bénéficiaires

data$pourcBenef <- (data$Bénéficiaires / data[,"Nombre d'étudiants"]) * 100

############################################################################################
###		highcharts bubble chart: %
############################################################################################

for (i in 1:ncol(trad)) {

	lang <- colnames(trad)[i]
	output.html <- paste("01_bourse_bubble_", lang, ".html", sep ="")

	dd <- data
	dd$color <- swi_rpal[1]

	dd[,'canton'] <- trad[1:27,lang]

	a <- rCharts::Highcharts$new()
	a$series(hSeries( x = dd[,'pourcBenef'], y = dd[,'montantBenef'], z = dd[,"Nombre d'étudiants"],
		name = dd[,'iso2'], color = dd$color, series = dd$canton))

	a$chart(zoomType = "xy", type = "bubble", height = 400)
	a$plotOptions(bubble = list(dataLabels = list(enabled = T, verticalAlign = "middle", style = list(textShadow = 'none', fontSize = "0.6em"),
		color = 'black', useHTML = T, formatter = "#! function() { return this.point.name; } !#"),
		minSize = 15, maxSize = 75))

	#formatter <- "#! function() { return '<b>' + this.series.name + '</b><br/><br/>' + this.x + '%  -  ' + this.y + '%';} !#"

	x.tooltip <- gsub("'", " ", trad['xlab',lang])
	y.tooltip <- gsub("'", " ", trad['ylab',lang])
	z.tooltip <- gsub("'", " ", trad['ztool',lang])

	formatter <- paste("#! function() {", "return '<strong>' + this.series.name + ",
		 "'</strong><div class=\"tooltip\" style=\"color:#686868;font-size:0.8em\"><br/>", y.tooltip, ": <b>' + this.y + '", "</b><br/>",  x.tooltip, ": <b>' + this.x + '",
		 "</b><br/>", z.tooltip, ": ' + this.point.z +","'</div>';",
		"} !#", sep = "")

	a$tooltip( useHTML = T, formatter = formatter)

	a$yAxis(title = list(text = trad['ylab',lang], style = list(fontWeight = "bold")), labels = list(format = '{value}'), floor = 4000, ceiling = 14000)
	a$xAxis(title = list(text = trad['xlab',lang], style = list(fontWeight = "bold")), labels = list(format = '{value}%'), min = 0, max = 25,
		lineColor = list ('#FF0000'))
	a$legend(enabled = F)
	#a

	hChart.html <- tempfile("hchart_labelledBubble.html")
	a$save(hChart.html)

	# Example of converting a highcharts-rCharts html chart into a responsive one

	hChart2responsiveHTML(hChart.html, output.html = output.html, h2 = trad['title',lang], descr = trad['descr',lang],
		source = trad['source',lang], h3 = "")

}





