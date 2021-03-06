#' ---
#' title: "How plausible are more than 1MM Refugees hitting the sea shores of Europe?"
#' author: "Roland Kofler"
#' date: "Nov 1th, 2015"
#' output: html_document
#' ---

#' # Rationale
#' 
#' > As of 31 December 2015, how many refugees and migrants will UNHCR report as having arrived in Europe by sea?
#' > Europe is facing a refugee crisis [...]. Question will be resolved using the data available on UNHCR's Emergency Response Page for the Mediterranean as of 31 December 2015 (http://data.unhcr.org/mediterranean/regional.html ).
#'
#' -- from an [Open Good Judgment Question](https://www.gjopen.com/questions/45-as-of-31-december-2015-how-many-refugees-and-migrants-will-unhcr-report-as-having-arrived-in-europe-by-sea)
#' 
#' The code is in the public domain and I encourage you to play! (CC BY 3.0) Roland Kofler. 
#' Any Idea how to improve? Contact me via Gmail: my name separated by a dot.
#' 
#' [Source](https://github.com/rolandkofler/refugees) on GitHub
#'
#' Found [the freaking raw data](http://data.unhcr.org/data_sources/mediterranean/data.xls?_=1446294376729)
#' 
#' Its [all about Lesbos](https://docs.google.com/spreadsheets/d/1maUaql6nBbNsDN7aee1KMVtmLWWZwas3vBdhWbDo93E/edit#gid=1996949114)! 
#' 
#' ## Facts
October=10 
September=9   #you get the idea
reportedAmount=723221
aMillion=10^6
N=10000;
arrivals2014= c(3270, 4369, 7283, 17084, 16627, 26221, 28303, 33478, 33944, 23050, 13318, 9107)
arrivals2015= c( 5546,  7343,	10184,	29441,	40117,	53987,	75483,	130837,	172843,	197440)
lesbosArrivals2014= c(290, 413, 555, 620, 461, 824, 873, 1064, 1778, 2072, 959, 802)
lesbosArrivals2015= c(737, 1002, 3348, 4990, 7228, 14796, 23721, 56579, 95384, 124698)

syrianRefugees=4000000

#' # Data Analysis
arrivalsFor2Years <- c(arrivals2014, arrivals2015)
arrivalTimeseries <- ts(arrivalsFor2Years, start=c(2014, 1), end=c(2015, 10), frequency=12)

lesbosArrivalsFor2Years <- c(lesbosArrivals2014, lesbosArrivals2015)
lesbosTimeseries <- ts(lesbosArrivalsFor2Years, start=c(2014, 1), end=c(2015, 10), frequency=12)

#' ## Monthplots
#' They are looking as nobody can stop the trend
#' Lesbos is the main door to the EU, being the greatest Island of the EU only a few km ashore of Turkey it is reachable with a dingi boat
#' 
monthplot(arrivalTimeseries)
monthplot(lesbosTimeseries)
#library(forecast)
#seasonplot(arrivalTimeseries) 
#seasonplot(lesbosTimeseries) 

#' ## Extreme situation in Lesbos
plot(lesbosArrivals2015/ lesbosArrivals2014[1:10], type = 'o', main="Lesbos 2015 vs Lesbos 2014")

changeRateLesbosArrivalsYoY <- lesbosArrivalsFor2Years/arrivalsFor2Years
plot(changeRateLesbosArrivalsYoY, type = 'o', main="How Lesbos became the door to Europe", xlab="month since Jan 2014", ylab="% of Lesbos Refugees to Total")

#' If you are a Refugee Maximalista -- someone who believes that more than a million will come -- its worth to take a look at 
#' [How Greek Island behaved in Winter 2014](https://docs.google.com/spreadsheets/d/1maUaql6nBbNsDN7aee1KMVtmLWWZwas3vBdhWbDo93E/pubchart?oid=219698616&format=interactive)
#' 

#' # Monte Carlo Simulation
#' ## model parameters
#' if refugee inflow behave the same like last year in Lesbos 
mean1= lesbosArrivals2014[11]/lesbosArrivals2014[10]
mean2= lesbosArrivals2014[12]/lesbosArrivals2014[10]

#' we get means mean1=`r mean1`, mean2=`r  mean2`
#' 
#' **BIG OPEN Question: how to estimate our Standard Deviation?**
#' If I understand the bell curve correctly I shoud set a SD so that no event lays outside of the plausible range. so not below already reportedAmount
#' If thats the case > 1mm is so imporbable. 
standardDeviation= 0.18
weight1= rnorm(N, mean=mean1, sd=standardDeviation)
weight2= rnorm(N, mean=mean2, sd=standardDeviation)

#' ## Calculations
arrivalsNovember = arrivals2015[October] * weight1
arrivalsDecember = arrivals2015[October] * weight2 
futureArrivals= arrivalsNovember + arrivalsDecember
refugeesAmount = reportedAmount + futureArrivals
plot(refugeesAmount)
abline(h=aMillion, col="red");
abline(h=reportedAmount, col="green");
overAmillion = refugeesAmount > 10^6
ProbabilityOverMillion= sum(overAmillion)/N
#' ## Probability of more than a Million Refuges
print(ProbabilityOverMillion * 100)




