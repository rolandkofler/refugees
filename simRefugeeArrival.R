#' ---
#' title: "Refugee Analysis"
#' author: "Roland Kofler"
#' date: "Oct 31rd, 2015"
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
#' ## Initializations
October=10 
September=9   #you get the idea
reportedAmount=723221
million=10^6
N=10000;
arrivals2014= c(3270, 4369, 7283, 17084, 16627, 26221, 28303, 33478, 33944, 23050, 13318, 9107)
arrivals2015= c( 5546,  7343,	10184,	29441,	40117,	53987,	75483,	130837,	172843,	197440)

#' # Monte Carlo Simulation
#' ## model parameters
mean1=0.9
mean2=0.6
weight1= rnorm(N, mean=mean1, sd=0.3)
weight2= rnorm(N, mean=mean2, sd=0.3)


#' ## Calculations
arrivalsNovember = arrivals2015[October] * weight1
arrivalsDecember = arrivals2015[October] * weight2 
futureArrivals= arrivalsNovember + arrivalsDecember
refugeesAmount = reportedAmount + futureArrivals
plot(refugeesAmount)
abline(h=million, col="red");
overAmillion = refugeesAmount > 10^6
ProbabilityOverMillion= sum(overAmillion)/N
print (ProbabilityOverMillion * 100)

#' # Timeseries Forecasting approach
imputedArrivalsFor2Years <- c(arrivals2014, arrivals2015) # arrivals[October]*mean1, arrivals[October]*mean2, arrivals[October]*mean2*mean1)
arrivalTimeseries <- ts(imputedArrivalsFor2Years, start=c(2014, 1), end=c(2015, 10), frequency=12)

#' ## Monthplots
#' They are looking as nobody can stop the trend
monthplot(arrivalTimeseries)
library(forecast)
seasonplot(arrivalTimeseries) 

#' ## Automated forecasting using an exponential model
#' Don't think this makes sense, the winter is not taken into account at all.
fit <- ets(arrivalTimeseries)
predicted = holt(arrivalTimeseries, alpha=0.8, beta=0.2, initial="simple", h=5)
plot(predicted)

