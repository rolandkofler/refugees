# As of 31 December 2015, how many refugees and migrants will UNHCR report as having arrived in Europe by sea?
# Europe is facing a refugee crisis [...]. Question will be resolved using the data available on UNHCR's Emergency Response Page for the Mediterranean as of 31 December 2015 (http://data.unhcr.org/mediterranean/regional.html ).
# https://www.gjopen.com/questions/45-as-of-31-december-2015-how-many-refugees-and-migrants-will-unhcr-report-as-having-arrived-in-europe-by-sea
# (CC BY 3.0) Roland Kofler, eccept the data which is UNHCR's copyright
#
# found the freaking data:
# http://data.unhcr.org/data_sources/mediterranean/data.xls?_=1446294376729

# initializations
October=10 
September=9                 #you got the idea
reportedAmount=723221
million=10^6
N=10000;
arrivals= c( 5546,  7343,	10184,	29441,	40117,	53987,	75483,	130837,	172843,	197440)
if (reportedAmount != sum(arrivals)) break;

# model parameters
weight1= rnorm(N, mean=0.8, sd=0.3)
weight2= rnorm(N, mean=0.7, sd=0.3)


# calculations
arrivalsNovember = arrivals[October] * weight1
arrivalsDecember = arrivals[October] * weight2 
futureArrivals= arrivalsNovember + arrivalsDecember
refugeesAmount = reportedAmount + futureArrivals
plot(refugeesAmount)
abline(h=million, col="red");
overAmillion = refugeesAmount > 10^6
ProbabilityOverMillion= sum(overAmillion)/N
print (ProbabilityOverMillion * 100)


