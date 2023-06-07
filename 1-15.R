#---------------Preperatory R Assignment----------------------

#1-----------------------------------------------------------
h = read.csv(file = 'spain2005.csv')
head(h)



#2----------------------------------------------------------
table(h$garage)



#3----------------------------------------------------------
pg = h[c("totalprice","garage")]
pg0 = pg[pg["garage"]==0]
pg0 = pg0[1:167]
meanPG0 = mean(pg0)

pg1 = pg[pg["garage"]==1]
pg1 = pg1[1:49]
meanPG1 = mean(pg1)

pg2 = pg[pg["garage"]==2]
pg2 = pg1[1:2]
meanPG2 = mean(pg2)

print("The mean price for no parking is:") 
print (meanPG0)

print("The mean price for 1 parking slot is:") 
print (meanPG1)

print("The mean price for 2 parking slots is:") 
print (meanPG2)




#4--------------------------------------------------------
el = h[c("elevator","garage")]
el0 = el[el["elevator"]==0]
el0 = el0[1:(length(el0)/2)] 
print(el0)
#It appears to be 0 flats with o elevators and 1 or more garages


el0g0=length(el0)
el0g1=0
el0g2=0


el1 = el[el["elevator"]==1]
el1 = el1[(length(el1)/2 +1):length(el1)] 

el1g0=length(which(el1==0))
el1g1=length(which(el1==1))
el1g2=length(which(el1==2))

sum=(el0g0+el0g1+el0g2+el1g0+el1g1+el1g2)
ft = matrix(c("0 elevators and 0 garages","0 elevators and 1 garages", 
"0 elevators and 2 garages", "1 elevators and 0 garages", "1 elevators and 1 garages", "1 elevators and 2 garages","No of Flats",
el0g0,el0g1,el0g2,el1g0,el1g1,el1g2, sum, el0g0/sum,el0g1/sum,el0g2/sum,el1g0/sum,el1g1/sum,el1g2/sum,sum/sum), nrow = 7, ncol = 3)

colnames(ft) <- c("Categories", "N", "Frequencies")
ft




#5-----------------------------------------------------
elgp = h[c("totalprice","elevator","garage")]
el0g0p=elgp[elgp$elevator == 0,]
#Mean Price for el=0 and g=0
el0g0Mp = mean(el0g0p[,"totalprice"])


el1gp=elgp[elgp$elevator == 1,]

el1g0p=el1gp[el1gp$garage == 0,]
#Mean Price for el=1 and g=0
el1g0Mp = mean(el1g0p[,"totalprice"])

el1g1p=el1gp[el1gp$garage == 1,]
#Mean Price for el=1 and g=1
el1g1Mp = mean(el1g1p[,"totalprice"])


el1g2p=el1gp[el1gp$garage == 2,]
#Mean Price for el=1 and g=2
el1g2Mp = mean(el1g2p[,"totalprice"])


print("There are 4 combinations")
print("The mean price for a flat with no elevators and no garage is")
el0g0Mp

print("The mean price for a flat with 1 elevator and no garage is")
el1g0Mp

print("The mean price for a flat with 1 elevator and 1 garage is")
el1g1Mp

print("The mean price for a flat with 1 elevator and 2 garages is")
el1g2Mp




#6-------------------------------------------------
nonZero=h[h$garage>=1,]
print(nonZero)



#7-------------------------------------------------
data.c=h[h$heating=="3B",]
data.c
#It is apparent that every flat of the 3B category has an elevator, meaning is redundant to make any other moves.

lengths(data.c)
print("In the file are contained 10 Flats")

#save
saveRDS(data.c, file = "data.c.rds")



#8-----------------------------------------------
#Load RDS
C3B=readRDS("data.c.rds")

#Mean Price and Area of 3B
P3Bmean = mean(C3B[,"totalprice"])
A3Bmean = mean(C3B[,"area"])


#Mean Price and Area of all the dataset
Phmean = mean(h[,"totalprice"])
Ahmean = mean(h[,"area"])



#Comparison
print("Total price of data.c and spain2005")
print(P3Bmean)
print(Phmean)
print("3B category shows a higher price than the average")



print("Area of data.c and spain2005")
print(A3Bmean)
print(Ahmean)
print("3B category shows a greater area than the average")




#9-------------------------------------------------
outVector = h[c("out")]


outVector50 = outVector[outVector$out=="E50",]
E50=length(outVector50)


outVector75 = outVector[outVector$out=="E75",]
E75=length(outVector75)

outVector100 = outVector[outVector$out=="E100",]
E100=length(outVector100)


sum=(E50+E75+E100)

outF = matrix(c("E50","E75","E100","sum", E50,E75,E100,sum, E50/sum,E75/sum,E100/sum,sum/sum ), nrow = 4, ncol = 3)
colnames(outF)=c("Categories", "N", "Frequencies")
outF





#Plot window
par(mfrow=c(4,2))


#Pie Plot
x=c(E50,E75,E100)
labels=c("E50","E75","E100")
pie(x, labels, main = "Percent of the exposed apartment", col = rainbow(length(x)))


#Barplot
barplot(x,main = "Percent of the exposed apartment",xlab = "Number of Flats",
ylab="Categories", names.arg = c("E50","E75","E100"), col = rainbow(length(x)), horiz = TRUE)



print("The barplot offers higher definition compaired to the pie")



#10-----------------------------------------------------
price = h[c("totalprice")]
price = price[price["totalprice"]!=0]

#Distribution histplot
hist = hist(price, breaks=8, col="Blue", xlab="Flat Price",main="Histogram showing a normal distribution with a positive skew")
print("It show to be aproximate to a normal distribution with a positive skew")



#11----------------------------------------------------
#plot of price and area as dots
plot(h$totalprice,h$area,main="Total price and Area comparison",xlab="Price",ylab="Area")


#Correlation calculation based on Pearson coefficient
cor(h$totalprice,h$area,method="pearson")

print("It appears to be a strong positive correlation between the price and area as expected")





#12----------------------------------------------------
#Total price and Area comparison plot
plot(h$totalprice,h$toilets,main="Total price and Area comparison",xlab="Price",ylab="Toilets")


cor(h$totalprice,h$toilets,method="pearson")


print("It appears to be a positive correlation between the price and the number of toilets")

print("These is also a non insignificant number of outliers")


#Area and Number of Toilets comparison plot
plot(h$area,h$toilets,main="Area and Number of Toilets comparison",xlab="Area",ylab="Toilets")
cor(h$area,h$toilets,method="pearson")

print("It seems to be some positive correlation between price and the number of toilets, also it appears to be somewhat weaker than the price")


print("We can confirm that the correlation between price and area is far stronger than price an the number of toilets or area and the number of toilets")


print("Also it is safe to assume that a big flat with two toilets it is also highly likely to be expensive")




#13-----------------------------------------------------
pt = h[c("totalprice","toilets")]
pt1=pt[pt$toilets==1,]
pt2=pt[pt$toilets==2,]


pt1=pt1[order(pt1[,"totalprice"]),]
lengths(pt1)

maxPrice = pt1[116,1]


pt2=pt2[order(pt2[,"totalprice"]),]
minPrice = pt2[1,1]

print("The range of price where flats with either one or two toilets can be founds starts at:")
options(scipen = 999) 
print(minPrice)
print("and ends at:")
print(maxPrice)


#14-------------------------------------------------------
pt = h
pt = pt[pt$totalprice<=maxPrice,]
pt = pt[pt$totalprice>=minPrice,]
pt=pt[order(pt[,"totalprice"]),]
pt


pt1 = pt1[pt1$totalprice>=minPrice,]
pt2 = pt2[pt2$totalprice<=maxPrice,]

pt1 = pt1[c("totalprice")]
pt1 = pt1[pt1["totalprice"]!=0]
meanPT1 = mean(pt1)

pt2 = pt2[c("totalprice")]
pt2 = pt2[pt2["totalprice"]!=0]
meanPT2 = mean(pt2)

sprintf("The extra cost for one more toiles is approximate to %s €",(meanPT2-meanPT1))
pt1=pt[pt$toilets==1,]
sprintf("By ammounting to %s percent of the mean price of a house with one toilet it seems to be an economically unwise choice ",((meanPT2-meanPT1)/mean(pt1[,2])*100))


#15----------------------------------------------------------
my_function = function(df,ver) 
{
  totalprice=c(ver[1])
  area=c(ver[2])
  df=df[order( df[,totalprice], df[,area] ),]
  return(df)
}

ver=c("totalprice","area")
df = as.data.frame(do.call(cbind,pt))
my_function(df,ver)

print("Finish--------------------------------------------------")
