library(arules)
library(arulesViz)
library(GGally)
library(corrplot)
library(psych)

book <- read.csv(file.choose())
View(book)
summary(book)
describe(book)
ggpairs(book)
corrplot(cor(book),method = "square",type = "lower")
sum(is.na(book))

# applying apriorimalgorithm
# apriori algorithm with support =0.02 , confi =0.5 & minlen = 3
rules_book <- apriori(as.matrix(book),parameter = list(support =0.02,confidence=0.5,minlen=3))
rules_book
inspect(head(sort(rules_book,by="lift")))
inspect(head(sort(rules_book,by="confidence")))
inspect(head(sort(rules_book,by="support")))
inspect(head(sort(rules_book,by=c("count","lift"))))  #maximum count = 299

head(quality(rules_book))

# visualisation
colfunction <- colorRampPalette(c("red","blue","yellow"))
plot(rules_book,method = "scatterplot",jitter=0,col=colfunction(5))
plot(rules_book,method = "paracoord")
plot(head(sort(rules_book,by="lift"),n=10),method = "graph")
plot(rules_book,method = "grouped matrix",col=colfunction(5),n)
plot(rules_book,method = "matrix")
plot(rules_book,method = "matrix3D")

# using differnt support and confidence

rules_book2 <- apriori(as.matrix(book),parameter = list(support = 0.01,confidence =0.7, minlen = 4))
rules_book2
inspect(head(sort(rules_book2,by="lift")))
inspect(head(sort(rules_book2,by="confidence")))
inspect(head(sort(rules_book2,by="support")))      # max count =178
inspect(head(sort(rules_book2,by=c("count","lift"))))

#plot methods matrix , mosaic , doubledecker , graph , paracoord , scatterplot , groupedmatrix , two-key plot , matrix3D ,iplots

plot(rules_book2,method = "two-key plot",jitter=0)
plot(rules_book2,method = "matrix3D")
plot(rules_book2,method = "matrix")
plot(head(sort(rules_book2,by="lift"),n=10),method = "graph")
plot(rules_book2,method = "paracoord")
plot(rules_book2,method = "grouped matrix",col=colfunction(5))
plot(rules_book2,method = "matrix",engine = "3d")

#___________________________________________________________________________________________________#

groceries <- read.transactions(file.choose(),sep = ",",format = "basket")
inspect(groceries[1:5])
groceries@itemInfo
itemFrequencyPlot(x=groceries,topN=10)

# applying Apriori algorithm 
# with support = 0.005 , confidence = 0.2 & minlen = 3

grocerie_rules <- apriori(groceries,parameter = list(support =0.005,confidence=0.2,minlen = 3))
grocerie_rules

inspect(head(sort(grocerie_rules,by="lift")))
inspect(head(sort(grocerie_rules,by="confidence")))
inspect(head(sort(grocerie_rules,by="support")))
inspect(head(sort(grocerie_rules,by=c("count","lift")))) #max count = 228

head(quality(grocerie_rules))

#plot methods matrix , mosaic , doubledecker , graph , paracoord , scatterplot , groupedmatrix , two-key plot , matrix3D ,iplots
#visualisation
plot(grocerie_rules,method = "scatterplot",jitter=0,col=colfunction(5))
plot(grocerie_rules,method = "grouped matrix",col=colfunction(5))
plot(head(sort(grocerie_rules,by="support"),n=20),method="graph")
plot(grocerie_rules,method = "paracoord")
plot(grocerie_rules,method = "matrix")

# using different support and confidence
grocerie_rules2 <- apriori(groceries,parameter = list(support=0.005,confidence = 0.5,minlen=2))
grocerie_rules2
inspect(head(sort(grocerie_rules2,by="lift")))
inspect(head(sort(grocerie_rules2,by="confidence")))
inspect(head(sort(grocerie_rules2,by="support")))
inspect(head(sort(grocerie_rules2,by=c("count","lift")))) #max count = 219

#visualisation
plot(grocerie_rules2,method = "scatterplot",jitter=0,col=colfunction(5))
plot(grocerie_rules2,method = "grouped matrix",col=colfunction(5))
plot(head(sort(grocerie_rules2,by="support"),n=20),method="graph")
plot(grocerie_rules2,method = "paracoord")
plot(grocerie_rules2,method = "matrix")



#_________________________________________________________________________________________________________#

movies <- read.csv(file.choose())
View(movies)
head(movies)
summary(movies)
describe(movies)
corrplot(cor(movies[,6:15]),method = "square",type = "upper")
ggpairs(movies[,6:15])

#applying apriori algorithm
# confidence =0.05 , support =0.005 ,minlen =3

movies_rules <- apriori(as.matrix(movies[,6:15]),parameter = list(support = 0.005,confidence= 0.05,minlen=3))
movies_rules
inspect(head(sort(movies_rules,by="lift")))
inspect(head(sort(movies_rules,by="confidence")))
inspect(head(sort(movies_rules,by="support")))
inspect(head(sort(movies_rules,by=c("count","lift"))))# max count = 4
 
head(quality(movies_rules))

#plot methods matrix , mosaic , doubledecker , graph , paracoord , scatterplot , groupedmatrix , two-key plot , matrix3D ,iplots

#visualisation
plot(movies_rules,method = "scatterplot",jitter=0,col=colfunction(5))
plot(movies_rules,method = "grouped matrix",col=colfunction(5))
plot(head(sort(movies_rules,by="lift"),n=10),method = "graph")
plot(movies_rules,method = "paracoord")
plot(movies_rules,method = "matrix")


# using different support and confidence

movies_rules2 <- apriori(as.matrix(movies[,6:15]),parameter = list(support =0.005, confidence =0.5,minlen=2))
movies_rules2
inspect(head(sort(movies_rules2,by="lift")))
inspect(head(sort(movies_rules2,by="confidence")))
inspect(head(sort(movies_rules2,by="support")))
inspect(head(sort(movies_rules2,by=c("count","lift"))))# max count = 6

#visualisation
plot(movies_rules2,method = "scatterplot",jitter=0,col=colfunction(5))
plot(movies_rules2,method = "grouped matrix",col=colfunction(5))
plot(head(sort(movies_rules2,by="lift"),n=20),method = "graph")
plot(movies_rules2,method = "paracoord")
plot(movies_rules,method = "matrix")
plot(movies_rules2,method = "two-key plot", jitter=0)
plot(movies_rules2,method = "matrix",engine = "3d")
