library('caTools')
library('tm')
library('rpart')
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
data= read.csv('train3.csv', stringsAsFactors = FALSE)
corpus = Corpus(VectorSource(data$Descript))

# Pre-process data
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, stemDocument)

# Create matrix
dtm = DocumentTermMatrix(corpus)

# Remove sparse terms
 dtm = removeSparseTerms(dtm, 0.95)
# Create data frame
labeledTerms = as.data.frame(as.matrix(dtm))

# Add in the outcome variable
labeledTerms$PdDistrict = data$PdDistrict
labeledTerms$Category = data$Category 
labeledTerms$Stolen = grepl("STOLEN",data$Descript)
labeledTerms$GT = grepl("GRAND THEFT",data$Descript)
labeledTerms$Burglary = grepl("BURGLARY",data$Descript)
labeledTerms$Assault = grepl("ASSAULT",data$Descript)
labeledTerms$Possesion = grepl("POSSESSION",data$Descript)
labeledTerms$Day <- weekdays(as.Date(data$Dates))

#Splitting into train and test data using caTools

set.seed(144)

spl = sample.split(labeledTerms$Category , 0.70)

train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

#Build CART Model
model = multinom(Category ~ ., data = train, MaxNWts = 2000) 
