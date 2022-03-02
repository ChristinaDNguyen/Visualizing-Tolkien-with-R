#Import the syuzhet package, which extracts sentiment
library(syuzhet)

#Read in "Farmer Giles of Ham"
farmergiles <- get_text_as_string(path_to_file = "C://Users//chris//Downloads//FarmerGiles.txt")

#Calculate one sentiemnt for the entire Farmer Giles
get_sentiment(farmergiles) #which indicates a value of -50, i.e. there is far more negative than
#positive language in "Farmer Giles of Ham." Note that this does NOT measure between -1 and +1
#as other sentiment lexicons might. It has a larger range of values.

#Tokenize the string of text so we can do a closer analysis. Tokenize it into "sentences" first.
farmer_giles_v <- get_sentences(farmergiles)
#Check that it tokenized properly into sentences by running the first three sentences.
farmer_giles_v[1:3] #It runs perfectly.

#Now calculate the sentiment for each sentence on its own.
farmer_giles_sentiments_v <- get_sentiment(farmer_giles_v)
farmer_giles_sentiments_v #That's a lot of great data about the sentiment of each sentence.
#Why is the third sentence -0.35? Let's see why it looks so negative by printing
#out the sentence and close reading it.
farmer_giles_v[c(3)] #I can see a "not" and "ignorant," which are definitely negative terms.

#Let's plot the sentiment over narrative time to get a bigger picture of how this works.
plot(
  farmer_giles_sentiments_v,
  type = "l",
  xlab = "narrative time",
  ylab = "sentiment value",
  main = "Raw sentiment values with <syuzhet> in 'Farmer Giles of Ham'"
)

#The graph spikes everywhere, so we ought to smooth it out. Let's try Loess smoothing.

simple_plot(farmer_giles_sentiments_v, title="'Farmer Giles of Ham' Simple Plot") #this smoothed
#the graph out AND scaled it down so that it DOES relatively fit between -1 and +1. 
#We get Loess smoothing in blue.

