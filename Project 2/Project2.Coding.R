#__________________________________________________________________________________________________________
#Project 2, part 1: Farmer Giles of Ham

#Import the <syuzhet> package, which extracts sentiment.
library(syuzhet)

#Read in "Farmer Giles of Ham"
farmergiles <- get_text_as_string(path_to_file = "C://Users//chris//Downloads//FarmerGiles.txt")

#Calculate one sentiment value for the entire Farmer Giles
get_sentiment(farmergiles) #which indicates a value of -50, i.e. there is far more
#negative than positive language in "Farmer Giles." Note that this does NOT measure between
#-1 and +1 as other lexicons might. It has a larger of values.

#Tokenize the string of text so we can do a closer analysis. Tokenize it into 
#sentences first.
farmer_giles_v <- get_sentences(farmergiles)
#Check that it tokenized properly by running the first 3  sentences
farmer_giles_v[1:3]

#Now calculate the sentiment for each sentence on its own.
farmer_giles_sentiments_v <- get_sentiment(farmer_giles_v)
farmer_giles_sentiments_v #That's a lot of great data about the sentiment of each sentence.
#Why is the third sentence's value -0.35? Let's check by printing out the sentence
#and close-reading it.
farmer_giles_v[c(3)] #I can see "not" and "ignorant" which are probably pulling the sentiment
#down.

#Let's plot the sentiment over narrative time to get a bigger picture of how this works.

plot(
  farmer_giles_sentiments_v,
  type = "l",
  xlab = "narrative time",
  ylab = "sentiment value",
  main = "Raw sentiment values with <syuzhet> in 'Farmer Giles of Ham'"
  )
#The graph spikes everywhere, so we ought to smooth it out. Let's try Loess smoothing.

simple_plot(farmer_giles_sentiments_v, title="'Farmer Giles of Ham' Simple Plot")
#that smoothed the graph out AND scaled it down so that it DOES relatively fit between
#-1 and +1 on the y axis.

#__________________________________________________________________________________________________________
#Project 2, part 2: Leaf by Niggle

library(syuzhet)



leafbyniggle <- get_text_as_string(path_to_file = "C://Users//chris//Downloads//Leaf.txt")



get_sentiment(leafbyniggle)
#which indicates a value of +14.6

leaf_v <- get_sentences(leafbyniggle)

leaf_v[1:3] 

leaf_sentiments_v <- get_sentiment(leaf_v)
leaf_sentiments_v 

plot(
leaf_sentiments_v,
  type = "l",
  xlab = "narrative time",
  ylab = "sentiment value",
  main = "Raw sentiment values with <syuzhet> in 'Leaf by Niggle'"
) 
#Yeah, so there is less variation - the ranges are less extreme in this graph than the last graph we did
#of Farmer Giles.

#Loess smooth the graph.

simple_plot(leaf_sentiments_v, title="'Leaf by Niggle' Simple Plot")
#this smoothed

#__________________________________________________________________________________________________________
#Project 2, part 3: Roverandom

library(syuzhet)

roverandom <- get_text_as_string(path_to_file = "C://Users//chris//Downloads//Roverandom.txt")

get_sentiment(roverandom)
#which indicates a value of -40.55

roverandom_v <- get_sentences(roverandom)
roverandom_v[1:3] 

roverandom_sentiments_v <- get_sentiment(roverandom_v)
roverandom_sentiments_v 

plot(
  roverandom_sentiments_v,
  type = "l",
  xlab = "narrative time",
  ylab = "sentiment value",
  main = "Raw sentiment values with <syuzhet> in 'Roverandom'"
) 


#Loess smooth the graph.

simple_plot(roverandom_sentiments_v, title="'Roverandom' Simple Plot")
#this smoothed

