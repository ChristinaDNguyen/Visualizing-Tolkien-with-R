#__________________________________________________________________________________________________________
#Project 3, part 1: Aragorn's poem on Beren and Luthien

#Is a poem, even an epic poem or a narrative poem, a short story? It's short,
#and it tells a story, but is it a short story? Can poetry ever be short stories,
#or must it be prose that is a short story?

library(syuzhet)

aragorn <- get_text_as_string(path_to_file = "C://Users//chris//Downloads//Aragorn.txt")

get_sentiment(aragorn) #which indicates a value of -4

aragorn_v <- get_sentences(aragorn)

aragorn_v[1:3]

#Now calculate the sentiment for each sentence on its own.
aragorn_sentiments_v <- get_sentiment(aragorn_v)
aragorn_sentiments_v
#Let's plot the sentiment over narrative time to get a bigger picture of how this works.

plot(
  aragorn_sentiments_v,
  type = "l",
  xlab = "narrative time",
  ylab = "sentiment value",
  main = "Raw sentiment values with <syuzhet> in 'Aragorn's poem of Beren and Luthien"
  )

#No need to smooth, too few values


#__________________________________________________________________________________________________________
#Project 3, part 2: The Adventures of Tom Bombadil

#Again a poem. Can it be a short story?

library(syuzhet)



tom <- get_text_as_string(path_to_file = "C://Users//chris//Downloads//Tom.txt")


get_sentiment(tom)
#which indicates a value of -2. that doesn't seem very fair since I generally got
#pleasent vibes while close reading. Let's try sentences to  be more specific.

tom_v <- get_sentences(tom)

tom_v[1:3] 

tom_sentiments_v <- get_sentiment(tom_v)
tom_sentiments_v 

plot(
tom_sentiments_v,
  type = "l",
  xlab = "narrative time",
  ylab = "sentiment value",
  main = "Raw sentiment values with <syuzhet> in 'The Adventures of Tom Bombadil'"
)

#Loess smooth

simple_plot(tom_sentiments_v, title="'The Adventures of Tom Bombadil' Simple Plot")
#this smoothed
