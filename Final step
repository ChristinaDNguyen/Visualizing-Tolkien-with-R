#-------- Project 5: Chapters 2, 3, and 4 applied to 'Leaf by Niggle' --------

#----Chapter 2: Cleaning the text and graphing word frequency

#This program builds on code from ch. 2 and 3 of the
#text. Note to self: regex was also introduced in INF1339: Computational 
#Thinking.

#First, read in the text file using the 'scan' function.
#The first half of the line of code is creating an object to which we
#perform that 'scan' function (which reads in a text). Note that 'scan' 
#function can also be used to grab text from the internet.

text_v <- scan("C:\\Users\\chris\\Downloads\\Sentiment_Analysis\\leafbyniggle.txt", what = "character", sep = ".")

#It has read in 136 items, the 136 sentences in the text. 

text_v

#OK, above it has printed the whole LBN story.

text_v[1]

#OK, above it has printed the item in the first index spot of the array, i.e.
#the first sentence of LBN. It's important to understand that the
#data inside this new text_v object is 'indexed.' Each line from the
#original text file has its own special container inside the text_v object, 
#and each container is numbered. We can access lines from the original 
#file by referencing their container or index number within a set of 
#square brackets.

text_v[2]

#Great, that line above returned the second sentence.

#To see how many sentences there are in total:

length(text_v)

#We tell it to start at the beginning (or wherever we like, just by changing the words in
#the quotation marks).

start_v <- which(text_v == "THERE was once")

#Skip the section of textbook that saves metadata, since we're not using
#Gutenberg as our source.

#Let's undo what we did and remove the line breaks (we don't need to know that  
#we have 136 sentences anymore.)  Collapse it back to normal.

text_v <- paste(text_v, collapse = " ")

length(text_v)

#Good, so only 1 line/token now, not 136. Check

text_v[1]

#Now that we have the entire short story loaded as a SINGLE STRING of 
#characters, we are ready to have some fun.
#First use the tolower function to convert all the text to lowercase

text_lower_v <- tolower(text_v)

#We now have a big blob of LBN in a single lowercase string. We need to extract 
#all of the words out of the full text string and put them into a nice organized
#list. Make sure we're using a CAPITAL W, not a lowercase one.

lbn_word_l <- strsplit(text_lower_v, "\\W")

#The strsplit function here takes two arguments and returns the list. The first
#argument is the object to be split and the second is an REGULAR EXPRESSION 
#that matches any non-word chracater (like a SPACE between words). This REGEX 
#therefore is used in strsplit to detect "word boundaries" and tokenize
#each word onto its own unit.

#Simplify this list into a vector using this function

lbn_word_v <- unlist(lbn_word_l)

#When discussing the str function above, we mentioned that the third item in
#the vector was an empty character
#string. Calling str(lbn_word_l) reveals the following

str(lbn_word_l)

#which means there are some empty spaces (empty strings) where a period or 
#other spaceused to be. They're sort of annoying so let's get rid of them. 
#These following two lines of code first identify where those NOT blanks are,
#then they print them, so we can see exactly where

not_blanks_v <- which(lbn_word_v != " ")
not_blanks_v

#Now overwrite lbn_word_v like this

lbn_word_v <- lbn_word_v[not_blanks_v]

#Alright so now all we have are words and no blanks spaces! Our corpus 
#is pretty clean. To check that we only have words in the vector

lbn_word_v

#Good, we have checked that it gives us words only. We're on page 26 of the 
#textbook now. We can also look words up by index number. E.g.

lbn_word_v[995]

#That tells us the 995th word is 'niggle'.
#Alternatively, if we know the exact positions, we can enter them directly
#using the c combination function to create a vector of positions or index 
#values. Try it, which asks us to list the fourth, fifth, and sixth words
#in the text

mypositions_v <- c(4,5,6)
lbn_word_v[mypositions_v]

#Say, for example, we want to find all the occurrences of 'picture.' For this we 
#can use the 'which' function and ask R to find which items in the vector satisfy 
#the condition of being the word 'picture'

which(lbn_word_v == "picture")

#That tells us that the words in all of those indexes listed in the console are "picture"

lbn_word_v[which(lbn_word_v == "picture")]

#OK, that above line printed out the contents of the indexes where 'picture' occurs

#Putting all the words from LBN into a vector of words (or more precisely, a 
#character vector) provides a handy way of organizing all  the words of 
#the short story in chronological order; it also provides a foundation
#for some deeper quantitative analysis. I want to know how many times 
#the word "picture" appears in the short story.

length(lbn_word_v[which(lbn_word_v == "picture")])

#Now I repeat this with the word "tree" so I can eventually compare how
#many times picture occurs vs tree occurs.

which(lbn_word_v == "tree")
lbn_word_v[which(lbn_word_v == "tree")]
length(lbn_word_v[which(lbn_word_v == "tree")])

#So we see that 'picture' occurs 24 times in the text while 'tree' only 
#occurs 20 times.

#I'm also curious as to how many unique word types there are in LBN. 

length(unique(lbn_word_v))

#There are 1349 unique word types. Big vocabulary for a short story! 
#I can even see if LBN abides by Zipf's law regarding the frequency of words in
#English, which says that the frequency of any word in a corpus is 
#inversely proportional to its rank or position in the overall frequency
#distribution. In other words, the second most frequent word will
#occur about half as often as the most frequent word. We use a new variable 
#type, a table.

lbn_freqs_t <- table(lbn_word_v)
#Let's print/see the first few items in the table just to check.

lbn_freqs_t[1:5]

#Now let's sort from most to least frequent word.

sorted_lbn_freqs_t <- sort(lbn_freqs_t, decreasing = TRUE)

#Now let's print it to check that it sorted

head(sorted_lbn_freqs_t)

#OK, unsurprisingly, 'the' occurred the most and 'he' occurred second most.

#It doesn't follow Zipf's law very well since 265 is not half of 354, and 235 is 
#not half of 265, v.v.

#Now plot! Once you have the top ten most frequent words stored in a vector,
#use R's built in plot function to visualized whether the frequencies of the 
#words correspond to Zipf's law using a visual interpretation.


top_ten <- sorted_lbn_freqs_t[1:11]
top_ten

plot(top_ten)
plot(top_ten, xlab = "Top ten words", ylab = "# of word occurances")
title(main="Frequency of the top 10 words in LBN")

#Using visual interpretation, there isn't a correlation with Zipf's law
#that wordfrequency = wordfrequency subscript[(position+1)] ^1/2

#----Chapter 3: Accessing and comparing word freq. data

#In Ch. 3, we dervice and compare word frequency data. We will skip the
#comparing of word frequencies between two texts for now as project 5.
#Project 6 will require this comparison between we have 3 texts from 
#"The Lord of the Rings" to compare to each other.


sorted_lbn_freqs_t["he"]
sorted_lbn_freqs_t["she"]

#Interestingly, the word "he" occurs 265 times and "she" occurs only 4 times
#This doesn't tell us a lot about the people in the story though, since
#"she" here refers to non-people objects as well, in the feminine
#case.

#Unlike the original lbn_word_v, where each word as indexed as a position
#in the vector, here in the frequency table the word types ARE the indexes.
#Think of it like an Excel chart with two columns, x and y. In the vector,
#the x column of the chart had index positions 1, 2, 3, 4, 5. In the table,
#the x column now has the words instead, and the y column has whatever
#characteristic of x you're studying.

#When accessing values in the vector lbn_word_v, I had to first figure out
#where in the vector (the index position) those tokens resided. But I don't
#have to do that now.

#With the data in a table object now, I can get both numerical indexing and 
#named indexing. So I can access a value in the table by either its
#numerical position (index) or by its name/label.

#I.e. these two lines return/print the same value. Most frequent word is 'the.'
#I had to use index position two becaues for some reason R identified blank
#spaces as the most frequently occuring word in LBN.

sorted_lbn_freqs_t[2]
sorted_lbn_freqs_t["the"]

#If I want to know just how much more frequent "the" is to "he"
sorted_lbn_freqs_t["the"]/sorted_lbn_freqs_t["he"]
#which tells us that "the" is 1.335849 times more frequent than "he".

#But really, for the purpose of Project 5, I have to know how much more
#frequent "picture" is than "tree"
sorted_lbn_freqs_t["picture"]/sorted_lbn_freqs_t["tree"]
#which tells us that "picture" is 1.2 times more frequent than "tree."

#Often when analyzing text, what you really need are not the raw number of 
#occurances of word types but the RELATIVE frequencies of word types,
#expressed as a percentage of the total words in the file. Converting
#raw counts to relative frequencies allows you to more easily compare
#the patterns of usage from one text to another, like in Project 6.
#In Proj. 6, I will want to compare the the ratio of the words "he":"she"
#between all three books in the LOTR series.

#As it stands, we have a sorted table of raw word counts. I want to covert
#those raw numbers to percentages, which means I have to divide
#each count by the total number of word tokens in the entire text
#I can access the total number of words using the length function on the 
#original word vector

length(lbn_word_v)
#which tells me that there are 8894 words in there
#and now the relative frequency of "the" would just be (954/8894)x100

lbn_length_v <- length(lbn_word_v)
sorted_lbn_rel_freqs_t <- 100*(sorted_lbn_freqs_t/lbn_length_v)
sorted_lbn_rel_freqs_t

#OK that last line printed out all the relative frequencies of every single
#word in LBN as percentages. Ignore the first entry with 15.8% since that's 
#counting spaces and I still don't know how to get rid of that

#The key thing to note about this expression is that R understands that it 
#needs to RECYCLE to result of length(lbn_word_v) and apply that result to each 
#and every value of the sorted_lbn_freqs_t variable. This recycling
#also words with definite values. In other words, if I wanted to multiply
#every value in a vector by 10, I can do so easily.

#But let's skip that for now. I can now access any word type and return its 
#relative freq. and percentage. This percentage shows the number of occurances
#every 100 words.

sorted_lbn_rel_freqs_t["picture"]
#which gave us 0.2698448

sorted_lbn_rel_freqs_t["the"]
#meaning 'the' occurs ~4 times every 100 words

#If I want to plot the top ten words, I can use the stuff I learned
#from ch. 2. Here I will add a few more arguments to plot in order
#to convey more information about the resulting image, and then
#I will add the axis function to reset the values on the x-axis with 
#the names of the top 10 words. Graph of RELATIVE (not ABSOLUTE) frequency...
#meaning we use the sorted_lbn_rel_freqs_t variable NOT the sorted_lbn_freqs_t
#one

plot(sorted_lbn_rel_freqs_t[2:11], type = "b", main = "Relative frequency of top 10 words in LBN", xlab = "Top 10 words in LBN", ylab = "Percentage of full text") 
axis(1, 2:11, labels = names(sorted_lbn_rel_freqs_t[2:11]))

#----Chapter 4
#This section explains how to use the positions of words in a vector to create
#distribution plots showing where words occur across a narrative.
#We introduce the grep function and show how to use regular expressions for
#more nuanced pattern matching

#First, make dispersion plots. We want to see exactly where in the texts
#different words tend to occur, i.e. where the words appear and
#how they behave over the course of  a short story/novel.
#At what points, for example, does Tolkien really *get into* writing
#about "picture" and "tree"? We can then compare these two.


#For this analysis, I will treat the order in which the words appear in the 
#text as a measure of time, called "novelistic time" in this case.
#I now need to create a sequence of numbers from 1 to n, where n is the
#position, or index number, of the last word in LBN. Here we practice
#just asking R to list a sequence from 1 to 8894 (we haven't assigned it
#to anything yet)

seq(from = 1, to = 8894)

#Putting the two functions together allows for an expresion like this which
#maps the sequence numbers to the words in LBN

n_time_v <- seq(from = 1, to = length(lbn_word_v))

#This expression returns an integer vector (n_time_v) containing the
#positions of every word in the book. 
n_time_v

#Now you need to locate the position of every occurance of "picture" in the
#short story, or, more specifically, in the lbn_word_v object. Use which to identify
#the locations in the vector that are in occurance of whale and then
#store them in a new integer vector called pictures_vector

pictures_v <- which(lbn_word_v == "picture")

pictures_v
#Do the same for "tree"

trees_v <- which(lbn_word_v == "tree")
trees_v

#We have above printed out all the locations of "picture" and "tree" in the 
#vector. 

#Our goal here is to create a dispersion plot where the x-axis is novelistic
#time. We have these x-axis values in the n_time_v object. Another vector 
#containing the values for plotting on the y-axis is now needed, and in
#this case, the values need only be some reflection of the logical condition of 
#TRUE where a "picture" is found and FALSE where a "picture" is not found. 
#Use NA for places where there is no found match.

p_count_v <- rep(NA, times = length(n_time_v))
p_count_v[pictures_v] <- 1

plot(p_count_v, main = "Token distribution plot of 'picture' in LBN", xlab = "Novelistic
     time", ylab = "occurances of 'picture'", type = "h", ylim = c(0,1), yaxt = 'n')
                 

#Repeat with "tree".

t_count_v <- rep(NA, times = length(n_time_v))
t_count_v[trees_v] <- 1

plot(t_count_v, main = "Token distribution plot of 'tree' in LBN", xlab = "Novelistic
     time", ylab = "occurances of 'tree'", type = "h", ylim = c(0,1), yaxt = 'n')

#Skip using grep from Ch. 4








                
  





#Potential sources of error: when cleaning the text, what happened to contractions like "he's"?




