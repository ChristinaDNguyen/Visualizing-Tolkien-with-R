#------ Project 6: Chapters 2, 3, and 4 applied to each section of'LOTR' ------

#----Chapter 2: Cleaning the text and graphing word frequency

#First, read in the text file using the 'scan' function.
#The first half of the line of code is creating an object to which we
#perform that 'scan' function (which reads in a text). Note that 'scan' 
#function can also be used to grab text from the internet.

text_v <- scan("C:\\Users\\chris\\Downloads\\Token distribution with Regex - LOTR\\Fellowship of the Ring.txt", what = "character", sep = ".")

#It has read in 4550 items, the 4550 sentences in the text. 

text_v

#OK, if we ran the above line, it would print the whole FOTR story, but let's skip that
#because the computer would take too

text_v[1]

#OK, above it has printed the item in the first index spot of the array, i.e.
#the first sentence of FOTR, which is Tolkien's full name. It's important to understand that the
#data inside this new text_v object is 'indexed.' Each line from the
#original text file has its own special container inside the text_v object, 
#and each container is numbered. We can access lines from the original 
#file by referencing their container or index number within a set of 
#square brackets.

text_v[2]
text_v[3]
text_v[4]

#Great, that lines above returned the second, third, fourth sentences.

#To see how many sentences there are in total:

length(text_v)

#There are 4550 sentences in FOTR.

#We tell it to start at the beginning (or wherever we like, just by changing the words in
#the quotation marks).

start_v <- which(text_v == "John Ronald Reuel Tolkien")

#Skip the section of textbook that saves metadata, since we're not using
#Gutenberg as our source.

#Let's undo what we did and remove the line breaks (we don't need to know that  
#we have 4550 sentences anymore.)  Collapse it back to normal.

text_v <- paste(text_v, collapse = " ")

length(text_v)

#Good, so the whole story of FOTR only 1 line/token now, not 4550 Check

text_v[1]

#Now that we have the entire short story loaded as a SINGLE STRING of 
#characters, we are ready to have some fun.
#First use the tolower function to convert all the text to lowercase

text_lower_v <- tolower(text_v)

#We now have a big blob of FOTR in a single lowercase string. We need to extract 
#all of the words out of the full text string and put them into a nice organized
#list. Make sure we're using a CAPITAL W, not a lowercase one.

fotr_word_l <- strsplit(text_lower_v, "\\W")

#The strsplit function here takes two arguments and returns the list. The first
#argument is the object to be split and the second is an REGULAR EXPRESSION 
#that matches any non-word character (like a SPACE between words). This REGEX 
#therefore is used in strsplit to detect "word boundaries" and tokenize
#each word onto its own unit.

#Simplify this list into a vector using this function

fotr_word_v <- unlist(fotr_word_l)

#When discussing the str function above, we mentioned that the third item in
#the vector was an empty character
#string. Calling str(lbn_word_l) reveals the following

str(fotr_word_l)

#which means there are some empty spaces (empty strings) where a period or 
#other spaceused to be. They're sort of annoying so let's get rid of them. 
#These following two lines of code first identify where those NOT blanks are,
#then they print them, so we can see exactly where

not_blanks_v <- which(fotr_word_v != " ")
not_blanks_v

#Now overwrite fotr_word_v like this

fotr_word_v <- fotr_word_v[not_blanks_v]

#Alright so now all we have are words and no blanks spaces! Our corpus 
#is pretty clean. To check that we only have words in the vector

fotr_word_v

#Good, we have checked that it gives us words only. (It sort of looks
#like there are a few spaces, but we'll chalk that up to an error
#we'll have to account for later. Is it because of a badly written
#regex?) We're on page 26 of the 
#textbook now. We can also look words up by index number. E.g.

fotr_word_v[995]

#That tells us the 995th word is 'obligation'.
#Alternatively, if we know the exact positions, we can enter them directly
#using the c combination function to create a vector of positions or index 
#values. Try it, which asks us to list the fourth, fifth, and sixth words
#in the text, and then the seventieth, seventyfirst, and seventysecond

mypositions_v <- c(4,5,6)
fotr_word_v[mypositions_v]

mypositions_v <- c(70, 71, 72)
fotr_word_v[mypositions_v]

#Say, for example, we want to find all the occurrences of 'ring.' For this we 
#can use the 'which' function and ask R to find which items in the vector satisfy 
#the condition of being the word 'ring'

which(fotr_word_v == "ring")

#That tells us that the words in all of those indexes listed in the console are "ring"
#which sure looks like a lot


#OK, that above line printed out the contents of the indexes where 'ring' occurs

#Putting all the words from FOTR into a vector of words (or more precisely, a 
#character vector) provides a handy way of organizing all  the words of 
#the short story in chronological order; it also provides a foundation
#for some deeper quantitative analysis. I want to know HOW MANY TIMES
#the word "ring" appears in the short story, rather than just WHERE.

length(fotr_word_v[which(fotr_word_v == "ring")])

#That means "ring" occurred 343 times throughout the entire FOTR.

#I'm also curious as to how many unique word types there are in FOTR. 

length(unique(fotr_word_v))

#There are 8796 unique word types in FOTR, which is several times more
#vocabulary than in the short story LBN. That makes sense.

#I can even see if FOTR abides by Zipf's law regarding the frequency of words in
#English, which says that the frequency of any word in a corpus is 
#inversely proportional to its rank or position in the overall frequency
#distribution. In other words, the second most frequent word will
#occur about half as often as the most frequent word. We use a new variable 
#type, a table.

fotr_freqs_t <- table(fotr_word_v)

#Let's print/see the first few items in the table just to check.
fotr_freqs_t[1:10]

#Now let's sort from most to least frequent word.

sorted_fotr_freqs_t <- sort(fotr_freqs_t, decreasing = TRUE)

#Now let's print it to check that it sorted

head(sorted_fotr_freqs_t)

#OK, unsurprisingly, 'the' occurred the most and 'and' occurred second most.

#It doesn't follow Zipf's law very well since 7500 IS about half of 11734,
#but 5085 is not is half of 7563, etc.

#Now plot! Once you have the top ten most frequent words stored in a vector,
#use R's built in plot function to visualized whether the frequencies of the 
#words correspond to Zipf's law using a visual interpretation.


top_ten <- sorted_fotr_freqs_t[1:11]
top_ten

plot(top_ten)
plot(top_ten, xlab = "Top ten words", ylab = "# of word occurances")
title(main="Frequency of the top 10 words in FOTR")

#Using visual interpretation, there isn't a correlation with Zipf's law
#that wordfrequency = wordfrequency subscript[(position+1)] ^1/2

#----Chapter 3: Accessing and comparing word freq. data

#In Ch. 3, we derive and compare word frequency data. We will do this first for 
#FOTR but we will later on also do it for TT and ROTK.


sorted_fotr_freqs_t["he"]
sorted_fotr_freqs_t["she"]

#Interestingly, the word "he" occurs 3028 times and "she" occurs only 159 times
#This doesn't tell us a lot about the people in the story though, since
#"she" here might refer to non-people objects as well, in the feminine
#case.

#Unlike the original fotr_word_v, where each word as indexed as a position
#in the vector, here in the frequency table the word types ARE the indexes.
#Think of it like an Excel chart with two columns, x and y. In the vector,
#the x column of the chart had index positions 1, 2, 3, 4, 5. In the table,
#the x column now has the words instead, and the y column has whatever
#characteristic of x you're studying.

#When accessing values in the vector fotr_word_v, I had to first figure out
#where in the vector (the index position) those tokens resided. But I don't
#have to do that now.

#With the data in a table object now, I can get both numerical indexing and 
#named indexing. So I can access a value in the table by either its
#numerical position (index) or by its name/label.

#I.e. these two lines return/print the same value. Most frequent word is 'the.'
#I had to use index position two (again, just like when working with
#LBN) because for some reason R identified blank
#spaces as the most frequently occurring word in LBN.

sorted_fotr_freqs_t[2]
sorted_fotr_freqs_t["the"]

#If I want to know just how much more frequent "the" is to "and"
sorted_fotr_freqs_t["the"]/sorted_fotr_freqs_t["and"]
#which tells us that "the" is 1.335849 times more frequent than "and".

#Often when analyzing text, what you really need are not the raw number of 
#occurances of word types but the RELATIVE frequencies of word types,
#expressed as a percentage of the total words in the file. Converting
#raw counts to relative frequencies allows you to more easily compare
#the patterns of usage from one text to another, like here in Project 6.
#Here in Proj. 6, I want to compare the the ratio of the words "adventure":"home"
#between all three books in the LOTR series.

#As it stands, we have a sorted table of raw word counts. I want to covert
#those raw numbers to percentages, which means I have to divide
#each count by the total number of word tokens in the entire text
#I can access the total number of words using the length function on the 
#original word vector

length(fotr_word_v)
#which tells me that there are 226,550 words in there
#and now the relative frequency of "the" would just be (one thousand whatever/
#226,550)x100

fotr_length_v <- length(fotr_word_v)
sorted_fotr_rel_freqs_t <- 100*(sorted_fotr_freqs_t/fotr_length_v)
sorted_fotr_rel_freqs_t

#OK that last line printed out all the relative frequencies of every single
#word in FOTR as percentages. Because there's a great variety of words
#used every 100 words, most of the values for relative frequencies are 
#smaller than 1 (i.e. that specific word appears less than once per 100 words.)

#The key thing to note about this expression is that R understands that it 
#needs to RECYCLE to result of length(fotr_word_v) and apply that result to each 
#and every value of the sorted_fotr_freqs_t variable. This recycling
#also words with definite values. In other words, if I wanted to multiply
#every value in a vector by 10, I can do so easily.

#But let's skip that for now. I can now access any word type and return its 
#relative freq. and percentage. This percentage shows the number of occurances
#every 100 words.

sorted_fotr_rel_freqs_t["adventure"]
#which gave us 0.006621055 times every 100 words

sorted_fotr_rel_freqs_t["home"]
#meaning 'home' occurs 0.03310527 times every 100 words. In other words,
#"home" appears about 5 times much more than "Adventure" does. Verrrrry
#interesting!

#If I want to plot the top ten words, I can use the stuff I learned
#from ch. 2. Here I will add a few more arguments to plot in order
#to convey more information about the resulting image, and then
#I will add the axis function to reset the values on the x-axis with 
#the names of the top 10 words. Graph of RELATIVE (not ABSOLUTE) frequency...
#meaning we use the sorted_fotr_rel_freqs_t variable NOT the sorted_fotr_freqs_t
#one

plot(sorted_fotr_rel_freqs_t[2:11], type = "b", main = "Relative frequency of top 10 words in FOTR", xlab = "Top 10 words in FOTR", ylab = "Percentage of full text") 
axis(1, 2:11, labels = names(sorted_fotr_rel_freqs_t[2:11]))

#----Chapter 4
#This section explains how to use the positions of words in a vector to create
#distribution plots showing where words occur across a narrative.
#We introduce the grep function and show how to use regular expressions for
#more nuanced pattern matching

#First, make dispersion plots. We want to see exactly where in the texts
#different words tend to occur, i.e. where the words appear and
#how they behave over the course of  a short story/novel.

#For this analysis, I will treat the order in which the words appear in the 
#text as a measure of time, called "novelistic time" in this case.
#I now need to create a sequence of numbers from 1 to n, where n is the
#position, or index number, of the last word in LBN. 

#Putting the two functions together allows for an expresion like this which
#maps the sequence numbers to the words in FOTR

n_time_v <- seq(from = 1, to = length(fotr_word_v))

#This expression returns an integer vector (n_time_v) containing the
#positions of every word in the book. 
n_time_v

#Now you need to locate the position of every occurance of "adventure" in the
#FOTR, or, more specifically, in the fotr_word_v object. Use which to identify
#the locations in the vector that are in occurance of whale and then
#store them in a new integer vector called adventure_vector

adventure_v <- which(fotr_word_v == "adventure")
adventure_v

#Do the same for "home"

home_v <- which(fotr_word_v == "home")
home_v

#Yep, the two printouts of "adventure" and "home" look right, since "home"
#does seem to have 5 times more occurances than "adventure"

#We have above printed out all the locations of "adventure" and "home" in the 
#vector. 

#Our goal here is to create a dispersion plot where the x-axis is novelistic
#time. We have these x-axis values in the n_time_v object. Another vector 
#containing the values for plotting on the y-axis is now needed, and in
#this case, the values need only be some reflection of the logical condition of 
#TRUE where a "adventure" is found and FALSE where a "adventure" is not found. 
#Use NA for places where there is no found match.

a_count_v <- rep(NA, times = length(n_time_v))
a_count_v[adventure_v] <- 1

plot(p_count_v, main = "Token distribution plot of 'adventure' in FOTR", xlab = "Novelistic
     time", ylab = "occurances of 'adventure'", type = "h", ylim = c(0,1), yaxt = 'n')
                 

#Repeat with "home".

h_count_v <- rep(NA, times = length(n_time_v))
h_count_v[home_v] <- 1

plot(t_count_v, main = "Token distribution plot of 'home' in FOTR", xlab = "Novelistic
     time", ylab = "occurances of 'home'", type = "h", ylim = c(0,1), yaxt = 'n')

#Skip using grep from Ch. 4

#Potential sources of error: when cleaning the text, what happened to contractions like "he's"?




