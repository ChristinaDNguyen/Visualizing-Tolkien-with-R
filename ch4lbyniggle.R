#This program explains how to use the positions of words in a vector to 
#create distribution plots showing where words occur across a narrative.
#We introduce the <grep> function and show how to use regular expressions for
#more nuanced pattern searching. It builds on code from ch. 2 and 3 of the
#text. Note to self: regex was also introduced in INF1339: Computational 
#Thinking.

#Start-up code: Copy code used from ch. 2 and 3 of textbook. We write code
#needed to analyze 'Leaf by Niggle.' First, read in the text using 'scan' function.
#The first half of the line of code is creating an object to which we
#perform that 'scan' function (which reads in a text). Note that 'scan' function
#can also be used to grab text from the internet.

text_v <- scan("C:\\Users\\chris\\Downloads\\Sentiment_Analysis\\leafbyniggle.txt", what = "character", sep = ".")

#It has read in 136 items, the 136 sentences in the text. 

text_v
#OK, above it has printed the whole Leaf by Niggle story.

text_v[1]

#OK, above it has printed the item in the first index spot of the array, i.e.
# the first sentence of Leaf by Niggle. It's important to understand that the
#data inside this new text_v object is 'indexed.' Each line from the
#original text file has its own special container inside the text_v object, 
#and each container is numbered. You can access lines from the original 
#file by referencing their container or index number within a set of 
#square brackets.

text_v[2]

#Great, that line above returned the second sentence

#To see how many sentences there are in total:

length(text_v)

#we tell it to start at the beginning (or wherever we like, just by changing the words in
#the quote marks)

start_v <- which(text_v == "THERE was once")
#We see 136 sentences. Great.  We can save this info as metadata if we want to, but let's skip that.

#Let's undo what we did and remove the line breaks (we don't need to know about 136 sentences anymore.) 
#Collapse it back to normal.

text_v <- paste(text_v, collapse = " ")

length(text_v)

#good so only 1 line/token now, not 136. check:

text_v[1]

#Now that you have the novel loaded as a SINGLE STRING of characters, you are ready to have some fun.
#First use the tolower function to convert all the text to lowercase

text_lower_v <- tolower(text_v)
#We now have a big blob of LBN in a single lowercase string. We need to extract 
#all of the words out of the full text string and put them into a nice organized
#list. Make sure you're using a CAPITAL W, not a lowercase one.

lbn_word_l <- strsplit(text_lower_v, "\\W")

#The strsplit function here takes two arguments and returns the list. The first argument is the object to be split
#and the second is an REGULAR EXPRESSION that matches any non-word chracater (like a SPACE between words). This REGEX 
#therefore is used in strsplit to detect "word boundaries" and tokenize each word onto its own unit.

#Simplify this list into a vector using this function

lbn_word_v <- unlist(lbn_word_l)

#When discussing the str function above, we mentioned that the third item in the vector was an empty character
#string. Calling str(lbn_word_l) reveals the following

str(lbn_word_l)

#which means there are some empty spaces (empty strings) where a period or other space
#used to be. They're sort of annoying so let's get rid of them. These following two 
#lines of code first identify where those NOT blanks are, then they print them
#so we can see exactly where

not_blanks_v <- which(lbn_word_v != " ")
not_blanks_v

#Now overwrite lbn_word_v like this

lbn_word_v <- lbn_word_v[not_blanks_v]

#Alright so now all we have are words and no blanks spaces! Our corpus is pretty clean. 
#Just for fun, now we enter

lbn_word_v

#Good, we have checked that it gives us words only. We're on page 26 of the textbook
#now. We can also look words up by index number. E.g.

lbn_word_v[995]

#That tells us the 995th word is niggle.
#Alternatively, if you know the exact positions, you can enter them directly using the c combination function to create a vector of positions or index values. Try it
#which asks us to list the fourth fifth and sixth words in the text
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

#So we see that 'picture' occurs 24 times in the text while 'tree' only occurs 20 times

#I'm also curious as to how many unique word types there are in LBN. 

length(unique(lbn_word_v))

#There are 1349 unique word types. Big vocabulary for a short story! 
#I can even see if LBN abised by Zipf's law regarding the frequency of words in
#English, which says that the frequency of any word in a corpus is 
#inversely proportional to its rank or position
#in the overall frequency distribution. In other words, the second most
#frequent word will occur about half as often as the most frequnt
#word. We use a new variable type, a table

lbn_freqs_t <- table(lbn_word_v)
#let's see the first few items in the table just to check

lbn_freqs_t[1:5]

#Now let's sort from most to least frequent word

sorted_lbn_freqs_t <- sort(lbn_freqs_t, decreasing = TRUE)
#now let's print it to check that it sorted

head(sorted_lbn_freqs_t)

#OK, unsurprisingly, 'the' occured the most and 'he' occured second most.
#It doesn't follow Zipf's law very well since 265 is not half of 354, and 235 is 
#not half of 265.

#Now plot! Once you have the top ten most frequent words stored in a vector,
#use R's built in plot function to visualized whether the frequencies of the words correspond 
#using a visual interpretation


top_ten <- sorted_lbn_freqs_t[1:10]
top_ten

plot(top_ten)
plot(top_ten, xlab = "Top ten words", ylab = "# of word occurances")
title(main="Frequency of the top 10 words in LBN")



#OK CHAPTER 2 OF THE TEXTBOOK IS DONE
#NOW MOVE ONTO CHAPTER 3

