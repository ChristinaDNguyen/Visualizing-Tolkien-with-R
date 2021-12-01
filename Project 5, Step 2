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

#Say, for example, we want to find all the occurrences of 'man.' For this we 
#can use the 'which' function and ask R to find which items in the vector satisfy 
#the condition of being the word 'man.'

which(lbn_word_v == "man")

#That tells us that the words in all of those indexes listed in the console are "man"

lbn_word_v[which(lbn_word_v == "man")]

#OK, that above line printed out the contents of the indexes where 'man' occurs





