#------ Project 6: Chapters 2, 3, and 4 applied to each section of'LOTR' ------


#__________________________________FELLOWSHIP OF THE RING_______________________
#----Chapter 2: Cleaning the text and graphing word frequency-------------------

#Read the text into a vector object called "text_v" with the "scan" function.

text_v <- scan("C:\\Users\\chris\\Downloads\\Token distribution with Regex - LOTR\\Fellowship of the Ring.txt", what = "character", sep = ".")

#It has read in 4550 items, i.e. the 4550 sentences in the text. 

#We could (but won't) print the whole FOTR story by running the below command
#(remove the hashtag from the beginning and run as normal):

#text_v 

#[variable a in synthesis chart] 
#Calculate total number of sentences in FOTR text:

length(text_v)

#There are 4550 sentences in FOTR.

#We tell the program to start reading from the line called "John
#Ronald Reuel Tolkien," i.e. the beginning of the text.

start_v <- which(text_v == "John Ronald Reuel Tolkien")

#Let's remove the line breaks (we don't need to know that  
#we have 4550 sentences anymore.)  Collapse it back to normal.

text_v <- paste(text_v, collapse = " ")

length(text_v)

#Good, so the whole story of FOTR is only 1 line/token now, not 4550. Check:

text_v[1]

#Now that we have the entire short story loaded as a SINGLE STRING of 
#characters, we continue.
#First we use the "tolower" function to convert all the text to lowercase.

text_lower_v <- tolower(text_v)

#We now have all of FOTR in a single lowercase string. We will extract 
#all of the words out of the full text string and put them into a nice organized
#list. 

fotr_word_l <- strsplit(text_lower_v, "\\W")

#The strsplit function here takes two arguments and returns the list. The first
#argument is the object to be split and the second is an REGULAR EXPRESSION 
#that matches any non-word character (like a SPACE between words). This REGEX 
#therefore is used in strsplit to detect "word boundaries" and tokenize
#each word onto its own unit.

#Simplify this list into a vector so we can manipulate it easily:

fotr_word_v <- unlist(fotr_word_l)

#Print the string of the list now to check what each index position holds.

str(fotr_word_l)

#In case there are some empty spaces (remember, we only want words in the 
#string), we will get rid of them. 
#These following two lines of code first identify where those NOT blanks are,
#then they print them, so we can see exactly where

not_blanks_v <- which(fotr_word_v != " ")
not_blanks_v

#Now overwrite fotr_word_v like this

fotr_word_v <- fotr_word_v[not_blanks_v]

#So now all we have are words and no blanks spaces! Our corpus 
#is clean. To check that we only have words in the vector

fotr_word_v

#Good, we have checked that it gives us words only. (It sort of looks
#like R thinks that "" is a word (what even is that?), but we'll chalk that 
#up to an error we'll have to account for later. Is it because of a
#badly written search function?) We're on page 26 of the textbook now.

#I'm curious as to how many unique word types there are in FOTR [variable
#b in the synthesis chart]. 

length(unique(fotr_word_v))

#There are 8796 unique word types in FOTR.

#I check if FOTR abides by Zipf's law.  We use a new variable 
#type, a table, to tell R to list the word and its ABSOLUTE frequency.

fotr_freqs_t <- table(fotr_word_v)

#Now we sort the table from most to least frequent word, and
#print it out to check.

sorted_fotr_freqs_t <- sort(fotr_freqs_t, decreasing = TRUE)
sorted_fotr_freqs_t

head(sorted_fotr_freqs_t)

#Great, so we have the absolute frequencies printed above.

#[Variable c in the synthesis chart] FOTR doesn't follow Zipf's law very
#well since 7500 IS about half of 11734, but 5085 is not is half of 7563, etc.

#Now plot. We have the top ten most frequent words stored in a vector,
#so we plot to visualize whether the frequencies of the 
#words correspond to Zipf's law using a visual interpretation.


top_ten <- sorted_fotr_freqs_t[1:11]
top_ten

plot(top_ten)
plot(top_ten, xlab = "Top ten words", ylab = "# of word occurances")
title(main="Frequency of the top 10 words in FOTR")

#Using visual interpretation, there isn't a correlation with Zipf's law
#that wordfrequency = wordfrequency subscript[(position+1)] ^1/2

#----Chapter 3: Accessing and comparing word freq. data-------------------------

#In Ch. 3, we derive and compare word frequency data. We will do this first for 
#FOTR but we will later on also do it for TT and ROTK.

#[Variable d in the synthesis chart] We calculate the absolute frequencies
#of "he" and "she" so we can write down a ratio

sorted_fotr_freqs_t["he"]
sorted_fotr_freqs_t["she"]

#Interestingly, the word "he" occurs 3028 times and "she" occurs only 159 times
#This doesn't tell us a lot about the people in the story though, since
#"she" here might refer to non-people objects as well, in the feminine
#case.

#Unlike the original fotr_word_v, where each word is indexed as a position
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

#If I want to know just how much MORE frequent "he" is to "she"
sorted_fotr_freqs_t["he"]/sorted_fotr_freqs_t["she"]
#which tells us that "the" is 19.04403 times more frequent than "she".

#[Variable e in synthesis chart] Repeat for adventure:home ratio.

sorted_fotr_freqs_t["adventure"]
sorted_fotr_freqs_t["home"]
sorted_fotr_freqs_t["home"]/sorted_fotr_freqs_t["adventure"]
#which tells us that "home" is 5 times more frequent than "adventure"

#Let's get a relative frequency of the words in FOTR. Converting
#raw counts to relative frequencies allows us to more easily compare
#the patterns of usage from one text to another, like here in Project 6.
#Let's calculate the relative frequency of "he," "she," "home," and 
#"adventure."

#As it stands, we have a sorted table of raw word counts. I want to covert
#those raw numbers to percentages, which means I have to divide
#each count by the total number of word tokens in the entire text
#I can access the total number of words using the length function on the 
#original word vector

length(fotr_word_v)
#which tells me that there are 226,550 words in there
#We make a chart to hold all the RELATIVE frequencies, i.e. an x percentage
#of the text is made up  of this word.

fotr_length_v <- length(fotr_word_v)
sorted_fotr_rel_freqs_t <- 100*(sorted_fotr_freqs_t/fotr_length_v)
sorted_fotr_rel_freqs_t

#OK that last line printed out all the relative frequencies of every single
#word in FOTR as PERCENTAGES Because there's a great variety of words
#used every 100 words, most of the values for relative frequencies are 
#smaller than 1 (i.e. that specific word appears less than once per 100 words.)
#I can now access any word type and return its relative freq. as a percentage. 


sorted_fotr_rel_freqs_t["he"]
sorted_fotr_rel_freqs_t["she"]

sorted_fotr_rel_freqs_t["adventure"]
#which gave us 0.006621055 % of text

sorted_fotr_rel_freqs_t["home"]
#meaning 'home' occurs 0.03310527% of text

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
#position, or index number, of the last word in FOTR 

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
length(adventure_v)

#Do the same for "home"

home_v <- which(fotr_word_v == "home")
home_v
length(home_v)
#Yep, the two printouts of "adventure" and "home" look right, since "home"
#does seem to have 5 times more occurances than "adventure" (75/15=5)

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

#other source of error: does R deal with accented letters? Yes, it does, look below
#for the example with sméagol      

#_________________________________THE TWO TOWERS________________________________


#First, read in the text file using the 'scan' function.


text_v <- scan("C:\\Users\\chris\\Downloads\\Token distribution with Regex - LOTR\\Two Towers.txt", what = "character", sep = ".")

#[variable on synthesis chart] It has read in 4496 items, the 4496 sentences in the text. 
#It's SUPER IMPROTANT to remember that we are overriding any previous
#data stored in text_v, i.e. text from the previous story, FOTR. If we want
#to find FOTR's info again, we'll need to go back to the top and run just that section of
#code from scratch

text_v


#OK, running the above line, we get the print of the whole Two Towers story.

#To see how many sentences there are in total:

length(text_v)

#There are 4496 sentences in FOTR.

#We tell it to start at the beginning (or wherever we like, just by changing the words in
#the quotation marks).

start_v <- which(text_v == "John Ronald Reuel Tolkien")


#Let's undo what we did and remove the line breaks (we don't need to know that  
#we have 4496 sentences anymore.)  Collapse it back to normal.

text_v <- paste(text_v, collapse = " ")

length(text_v)


#Now that we have the entire short story loaded as a SINGLE STRING of 
#characters, we are ready to have some fun.
#First use the tolower function to convert all the text to lowercase

text_lower_v <- tolower(text_v)

#We now have a big blob of TT in a single lowercase string. We need to extract 
#all of the words out of the full text string and put them into a nice organized
#list. Make sure we're using a CAPITAL W, not a lowercase one.

tt_word_l <- strsplit(text_lower_v, "\\W")


#use this above to split each word onto its own unit.

#Simplify this list into a vector using this function

tt_word_v <- unlist(tt_word_l)

#[variable in synthesis chart]find how many words are in TT
length(tt_word_v)

#which means there are some empty spaces (empty strings) where a period or 
#other spaceused to be. They're sort of annoying so let's get rid of them. 
#These following two lines of code first identify where those NOT blanks are,
#then they print them, so we can see exactly where

not_blanks_v <- which(tt_word_v != " ")
not_blanks_v

#Now overwrite tt_word_v like this

tt_word_v <- tt_word_v[not_blanks_v]

#Alright so now all we have are words and no blanks spaces! Our corpus 
#is pretty clean. To check that we only have words in the vector

tt_word_v

#Good, we have checked that it gives us words only. (It sort of looks
#like there are a few spaces, but we'll chalk that up to an error
#we'll have to account for later. Is it because of a badly written
#regex?) We're on page 26 of the 
#textbook now. We can also look words up by index number. E.g.



#I'm  curious as to how many unique word types there are in TT 

length(unique(tt_word_v))

#There are 7831 unique words types in TT.

#I can even see if TT abides by Zipf's law. We use a new variable 
#type, a table.

tt_freqs_t <- table(tt_word_v)

#Let's print/see the first few items in the table just to check.
tt_freqs_t[1:10]

#Now let's sort from most to least frequent word.

sorted_tt_freqs_t <- sort(tt_freqs_t, decreasing = TRUE)
sorted_tt_freqs_t

#we can actually see that the top 3 most frequently occuring
#charcater names in order are frodo, sam, and gandalf
#gollum also appears more often than smeagol; gollum appears 319 times
#while sméagol appears 143 times

#Now let's print it to check that it sorted

head(sorted_tt_freqs_t)

#OK, unsurprisingly, 'the' occurred the most and 'and' occurred second most in 
#TT, just like we saw in FOTR.

#This one doesn't follow Zipf's law well either.

#Now plot top 10 words' absolute frequency.

top_ten <- sorted_tt_freqs_t[1:11]
top_ten

plot(top_ten)
plot(top_ten, xlab = "Top ten words", ylab = "# of word occurances")
title(main="Absolute frequency of the top 10 words in TT")

#Using visual interpretation, there isn't a correlation with Zipf's law
#that wordfrequency = wordfrequency subscript[(position+1)] ^1/2

#----Chapter 3: Accessing and comparing word freq. data

#In Ch. 3, we derive and compare word frequency data. 
#[variable in synthesis chart]

sorted_tt_freqs_t["he"]
sorted_tt_freqs_t["she"]


sorted_tt_freqs_t["home"]
sorted_tt_freqs_t["adventure"]


#getting the relative frequency...

#As it stands, we have a sorted table of raw word counts. I want to covert
#those raw numbers to percentages, which means I have to divide
#each count by the total number of word tokens in the entire text
#I can access the total number of words using the length function on the 
#original word vector

length(tt_word_v)

#and now the relative frequency of "the" would just be ...

tt_length_v <- length(tt_word_v)
sorted_tt_rel_freqs_t <- 100*(sorted_tt_freqs_t/tt_length_v)
sorted_tt_rel_freqs_t

#OK that last line printed out all the relative frequencies of every single
#word in TT as percentages of the text.
#I can now access any word type and return its 
#relative freq. as a percentage. This percentage shows the number of occurances
#every 100 words (of course, think of it like per-cent, per 100)

sorted_tt_rel_freqs_t["adventure"]
#which gave us 0 times every 100 words - it doesn't occur?

#ah but what if we tried "adventures"?

sorted_tt_rel_freqs_t["adventures"]


sorted_tt_freqs_t["home"]
sorted_tt_rel_freqs_t["home"]

#[variable on synthesis chart]

sorted_tt_rel_freqs_t["he"]
sorted_tt_rel_freqs_t["she"]

#There's little point plotting the relative frequency, but we can surely
#use the chart of relative frequency later, in the synthesis step
#of this project, to compare FOTR, TT, and ROTK.

#----Chapter 4: token distribution/dispersion plots

#First, make dispersion plots. 

#map the sequence numbers (novelistic time) to the words in TT

n_time_v <- seq(from = 1, to = length(tt_word_v))

#This expression returns an integer vector (n_time_v) containing the
#positions of every word in the book. 
n_time_v

#Now you need to locate the position of every occurance of "adventure" in the
#FOTR, or, more specifically, in the fotr_word_v object. Use which to identify
#the locations in the vector that are in occurance of whale and then
#store them in a new integer vector called adventure_vector

adventure_v <- which(tt_word_v == "adventure")
adventure_v
length(adventure_v)

#again, didn't happen

#Do the same for "home"

home_v <- which(tt_word_v == "home")
home_v
length(home_v)


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


plot(a_count_v, main = "Token distribution plot of 'adventure' in TT", xlab = "Novelistic
     time", ylab = "occurances of 'adventure'", type = "h", ylim = c(0,1), yaxt = 'n')


#Repeat with "home".

h_count_v <- rep(NA, times = length(n_time_v))
h_count_v[home_v] <- 1

plot(t_count_v, main = "Token distribution plot of 'home' in TT", xlab = "Novelistic
     time", ylab = "occurances of 'home'", type = "h", ylim = c(0,1), yaxt = 'n')





#_____________________________THE RETURN OF THE KING____________________________


#First, read in the text file using the 'scan' function.


text_v <- scan("C:\\Users\\chris\\Downloads\\Token distribution with Regex - LOTR\\Return of the King.txt", what = "character", sep = ".")

#[variable on synthesis chart] It has read in 9235 items, the 9235 sentences in the text. 
#It's SUPER IMPROTANT to remember that we are overriding any previous
#data stored in text_v, i.e. text from the previous stories, FOTR and TT. If we want
#to find FOTR or TT's info again, we'll need to go back and run just that section of
#code from scratch

text_v


#OK, running the above line, we get the print of the whole ROTK story.

#To see how many sentences there are in total:

length(text_v)

#There are 9235 sentences in FOTR.

#We tell it to start at the beginning (or wherever we like, just by changing the words in
#the quotation marks).

start_v <- which(text_v == "John Ronald Reuel Tolkien")


#Let's undo what we did and remove the line breaks.  Collapse it back to normal.

text_v <- paste(text_v, collapse = " ")

length(text_v)


#Now that we have the entire short story loaded as a SINGLE STRING of 
#characters, we are ready to have some fun.
#First use the tolower function to convert all the text to lowercase

text_lower_v <- tolower(text_v)

#We now have a big blob of ROTK in a single lowercase string. We need to extract 
#all of the words out of the full text string and put them into a nice organized
#list. Make sure we're using a CAPITAL W, not a lowercase one.

rotk_word_l <- strsplit(text_lower_v, "\\W")


#use this above to split each word onto its own unit.

#Simplify this list into a vector using this function

rotk_word_v <- unlist(rotk_word_l)

#[variable in synthesis chart]find how many words are in ROTK
length(rotk_word_v)

#which means there are some empty spaces (empty strings) where a period or 
#other spaceused to be. They're sort of annoying so let's get rid of them. 
#These following two lines of code first identify where those NOT blanks are,
#then they print them, so we can see exactly where

not_blanks_v <- which(rotk_word_v != " ")
not_blanks_v

#Now overwrite tt_word_v like this

rotk_word_v <- rotk_word_v[not_blanks_v]

#Alright so now all we have are words and no blanks spaces! Our corpus 
#is pretty clean. To check that we only have words in the vector

rotk_word_v
length(rotk_word_v)

#Good, we have checked that it gives us words only. (It sort of looks
#like there are a few spaces, but we'll chalk that up to an error
#we'll have to account for later. Is it because of a badly written
#regex?) We're on page 26 of the 
#textbook now. We can also look words up by index number.

#I'm  curious as to how many unique word types there are in ROTK

length(unique(rotk_word_v))

#There are 7126 unique words types in ROTK

#I can even see if ROTK abides by Zipf's law. We use a new variable 
#type, a table.

rotk_freqs_t <- table(rotk_word_v)

#Let's print/see the first few items in the table just to check.
rotk_freqs_t[1:10]

#Now let's sort from most to least frequent word.

sorted_rotk_freqs_t <- sort(rotk_freqs_t, decreasing = TRUE)
sorted_rotk_freqs_t


#Now let's print it's head (just top 5 or 6) to check that it sorted

head(sorted_rotk_freqs_t)

#OK, unsurprisingly, 'the' occurred the most and 'and' occurred second most in 
#TT, just like we saw in FOTR and TT.

#This one doesn't follow Zipf's law well either.

#Now plot top 10 words' absolute frequency.

top_ten <- sorted_rotk_freqs_t[1:11]
top_ten

plot(top_ten)
plot(top_ten, xlab = "Top ten words", ylab = "# of word occurances")
title(main="Absolute frequency of the top 10 words in ROTK")

#Using visual interpretation, there isn't a correlation with Zipf's law
#that wordfrequency = wordfrequency subscript[(position+1)] ^1/2

#----Chapter 3: Accessing and comparing word freq. data

#In Ch. 3, we derive and compare word frequency data. 
#[variable in synthesis chart]

sorted_rotk_freqs_t["he"]
sorted_rotk_freqs_t["she"]


sorted_rotk_freqs_t["home"]
sorted_rotk_freqs_t["adventure"]


#getting the relative frequency...

#As it stands, we have a sorted table of raw word counts. I want to covert
#those raw numbers to percentages, which means I have to divide
#each count by the total number of word tokens in the entire text
#I can access the total number of words using the length function on the 
#original word vector

length(rotk_word_v)

#and now the relative frequency of "the" would just be ...

rotk_length_v <- length(rotk_word_v)
sorted_rotk_rel_freqs_t <- 100*(sorted_rotk_freqs_t/rotk_length_v)
sorted_rotk_rel_freqs_t

#OK that last line printed out all the relative frequencies of every single
#word in TT as percentages of the text.
#I can now access any word type and return its 
#relative freq. as a percentage. This percentage shows the number of occurances
#every 100 words (of course, think of it like per-cent, per 100)

sorted_rotk_rel_freqs_t["adventure"]


sorted_rotk_freqs_t["home"]
sorted_rotk_rel_freqs_t["home"]

#[variable on synthesis chart]

sorted_rotk_rel_freqs_t["he"]
sorted_rotk_rel_freqs_t["she"]

#There's little point plotting the relative frequency, but we can surely
#use the chart of relative frequency later, in the synthesis step
#of this project, to compare FOTR, TT, and ROTK.

#----Chapter 4: token distribution/dispersion plots

#First, make dispersion plots. 

#map the sequence numbers (novelistic time) to the words in ROTK

n_time_v <- seq(from = 1, to = length(rotk_word_v))

#This expression returns an integer vector (n_time_v) containing the
#positions of every word in the book. 
n_time_v

#Now you need to locate the position of every occurance of "adventure" in the
#ROTK, or, more specifically, in the rotk_word_v object. Use which to identify
#the locations in the vector that are in occurance of "adventure" and then
#store them in a new integer vector called adventure_vector

adventure_v <- which(rotk_word_v == "adventure")
adventure_v
length(adventure_v)


#Do the same for "home"

home_v <- which(tt_word_v == "home")
home_v
length(home_v)


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


plot(a_count_v, main = "Token distribution plot of 'adventure' in ROTK", xlab = "Novelistic
     time", ylab = "occurances of 'adventure'", type = "h", ylim = c(0,1), yaxt = 'n')


#Repeat with "home".

h_count_v <- rep(NA, times = length(n_time_v))
h_count_v[home_v] <- 1

plot(t_count_v, main = "Token distribution plot of 'home' in ROTK", xlab = "Novelistic
     time", ylab = "occurances of 'home'", type = "h", ylim = c(0,1), yaxt = 'n')






