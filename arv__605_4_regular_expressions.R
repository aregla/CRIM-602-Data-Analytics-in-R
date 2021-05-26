##Assignment #4: Regular Expression p. 1 
##Name: Alejandra Regla-Vargas
##Date: October 4, 2020 

#Load data and set working directory 
setwd("~/Desktop/Coursework-year-3/SOC 605/Week 1/Data/Scrabble")
words <- scan(file="scrabble4letter.txt",what="")

#1. All words that start with Z 
grep("^[z]",words,value=TRUE)


#2. All words that have zz in them 
grep("zz", words, value=TRUE)

grep("[^a][e][i][o][u]",data.text,value=TRUE)


#3. All words that do not have "a", "e", "i", "o", or "u"
grep("[^a][e][i][o][u]",words,value=TRUE)

#4. In which letter position are you most likely to find an "r"
#locate words with letter r 
grep("r",words,value=TRUE)

#Start 
r.start <- grep("^r", words, value = TRUE)
length(r.start)
#End 
r.end <- grep("r$", words, value = TRUE)
length(r.end)

#Not an edge of the word 
r.notedge <- grep("^[r]|[r]$", letter.r, value=TRUE, invert = TRUE)
length(r.notedge)

#Solution: Words that include the letter "r" are most likely to be non-edge words, meaning they do not begin or end with r (n= 538) 


#5. Words that rhyme with "mitt"
grep("itt$",words,value=TRUE)

#bitt rhymes with mitt 

q.5.2 <- grep("it$",q.5,value=TRUE)
q.5.3 <- grep("ai", q.5.2, value=TRUE, invert=TRUE)
grep("ui", q.5.3, value=TRUE, invert=TRUE)

#brit, chit, crit, grit, knit, shit, skit, slit, smit, whit, writ 



