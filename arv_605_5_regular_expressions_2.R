##Assignment #5: Regular Expression p. 2
##Name: Alejandra Regla-Vargas
##Date: October 8, 2020

#Load data and set working directory 
setwd("~/Desktop/Coursework-year-3/SOC 605/Week 1/Data/Scrabble")
words <- scan(file="scrabble4letter.txt",what="")

# 1. All words with double letters 
grep("([A-Za-z])\\1", words,value=TRUE)

#2 Palindromes, words spelled the same backwards and forwards 
grep(pattern = '(.)(.)\\2\\1', words, value = TRUE, ignore.case = TRUE)

#1 Replace all double "l"s with a single "l"
ll <-grep("ll", words, value=TRUE)
gsub("ll", "l", ll)

#2 Capitalize all words that start with the letter "g"
g <-grep("^g", words, value=TRUE)
gsub("g", "G", g)

#3 Delete the last three letters of every word
minus_3 <- gsub('.{3}$', '', words)

#4 Use table() to tabulate the most common starting letter for four letter words
words %>% 
  paste(collapse="") %>% 
  strsplit(split="") %>% unlist %>% 
  `[`(!. %in% c("", " ", ".", ",")) %>% 
  table %>% 
  barplot

grep("^a",words,value=TRUE)
grep("^e",words,value=TRUE)

#More words begin with the letter A 
