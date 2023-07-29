# A brief to R

# Working directory
getwd()

# Getting Start

# Print out Hello World

a <- "Hello World!"
print("Hello World")
print(a)
cat(a)
b <- "Welcome to ISCM"
print("Welcome to ISCM")
print(b)
cat(b)
cat(a, b)
print(paste(a, b))

a=2
b=3
c = a+b
c


library(tidyverse)
library(dplyr)
library(reshape)
library(reshape2)
library(readxl)
library(xlsx)








# Package in R
#install package (using CRAN, or using Dev.Tool)
#---------->install.packages()
#loading package
#---------->library()

# let play some funs with Emoji package
#https://cran.r-project.org/web/packages/emoji/index.html

library(emoji)

arrow("up-down")

arrow(c("up", "up", "down", "down", "left", "right", "left", "right"))


times <- as.POSIXct("2021-09-17 14:33:21 PDT") + seq(1:30) * 3500
clock(times)

string <- paste(c(letters[1:4], emoji_name[1:6]), collapse = " ")
emoji_count(string)
emoji_count(emoji_name[1:6])

#https://github.com/hadley/emo

run = "running"
replicate(3,emo::ji(run))

# Cong Tru Nhan Chia

library(car)




# R is a calculator
3+4

3-4

3*4

3/4

# Rounding

1/6

round(1/6,2)
n = 1:5
round(1/6,n)

#power and log

3**4
3^4

2^3

2**3

2*3

log(3)

d <- log(3)

exp(3)

e = exp(3)

f = d+e +pi




pi





# --> there are some predetermine function name in R and we should avoid to use them.


# Object in R --> R is a object oriented program.

object1 <- 3
object2 <-4

x <- object1+object2
x


# Objects can be numeric or character


char_obj1 <- "Chao"
char_obj2 <- "ISCM!"

char_obj3 <- char_obj1 + char_obj2



# vector function

my_vec <- c(2, 3, 1, 6, 4, 3, 3, 7)

my_vec

#random

mean(my_vec)

median(my_vec)

median(c(1,2,3,4,6))



sd(my_vec)

var(my_vec)

max(my_vec)

min(my_vec)

which.max(my_vec)

which.min(my_vec)

length(my_vec)


#sequence function

my_seq1 <- c(1,2,3,4,5)

n=5
vector = 1:n




my_seq1 <- 1:n


my_seq1



my_seq1^2

my_seq2 <- 5:1

my_seq2


my_seq3 <- seq(from = 1, to = 5, by = 0.3)

my_seq3


# Replication

my_rep1 <- rep(3,5)

my_rep1


my_rep2 <- rep("Hellow World!",4)

my_rep2

a <- 1:5

a

b =4


my_rep3 <- rep(a,b)

my_rep3





my_seq4 <- rep(1:5, each = 4)
my_seq4


# Working with vectors
# vector indexes
my_vec <- c(2, 3, 1, 6, 4, 3, 3, 7)

my_vec[2]





# Missing Data

temp  <- c(7.2, NA, 7.1, 6.9, 6.5, 5.8, 5.8, 5.5, NA, 5.5)
temp
mean_temp <- mean(temp)
mean_temp

mean_temp <- mean(temp, na.rm = TRUE)
mean_temp


# Data Types

#numberic, integers, logical, character
# using class() to find the type of data

class(temp)


class(a)

logi <- TRUE
class(logi)

list_1 <- list(c("black", "yellow", "orange"),
               c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
               matrix(1:6, nrow = 3))
list_1



p.height <- c(180, 155, 160, 167, 181)
p.weight <- c(65, 50, 52, 58, 70)
p.names <- c("Joanna", "Charlotte", "Helen", "Karen", "Amy")

dataf <- data.frame(height = p.height, weight = p.weight, names = p.names)
dataf

## Data importing

flowers01 <- read.table(file = '../Data/rbook/flower.txt', header = TRUE, sep = "\t",
                      stringsAsFactors = TRUE)

flowers02 <- read.csv(file = '../Data/rbook/flower.csv') 

flowers03 <- read.delim(file = '../Data/rbook/flower.txt') 


library(readr)
# import white space delimited files
flowers04 <- read_table(file = '../Data/rbook/flower.txt', col_names = TRUE)

# import comma delimited files
flowers05 <- read_csv(file = '../Data/rbook/flower.csv')

# import tab delimited files
flowers06 <- read_delim(file = '../Data/rbook/flower.txt', delim = "\t")

# or use
flowers07 <- read_tsv(file = '../Data/rbook/flower.txt')




### RESHAPING
library(reshape)
library(reshape2)

long_data <- data.frame(
  subject = rep(c("A", "B", "C", "D"), each = 3),
  sex = rep(c("M", "F", "F", "M"), each =3),
  condition = rep(c("control", "cond1", "cond2"), times = 4),
  measurement = c(12.9, 14.2, 8.7, 5.2, 12.6, 10.1, 8.9,
                  12.1, 14.2, 10.5, 12.9, 11.9))
long_data

wide_data <- data.frame(subject = c("A", "B", "C", "D"),
                        sex = c("M", "F", "F", "M"),
                        control = c(12.9, 5.2, 8.9, 10.5),
                        cond1 = c(14.2, 12.6, 12.1, 12.9),
                        cond2 = c(8.7, 10.1, 14.2, 11.9))
wide_data

library(reshape)

my_long_df <- melt(data = wide_data, id.vars = c("subject", "sex"),
                   measured.vars = c("control", "cond1", "cond2"),
                   variable.name = "condition", value.name = "measurement")
my_long_df


my_wide_df <- dcast(data = long_data, subject + sex ~ condition, 
                    value.var = "measurement")


my_wide_df