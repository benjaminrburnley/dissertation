library(quanteda)
library(quanteda.textplots)
library(readtext)


# import ------------------------------------------------------------------
# create a temprary directory to store texts
dir.create("tmp")
# download texts
download.file(url = "https://www.gutenberg.org/files/1342/1342-0.txt", 
              destfile = "tmp/Pride and Prejudice_Jane Austen_2008_English.txt")
download.file(url = "https://www.gutenberg.org/files/98/98-0.txt", 
              destfile = "tmp/A Tale of Two Cities_Charles Dickens_2009_English.txt")
# read in texts
dataframe <- readtext("tmp/*.txt",
                      docvarsfrom = "filenames",
                      docvarnames = c("title", "author", 
                                      "year uploaded", "language"),
                      dvsep = "_",
                      encoding = "UTF-8")
# delete tmp directory
unlink("tmp", recursive = TRUE)


# wrangle -----------------------------------------------------------------

# make corpus
doc.corpus <- corpus(dataframe)
summary(doc.corpus)

# other token options
doc.tokens <- tokens(doc.corpus)
doc.tokens.sentence <- tokens(doc.corpus, what = "sentence")
doc.tokens.character <- tokens(doc.corpus, what = "character")

# removing punctuation and numbers
doc.tokens <- tokens(doc.tokens, remove_punct = TRUE, 
                     remove_numbers = TRUE)

# removing stop words
doc.tokens <- tokens_select(doc.tokens, stopwords('english'),selection='remove')

# stem tokens 
doc.tokens <- tokens_wordstem(doc.tokens)

# take to lower case 
doc.tokens <- tokens_tolower(doc.tokens)

# create dfm 
doc.dfm.final <- dfm(doc.tokens)

# Keyword Analysis  ---------------------------------------------------------

# keywords in context - note this will not work on dfm
head(kwic(doc.tokens, "love", window = 3))

# to see all keywords-in-context
# View(kwic(doc.tokens, "love", window = 3))

# top features
topfeatures(doc.dfm.final, 5)
topfeatures(doc.dfm.final, 20)


# cooccurance  ------------------------------------------------------------

# create feature coocurance matrix 
doc.fcm = doc.tokens |> 
  fcm(context = "window", window = 5, tri = F)

# chose 30 most frequent features 
topfeats = names(topfeatures(doc.fcm, 30))

# plot 
set.seed(100)
fcm_select(doc.fcm, topfeats) |> 
  textplot_network(min_freq = 0.6)

