#Einstellungen und Bibliotheken#################################################

setwd("C:/Users/sophi/OneDrive/Dokumente/R")
options(stringsAsFactors = FALSE)
library(quanteda)
require(topicmodels)
library(LDAvis)
library("Rtsne")
library(udpipe)
ud_model <- udpipe_download_model(language = "german")
ud_model <- udpipe_load_model(ud_model$file_model)

#Text preprocessing#############################################################

#nur für Brief an den Vater
textdata <- readLines("C:/Users/sophi/OneDrive/Dokumente/R/Brief/kafka_Brief_1919.txt", encoding = "UTF-8")
textdata <- paste(textdata, collapse = " ")

#nur für die Werkesammlung
text_dir <- "C:/Users/sophi/OneDrive/Dokumente/R/texte/"
textdata <- sapply(list.files(text_dir, pattern = "\\.txt$", full.names = TRUE), readLines, encoding = "UTF-8")
textdata <- sapply(textdata, paste, collapse = " ")  

#nur Texte mit explizitem Vater
text_dir <- "C:/Users/sophi/OneDrive/Dokumente/R/Vater"
textdata <- sapply(list.files(text_dir, pattern = "\\.txt$", full.names = TRUE), readLines, encoding = "UTF-8")
textdata <- sapply(textdata, paste, collapse = " ")

prozess_corpus <- corpus(textdata)
lemma_data <- as.data.frame(udpipe_annotate(ud_model, x = paste(textdata, collapse = " ")))
lemma_list <- lapply(textdata, function(x) as.data.frame(udpipe_annotate(ud_model, x = x)))
# lemma_data <- do.call(rbind, lemma_list)
lemma_data$lemma[is.na(lemma_data$lemma)] <- lemma_data$token[is.na(lemma_data$lemma)]
stopwords <- stopwords("de")

corpus_tokens <- prozess_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(pattern = lemma_data$token, replacement = lemma_data$lemma, valuetype = "fixed") %>%
  tokens_tolower() %>%
  tokens_remove(pattern = stopwords, padding = T)

prozess_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens, min_count = 5)
prozess_collocations <- prozess_collocations[1:100, ]

#nur für Brief an den Vater
prozess_collocations <- na.omit(prozess_collocations)

corpus_tokens <- tokens_compound(corpus_tokens, prozess_collocations)

#Model Calculation##############################################################

DTM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm()

#dim(DTM)

top7_terms <- c("er", "es", "sie", "k", "ihr", "wir", "sagen")
DTM <- DTM[, !(colnames(DTM) %in% top7_terms)]
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx]

#LDA############################################################################

K <- 20
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, seed = 1, verbose = 25, alpha = 0.02))

tmResult <- posterior(topicModel)
attributes(tmResult)
ncol(DTM)
beta <- tmResult$terms
dim(beta)
rowSums(beta)
nrow(DTM)

theta <- tmResult$topics
dim(theta)
rowSums(theta)[1:10]
terms(topicModel, 10)
top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")

#Visualization##################################################################

svd_tsne <- function(x) {
  tsne_matrix <- svd(x)$u
  Rtsne(tsne_matrix, perplexity = 5)$Y
}

json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM), vocab = colnames(DTM), term.frequency = colSums(DTM), mds.method = svd_tsne, plot.opts = list(xlab = "", ylab = ""))
serVis(json)

#Filtering the topics###########################################################

topicsToFilter <- grep("vater", topicNames, ignore.case = TRUE)
print(topicsToFilter)

for (topic in topicsToFilter) {
  cat("\nWichtige Begriffe für Topic", topic, ":\n")
  print(terms(topicModel, 10)[, topic])
}

require(wordcloud2)
topicToViz <- 14
top40terms <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:40]
words <- names(top40terms)
probabilities <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:40]
wordcloud2(data.frame(words, probabilities), shuffle = FALSE)

#tes
