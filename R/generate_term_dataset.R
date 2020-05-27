#' Generate frequencies of terms for each paper
#'
#' @param cleaned_text output of 'clean.text' - a list of cleaned text files
#' @param in_dir directory with input text files
#' @param keywords A set of keywords as characters (i.e. traits of interest) in a vector
#'
#' @return A list of frequencies of keywords for each paper
#' @export
#'
#' @examples
#' ct<-clean.text(in_dir = "./inst/extdata/test_pdfs/")
#' generate.term.dataset(cleaned_text = ct, in_dir = "./inst/extdata/testpdfs/",keywords = c("bisse","musse"))
#'
generate.term.dataset <- function(cleaned_text,in_dir, keywords) {

    options(stringsAsFactors = FALSE)

    data_files <- list.files(path = in_dir, pattern = "*.txt$", full.names = T)


    # empty lists to fill
    combined_text <- list(rep("NULL", length(cleaned_text)))
    sorted_words <- list(rep("NULL", length(cleaned_text)))
    term_freq <- vector()

    for (k in 1:length(cleaned_text)) {
        if (length(cleaned_text[[k]]) > 0) {

            combined_text[[k]] <- stri_join_list(list(cleaned_text[[k]]))

            # change text list to corpus format
            corpus_txt <- Corpus(VectorSource(combined_text[[k]]))

            # make character maps
            corp <- tm_map(corpus_txt, removePunctuation, ucp = TRUE)

            # make term matrix
            corp.tdm <- TermDocumentMatrix(corp, control = list(removePunctuation = T, stopwords = TRUE, tolower = TRUE, stemming = F,
                removeNumbers = T))

            corp.tdm

            # calculate frequency of terms
            ft <- findFreqTerms(corp.tdm, lowfreq = 1, highfreq = Inf)
            as.matrix(corp.tdm[ft, ])

            ft <- ft[ft %in% keywords]

            # change into matrix and sort by frequency
            ft.tdm <- as.matrix(corp.tdm[ft, ])
            sorted_words[[k]] <- sort(apply(ft.tdm, 1, sum), decreasing = TRUE)
            term_freq <- cbind(term_freq, names(sorted_words[[k]]))

        }
    }

    ### FIGURE OUT WARNINGS HERE

    return(sorted_words)

    save.image("biglistterms.Rdata")

}
