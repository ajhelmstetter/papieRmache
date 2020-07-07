#' Clean text files
#'
#' @param in_dir directory with input text files
#' @param all_keywords vector with all keywords you are interested in
#' @return list of cleaned texts
#' @export
#'
#' @examples
#'
#' download.file("https://github.com/ajhelmstetter/papieRmache/raw/master/inst/extdata/test_pdfs.zip", destfile = "./test_pdfs.zip")
#' unzip("./test_pdfs.zip")
#' clean.text(in_dir = "./test_pdfs/",all_keywords=kw)
clean.text <- function(in_dir,all_keywords) {

    ###
    # true/false param to be added for taxonomy
    ###

    options(stringsAsFactors = FALSE)

    # Build a dictionary of lemmas (stems of words)

    ###
    # make customisable
    ###

    lemma_data <- lemma_data

    # extended stop word list

    ###
    # make customisable
    ###

    stopwords_extended <- stopwords_extended

    print("input directory:")
    print(in_dir)
    # list all text files in folder
    data_files <- list.files(path = in_dir, pattern = "*.txt$", full.names = T)

        # create vector of 4digit year in each filename
    list_txt <- list()
    # loop through and read all text files to a list
    for (j in c(1:length(data_files))) {
        list_txt[[j]] <- readtext(data_files[j], text_field = "texts")
    }

    list_years <- vector()
    # add years to vector
    for (i in 1:length(list_txt)) {
        m <- gregexpr("[0-9]+", list_txt[[i]]$doc_id)
        list_years[i] <- regmatches(list_txt[[i]]$doc_id, m)[[1]]
    }


    cleaned_text <- list()

    # will need to start loop here try those paras that have methods/results and name of model unique if multiple

    for (h in 1:length(list_txt)) {

        #print filename

        print(paste("file",h,data_files[h]))

        # read in text files
        extracted_texts <- readtext(data_files[h], docvarsfrom = "filepaths", dvsep = "/")

        # the extracted_texts can be written by write.csv2 to disk for later use.
        write.csv2(extracted_texts, file = "text_extracts.csv", fileEncoding = "UTF-8")

        ###
        # figure out if i really need to read/write csv
        ###

        textdata <- read.csv("text_extracts.csv", header = TRUE, sep = ",", encoding = "UTF-8")
        system('rm text_extracts.csv')


        # change text into corpus
        sotu_corpus <- corpus(textdata[1, ])
        sotu_corpus <- tolower(sotu_corpus)
        sotu_corpus <- replace_white(sotu_corpus)
        sotu_corpus <- stripWhitespace(sotu_corpus)

        # removes last section (but if split words are in references section some may remain)

        corpus_split <- corpus_segment(sotu_corpus, pattern = "references", valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE,
            extract_pattern = F, pattern_position = c("before", "after"), use_docvars = TRUE)

        corpus_split <- corpus_split[1:(length(corpus_split) - 1)]

        textdata <- stri_paste_list(list(corpus_split), sep = "", collapse = NULL)

        ## length(paste(corpus_split[[1]])) textdata<-stri_paste_list(list(corpus_split[[1]]), sep = '', collapse = NULL)
        sotu_corpus <- corpus(textdata)

        # split text files by keyword sections<-c('methods','results','discussion','literature cited','acknowledgments') for(m in
        # 1:length(sections)){ corpus_split<-strsplit(sotu_corpus[[1]],sections[m]) for(n in 1:length(corpus_split[[1]])){
        # corpus_split[[1]][n]<-paste(sections[m],corpus_split[[1]][n],sep = ' ') } }

        # reformat into paras
        corpus_split <- strsplit(sotu_corpus[[1]], "\n\n")
        corpus_split <- corpus_split[[1]]
        length(corpus_split)

        for (o in 1:length(corpus_split)) {
            corpus_split[o] <- gsub("\\s+", " ", corpus_split[o])
            corpus_split[o] <- gsub("\n", " ", corpus_split[o])
            corpus_split[o] <- gsub("\\\\", " ", corpus_split[o])
        }

        # find out format of article if normal paper layout methods line number should be smaller than results or discussion
        # m_loc<min(r_loc) || m_loc<min(d_loc)

        kw <- all_keywords

        ## ask about whether you want to include taxonomy (long)

        mod_pres <<- kw

        # find paragraphs that mention models
        lines_with_mods <- vector()
        for (p in 1:length(mod_pres)) {
            lines_with_mods <- c(lines_with_mods, grep(mod_pres[p], corpus_split, ignore.case = TRUE))
        }
        cleaned_text[[h]] <- corpus_split[unique(lines_with_mods)]
        cl_sp <- strsplit(cleaned_text[[h]], "reference")
        cl_sp <- cl_sp[1:(length(cl_sp) - 1)]
        cleaned_text[[h]] <- paste(cl_sp, collapse = " ")

    }

    return(cleaned_text)
}
