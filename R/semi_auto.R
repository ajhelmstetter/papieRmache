####################################################################################################################
########################################## SEMI.AUTO ###############################################################
####################################################################################################################

#' Interactively retrieve qualitative data from a paper
#'
#' @param keywords A set of keywords as characters (i.e. traits of interest) in a vector
#' @param highlight A set of keywords as characters (i.e. words related to trait of interest) in a vector
#' @param n A number specifying the threshold to consider keyword present
#' @param exact binary 0/1 - match exactly the words?
#' @param in_dir directory with input text files
#' @param cleaned_text output of 'clean.text' - a list of cleaned text files
#' @param sorted_words output of 'generate.term.dataset' - a list of frequency of searched words mentioned in each paper
#'
#' @return A data frame containing matches above threshold and verified matches
#' @export
#'
#' @examples
#' download.file("https://github.com/ajhelmstetter/papieRmache/raw/master/inst/extdata/test_pdfs.zip", destfile = "./test_pdfs.zip")
#' unzip("./test_pdfs.zip")
#'
#' ct<-clean.text(in_dir = "./test_pdfs/",all_keywords=kw)
#'
#' keywords<-c('bisse','musse')
#'
#' td<-generate.term.dataset(cleaned_text = ct, in_dir = "./testpdfs/",keywords = keywords)
#'
#' semi.auto(in_dir = "./test_pdfs/", keywords = keywords, highlight = c('increase','decrease'), exact = 0, n = 5, cleaned_text = ct, sorted_words = td)
#'
semi.auto <-
    function(in_dir ,
             keywords,
             highlight,
             n,
             exact,
             cleaned_text,
             sorted_words) {

        options(stringsAsFactors = FALSE)

        data_files <-
            list.files(path = in_dir,
                       pattern = "*.txt$",
                       full.names = T)

        traits_mat <-
            matrix(nrow = length(cleaned_text),
                   ncol = length(keywords))
        colnames(traits_mat) <- keywords

        head(traits_mat)

        # check where traits are present, may be present and are not present
        # based on threshold 'n'


        if(exact == 1){
        for (h in 1:length(keywords)) {
                for (j in 1:length(cleaned_text)) {
                    if (keywords[h] %in% names(sorted_words[[j]]) == FALSE) {
                        traits_mat[, keywords[h]][j] <- "n"
                    } else if (sorted_words[[j]][keywords[h]] > n) {
                        traits_mat[, keywords[h]][j] <- "y"
                    } else {
                        traits_mat[, keywords[h]][j] <- "m"
                    }
                }
        }
        } else {
        for (h in 1:length(keywords)) {
            for (j in 1:length(cleaned_text)) {
                if (length(grepl(keywords[h],names(sorted_words[[j]]))) < 1) {
                    traits_mat[, keywords[h]][j] <- "n"
                } else if (grepl(keywords[h],names(sorted_words[[j]]))[1] == FALSE) {
                    traits_mat[, keywords[h]][j] <- "n"
                } else if (sum(sorted_words[[j]][grep(keywords[h],names(sorted_words[[j]]))]) > n) {
                    traits_mat[, keywords[h]][j] <- "y"
                } else {
                    traits_mat[, keywords[h]][j] <- "m"
                }
            }
        }
        }

        # get which paragraphs may mention trait in document

        # make file for output
        cat("# trait used to check", sep = "\n", file = "trait.Rmd")

        # print to console width
        options(width = 80)

        for (g in 1:length(keywords)) {

            #create Rmd file
            #cat(
            #    paste("##", keywords[g]),
            #    sep = "\n",
            #    file = "trait.Rmd",
            #    append = T
            #)

            # column with for trait
            traits_mat_y <- traits_mat[, keywords[g]]

            # which articles may reference trait
            mp <- which(traits_mat_y == "m")

            if (length(mp) > 0) {
                for (d in 1:length(mp)) {
                    # cat trait name
                    cat(
                        paste("###", data_files[mp[d]]),
                        sep = "\n \n",
                        file = "trait.Rmd",
                        append = T
                    )
                    ### merge adjacent paragraphs
                    comb_paras <-
                        paste(cleaned_text[mp[d]], collapse = " ")

                    # Split the string into individual words
                    splitString <- strsplit(comb_paras, " ")[[1]]

                    # Find the location of the word of interest
                    loc <- grep(keywords[g], splitString)

                    # Subset as you normally would
                    comb_occs <- list()

                    if (length(loc) > 0) {
                        for (j in 1:length(loc)) {
                            if ((loc[j] - 50) > 0) {
                                cat(
                                    paste(splitString[c((loc[j] - 50):(loc[j] + 50))], collapse = " "),
                                    sep = "\n",
                                    file = "trait.Rmd",
                                    append = T
                                )
                                cat(
                                    paste(""),
                                    sep = "\n",
                                    file = "trait.Rmd",
                                    append = T
                                )

                                comb_occs[[j]] <-
                                    paste(splitString[c((loc[j] - 50):(loc[j] + 50))], collapse = " ")
                            } else {
                                cat(
                                    paste(splitString[c((loc[j]):(loc[j] + 50))], collapse = " "),
                                    sep = "\n",
                                    file = "trait.Rmd",
                                    append = T
                                )
                                cat(
                                    paste(""),
                                    sep = "\n",
                                    file = "trait.Rmd",
                                    append = T
                                )

                                comb_occs[[j]] <-
                                    paste(splitString[c((loc[j]):(loc[j] + 50))], collapse = " ")
                            }
                        }
                    } else {
                        comb_occs[[1]] <- "error"
                    }

                    #QUESTION ASKED
                    cat(paste(
                        paste(
                            "Is",
                            red$bold$underline(keywords[g]),
                            "used in",
                            data_files[mp[d]],
                            "?",
                            sep = " "
                        ),
                        sep = "\n"
                    ))
                    cat("", sep = "\n")
                    cat("", sep = "\n")

                    #
                    for (k in 1:length(comb_occs)) {
                        unique_words <- lapply(strsplit(comb_occs[[k]], " "), function(x) {
                            x[!x == ""]
                        })

                        # creating a dataframe with crayonized text
                        df <-
                            tibble::enframe(unique_words) %>% tidyr::unnest(cols=c(value)) %>%
                            # here you can specify the color/word combinations you need
                            dplyr::mutate(
                                .data = .,
                                value2 = dplyr::case_when(
                                    grepl(keywords[g], value) ~ crayon::red$bold$underline(value),
                                    grepl(paste(highlight,collapse="|"), value) ~ crayon::blue$bold$underline(value),
                                    TRUE ~ value
                                )
                            ) %>% dplyr::select(., -value)

                        cat(df$value2)

                        # cat(paste(comb_occs[[k]]),sep='\n'))
                        cat("", sep = "\n")
                        cat("", sep = "\n")
                    }
                    answ <- readline(prompt = "y or n?: ")
                    traits_mat[mp[d], keywords[g]] <- answ

                }
            }
        }

        rownames(traits_mat) <- data_files

        ### Transform data into values per paper (e.g. names of models used)

        traits_vect <- vector()

        if (length(keywords) == 1) {
            for (i in 1:length(rownames(traits_mat))) {
                if (traits_mat[i, 1] == "y") {
                    traits_vect[i] <- colnames(traits_mat)
                } else {
                    traits_vect[i] <- NA
                }

            }
        } else {
            for (i in 1:length(rownames(traits_mat))) {
                traits_vect[i] <-
                    paste(names(traits_mat[i,][traits_mat[i,] == "y"]), collapse = ",")
            }
        }



        names(traits_vect) <- data_files

        return(traits_vect)

    }


####################################################################################################################
########################################## SEMI.AUTO.PAIRED ########################################################
####################################################################################################################


#' Interactively retrieve paired qualitative data from a paper. If you want to exit the interactive mache earlier type 'quit' as your response
#'
#' @param keywords1 A vector of keywords as characters (i.e. traits of interest) in a vector
#' @param keywords2 A second vector of keywords that must also be in the extracted text (exact matches only)
#' @param highlight A vector of keywords that are highlighted in the printed text
#' @param n A number specifying the threshold to consider keyword present
#' @param in_dir directory with input text files
#' @param cleaned_text output of 'clean.text' - a list of cleaned text files
#' @param sorted_words output of 'generate.term.dataset' - a list of frequency of searched words mentioned in each paper
#'
#' @return A data frame containing matches above threshold and verified matches
#' @export
#'
#' @examples
#' download.file("https://github.com/ajhelmstetter/papieRmache/raw/master/inst/extdata/test_pdfs.zip", destfile = "./test_pdfs.zip")
#' unzip("./test_pdfs.zip")
#'
#' ct<-clean.text(in_dir = "./test_pdfs",all_keywords=kw)
#'
#' keywords1<-c('diversification')
#' keywords2<-c('increase','decrease')
#'
#' td<-generate.term.dataset(cleaned_text = ct, in_dir = "./testpdfs/",keywords = keywords1)
#'
#' semi.auto.paired(in_dir = "./test_pdfs/", keywords1 = keywords1, keywords2 = keywords2, highlight = c('increase','decrease'), n = 100, cleaned_text = ct, sorted_words = td)
#'
semi.auto.paired <-
    function(in_dir ,
             keywords1,
             keywords2,
             highlight,
             n,
             cleaned_text,
             sorted_words) {

        options(stringsAsFactors = FALSE)

        data_files <-
            list.files(path = in_dir,
                       pattern = "*.txt$",
                       full.names = T)

        data_files_names <-
            list.files(path = in_dir,
                       pattern = "*.txt$")

        traits_mat <-
            matrix(nrow = length(cleaned_text),
                   ncol = length(keywords1))
        colnames(traits_mat) <- keywords1

        head(traits_mat)

        # check where traits are present, may be present and are not present
        for (h in 1:length(keywords1)) {
            for (j in 1:length(cleaned_text)) {
                if (keywords1[h] %in% names(sorted_words[[j]]) == FALSE) {
                    traits_mat[, keywords1[h]][j] <- "n"
                } else {
                    traits_mat[, keywords1[h]][j] <- "m"
                }
            }
        }

        # get which paragraphs may mention trait in document

        # print to console width
        options(width = 80)

        for (g in 1:length(keywords1)) {

            # column with for trait
            traits_mat_y <- traits_mat[, keywords1[g]]

            # which articles may reference trait
            mp <- which(traits_mat_y == "m")

            if (length(mp) > 0) {

                for (d in 1:length(mp)) {

                    ### merge adjacent paragraphs
                    comb_paras <-
                        paste(cleaned_text[mp[d]], collapse = " ")

                    # Split the string into individual words
                    splitString <- strsplit(comb_paras, " ")[[1]]

                    # Find the location of the word of interest
                    loc <- grep(keywords1[g], splitString)

                    # Subset as you normally would
                    comb_occs <- list()

                    if (length(loc) > 0) {
                        for (j in 1:length(loc)) {
                            if ((loc[j] - 50) > 0) {
                                comb_occs[[j]] <-
                                    paste(splitString[c((loc[j] - 50):(loc[j] + 50))], collapse = " ")
                            } else {
                                comb_occs[[j]] <-
                                    paste(splitString[c((loc[j]):(loc[j] + 50))], collapse = " ")
                            }
                        }
                    } else {
                        comb_occs[[1]] <- "error"
                    }

                    #remove paras without mention of keywords2
                    comb_occs_paired<-list()

                    cnt<-1

                    for(p in 1:length(comb_occs)){

                        #if(length(keywords2 %in% comb_occs[[p]])){
                        if(length(grep(paste(keywords2,collapse=" | "), comb_occs[[p]])) > 0 ){
                            comb_occs_paired[[cnt]]<-comb_occs[[p]]
                            cnt<-cnt+1
                        }
                    }

                    comb_occs<-comb_occs_paired

                    if(length(comb_occs)>1){

                    #QUESTION ASKED
                    cat("", sep = "\n")
                    cat("-----------------------------------------------------------------------------", sep = "")
                    cat("", sep = "\n")
                    cat(paste(
                        paste(
                            red$bold$underline(keywords1[g]),
                            "in",
                            data_files_names[mp[d]],
                            sep = " "
                        ),
                        sep = "\n"
                    ))
                    cat("", sep = "\n")
                    cat("-----------------------------------------------------------------------------", sep = "")
                    cat("", sep = "\n")
                    cat("", sep = "\n")

                    #
                    for (k in 1:length(comb_occs)) {
                        unique_words <- lapply(strsplit(comb_occs[[k]], " "), function(x) {
                            x[!x == ""]
                        })

                        # creating a dataframe with crayonized text
                        df <-
                            tibble::enframe(unique_words) %>% tidyr::unnest(cols = c(value)) %>%
                            # here you can specify the color/word combinations you need
                            dplyr::mutate(
                                .data = .,
                                value2 = dplyr::case_when(
                                    grepl(keywords1[g], value) ~ crayon::red$bold$underline(value),
                                    grepl(paste(keywords2,collapse="|"), value) ~ crayon::green$bold$underline(value),
                                    grepl(paste(highlight,collapse="|"), value) ~ crayon::blue$bold$underline(value),
                                    TRUE ~ value
                                )
                            ) %>% dplyr::select(., -value)

                        cat(df$value2)

                        # cat(paste(comb_occs[[k]]),sep='\n'))
                        cat("", sep = "\n")
                        cat("", sep = "\n")
                    }
                    answ <- readline(prompt = "entry: ")

                    if(answ == "quit"){
                        #rownames(traits_mat) <- data_files_names
                        traits_mat <- data.frame(traits_mat)
                        traits_mat[traits_mat == ""] <- NA
                        traits_mat[traits_mat == "n"] <- NA
                        break
                    } else {
                        traits_mat[mp[d], keywords1[g]] <- answ
                    }



                    }
                if(answ == "quit"){
                    break
                }
                }
            if(answ == "quit"){
                break
            }
            }
        if(answ == "quit"){
            return(traits_mat)
            break
        }
            rownames(traits_mat) <- data_files_names

            traits_mat <- data.frame(traits_mat)

            traits_mat[traits_mat == ""] <- NA
            traits_mat[traits_mat == "n"] <- NA

            return(traits_mat)
        }
        return(traits_mat)
    }


####################################################################################################################
########################################## SEMI.AUTO.VALUE #########################################################
####################################################################################################################


#' Interactively retrieve quantitative data from a paper
#'
#' @param keywords A set of keywords as characters (i.e. traits of interest) in a vector
#' @param highlight A set of keywords as characters (i.e. words related to trait of interest) in a vector
#' @param in_dir directory with input text files
#' @param cleaned_text output of 'clean.text' - a list of cleaned text files
#' @param sorted_words output of 'generate.term.dataset' - a list of frequency of searched words mentioned in each paper
#'
#' @return A data frame containing matches above threshold and verified matches
#' @export
#'
#' @examples
#' download.file("https://github.com/ajhelmstetter/papieRmache/raw/master/inst/extdata/test_pdfs.zip", destfile = "./test_pdfs.zip")
#' unzip("./test_pdfs.zip")
#'
#' ct<-clean.text(in_dir = "./test_pdfs/",all_keywords=kw)
#'
#' keywords<-c('species')
#'
#' td<-generate.term.dataset(cleaned_text = ct, in_dir = "./testpdfs/",keywords = keywords)
#' semi.auto.value(in_dir = "./inst/extdata/test_pdfs/", keywords = keywords, highlight = c('increase','decrease'), cleaned_text = ct, sorted_words = td)
#'
semi.auto.value <-
    function(in_dir,
             keywords,
             highlight,
             cleaned_text,
             sorted_words) {
        ###
        # could provide T/F input that will include taxonomy
        ###

        options(stringsAsFactors = FALSE)

        data_files <-
            list.files(path = in_dir,
                       pattern = "*.txt$",
                       full.names = T)

        traits_mat <-
            matrix(nrow = length(cleaned_text),
                   ncol = length(keywords))
        colnames(traits_mat) <- keywords

        head(traits_mat)

        # check where traits are present, may be present and are not present

        for (h in 1:length(keywords)) {
            for (j in 1:length(cleaned_text)) {
                if (keywords[h] %in% names(sorted_words[[j]]) == FALSE) {
                    traits_mat[, keywords[h]][j] <- "n"
                } else {
                    traits_mat[, keywords[h]][j] <- "m"
                }
            }
        }

        # get which paragraphs may mention trait in document

        # make file for output
        cat("# trait used to check", sep = "\n", file = "trait.Rmd")

        # print to console width
        options(width = 80)

        for (g in 1:length(keywords)) {
            # trait name
            cat(
                paste("##", keywords[g]),
                sep = "\n",
                file = "trait.Rmd",
                append = T
            )

            # column with for trait
            traits_mat_y <- traits_mat[, keywords[g]]

            # which articles may reference trait
            mp <- which(traits_mat_y == "m")

            if (length(mp) > 0) {
                for (d in 1:length(mp)) {
                    # cat trait name
                    cat(
                        paste("###", data_files[mp[d]]),
                        sep = "\n \n",
                        file = "trait.Rmd",
                        append = T
                    )
                    ### merge adjacent paragraphs
                    comb_paras <-
                        paste(cleaned_text[mp[d]], collapse = " ")

                    # Split the string into individual words
                    splitString <- strsplit(comb_paras, " ")[[1]]

                    # Find the location of the word of interest
                    loc <-
                        grep(paste("\\<", keywords[g], "\\>", sep = ""),
                             splitString)

                    # Subset as you normally would

                    comb_occs <- list()

                    if (length(loc) > 0) {
                        for (j in 1:length(loc)) {
                            if ((loc[j] - 50) > 0) {
                                cat(
                                    paste(splitString[c((loc[j] - 50):(loc[j] + 50))], collapse = " "),
                                    sep = "\n",
                                    file = "trait.Rmd",
                                    append = T
                                )
                                cat(
                                    paste(""),
                                    sep = "\n",
                                    file = "trait.Rmd",
                                    append = T
                                )

                                comb_occs[[j]] <-
                                    paste(splitString[c((loc[j] - 50):(loc[j] + 50))], collapse = " ")
                            } else {
                                cat(
                                    paste(splitString[c((loc[j]):(loc[j] + 50))], collapse = " "),
                                    sep = "\n",
                                    file = "trait.Rmd",
                                    append = T
                                )
                                cat(
                                    paste(""),
                                    sep = "\n",
                                    file = "trait.Rmd",
                                    append = T
                                )

                                comb_occs[[j]] <-
                                    paste(splitString[c((loc[j]):(loc[j] + 50))], collapse = " ")
                            }
                        }
                    } else {
                        comb_occs[[1]] <- "error"
                    }

                    cat(paste(
                        paste(
                            "Identify",
                            red$bold$underline(keywords[g]),
                            "value in",
                            data_files[mp[d]],
                            sep = " "
                        ),
                        sep = "\n"
                    ))
                    cat("", sep = "\n")
                    cat("", sep = "\n")
                    for (k in 1:length(comb_occs)) {
                        unique_words <- lapply(strsplit(comb_occs[[k]], " "), function(x) {
                            x[!x == ""]
                        })

                        # creating a dataframe with crayonized text
                        df <-
                            tibble::enframe(unique_words) %>% tidyr::unnest(cols=c(value)) %>%
                            # here you can specify the color/word combinations you need
                            dplyr::mutate(
                                .data = .,
                                value2 = dplyr::case_when(
                                    grepl(keywords[g], value) ~ crayon::red$bold$underline(value),
                                    grepl(paste(highlight,collapse="|"), value) ~ crayon::blue$bold$underline(value),
                                    TRUE ~ value
                                )
                            ) %>% dplyr::select(., -value)

                        cat(df$value2)

                        # cat(paste(comb_occs[[k]]),sep='\n'))
                        cat("", sep = "\n")
                        cat("", sep = "\n")
                    }
                    answ <- readline(prompt = "value: ")
                    traits_mat[mp[d], keywords[g]] <- answ

                }
            }
        }

        rownames(traits_mat) <- data_files

        traits_mat <- data.frame(traits_mat)

        traits_mat[traits_mat == ""] <- NA
        traits_mat[traits_mat == "n"] <- NA

        return(traits_mat)

    }
