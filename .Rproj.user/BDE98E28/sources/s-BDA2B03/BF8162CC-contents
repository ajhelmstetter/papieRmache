options(stringsAsFactors = FALSE)

data_files <- list.files(pattern = ".txt$")

load("cleaned_text.Rdata")

keywords <- read.table("../keywords.txt")
keywords <- keywords$V1

# save.image('biglistterms.Rdata')
load("biglistterms.Rdata")

### Models used

mod_pres <- c("bisse", "quasse", "bisseness", "geosse", "classe", "musse", "hisse", "fisse", "geohisse", "chromosse", "secsse",
    "cid")
stats <- read.table("../stats.txt")
stats <- stats$V1
tax <- read.table("../taxonomy.txt")
tax <- tax$V1
traits <- read.table("../traits.txt")
traits <- traits$V1

mod_mat <- matrix(nrow = length(cleaned_text), ncol = length(mod_pres))
colnames(mod_mat) <- mod_pres

head(mod_mat)

# check where models are present, may be present and are not present add year if yes
for (h in 1:length(mod_pres)) {
    for (j in 1:length(cleaned_text)) {
        if (mod_pres[h] %in% names(sorted_words[[j]]) == FALSE) {
            mod_mat[, mod_pres[h]][j] <- "n"
        } else if (sorted_words[[j]][mod_pres[h]] > 4) {
            mod_mat[, mod_pres[h]][j] <- list_years[j]
        } else {
            mod_mat[, mod_pres[h]][j] <- "m"
        }
    }
}

## need section checking whether model is actually present in 'm'

# instead of year put y
for (h in 1:length(mod_pres)) {
    for (j in 1:length(cleaned_text)) {
        if (mod_pres[h] %in% names(sorted_words[[j]]) == FALSE) {
            mod_mat[, mod_pres[h]][j] <- "n"
        } else if (sorted_words[[j]][mod_pres[h]] > 4) {
            mod_mat[, mod_pres[h]][j] <- "y"
        } else {
            mod_mat[, mod_pres[h]][j] <- "m"
        }
    }
}

# get which paragraphs may mention model in document

# make file for output
cat("# SSE model use to check", sep = "\n", file = "model.Rmd")

# print to console width
options(width = 80)

for (g in 1:length(mod_pres)) {

    # model name
    cat(paste("##", mod_pres[g]), sep = "\n", file = "model.Rmd", append = T)

    # column with for model
    mod_mat_y <- mod_mat[, mod_pres[g]]

    # which articles may reference model
    mp <- which(mod_mat_y == "m")

    if (length(mp) > 0) {

        for (d in 1:length(mp)) {

            # cat model name
            cat(paste("###", data_files[mp[d]]), sep = "\n \n", file = "model.Rmd", append = T)
            ### merge adjacent paragraphs
            comb_paras <- paste(cleaned_text[mp[d]], collapse = " ")

            # Split the string into individual words
            splitString <- strsplit(comb_paras, " ")[[1]]

            # Find the location of the word of interest
            loc <- grep(mod_pres[g], splitString)

            # Subset as you normally would

            comb_occs <- list()
            for (j in 1:length(loc)) {
                cat(paste(splitString[c((loc[j] - 50):(loc[j] + 50))], collapse = " "), sep = "\n", file = "model.Rmd", append = T)
                cat(paste(""), sep = "\n", file = "model.Rmd", append = T)

                comb_occs[[j]] <- paste(splitString[c((loc[j] - 50):(loc[j] + 50))], collapse = " ")
            }

            cat(paste(paste("Is", mod_pres[g], "used in", data_files[mp[d]], "?", sep = " "), sep = "\n"))
            cat("", sep = "\n")
            cat("", sep = "\n")
            for (k in 1:length(comb_occs)) {
                cat(paste(comb_occs[[k]]), sep = "\n")
                cat("", sep = "\n")
                cat("", sep = "\n")
            }
            answ <- readline(prompt = "y or n?: ")
            mod_mat[mp[d], mod_pres[g]] <- answ

        }
    }
}


# render('model.Rmd', pdf_document())

# extract text of para, print to pdf?
for (j in 1:length(cleaned_text[[mp[1]]])) {
    if (mod_pres[h] %in% names(sorted_words[[j]]) == FALSE) {
        mod_mat[, mod_pres[h]][j] <- "n"
    } else if (sorted_words[[j]][mod_pres[h]] > 4) {
        mod_mat[, mod_pres[h]][j] <- "y"
    } else {
        mod_mat[, mod_pres[h]][j] <- "m"
    }
}



# fill in table substituting 'y' with year
for (n in 1:length(colnames(mod_mat))) {
    for (m in 1:length(mod_mat[, n])) {
        if (mod_mat[m, n] == "y") {
            mod_mat[m, n] <- list_years[m]
        }
    }
}


year_mat <- matrix(nrow = 0, ncol = 3)

for (h in 1:length(mod_pres)) {
    tab_mod <- table(mod_mat[, mod_pres[h]])[names(table(mod_mat[, mod_pres[h]])) != "n"]
    tab_mod <- tab_mod[names(tab_mod) != "m"]
    bi <- data.frame(tab_mod, row.names = names(tab_mod))
    foo <- cbind(rownames(bi), tab_mod, rep(mod_pres[h], length(bi[, 1])))
    year_mat <- rbind(year_mat, foo)

}

year_mat <- data.frame(year_mat)

colnames(year_mat) <- c("year", "freq", "model")

# stacked barplot
ggplot(year_mat, aes(fill = model, y = freq, x = year)) + geom_bar(position = "stack", stat = "identity") + xlab("Year of publication") +
    ylab("-SSE model use") + guides(fill = guide_legend(title = "Models used"))





## Which traits are used?

### Do this with roots of words?

# read in trait keyword list
traits <- read.table("../traits.txt")
traits <- traits$V1

# make empty table to fill
trait_mat <- matrix(nrow = length(cleaned_text), ncol = length(traits))
colnames(trait_mat) <- traits

# check where traits are assessed, may be assessed and are not assessed

for (h in 1:length(traits)) {
    for (j in 1:length(cleaned_text)) {
        if (traits[h] %in% names(sorted_words[[j]]) == FALSE) {
            trait_mat[, traits[h]][j] <- "n"
        } else if (sorted_words[[j]][traits[h]] > 2) {
            trait_mat[, traits[h]][j] <- "y"
        } else {
            trait_mat[, traits[h]][j] <- "m"
        }

    }

}


### Trait assessment frequency

# empty vector
trait_freq <- vector()

# get frequency of only those traits repeatedly mentioned
for (h in 1:length(traits)) {
    trait_freq[h] <- table(trait_mat[, traits[h]])["y"]
}

names(trait_freq) <- traits

# convert NAs to 0s for plotting
trait_freq[is.na(trait_freq)] <- 0

trait_freq_df <- data.frame(names(trait_freq), trait_freq)
colnames(trait_freq_df) <- c("trait", "freq")


# could colour based on type of trait?
ggplot(trait_freq_df, aes(y = freq, x = reorder(trait, freq))) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90)) +
    coord_flip()


## Trait effect

# Paragraphs that contain trait with either increase or decrease and rate and paragraphs before and after? how to write
# paras to pdf for easy reading? what if both increase and decrease are present?


trait_effects <- vector(length = length(cleaned_text))
trait_effects <- data.frame(trait_effects)

# find paragraphs that mention SSE models
for (o in 1:length(traits)) {
    res <- vector()
    for (p in 1:length(cleaned_text)) {

        # here, it may also get words that contain the word
        foo <- grep(paste(traits[o]), cleaned_text[[p]], ignore.case = TRUE)

        if (length(foo) > 0) {

            # i think this looks in the whole text, could pinpoint specific paragraphs/close paragraphs

            bar <- grep("increase", cleaned_text[[p]][foo], ignore.case = TRUE)
            res[p] <- "increase"

            if (length(bar) < 1) {
                bar <- grep("decrease", cleaned_text[[p]][foo], ignore.case = TRUE)
                res[p] <- "decrease"
            }
        } else {
            res[p] <- "NA"
        }

    }

    trait_effects[, o] <- res
}

colnames(trait_effects) <- traits
head(trait_effects)

# frequency of increase/decrease mentioned in papers that look at trait?
trait_effects_freq <- table(trait_effects[, 1])

# why is this 2:?
for (i in 2:length(traits)) {
    if (length(table(trait_effects[, i])) == 3) {
        trait_effects_freq <- rbind(trait_effects_freq, table(trait_effects[, i]))
        rownames(trait_effects_freq)[i] <- traits[i]
    } else {
        trait_effects_freq <- rbind(trait_effects_freq, c(0, 0, 0))
    }
}

# reformat for barplot
rownames(trait_effects_freq)[1] <- traits[1]
trait_effects_freq <- trait_effects_freq[, 1:2]
trait_effects_freq <- data.frame(trait_effects_freq)
trait_effects_freq$trait <- row.names(trait_effects_freq)
trait_effects_freq <- trait_effects_freq[trait_effects_freq$decrease != 0, ]
trait_effects_freq <- melt(trait_effects_freq, "trait")

# grouped barplot
ggplot(trait_effects_freq, aes(fill = variable, y = value, x = reorder(trait, value))) + geom_bar(position = "dodge", stat = "identity") +
    theme(axis.text.x = element_text(angle = 90)) + coord_flip()
