# PRIOR STEP: REMOVE '()' and other strange characters from file names
files <- list.files(pattern = ".pdf$")

# only need to run first time convert them to text (deals with column issues)

# for (i in 1:length(files)){ system(paste('pdftotext ',files[i])) }
