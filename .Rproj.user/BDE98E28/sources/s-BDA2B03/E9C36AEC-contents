## Install Missing Packages (listed in DESCRIPTION) ----
devtools::install_deps()
## Attach Required Packages (listed in `Depends` section in DESCRIPTION) ----
devtools::load_all()


load("cleaned_text.Rdata")
load("biglistterms.Rdata")

### Keyword examples (SSE review)

# Age of the Tree
keywords <- c("age", "tmrca", "mrca", "root", "mya", "ma", "million", "years")

# number of tips
keywords <- c("tips", "samples", "species", "phylogenetic", "phylogeny")

# sampling fraction
keywords <- c("sampling")

# transition rate
keywords <- c("transition")

# root assumption
keywords <- c("assumption", "root")

# other programs
keywords <- c("bamm", "medusa", "rpanda", "tess")

# traits and states
keywords <- read.table("../traits.txt")
keywords <- keywords$V1

# taxonomy
keywords <- read.table("../taxonomy.txt")
keywords <- keywords$V1

# SSE models
keywords <- read.table("../models.txt")
keywords <- keywords$V1

### Function for qualitative data (e.g. model names)
tax_tab <- semi.auto(keywords, 4, "sse_model")

### Function for quantitative data (e.g. number of species)
tax_tab <- semi.auto.value(keywords, "sse_model")


### Combine each element

dflist <- list.files("./", pattern = "*.RDS")

# make into loop
df <- cbind(readRDS(dflist[1]), readRDS(dflist[2]), readRDS(dflist[3]), readRDS(dflist[4]), readRDS(dflist[5]))

write.csv(df, "out.csv")

### ISSUES

# reference list repeated mentions in close proximity of common words like 'species' - until next use of word? remove lines
# with orcid ID
