# papieRmache

An R package for chewing up papers, spitting out the information you don't want, keeping the information you do.

To install:

```r
install.packages("devtools") # if you have not installed "devtools" package
devtools::install_github("ajhelmstetter/papieRmache")
help(package=papieRmache)
```

Before running papieRmache, PDFs should be converted to text files using [pdftotext](https://en.wikipedia.org/wiki/Pdftotext).

There's less chance for annoying errors if special characters are removed from PDF names prior to conversion/papieRmache:

```bash
#example to replace various characters with underscores
rename 's/[\.,-]/_/g' *
rename 's/[\"]//g' *
rename "s/\'//g" *
rename 's/ /_/g' *
rename 's/_+/_/g' *
rename 's/\(//g' *
rename 's/\)//g' *

#fix file suffix
rename 's/_pdf/\.pdf/g' *
```

PDFs can be converted rapidly, in batch, as follows :

```bash
#run in folder containing PDFs of interest
ls -1 ./ | \
while read sample; do
  	pdftotext $sample
done
```

![Example papieRmache output](https://ajhelmstetter.github.io/images/papieRmache.png)
