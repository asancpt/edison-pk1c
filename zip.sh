cp index.R $1.R
zip -FSr releases/pk1c-$1.zip $1.R documentation.Rmd R/function.R
rm $1.R
