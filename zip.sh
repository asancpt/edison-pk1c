cp index.R $1.R
zip releases/pk1c-$1.zip $1.R documentation.Rmd R/pk1coma-simple.R
rm $1.R
