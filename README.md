<img src="inst/app/www/my_sticker.png" align="right" width="100" />

## How to run the app (local)

Copy/paste the following lines into the R console in RStudio :

```r
install.packages("remotes")
remotes::install_github("Antoine-T17/daphfeedR", dependencies = TRUE, upgrade = "never")

daphfeedR::run_app()
```

