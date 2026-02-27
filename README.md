<img src="inst/app/www/my_sticker.png" style="float:right; width:220px;" />

## How to run the app (local)

Copy/paste the following lines into the R console in RStudio :

```r
install.packages("remotes")
remotes::install_github("Antoine-T17/daphfeedR", dependencies = TRUE, upgrade = "never")

daphfeedR::run_app()
```

