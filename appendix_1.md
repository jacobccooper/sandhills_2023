Appendix 1: An update to the Birds of the Nebraska National Forest
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)
library(terra)
```

    ## terra 1.7.39
    ## 
    ## Attaching package: 'terra'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following object is masked from 'package:terra':
    ## 
    ##     shift
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
gbif <- read.delim(paste0(filepath,"0104629-230530130749713.csv"),sep = "\t")

# remove unclassifiable
gbif$species[which(gbif$species=="Junco hyemalis"&
        gbif$infraspecificEpithet=="")] <- "Junco sp."

gbif$species[which(gbif$species=="Setophaga coronata"&
        gbif$infraspecificEpithet=="")] <- "Setophaga coronata/auduboni"

# split some taxa for better resolution
ssp_rename <- function(genus,from,to){
  index <- which(gbif$infraspecificEpithet==from&
                   gbif$species%like%genus)
  if(length(index)<1){
    print(paste0("No matches for ",from))
  }else{
    gbif$species[index] <- to
    print(paste0(from," changed to ",to))
  }
}

sp_list <-  c("Junco","hyemalis","Junco hyemalis",
             "Junco","oreganus","Junco oreganus",
             "Junco","aikeni","Junco aikeni",
             "Junco","mearnsi","Junco mearnsi",
             "Junco","cismontanus","Junco cismontanus",
             "Junco","caniceps","Junco caniceps",
             "Junco","shufeldti",
                    "Junco oreganus",
             "Junco","montanus",
                    "Junco oreganus",
             "Setophaga","coronata","Setophaga coronata",
             "Setophaga","","Setophaga coronata/auduboni")

renamer <- matrix(data=sp_list,
       nrow=(length(sp_list)/3),ncol=3,
       byrow = T) %>% as.data.frame()
colnames(renamer) <- c("genus","from","to")

for(i in 1:nrow(renamer)){
  ssp_rename(genus = renamer$genus[i],
             from = renamer$from[i],
             to = renamer$to[i])
}

# reduce to relevant columns
# all have coords??

gbif <- gbif%>%
  select(species, locality, individualCount,
         decimalLongitude, decimalLatitude, eventDate) %>%
  unique()

taxo_order <- read_csv(paste0(filepath,"sp_list_ne.csv"))
taxo_order <- taxo_order%>%
  rename(species = scientific)

gbif <- gbif%>%
  left_join(taxo_order,by="species")

missing <- gbif$species[which(is.na(gbif$order))]%>%unique()%>%
  as.data.frame
# write as file, then put new names
# write_csv(missing,paste0(filepath,"missing.csv"))


# rename missing taxa... there's several of them!

rename_taxon <- function(gbif,wrong,right,keep){
  index <- which(gbif$species==wrong)
  if(length(index)<1){
    return(gbif)
  }else{
    if(keep==F){
      gbif <- gbif[-index,]
    }else{
      gbif$species[index] <- right
    }
    return(gbif)
  }
}

missing <- read_csv(paste0(filepath,"missing.csv"))

for(i in 1:nrow(missing)){
  right <- missing$right[i]
  wrong <- missing$wrong[i]
  keep <- missing$keep[i]
  gbif <- rename_taxon(gbif=gbif,right=right,wrong=wrong,keep=keep)
}

# reapply order
gbif <- gbif%>%
  select(-order,-english)%>%
  full_join(taxo_order,by="species")

gbif <- gbif[order(gbif$order),]

gbif <- gbif[-which(gbif$species==""),]

write_csv(gbif,paste0(filepath,"reduced_gbif.csv"))
```

We are also going to import and reformat eBird data. Note that for
*Junco*, we are only using populations predesignated to subspecies since
roughly 57% of western *Junco* are *hyemalis* in Nebraska; thus, we
can’t make broad assumptions. To contrast, 98% in eastern Nebraska are
*hyemalis*.

``` r
# reformat eBird data
# add to ensure nothing is missing in GBIF!
# first run revealed some data lacking

ebird <- read.delim(paste0(filepath,
                           "ebd_US-NE_relJun-2023/ebd_US-NE_relJun-2023.txt"),
                    sep = "\t",quote="")

# remove unknowns
`%notlike%` <- Negate(`%like%`)
ebird <- ebird[which(ebird$SCIENTIFIC.NAME%notlike%"sp."),]

# split some taxa for better resolution
ssp_rename <- function(from,to){
  index <- which(ebird$SUBSPECIES.SCIENTIFIC.NAME==from)
  if(length(index)<1){
    print(paste0("No matches for ",from))
  }else{
    ebird$SCIENTIFIC.NAME[index] <- to
    print(paste0(from," changed to ",to))
  }
}

# remove unclassifiable
ebird$SCIENTIFIC.NAME[which(ebird$SCIENTIFIC.NAME=="Junco hyemalis"&
        ebird$SUBSPECIES.SCIENTIFIC.NAME=="")] <- "Junco sp."

index <- which(ebird$SCIENTIFIC.NAME=="Setophaga coronata"&
        ebird$SUBSPECIES.SCIENTIFIC.NAME=="")

ebird$SCIENTIFIC.NAME[index] <- "Setophaga coronata/auduboni"

sp_list <-  c("Junco hyemalis hyemalis/carolinensis","Junco hyemalis",
             "Junco hyemalis [oreganus Group]","Junco oreganus",
             "Junco hyemalis aikeni","Junco aikeni",
             "Junco hyemalis mearnsi","Junco mearnsi",
             "Junco hyemalis cismontanus","Junco cismontanus",
             "Junco hyemalis caniceps","Junco caniceps",
             "Junco hyemalis hyemalis/carolinensis/cismontanus",
                    "Junco hyemalis/cismontanus",
             "Junco hyemalis oreganus x mearnsi",
                    "Junco hyemalis oreganus x mearnsi",
             "Junco hyemalis mearnsi x aikeni",
                    "Junco hyemalis mearnsi x aikeni",
             "Setophaga coronata coronata","Setophaga coronata",
             "Setophaga coronata auduboni","Setophaga auduboni",
             "Setophaga coronata coronata x auduboni",
                    "Setophaga coronata coronata x auduboni")

renamer <- matrix(data=sp_list,
       nrow=(length(sp_list)/2),ncol=2,
       byrow = T) %>% as.data.frame()
colnames(renamer) <- c("from","to")

for(i in 1:nrow(renamer)){
  ssp_rename(from = renamer$from[i],
             to = renamer$to[i])
}

# reduce to relevant columns
# all have coords??

ebird <- ebird[order(ebird$TAXONOMIC.ORDER),]

ebird <- ebird%>%
  select(SCIENTIFIC.NAME, OBSERVATION.COUNT,
         LONGITUDE, LATITUDE, OBSERVATION.DATE) %>%
  unique()%>%
  rename("species" = SCIENTIFIC.NAME,
         "individualCount" = OBSERVATION.COUNT, "decimalLongitude" = LONGITUDE,
         "decimalLatitude" = LATITUDE, "eventDate" = OBSERVATION.DATE)%>%
  unique()

write_csv(ebird,paste0(filepath,"reduced_ebird.csv"))
```

``` r
# note some longitudes are wrong; remove anything over 0
# locality columns causing problems for ebird

gbif <- read_csv(paste0(filepath,"reduced_gbif.csv"))%>%
  filter(decimalLongitude < 0) %>%
  select(-order,-english,-locality)

ebird <- read_csv(paste0(filepath,"reduced_ebird.csv"),quote="")

gbif <- rbind(ebird,gbif) %>%
  unique()

rm(ebird)
# load shapefile

nf <- vect(paste0(filepath,"S_USA.AdministrativeForest/S_USA.AdministrativeForest.shp"))

ne_nf <- nf$FORESTNAME[which(nf$FORESTNAME%like%"Nebraska")]

# select NE National Forest
ne_nf <- nf[which(nf$FORESTNAME==ne_nf)]

# get points in Nebraska NF
ne_crs <- crs(ne_nf)

gbif_vect <- vect(gbif,geom=c("decimalLongitude","decimalLatitude"),crs = ne_crs)

ne_gbif <- extract(y = gbif_vect,x = ne_nf)

# get non-empty rows
index <- which(!is.na(ne_gbif$FORESTNAME))

ne_nf_gbif <- gbif[index,]

write_csv(ne_nf_gbif,paste0(filepath,"ne_natl_forest_birds.csv"))
```

``` r
ne_nf_gbif <- read_csv(paste0(filepath,"ne_natl_forest_birds.csv"))

bessey <- which(ne_nf_gbif$decimalLatitude<42.5&ne_nf_gbif$decimalLongitude>-101)
mackelvie <- which(ne_nf_gbif$decimalLatitude>42.3&ne_nf_gbif$decimalLongitude>-102)
pine_ridge_oglala <- which(ne_nf_gbif$decimalLatitude>42.3&ne_nf_gbif$decimalLongitude<c(-102))

ne_nf_gbif$district <- "Unknown"

ne_nf_gbif$district[bessey] <- "Bessey"
ne_nf_gbif$district[mackelvie] <- "MacKelvie"
ne_nf_gbif$district[pine_ridge_oglala] <- "Pine Ridge / Oglala"

write_csv(ne_nf_gbif,paste0(filepath,"ne_natl_forest_birds.csv"))
```

# District Summaries

``` r
ne_nf_gbif <- read_csv(paste0(filepath,"ne_natl_forest_birds.csv"))
```

    ## Rows: 64305 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): species, individualCount, district
    ## dbl  (2): decimalLongitude, decimalLatitude
    ## date (1): eventDate
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ne_nf_gbif$species <- as.factor(ne_nf_gbif$species)
ne_nf_gbif$eventDate <- as.Date(ne_nf_gbif$eventDate)
ne_nf_gbif$district <- as.factor(ne_nf_gbif$district)
```

## Bessey

``` r
districtR <- function(ne_nf_gbif,district){
  sub.ne <- ne_nf_gbif[ne_nf_gbif$district==district,]
  sub.ne$eventDate <- year(sub.ne$eventDate)
  sp_list <- unique(sub.ne$species)
  hist(sub.ne$eventDate,main=district,breaks=100,
       xlab="Year")
  for(i in 1:length(sp_list)){
    sp_x <- sp_list[i]
    sp_ne <- sub.ne[which(sub.ne$species==sp_x),]%>%
      select(species,eventDate)
    if(nrow(sp_ne)<1){
      next
    }
    print(summary(sp_ne))
    hist(sp_ne$eventDate,main=sp_x,breaks=100,
       xlab="Year")
  }
}
```

``` r
districtR(ne_nf_gbif = ne_nf_gbif,district = "Bessey")
```

![](appendix_1_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ##                species    eventDate   
    ##  Anser albifrons   :3   Min.   :2004  
    ##  Acanthis flammea  :0   1st Qu.:2004  
    ##  Accipiter cooperii:0   Median :2004  
    ##  Accipiter gentilis:0   Mean   :2010  
    ##  Accipiter striatus:0   3rd Qu.:2014  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

    ##                species    eventDate   
    ##  Branta hutchinsii :4   Min.   :2016  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

    ##                species      eventDate   
    ##  Branta canadensis :126   Min.   :2002  
    ##  Acanthis flammea  :  0   1st Qu.:2012  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->

    ##                species    eventDate   
    ##  Cygnus buccinator :3   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-5.png)<!-- -->

    ##                           species    eventDate   
    ##  Cygnus buccinator/columbianus:1   Min.   :2022  
    ##  Acanthis flammea             :0   1st Qu.:2022  
    ##  Accipiter cooperii           :0   Median :2022  
    ##  Accipiter gentilis           :0   Mean   :2022  
    ##  Accipiter striatus           :0   3rd Qu.:2022  
    ##  Actitis macularius           :0   Max.   :2022  
    ##  (Other)                      :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-6.png)<!-- -->

    ##                species     eventDate   
    ##  Spatula discors   :59   Min.   :1986  
    ##  Acanthis flammea  : 0   1st Qu.:2010  
    ##  Accipiter cooperii: 0   Median :2012  
    ##  Accipiter gentilis: 0   Mean   :2012  
    ##  Accipiter striatus: 0   3rd Qu.:2018  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-7.png)<!-- -->

    ##                        species    eventDate   
    ##  Spatula discors/cyanoptera:2   Min.   :2018  
    ##  Acanthis flammea          :0   1st Qu.:2019  
    ##  Accipiter cooperii        :0   Median :2020  
    ##  Accipiter gentilis        :0   Mean   :2020  
    ##  Accipiter striatus        :0   3rd Qu.:2021  
    ##  Actitis macularius        :0   Max.   :2022  
    ##  (Other)                   :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-8.png)<!-- -->

    ##                species     eventDate   
    ##  Spatula clypeata  :19   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2012  
    ##  Accipiter cooperii: 0   Median :2014  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-9.png)<!-- -->

    ##                species     eventDate   
    ##  Mareca strepera   :12   Min.   :2000  
    ##  Acanthis flammea  : 0   1st Qu.:2010  
    ##  Accipiter cooperii: 0   Median :2019  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-10.png)<!-- -->

    ##                species     eventDate   
    ##  Mareca americana  :11   Min.   :2012  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2016  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-11.png)<!-- -->

    ##                species     eventDate   
    ##  Anas platyrhynchos:72   Min.   :1985  
    ##  Acanthis flammea  : 0   1st Qu.:2010  
    ##  Accipiter cooperii: 0   Median :2016  
    ##  Accipiter gentilis: 0   Mean   :2012  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-12.png)<!-- -->

    ##                species    eventDate   
    ##  Anas acuta        :4   Min.   :2000  
    ##  Acanthis flammea  :0   1st Qu.:2000  
    ##  Accipiter cooperii:0   Median :2010  
    ##  Accipiter gentilis:0   Mean   :2010  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-13.png)<!-- -->

    ##                species     eventDate   
    ##  Anas crecca       :15   Min.   :2000  
    ##  Acanthis flammea  : 0   1st Qu.:2009  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2014  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-14.png)<!-- -->

    ##                species    eventDate   
    ##  Aythya valisineria:2   Min.   :2022  
    ##  Acanthis flammea  :0   1st Qu.:2022  
    ##  Accipiter cooperii:0   Median :2022  
    ##  Accipiter gentilis:0   Mean   :2022  
    ##  Accipiter striatus:0   3rd Qu.:2023  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-15.png)<!-- -->

    ##                species    eventDate   
    ##  Aythya americana  :9   Min.   :2014  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2019  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-16.png)<!-- -->

    ##                species     eventDate   
    ##  Aythya collaris   :17   Min.   :2000  
    ##  Acanthis flammea  : 0   1st Qu.:2012  
    ##  Accipiter cooperii: 0   Median :2016  
    ##  Accipiter gentilis: 0   Mean   :2015  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-17.png)<!-- -->

    ##                species    eventDate   
    ##  Aythya marila     :2   Min.   :2000  
    ##  Acanthis flammea  :0   1st Qu.:2000  
    ##  Accipiter cooperii:0   Median :2000  
    ##  Accipiter gentilis:0   Mean   :2000  
    ##  Accipiter striatus:0   3rd Qu.:2000  
    ##  Actitis macularius:0   Max.   :2000  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-18.png)<!-- -->

    ##                species     eventDate   
    ##  Aythya affinis    :12   Min.   :2005  
    ##  Acanthis flammea  : 0   1st Qu.:2010  
    ##  Accipiter cooperii: 0   Median :2016  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2023  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-19.png)<!-- -->

    ##                species    eventDate   
    ##  Bucephala albeola :4   Min.   :2016  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2019  
    ##  Accipiter gentilis:0   Mean   :2019  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-20.png)<!-- -->

    ##                species    eventDate   
    ##  Bucephala clangula:4   Min.   :2001  
    ##  Acanthis flammea  :0   1st Qu.:2001  
    ##  Accipiter cooperii:0   Median :2012  
    ##  Accipiter gentilis:0   Mean   :2012  
    ##  Accipiter striatus:0   3rd Qu.:2023  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-21.png)<!-- -->

    ##                species    eventDate   
    ##  Mergus merganser  :6   Min.   :2009  
    ##  Acanthis flammea  :0   1st Qu.:2010  
    ##  Accipiter cooperii:0   Median :2014  
    ##  Accipiter gentilis:0   Mean   :2013  
    ##  Accipiter striatus:0   3rd Qu.:2016  
    ##  Actitis macularius:0   Max.   :2016  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-22.png)<!-- -->

    ##                species    eventDate   
    ##  Oxyura jamaicensis:7   Min.   :2010  
    ##  Acanthis flammea  :0   1st Qu.:2011  
    ##  Accipiter cooperii:0   Median :2012  
    ##  Accipiter gentilis:0   Mean   :2014  
    ##  Accipiter striatus:0   3rd Qu.:2016  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-23.png)<!-- -->

    ##                 species     eventDate   
    ##  Colinus virginianus:41   Min.   :2002  
    ##  Acanthis flammea   : 0   1st Qu.:2017  
    ##  Accipiter cooperii : 0   Median :2019  
    ##  Accipiter gentilis : 0   Mean   :2017  
    ##  Accipiter striatus : 0   3rd Qu.:2020  
    ##  Actitis macularius : 0   Max.   :2022  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-24.png)<!-- -->

    ##                 species     eventDate   
    ##  Meleagris gallopavo:94   Min.   :2000  
    ##  Acanthis flammea   : 0   1st Qu.:2012  
    ##  Accipiter cooperii : 0   Median :2016  
    ##  Accipiter gentilis : 0   Mean   :2016  
    ##  Accipiter striatus : 0   3rd Qu.:2020  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-25.png)<!-- -->

    ##                      species     eventDate   
    ##  Tympanuchus phasianellus:85   Min.   :1957  
    ##  Acanthis flammea        : 0   1st Qu.:2003  
    ##  Accipiter cooperii      : 0   Median :2015  
    ##  Accipiter gentilis      : 0   Mean   :2009  
    ##  Accipiter striatus      : 0   3rd Qu.:2020  
    ##  Actitis macularius      : 0   Max.   :2023  
    ##  (Other)                 : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-26.png)<!-- -->

    ##                species     eventDate   
    ##  Tympanuchus cupido:38   Min.   :1986  
    ##  Acanthis flammea  : 0   1st Qu.:1999  
    ##  Accipiter cooperii: 0   Median :2007  
    ##  Accipiter gentilis: 0   Mean   :2007  
    ##  Accipiter striatus: 0   3rd Qu.:2017  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-27.png)<!-- -->

    ##                 species     eventDate   
    ##  Phasianus colchicus:30   Min.   :1986  
    ##  Acanthis flammea   : 0   1st Qu.:2004  
    ##  Accipiter cooperii : 0   Median :2009  
    ##  Accipiter gentilis : 0   Mean   :2009  
    ##  Accipiter striatus : 0   3rd Qu.:2017  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-28.png)<!-- -->

    ##                 species     eventDate   
    ##  Podilymbus podiceps:15   Min.   :1987  
    ##  Acanthis flammea   : 0   1st Qu.:2012  
    ##  Accipiter cooperii : 0   Median :2015  
    ##  Accipiter gentilis : 0   Mean   :2012  
    ##  Accipiter striatus : 0   3rd Qu.:2017  
    ##  Actitis macularius : 0   Max.   :2021  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-29.png)<!-- -->

    ##                  species    eventDate   
    ##  Podiceps nigricollis:3   Min.   :2007  
    ##  Acanthis flammea    :0   1st Qu.:2007  
    ##  Accipiter cooperii  :0   Median :2007  
    ##  Accipiter gentilis  :0   Mean   :2012  
    ##  Accipiter striatus  :0   3rd Qu.:2015  
    ##  Actitis macularius  :0   Max.   :2023  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-30.png)<!-- -->

    ##                species     eventDate   
    ##  Columba livia     :14   Min.   :1994  
    ##  Acanthis flammea  : 0   1st Qu.:2002  
    ##  Accipiter cooperii: 0   Median :2007  
    ##  Accipiter gentilis: 0   Mean   :2008  
    ##  Accipiter striatus: 0   3rd Qu.:2014  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-31.png)<!-- -->

    ##                   species      eventDate   
    ##  Streptopelia decaocto:101   Min.   :2007  
    ##  Acanthis flammea     :  0   1st Qu.:2014  
    ##  Accipiter cooperii   :  0   Median :2016  
    ##  Accipiter gentilis   :  0   Mean   :2017  
    ##  Accipiter striatus   :  0   3rd Qu.:2020  
    ##  Actitis macularius   :  0   Max.   :2023  
    ##  (Other)              :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-32.png)<!-- -->

    ##                species      eventDate   
    ##  Zenaida macroura  :307   Min.   :1985  
    ##  Acanthis flammea  :  0   1st Qu.:2012  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2014  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-33.png)<!-- -->

    ##                 species     eventDate   
    ##  Coccyzus americanus:36   Min.   :2007  
    ##  Acanthis flammea   : 0   1st Qu.:2015  
    ##  Accipiter cooperii : 0   Median :2018  
    ##  Accipiter gentilis : 0   Mean   :2017  
    ##  Accipiter striatus : 0   3rd Qu.:2019  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-34.png)<!-- -->

    ##                      species    eventDate   
    ##  Coccyzus erythropthalmus:6   Min.   :1957  
    ##  Acanthis flammea        :0   1st Qu.:2014  
    ##  Accipiter cooperii      :0   Median :2014  
    ##  Accipiter gentilis      :0   Mean   :2006  
    ##  Accipiter striatus      :0   3rd Qu.:2014  
    ##  Actitis macularius      :0   Max.   :2022  
    ##  (Other)                 :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-35.png)<!-- -->

    ##                species     eventDate   
    ##  Chordeiles minor  :39   Min.   :1986  
    ##  Acanthis flammea  : 0   1st Qu.:2011  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2012  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-36.png)<!-- -->

    ##                      species     eventDate   
    ##  Phalaenoptilus nuttallii:25   Min.   :1957  
    ##  Acanthis flammea        : 0   1st Qu.:1995  
    ##  Accipiter cooperii      : 0   Median :2009  
    ##  Accipiter gentilis      : 0   Mean   :2006  
    ##  Accipiter striatus      : 0   3rd Qu.:2018  
    ##  Actitis macularius      : 0   Max.   :2020  
    ##  (Other)                 : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-37.png)<!-- -->

    ##                   species    eventDate   
    ##  Antrostomus vociferus:6   Min.   :2009  
    ##  Acanthis flammea     :0   1st Qu.:2010  
    ##  Accipiter cooperii   :0   Median :2015  
    ##  Accipiter gentilis   :0   Mean   :2013  
    ##  Accipiter striatus   :0   3rd Qu.:2016  
    ##  Actitis macularius   :0   Max.   :2016  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-38.png)<!-- -->

    ##                species     eventDate   
    ##  Chaetura pelagica :44   Min.   :1986  
    ##  Acanthis flammea  : 0   1st Qu.:2011  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2013  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-39.png)<!-- -->

    ##                  species    eventDate   
    ##  Archilochus colubris:2   Min.   :1990  
    ##  Acanthis flammea    :0   1st Qu.:1990  
    ##  Accipiter cooperii  :0   Median :1990  
    ##  Accipiter gentilis  :0   Mean   :1990  
    ##  Accipiter striatus  :0   3rd Qu.:1990  
    ##  Actitis macularius  :0   Max.   :1990  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-40.png)<!-- -->

    ##                species    eventDate   
    ##  Porzana carolina  :2   Min.   :2009  
    ##  Acanthis flammea  :0   1st Qu.:2009  
    ##  Accipiter cooperii:0   Median :2009  
    ##  Accipiter gentilis:0   Mean   :2009  
    ##  Accipiter striatus:0   3rd Qu.:2009  
    ##  Actitis macularius:0   Max.   :2009  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-41.png)<!-- -->

    ##                species     eventDate   
    ##  Fulica americana  :11   Min.   :2000  
    ##  Acanthis flammea  : 0   1st Qu.:2012  
    ##  Accipiter cooperii: 0   Median :2013  
    ##  Accipiter gentilis: 0   Mean   :2014  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-42.png)<!-- -->

    ##                 species     eventDate   
    ##  Antigone canadensis:14   Min.   :2000  
    ##  Acanthis flammea   : 0   1st Qu.:2011  
    ##  Accipiter cooperii : 0   Median :2017  
    ##  Accipiter gentilis : 0   Mean   :2014  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2021  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-43.png)<!-- -->

    ##                     species    eventDate   
    ##  Recurvirostra americana:2   Min.   :1986  
    ##  Acanthis flammea       :0   1st Qu.:1986  
    ##  Accipiter cooperii     :0   Median :1986  
    ##  Accipiter gentilis     :0   Mean   :1986  
    ##  Accipiter striatus     :0   3rd Qu.:1986  
    ##  Actitis macularius     :0   Max.   :1986  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-44.png)<!-- -->

    ##                species    eventDate   
    ##  Pluvialis dominica:2   Min.   :2018  
    ##  Acanthis flammea  :0   1st Qu.:2018  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2018  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-45.png)<!-- -->

    ##                  species     eventDate   
    ##  Charadrius vociferus:92   Min.   :1978  
    ##  Acanthis flammea    : 0   1st Qu.:2007  
    ##  Accipiter cooperii  : 0   Median :2015  
    ##  Accipiter gentilis  : 0   Mean   :2012  
    ##  Accipiter striatus  : 0   3rd Qu.:2019  
    ##  Actitis macularius  : 0   Max.   :2023  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-46.png)<!-- -->

    ##                  species     eventDate   
    ##  Bartramia longicauda:18   Min.   :1986  
    ##  Acanthis flammea    : 0   1st Qu.:2010  
    ##  Accipiter cooperii  : 0   Median :2017  
    ##  Accipiter gentilis  : 0   Mean   :2012  
    ##  Accipiter striatus  : 0   3rd Qu.:2017  
    ##  Actitis macularius  : 0   Max.   :2023  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-47.png)<!-- -->

    ##                 species     eventDate   
    ##  Numenius americanus:20   Min.   :2005  
    ##  Acanthis flammea   : 0   1st Qu.:2007  
    ##  Accipiter cooperii : 0   Median :2013  
    ##  Accipiter gentilis : 0   Mean   :2013  
    ##  Accipiter striatus : 0   3rd Qu.:2018  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-48.png)<!-- -->

    ##                species    eventDate   
    ##  Calidris minutilla:2   Min.   :2009  
    ##  Acanthis flammea  :0   1st Qu.:2009  
    ##  Accipiter cooperii:0   Median :2009  
    ##  Accipiter gentilis:0   Mean   :2009  
    ##  Accipiter striatus:0   3rd Qu.:2009  
    ##  Actitis macularius:0   Max.   :2009  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-49.png)<!-- -->

    ##                species    eventDate   
    ##  Gallinago delicata:6   Min.   :1989  
    ##  Acanthis flammea  :0   1st Qu.:1993  
    ##  Accipiter cooperii:0   Median :2005  
    ##  Accipiter gentilis:0   Mean   :2003  
    ##  Accipiter striatus:0   3rd Qu.:2012  
    ##  Actitis macularius:0   Max.   :2015  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-50.png)<!-- -->

    ##                  species     eventDate   
    ##  Actitis macularius  :21   Min.   :1986  
    ##  Acanthis flammea    : 0   1st Qu.:2007  
    ##  Accipiter cooperii  : 0   Median :2014  
    ##  Accipiter gentilis  : 0   Mean   :2011  
    ##  Accipiter striatus  : 0   3rd Qu.:2017  
    ##  Aechmophorus clarkii: 0   Max.   :2022  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-51.png)<!-- -->

    ##                species    eventDate   
    ##  Tringa melanoleuca:5   Min.   :1986  
    ##  Acanthis flammea  :0   1st Qu.:1986  
    ##  Accipiter cooperii:0   Median :2000  
    ##  Accipiter gentilis:0   Mean   :1999  
    ##  Accipiter striatus:0   3rd Qu.:2000  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-52.png)<!-- -->

    ##                species    eventDate   
    ##  Larus delawarensis:4   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2011  
    ##  Accipiter cooperii:0   Median :2016  
    ##  Accipiter gentilis:0   Mean   :2016  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-53.png)<!-- -->

    ##                species    eventDate   
    ##  Chlidonias niger  :2   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-54.png)<!-- -->

    ##                species    eventDate   
    ##  Sterna forsteri   :2   Min.   :2018  
    ##  Acanthis flammea  :0   1st Qu.:2018  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2018  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-55.png)<!-- -->

    ##                 species     eventDate   
    ##  Nannopterum auritum:23   Min.   :1986  
    ##  Acanthis flammea   : 0   1st Qu.:1991  
    ##  Accipiter cooperii : 0   Median :2007  
    ##  Accipiter gentilis : 0   Mean   :2005  
    ##  Accipiter striatus : 0   3rd Qu.:2018  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-56.png)<!-- -->

    ##                       species    eventDate   
    ##  Pelecanus erythrorhynchos:4   Min.   :1995  
    ##  Acanthis flammea         :0   1st Qu.:1995  
    ##  Accipiter cooperii       :0   Median :2008  
    ##  Accipiter gentilis       :0   Mean   :2008  
    ##  Accipiter striatus       :0   3rd Qu.:2021  
    ##  Actitis macularius       :0   Max.   :2021  
    ##  (Other)                  :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-57.png)<!-- -->

    ##                   species    eventDate   
    ##  Botaurus lentiginosus:1   Min.   :2023  
    ##  Acanthis flammea     :0   1st Qu.:2023  
    ##  Accipiter cooperii   :0   Median :2023  
    ##  Accipiter gentilis   :0   Mean   :2023  
    ##  Accipiter striatus   :0   3rd Qu.:2023  
    ##  Actitis macularius   :0   Max.   :2023  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-58.png)<!-- -->

    ##                species     eventDate   
    ##  Ardea herodias    :33   Min.   :1905  
    ##  Acanthis flammea  : 0   1st Qu.:2000  
    ##  Accipiter cooperii: 0   Median :2009  
    ##  Accipiter gentilis: 0   Mean   :2005  
    ##  Accipiter striatus: 0   3rd Qu.:2017  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-59.png)<!-- -->

    ##                 species    eventDate   
    ##  Butorides virescens:3   Min.   :2007  
    ##  Acanthis flammea   :0   1st Qu.:2007  
    ##  Accipiter cooperii :0   Median :2007  
    ##  Accipiter gentilis :0   Mean   :2012  
    ##  Accipiter striatus :0   3rd Qu.:2014  
    ##  Actitis macularius :0   Max.   :2022  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-60.png)<!-- -->

    ##                species    eventDate   
    ##  Plegadis chihi    :4   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2015  
    ##  Accipiter gentilis:0   Mean   :2015  
    ##  Accipiter striatus:0   3rd Qu.:2017  
    ##  Actitis macularius:0   Max.   :2017  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-61.png)<!-- -->

    ##                        species    eventDate   
    ##  Plegadis falcinellus/chihi:1   Min.   :2017  
    ##  Acanthis flammea          :0   1st Qu.:2017  
    ##  Accipiter cooperii        :0   Median :2017  
    ##  Accipiter gentilis        :0   Mean   :2017  
    ##  Accipiter striatus        :0   3rd Qu.:2017  
    ##  Actitis macularius        :0   Max.   :2017  
    ##  (Other)                   :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-62.png)<!-- -->

    ##                species      eventDate   
    ##  Cathartes aura    :207   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2012  
    ##  Accipiter cooperii:  0   Median :2016  
    ##  Accipiter gentilis:  0   Mean   :2015  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-63.png)<!-- -->

    ##                species     eventDate   
    ##  Pandion haliaetus :11   Min.   :1986  
    ##  Acanthis flammea  : 0   1st Qu.:2002  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2010  
    ##  Accipiter striatus: 0   3rd Qu.:2018  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-64.png)<!-- -->

    ##                species    eventDate   
    ##  Aquila chrysaetos :6   Min.   :1987  
    ##  Acanthis flammea  :0   1st Qu.:1988  
    ##  Accipiter cooperii:0   Median :1989  
    ##  Accipiter gentilis:0   Mean   :1999  
    ##  Accipiter striatus:0   3rd Qu.:2013  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-65.png)<!-- -->

    ##                species     eventDate   
    ##  Circus hudsonius  :51   Min.   :1986  
    ##  Acanthis flammea  : 0   1st Qu.:2000  
    ##  Accipiter cooperii: 0   Median :2011  
    ##  Accipiter gentilis: 0   Mean   :2008  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-66.png)<!-- -->

    ##                  species     eventDate   
    ##  Accipiter striatus  :14   Min.   :1985  
    ##  Acanthis flammea    : 0   1st Qu.:1989  
    ##  Accipiter cooperii  : 0   Median :1995  
    ##  Accipiter gentilis  : 0   Mean   :2001  
    ##  Actitis macularius  : 0   3rd Qu.:2013  
    ##  Aechmophorus clarkii: 0   Max.   :2021  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-67.png)<!-- -->

    ##                  species     eventDate   
    ##  Accipiter cooperii  :21   Min.   :1990  
    ##  Acanthis flammea    : 0   1st Qu.:2009  
    ##  Accipiter gentilis  : 0   Median :2018  
    ##  Accipiter striatus  : 0   Mean   :2014  
    ##  Actitis macularius  : 0   3rd Qu.:2020  
    ##  Aechmophorus clarkii: 0   Max.   :2022  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-68.png)<!-- -->

    ##                      species     eventDate   
    ##  Haliaeetus leucocephalus:30   Min.   :1978  
    ##  Acanthis flammea        : 0   1st Qu.:2014  
    ##  Accipiter cooperii      : 0   Median :2018  
    ##  Accipiter gentilis      : 0   Mean   :2015  
    ##  Accipiter striatus      : 0   3rd Qu.:2021  
    ##  Actitis macularius      : 0   Max.   :2023  
    ##  (Other)                 : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-69.png)<!-- -->

    ##                species    eventDate   
    ##  Buteo platypterus :6   Min.   :2015  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-70.png)<!-- -->

    ##                species     eventDate   
    ##  Buteo swainsoni   :22   Min.   :1986  
    ##  Acanthis flammea  : 0   1st Qu.:2003  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2010  
    ##  Accipiter striatus: 0   3rd Qu.:2018  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-71.png)<!-- -->

    ##                species      eventDate   
    ##  Buteo jamaicensis :147   Min.   :1978  
    ##  Acanthis flammea  :  0   1st Qu.:2005  
    ##  Accipiter cooperii:  0   Median :2014  
    ##  Accipiter gentilis:  0   Mean   :2010  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-72.png)<!-- -->

    ##                species    eventDate   
    ##  Buteo lagopus     :8   Min.   :2001  
    ##  Acanthis flammea  :0   1st Qu.:2004  
    ##  Accipiter cooperii:0   Median :2005  
    ##  Accipiter gentilis:0   Mean   :2008  
    ##  Accipiter striatus:0   3rd Qu.:2009  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-73.png)<!-- -->

    ##                species    eventDate   
    ##  Buteo regalis     :2   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-74.png)<!-- -->

    ##                species    eventDate   
    ##  Tyto alba         :2   Min.   :1987  
    ##  Acanthis flammea  :0   1st Qu.:1987  
    ##  Accipiter cooperii:0   Median :1987  
    ##  Accipiter gentilis:0   Mean   :1987  
    ##  Accipiter striatus:0   3rd Qu.:1987  
    ##  Actitis macularius:0   Max.   :1987  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-75.png)<!-- -->

    ##                species     eventDate   
    ##  Megascops asio    :20   Min.   :1983  
    ##  Acanthis flammea  : 0   1st Qu.:1999  
    ##  Accipiter cooperii: 0   Median :2008  
    ##  Accipiter gentilis: 0   Mean   :2005  
    ##  Accipiter striatus: 0   3rd Qu.:2012  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-76.png)<!-- -->

    ##                species     eventDate   
    ##  Bubo virginianus  :45   Min.   :1978  
    ##  Acanthis flammea  : 0   1st Qu.:2000  
    ##  Accipiter cooperii: 0   Median :2014  
    ##  Accipiter gentilis: 0   Mean   :2009  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-77.png)<!-- -->

    ##                species    eventDate   
    ##  Athene cunicularia:3   Min.   :2005  
    ##  Acanthis flammea  :0   1st Qu.:2005  
    ##  Accipiter cooperii:0   Median :2005  
    ##  Accipiter gentilis:0   Mean   :2011  
    ##  Accipiter striatus:0   3rd Qu.:2014  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-78.png)<!-- -->

    ##                species    eventDate   
    ##  Aegolius acadicus :4   Min.   :2016  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2017  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2018  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-79.png)<!-- -->

    ##                species     eventDate   
    ##  Megaceryle alcyon :62   Min.   :1985  
    ##  Acanthis flammea  : 0   1st Qu.:2002  
    ##  Accipiter cooperii: 0   Median :2014  
    ##  Accipiter gentilis: 0   Mean   :2008  
    ##  Accipiter striatus: 0   3rd Qu.:2017  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-80.png)<!-- -->

    ##                species     eventDate   
    ##  Sphyrapicus varius:11   Min.   :1984  
    ##  Acanthis flammea  : 0   1st Qu.:1989  
    ##  Accipiter cooperii: 0   Median :2004  
    ##  Accipiter gentilis: 0   Mean   :2003  
    ##  Accipiter striatus: 0   3rd Qu.:2016  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-81.png)<!-- -->

    ##                        species      eventDate   
    ##  Melanerpes erythrocephalus:116   Min.   :1986  
    ##  Acanthis flammea          :  0   1st Qu.:2014  
    ##  Accipiter cooperii        :  0   Median :2018  
    ##  Accipiter gentilis        :  0   Mean   :2015  
    ##  Accipiter striatus        :  0   3rd Qu.:2020  
    ##  Actitis macularius        :  0   Max.   :2023  
    ##  (Other)                   :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-82.png)<!-- -->

    ##                  species     eventDate   
    ##  Melanerpes carolinus:21   Min.   :1990  
    ##  Acanthis flammea    : 0   1st Qu.:2006  
    ##  Accipiter cooperii  : 0   Median :2017  
    ##  Accipiter gentilis  : 0   Mean   :2012  
    ##  Accipiter striatus  : 0   3rd Qu.:2020  
    ##  Actitis macularius  : 0   Max.   :2023  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-83.png)<!-- -->

    ##                 species      eventDate   
    ##  Dryobates pubescens:171   Min.   :1985  
    ##  Acanthis flammea   :  0   1st Qu.:2005  
    ##  Accipiter cooperii :  0   Median :2014  
    ##  Accipiter gentilis :  0   Mean   :2011  
    ##  Accipiter striatus :  0   3rd Qu.:2018  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-84.png)<!-- -->

    ##                species     eventDate   
    ##  Dryobates villosus:87   Min.   :1986  
    ##  Acanthis flammea  : 0   1st Qu.:2005  
    ##  Accipiter cooperii: 0   Median :2012  
    ##  Accipiter gentilis: 0   Mean   :2009  
    ##  Accipiter striatus: 0   3rd Qu.:2016  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-85.png)<!-- -->

    ##                species      eventDate   
    ##  Colaptes auratus  :244   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2009  
    ##  Accipiter cooperii:  0   Median :2015  
    ##  Accipiter gentilis:  0   Mean   :2012  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-86.png)<!-- -->

    ##                species    eventDate   
    ##  Falco columbarius :8   Min.   :1983  
    ##  Acanthis flammea  :0   1st Qu.:2005  
    ##  Accipiter cooperii:0   Median :2016  
    ##  Accipiter gentilis:0   Mean   :2009  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-87.png)<!-- -->

    ##                species    eventDate   
    ##  Falco mexicanus   :4   Min.   :1990  
    ##  Acanthis flammea  :0   1st Qu.:1990  
    ##  Accipiter cooperii:0   Median :2004  
    ##  Accipiter gentilis:0   Mean   :2004  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2018  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-88.png)<!-- -->

    ##                 species     eventDate   
    ##  Contopus sordidulus:14   Min.   :2010  
    ##  Acanthis flammea   : 0   1st Qu.:2014  
    ##  Accipiter cooperii : 0   Median :2014  
    ##  Accipiter gentilis : 0   Mean   :2017  
    ##  Accipiter striatus : 0   3rd Qu.:2023  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-89.png)<!-- -->

    ##                species     eventDate   
    ##  Contopus virens   :19   Min.   :2014  
    ##  Acanthis flammea  : 0   1st Qu.:2018  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-90.png)<!-- -->

    ##                        species    eventDate   
    ##  Contopus sordidulus/virens:1   Min.   :2014  
    ##  Acanthis flammea          :0   1st Qu.:2014  
    ##  Accipiter cooperii        :0   Median :2014  
    ##  Accipiter gentilis        :0   Mean   :2014  
    ##  Accipiter striatus        :0   3rd Qu.:2014  
    ##  Actitis macularius        :0   Max.   :2014  
    ##  (Other)                   :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-91.png)<!-- -->

    ##                species    eventDate   
    ##  Empidonax alnorum :2   Min.   :2018  
    ##  Acanthis flammea  :0   1st Qu.:2018  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2018  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-92.png)<!-- -->

    ##                species     eventDate   
    ##  Empidonax traillii:10   Min.   :2008  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2016  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-93.png)<!-- -->

    ##                        species    eventDate   
    ##  Empidonax alnorum/traillii:1   Min.   :2023  
    ##  Acanthis flammea          :0   1st Qu.:2023  
    ##  Accipiter cooperii        :0   Median :2023  
    ##  Accipiter gentilis        :0   Mean   :2023  
    ##  Accipiter striatus        :0   3rd Qu.:2023  
    ##  Actitis macularius        :0   Max.   :2023  
    ##  (Other)                   :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-94.png)<!-- -->

    ##                species     eventDate   
    ##  Empidonax minimus :25   Min.   :1990  
    ##  Acanthis flammea  : 0   1st Qu.:2012  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2013  
    ##  Accipiter striatus: 0   3rd Qu.:2017  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-95.png)<!-- -->

    ##                species     eventDate   
    ##  Sayornis phoebe   :42   Min.   :1994  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-96.png)<!-- -->

    ##                species    eventDate   
    ##  Sayornis saya     :2   Min.   :2020  
    ##  Acanthis flammea  :0   1st Qu.:2020  
    ##  Accipiter cooperii:0   Median :2020  
    ##  Accipiter gentilis:0   Mean   :2020  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2020  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-97.png)<!-- -->

    ##                species     eventDate   
    ##  Myiarchus crinitus:72   Min.   :1990  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2015  
    ##  Accipiter striatus: 0   3rd Qu.:2018  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-98.png)<!-- -->

    ##                 species     eventDate   
    ##  Tyrannus verticalis:50   Min.   :1994  
    ##  Acanthis flammea   : 0   1st Qu.:2011  
    ##  Accipiter cooperii : 0   Median :2015  
    ##  Accipiter gentilis : 0   Mean   :2014  
    ##  Accipiter striatus : 0   3rd Qu.:2018  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-99.png)<!-- -->

    ##                species      eventDate   
    ##  Tyrannus tyrannus :113   Min.   :1957  
    ##  Acanthis flammea  :  0   1st Qu.:2011  
    ##  Accipiter cooperii:  0   Median :2015  
    ##  Accipiter gentilis:  0   Mean   :2013  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-100.png)<!-- -->

    ##                species      eventDate   
    ##  Vireo bellii      :153   Min.   :1957  
    ##  Acanthis flammea  :  0   1st Qu.:2012  
    ##  Accipiter cooperii:  0   Median :2015  
    ##  Accipiter gentilis:  0   Mean   :2014  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-101.png)<!-- -->

    ##                species    eventDate   
    ##  Vireo flavifrons  :4   Min.   :2015  
    ##  Acanthis flammea  :0   1st Qu.:2015  
    ##  Accipiter cooperii:0   Median :2015  
    ##  Accipiter gentilis:0   Mean   :2015  
    ##  Accipiter striatus:0   3rd Qu.:2015  
    ##  Actitis macularius:0   Max.   :2015  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-102.png)<!-- -->

    ##                  species    eventDate   
    ##  Vireo philadelphicus:4   Min.   :2015  
    ##  Acanthis flammea    :0   1st Qu.:2015  
    ##  Accipiter cooperii  :0   Median :2015  
    ##  Accipiter gentilis  :0   Mean   :2015  
    ##  Accipiter striatus  :0   3rd Qu.:2015  
    ##  Actitis macularius  :0   Max.   :2015  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-103.png)<!-- -->

    ##                species     eventDate   
    ##  Vireo gilvus      :40   Min.   :1990  
    ##  Acanthis flammea  : 0   1st Qu.:2012  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2013  
    ##  Accipiter striatus: 0   3rd Qu.:2017  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-104.png)<!-- -->

    ##                species      eventDate   
    ##  Vireo olivaceus   :117   Min.   :1957  
    ##  Acanthis flammea  :  0   1st Qu.:2014  
    ##  Accipiter cooperii:  0   Median :2016  
    ##  Accipiter gentilis:  0   Mean   :2015  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-105.png)<!-- -->

    ##                 species     eventDate   
    ##  Lanius ludovicianus:13   Min.   :1987  
    ##  Acanthis flammea   : 0   1st Qu.:2000  
    ##  Accipiter cooperii : 0   Median :2016  
    ##  Accipiter gentilis : 0   Mean   :2010  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-106.png)<!-- -->

    ##                species    eventDate   
    ##  Lanius borealis   :6   Min.   :1999  
    ##  Acanthis flammea  :0   1st Qu.:2000  
    ##  Accipiter cooperii:0   Median :2004  
    ##  Accipiter gentilis:0   Mean   :2008  
    ##  Accipiter striatus:0   3rd Qu.:2017  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-107.png)<!-- -->

    ##                 species      eventDate   
    ##  Cyanocitta cristata:226   Min.   :1985  
    ##  Acanthis flammea   :  0   1st Qu.:2010  
    ##  Accipiter cooperii :  0   Median :2017  
    ##  Accipiter gentilis :  0   Mean   :2013  
    ##  Accipiter striatus :  0   3rd Qu.:2021  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-108.png)<!-- -->

    ##                species     eventDate   
    ##  Pica hudsonia     :20   Min.   :1978  
    ##  Acanthis flammea  : 0   1st Qu.:1986  
    ##  Accipiter cooperii: 0   Median :1988  
    ##  Accipiter gentilis: 0   Mean   :1990  
    ##  Accipiter striatus: 0   3rd Qu.:1994  
    ##  Actitis macularius: 0   Max.   :2004  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-109.png)<!-- -->

    ##                   species      eventDate   
    ##  Corvus brachyrhynchos:284   Min.   :1978  
    ##  Acanthis flammea     :  0   1st Qu.:2006  
    ##  Accipiter cooperii   :  0   Median :2015  
    ##  Accipiter gentilis   :  0   Mean   :2012  
    ##  Accipiter striatus   :  0   3rd Qu.:2020  
    ##  Actitis macularius   :  0   Max.   :2023  
    ##  (Other)              :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-110.png)<!-- -->

    ##                  species      eventDate   
    ##  Poecile atricapillus:267   Min.   :1985  
    ##  Acanthis flammea    :  0   1st Qu.:2009  
    ##  Accipiter cooperii  :  0   Median :2015  
    ##  Accipiter gentilis  :  0   Mean   :2012  
    ##  Accipiter striatus  :  0   3rd Qu.:2020  
    ##  Actitis macularius  :  0   Max.   :2023  
    ##  (Other)             :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-111.png)<!-- -->

    ##                  species     eventDate   
    ##  Eremophila alpestris:73   Min.   :1978  
    ##  Acanthis flammea    : 0   1st Qu.:2002  
    ##  Accipiter cooperii  : 0   Median :2009  
    ##  Accipiter gentilis  : 0   Mean   :2008  
    ##  Accipiter striatus  : 0   3rd Qu.:2021  
    ##  Actitis macularius  : 0   Max.   :2023  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-112.png)<!-- -->

    ##                        species     eventDate   
    ##  Stelgidopteryx serripennis:32   Min.   :2007  
    ##  Acanthis flammea          : 0   1st Qu.:2015  
    ##  Accipiter cooperii        : 0   Median :2017  
    ##  Accipiter gentilis        : 0   Mean   :2017  
    ##  Accipiter striatus        : 0   3rd Qu.:2021  
    ##  Actitis macularius        : 0   Max.   :2023  
    ##  (Other)                   : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-113.png)<!-- -->

    ##                 species     eventDate   
    ##  Tachycineta bicolor:32   Min.   :2009  
    ##  Acanthis flammea   : 0   1st Qu.:2015  
    ##  Accipiter cooperii : 0   Median :2017  
    ##  Accipiter gentilis : 0   Mean   :2017  
    ##  Accipiter striatus : 0   3rd Qu.:2020  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-114.png)<!-- -->

    ##                species     eventDate   
    ##  Riparia riparia   :10   Min.   :1986  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2014  
    ##  Accipiter gentilis: 0   Mean   :2011  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-115.png)<!-- -->

    ##                species      eventDate   
    ##  Hirundo rustica   :173   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2012  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2014  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-116.png)<!-- -->

    ##                      species      eventDate   
    ##  Petrochelidon pyrrhonota:110   Min.   :1986  
    ##  Acanthis flammea        :  0   1st Qu.:2014  
    ##  Accipiter cooperii      :  0   Median :2017  
    ##  Accipiter gentilis      :  0   Mean   :2016  
    ##  Accipiter striatus      :  0   3rd Qu.:2021  
    ##  Actitis macularius      :  0   Max.   :2023  
    ##  (Other)                 :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-117.png)<!-- -->

    ##                 species     eventDate   
    ##  Corthylio calendula:21   Min.   :1985  
    ##  Acanthis flammea   : 0   1st Qu.:1987  
    ##  Accipiter cooperii : 0   Median :2012  
    ##  Accipiter gentilis : 0   Mean   :2005  
    ##  Accipiter striatus : 0   3rd Qu.:2015  
    ##  Actitis macularius : 0   Max.   :2022  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-118.png)<!-- -->

    ##                species     eventDate   
    ##  Regulus satrapa   :17   Min.   :1986  
    ##  Acanthis flammea  : 0   1st Qu.:1986  
    ##  Accipiter cooperii: 0   Median :1989  
    ##  Accipiter gentilis: 0   Mean   :1995  
    ##  Accipiter striatus: 0   3rd Qu.:2004  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-119.png)<!-- -->

    ##                species      eventDate   
    ##  Sitta canadensis  :271   Min.   :1978  
    ##  Acanthis flammea  :  0   1st Qu.:2009  
    ##  Accipiter cooperii:  0   Median :2016  
    ##  Accipiter gentilis:  0   Mean   :2012  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-120.png)<!-- -->

    ##                species      eventDate   
    ##  Sitta carolinensis:110   Min.   :1994  
    ##  Acanthis flammea  :  0   1st Qu.:2011  
    ##  Accipiter cooperii:  0   Median :2016  
    ##  Accipiter gentilis:  0   Mean   :2014  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-121.png)<!-- -->

    ##                species     eventDate   
    ##  Sitta pygmaea     :14   Min.   :2014  
    ##  Acanthis flammea  : 0   1st Qu.:2017  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-122.png)<!-- -->

    ##                species    eventDate   
    ##  Certhia americana :6   Min.   :1992  
    ##  Acanthis flammea  :0   1st Qu.:1995  
    ##  Accipiter cooperii:0   Median :2004  
    ##  Accipiter gentilis:0   Mean   :2002  
    ##  Accipiter striatus:0   3rd Qu.:2008  
    ##  Actitis macularius:0   Max.   :2009  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-123.png)<!-- -->

    ##                 species    eventDate   
    ##  Polioptila caerulea:6   Min.   :2009  
    ##  Acanthis flammea   :0   1st Qu.:2012  
    ##  Accipiter cooperii :0   Median :2020  
    ##  Accipiter gentilis :0   Mean   :2017  
    ##  Accipiter striatus :0   3rd Qu.:2022  
    ##  Actitis macularius :0   Max.   :2023  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-124.png)<!-- -->

    ##                species      eventDate   
    ##  Troglodytes aedon :205   Min.   :1985  
    ##  Acanthis flammea  :  0   1st Qu.:2014  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2015  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-125.png)<!-- -->

    ##                   species    eventDate   
    ##  Cistothorus stellaris:4   Min.   :2010  
    ##  Acanthis flammea     :0   1st Qu.:2010  
    ##  Accipiter cooperii   :0   Median :2014  
    ##  Accipiter gentilis   :0   Mean   :2014  
    ##  Accipiter striatus   :0   3rd Qu.:2018  
    ##  Actitis macularius   :0   Max.   :2018  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-126.png)<!-- -->

    ##                   species    eventDate   
    ##  Cistothorus palustris:6   Min.   :2013  
    ##  Acanthis flammea     :0   1st Qu.:2014  
    ##  Accipiter cooperii   :0   Median :2017  
    ##  Accipiter gentilis   :0   Mean   :2016  
    ##  Accipiter striatus   :0   3rd Qu.:2018  
    ##  Actitis macularius   :0   Max.   :2019  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-127.png)<!-- -->

    ##                species     eventDate   
    ##  Sturnus vulgaris  :79   Min.   :1978  
    ##  Acanthis flammea  : 0   1st Qu.:2006  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2013  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-128.png)<!-- -->

    ##                    species     eventDate   
    ##  Dumetella carolinensis:59   Min.   :1990  
    ##  Acanthis flammea      : 0   1st Qu.:2006  
    ##  Accipiter cooperii    : 0   Median :2014  
    ##  Accipiter gentilis    : 0   Mean   :2011  
    ##  Accipiter striatus    : 0   3rd Qu.:2018  
    ##  Actitis macularius    : 0   Max.   :2023  
    ##  (Other)               : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-129.png)<!-- -->

    ##                species      eventDate   
    ##  Toxostoma rufum   :117   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2009  
    ##  Accipiter cooperii:  0   Median :2015  
    ##  Accipiter gentilis:  0   Mean   :2012  
    ##  Accipiter striatus:  0   3rd Qu.:2018  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-130.png)<!-- -->

    ##                species      eventDate   
    ##  Sialia sialis     :135   Min.   :1985  
    ##  Acanthis flammea  :  0   1st Qu.:2010  
    ##  Accipiter cooperii:  0   Median :2015  
    ##  Accipiter gentilis:  0   Mean   :2011  
    ##  Accipiter striatus:  0   3rd Qu.:2019  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-131.png)<!-- -->

    ##                species    eventDate   
    ##  Sialia currucoides:3   Min.   :1989  
    ##  Acanthis flammea  :0   1st Qu.:1989  
    ##  Accipiter cooperii:0   Median :1989  
    ##  Accipiter gentilis:0   Mean   :2000  
    ##  Accipiter striatus:0   3rd Qu.:2006  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-132.png)<!-- -->

    ##                 species     eventDate   
    ##  Myadestes townsendi:62   Min.   :1985  
    ##  Acanthis flammea   : 0   1st Qu.:2005  
    ##  Accipiter cooperii : 0   Median :2014  
    ##  Accipiter gentilis : 0   Mean   :2011  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-133.png)<!-- -->

    ##                species     eventDate   
    ##  Catharus ustulatus:26   Min.   :1990  
    ##  Acanthis flammea  : 0   1st Qu.:2006  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2012  
    ##  Accipiter striatus: 0   3rd Qu.:2018  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-134.png)<!-- -->

    ##                species    eventDate   
    ##  Catharus guttatus :6   Min.   :1985  
    ##  Acanthis flammea  :0   1st Qu.:1993  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2007  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2018  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-135.png)<!-- -->

    ##                  species    eventDate   
    ##  Hylocichla mustelina:2   Min.   :2016  
    ##  Acanthis flammea    :0   1st Qu.:2016  
    ##  Accipiter cooperii  :0   Median :2016  
    ##  Accipiter gentilis  :0   Mean   :2016  
    ##  Accipiter striatus  :0   3rd Qu.:2016  
    ##  Actitis macularius  :0   Max.   :2016  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-136.png)<!-- -->

    ##                species      eventDate   
    ##  Turdus migratorius:450   Min.   :1957  
    ##  Acanthis flammea  :  0   1st Qu.:2012  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2014  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-137.png)<!-- -->

    ##                 species      eventDate   
    ##  Bombycilla cedrorum:209   Min.   :1986  
    ##  Acanthis flammea   :  0   1st Qu.:2011  
    ##  Accipiter cooperii :  0   Median :2015  
    ##  Accipiter gentilis :  0   Mean   :2013  
    ##  Accipiter striatus :  0   3rd Qu.:2019  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-138.png)<!-- -->

    ##                species     eventDate   
    ##  Passer domesticus :36   Min.   :1987  
    ##  Acanthis flammea  : 0   1st Qu.:2004  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2010  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-139.png)<!-- -->

    ##                species    eventDate   
    ##  Anthus rubescens  :2   Min.   :1987  
    ##  Acanthis flammea  :0   1st Qu.:1987  
    ##  Accipiter cooperii:0   Median :1987  
    ##  Accipiter gentilis:0   Mean   :1987  
    ##  Accipiter striatus:0   3rd Qu.:1987  
    ##  Actitis macularius:0   Max.   :1987  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-140.png)<!-- -->

    ##                  species      eventDate   
    ##  Haemorhous mexicanus:191   Min.   :1985  
    ##  Acanthis flammea    :  0   1st Qu.:2012  
    ##  Accipiter cooperii  :  0   Median :2015  
    ##  Accipiter gentilis  :  0   Mean   :2015  
    ##  Accipiter striatus  :  0   3rd Qu.:2020  
    ##  Actitis macularius  :  0   Max.   :2023  
    ##  (Other)             :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-141.png)<!-- -->

    ##                  species    eventDate   
    ##  Haemorhous purpureus:8   Min.   :1996  
    ##  Acanthis flammea    :0   1st Qu.:2002  
    ##  Accipiter cooperii  :0   Median :2012  
    ##  Accipiter gentilis  :0   Mean   :2010  
    ##  Accipiter striatus  :0   3rd Qu.:2020  
    ##  Actitis macularius  :0   Max.   :2021  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-142.png)<!-- -->

    ##                species     eventDate   
    ##  Loxia curvirostra :51   Min.   :1987  
    ##  Acanthis flammea  : 0   1st Qu.:2012  
    ##  Accipiter cooperii: 0   Median :2019  
    ##  Accipiter gentilis: 0   Mean   :2013  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-143.png)<!-- -->

    ##                species     eventDate   
    ##  Spinus pinus      :75   Min.   :1985  
    ##  Acanthis flammea  : 0   1st Qu.:2000  
    ##  Accipiter cooperii: 0   Median :2012  
    ##  Accipiter gentilis: 0   Mean   :2008  
    ##  Accipiter striatus: 0   3rd Qu.:2016  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-144.png)<!-- -->

    ##                species      eventDate   
    ##  Spinus tristis    :384   Min.   :1957  
    ##  Acanthis flammea  :  0   1st Qu.:2014  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2015  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-145.png)<!-- -->

    ##                  species    eventDate   
    ##  Calcarius lapponicus:2   Min.   :2020  
    ##  Acanthis flammea    :0   1st Qu.:2020  
    ##  Accipiter cooperii  :0   Median :2020  
    ##  Accipiter gentilis  :0   Mean   :2020  
    ##  Accipiter striatus  :0   3rd Qu.:2020  
    ##  Actitis macularius  :0   Max.   :2020  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-146.png)<!-- -->

    ##                   species     eventDate   
    ##  Ammodramus savannarum:81   Min.   :1907  
    ##  Acanthis flammea     : 0   1st Qu.:2010  
    ##  Accipiter cooperii   : 0   Median :2017  
    ##  Accipiter gentilis   : 0   Mean   :2013  
    ##  Accipiter striatus   : 0   3rd Qu.:2020  
    ##  Actitis macularius   : 0   Max.   :2023  
    ##  (Other)              : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-147.png)<!-- -->

    ##                species      eventDate   
    ##  Spizella passerina:324   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2011  
    ##  Accipiter cooperii:  0   Median :2015  
    ##  Accipiter gentilis:  0   Mean   :2014  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-148.png)<!-- -->

    ##                species     eventDate   
    ##  Spizella pallida  :68   Min.   :1987  
    ##  Acanthis flammea  : 0   1st Qu.:2006  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2012  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-149.png)<!-- -->

    ##                species      eventDate   
    ##  Spizella pusilla  :205   Min.   :1957  
    ##  Acanthis flammea  :  0   1st Qu.:2010  
    ##  Accipiter cooperii:  0   Median :2015  
    ##  Accipiter gentilis:  0   Mean   :2013  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-150.png)<!-- -->

    ##                  species      eventDate   
    ##  Chondestes grammacus:182   Min.   :1957  
    ##  Acanthis flammea    :  0   1st Qu.:2014  
    ##  Accipiter cooperii  :  0   Median :2017  
    ##  Accipiter gentilis  :  0   Mean   :2015  
    ##  Accipiter striatus  :  0   3rd Qu.:2021  
    ##  Actitis macularius  :  0   Max.   :2023  
    ##  (Other)             :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-151.png)<!-- -->

    ##                  species     eventDate   
    ##  Spizelloides arborea:28   Min.   :1999  
    ##  Acanthis flammea    : 0   1st Qu.:2005  
    ##  Accipiter cooperii  : 0   Median :2013  
    ##  Accipiter gentilis  : 0   Mean   :2013  
    ##  Accipiter striatus  : 0   3rd Qu.:2022  
    ##  Actitis macularius  : 0   Max.   :2023  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-152.png)<!-- -->

    ##                species      eventDate   
    ##  Junco sp.         :102   Min.   :1985  
    ##  Acanthis flammea  :  0   1st Qu.:2000  
    ##  Accipiter cooperii:  0   Median :2014  
    ##  Accipiter gentilis:  0   Mean   :2010  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-153.png)<!-- -->

    ##                species     eventDate   
    ##  Junco hyemalis    :13   Min.   :1985  
    ##  Acanthis flammea  : 0   1st Qu.:2004  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2011  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-154.png)<!-- -->

    ##                    species     eventDate   
    ##  Zonotrichia leucophrys:60   Min.   :1985  
    ##  Acanthis flammea      : 0   1st Qu.:1989  
    ##  Accipiter cooperii    : 0   Median :2015  
    ##  Accipiter gentilis    : 0   Mean   :2008  
    ##  Accipiter striatus    : 0   3rd Qu.:2021  
    ##  Actitis macularius    : 0   Max.   :2023  
    ##  (Other)               : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-155.png)<!-- -->

    ##                 species     eventDate   
    ##  Zonotrichia querula:46   Min.   :1985  
    ##  Acanthis flammea   : 0   1st Qu.:1988  
    ##  Accipiter cooperii : 0   Median :2000  
    ##  Accipiter gentilis : 0   Mean   :2001  
    ##  Accipiter striatus : 0   3rd Qu.:2014  
    ##  Actitis macularius : 0   Max.   :2022  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-156.png)<!-- -->

    ##                    species     eventDate   
    ##  Zonotrichia albicollis:24   Min.   :1985  
    ##  Acanthis flammea      : 0   1st Qu.:1987  
    ##  Accipiter cooperii    : 0   Median :2004  
    ##  Accipiter gentilis    : 0   Mean   :2004  
    ##  Accipiter striatus    : 0   3rd Qu.:2018  
    ##  Actitis macularius    : 0   Max.   :2023  
    ##  (Other)               : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-157.png)<!-- -->

    ##                 species     eventDate   
    ##  Pooecetes gramineus:43   Min.   :1987  
    ##  Acanthis flammea   : 0   1st Qu.:2006  
    ##  Accipiter cooperii : 0   Median :2014  
    ##  Accipiter gentilis : 0   Mean   :2011  
    ##  Accipiter striatus : 0   3rd Qu.:2018  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-158.png)<!-- -->

    ##                       species    eventDate   
    ##  Passerculus sandwichensis:5   Min.   :1994  
    ##  Acanthis flammea         :0   1st Qu.:1994  
    ##  Accipiter cooperii       :0   Median :2015  
    ##  Accipiter gentilis       :0   Mean   :2008  
    ##  Accipiter striatus       :0   3rd Qu.:2015  
    ##  Actitis macularius       :0   Max.   :2022  
    ##  (Other)                  :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-159.png)<!-- -->

    ##                species      eventDate   
    ##  Pipilo maculatus  :244   Min.   :1957  
    ##  Acanthis flammea  :  0   1st Qu.:2009  
    ##  Accipiter cooperii:  0   Median :2015  
    ##  Accipiter gentilis:  0   Mean   :2012  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-160.png)<!-- -->

    ##                     species    eventDate   
    ##  Pipilo erythrophthalmus:8   Min.   :1994  
    ##  Acanthis flammea       :0   1st Qu.:2009  
    ##  Accipiter cooperii     :0   Median :2016  
    ##  Accipiter gentilis     :0   Mean   :2012  
    ##  Accipiter striatus     :0   3rd Qu.:2019  
    ##  Actitis macularius     :0   Max.   :2020  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-161.png)<!-- -->

    ##                                 species    eventDate   
    ##  Pipilo maculatus x erythrophthalmus:3   Min.   :2015  
    ##  Acanthis flammea                   :0   1st Qu.:2016  
    ##  Accipiter cooperii                 :0   Median :2018  
    ##  Accipiter gentilis                 :0   Mean   :2018  
    ##  Accipiter striatus                 :0   3rd Qu.:2020  
    ##  Actitis macularius                 :0   Max.   :2022  
    ##  (Other)                            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-162.png)<!-- -->

    ##                               species     eventDate   
    ##  Pipilo maculatus/erythrophthalmus:19   Min.   :1987  
    ##  Acanthis flammea                 : 0   1st Qu.:2010  
    ##  Accipiter cooperii               : 0   Median :2015  
    ##  Accipiter gentilis               : 0   Mean   :2012  
    ##  Accipiter striatus               : 0   3rd Qu.:2019  
    ##  Actitis macularius               : 0   Max.   :2021  
    ##  (Other)                          : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-163.png)<!-- -->

    ##                species     eventDate   
    ##  Icteria virens    :32   Min.   :1957  
    ##  Acanthis flammea  : 0   1st Qu.:2004  
    ##  Accipiter cooperii: 0   Median :2016  
    ##  Accipiter gentilis: 0   Mean   :2008  
    ##  Accipiter striatus: 0   3rd Qu.:2018  
    ##  Actitis macularius: 0   Max.   :2020  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-164.png)<!-- -->

    ##                           species    eventDate   
    ##  Xanthocephalus xanthocephalus:9   Min.   :2007  
    ##  Acanthis flammea             :0   1st Qu.:2015  
    ##  Accipiter cooperii           :0   Median :2016  
    ##  Accipiter gentilis           :0   Mean   :2015  
    ##  Accipiter striatus           :0   3rd Qu.:2018  
    ##  Actitis macularius           :0   Max.   :2022  
    ##  (Other)                      :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-165.png)<!-- -->

    ##                   species     eventDate   
    ##  Dolichonyx oryzivorus:11   Min.   :2017  
    ##  Acanthis flammea     : 0   1st Qu.:2018  
    ##  Accipiter cooperii   : 0   Median :2019  
    ##  Accipiter gentilis   : 0   Mean   :2020  
    ##  Accipiter striatus   : 0   3rd Qu.:2022  
    ##  Actitis macularius   : 0   Max.   :2023  
    ##  (Other)              : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-166.png)<!-- -->

    ##                species      eventDate   
    ##  Sturnella neglecta:191   Min.   :1978  
    ##  Acanthis flammea  :  0   1st Qu.:2007  
    ##  Accipiter cooperii:  0   Median :2015  
    ##  Accipiter gentilis:  0   Mean   :2012  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-167.png)<!-- -->

    ##                species    eventDate   
    ##  Sturnella magna   :6   Min.   :2017  
    ##  Acanthis flammea  :0   1st Qu.:2017  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2019  
    ##  Actitis macularius:0   Max.   :2020  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-168.png)<!-- -->

    ##                      species    eventDate   
    ##  Sturnella neglecta/magna:9   Min.   :2004  
    ##  Acanthis flammea        :0   1st Qu.:2012  
    ##  Accipiter cooperii      :0   Median :2020  
    ##  Accipiter gentilis      :0   Mean   :2016  
    ##  Accipiter striatus      :0   3rd Qu.:2021  
    ##  Actitis macularius      :0   Max.   :2023  
    ##  (Other)                 :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-169.png)<!-- -->

    ##                species     eventDate   
    ##  Icterus galbula   :64   Min.   :1996  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2015  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-170.png)<!-- -->

    ##                         species    eventDate   
    ##  Icterus bullockii x galbula:1   Min.   :1986  
    ##  Acanthis flammea           :0   1st Qu.:1986  
    ##  Accipiter cooperii         :0   Median :1986  
    ##  Accipiter gentilis         :0   Mean   :1986  
    ##  Accipiter striatus         :0   3rd Qu.:1986  
    ##  Actitis macularius         :0   Max.   :1986  
    ##  (Other)                    :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-171.png)<!-- -->

    ##                 species      eventDate   
    ##  Agelaius phoeniceus:162   Min.   :1986  
    ##  Acanthis flammea   :  0   1st Qu.:2012  
    ##  Accipiter cooperii :  0   Median :2018  
    ##  Accipiter gentilis :  0   Mean   :2015  
    ##  Accipiter striatus :  0   3rd Qu.:2021  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-172.png)<!-- -->

    ##                species      eventDate   
    ##  Molothrus ater    :124   Min.   :1990  
    ##  Acanthis flammea  :  0   1st Qu.:2012  
    ##  Accipiter cooperii:  0   Median :2016  
    ##  Accipiter gentilis:  0   Mean   :2015  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-173.png)<!-- -->

    ##                    species    eventDate   
    ##  Euphagus cyanocephalus:4   Min.   :2020  
    ##  Acanthis flammea      :0   1st Qu.:2020  
    ##  Accipiter cooperii    :0   Median :2020  
    ##  Accipiter gentilis    :0   Mean   :2020  
    ##  Accipiter striatus    :0   3rd Qu.:2021  
    ##  Actitis macularius    :0   Max.   :2021  
    ##  (Other)               :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-174.png)<!-- -->

    ##                species      eventDate   
    ##  Quiscalus quiscula:121   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2010  
    ##  Accipiter cooperii:  0   Median :2016  
    ##  Accipiter gentilis:  0   Mean   :2013  
    ##  Accipiter striatus:  0   3rd Qu.:2019  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-175.png)<!-- -->

    ##                 species    eventDate   
    ##  Quiscalus mexicanus:4   Min.   :2009  
    ##  Acanthis flammea   :0   1st Qu.:2009  
    ##  Accipiter cooperii :0   Median :2014  
    ##  Accipiter gentilis :0   Mean   :2014  
    ##  Accipiter striatus :0   3rd Qu.:2018  
    ##  Actitis macularius :0   Max.   :2018  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-176.png)<!-- -->

    ##                 species     eventDate   
    ##  Seiurus aurocapilla:67   Min.   :2002  
    ##  Acanthis flammea   : 0   1st Qu.:2015  
    ##  Accipiter cooperii : 0   Median :2017  
    ##  Accipiter gentilis : 0   Mean   :2016  
    ##  Accipiter striatus : 0   3rd Qu.:2019  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-177.png)<!-- -->

    ##                species     eventDate   
    ##  Mniotilta varia   :14   Min.   :1957  
    ##  Acanthis flammea  : 0   1st Qu.:1986  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2001  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-178.png)<!-- -->

    ##                   species    eventDate   
    ##  Leiothlypis peregrina:5   Min.   :2015  
    ##  Acanthis flammea     :0   1st Qu.:2017  
    ##  Accipiter cooperii   :0   Median :2017  
    ##  Accipiter gentilis   :0   Mean   :2017  
    ##  Accipiter striatus   :0   3rd Qu.:2018  
    ##  Actitis macularius   :0   Max.   :2018  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-179.png)<!-- -->

    ##                species     eventDate   
    ##  Leiothlypis celata:43   Min.   :1985  
    ##  Acanthis flammea  : 0   1st Qu.:1989  
    ##  Accipiter cooperii: 0   Median :2009  
    ##  Accipiter gentilis: 0   Mean   :2005  
    ##  Accipiter striatus: 0   3rd Qu.:2015  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-180.png)<!-- -->

    ##                     species    eventDate   
    ##  Leiothlypis ruficapilla:2   Min.   :1990  
    ##  Acanthis flammea       :0   1st Qu.:1990  
    ##  Accipiter cooperii     :0   Median :1990  
    ##  Accipiter gentilis     :0   Mean   :1990  
    ##  Accipiter striatus     :0   3rd Qu.:1990  
    ##  Actitis macularius     :0   Max.   :1990  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-181.png)<!-- -->

    ##                     species    eventDate   
    ##  Geothlypis philadelphia:2   Min.   :2009  
    ##  Acanthis flammea       :0   1st Qu.:2009  
    ##  Accipiter cooperii     :0   Median :2009  
    ##  Accipiter gentilis     :0   Mean   :2009  
    ##  Accipiter striatus     :0   3rd Qu.:2009  
    ##  Actitis macularius     :0   Max.   :2009  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-182.png)<!-- -->

    ##                species      eventDate   
    ##  Geothlypis trichas:107   Min.   :1957  
    ##  Acanthis flammea  :  0   1st Qu.:2014  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2015  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-183.png)<!-- -->

    ##                species    eventDate   
    ##  Setophaga citrina :4   Min.   :2015  
    ##  Acanthis flammea  :0   1st Qu.:2015  
    ##  Accipiter cooperii:0   Median :2015  
    ##  Accipiter gentilis:0   Mean   :2015  
    ##  Accipiter striatus:0   3rd Qu.:2015  
    ##  Actitis macularius:0   Max.   :2015  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-184.png)<!-- -->

    ##                 species     eventDate   
    ##  Setophaga ruticilla:25   Min.   :1957  
    ##  Acanthis flammea   : 0   1st Qu.:2015  
    ##  Accipiter cooperii : 0   Median :2017  
    ##  Accipiter gentilis : 0   Mean   :2013  
    ##  Accipiter striatus : 0   3rd Qu.:2020  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-185.png)<!-- -->

    ##                species    eventDate   
    ##  Setophaga castanea:2   Min.   :1985  
    ##  Acanthis flammea  :0   1st Qu.:1985  
    ##  Accipiter cooperii:0   Median :1985  
    ##  Accipiter gentilis:0   Mean   :1985  
    ##  Accipiter striatus:0   3rd Qu.:1985  
    ##  Actitis macularius:0   Max.   :1985  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-186.png)<!-- -->

    ##                species      eventDate   
    ##  Setophaga petechia:130   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2014  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-187.png)<!-- -->

    ##                    species    eventDate   
    ##  Setophaga pensylvanica:1   Min.   :2015  
    ##  Acanthis flammea      :0   1st Qu.:2015  
    ##  Accipiter cooperii    :0   Median :2015  
    ##  Accipiter gentilis    :0   Mean   :2015  
    ##  Accipiter striatus    :0   3rd Qu.:2015  
    ##  Actitis macularius    :0   Max.   :2015  
    ##  (Other)               :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-188.png)<!-- -->

    ##                species    eventDate   
    ##  Setophaga striata :5   Min.   :2015  
    ##  Acanthis flammea  :0   1st Qu.:2015  
    ##  Accipiter cooperii:0   Median :2015  
    ##  Accipiter gentilis:0   Mean   :2017  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-189.png)<!-- -->

    ##                         species     eventDate   
    ##  Setophaga coronata/auduboni:28   Min.   :1985  
    ##  Acanthis flammea           : 0   1st Qu.:2010  
    ##  Accipiter cooperii         : 0   Median :2016  
    ##  Accipiter gentilis         : 0   Mean   :2012  
    ##  Accipiter striatus         : 0   3rd Qu.:2021  
    ##  Actitis macularius         : 0   Max.   :2023  
    ##  (Other)                    : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-190.png)<!-- -->

    ##                species     eventDate   
    ##  Setophaga coronata:48   Min.   :1985  
    ##  Acanthis flammea  : 0   1st Qu.:2000  
    ##  Accipiter cooperii: 0   Median :2012  
    ##  Accipiter gentilis: 0   Mean   :2008  
    ##  Accipiter striatus: 0   3rd Qu.:2017  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-191.png)<!-- -->

    ##                species     eventDate   
    ##  Cardellina pusilla:15   Min.   :1985  
    ##  Acanthis flammea  : 0   1st Qu.:1992  
    ##  Accipiter cooperii: 0   Median :2006  
    ##  Accipiter gentilis: 0   Mean   :2003  
    ##  Accipiter striatus: 0   3rd Qu.:2012  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-192.png)<!-- -->

    ##                   species      eventDate   
    ##  Cardinalis cardinalis:252   Min.   :1986  
    ##  Acanthis flammea     :  0   1st Qu.:2012  
    ##  Accipiter cooperii   :  0   Median :2017  
    ##  Accipiter gentilis   :  0   Mean   :2014  
    ##  Accipiter striatus   :  0   3rd Qu.:2020  
    ##  Actitis macularius   :  0   Max.   :2023  
    ##  (Other)              :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-193.png)<!-- -->

    ##                     species     eventDate   
    ##  Pheucticus ludovicianus:12   Min.   :2002  
    ##  Acanthis flammea       : 0   1st Qu.:2012  
    ##  Accipiter cooperii     : 0   Median :2020  
    ##  Accipiter gentilis     : 0   Mean   :2016  
    ##  Accipiter striatus     : 0   3rd Qu.:2023  
    ##  Actitis macularius     : 0   Max.   :2023  
    ##  (Other)                : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-194.png)<!-- -->

    ##                       species     eventDate   
    ##  Pheucticus melanocephalus:10   Min.   :1957  
    ##  Acanthis flammea         : 0   1st Qu.:2012  
    ##  Accipiter cooperii       : 0   Median :2015  
    ##  Accipiter gentilis       : 0   Mean   :2004  
    ##  Accipiter striatus       : 0   3rd Qu.:2017  
    ##  Actitis macularius       : 0   Max.   :2018  
    ##  (Other)                  : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-195.png)<!-- -->

    ##                                    species    eventDate   
    ##  Pheucticus ludovicianus/melanocephalus:1   Min.   :2021  
    ##  Acanthis flammea                      :0   1st Qu.:2021  
    ##  Accipiter cooperii                    :0   Median :2021  
    ##  Accipiter gentilis                    :0   Mean   :2021  
    ##  Accipiter striatus                    :0   3rd Qu.:2021  
    ##  Actitis macularius                    :0   Max.   :2021  
    ##  (Other)                               :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-196.png)<!-- -->

    ##                species     eventDate   
    ##  Passerina caerulea:57   Min.   :2002  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-197.png)<!-- -->

    ##                species    eventDate   
    ##  Passerina amoena  :6   Min.   :2012  
    ##  Acanthis flammea  :0   1st Qu.:2012  
    ##  Accipiter cooperii:0   Median :2012  
    ##  Accipiter gentilis:0   Mean   :2016  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-198.png)<!-- -->

    ##                species     eventDate   
    ##  Passerina cyanea  :81   Min.   :2002  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-199.png)<!-- -->

    ##                     species    eventDate   
    ##  Passerina amoena/cyanea:1   Min.   :2014  
    ##  Acanthis flammea       :0   1st Qu.:2014  
    ##  Accipiter cooperii     :0   Median :2014  
    ##  Accipiter gentilis     :0   Mean   :2014  
    ##  Accipiter striatus     :0   3rd Qu.:2014  
    ##  Actitis macularius     :0   Max.   :2014  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-200.png)<!-- -->

    ##                species     eventDate   
    ##  Spiza americana   :37   Min.   :2010  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-201.png)<!-- -->

    ##                species     eventDate   
    ##  Aix sponsa        :15   Min.   :1990  
    ##  Acanthis flammea  : 0   1st Qu.:2008  
    ##  Accipiter cooperii: 0   Median :2011  
    ##  Accipiter gentilis: 0   Mean   :2009  
    ##  Accipiter striatus: 0   3rd Qu.:2012  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-202.png)<!-- -->

    ##                species    eventDate   
    ##  Rallus limicola   :1   Min.   :1957  
    ##  Acanthis flammea  :0   1st Qu.:1957  
    ##  Accipiter cooperii:0   Median :1957  
    ##  Accipiter gentilis:0   Mean   :1957  
    ##  Accipiter striatus:0   3rd Qu.:1957  
    ##  Actitis macularius:0   Max.   :1957  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-203.png)<!-- -->

    ##                species    eventDate   
    ##  Asio otus         :2   Min.   :2009  
    ##  Acanthis flammea  :0   1st Qu.:2011  
    ##  Accipiter cooperii:0   Median :2014  
    ##  Accipiter gentilis:0   Mean   :2014  
    ##  Accipiter striatus:0   3rd Qu.:2016  
    ##  Actitis macularius:0   Max.   :2018  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-204.png)<!-- -->

    ##                species     eventDate   
    ##  Falco sparverius  :45   Min.   :1986  
    ##  Acanthis flammea  : 0   1st Qu.:2004  
    ##  Accipiter cooperii: 0   Median :2010  
    ##  Accipiter gentilis: 0   Mean   :2009  
    ##  Accipiter striatus: 0   3rd Qu.:2015  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-205.png)<!-- -->

    ##                 species    eventDate   
    ##  Tyrannus forficatus:1   Min.   :1957  
    ##  Acanthis flammea   :0   1st Qu.:1957  
    ##  Accipiter cooperii :0   Median :1957  
    ##  Accipiter gentilis :0   Mean   :1957  
    ##  Accipiter striatus :0   3rd Qu.:1957  
    ##  Actitis macularius :0   Max.   :1957  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-206.png)<!-- -->

    ##                        species    eventDate   
    ##  Coccothraustes vespertinus:2   Min.   :1986  
    ##  Acanthis flammea          :0   1st Qu.:1992  
    ##  Accipiter cooperii        :0   Median :1998  
    ##  Accipiter gentilis        :0   Mean   :1998  
    ##  Accipiter striatus        :0   3rd Qu.:2005  
    ##  Actitis macularius        :0   Max.   :2011  
    ##  (Other)                   :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-207.png)<!-- -->

    ##                     species    eventDate   
    ##  Calamospiza melanocorys:4   Min.   :1990  
    ##  Acanthis flammea       :0   1st Qu.:2003  
    ##  Accipiter cooperii     :0   Median :2008  
    ##  Accipiter gentilis     :0   Mean   :2006  
    ##  Accipiter striatus     :0   3rd Qu.:2011  
    ##  Actitis macularius     :0   Max.   :2017  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-208.png)<!-- -->

    ##                species     eventDate   
    ##  Melospiza melodia :21   Min.   :1986  
    ##  Acanthis flammea  : 0   1st Qu.:1995  
    ##  Accipiter cooperii: 0   Median :2014  
    ##  Accipiter gentilis: 0   Mean   :2007  
    ##  Accipiter striatus: 0   3rd Qu.:2017  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-209.png)<!-- -->

    ##                 species     eventDate   
    ##  Melospiza lincolnii:16   Min.   :1985  
    ##  Acanthis flammea   : 0   1st Qu.:1993  
    ##  Accipiter cooperii : 0   Median :2010  
    ##  Accipiter gentilis : 0   Mean   :2006  
    ##  Accipiter striatus : 0   3rd Qu.:2016  
    ##  Actitis macularius : 0   Max.   :2021  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-210.png)<!-- -->

    ##                 species    eventDate   
    ##  Melospiza georgiana:2   Min.   :1987  
    ##  Acanthis flammea   :0   1st Qu.:1996  
    ##  Accipiter cooperii :0   Median :2004  
    ##  Accipiter gentilis :0   Mean   :2004  
    ##  Accipiter striatus :0   3rd Qu.:2012  
    ##  Actitis macularius :0   Max.   :2021  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-211.png)<!-- -->

    ##                species     eventDate   
    ##  Icterus spurius   :37   Min.   :1957  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2014  
    ##  Accipiter striatus: 0   3rd Qu.:2018  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-9-212.png)<!-- -->

## MacKelvie

``` r
districtR(ne_nf_gbif = ne_nf_gbif,district = "MacKelvie")
```

![](appendix_1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

    ##                species    eventDate   
    ##  Branta hutchinsii :2   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

    ##                species     eventDate   
    ##  Branta canadensis :41   Min.   :2009  
    ##  Acanthis flammea  : 0   1st Qu.:2017  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

    ##                species     eventDate   
    ##  Cygnus buccinator :19   Min.   :2009  
    ##  Acanthis flammea  : 0   1st Qu.:2019  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

    ##                species     eventDate   
    ##  Spatula discors   :56   Min.   :2015  
    ##  Acanthis flammea  : 0   1st Qu.:2018  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-5.png)<!-- -->

    ##                species     eventDate   
    ##  Spatula clypeata  :16   Min.   :2016  
    ##  Acanthis flammea  : 0   1st Qu.:2020  
    ##  Accipiter cooperii: 0   Median :2021  
    ##  Accipiter gentilis: 0   Mean   :2021  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-6.png)<!-- -->

    ##                species     eventDate   
    ##  Mareca strepera   :21   Min.   :2015  
    ##  Acanthis flammea  : 0   1st Qu.:2020  
    ##  Accipiter cooperii: 0   Median :2021  
    ##  Accipiter gentilis: 0   Mean   :2020  
    ##  Accipiter striatus: 0   3rd Qu.:2023  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-7.png)<!-- -->

    ##                species     eventDate   
    ##  Mareca americana  :10   Min.   :2016  
    ##  Acanthis flammea  : 0   1st Qu.:2021  
    ##  Accipiter cooperii: 0   Median :2021  
    ##  Accipiter gentilis: 0   Mean   :2020  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-8.png)<!-- -->

    ##                species     eventDate   
    ##  Anas platyrhynchos:87   Min.   :2009  
    ##  Acanthis flammea  : 0   1st Qu.:2018  
    ##  Accipiter cooperii: 0   Median :2021  
    ##  Accipiter gentilis: 0   Mean   :2020  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-9.png)<!-- -->

    ##                species     eventDate   
    ##  Anas acuta        :15   Min.   :2016  
    ##  Acanthis flammea  : 0   1st Qu.:2020  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2020  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-10.png)<!-- -->

    ##                species     eventDate   
    ##  Anas crecca       :14   Min.   :2016  
    ##  Acanthis flammea  : 0   1st Qu.:2020  
    ##  Accipiter cooperii: 0   Median :2021  
    ##  Accipiter gentilis: 0   Mean   :2021  
    ##  Accipiter striatus: 0   3rd Qu.:2023  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-11.png)<!-- -->

    ##                species    eventDate   
    ##  Aythya valisineria:5   Min.   :2019  
    ##  Acanthis flammea  :0   1st Qu.:2019  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-12.png)<!-- -->

    ##                species     eventDate   
    ##  Aythya americana  :11   Min.   :2019  
    ##  Acanthis flammea  : 0   1st Qu.:2020  
    ##  Accipiter cooperii: 0   Median :2022  
    ##  Accipiter gentilis: 0   Mean   :2021  
    ##  Accipiter striatus: 0   3rd Qu.:2023  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-13.png)<!-- -->

    ##                species     eventDate   
    ##  Aythya collaris   :20   Min.   :2018  
    ##  Acanthis flammea  : 0   1st Qu.:2019  
    ##  Accipiter cooperii: 0   Median :2021  
    ##  Accipiter gentilis: 0   Mean   :2021  
    ##  Accipiter striatus: 0   3rd Qu.:2023  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-14.png)<!-- -->

    ##                species    eventDate   
    ##  Aythya marila     :1   Min.   :2023  
    ##  Acanthis flammea  :0   1st Qu.:2023  
    ##  Accipiter cooperii:0   Median :2023  
    ##  Accipiter gentilis:0   Mean   :2023  
    ##  Accipiter striatus:0   3rd Qu.:2023  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-15.png)<!-- -->

    ##                species    eventDate   
    ##  Aythya affinis    :4   Min.   :2023  
    ##  Acanthis flammea  :0   1st Qu.:2023  
    ##  Accipiter cooperii:0   Median :2023  
    ##  Accipiter gentilis:0   Mean   :2023  
    ##  Accipiter striatus:0   3rd Qu.:2023  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-16.png)<!-- -->

    ##                species    eventDate   
    ##  Bucephala albeola :3   Min.   :2023  
    ##  Acanthis flammea  :0   1st Qu.:2023  
    ##  Accipiter cooperii:0   Median :2023  
    ##  Accipiter gentilis:0   Mean   :2023  
    ##  Accipiter striatus:0   3rd Qu.:2023  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-17.png)<!-- -->

    ##                species    eventDate   
    ##  Bucephala clangula:2   Min.   :2023  
    ##  Acanthis flammea  :0   1st Qu.:2023  
    ##  Accipiter cooperii:0   Median :2023  
    ##  Accipiter gentilis:0   Mean   :2023  
    ##  Accipiter striatus:0   3rd Qu.:2023  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-18.png)<!-- -->

    ##                   species    eventDate   
    ##  Lophodytes cucullatus:5   Min.   :2012  
    ##  Acanthis flammea     :0   1st Qu.:2012  
    ##  Accipiter cooperii   :0   Median :2015  
    ##  Accipiter gentilis   :0   Mean   :2015  
    ##  Accipiter striatus   :0   3rd Qu.:2015  
    ##  Actitis macularius   :0   Max.   :2023  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-19.png)<!-- -->

    ##                species    eventDate   
    ##  Oxyura jamaicensis:4   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-20.png)<!-- -->

    ##                 species     eventDate   
    ##  Colinus virginianus:10   Min.   :2014  
    ##  Acanthis flammea   : 0   1st Qu.:2020  
    ##  Accipiter cooperii : 0   Median :2022  
    ##  Accipiter gentilis : 0   Mean   :2020  
    ##  Accipiter striatus : 0   3rd Qu.:2022  
    ##  Actitis macularius : 0   Max.   :2022  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-21.png)<!-- -->

    ##                 species     eventDate   
    ##  Meleagris gallopavo:15   Min.   :2009  
    ##  Acanthis flammea   : 0   1st Qu.:2014  
    ##  Accipiter cooperii : 0   Median :2015  
    ##  Accipiter gentilis : 0   Mean   :2015  
    ##  Accipiter striatus : 0   3rd Qu.:2016  
    ##  Actitis macularius : 0   Max.   :2022  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-22.png)<!-- -->

    ##                      species     eventDate   
    ##  Tympanuchus phasianellus:19   Min.   :1996  
    ##  Acanthis flammea        : 0   1st Qu.:2015  
    ##  Accipiter cooperii      : 0   Median :2016  
    ##  Accipiter gentilis      : 0   Mean   :2016  
    ##  Accipiter striatus      : 0   3rd Qu.:2018  
    ##  Actitis macularius      : 0   Max.   :2022  
    ##  (Other)                 : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-23.png)<!-- -->

    ##                species    eventDate   
    ##  Tympanuchus cupido:9   Min.   :2015  
    ##  Acanthis flammea  :0   1st Qu.:2015  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-24.png)<!-- -->

    ##                 species     eventDate   
    ##  Phasianus colchicus:14   Min.   :2011  
    ##  Acanthis flammea   : 0   1st Qu.:2015  
    ##  Accipiter cooperii : 0   Median :2016  
    ##  Accipiter gentilis : 0   Mean   :2017  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-25.png)<!-- -->

    ##                 species     eventDate   
    ##  Podilymbus podiceps:26   Min.   :2011  
    ##  Acanthis flammea   : 0   1st Qu.:2018  
    ##  Accipiter cooperii : 0   Median :2020  
    ##  Accipiter gentilis : 0   Mean   :2019  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2022  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-26.png)<!-- -->

    ##                  species    eventDate   
    ##  Podiceps nigricollis:5   Min.   :2021  
    ##  Acanthis flammea    :0   1st Qu.:2021  
    ##  Accipiter cooperii  :0   Median :2021  
    ##  Accipiter gentilis  :0   Mean   :2021  
    ##  Accipiter striatus  :0   3rd Qu.:2021  
    ##  Actitis macularius  :0   Max.   :2023  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-27.png)<!-- -->

    ##                       species    eventDate   
    ##  Aechmophorus occidentalis:1   Min.   :2023  
    ##  Acanthis flammea         :0   1st Qu.:2023  
    ##  Accipiter cooperii       :0   Median :2023  
    ##  Accipiter gentilis       :0   Mean   :2023  
    ##  Accipiter striatus       :0   3rd Qu.:2023  
    ##  Actitis macularius       :0   Max.   :2023  
    ##  (Other)                  :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-28.png)<!-- -->

    ##                species      eventDate   
    ##  Zenaida macroura  :123   Min.   :2009  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2019  
    ##  Accipiter gentilis:  0   Mean   :2018  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-29.png)<!-- -->

    ##                 species    eventDate   
    ##  Coccyzus americanus:5   Min.   :2011  
    ##  Acanthis flammea   :0   1st Qu.:2011  
    ##  Accipiter cooperii :0   Median :2014  
    ##  Accipiter gentilis :0   Mean   :2014  
    ##  Accipiter striatus :0   3rd Qu.:2014  
    ##  Actitis macularius :0   Max.   :2022  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-30.png)<!-- -->

    ##                species     eventDate   
    ##  Chordeiles minor  :63   Min.   :2009  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2019  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-31.png)<!-- -->

    ##                      species    eventDate   
    ##  Phalaenoptilus nuttallii:7   Min.   :2015  
    ##  Acanthis flammea        :0   1st Qu.:2018  
    ##  Accipiter cooperii      :0   Median :2021  
    ##  Accipiter gentilis      :0   Mean   :2020  
    ##  Accipiter striatus      :0   3rd Qu.:2022  
    ##  Actitis macularius      :0   Max.   :2023  
    ##  (Other)                 :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-32.png)<!-- -->

    ##                species    eventDate   
    ##  Chaetura pelagica :2   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-33.png)<!-- -->

    ##                species    eventDate   
    ##  Rallus limicola   :5   Min.   :2014  
    ##  Acanthis flammea  :0   1st Qu.:2014  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2017  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-34.png)<!-- -->

    ##                species     eventDate   
    ##  Porzana carolina  :10   Min.   :2015  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-35.png)<!-- -->

    ##                species     eventDate   
    ##  Fulica americana  :15   Min.   :2016  
    ##  Acanthis flammea  : 0   1st Qu.:2018  
    ##  Accipiter cooperii: 0   Median :2019  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-36.png)<!-- -->

    ##                 species    eventDate   
    ##  Antigone canadensis:4   Min.   :2015  
    ##  Acanthis flammea   :0   1st Qu.:2015  
    ##  Accipiter cooperii :0   Median :2016  
    ##  Accipiter gentilis :0   Mean   :2016  
    ##  Accipiter striatus :0   3rd Qu.:2016  
    ##  Actitis macularius :0   Max.   :2016  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-37.png)<!-- -->

    ##                  species    eventDate   
    ##  Himantopus mexicanus:2   Min.   :2020  
    ##  Acanthis flammea    :0   1st Qu.:2020  
    ##  Accipiter cooperii  :0   Median :2020  
    ##  Accipiter gentilis  :0   Mean   :2020  
    ##  Accipiter striatus  :0   3rd Qu.:2020  
    ##  Actitis macularius  :0   Max.   :2020  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-38.png)<!-- -->

    ##                  species     eventDate   
    ##  Charadrius vociferus:60   Min.   :2012  
    ##  Acanthis flammea    : 0   1st Qu.:2015  
    ##  Accipiter cooperii  : 0   Median :2019  
    ##  Accipiter gentilis  : 0   Mean   :2019  
    ##  Accipiter striatus  : 0   3rd Qu.:2021  
    ##  Actitis macularius  : 0   Max.   :2023  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-39.png)<!-- -->

    ##                  species     eventDate   
    ##  Bartramia longicauda:65   Min.   :2009  
    ##  Acanthis flammea    : 0   1st Qu.:2015  
    ##  Accipiter cooperii  : 0   Median :2019  
    ##  Accipiter gentilis  : 0   Mean   :2018  
    ##  Accipiter striatus  : 0   3rd Qu.:2021  
    ##  Actitis macularius  : 0   Max.   :2023  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-40.png)<!-- -->

    ##                 species     eventDate   
    ##  Numenius americanus:16   Min.   :2016  
    ##  Acanthis flammea   : 0   1st Qu.:2018  
    ##  Accipiter cooperii : 0   Median :2020  
    ##  Accipiter gentilis : 0   Mean   :2020  
    ##  Accipiter striatus : 0   3rd Qu.:2023  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-41.png)<!-- -->

    ##                species    eventDate   
    ##  Limosa fedoa      :1   Min.   :2023  
    ##  Acanthis flammea  :0   1st Qu.:2023  
    ##  Accipiter cooperii:0   Median :2023  
    ##  Accipiter gentilis:0   Mean   :2023  
    ##  Accipiter striatus:0   3rd Qu.:2023  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-42.png)<!-- -->

    ##                  species    eventDate   
    ##  Calidris fuscicollis:1   Min.   :2022  
    ##  Acanthis flammea    :0   1st Qu.:2022  
    ##  Accipiter cooperii  :0   Median :2022  
    ##  Accipiter gentilis  :0   Mean   :2022  
    ##  Accipiter striatus  :0   3rd Qu.:2022  
    ##  Actitis macularius  :0   Max.   :2022  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-43.png)<!-- -->

    ##                species    eventDate   
    ##  Calidris pusilla  :1   Min.   :2022  
    ##  Acanthis flammea  :0   1st Qu.:2022  
    ##  Accipiter cooperii:0   Median :2022  
    ##  Accipiter gentilis:0   Mean   :2022  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-44.png)<!-- -->

    ##                     species    eventDate   
    ##  Limnodromus scolopaceus:1   Min.   :2023  
    ##  Acanthis flammea       :0   1st Qu.:2023  
    ##  Accipiter cooperii     :0   Median :2023  
    ##  Accipiter gentilis     :0   Mean   :2023  
    ##  Accipiter striatus     :0   3rd Qu.:2023  
    ##  Actitis macularius     :0   Max.   :2023  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-45.png)<!-- -->

    ##                species     eventDate   
    ##  Gallinago delicata:10   Min.   :2014  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-46.png)<!-- -->

    ##                 species     eventDate   
    ##  Phalaropus tricolor:15   Min.   :2015  
    ##  Acanthis flammea   : 0   1st Qu.:2016  
    ##  Accipiter cooperii : 0   Median :2017  
    ##  Accipiter gentilis : 0   Mean   :2019  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-47.png)<!-- -->

    ##                  species    eventDate   
    ##  Actitis macularius  :6   Min.   :2021  
    ##  Acanthis flammea    :0   1st Qu.:2021  
    ##  Accipiter cooperii  :0   Median :2021  
    ##  Accipiter gentilis  :0   Mean   :2021  
    ##  Accipiter striatus  :0   3rd Qu.:2021  
    ##  Aechmophorus clarkii:0   Max.   :2022  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-48.png)<!-- -->

    ##                species    eventDate   
    ##  Tringa solitaria  :2   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2022  
    ##  Accipiter cooperii:0   Median :2022  
    ##  Accipiter gentilis:0   Mean   :2022  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-49.png)<!-- -->

    ##                species    eventDate   
    ##  Tringa melanoleuca:1   Min.   :2023  
    ##  Acanthis flammea  :0   1st Qu.:2023  
    ##  Accipiter cooperii:0   Median :2023  
    ##  Accipiter gentilis:0   Mean   :2023  
    ##  Accipiter striatus:0   3rd Qu.:2023  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-50.png)<!-- -->

    ##                species    eventDate   
    ##  Tringa semipalmata:3   Min.   :2019  
    ##  Acanthis flammea  :0   1st Qu.:2019  
    ##  Accipiter cooperii:0   Median :2019  
    ##  Accipiter gentilis:0   Mean   :2020  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-51.png)<!-- -->

    ##                species    eventDate   
    ##  Tringa flavipes   :4   Min.   :2016  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2019  
    ##  Actitis macularius:0   Max.   :2019  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-52.png)<!-- -->

    ##                species    eventDate   
    ##  Larus delawarensis:5   Min.   :2018  
    ##  Acanthis flammea  :0   1st Qu.:2018  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2020  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-53.png)<!-- -->

    ##                species    eventDate   
    ##  Chlidonias niger  :2   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2022  
    ##  Accipiter gentilis:0   Mean   :2022  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-54.png)<!-- -->

    ##                species    eventDate   
    ##  Sterna forsteri   :3   Min.   :2017  
    ##  Acanthis flammea  :0   1st Qu.:2017  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2019  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-55.png)<!-- -->

    ##                 species    eventDate   
    ##  Nannopterum auritum:9   Min.   :2014  
    ##  Acanthis flammea   :0   1st Qu.:2015  
    ##  Accipiter cooperii :0   Median :2017  
    ##  Accipiter gentilis :0   Mean   :2018  
    ##  Accipiter striatus :0   3rd Qu.:2021  
    ##  Actitis macularius :0   Max.   :2023  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-56.png)<!-- -->

    ##                       species     eventDate   
    ##  Pelecanus erythrorhynchos:21   Min.   :2012  
    ##  Acanthis flammea         : 0   1st Qu.:2015  
    ##  Accipiter cooperii       : 0   Median :2019  
    ##  Accipiter gentilis       : 0   Mean   :2018  
    ##  Accipiter striatus       : 0   3rd Qu.:2021  
    ##  Actitis macularius       : 0   Max.   :2023  
    ##  (Other)                  : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-57.png)<!-- -->

    ##                   species     eventDate   
    ##  Botaurus lentiginosus:20   Min.   :2016  
    ##  Acanthis flammea     : 0   1st Qu.:2018  
    ##  Accipiter cooperii   : 0   Median :2020  
    ##  Accipiter gentilis   : 0   Mean   :2020  
    ##  Accipiter striatus   : 0   3rd Qu.:2021  
    ##  Actitis macularius   : 0   Max.   :2023  
    ##  (Other)              : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-58.png)<!-- -->

    ##                species     eventDate   
    ##  Ardea herodias    :34   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-59.png)<!-- -->

    ##                species    eventDate   
    ##  Bubulcus ibis     :3   Min.   :2020  
    ##  Acanthis flammea  :0   1st Qu.:2020  
    ##  Accipiter cooperii:0   Median :2020  
    ##  Accipiter gentilis:0   Mean   :2020  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2020  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-60.png)<!-- -->

    ##                  species    eventDate   
    ##  Plegadis falcinellus:1   Min.   :2022  
    ##  Acanthis flammea    :0   1st Qu.:2022  
    ##  Accipiter cooperii  :0   Median :2022  
    ##  Accipiter gentilis  :0   Mean   :2022  
    ##  Accipiter striatus  :0   3rd Qu.:2022  
    ##  Actitis macularius  :0   Max.   :2022  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-61.png)<!-- -->

    ##                species    eventDate   
    ##  Plegadis chihi    :5   Min.   :2019  
    ##  Acanthis flammea  :0   1st Qu.:2019  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2020  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-62.png)<!-- -->

    ##                        species    eventDate   
    ##  Plegadis falcinellus/chihi:1   Min.   :2022  
    ##  Acanthis flammea          :0   1st Qu.:2022  
    ##  Accipiter cooperii        :0   Median :2022  
    ##  Accipiter gentilis        :0   Mean   :2022  
    ##  Accipiter striatus        :0   3rd Qu.:2022  
    ##  Actitis macularius        :0   Max.   :2022  
    ##  (Other)                   :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-63.png)<!-- -->

    ##                species     eventDate   
    ##  Cathartes aura    :46   Min.   :2013  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2019  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-64.png)<!-- -->

    ##                species     eventDate   
    ##  Circus hudsonius  :11   Min.   :2013  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2016  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-65.png)<!-- -->

    ##                  species    eventDate   
    ##  Accipiter cooperii  :8   Min.   :2013  
    ##  Acanthis flammea    :0   1st Qu.:2014  
    ##  Accipiter gentilis  :0   Median :2015  
    ##  Accipiter striatus  :0   Mean   :2016  
    ##  Actitis macularius  :0   3rd Qu.:2017  
    ##  Aechmophorus clarkii:0   Max.   :2020  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-66.png)<!-- -->

    ##                      species    eventDate   
    ##  Haliaeetus leucocephalus:6   Min.   :2015  
    ##  Acanthis flammea        :0   1st Qu.:2015  
    ##  Accipiter cooperii      :0   Median :2016  
    ##  Accipiter gentilis      :0   Mean   :2018  
    ##  Accipiter striatus      :0   3rd Qu.:2021  
    ##  Actitis macularius      :0   Max.   :2023  
    ##  (Other)                 :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-67.png)<!-- -->

    ##                species     eventDate   
    ##  Buteo swainsoni   :23   Min.   :2009  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-68.png)<!-- -->

    ##                species     eventDate   
    ##  Buteo jamaicensis :40   Min.   :2009  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-69.png)<!-- -->

    ##                species    eventDate   
    ##  Buteo regalis     :2   Min.   :2009  
    ##  Acanthis flammea  :0   1st Qu.:2009  
    ##  Accipiter cooperii:0   Median :2009  
    ##  Accipiter gentilis:0   Mean   :2009  
    ##  Accipiter striatus:0   3rd Qu.:2009  
    ##  Actitis macularius:0   Max.   :2009  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-70.png)<!-- -->

    ##                species    eventDate   
    ##  Megascops asio    :2   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2011  
    ##  Accipiter cooperii:0   Median :2011  
    ##  Accipiter gentilis:0   Mean   :2011  
    ##  Accipiter striatus:0   3rd Qu.:2011  
    ##  Actitis macularius:0   Max.   :2011  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-71.png)<!-- -->

    ##                species     eventDate   
    ##  Bubo virginianus  :23   Min.   :2009  
    ##  Acanthis flammea  : 0   1st Qu.:2013  
    ##  Accipiter cooperii: 0   Median :2013  
    ##  Accipiter gentilis: 0   Mean   :2015  
    ##  Accipiter striatus: 0   3rd Qu.:2018  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-72.png)<!-- -->

    ##                species    eventDate   
    ##  Athene cunicularia:2   Min.   :2018  
    ##  Acanthis flammea  :0   1st Qu.:2018  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2018  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-73.png)<!-- -->

    ##                species    eventDate   
    ##  Megaceryle alcyon :1   Min.   :2022  
    ##  Acanthis flammea  :0   1st Qu.:2022  
    ##  Accipiter cooperii:0   Median :2022  
    ##  Accipiter gentilis:0   Mean   :2022  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-74.png)<!-- -->

    ##                        species     eventDate   
    ##  Melanerpes erythrocephalus:26   Min.   :2011  
    ##  Acanthis flammea          : 0   1st Qu.:2015  
    ##  Accipiter cooperii        : 0   Median :2018  
    ##  Accipiter gentilis        : 0   Mean   :2018  
    ##  Accipiter striatus        : 0   3rd Qu.:2021  
    ##  Actitis macularius        : 0   Max.   :2023  
    ##  (Other)                   : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-75.png)<!-- -->

    ##                  species    eventDate   
    ##  Melanerpes carolinus:2   Min.   :2012  
    ##  Acanthis flammea    :0   1st Qu.:2012  
    ##  Accipiter cooperii  :0   Median :2012  
    ##  Accipiter gentilis  :0   Mean   :2012  
    ##  Accipiter striatus  :0   3rd Qu.:2012  
    ##  Actitis macularius  :0   Max.   :2012  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-76.png)<!-- -->

    ##                 species     eventDate   
    ##  Dryobates pubescens:32   Min.   :2011  
    ##  Acanthis flammea   : 0   1st Qu.:2014  
    ##  Accipiter cooperii : 0   Median :2020  
    ##  Accipiter gentilis : 0   Mean   :2018  
    ##  Accipiter striatus : 0   3rd Qu.:2022  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-77.png)<!-- -->

    ##                species     eventDate   
    ##  Dryobates villosus:18   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2020  
    ##  Accipiter cooperii: 0   Median :2021  
    ##  Accipiter gentilis: 0   Mean   :2020  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-78.png)<!-- -->

    ##                species     eventDate   
    ##  Colaptes auratus  :35   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2013  
    ##  Accipiter cooperii: 0   Median :2014  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-79.png)<!-- -->

    ##                species    eventDate   
    ##  Falco columbarius :2   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2013  
    ##  Accipiter gentilis:0   Mean   :2013  
    ##  Accipiter striatus:0   3rd Qu.:2013  
    ##  Actitis macularius:0   Max.   :2013  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-80.png)<!-- -->

    ##                 species     eventDate   
    ##  Contopus sordidulus:50   Min.   :2011  
    ##  Acanthis flammea   : 0   1st Qu.:2016  
    ##  Accipiter cooperii : 0   Median :2021  
    ##  Accipiter gentilis : 0   Mean   :2019  
    ##  Accipiter striatus : 0   3rd Qu.:2022  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-81.png)<!-- -->

    ##                species    eventDate   
    ##  Contopus virens   :4   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2015  
    ##  Accipiter gentilis:0   Mean   :2015  
    ##  Accipiter striatus:0   3rd Qu.:2017  
    ##  Actitis macularius:0   Max.   :2017  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-82.png)<!-- -->

    ##                        species    eventDate   
    ##  Contopus sordidulus/virens:2   Min.   :2022  
    ##  Acanthis flammea          :0   1st Qu.:2022  
    ##  Accipiter cooperii        :0   Median :2022  
    ##  Accipiter gentilis        :0   Mean   :2022  
    ##  Accipiter striatus        :0   3rd Qu.:2022  
    ##  Actitis macularius        :0   Max.   :2022  
    ##  (Other)                   :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-83.png)<!-- -->

    ##                species     eventDate   
    ##  Empidonax minimus :10   Min.   :2013  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-84.png)<!-- -->

    ##                species    eventDate   
    ##  Sayornis phoebe   :1   Min.   :2023  
    ##  Acanthis flammea  :0   1st Qu.:2023  
    ##  Accipiter cooperii:0   Median :2023  
    ##  Accipiter gentilis:0   Mean   :2023  
    ##  Accipiter striatus:0   3rd Qu.:2023  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-85.png)<!-- -->

    ##                species    eventDate   
    ##  Sayornis saya     :2   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2013  
    ##  Accipiter gentilis:0   Mean   :2013  
    ##  Accipiter striatus:0   3rd Qu.:2013  
    ##  Actitis macularius:0   Max.   :2013  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-86.png)<!-- -->

    ##                species     eventDate   
    ##  Myiarchus crinitus:37   Min.   :2012  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-87.png)<!-- -->

    ##                 species     eventDate   
    ##  Tyrannus verticalis:40   Min.   :2011  
    ##  Acanthis flammea   : 0   1st Qu.:2015  
    ##  Accipiter cooperii : 0   Median :2017  
    ##  Accipiter gentilis : 0   Mean   :2017  
    ##  Accipiter striatus : 0   3rd Qu.:2020  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-88.png)<!-- -->

    ##                species     eventDate   
    ##  Tyrannus tyrannus :90   Min.   :2009  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2019  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-89.png)<!-- -->

    ##                species     eventDate   
    ##  Vireo bellii      :30   Min.   :2012  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2016  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-90.png)<!-- -->

    ##                species    eventDate   
    ##  Vireo solitarius  :2   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2013  
    ##  Accipiter gentilis:0   Mean   :2013  
    ##  Accipiter striatus:0   3rd Qu.:2013  
    ##  Actitis macularius:0   Max.   :2013  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-91.png)<!-- -->

    ##                species    eventDate   
    ##  Vireo gilvus      :5   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2016  
    ##  Accipiter striatus:0   3rd Qu.:2017  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-92.png)<!-- -->

    ##                species     eventDate   
    ##  Vireo olivaceus   :11   Min.   :2013  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2018  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-93.png)<!-- -->

    ##                 species     eventDate   
    ##  Lanius ludovicianus:12   Min.   :2009  
    ##  Acanthis flammea   : 0   1st Qu.:2011  
    ##  Accipiter cooperii : 0   Median :2018  
    ##  Accipiter gentilis : 0   Mean   :2016  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-94.png)<!-- -->

    ##                species    eventDate   
    ##  Lanius borealis   :2   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2013  
    ##  Accipiter gentilis:0   Mean   :2013  
    ##  Accipiter striatus:0   3rd Qu.:2013  
    ##  Actitis macularius:0   Max.   :2013  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-95.png)<!-- -->

    ##                 species     eventDate   
    ##  Cyanocitta cristata:33   Min.   :2011  
    ##  Acanthis flammea   : 0   1st Qu.:2015  
    ##  Accipiter cooperii : 0   Median :2021  
    ##  Accipiter gentilis : 0   Mean   :2019  
    ##  Accipiter striatus : 0   3rd Qu.:2022  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-96.png)<!-- -->

    ##                species    eventDate   
    ##  Pica hudsonia     :2   Min.   :2015  
    ##  Acanthis flammea  :0   1st Qu.:2015  
    ##  Accipiter cooperii:0   Median :2015  
    ##  Accipiter gentilis:0   Mean   :2015  
    ##  Accipiter striatus:0   3rd Qu.:2015  
    ##  Actitis macularius:0   Max.   :2015  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-97.png)<!-- -->

    ##                   species     eventDate   
    ##  Corvus brachyrhynchos:79   Min.   :2011  
    ##  Acanthis flammea     : 0   1st Qu.:2014  
    ##  Accipiter cooperii   : 0   Median :2020  
    ##  Accipiter gentilis   : 0   Mean   :2018  
    ##  Accipiter striatus   : 0   3rd Qu.:2021  
    ##  Actitis macularius   : 0   Max.   :2023  
    ##  (Other)              : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-98.png)<!-- -->

    ##                  species     eventDate   
    ##  Poecile atricapillus:69   Min.   :2009  
    ##  Acanthis flammea    : 0   1st Qu.:2013  
    ##  Accipiter cooperii  : 0   Median :2017  
    ##  Accipiter gentilis  : 0   Mean   :2017  
    ##  Accipiter striatus  : 0   3rd Qu.:2021  
    ##  Actitis macularius  : 0   Max.   :2023  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-99.png)<!-- -->

    ##                  species     eventDate   
    ##  Eremophila alpestris:57   Min.   :2011  
    ##  Acanthis flammea    : 0   1st Qu.:2015  
    ##  Accipiter cooperii  : 0   Median :2019  
    ##  Accipiter gentilis  : 0   Mean   :2018  
    ##  Accipiter striatus  : 0   3rd Qu.:2021  
    ##  Actitis macularius  : 0   Max.   :2023  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-100.png)<!-- -->

    ##                        species     eventDate   
    ##  Stelgidopteryx serripennis:10   Min.   :2015  
    ##  Acanthis flammea          : 0   1st Qu.:2015  
    ##  Accipiter cooperii        : 0   Median :2016  
    ##  Accipiter gentilis        : 0   Mean   :2018  
    ##  Accipiter striatus        : 0   3rd Qu.:2019  
    ##  Actitis macularius        : 0   Max.   :2023  
    ##  (Other)                   : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-101.png)<!-- -->

    ##                species    eventDate   
    ##  Progne subis      :1   Min.   :2023  
    ##  Acanthis flammea  :0   1st Qu.:2023  
    ##  Accipiter cooperii:0   Median :2023  
    ##  Accipiter gentilis:0   Mean   :2023  
    ##  Accipiter striatus:0   3rd Qu.:2023  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-102.png)<!-- -->

    ##                 species     eventDate   
    ##  Tachycineta bicolor:24   Min.   :2015  
    ##  Acanthis flammea   : 0   1st Qu.:2018  
    ##  Accipiter cooperii : 0   Median :2020  
    ##  Accipiter gentilis : 0   Mean   :2019  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-103.png)<!-- -->

    ##                species    eventDate   
    ##  Riparia riparia   :8   Min.   :2015  
    ##  Acanthis flammea  :0   1st Qu.:2020  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2020  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-104.png)<!-- -->

    ##                species     eventDate   
    ##  Hirundo rustica   :37   Min.   :2013  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-105.png)<!-- -->

    ##                      species     eventDate   
    ##  Petrochelidon pyrrhonota:10   Min.   :2015  
    ##  Acanthis flammea        : 0   1st Qu.:2017  
    ##  Accipiter cooperii      : 0   Median :2020  
    ##  Accipiter gentilis      : 0   Mean   :2019  
    ##  Accipiter striatus      : 0   3rd Qu.:2022  
    ##  Actitis macularius      : 0   Max.   :2023  
    ##  (Other)                 : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-106.png)<!-- -->

    ##                 species    eventDate   
    ##  Corthylio calendula:5   Min.   :2013  
    ##  Acanthis flammea   :0   1st Qu.:2013  
    ##  Accipiter cooperii :0   Median :2013  
    ##  Accipiter gentilis :0   Mean   :2015  
    ##  Accipiter striatus :0   3rd Qu.:2013  
    ##  Actitis macularius :0   Max.   :2023  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-107.png)<!-- -->

    ##                species     eventDate   
    ##  Sitta canadensis  :41   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-108.png)<!-- -->

    ##                species     eventDate   
    ##  Sitta carolinensis:14   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2012  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-109.png)<!-- -->

    ##                species    eventDate   
    ##  Sitta pygmaea     :5   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2011  
    ##  Accipiter cooperii:0   Median :2011  
    ##  Accipiter gentilis:0   Mean   :2013  
    ##  Accipiter striatus:0   3rd Qu.:2011  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-110.png)<!-- -->

    ##                species    eventDate   
    ##  Certhia americana :4   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2011  
    ##  Accipiter cooperii:0   Median :2012  
    ##  Accipiter gentilis:0   Mean   :2012  
    ##  Accipiter striatus:0   3rd Qu.:2014  
    ##  Actitis macularius:0   Max.   :2014  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-111.png)<!-- -->

    ##                 species    eventDate   
    ##  Polioptila caerulea:4   Min.   :2021  
    ##  Acanthis flammea   :0   1st Qu.:2021  
    ##  Accipiter cooperii :0   Median :2022  
    ##  Accipiter gentilis :0   Mean   :2022  
    ##  Accipiter striatus :0   3rd Qu.:2022  
    ##  Actitis macularius :0   Max.   :2023  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-112.png)<!-- -->

    ##                species     eventDate   
    ##  Troglodytes aedon :63   Min.   :2009  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-113.png)<!-- -->

    ##                   species    eventDate   
    ##  Cistothorus stellaris:1   Min.   :2022  
    ##  Acanthis flammea     :0   1st Qu.:2022  
    ##  Accipiter cooperii   :0   Median :2022  
    ##  Accipiter gentilis   :0   Mean   :2022  
    ##  Accipiter striatus   :0   3rd Qu.:2022  
    ##  Actitis macularius   :0   Max.   :2022  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-114.png)<!-- -->

    ##                   species    eventDate   
    ##  Cistothorus palustris:2   Min.   :2015  
    ##  Acanthis flammea     :0   1st Qu.:2015  
    ##  Accipiter cooperii   :0   Median :2015  
    ##  Accipiter gentilis   :0   Mean   :2015  
    ##  Accipiter striatus   :0   3rd Qu.:2015  
    ##  Actitis macularius   :0   Max.   :2015  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-115.png)<!-- -->

    ##                species    eventDate   
    ##  Sturnus vulgaris  :5   Min.   :2019  
    ##  Acanthis flammea  :0   1st Qu.:2019  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2020  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-116.png)<!-- -->

    ##                    species     eventDate   
    ##  Dumetella carolinensis:10   Min.   :2013  
    ##  Acanthis flammea      : 0   1st Qu.:2013  
    ##  Accipiter cooperii    : 0   Median :2013  
    ##  Accipiter gentilis    : 0   Mean   :2016  
    ##  Accipiter striatus    : 0   3rd Qu.:2021  
    ##  Actitis macularius    : 0   Max.   :2021  
    ##  (Other)               : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-117.png)<!-- -->

    ##                species     eventDate   
    ##  Toxostoma rufum   :35   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2013  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-118.png)<!-- -->

    ##                species    eventDate   
    ##  Mimus polyglottos :2   Min.   :2018  
    ##  Acanthis flammea  :0   1st Qu.:2018  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2018  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-119.png)<!-- -->

    ##                species     eventDate   
    ##  Sialia sialis     :18   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-120.png)<!-- -->

    ##                 species    eventDate   
    ##  Myadestes townsendi:6   Min.   :2013  
    ##  Acanthis flammea   :0   1st Qu.:2013  
    ##  Accipiter cooperii :0   Median :2013  
    ##  Accipiter gentilis :0   Mean   :2013  
    ##  Accipiter striatus :0   3rd Qu.:2013  
    ##  Actitis macularius :0   Max.   :2013  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-121.png)<!-- -->

    ##                species    eventDate   
    ##  Catharus ustulatus:3   Min.   :2022  
    ##  Acanthis flammea  :0   1st Qu.:2022  
    ##  Accipiter cooperii:0   Median :2022  
    ##  Accipiter gentilis:0   Mean   :2022  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-122.png)<!-- -->

    ##                species     eventDate   
    ##  Turdus migratorius:82   Min.   :2009  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2021  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-123.png)<!-- -->

    ##                 species     eventDate   
    ##  Bombycilla cedrorum:28   Min.   :2011  
    ##  Acanthis flammea   : 0   1st Qu.:2017  
    ##  Accipiter cooperii : 0   Median :2020  
    ##  Accipiter gentilis : 0   Mean   :2019  
    ##  Accipiter striatus : 0   3rd Qu.:2022  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-124.png)<!-- -->

    ##                species    eventDate   
    ##  Passer domesticus :2   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-125.png)<!-- -->

    ##                  species    eventDate   
    ##  Haemorhous mexicanus:3   Min.   :2016  
    ##  Acanthis flammea    :0   1st Qu.:2016  
    ##  Accipiter cooperii  :0   Median :2016  
    ##  Accipiter gentilis  :0   Mean   :2018  
    ##  Accipiter striatus  :0   3rd Qu.:2019  
    ##  Actitis macularius  :0   Max.   :2022  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-126.png)<!-- -->

    ##                species    eventDate   
    ##  Loxia curvirostra :8   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2011  
    ##  Accipiter cooperii:0   Median :2014  
    ##  Accipiter gentilis:0   Mean   :2016  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-127.png)<!-- -->

    ##                species    eventDate   
    ##  Spinus pinus      :4   Min.   :2016  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2020  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-128.png)<!-- -->

    ##                species     eventDate   
    ##  Spinus tristis    :83   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-129.png)<!-- -->

    ##                species    eventDate   
    ##  Calcarius ornatus :2   Min.   :2015  
    ##  Acanthis flammea  :0   1st Qu.:2015  
    ##  Accipiter cooperii:0   Median :2015  
    ##  Accipiter gentilis:0   Mean   :2015  
    ##  Accipiter striatus:0   3rd Qu.:2015  
    ##  Actitis macularius:0   Max.   :2015  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-130.png)<!-- -->

    ##                species    eventDate   
    ##  Peucaea cassinii  :1   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2013  
    ##  Accipiter gentilis:0   Mean   :2013  
    ##  Accipiter striatus:0   3rd Qu.:2013  
    ##  Actitis macularius:0   Max.   :2013  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-131.png)<!-- -->

    ##                   species     eventDate   
    ##  Ammodramus savannarum:76   Min.   :2011  
    ##  Acanthis flammea     : 0   1st Qu.:2015  
    ##  Accipiter cooperii   : 0   Median :2018  
    ##  Accipiter gentilis   : 0   Mean   :2017  
    ##  Accipiter striatus   : 0   3rd Qu.:2021  
    ##  Actitis macularius   : 0   Max.   :2023  
    ##  (Other)              : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-132.png)<!-- -->

    ##                species     eventDate   
    ##  Spizella passerina:62   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2013  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-133.png)<!-- -->

    ##                species    eventDate   
    ##  Spizella pallida  :8   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2016  
    ##  Accipiter gentilis:0   Mean   :2016  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2020  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-134.png)<!-- -->

    ##                species     eventDate   
    ##  Spizella pusilla  :61   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-135.png)<!-- -->

    ##                  species     eventDate   
    ##  Chondestes grammacus:97   Min.   :2009  
    ##  Acanthis flammea    : 0   1st Qu.:2015  
    ##  Accipiter cooperii  : 0   Median :2018  
    ##  Accipiter gentilis  : 0   Mean   :2018  
    ##  Accipiter striatus  : 0   3rd Qu.:2021  
    ##  Actitis macularius  : 0   Max.   :2023  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-136.png)<!-- -->

    ##                  species    eventDate   
    ##  Spizelloides arborea:9   Min.   :2013  
    ##  Acanthis flammea    :0   1st Qu.:2013  
    ##  Accipiter cooperii  :0   Median :2017  
    ##  Accipiter gentilis  :0   Mean   :2017  
    ##  Accipiter striatus  :0   3rd Qu.:2021  
    ##  Actitis macularius  :0   Max.   :2023  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-137.png)<!-- -->

    ##                species    eventDate   
    ##  Junco sp.         :8   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2016  
    ##  Accipiter gentilis:0   Mean   :2017  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-138.png)<!-- -->

    ##                species    eventDate   
    ##  Junco hyemalis    :2   Min.   :2020  
    ##  Acanthis flammea  :0   1st Qu.:2020  
    ##  Accipiter cooperii:0   Median :2020  
    ##  Accipiter gentilis:0   Mean   :2020  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2020  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-139.png)<!-- -->

    ##                    species    eventDate   
    ##  Zonotrichia leucophrys:7   Min.   :2013  
    ##  Acanthis flammea      :0   1st Qu.:2013  
    ##  Accipiter cooperii    :0   Median :2013  
    ##  Accipiter gentilis    :0   Mean   :2016  
    ##  Accipiter striatus    :0   3rd Qu.:2020  
    ##  Actitis macularius    :0   Max.   :2023  
    ##  (Other)               :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-140.png)<!-- -->

    ##                 species    eventDate   
    ##  Zonotrichia querula:4   Min.   :2013  
    ##  Acanthis flammea   :0   1st Qu.:2013  
    ##  Accipiter cooperii :0   Median :2013  
    ##  Accipiter gentilis :0   Mean   :2013  
    ##  Accipiter striatus :0   3rd Qu.:2013  
    ##  Actitis macularius :0   Max.   :2013  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-141.png)<!-- -->

    ##                    species    eventDate   
    ##  Zonotrichia albicollis:2   Min.   :2021  
    ##  Acanthis flammea      :0   1st Qu.:2021  
    ##  Accipiter cooperii    :0   Median :2021  
    ##  Accipiter gentilis    :0   Mean   :2021  
    ##  Accipiter striatus    :0   3rd Qu.:2021  
    ##  Actitis macularius    :0   Max.   :2021  
    ##  (Other)               :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-142.png)<!-- -->

    ##                 species     eventDate   
    ##  Pooecetes gramineus:10   Min.   :2013  
    ##  Acanthis flammea   : 0   1st Qu.:2013  
    ##  Accipiter cooperii : 0   Median :2013  
    ##  Accipiter gentilis : 0   Mean   :2014  
    ##  Accipiter striatus : 0   3rd Qu.:2015  
    ##  Actitis macularius : 0   Max.   :2015  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-143.png)<!-- -->

    ##                       species    eventDate   
    ##  Passerculus sandwichensis:7   Min.   :2016  
    ##  Acanthis flammea         :0   1st Qu.:2016  
    ##  Accipiter cooperii       :0   Median :2016  
    ##  Accipiter gentilis       :0   Mean   :2017  
    ##  Accipiter striatus       :0   3rd Qu.:2016  
    ##  Actitis macularius       :0   Max.   :2023  
    ##  (Other)                  :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-144.png)<!-- -->

    ##                species     eventDate   
    ##  Pipilo maculatus  :31   Min.   :2012  
    ##  Acanthis flammea  : 0   1st Qu.:2013  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-145.png)<!-- -->

    ##                               species    eventDate   
    ##  Pipilo maculatus/erythrophthalmus:2   Min.   :2020  
    ##  Acanthis flammea                 :0   1st Qu.:2021  
    ##  Accipiter cooperii               :0   Median :2022  
    ##  Accipiter gentilis               :0   Mean   :2022  
    ##  Accipiter striatus               :0   3rd Qu.:2022  
    ##  Actitis macularius               :0   Max.   :2023  
    ##  (Other)                          :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-146.png)<!-- -->

    ##                species    eventDate   
    ##  Icteria virens    :6   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2013  
    ##  Accipiter gentilis:0   Mean   :2014  
    ##  Accipiter striatus:0   3rd Qu.:2016  
    ##  Actitis macularius:0   Max.   :2017  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-147.png)<!-- -->

    ##                           species     eventDate   
    ##  Xanthocephalus xanthocephalus:33   Min.   :2015  
    ##  Acanthis flammea             : 0   1st Qu.:2019  
    ##  Accipiter cooperii           : 0   Median :2021  
    ##  Accipiter gentilis           : 0   Mean   :2020  
    ##  Accipiter striatus           : 0   3rd Qu.:2021  
    ##  Actitis macularius           : 0   Max.   :2023  
    ##  (Other)                      : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-148.png)<!-- -->

    ##                   species    eventDate   
    ##  Dolichonyx oryzivorus:9   Min.   :2011  
    ##  Acanthis flammea     :0   1st Qu.:2015  
    ##  Accipiter cooperii   :0   Median :2018  
    ##  Accipiter gentilis   :0   Mean   :2017  
    ##  Accipiter striatus   :0   3rd Qu.:2021  
    ##  Actitis macularius   :0   Max.   :2022  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-149.png)<!-- -->

    ##                species      eventDate   
    ##  Sturnella neglecta:112   Min.   :2011  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2018  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-150.png)<!-- -->

    ##                species    eventDate   
    ##  Sturnella magna   :5   Min.   :2016  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2019  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-151.png)<!-- -->

    ##                      species    eventDate   
    ##  Sturnella neglecta/magna:5   Min.   :2009  
    ##  Acanthis flammea        :0   1st Qu.:2012  
    ##  Accipiter cooperii      :0   Median :2012  
    ##  Accipiter gentilis      :0   Mean   :2013  
    ##  Accipiter striatus      :0   3rd Qu.:2014  
    ##  Actitis macularius      :0   Max.   :2018  
    ##  (Other)                 :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-152.png)<!-- -->

    ##                species    eventDate   
    ##  Icterus galbula   :7   Min.   :2016  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2019  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-153.png)<!-- -->

    ##                 species     eventDate   
    ##  Agelaius phoeniceus:88   Min.   :2009  
    ##  Acanthis flammea   : 0   1st Qu.:2017  
    ##  Accipiter cooperii : 0   Median :2020  
    ##  Accipiter gentilis : 0   Mean   :2019  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-154.png)<!-- -->

    ##                species     eventDate   
    ##  Molothrus ater    :72   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2019  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-155.png)<!-- -->

    ##                species     eventDate   
    ##  Quiscalus quiscula:36   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2021  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-156.png)<!-- -->

    ##                 species    eventDate   
    ##  Quiscalus mexicanus:2   Min.   :2015  
    ##  Acanthis flammea   :0   1st Qu.:2015  
    ##  Accipiter cooperii :0   Median :2015  
    ##  Accipiter gentilis :0   Mean   :2015  
    ##  Accipiter striatus :0   3rd Qu.:2015  
    ##  Actitis macularius :0   Max.   :2015  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-157.png)<!-- -->

    ##                 species    eventDate   
    ##  Seiurus aurocapilla:8   Min.   :2013  
    ##  Acanthis flammea   :0   1st Qu.:2014  
    ##  Accipiter cooperii :0   Median :2016  
    ##  Accipiter gentilis :0   Mean   :2017  
    ##  Accipiter striatus :0   3rd Qu.:2018  
    ##  Actitis macularius :0   Max.   :2023  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-158.png)<!-- -->

    ##                     species    eventDate   
    ##  Parkesia noveboracensis:1   Min.   :2023  
    ##  Acanthis flammea       :0   1st Qu.:2023  
    ##  Accipiter cooperii     :0   Median :2023  
    ##  Accipiter gentilis     :0   Mean   :2023  
    ##  Accipiter striatus     :0   3rd Qu.:2023  
    ##  Actitis macularius     :0   Max.   :2023  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-159.png)<!-- -->

    ##                species    eventDate   
    ##  Mniotilta varia   :3   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2013  
    ##  Accipiter gentilis:0   Mean   :2016  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-160.png)<!-- -->

    ##                species     eventDate   
    ##  Leiothlypis celata:11   Min.   :2013  
    ##  Acanthis flammea  : 0   1st Qu.:2013  
    ##  Accipiter cooperii: 0   Median :2013  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-161.png)<!-- -->

    ##                     species    eventDate   
    ##  Leiothlypis ruficapilla:1   Min.   :2023  
    ##  Acanthis flammea       :0   1st Qu.:2023  
    ##  Accipiter cooperii     :0   Median :2023  
    ##  Accipiter gentilis     :0   Mean   :2023  
    ##  Accipiter striatus     :0   3rd Qu.:2023  
    ##  Actitis macularius     :0   Max.   :2023  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-162.png)<!-- -->

    ##                species     eventDate   
    ##  Geothlypis trichas:34   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-163.png)<!-- -->

    ##                 species     eventDate   
    ##  Setophaga ruticilla:11   Min.   :2013  
    ##  Acanthis flammea   : 0   1st Qu.:2013  
    ##  Accipiter cooperii : 0   Median :2015  
    ##  Accipiter gentilis : 0   Mean   :2016  
    ##  Accipiter striatus : 0   3rd Qu.:2018  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-164.png)<!-- -->

    ##                species     eventDate   
    ##  Setophaga petechia:49   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2019  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-165.png)<!-- -->

    ##                         species    eventDate   
    ##  Setophaga coronata/auduboni:4   Min.   :2013  
    ##  Acanthis flammea           :0   1st Qu.:2013  
    ##  Accipiter cooperii         :0   Median :2018  
    ##  Accipiter gentilis         :0   Mean   :2018  
    ##  Accipiter striatus         :0   3rd Qu.:2023  
    ##  Actitis macularius         :0   Max.   :2023  
    ##  (Other)                    :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-166.png)<!-- -->

    ##                species    eventDate   
    ##  Setophaga coronata:3   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2013  
    ##  Accipiter gentilis:0   Mean   :2016  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-167.png)<!-- -->

    ##                species    eventDate   
    ##  Cardellina pusilla:6   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2013  
    ##  Accipiter gentilis:0   Mean   :2013  
    ##  Accipiter striatus:0   3rd Qu.:2013  
    ##  Actitis macularius:0   Max.   :2013  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-168.png)<!-- -->

    ##                   species     eventDate   
    ##  Cardinalis cardinalis:24   Min.   :2013  
    ##  Acanthis flammea     : 0   1st Qu.:2013  
    ##  Accipiter cooperii   : 0   Median :2013  
    ##  Accipiter gentilis   : 0   Mean   :2014  
    ##  Accipiter striatus   : 0   3rd Qu.:2013  
    ##  Actitis macularius   : 0   Max.   :2019  
    ##  (Other)              : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-169.png)<!-- -->

    ##                     species    eventDate   
    ##  Pheucticus ludovicianus:2   Min.   :2020  
    ##  Acanthis flammea       :0   1st Qu.:2020  
    ##  Accipiter cooperii     :0   Median :2020  
    ##  Accipiter gentilis     :0   Mean   :2020  
    ##  Accipiter striatus     :0   3rd Qu.:2020  
    ##  Actitis macularius     :0   Max.   :2020  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-170.png)<!-- -->

    ##                       species    eventDate   
    ##  Pheucticus melanocephalus:2   Min.   :2022  
    ##  Acanthis flammea         :0   1st Qu.:2022  
    ##  Accipiter cooperii       :0   Median :2022  
    ##  Accipiter gentilis       :0   Mean   :2022  
    ##  Accipiter striatus       :0   3rd Qu.:2023  
    ##  Actitis macularius       :0   Max.   :2023  
    ##  (Other)                  :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-171.png)<!-- -->

    ##                species     eventDate   
    ##  Passerina caerulea:37   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-172.png)<!-- -->

    ##                species    eventDate   
    ##  Passerina cyanea  :2   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2011  
    ##  Accipiter cooperii:0   Median :2011  
    ##  Accipiter gentilis:0   Mean   :2011  
    ##  Accipiter striatus:0   3rd Qu.:2011  
    ##  Actitis macularius:0   Max.   :2011  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-173.png)<!-- -->

    ##                species     eventDate   
    ##  Spiza americana   :24   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-174.png)<!-- -->

    ##                species     eventDate   
    ##  Aix sponsa        :10   Min.   :2015  
    ##  Acanthis flammea  : 0   1st Qu.:2020  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2020  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-175.png)<!-- -->

    ##                species    eventDate   
    ##  Asio otus         :2   Min.   :2020  
    ##  Acanthis flammea  :0   1st Qu.:2020  
    ##  Accipiter cooperii:0   Median :2020  
    ##  Accipiter gentilis:0   Mean   :2020  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-176.png)<!-- -->

    ##                species    eventDate   
    ##  Falco sparverius  :8   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2016  
    ##  Accipiter gentilis:0   Mean   :2016  
    ##  Accipiter striatus:0   3rd Qu.:2019  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-177.png)<!-- -->

    ##                     species    eventDate   
    ##  Calamospiza melanocorys:5   Min.   :2015  
    ##  Acanthis flammea       :0   1st Qu.:2015  
    ##  Accipiter cooperii     :0   Median :2015  
    ##  Accipiter gentilis     :0   Mean   :2017  
    ##  Accipiter striatus     :0   3rd Qu.:2018  
    ##  Actitis macularius     :0   Max.   :2021  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-178.png)<!-- -->

    ##                 species    eventDate   
    ##  Melospiza lincolnii:6   Min.   :2013  
    ##  Acanthis flammea   :0   1st Qu.:2013  
    ##  Accipiter cooperii :0   Median :2013  
    ##  Accipiter gentilis :0   Mean   :2014  
    ##  Accipiter striatus :0   3rd Qu.:2013  
    ##  Actitis macularius :0   Max.   :2020  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-179.png)<!-- -->

    ##                 species    eventDate   
    ##  Melospiza georgiana:1   Min.   :2013  
    ##  Acanthis flammea   :0   1st Qu.:2013  
    ##  Accipiter cooperii :0   Median :2013  
    ##  Accipiter gentilis :0   Mean   :2013  
    ##  Accipiter striatus :0   3rd Qu.:2013  
    ##  Actitis macularius :0   Max.   :2013  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-180.png)<!-- -->

    ##                species     eventDate   
    ##  Icterus spurius   :14   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2016  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-10-181.png)<!-- -->

## Pine Ridge / Oglala

``` r
districtR(ne_nf_gbif = ne_nf_gbif,district="Pine Ridge / Oglala")
```

![](appendix_1_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

    ##                species    eventDate   
    ##  Anser caerulescens:2   Min.   :2017  
    ##  Acanthis flammea  :0   1st Qu.:2017  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2017  
    ##  Accipiter striatus:0   3rd Qu.:2017  
    ##  Actitis macularius:0   Max.   :2017  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

    ##                species    eventDate   
    ##  Branta hutchinsii :2   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

    ##                species     eventDate   
    ##  Branta canadensis :81   Min.   :2010  
    ##  Acanthis flammea  : 0   1st Qu.:2017  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

    ##                          species    eventDate   
    ##  Branta hutchinsii/canadensis:2   Min.   :2011  
    ##  Acanthis flammea            :0   1st Qu.:2012  
    ##  Accipiter cooperii          :0   Median :2014  
    ##  Accipiter gentilis          :0   Mean   :2014  
    ##  Accipiter striatus          :0   3rd Qu.:2016  
    ##  Actitis macularius          :0   Max.   :2017  
    ##  (Other)                     :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-5.png)<!-- -->

    ##                species    eventDate   
    ##  Cygnus buccinator :2   Min.   :2020  
    ##  Acanthis flammea  :0   1st Qu.:2020  
    ##  Accipiter cooperii:0   Median :2020  
    ##  Accipiter gentilis:0   Mean   :2020  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2020  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-6.png)<!-- -->

    ##                species      eventDate   
    ##  Spatula discors   :168   Min.   :1984  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2019  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-7.png)<!-- -->

    ##                species    eventDate   
    ##  Spatula cyanoptera:5   Min.   :2018  
    ##  Acanthis flammea  :0   1st Qu.:2018  
    ##  Accipiter cooperii:0   Median :2020  
    ##  Accipiter gentilis:0   Mean   :2020  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-8.png)<!-- -->

    ##                        species    eventDate   
    ##  Spatula discors/cyanoptera:2   Min.   :2014  
    ##  Acanthis flammea          :0   1st Qu.:2016  
    ##  Accipiter cooperii        :0   Median :2018  
    ##  Accipiter gentilis        :0   Mean   :2018  
    ##  Accipiter striatus        :0   3rd Qu.:2020  
    ##  Actitis macularius        :0   Max.   :2022  
    ##  (Other)                   :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-9.png)<!-- -->

    ##                species      eventDate   
    ##  Spatula clypeata  :111   Min.   :1984  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2019  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-10.png)<!-- -->

    ##                species     eventDate   
    ##  Mareca strepera   :70   Min.   :1984  
    ##  Acanthis flammea  : 0   1st Qu.:2017  
    ##  Accipiter cooperii: 0   Median :2019  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-11.png)<!-- -->

    ##                species     eventDate   
    ##  Mareca americana  :40   Min.   :2000  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-12.png)<!-- -->

    ##                species      eventDate   
    ##  Anas platyrhynchos:346   Min.   :1984  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-13.png)<!-- -->

    ##                species     eventDate   
    ##  Anas acuta        :51   Min.   :1984  
    ##  Acanthis flammea  : 0   1st Qu.:2012  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2015  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-14.png)<!-- -->

    ##                species     eventDate   
    ##  Anas crecca       :35   Min.   :1995  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2016  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-15.png)<!-- -->

    ##                species    eventDate   
    ##  Aythya valisineria:2   Min.   :1984  
    ##  Acanthis flammea  :0   1st Qu.:1994  
    ##  Accipiter cooperii:0   Median :2004  
    ##  Accipiter gentilis:0   Mean   :2004  
    ##  Accipiter striatus:0   3rd Qu.:2013  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-16.png)<!-- -->

    ##                species     eventDate   
    ##  Aythya americana  :19   Min.   :1985  
    ##  Acanthis flammea  : 0   1st Qu.:2011  
    ##  Accipiter cooperii: 0   Median :2014  
    ##  Accipiter gentilis: 0   Mean   :2012  
    ##  Accipiter striatus: 0   3rd Qu.:2018  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-17.png)<!-- -->

    ##                species     eventDate   
    ##  Aythya collaris   :21   Min.   :1984  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2019  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-18.png)<!-- -->

    ##                species     eventDate   
    ##  Aythya affinis    :10   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2011  
    ##  Accipiter cooperii: 0   Median :2019  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-19.png)<!-- -->

    ##                species     eventDate   
    ##  Bucephala albeola :18   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2019  
    ##  Accipiter cooperii: 0   Median :2019  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-20.png)<!-- -->

    ##                species    eventDate   
    ##  Bucephala clangula:2   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2011  
    ##  Accipiter cooperii:0   Median :2011  
    ##  Accipiter gentilis:0   Mean   :2011  
    ##  Accipiter striatus:0   3rd Qu.:2011  
    ##  Actitis macularius:0   Max.   :2011  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-21.png)<!-- -->

    ##                   species    eventDate   
    ##  Lophodytes cucullatus:6   Min.   :2016  
    ##  Acanthis flammea     :0   1st Qu.:2017  
    ##  Accipiter cooperii   :0   Median :2021  
    ##  Accipiter gentilis   :0   Mean   :2019  
    ##  Accipiter striatus   :0   3rd Qu.:2021  
    ##  Actitis macularius   :0   Max.   :2021  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-22.png)<!-- -->

    ##                species     eventDate   
    ##  Mergus merganser  :10   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2016  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-23.png)<!-- -->

    ##                species     eventDate   
    ##  Oxyura jamaicensis:31   Min.   :2009  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-24.png)<!-- -->

    ##                 species    eventDate   
    ##  Colinus virginianus:3   Min.   :1933  
    ##  Acanthis flammea   :0   1st Qu.:1934  
    ##  Accipiter cooperii :0   Median :1934  
    ##  Accipiter gentilis :0   Mean   :1963  
    ##  Accipiter striatus :0   3rd Qu.:1978  
    ##  Actitis macularius :0   Max.   :2023  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-25.png)<!-- -->

    ##                 species      eventDate   
    ##  Meleagris gallopavo:541   Min.   :1988  
    ##  Acanthis flammea   :  0   1st Qu.:2016  
    ##  Accipiter cooperii :  0   Median :2018  
    ##  Accipiter gentilis :  0   Mean   :2017  
    ##  Accipiter striatus :  0   3rd Qu.:2020  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-26.png)<!-- -->

    ##                      species     eventDate   
    ##  Tympanuchus phasianellus:93   Min.   :1984  
    ##  Acanthis flammea        : 0   1st Qu.:2017  
    ##  Accipiter cooperii      : 0   Median :2019  
    ##  Accipiter gentilis      : 0   Mean   :2017  
    ##  Accipiter striatus      : 0   3rd Qu.:2020  
    ##  Actitis macularius      : 0   Max.   :2023  
    ##  (Other)                 : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-27.png)<!-- -->

    ##                 species     eventDate   
    ##  Phasianus colchicus:46   Min.   :1984  
    ##  Acanthis flammea   : 0   1st Qu.:2014  
    ##  Accipiter cooperii : 0   Median :2018  
    ##  Accipiter gentilis : 0   Mean   :2015  
    ##  Accipiter striatus : 0   3rd Qu.:2020  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-28.png)<!-- -->

    ##                 species     eventDate   
    ##  Podilymbus podiceps:75   Min.   :2000  
    ##  Acanthis flammea   : 0   1st Qu.:2014  
    ##  Accipiter cooperii : 0   Median :2018  
    ##  Accipiter gentilis : 0   Mean   :2017  
    ##  Accipiter striatus : 0   3rd Qu.:2020  
    ##  Actitis macularius : 0   Max.   :2022  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-29.png)<!-- -->

    ##                  species     eventDate   
    ##  Podiceps nigricollis:35   Min.   :1984  
    ##  Acanthis flammea    : 0   1st Qu.:2012  
    ##  Accipiter cooperii  : 0   Median :2016  
    ##  Accipiter gentilis  : 0   Mean   :2016  
    ##  Accipiter striatus  : 0   3rd Qu.:2021  
    ##  Actitis macularius  : 0   Max.   :2023  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-30.png)<!-- -->

    ##                       species     eventDate   
    ##  Aechmophorus occidentalis:25   Min.   :1982  
    ##  Acanthis flammea         : 0   1st Qu.:2009  
    ##  Accipiter cooperii       : 0   Median :2013  
    ##  Accipiter gentilis       : 0   Mean   :2009  
    ##  Accipiter striatus       : 0   3rd Qu.:2013  
    ##  Actitis macularius       : 0   Max.   :2022  
    ##  (Other)                  : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-31.png)<!-- -->

    ##                  species    eventDate   
    ##  Aechmophorus clarkii:2   Min.   :2013  
    ##  Acanthis flammea    :0   1st Qu.:2013  
    ##  Accipiter cooperii  :0   Median :2013  
    ##  Accipiter gentilis  :0   Mean   :2013  
    ##  Accipiter striatus  :0   3rd Qu.:2013  
    ##  Actitis macularius  :0   Max.   :2013  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-32.png)<!-- -->

    ##                               species    eventDate   
    ##  Aechmophorus occidentalis/clarkii:2   Min.   :2011  
    ##  Acanthis flammea                 :0   1st Qu.:2013  
    ##  Accipiter cooperii               :0   Median :2016  
    ##  Accipiter gentilis               :0   Mean   :2016  
    ##  Accipiter striatus               :0   3rd Qu.:2018  
    ##  Actitis macularius               :0   Max.   :2020  
    ##  (Other)                          :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-33.png)<!-- -->

    ##                species      eventDate   
    ##  Columba livia     :123   Min.   :1991  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-34.png)<!-- -->

    ##                   species      eventDate   
    ##  Streptopelia decaocto:539   Min.   :2007  
    ##  Acanthis flammea     :  0   1st Qu.:2014  
    ##  Accipiter cooperii   :  0   Median :2016  
    ##  Accipiter gentilis   :  0   Mean   :2016  
    ##  Accipiter striatus   :  0   3rd Qu.:2018  
    ##  Actitis macularius   :  0   Max.   :2023  
    ##  (Other)              :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-35.png)<!-- -->

    ##                species       eventDate   
    ##  Zenaida macroura  :1367   Min.   :1975  
    ##  Acanthis flammea  :   0   1st Qu.:2015  
    ##  Accipiter cooperii:   0   Median :2018  
    ##  Accipiter gentilis:   0   Mean   :2017  
    ##  Accipiter striatus:   0   3rd Qu.:2020  
    ##  Actitis macularius:   0   Max.   :2023  
    ##  (Other)           :   0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-36.png)<!-- -->

    ##                 species     eventDate   
    ##  Coccyzus americanus:29   Min.   :2017  
    ##  Acanthis flammea   : 0   1st Qu.:2018  
    ##  Accipiter cooperii : 0   Median :2020  
    ##  Accipiter gentilis : 0   Mean   :2020  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-37.png)<!-- -->

    ##                species      eventDate   
    ##  Chordeiles minor  :318   Min.   :1959  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2019  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-38.png)<!-- -->

    ##                      species      eventDate   
    ##  Phalaenoptilus nuttallii:142   Min.   :1981  
    ##  Acanthis flammea        :  0   1st Qu.:2015  
    ##  Accipiter cooperii      :  0   Median :2016  
    ##  Accipiter gentilis      :  0   Mean   :2016  
    ##  Accipiter striatus      :  0   3rd Qu.:2020  
    ##  Actitis macularius      :  0   Max.   :2023  
    ##  (Other)                 :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-39.png)<!-- -->

    ##                species     eventDate   
    ##  Chaetura pelagica :31   Min.   :1988  
    ##  Acanthis flammea  : 0   1st Qu.:2006  
    ##  Accipiter cooperii: 0   Median :2016  
    ##  Accipiter gentilis: 0   Mean   :2012  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-40.png)<!-- -->

    ##                  species     eventDate   
    ##  Aeronautes saxatalis:66   Min.   :1991  
    ##  Acanthis flammea    : 0   1st Qu.:2018  
    ##  Accipiter cooperii  : 0   Median :2020  
    ##  Accipiter gentilis  : 0   Mean   :2017  
    ##  Accipiter striatus  : 0   3rd Qu.:2021  
    ##  Actitis macularius  : 0   Max.   :2023  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-41.png)<!-- -->

    ##                  species    eventDate   
    ##  Archilochus colubris:2   Min.   :2016  
    ##  Acanthis flammea    :0   1st Qu.:2016  
    ##  Accipiter cooperii  :0   Median :2016  
    ##  Accipiter gentilis  :0   Mean   :2016  
    ##  Accipiter striatus  :0   3rd Qu.:2016  
    ##  Actitis macularius  :0   Max.   :2016  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-42.png)<!-- -->

    ##                species    eventDate   
    ##  Porzana carolina  :7   Min.   :2015  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2019  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-43.png)<!-- -->

    ##                species     eventDate   
    ##  Fulica americana  :77   Min.   :2000  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-44.png)<!-- -->

    ##                 species     eventDate   
    ##  Antigone canadensis:29   Min.   :2009  
    ##  Acanthis flammea   : 0   1st Qu.:2016  
    ##  Accipiter cooperii : 0   Median :2018  
    ##  Accipiter gentilis : 0   Mean   :2018  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2022  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-45.png)<!-- -->

    ##                     species    eventDate   
    ##  Recurvirostra americana:9   Min.   :1981  
    ##  Acanthis flammea       :0   1st Qu.:2011  
    ##  Accipiter cooperii     :0   Median :2018  
    ##  Accipiter gentilis     :0   Mean   :2009  
    ##  Accipiter striatus     :0   3rd Qu.:2019  
    ##  Actitis macularius     :0   Max.   :2022  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-46.png)<!-- -->

    ##                  species      eventDate   
    ##  Charadrius vociferus:309   Min.   :1975  
    ##  Acanthis flammea    :  0   1st Qu.:2016  
    ##  Accipiter cooperii  :  0   Median :2019  
    ##  Accipiter gentilis  :  0   Mean   :2017  
    ##  Accipiter striatus  :  0   3rd Qu.:2021  
    ##  Actitis macularius  :  0   Max.   :2023  
    ##  (Other)             :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-47.png)<!-- -->

    ##                  species      eventDate   
    ##  Bartramia longicauda:192   Min.   :1985  
    ##  Acanthis flammea    :  0   1st Qu.:2016  
    ##  Accipiter cooperii  :  0   Median :2019  
    ##  Accipiter gentilis  :  0   Mean   :2017  
    ##  Accipiter striatus  :  0   3rd Qu.:2021  
    ##  Actitis macularius  :  0   Max.   :2023  
    ##  (Other)             :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-48.png)<!-- -->

    ##                 species     eventDate   
    ##  Numenius americanus:62   Min.   :1971  
    ##  Acanthis flammea   : 0   1st Qu.:2015  
    ##  Accipiter cooperii : 0   Median :2019  
    ##  Accipiter gentilis : 0   Mean   :2015  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-49.png)<!-- -->

    ##                species    eventDate   
    ##  Limosa fedoa      :6   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2012  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2017  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-50.png)<!-- -->

    ##                species    eventDate   
    ##  Calidris bairdii  :6   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2012  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2017  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-51.png)<!-- -->

    ##                species    eventDate   
    ##  Calidris minutilla:3   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-52.png)<!-- -->

    ##                species    eventDate   
    ##  Calidris melanotos:1   Min.   :2022  
    ##  Acanthis flammea  :0   1st Qu.:2022  
    ##  Accipiter cooperii:0   Median :2022  
    ##  Accipiter gentilis:0   Mean   :2022  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-53.png)<!-- -->

    ##                species    eventDate   
    ##  Calidris pusilla  :1   Min.   :2022  
    ##  Acanthis flammea  :0   1st Qu.:2022  
    ##  Accipiter cooperii:0   Median :2022  
    ##  Accipiter gentilis:0   Mean   :2022  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-54.png)<!-- -->

    ##                     species    eventDate   
    ##  Limnodromus scolopaceus:2   Min.   :2013  
    ##  Acanthis flammea       :0   1st Qu.:2013  
    ##  Accipiter cooperii     :0   Median :2013  
    ##  Accipiter gentilis     :0   Mean   :2013  
    ##  Accipiter striatus     :0   3rd Qu.:2013  
    ##  Actitis macularius     :0   Max.   :2013  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-55.png)<!-- -->

    ##                species     eventDate   
    ##  Gallinago delicata:21   Min.   :2014  
    ##  Acanthis flammea  : 0   1st Qu.:2017  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-56.png)<!-- -->

    ##                 species     eventDate   
    ##  Phalaropus tricolor:58   Min.   :2011  
    ##  Acanthis flammea   : 0   1st Qu.:2018  
    ##  Accipiter cooperii : 0   Median :2019  
    ##  Accipiter gentilis : 0   Mean   :2019  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-57.png)<!-- -->

    ##                species    eventDate   
    ##  Phalaropus lobatus:3   Min.   :2018  
    ##  Acanthis flammea  :0   1st Qu.:2018  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2020  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-58.png)<!-- -->

    ##                  species     eventDate   
    ##  Actitis macularius  :60   Min.   :1987  
    ##  Acanthis flammea    : 0   1st Qu.:2014  
    ##  Accipiter cooperii  : 0   Median :2018  
    ##  Accipiter gentilis  : 0   Mean   :2016  
    ##  Accipiter striatus  : 0   3rd Qu.:2021  
    ##  Aechmophorus clarkii: 0   Max.   :2022  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-59.png)<!-- -->

    ##                species    eventDate   
    ##  Tringa solitaria  :6   Min.   :2015  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2023  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-60.png)<!-- -->

    ##                species    eventDate   
    ##  Tringa melanoleuca:9   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2020  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2019  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-61.png)<!-- -->

    ##                species    eventDate   
    ##  Tringa flavipes   :9   Min.   :2000  
    ##  Acanthis flammea  :0   1st Qu.:2018  
    ##  Accipiter cooperii:0   Median :2019  
    ##  Accipiter gentilis:0   Mean   :2015  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-62.png)<!-- -->

    ##                  species    eventDate   
    ##  Leucophaeus pipixcan:4   Min.   :1985  
    ##  Acanthis flammea    :0   1st Qu.:1985  
    ##  Accipiter cooperii  :0   Median :2000  
    ##  Accipiter gentilis  :0   Mean   :2000  
    ##  Accipiter striatus  :0   3rd Qu.:2016  
    ##  Actitis macularius  :0   Max.   :2016  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-63.png)<!-- -->

    ##                species    eventDate   
    ##  Larus delawarensis:4   Min.   :1985  
    ##  Acanthis flammea  :0   1st Qu.:1985  
    ##  Accipiter cooperii:0   Median :2004  
    ##  Accipiter gentilis:0   Mean   :2004  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-64.png)<!-- -->

    ##                species    eventDate   
    ##  Chlidonias niger  :2   Min.   :2003  
    ##  Acanthis flammea  :0   1st Qu.:2003  
    ##  Accipiter cooperii:0   Median :2003  
    ##  Accipiter gentilis:0   Mean   :2003  
    ##  Accipiter striatus:0   3rd Qu.:2003  
    ##  Actitis macularius:0   Max.   :2003  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-65.png)<!-- -->

    ##                species    eventDate   
    ##  Gavia immer       :2   Min.   :2018  
    ##  Acanthis flammea  :0   1st Qu.:2018  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2018  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-66.png)<!-- -->

    ##                 species     eventDate   
    ##  Nannopterum auritum:24   Min.   :1984  
    ##  Acanthis flammea   : 0   1st Qu.:2015  
    ##  Accipiter cooperii : 0   Median :2018  
    ##  Accipiter gentilis : 0   Mean   :2014  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-67.png)<!-- -->

    ##                       species    eventDate   
    ##  Pelecanus erythrorhynchos:9   Min.   :2000  
    ##  Acanthis flammea         :0   1st Qu.:2018  
    ##  Accipiter cooperii       :0   Median :2019  
    ##  Accipiter gentilis       :0   Mean   :2015  
    ##  Accipiter striatus       :0   3rd Qu.:2020  
    ##  Actitis macularius       :0   Max.   :2022  
    ##  (Other)                  :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-68.png)<!-- -->

    ##                   species    eventDate   
    ##  Botaurus lentiginosus:1   Min.   :1984  
    ##  Acanthis flammea     :0   1st Qu.:1984  
    ##  Accipiter cooperii   :0   Median :1984  
    ##  Accipiter gentilis   :0   Mean   :1984  
    ##  Accipiter striatus   :0   3rd Qu.:1984  
    ##  Actitis macularius   :0   Max.   :1984  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-69.png)<!-- -->

    ##                species      eventDate   
    ##  Ardea herodias    :169   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2019  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-70.png)<!-- -->

    ##                species    eventDate   
    ##  Ardea alba        :2   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-71.png)<!-- -->

    ##                species    eventDate   
    ##  Bubulcus ibis     :2   Min.   :2014  
    ##  Acanthis flammea  :0   1st Qu.:2014  
    ##  Accipiter cooperii:0   Median :2014  
    ##  Accipiter gentilis:0   Mean   :2014  
    ##  Accipiter striatus:0   3rd Qu.:2014  
    ##  Actitis macularius:0   Max.   :2014  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-72.png)<!-- -->

    ##                   species     eventDate   
    ##  Nycticorax nycticorax:12   Min.   :1984  
    ##  Acanthis flammea     : 0   1st Qu.:2013  
    ##  Accipiter cooperii   : 0   Median :2014  
    ##  Accipiter gentilis   : 0   Mean   :2014  
    ##  Accipiter striatus   : 0   3rd Qu.:2021  
    ##  Actitis macularius   : 0   Max.   :2022  
    ##  (Other)              : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-73.png)<!-- -->

    ##                species    eventDate   
    ##  Plegadis chihi    :3   Min.   :2018  
    ##  Acanthis flammea  :0   1st Qu.:2018  
    ##  Accipiter cooperii:0   Median :2018  
    ##  Accipiter gentilis:0   Mean   :2019  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-74.png)<!-- -->

    ##                        species    eventDate   
    ##  Plegadis falcinellus/chihi:1   Min.   :2020  
    ##  Acanthis flammea          :0   1st Qu.:2020  
    ##  Accipiter cooperii        :0   Median :2020  
    ##  Accipiter gentilis        :0   Mean   :2020  
    ##  Accipiter striatus        :0   3rd Qu.:2020  
    ##  Actitis macularius        :0   Max.   :2020  
    ##  (Other)                   :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-75.png)<!-- -->

    ##                species      eventDate   
    ##  Cathartes aura    :947   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-76.png)<!-- -->

    ##                species     eventDate   
    ##  Pandion haliaetus :46   Min.   :2010  
    ##  Acanthis flammea  : 0   1st Qu.:2013  
    ##  Accipiter cooperii: 0   Median :2016  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-77.png)<!-- -->

    ##                species      eventDate   
    ##  Aquila chrysaetos :120   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2017  
    ##  Accipiter cooperii:  0   Median :2020  
    ##  Accipiter gentilis:  0   Mean   :2018  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-78.png)<!-- -->

    ##                species     eventDate   
    ##  Circus hudsonius  :90   Min.   :1999  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-79.png)<!-- -->

    ##                  species     eventDate   
    ##  Accipiter striatus  :47   Min.   :2009  
    ##  Acanthis flammea    : 0   1st Qu.:2014  
    ##  Accipiter cooperii  : 0   Median :2017  
    ##  Accipiter gentilis  : 0   Mean   :2016  
    ##  Actitis macularius  : 0   3rd Qu.:2019  
    ##  Aechmophorus clarkii: 0   Max.   :2022  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-80.png)<!-- -->

    ##                  species     eventDate   
    ##  Accipiter cooperii  :97   Min.   :1984  
    ##  Acanthis flammea    : 0   1st Qu.:2015  
    ##  Accipiter gentilis  : 0   Median :2017  
    ##  Accipiter striatus  : 0   Mean   :2016  
    ##  Actitis macularius  : 0   3rd Qu.:2021  
    ##  Aechmophorus clarkii: 0   Max.   :2023  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-81.png)<!-- -->

    ##                  species    eventDate   
    ##  Accipiter gentilis  :2   Min.   :1989  
    ##  Acanthis flammea    :0   1st Qu.:1989  
    ##  Accipiter cooperii  :0   Median :1989  
    ##  Accipiter striatus  :0   Mean   :1989  
    ##  Actitis macularius  :0   3rd Qu.:1989  
    ##  Aechmophorus clarkii:0   Max.   :1989  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-82.png)<!-- -->

    ##                      species     eventDate   
    ##  Haliaeetus leucocephalus:38   Min.   :2014  
    ##  Acanthis flammea        : 0   1st Qu.:2017  
    ##  Accipiter cooperii      : 0   Median :2020  
    ##  Accipiter gentilis      : 0   Mean   :2019  
    ##  Accipiter striatus      : 0   3rd Qu.:2021  
    ##  Actitis macularius      : 0   Max.   :2022  
    ##  (Other)                 : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-83.png)<!-- -->

    ##                species     eventDate   
    ##  Buteo platypterus :10   Min.   :2015  
    ##  Acanthis flammea  : 0   1st Qu.:2017  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-84.png)<!-- -->

    ##                species      eventDate   
    ##  Buteo swainsoni   :149   Min.   :1981  
    ##  Acanthis flammea  :  0   1st Qu.:2010  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2014  
    ##  Accipiter striatus:  0   3rd Qu.:2019  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-85.png)<!-- -->

    ##                species      eventDate   
    ##  Buteo jamaicensis :640   Min.   :1984  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-86.png)<!-- -->

    ##                species     eventDate   
    ##  Buteo lagopus     :27   Min.   :2018  
    ##  Acanthis flammea  : 0   1st Qu.:2020  
    ##  Accipiter cooperii: 0   Median :2021  
    ##  Accipiter gentilis: 0   Mean   :2020  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-87.png)<!-- -->

    ##                species     eventDate   
    ##  Buteo regalis     :36   Min.   :2010  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-88.png)<!-- -->

    ##                species    eventDate   
    ##  Tyto alba         :4   Min.   :2015  
    ##  Acanthis flammea  :0   1st Qu.:2015  
    ##  Accipiter cooperii:0   Median :2015  
    ##  Accipiter gentilis:0   Mean   :2015  
    ##  Accipiter striatus:0   3rd Qu.:2015  
    ##  Actitis macularius:0   Max.   :2015  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-89.png)<!-- -->

    ##                species     eventDate   
    ##  Megascops asio    :54   Min.   :1984  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-90.png)<!-- -->

    ##                species    eventDate   
    ##  Bubo scandiacus   :2   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-91.png)<!-- -->

    ##                species      eventDate   
    ##  Bubo virginianus  :273   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2016  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2022  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-92.png)<!-- -->

    ##                species     eventDate   
    ##  Athene cunicularia:32   Min.   :1984  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-93.png)<!-- -->

    ##                species    eventDate   
    ##  Strix varia       :2   Min.   :2016  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2016  
    ##  Accipiter gentilis:0   Mean   :2016  
    ##  Accipiter striatus:0   3rd Qu.:2016  
    ##  Actitis macularius:0   Max.   :2016  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-94.png)<!-- -->

    ##                species     eventDate   
    ##  Asio flammeus     :13   Min.   :2004  
    ##  Acanthis flammea  : 0   1st Qu.:2010  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2014  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-95.png)<!-- -->

    ##                species    eventDate   
    ##  Aegolius acadicus :5   Min.   :2009  
    ##  Acanthis flammea  :0   1st Qu.:2009  
    ##  Accipiter cooperii:0   Median :2020  
    ##  Accipiter gentilis:0   Mean   :2016  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-96.png)<!-- -->

    ##                species      eventDate   
    ##  Megaceryle alcyon :203   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-97.png)<!-- -->

    ##                species    eventDate   
    ##  Sphyrapicus varius:2   Min.   :2009  
    ##  Acanthis flammea  :0   1st Qu.:2009  
    ##  Accipiter cooperii:0   Median :2009  
    ##  Accipiter gentilis:0   Mean   :2009  
    ##  Accipiter striatus:0   3rd Qu.:2009  
    ##  Actitis macularius:0   Max.   :2009  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-98.png)<!-- -->

    ##                  species    eventDate   
    ##  Sphyrapicus nuchalis:1   Min.   :2014  
    ##  Acanthis flammea    :0   1st Qu.:2014  
    ##  Accipiter cooperii  :0   Median :2014  
    ##  Accipiter gentilis  :0   Mean   :2014  
    ##  Accipiter striatus  :0   3rd Qu.:2014  
    ##  Actitis macularius  :0   Max.   :2014  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-99.png)<!-- -->

    ##                         species    eventDate   
    ##  Sphyrapicus varius/nuchalis:1   Min.   :2018  
    ##  Acanthis flammea           :0   1st Qu.:2018  
    ##  Accipiter cooperii         :0   Median :2018  
    ##  Accipiter gentilis         :0   Mean   :2018  
    ##  Accipiter striatus         :0   3rd Qu.:2018  
    ##  Actitis macularius         :0   Max.   :2018  
    ##  (Other)                    :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-100.png)<!-- -->

    ##                species     eventDate   
    ##  Melanerpes lewis  :44   Min.   :1980  
    ##  Acanthis flammea  : 0   1st Qu.:1988  
    ##  Accipiter cooperii: 0   Median :2000  
    ##  Accipiter gentilis: 0   Mean   :2002  
    ##  Accipiter striatus: 0   3rd Qu.:2017  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-101.png)<!-- -->

    ##                        species      eventDate   
    ##  Melanerpes erythrocephalus:938   Min.   :1985  
    ##  Acanthis flammea          :  0   1st Qu.:2016  
    ##  Accipiter cooperii        :  0   Median :2018  
    ##  Accipiter gentilis        :  0   Mean   :2018  
    ##  Accipiter striatus        :  0   3rd Qu.:2020  
    ##  Actitis macularius        :  0   Max.   :2023  
    ##  (Other)                   :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-102.png)<!-- -->

    ##                  species     eventDate   
    ##  Melanerpes carolinus:10   Min.   :2011  
    ##  Acanthis flammea    : 0   1st Qu.:2013  
    ##  Accipiter cooperii  : 0   Median :2014  
    ##  Accipiter gentilis  : 0   Mean   :2014  
    ##  Accipiter striatus  : 0   3rd Qu.:2015  
    ##  Actitis macularius  : 0   Max.   :2017  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-103.png)<!-- -->

    ##                species    eventDate   
    ##  Picoides dorsalis :8   Min.   :2014  
    ##  Acanthis flammea  :0   1st Qu.:2014  
    ##  Accipiter cooperii:0   Median :2014  
    ##  Accipiter gentilis:0   Mean   :2014  
    ##  Accipiter striatus:0   3rd Qu.:2014  
    ##  Actitis macularius:0   Max.   :2014  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-104.png)<!-- -->

    ##                 species      eventDate   
    ##  Dryobates pubescens:876   Min.   :1975  
    ##  Acanthis flammea   :  0   1st Qu.:2015  
    ##  Accipiter cooperii :  0   Median :2018  
    ##  Accipiter gentilis :  0   Mean   :2017  
    ##  Accipiter striatus :  0   3rd Qu.:2020  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-105.png)<!-- -->

    ##                species      eventDate   
    ##  Dryobates villosus:980   Min.   :1975  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-106.png)<!-- -->

    ##                          species    eventDate   
    ##  Dryobates pubescens/villosus:1   Min.   :2021  
    ##  Acanthis flammea            :0   1st Qu.:2021  
    ##  Accipiter cooperii          :0   Median :2021  
    ##  Accipiter gentilis          :0   Mean   :2021  
    ##  Accipiter striatus          :0   3rd Qu.:2021  
    ##  Actitis macularius          :0   Max.   :2021  
    ##  (Other)                     :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-107.png)<!-- -->

    ##                species       eventDate   
    ##  Colaptes auratus  :1473   Min.   :1975  
    ##  Acanthis flammea  :   0   1st Qu.:2015  
    ##  Accipiter cooperii:   0   Median :2018  
    ##  Accipiter gentilis:   0   Mean   :2017  
    ##  Accipiter striatus:   0   3rd Qu.:2020  
    ##  Actitis macularius:   0   Max.   :2023  
    ##  (Other)           :   0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-108.png)<!-- -->

    ##                species     eventDate   
    ##  Falco columbarius :13   Min.   :2014  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2018  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-109.png)<!-- -->

    ##                species    eventDate   
    ##  Falco peregrinus  :8   Min.   :2016  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2016  
    ##  Accipiter gentilis:0   Mean   :2017  
    ##  Accipiter striatus:0   3rd Qu.:2017  
    ##  Actitis macularius:0   Max.   :2020  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-110.png)<!-- -->

    ##                species     eventDate   
    ##  Falco mexicanus   :64   Min.   :1935  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2012  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-111.png)<!-- -->

    ##                species     eventDate   
    ##  Contopus cooperi  :16   Min.   :2010  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-112.png)<!-- -->

    ##                 species      eventDate   
    ##  Contopus sordidulus:844   Min.   :1959  
    ##  Acanthis flammea   :  0   1st Qu.:2015  
    ##  Accipiter cooperii :  0   Median :2018  
    ##  Accipiter gentilis :  0   Mean   :2017  
    ##  Accipiter striatus :  0   3rd Qu.:2020  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-113.png)<!-- -->

    ##                species    eventDate   
    ##  Contopus virens   :3   Min.   :2009  
    ##  Acanthis flammea  :0   1st Qu.:2014  
    ##  Accipiter cooperii:0   Median :2020  
    ##  Accipiter gentilis:0   Mean   :2016  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2020  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-114.png)<!-- -->

    ##                        species    eventDate   
    ##  Contopus sordidulus/virens:1   Min.   :2010  
    ##  Acanthis flammea          :0   1st Qu.:2010  
    ##  Accipiter cooperii        :0   Median :2010  
    ##  Accipiter gentilis        :0   Mean   :2010  
    ##  Accipiter striatus        :0   3rd Qu.:2010  
    ##  Actitis macularius        :0   Max.   :2010  
    ##  (Other)                   :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-115.png)<!-- -->

    ##                species    eventDate   
    ##  Empidonax alnorum :3   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-116.png)<!-- -->

    ##                species     eventDate   
    ##  Empidonax traillii:40   Min.   :2010  
    ##  Acanthis flammea  : 0   1st Qu.:2018  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2019  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-117.png)<!-- -->

    ##                species     eventDate   
    ##  Empidonax minimus :73   Min.   :2007  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-118.png)<!-- -->

    ##                 species    eventDate   
    ##  Empidonax hammondii:7   Min.   :2020  
    ##  Acanthis flammea   :0   1st Qu.:2020  
    ##  Accipiter cooperii :0   Median :2020  
    ##  Accipiter gentilis :0   Mean   :2021  
    ##  Accipiter striatus :0   3rd Qu.:2022  
    ##  Actitis macularius :0   Max.   :2022  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-119.png)<!-- -->

    ##                species    eventDate   
    ##  Empidonax wrightii:2   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2013  
    ##  Accipiter gentilis:0   Mean   :2013  
    ##  Accipiter striatus:0   3rd Qu.:2013  
    ##  Actitis macularius:0   Max.   :2013  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-120.png)<!-- -->

    ##                   species    eventDate   
    ##  Empidonax oberholseri:8   Min.   :2013  
    ##  Acanthis flammea     :0   1st Qu.:2014  
    ##  Accipiter cooperii   :0   Median :2014  
    ##  Accipiter gentilis   :0   Mean   :2017  
    ##  Accipiter striatus   :0   3rd Qu.:2021  
    ##  Actitis macularius   :0   Max.   :2023  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-121.png)<!-- -->

    ##                    species      eventDate   
    ##  Empidonax occidentalis:129   Min.   :1994  
    ##  Acanthis flammea      :  0   1st Qu.:2015  
    ##  Accipiter cooperii    :  0   Median :2019  
    ##  Accipiter gentilis    :  0   Mean   :2017  
    ##  Accipiter striatus    :  0   3rd Qu.:2021  
    ##  Actitis macularius    :  0   Max.   :2023  
    ##  (Other)               :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-122.png)<!-- -->

    ##                               species    eventDate   
    ##  Empidonax difficilis/occidentalis:1   Min.   :2015  
    ##  Acanthis flammea                 :0   1st Qu.:2015  
    ##  Accipiter cooperii               :0   Median :2015  
    ##  Accipiter gentilis               :0   Mean   :2015  
    ##  Accipiter striatus               :0   3rd Qu.:2015  
    ##  Actitis macularius               :0   Max.   :2015  
    ##  (Other)                          :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-123.png)<!-- -->

    ##                species      eventDate   
    ##  Sayornis phoebe   :283   Min.   :1991  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-124.png)<!-- -->

    ##                species      eventDate   
    ##  Sayornis saya     :220   Min.   :1945  
    ##  Acanthis flammea  :  0   1st Qu.:2013  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2015  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-125.png)<!-- -->

    ##                species      eventDate   
    ##  Myiarchus crinitus:379   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-126.png)<!-- -->

    ##                 species     eventDate   
    ##  Tyrannus vociferans:40   Min.   :1984  
    ##  Acanthis flammea   : 0   1st Qu.:2013  
    ##  Accipiter cooperii : 0   Median :2016  
    ##  Accipiter gentilis : 0   Mean   :2015  
    ##  Accipiter striatus : 0   3rd Qu.:2020  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-127.png)<!-- -->

    ##                 species      eventDate   
    ##  Tyrannus verticalis:381   Min.   :1975  
    ##  Acanthis flammea   :  0   1st Qu.:2015  
    ##  Accipiter cooperii :  0   Median :2018  
    ##  Accipiter gentilis :  0   Mean   :2016  
    ##  Accipiter striatus :  0   3rd Qu.:2020  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-128.png)<!-- -->

    ##                            species    eventDate   
    ##  Tyrannus vociferans/verticalis:2   Min.   :2012  
    ##  Acanthis flammea              :0   1st Qu.:2014  
    ##  Accipiter cooperii            :0   Median :2017  
    ##  Accipiter gentilis            :0   Mean   :2017  
    ##  Accipiter striatus            :0   3rd Qu.:2020  
    ##  Actitis macularius            :0   Max.   :2022  
    ##  (Other)                       :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-129.png)<!-- -->

    ##                species      eventDate   
    ##  Tyrannus tyrannus :743   Min.   :1959  
    ##  Acanthis flammea  :  0   1st Qu.:2017  
    ##  Accipiter cooperii:  0   Median :2019  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-130.png)<!-- -->

    ##                species     eventDate   
    ##  Vireo bellii      :29   Min.   :1984  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-131.png)<!-- -->

    ##                species     eventDate   
    ##  Vireo cassinii    :16   Min.   :2013  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2014  
    ##  Accipiter gentilis: 0   Mean   :2015  
    ##  Accipiter striatus: 0   3rd Qu.:2017  
    ##  Actitis macularius: 0   Max.   :2018  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-132.png)<!-- -->

    ##                species    eventDate   
    ##  Vireo solitarius  :2   Min.   :2017  
    ##  Acanthis flammea  :0   1st Qu.:2017  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2017  
    ##  Accipiter striatus:0   3rd Qu.:2017  
    ##  Actitis macularius:0   Max.   :2017  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-133.png)<!-- -->

    ##                species      eventDate   
    ##  Vireo plumbeus    :122   Min.   :2004  
    ##  Acanthis flammea  :  0   1st Qu.:2010  
    ##  Accipiter cooperii:  0   Median :2015  
    ##  Accipiter gentilis:  0   Mean   :2015  
    ##  Accipiter striatus:  0   3rd Qu.:2018  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-134.png)<!-- -->

    ##                     species    eventDate   
    ##  Vireo cassinii/plumbeus:1   Min.   :2021  
    ##  Acanthis flammea       :0   1st Qu.:2021  
    ##  Accipiter cooperii     :0   Median :2021  
    ##  Accipiter gentilis     :0   Mean   :2021  
    ##  Accipiter striatus     :0   3rd Qu.:2021  
    ##  Actitis macularius     :0   Max.   :2021  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-135.png)<!-- -->

    ##                  species    eventDate   
    ##  Vireo philadelphicus:1   Min.   :2022  
    ##  Acanthis flammea    :0   1st Qu.:2022  
    ##  Accipiter cooperii  :0   Median :2022  
    ##  Accipiter gentilis  :0   Mean   :2022  
    ##  Accipiter striatus  :0   3rd Qu.:2022  
    ##  Actitis macularius  :0   Max.   :2022  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-136.png)<!-- -->

    ##                species      eventDate   
    ##  Vireo gilvus      :106   Min.   :1901  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2015  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-137.png)<!-- -->

    ##                species      eventDate   
    ##  Vireo olivaceus   :444   Min.   :1975  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2019  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-138.png)<!-- -->

    ##                 species      eventDate   
    ##  Lanius ludovicianus:148   Min.   :1981  
    ##  Acanthis flammea   :  0   1st Qu.:2014  
    ##  Accipiter cooperii :  0   Median :2018  
    ##  Accipiter gentilis :  0   Mean   :2016  
    ##  Accipiter striatus :  0   3rd Qu.:2021  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-139.png)<!-- -->

    ##                species     eventDate   
    ##  Lanius borealis   :13   Min.   :2014  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2021  
    ##  Accipiter gentilis: 0   Mean   :2018  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-140.png)<!-- -->

    ##                       species     eventDate   
    ##  Gymnorhinus cyanocephalus:23   Min.   :1986  
    ##  Acanthis flammea         : 0   1st Qu.:1988  
    ##  Accipiter cooperii       : 0   Median :2017  
    ##  Accipiter gentilis       : 0   Mean   :2006  
    ##  Accipiter striatus       : 0   3rd Qu.:2020  
    ##  Actitis macularius       : 0   Max.   :2022  
    ##  (Other)                  : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-141.png)<!-- -->

    ##                 species      eventDate   
    ##  Cyanocitta cristata:995   Min.   :1975  
    ##  Acanthis flammea   :  0   1st Qu.:2015  
    ##  Accipiter cooperii :  0   Median :2018  
    ##  Accipiter gentilis :  0   Mean   :2017  
    ##  Accipiter striatus :  0   3rd Qu.:2020  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-142.png)<!-- -->

    ##                species      eventDate   
    ##  Pica hudsonia     :152   Min.   :1971  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2020  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-143.png)<!-- -->

    ##                  species     eventDate   
    ##  Nucifraga columbiana:16   Min.   :2017  
    ##  Acanthis flammea    : 0   1st Qu.:2017  
    ##  Accipiter cooperii  : 0   Median :2017  
    ##  Accipiter gentilis  : 0   Mean   :2017  
    ##  Accipiter striatus  : 0   3rd Qu.:2017  
    ##  Actitis macularius  : 0   Max.   :2017  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-144.png)<!-- -->

    ##                   species       eventDate   
    ##  Corvus brachyrhynchos:1250   Min.   :1975  
    ##  Acanthis flammea     :   0   1st Qu.:2015  
    ##  Accipiter cooperii   :   0   Median :2017  
    ##  Accipiter gentilis   :   0   Mean   :2017  
    ##  Accipiter striatus   :   0   3rd Qu.:2020  
    ##  Actitis macularius   :   0   Max.   :2023  
    ##  (Other)              :   0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-145.png)<!-- -->

    ##                  species       eventDate   
    ##  Poecile atricapillus:1790   Min.   :1971  
    ##  Acanthis flammea    :   0   1st Qu.:2015  
    ##  Accipiter cooperii  :   0   Median :2018  
    ##  Accipiter gentilis  :   0   Mean   :2017  
    ##  Accipiter striatus  :   0   3rd Qu.:2020  
    ##  Actitis macularius  :   0   Max.   :2023  
    ##  (Other)             :   0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-146.png)<!-- -->

    ##                  species      eventDate   
    ##  Eremophila alpestris:281   Min.   :1946  
    ##  Acanthis flammea    :  0   1st Qu.:2016  
    ##  Accipiter cooperii  :  0   Median :2019  
    ##  Accipiter gentilis  :  0   Mean   :2015  
    ##  Accipiter striatus  :  0   3rd Qu.:2021  
    ##  Actitis macularius  :  0   Max.   :2023  
    ##  (Other)             :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-147.png)<!-- -->

    ##                        species      eventDate   
    ##  Stelgidopteryx serripennis:196   Min.   :1975  
    ##  Acanthis flammea          :  0   1st Qu.:2012  
    ##  Accipiter cooperii        :  0   Median :2018  
    ##  Accipiter gentilis        :  0   Mean   :2015  
    ##  Accipiter striatus        :  0   3rd Qu.:2020  
    ##  Actitis macularius        :  0   Max.   :2023  
    ##  (Other)                   :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-148.png)<!-- -->

    ##                 species      eventDate   
    ##  Tachycineta bicolor:228   Min.   :1975  
    ##  Acanthis flammea   :  0   1st Qu.:2017  
    ##  Accipiter cooperii :  0   Median :2020  
    ##  Accipiter gentilis :  0   Mean   :2018  
    ##  Accipiter striatus :  0   3rd Qu.:2021  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-149.png)<!-- -->

    ##                    species      eventDate   
    ##  Tachycineta thalassina:247   Min.   :1984  
    ##  Acanthis flammea      :  0   1st Qu.:2015  
    ##  Accipiter cooperii    :  0   Median :2018  
    ##  Accipiter gentilis    :  0   Mean   :2016  
    ##  Accipiter striatus    :  0   3rd Qu.:2020  
    ##  Actitis macularius    :  0   Max.   :2023  
    ##  (Other)               :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-150.png)<!-- -->

    ##                species     eventDate   
    ##  Riparia riparia   :28   Min.   :1975  
    ##  Acanthis flammea  : 0   1st Qu.:2013  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2015  
    ##  Accipiter striatus: 0   3rd Qu.:2021  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-151.png)<!-- -->

    ##                species      eventDate   
    ##  Hirundo rustica   :534   Min.   :1984  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-152.png)<!-- -->

    ##                      species      eventDate   
    ##  Petrochelidon pyrrhonota:180   Min.   :1975  
    ##  Acanthis flammea        :  0   1st Qu.:2015  
    ##  Accipiter cooperii      :  0   Median :2018  
    ##  Accipiter gentilis      :  0   Mean   :2016  
    ##  Accipiter striatus      :  0   3rd Qu.:2020  
    ##  Actitis macularius      :  0   Max.   :2023  
    ##  (Other)                 :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-153.png)<!-- -->

    ##                 species     eventDate   
    ##  Corthylio calendula:84   Min.   :2003  
    ##  Acanthis flammea   : 0   1st Qu.:2015  
    ##  Accipiter cooperii : 0   Median :2016  
    ##  Accipiter gentilis : 0   Mean   :2016  
    ##  Accipiter striatus : 0   3rd Qu.:2018  
    ##  Actitis macularius : 0   Max.   :2022  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-154.png)<!-- -->

    ##                species    eventDate   
    ##  Regulus satrapa   :5   Min.   :2017  
    ##  Acanthis flammea  :0   1st Qu.:2017  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2018  
    ##  Accipiter striatus:0   3rd Qu.:2017  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-155.png)<!-- -->

    ##                species      eventDate   
    ##  Sitta canadensis  :902   Min.   :1975  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-156.png)<!-- -->

    ##                species       eventDate   
    ##  Sitta carolinensis:1077   Min.   :1975  
    ##  Acanthis flammea  :   0   1st Qu.:2015  
    ##  Accipiter cooperii:   0   Median :2017  
    ##  Accipiter gentilis:   0   Mean   :2017  
    ##  Accipiter striatus:   0   3rd Qu.:2020  
    ##  Actitis macularius:   0   Max.   :2023  
    ##  (Other)           :   0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-157.png)<!-- -->

    ##                species       eventDate   
    ##  Sitta pygmaea     :1123   Min.   :1975  
    ##  Acanthis flammea  :   0   1st Qu.:2015  
    ##  Accipiter cooperii:   0   Median :2017  
    ##  Accipiter gentilis:   0   Mean   :2017  
    ##  Accipiter striatus:   0   3rd Qu.:2020  
    ##  Actitis macularius:   0   Max.   :2023  
    ##  (Other)           :   0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-158.png)<!-- -->

    ##                species      eventDate   
    ##  Certhia americana :105   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2010  
    ##  Accipiter cooperii:  0   Median :2016  
    ##  Accipiter gentilis:  0   Mean   :2014  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2022  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-159.png)<!-- -->

    ##                 species     eventDate   
    ##  Polioptila caerulea:85   Min.   :2013  
    ##  Acanthis flammea   : 0   1st Qu.:2017  
    ##  Accipiter cooperii : 0   Median :2019  
    ##  Accipiter gentilis : 0   Mean   :2019  
    ##  Accipiter striatus : 0   3rd Qu.:2021  
    ##  Actitis macularius : 0   Max.   :2023  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-160.png)<!-- -->

    ##                  species      eventDate   
    ##  Salpinctes obsoletus:228   Min.   :1984  
    ##  Acanthis flammea    :  0   1st Qu.:2015  
    ##  Accipiter cooperii  :  0   Median :2018  
    ##  Accipiter gentilis  :  0   Mean   :2016  
    ##  Accipiter striatus  :  0   3rd Qu.:2020  
    ##  Actitis macularius  :  0   Max.   :2023  
    ##  (Other)             :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-161.png)<!-- -->

    ##                species       eventDate   
    ##  Troglodytes aedon :1394   Min.   :1975  
    ##  Acanthis flammea  :   0   1st Qu.:2015  
    ##  Accipiter cooperii:   0   Median :2018  
    ##  Accipiter gentilis:   0   Mean   :2017  
    ##  Accipiter striatus:   0   3rd Qu.:2020  
    ##  Actitis macularius:   0   Max.   :2023  
    ##  (Other)           :   0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-162.png)<!-- -->

    ##                   species    eventDate   
    ##  Troglodytes pacificus:4   Min.   :2017  
    ##  Acanthis flammea     :0   1st Qu.:2017  
    ##  Accipiter cooperii   :0   Median :2017  
    ##  Accipiter gentilis   :0   Mean   :2017  
    ##  Accipiter striatus   :0   3rd Qu.:2017  
    ##  Actitis macularius   :0   Max.   :2017  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-163.png)<!-- -->

    ##                  species    eventDate   
    ##  Troglodytes hiemalis:2   Min.   :2018  
    ##  Acanthis flammea    :0   1st Qu.:2018  
    ##  Accipiter cooperii  :0   Median :2018  
    ##  Accipiter gentilis  :0   Mean   :2018  
    ##  Accipiter striatus  :0   3rd Qu.:2018  
    ##  Actitis macularius  :0   Max.   :2018  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-164.png)<!-- -->

    ##                   species    eventDate   
    ##  Cistothorus palustris:5   Min.   :2020  
    ##  Acanthis flammea     :0   1st Qu.:2020  
    ##  Accipiter cooperii   :0   Median :2021  
    ##  Accipiter gentilis   :0   Mean   :2021  
    ##  Accipiter striatus   :0   3rd Qu.:2021  
    ##  Actitis macularius   :0   Max.   :2022  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-165.png)<!-- -->

    ##                species      eventDate   
    ##  Sturnus vulgaris  :672   Min.   :1975  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-166.png)<!-- -->

    ##                    species      eventDate   
    ##  Dumetella carolinensis:258   Min.   :1996  
    ##  Acanthis flammea      :  0   1st Qu.:2015  
    ##  Accipiter cooperii    :  0   Median :2018  
    ##  Accipiter gentilis    :  0   Mean   :2017  
    ##  Accipiter striatus    :  0   3rd Qu.:2020  
    ##  Actitis macularius    :  0   Max.   :2023  
    ##  (Other)               :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-167.png)<!-- -->

    ##                   species    eventDate   
    ##  Toxostoma curvirostre:2   Min.   :2016  
    ##  Acanthis flammea     :0   1st Qu.:2016  
    ##  Accipiter cooperii   :0   Median :2016  
    ##  Accipiter gentilis   :0   Mean   :2016  
    ##  Accipiter striatus   :0   3rd Qu.:2016  
    ##  Actitis macularius   :0   Max.   :2016  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-168.png)<!-- -->

    ##                species      eventDate   
    ##  Toxostoma rufum   :472   Min.   :1985  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-169.png)<!-- -->

    ##                  species    eventDate   
    ##  Oreoscoptes montanus:5   Min.   :1901  
    ##  Acanthis flammea    :0   1st Qu.:2012  
    ##  Accipiter cooperii  :0   Median :2012  
    ##  Accipiter gentilis  :0   Mean   :1991  
    ##  Accipiter striatus  :0   3rd Qu.:2016  
    ##  Actitis macularius  :0   Max.   :2016  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-170.png)<!-- -->

    ##                species    eventDate   
    ##  Mimus polyglottos :5   Min.   :2018  
    ##  Acanthis flammea  :0   1st Qu.:2018  
    ##  Accipiter cooperii:0   Median :2019  
    ##  Accipiter gentilis:0   Mean   :2019  
    ##  Accipiter striatus:0   3rd Qu.:2019  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-171.png)<!-- -->

    ##                species      eventDate   
    ##  Sialia sialis     :884   Min.   :1998  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2019  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-172.png)<!-- -->

    ##                species      eventDate   
    ##  Sialia currucoides:598   Min.   :1984  
    ##  Acanthis flammea  :  0   1st Qu.:2014  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2015  
    ##  Accipiter striatus:  0   3rd Qu.:2019  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-173.png)<!-- -->

    ##                         species    eventDate   
    ##  Sialia sialis x currucoides:1   Min.   :1985  
    ##  Acanthis flammea           :0   1st Qu.:1985  
    ##  Accipiter cooperii         :0   Median :1985  
    ##  Accipiter gentilis         :0   Mean   :1985  
    ##  Accipiter striatus         :0   3rd Qu.:1985  
    ##  Actitis macularius         :0   Max.   :1985  
    ##  (Other)                    :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-174.png)<!-- -->

    ##                 species      eventDate   
    ##  Myadestes townsendi:152   Min.   :1978  
    ##  Acanthis flammea   :  0   1st Qu.:2015  
    ##  Accipiter cooperii :  0   Median :2017  
    ##  Accipiter gentilis :  0   Mean   :2016  
    ##  Accipiter striatus :  0   3rd Qu.:2020  
    ##  Actitis macularius :  0   Max.   :2022  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-175.png)<!-- -->

    ##                 species    eventDate   
    ##  Catharus fuscescens:6   Min.   :2009  
    ##  Acanthis flammea   :0   1st Qu.:2013  
    ##  Accipiter cooperii :0   Median :2016  
    ##  Accipiter gentilis :0   Mean   :2016  
    ##  Accipiter striatus :0   3rd Qu.:2018  
    ##  Actitis macularius :0   Max.   :2023  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-176.png)<!-- -->

    ##                species      eventDate   
    ##  Catharus ustulatus:229   Min.   :1973  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-177.png)<!-- -->

    ##                species     eventDate   
    ##  Catharus guttatus :41   Min.   :2010  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2018  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-178.png)<!-- -->

    ##                species       eventDate   
    ##  Turdus migratorius:1777   Min.   :1959  
    ##  Acanthis flammea  :   0   1st Qu.:2015  
    ##  Accipiter cooperii:   0   Median :2018  
    ##  Accipiter gentilis:   0   Mean   :2017  
    ##  Accipiter striatus:   0   3rd Qu.:2020  
    ##  Actitis macularius:   0   Max.   :2023  
    ##  (Other)           :   0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-179.png)<!-- -->

    ##                 species      eventDate   
    ##  Bombycilla cedrorum:558   Min.   :1984  
    ##  Acanthis flammea   :  0   1st Qu.:2015  
    ##  Accipiter cooperii :  0   Median :2018  
    ##  Accipiter gentilis :  0   Mean   :2017  
    ##  Accipiter striatus :  0   3rd Qu.:2020  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-180.png)<!-- -->

    ##                species     eventDate   
    ##  Passer domesticus :71   Min.   :1975  
    ##  Acanthis flammea  : 0   1st Qu.:2008  
    ##  Accipiter cooperii: 0   Median :2013  
    ##  Accipiter gentilis: 0   Mean   :2010  
    ##  Accipiter striatus: 0   3rd Qu.:2017  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-181.png)<!-- -->

    ##                species    eventDate   
    ##  Anthus rubescens  :3   Min.   :2017  
    ##  Acanthis flammea  :0   1st Qu.:2017  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2019  
    ##  Accipiter striatus:0   3rd Qu.:2020  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-182.png)<!-- -->

    ##                     species     eventDate   
    ##  Leucosticte tephrocotis:15   Min.   :2020  
    ##  Acanthis flammea       : 0   1st Qu.:2020  
    ##  Accipiter cooperii     : 0   Median :2020  
    ##  Accipiter gentilis     : 0   Mean   :2020  
    ##  Accipiter striatus     : 0   3rd Qu.:2020  
    ##  Actitis macularius     : 0   Max.   :2020  
    ##  (Other)                : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-183.png)<!-- -->

    ##                  species      eventDate   
    ##  Haemorhous mexicanus:226   Min.   :2000  
    ##  Acanthis flammea    :  0   1st Qu.:2015  
    ##  Accipiter cooperii  :  0   Median :2018  
    ##  Accipiter gentilis  :  0   Mean   :2017  
    ##  Accipiter striatus  :  0   3rd Qu.:2020  
    ##  Actitis macularius  :  0   Max.   :2023  
    ##  (Other)             :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-184.png)<!-- -->

    ##                  species    eventDate   
    ##  Haemorhous purpureus:2   Min.   :2014  
    ##  Acanthis flammea    :0   1st Qu.:2014  
    ##  Accipiter cooperii  :0   Median :2014  
    ##  Accipiter gentilis  :0   Mean   :2014  
    ##  Accipiter striatus  :0   3rd Qu.:2014  
    ##  Actitis macularius  :0   Max.   :2014  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-185.png)<!-- -->

    ##                 species     eventDate   
    ##  Haemorhous cassinii:12   Min.   :1985  
    ##  Acanthis flammea   : 0   1st Qu.:2015  
    ##  Accipiter cooperii : 0   Median :2017  
    ##  Accipiter gentilis : 0   Mean   :2012  
    ##  Accipiter striatus : 0   3rd Qu.:2017  
    ##  Actitis macularius : 0   Max.   :2020  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-186.png)<!-- -->

    ##                  species    eventDate   
    ##  Acanthis flammea    :9   Min.   :2015  
    ##  Accipiter cooperii  :0   1st Qu.:2020  
    ##  Accipiter gentilis  :0   Median :2021  
    ##  Accipiter striatus  :0   Mean   :2019  
    ##  Actitis macularius  :0   3rd Qu.:2021  
    ##  Aechmophorus clarkii:0   Max.   :2021  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-187.png)<!-- -->

    ##                species      eventDate   
    ##  Loxia curvirostra :939   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-188.png)<!-- -->

    ##                species      eventDate   
    ##  Spinus pinus      :302   Min.   :1985  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-189.png)<!-- -->

    ##                species    eventDate   
    ##  Spinus psaltria   :3   Min.   :2020  
    ##  Acanthis flammea  :0   1st Qu.:2020  
    ##  Accipiter cooperii:0   Median :2020  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-190.png)<!-- -->

    ##                species       eventDate   
    ##  Spinus tristis    :1557   Min.   :1984  
    ##  Acanthis flammea  :   0   1st Qu.:2015  
    ##  Accipiter cooperii:   0   Median :2017  
    ##  Accipiter gentilis:   0   Mean   :2017  
    ##  Accipiter striatus:   0   3rd Qu.:2020  
    ##  Actitis macularius:   0   Max.   :2023  
    ##  (Other)           :   0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-191.png)<!-- -->

    ##                  species    eventDate   
    ##  Calcarius lapponicus:2   Min.   :2016  
    ##  Acanthis flammea    :0   1st Qu.:2016  
    ##  Accipiter cooperii  :0   Median :2016  
    ##  Accipiter gentilis  :0   Mean   :2016  
    ##  Accipiter striatus  :0   3rd Qu.:2016  
    ##  Actitis macularius  :0   Max.   :2016  
    ##  (Other)             :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-192.png)<!-- -->

    ##                species     eventDate   
    ##  Calcarius ornatus :11   Min.   :1995  
    ##  Acanthis flammea  : 0   1st Qu.:2013  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2014  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-193.png)<!-- -->

    ##                    species    eventDate   
    ##  Rhynchophanes mccownii:8   Min.   :1995  
    ##  Acanthis flammea      :0   1st Qu.:2006  
    ##  Accipiter cooperii    :0   Median :2012  
    ##  Accipiter gentilis    :0   Mean   :2010  
    ##  Accipiter striatus    :0   3rd Qu.:2016  
    ##  Actitis macularius    :0   Max.   :2023  
    ##  (Other)               :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-194.png)<!-- -->

    ##                species    eventDate   
    ##  Peucaea cassinii  :2   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2011  
    ##  Accipiter cooperii:0   Median :2011  
    ##  Accipiter gentilis:0   Mean   :2011  
    ##  Accipiter striatus:0   3rd Qu.:2011  
    ##  Actitis macularius:0   Max.   :2011  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-195.png)<!-- -->

    ##                   species      eventDate   
    ##  Ammodramus savannarum:130   Min.   :1986  
    ##  Acanthis flammea     :  0   1st Qu.:2015  
    ##  Accipiter cooperii   :  0   Median :2018  
    ##  Accipiter gentilis   :  0   Mean   :2016  
    ##  Accipiter striatus   :  0   3rd Qu.:2020  
    ##  Actitis macularius   :  0   Max.   :2023  
    ##  (Other)              :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-196.png)<!-- -->

    ##                species      eventDate   
    ##  Spizella passerina:938   Min.   :1959  
    ##  Acanthis flammea  :  0   1st Qu.:2014  
    ##  Accipiter cooperii:  0   Median :2016  
    ##  Accipiter gentilis:  0   Mean   :2015  
    ##  Accipiter striatus:  0   3rd Qu.:2019  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-197.png)<!-- -->

    ##                species      eventDate   
    ##  Spizella pallida  :258   Min.   :1986  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-198.png)<!-- -->

    ##                species     eventDate   
    ##  Spizella pusilla  :30   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-199.png)<!-- -->

    ##                species     eventDate   
    ##  Spizella breweri  :36   Min.   :1978  
    ##  Acanthis flammea  : 0   1st Qu.:2012  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2015  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-200.png)<!-- -->

    ##                  species      eventDate   
    ##  Chondestes grammacus:768   Min.   :1986  
    ##  Acanthis flammea    :  0   1st Qu.:2016  
    ##  Accipiter cooperii  :  0   Median :2019  
    ##  Accipiter gentilis  :  0   Mean   :2017  
    ##  Accipiter striatus  :  0   3rd Qu.:2021  
    ##  Actitis macularius  :  0   Max.   :2023  
    ##  (Other)             :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-201.png)<!-- -->

    ##                  species     eventDate   
    ##  Spizelloides arborea:26   Min.   :2013  
    ##  Acanthis flammea    : 0   1st Qu.:2017  
    ##  Accipiter cooperii  : 0   Median :2017  
    ##  Accipiter gentilis  : 0   Mean   :2017  
    ##  Accipiter striatus  : 0   3rd Qu.:2019  
    ##  Actitis macularius  : 0   Max.   :2021  
    ##  (Other)             : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-202.png)<!-- -->

    ##                species    eventDate   
    ##  Passerella iliaca :2   Min.   :2016  
    ##  Acanthis flammea  :0   1st Qu.:2016  
    ##  Accipiter cooperii:0   Median :2016  
    ##  Accipiter gentilis:0   Mean   :2016  
    ##  Accipiter striatus:0   3rd Qu.:2016  
    ##  Actitis macularius:0   Max.   :2016  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-203.png)<!-- -->

    ##                species      eventDate   
    ##  Junco sp.         :281   Min.   :1994  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-204.png)<!-- -->

    ##                species      eventDate   
    ##  Junco hyemalis    :130   Min.   :1994  
    ##  Acanthis flammea  :  0   1st Qu.:2014  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2022  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-205.png)<!-- -->

    ##                    species      eventDate   
    ##  Zonotrichia leucophrys:283   Min.   :2009  
    ##  Acanthis flammea      :  0   1st Qu.:2015  
    ##  Accipiter cooperii    :  0   Median :2016  
    ##  Accipiter gentilis    :  0   Mean   :2016  
    ##  Accipiter striatus    :  0   3rd Qu.:2018  
    ##  Actitis macularius    :  0   Max.   :2023  
    ##  (Other)               :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-206.png)<!-- -->

    ##                 species    eventDate   
    ##  Zonotrichia querula:7   Min.   :2014  
    ##  Acanthis flammea   :0   1st Qu.:2016  
    ##  Accipiter cooperii :0   Median :2017  
    ##  Accipiter gentilis :0   Mean   :2017  
    ##  Accipiter striatus :0   3rd Qu.:2018  
    ##  Actitis macularius :0   Max.   :2022  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-207.png)<!-- -->

    ##                    species     eventDate   
    ##  Zonotrichia albicollis:58   Min.   :2010  
    ##  Acanthis flammea      : 0   1st Qu.:2015  
    ##  Accipiter cooperii    : 0   Median :2016  
    ##  Accipiter gentilis    : 0   Mean   :2016  
    ##  Accipiter striatus    : 0   3rd Qu.:2018  
    ##  Actitis macularius    : 0   Max.   :2023  
    ##  (Other)               : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-208.png)<!-- -->

    ##                 species      eventDate   
    ##  Pooecetes gramineus:142   Min.   :1984  
    ##  Acanthis flammea   :  0   1st Qu.:2014  
    ##  Accipiter cooperii :  0   Median :2018  
    ##  Accipiter gentilis :  0   Mean   :2016  
    ##  Accipiter striatus :  0   3rd Qu.:2020  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-209.png)<!-- -->

    ##                       species     eventDate   
    ##  Passerculus sandwichensis:46   Min.   :1985  
    ##  Acanthis flammea         : 0   1st Qu.:2016  
    ##  Accipiter cooperii       : 0   Median :2017  
    ##  Accipiter gentilis       : 0   Mean   :2015  
    ##  Accipiter striatus       : 0   3rd Qu.:2018  
    ##  Actitis macularius       : 0   Max.   :2021  
    ##  (Other)                  : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-210.png)<!-- -->

    ##                species     eventDate   
    ##  Centronyx bairdii :15   Min.   :2010  
    ##  Acanthis flammea  : 0   1st Qu.:2010  
    ##  Accipiter cooperii: 0   Median :2010  
    ##  Accipiter gentilis: 0   Mean   :2011  
    ##  Accipiter striatus: 0   3rd Qu.:2010  
    ##  Actitis macularius: 0   Max.   :2019  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-211.png)<!-- -->

    ##                species    eventDate   
    ##  Pipilo chlorurus  :2   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2011  
    ##  Accipiter cooperii:0   Median :2011  
    ##  Accipiter gentilis:0   Mean   :2011  
    ##  Accipiter striatus:0   3rd Qu.:2011  
    ##  Actitis macularius:0   Max.   :2011  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-212.png)<!-- -->

    ##                species       eventDate   
    ##  Pipilo maculatus  :1541   Min.   :1957  
    ##  Acanthis flammea  :   0   1st Qu.:2015  
    ##  Accipiter cooperii:   0   Median :2017  
    ##  Accipiter gentilis:   0   Mean   :2016  
    ##  Accipiter striatus:   0   3rd Qu.:2020  
    ##  Actitis macularius:   0   Max.   :2023  
    ##  (Other)           :   0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-213.png)<!-- -->

    ##                species      eventDate   
    ##  Icteria virens    :596   Min.   :1984  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2019  
    ##  Accipiter gentilis:  0   Mean   :2018  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-214.png)<!-- -->

    ##                           species     eventDate   
    ##  Xanthocephalus xanthocephalus:51   Min.   :2013  
    ##  Acanthis flammea             : 0   1st Qu.:2016  
    ##  Accipiter cooperii           : 0   Median :2018  
    ##  Accipiter gentilis           : 0   Mean   :2018  
    ##  Accipiter striatus           : 0   3rd Qu.:2019  
    ##  Actitis macularius           : 0   Max.   :2023  
    ##  (Other)                      : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-215.png)<!-- -->

    ##                   species     eventDate   
    ##  Dolichonyx oryzivorus:21   Min.   :1975  
    ##  Acanthis flammea     : 0   1st Qu.:2017  
    ##  Accipiter cooperii   : 0   Median :2020  
    ##  Accipiter gentilis   : 0   Mean   :2015  
    ##  Accipiter striatus   : 0   3rd Qu.:2021  
    ##  Actitis macularius   : 0   Max.   :2023  
    ##  (Other)              : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-216.png)<!-- -->

    ##                species       eventDate   
    ##  Sturnella neglecta:1169   Min.   :1975  
    ##  Acanthis flammea  :   0   1st Qu.:2017  
    ##  Accipiter cooperii:   0   Median :2019  
    ##  Accipiter gentilis:   0   Mean   :2017  
    ##  Accipiter striatus:   0   3rd Qu.:2021  
    ##  Actitis macularius:   0   Max.   :2023  
    ##  (Other)           :   0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-217.png)<!-- -->

    ##                species    eventDate   
    ##  Sturnella magna   :8   Min.   :2011  
    ##  Acanthis flammea  :0   1st Qu.:2012  
    ##  Accipiter cooperii:0   Median :2015  
    ##  Accipiter gentilis:0   Mean   :2015  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2019  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-218.png)<!-- -->

    ##                      species     eventDate   
    ##  Sturnella neglecta/magna:25   Min.   :2002  
    ##  Acanthis flammea        : 0   1st Qu.:2015  
    ##  Accipiter cooperii      : 0   Median :2017  
    ##  Accipiter gentilis      : 0   Mean   :2016  
    ##  Accipiter striatus      : 0   3rd Qu.:2019  
    ##  Actitis macularius      : 0   Max.   :2023  
    ##  (Other)                 : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-219.png)<!-- -->

    ##                species      eventDate   
    ##  Icterus bullockii :186   Min.   :1955  
    ##  Acanthis flammea  :  0   1st Qu.:2018  
    ##  Accipiter cooperii:  0   Median :2020  
    ##  Accipiter gentilis:  0   Mean   :2018  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-220.png)<!-- -->

    ##                species     eventDate   
    ##  Icterus galbula   :23   Min.   :2011  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-221.png)<!-- -->

    ##                         species    eventDate   
    ##  Icterus bullockii x galbula:2   Min.   :2021  
    ##  Acanthis flammea           :0   1st Qu.:2021  
    ##  Accipiter cooperii         :0   Median :2022  
    ##  Accipiter gentilis         :0   Mean   :2022  
    ##  Accipiter striatus         :0   3rd Qu.:2022  
    ##  Actitis macularius         :0   Max.   :2022  
    ##  (Other)                    :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-222.png)<!-- -->

    ##                       species    eventDate   
    ##  Icterus bullockii/galbula:9   Min.   :1986  
    ##  Acanthis flammea         :0   1st Qu.:2012  
    ##  Accipiter cooperii       :0   Median :2017  
    ##  Accipiter gentilis       :0   Mean   :2011  
    ##  Accipiter striatus       :0   3rd Qu.:2021  
    ##  Actitis macularius       :0   Max.   :2022  
    ##  (Other)                  :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-223.png)<!-- -->

    ##                 species      eventDate   
    ##  Agelaius phoeniceus:614   Min.   :1975  
    ##  Acanthis flammea   :  0   1st Qu.:2017  
    ##  Accipiter cooperii :  0   Median :2020  
    ##  Accipiter gentilis :  0   Mean   :2018  
    ##  Accipiter striatus :  0   3rd Qu.:2021  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-224.png)<!-- -->

    ##                species      eventDate   
    ##  Molothrus ater    :461   Min.   :1995  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2019  
    ##  Accipiter gentilis:  0   Mean   :2018  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-225.png)<!-- -->

    ##                    species      eventDate   
    ##  Euphagus cyanocephalus:134   Min.   :1971  
    ##  Acanthis flammea      :  0   1st Qu.:2013  
    ##  Accipiter cooperii    :  0   Median :2019  
    ##  Accipiter gentilis    :  0   Mean   :2015  
    ##  Accipiter striatus    :  0   3rd Qu.:2020  
    ##  Actitis macularius    :  0   Max.   :2023  
    ##  (Other)               :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-226.png)<!-- -->

    ##                species      eventDate   
    ##  Quiscalus quiscula:456   Min.   :1975  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2020  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-227.png)<!-- -->

    ##                 species    eventDate   
    ##  Quiscalus mexicanus:4   Min.   :2010  
    ##  Acanthis flammea   :0   1st Qu.:2010  
    ##  Accipiter cooperii :0   Median :2014  
    ##  Accipiter gentilis :0   Mean   :2014  
    ##  Accipiter striatus :0   3rd Qu.:2019  
    ##  Actitis macularius :0   Max.   :2019  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-228.png)<!-- -->

    ##                 species      eventDate   
    ##  Seiurus aurocapilla:286   Min.   :1959  
    ##  Acanthis flammea   :  0   1st Qu.:2013  
    ##  Accipiter cooperii :  0   Median :2017  
    ##  Accipiter gentilis :  0   Mean   :2015  
    ##  Accipiter striatus :  0   3rd Qu.:2020  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-229.png)<!-- -->

    ##                     species     eventDate   
    ##  Parkesia noveboracensis:10   Min.   :1986  
    ##  Acanthis flammea       : 0   1st Qu.:2011  
    ##  Accipiter cooperii     : 0   Median :2018  
    ##  Accipiter gentilis     : 0   Mean   :2011  
    ##  Accipiter striatus     : 0   3rd Qu.:2018  
    ##  Actitis macularius     : 0   Max.   :2021  
    ##  (Other)                : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-230.png)<!-- -->

    ##                species      eventDate   
    ##  Mniotilta varia   :119   Min.   :1984  
    ##  Acanthis flammea  :  0   1st Qu.:2010  
    ##  Accipiter cooperii:  0   Median :2017  
    ##  Accipiter gentilis:  0   Mean   :2014  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-231.png)<!-- -->

    ##                   species     eventDate   
    ##  Leiothlypis peregrina:17   Min.   :2000  
    ##  Acanthis flammea     : 0   1st Qu.:2013  
    ##  Accipiter cooperii   : 0   Median :2014  
    ##  Accipiter gentilis   : 0   Mean   :2014  
    ##  Accipiter striatus   : 0   3rd Qu.:2020  
    ##  Actitis macularius   : 0   Max.   :2022  
    ##  (Other)              : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-232.png)<!-- -->

    ##                species      eventDate   
    ##  Leiothlypis celata:470   Min.   :2009  
    ##  Acanthis flammea  :  0   1st Qu.:2014  
    ##  Accipiter cooperii:  0   Median :2016  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2018  
    ##  Actitis macularius:  0   Max.   :2022  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-233.png)<!-- -->

    ##                     species     eventDate   
    ##  Leiothlypis ruficapilla:12   Min.   :2010  
    ##  Acanthis flammea       : 0   1st Qu.:2013  
    ##  Accipiter cooperii     : 0   Median :2014  
    ##  Accipiter gentilis     : 0   Mean   :2015  
    ##  Accipiter striatus     : 0   3rd Qu.:2018  
    ##  Actitis macularius     : 0   Max.   :2018  
    ##  (Other)                : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-234.png)<!-- -->

    ##                   species    eventDate   
    ##  Leiothlypis virginiae:2   Min.   :2019  
    ##  Acanthis flammea     :0   1st Qu.:2019  
    ##  Accipiter cooperii   :0   Median :2019  
    ##  Accipiter gentilis   :0   Mean   :2019  
    ##  Accipiter striatus   :0   3rd Qu.:2019  
    ##  Actitis macularius   :0   Max.   :2019  
    ##  (Other)              :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-235.png)<!-- -->

    ##                species     eventDate   
    ##  Geothlypis tolmiei:27   Min.   :2010  
    ##  Acanthis flammea  : 0   1st Qu.:2016  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-236.png)<!-- -->

    ##                species      eventDate   
    ##  Geothlypis trichas:142   Min.   :2000  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-237.png)<!-- -->

    ##                 species      eventDate   
    ##  Setophaga ruticilla:410   Min.   :1975  
    ##  Acanthis flammea   :  0   1st Qu.:2015  
    ##  Accipiter cooperii :  0   Median :2018  
    ##  Accipiter gentilis :  0   Mean   :2016  
    ##  Accipiter striatus :  0   3rd Qu.:2021  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-238.png)<!-- -->

    ##                species      eventDate   
    ##  Setophaga petechia:749   Min.   :1984  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2019  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-239.png)<!-- -->

    ##                    species    eventDate   
    ##  Setophaga pensylvanica:3   Min.   :2013  
    ##  Acanthis flammea      :0   1st Qu.:2013  
    ##  Accipiter cooperii    :0   Median :2013  
    ##  Accipiter gentilis    :0   Mean   :2016  
    ##  Accipiter striatus    :0   3rd Qu.:2018  
    ##  Actitis macularius    :0   Max.   :2023  
    ##  (Other)               :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-240.png)<!-- -->

    ##                species     eventDate   
    ##  Setophaga striata :22   Min.   :2003  
    ##  Acanthis flammea  : 0   1st Qu.:2010  
    ##  Accipiter cooperii: 0   Median :2014  
    ##  Accipiter gentilis: 0   Mean   :2014  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-241.png)<!-- -->

    ##                    species    eventDate   
    ##  Setophaga caerulescens:2   Min.   :2021  
    ##  Acanthis flammea      :0   1st Qu.:2021  
    ##  Accipiter cooperii    :0   Median :2021  
    ##  Accipiter gentilis    :0   Mean   :2021  
    ##  Accipiter striatus    :0   3rd Qu.:2021  
    ##  Actitis macularius    :0   Max.   :2021  
    ##  (Other)               :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-242.png)<!-- -->

    ##                species    eventDate   
    ##  Setophaga palmarum:8   Min.   :2009  
    ##  Acanthis flammea  :0   1st Qu.:2014  
    ##  Accipiter cooperii:0   Median :2017  
    ##  Accipiter gentilis:0   Mean   :2016  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2019  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-243.png)<!-- -->

    ##                         species      eventDate   
    ##  Setophaga coronata/auduboni:143   Min.   :1985  
    ##  Acanthis flammea           :  0   1st Qu.:2014  
    ##  Accipiter cooperii         :  0   Median :2018  
    ##  Accipiter gentilis         :  0   Mean   :2016  
    ##  Accipiter striatus         :  0   3rd Qu.:2020  
    ##  Actitis macularius         :  0   Max.   :2023  
    ##  (Other)                    :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-244.png)<!-- -->

    ##                species      eventDate   
    ##  Setophaga coronata:408   Min.   :1985  
    ##  Acanthis flammea  :  0   1st Qu.:2014  
    ##  Accipiter cooperii:  0   Median :2016  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2019  
    ##  Actitis macularius:  0   Max.   :2022  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-245.png)<!-- -->

    ##                 species     eventDate   
    ##  Setophaga townsendi:21   Min.   :2014  
    ##  Acanthis flammea   : 0   1st Qu.:2014  
    ##  Accipiter cooperii : 0   Median :2015  
    ##  Accipiter gentilis : 0   Mean   :2016  
    ##  Accipiter striatus : 0   3rd Qu.:2017  
    ##  Actitis macularius : 0   Max.   :2021  
    ##  (Other)            : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-246.png)<!-- -->

    ##                species      eventDate   
    ##  Cardellina pusilla:404   Min.   :2010  
    ##  Acanthis flammea  :  0   1st Qu.:2014  
    ##  Accipiter cooperii:  0   Median :2016  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2018  
    ##  Actitis macularius:  0   Max.   :2022  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-247.png)<!-- -->

    ##                species    eventDate   
    ##  Piranga rubra     :2   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-248.png)<!-- -->

    ##                 species      eventDate   
    ##  Piranga ludoviciana:343   Min.   :1984  
    ##  Acanthis flammea   :  0   1st Qu.:2014  
    ##  Accipiter cooperii :  0   Median :2017  
    ##  Accipiter gentilis :  0   Mean   :2015  
    ##  Accipiter striatus :  0   3rd Qu.:2020  
    ##  Actitis macularius :  0   Max.   :2023  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-249.png)<!-- -->

    ##                   species     eventDate   
    ##  Cardinalis cardinalis:11   Min.   :2012  
    ##  Acanthis flammea     : 0   1st Qu.:2015  
    ##  Accipiter cooperii   : 0   Median :2018  
    ##  Accipiter gentilis   : 0   Mean   :2018  
    ##  Accipiter striatus   : 0   3rd Qu.:2021  
    ##  Actitis macularius   : 0   Max.   :2023  
    ##  (Other)              : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-250.png)<!-- -->

    ##                     species    eventDate   
    ##  Pheucticus ludovicianus:8   Min.   :2009  
    ##  Acanthis flammea       :0   1st Qu.:2018  
    ##  Accipiter cooperii     :0   Median :2021  
    ##  Accipiter gentilis     :0   Mean   :2018  
    ##  Accipiter striatus     :0   3rd Qu.:2021  
    ##  Actitis macularius     :0   Max.   :2022  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-251.png)<!-- -->

    ##                       species      eventDate   
    ##  Pheucticus melanocephalus:553   Min.   :1955  
    ##  Acanthis flammea         :  0   1st Qu.:2015  
    ##  Accipiter cooperii       :  0   Median :2019  
    ##  Accipiter gentilis       :  0   Mean   :2016  
    ##  Accipiter striatus       :  0   3rd Qu.:2021  
    ##  Actitis macularius       :  0   Max.   :2023  
    ##  (Other)                  :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-252.png)<!-- -->

    ##                                      species    eventDate   
    ##  Pheucticus ludovicianus x melanocephalus:1   Min.   :2018  
    ##  Acanthis flammea                        :0   1st Qu.:2018  
    ##  Accipiter cooperii                      :0   Median :2018  
    ##  Accipiter gentilis                      :0   Mean   :2018  
    ##  Accipiter striatus                      :0   3rd Qu.:2018  
    ##  Actitis macularius                      :0   Max.   :2018  
    ##  (Other)                                 :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-253.png)<!-- -->

    ##                                    species    eventDate   
    ##  Pheucticus ludovicianus/melanocephalus:5   Min.   :2013  
    ##  Acanthis flammea                      :0   1st Qu.:2018  
    ##  Accipiter cooperii                    :0   Median :2020  
    ##  Accipiter gentilis                    :0   Mean   :2018  
    ##  Accipiter striatus                    :0   3rd Qu.:2020  
    ##  Actitis macularius                    :0   Max.   :2021  
    ##  (Other)                               :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-254.png)<!-- -->

    ##                species     eventDate   
    ##  Passerina caerulea:55   Min.   :1969  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2022  
    ##  Actitis macularius: 0   Max.   :2023  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-255.png)<!-- -->

    ##                species      eventDate   
    ##  Passerina amoena  :133   Min.   :1965  
    ##  Acanthis flammea  :  0   1st Qu.:2015  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2017  
    ##  Accipiter striatus:  0   3rd Qu.:2021  
    ##  Actitis macularius:  0   Max.   :2023  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-256.png)<!-- -->

    ##                species     eventDate   
    ##  Passerina cyanea  :48   Min.   :1991  
    ##  Acanthis flammea  : 0   1st Qu.:2013  
    ##  Accipiter cooperii: 0   Median :2015  
    ##  Accipiter gentilis: 0   Mean   :2014  
    ##  Accipiter striatus: 0   3rd Qu.:2019  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-257.png)<!-- -->

    ##                       species    eventDate   
    ##  Passerina amoena x cyanea:6   Min.   :2006  
    ##  Acanthis flammea         :0   1st Qu.:2014  
    ##  Accipiter cooperii       :0   Median :2019  
    ##  Accipiter gentilis       :0   Mean   :2016  
    ##  Accipiter striatus       :0   3rd Qu.:2020  
    ##  Actitis macularius       :0   Max.   :2022  
    ##  (Other)                  :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-258.png)<!-- -->

    ##                     species     eventDate   
    ##  Passerina amoena/cyanea:13   Min.   :2014  
    ##  Acanthis flammea       : 0   1st Qu.:2015  
    ##  Accipiter cooperii     : 0   Median :2015  
    ##  Accipiter gentilis     : 0   Mean   :2017  
    ##  Accipiter striatus     : 0   3rd Qu.:2020  
    ##  Actitis macularius     : 0   Max.   :2023  
    ##  (Other)                : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-259.png)<!-- -->

    ##                species     eventDate   
    ##  Spiza americana   :59   Min.   :1996  
    ##  Acanthis flammea  : 0   1st Qu.:2017  
    ##  Accipiter cooperii: 0   Median :2020  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2022  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-260.png)<!-- -->

    ##                species     eventDate   
    ##  Aix sponsa        :61   Min.   :2009  
    ##  Acanthis flammea  : 0   1st Qu.:2014  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2017  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-261.png)<!-- -->

    ##                species    eventDate   
    ##  Selasphorus rufus :2   Min.   :2013  
    ##  Acanthis flammea  :0   1st Qu.:2013  
    ##  Accipiter cooperii:0   Median :2013  
    ##  Accipiter gentilis:0   Mean   :2013  
    ##  Accipiter striatus:0   3rd Qu.:2013  
    ##  Actitis macularius:0   Max.   :2013  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-262.png)<!-- -->

    ##                species    eventDate   
    ##  Asio otus         :3   Min.   :2010  
    ##  Acanthis flammea  :0   1st Qu.:2012  
    ##  Accipiter cooperii:0   Median :2015  
    ##  Accipiter gentilis:0   Mean   :2015  
    ##  Accipiter striatus:0   3rd Qu.:2018  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-263.png)<!-- -->

    ##                species      eventDate   
    ##  Falco sparverius  :271   Min.   :1985  
    ##  Acanthis flammea  :  0   1st Qu.:2016  
    ##  Accipiter cooperii:  0   Median :2018  
    ##  Accipiter gentilis:  0   Mean   :2016  
    ##  Accipiter striatus:  0   3rd Qu.:2020  
    ##  Actitis macularius:  0   Max.   :2022  
    ##  (Other)           :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-264.png)<!-- -->

    ##                species    eventDate   
    ##  Corvus corax      :1   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2021  
    ##  Accipiter cooperii:0   Median :2021  
    ##  Accipiter gentilis:0   Mean   :2021  
    ##  Accipiter striatus:0   3rd Qu.:2021  
    ##  Actitis macularius:0   Max.   :2021  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-265.png)<!-- -->

    ##                species    eventDate   
    ##  Anthus spragueii  :2   Min.   :1920  
    ##  Acanthis flammea  :0   1st Qu.:1942  
    ##  Accipiter cooperii:0   Median :1965  
    ##  Accipiter gentilis:0   Mean   :1965  
    ##  Accipiter striatus:0   3rd Qu.:1988  
    ##  Actitis macularius:0   Max.   :2010  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-266.png)<!-- -->

    ##                        species    eventDate   
    ##  Coccothraustes vespertinus:8   Min.   :1993  
    ##  Acanthis flammea          :0   1st Qu.:2010  
    ##  Accipiter cooperii        :0   Median :2010  
    ##  Accipiter gentilis        :0   Mean   :2009  
    ##  Accipiter striatus        :0   3rd Qu.:2010  
    ##  Actitis macularius        :0   Max.   :2020  
    ##  (Other)                   :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-267.png)<!-- -->

    ##                     species      eventDate   
    ##  Calamospiza melanocorys:128   Min.   :1901  
    ##  Acanthis flammea       :  0   1st Qu.:2012  
    ##  Accipiter cooperii     :  0   Median :2016  
    ##  Accipiter gentilis     :  0   Mean   :2012  
    ##  Accipiter striatus     :  0   3rd Qu.:2019  
    ##  Actitis macularius     :  0   Max.   :2021  
    ##  (Other)                :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-268.png)<!-- -->

    ##                species     eventDate   
    ##  Melospiza melodia :36   Min.   :1986  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2017  
    ##  Accipiter gentilis: 0   Mean   :2016  
    ##  Accipiter striatus: 0   3rd Qu.:2018  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-269.png)<!-- -->

    ##                 species      eventDate   
    ##  Melospiza lincolnii:133   Min.   :2011  
    ##  Acanthis flammea   :  0   1st Qu.:2014  
    ##  Accipiter cooperii :  0   Median :2015  
    ##  Accipiter gentilis :  0   Mean   :2016  
    ##  Accipiter striatus :  0   3rd Qu.:2017  
    ##  Actitis macularius :  0   Max.   :2022  
    ##  (Other)            :  0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-270.png)<!-- -->

    ##                 species    eventDate   
    ##  Melospiza georgiana:7   Min.   :2014  
    ##  Acanthis flammea   :0   1st Qu.:2014  
    ##  Accipiter cooperii :0   Median :2014  
    ##  Accipiter gentilis :0   Mean   :2016  
    ##  Accipiter striatus :0   3rd Qu.:2019  
    ##  Actitis macularius :0   Max.   :2021  
    ##  (Other)            :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-271.png)<!-- -->

    ##                     species    eventDate   
    ##  Pipilo erythrophthalmus:2   Min.   :1955  
    ##  Acanthis flammea       :0   1st Qu.:1955  
    ##  Accipiter cooperii     :0   Median :1955  
    ##  Accipiter gentilis     :0   Mean   :1955  
    ##  Accipiter striatus     :0   3rd Qu.:1955  
    ##  Actitis macularius     :0   Max.   :1955  
    ##  (Other)                :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-272.png)<!-- -->

    ##                species     eventDate   
    ##  Icterus spurius   :84   Min.   :1985  
    ##  Acanthis flammea  : 0   1st Qu.:2015  
    ##  Accipiter cooperii: 0   Median :2018  
    ##  Accipiter gentilis: 0   Mean   :2015  
    ##  Accipiter striatus: 0   3rd Qu.:2020  
    ##  Actitis macularius: 0   Max.   :2021  
    ##  (Other)           : 0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-273.png)<!-- -->

    ##                species    eventDate   
    ##  Geothlypis formosa:1   Min.   :2022  
    ##  Acanthis flammea  :0   1st Qu.:2022  
    ##  Accipiter cooperii:0   Median :2022  
    ##  Accipiter gentilis:0   Mean   :2022  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-274.png)<!-- -->

    ##                species    eventDate   
    ##  Setophaga auduboni:4   Min.   :2021  
    ##  Acanthis flammea  :0   1st Qu.:2022  
    ##  Accipiter cooperii:0   Median :2022  
    ##  Accipiter gentilis:0   Mean   :2022  
    ##  Accipiter striatus:0   3rd Qu.:2022  
    ##  Actitis macularius:0   Max.   :2022  
    ##  (Other)           :0

![](appendix_1_files/figure-gfm/unnamed-chunk-11-275.png)<!-- -->

# Missing

What are the missing taxa indicated in this study?

``` r
# prep GBIF data
gbif <- read.delim(paste0(filepath,"0104629-230530130749713.csv"),sep = "\t")

# reduce to relevant columns
# all have coords??

gbif <- gbif%>%
  select(gbifID, institutionCode, catalogNumber, species, locality,
         decimalLongitude, decimalLatitude, year) %>%
  unique()

taxo_order <- read_csv(paste0(filepath,"sp_list_ne.csv"))
taxo_order <- taxo_order%>%
  rename(species = scientific)

# get missing taxa
missing <- read_csv(paste0(filepath,"missing.csv")) %>%
  filter(keep==F)%>%
  rename(species = wrong)

miss_tax <- gbif%>%
  inner_join(missing,"species")

miss_tax <- miss_tax[order(miss_tax$species),]

write_csv(miss_tax,paste0(filepath,"missing_list.csv"))
```

Many of the taxa that are listed as here but have not been confirmed in
the state are exotics or fossil species. One collection appears to have
placed all records from North America at the same locality (Musée des
Confluences, Lyon) The following merit (brief) following up:

| Species                 | Collection                | Locality       | Year                             | Note                                                     |
|-------------------------|---------------------------|----------------|----------------------------------|----------------------------------------------------------|
| *Agelaius tricolor*     | USGS                      | NA             | 1932                             | Mis ID?                                                  |
| *Dryobates scalaris*    | Great Backyard Bird Count | 69144 Keystone | 2000                             | Mis ID?                                                  |
| *Larus heermanni*       | USGS                      | NA             | 1933                             | Wrong locality?                                          |
| *Larus occidentalis*    | iNaturalist               | NA             | Valid record; outside study area |                                                          |
| *Megascops kennicottii* | University of Michigan    | Kearney        | 1925                             | Two birds; likely misidentified; checked with collection |
| *Passer montanus*       | Cornell                   | Several        |                                  | Multiple records; not accepted? Outside study area       |
| *Picoides arcticus*     | Yale Peabody Museum       | Lincoln        | 1895                             | Transcription error; refers to Lincoln, Maine            |
| *Poecile carolinensis*  | Cornell                   | Omaha          | 2000                             | Not on state list; ouside study area                     |
| *Progne tapera*         | USGS                      | NA             | 1923                             | No details; mis ID?                                      |
| *Sialia mexicana*       | USGS                      | Chadron        | 1922                             | Recently seen in state; predates current records?        |

Many thanks to Dr. Brett Benz, Dr. Ben Winger, and Dr. Kristof Zykowski
for the specimen information.

I’m including *Sialia* in the study for now - hoping to confirm.

# 1993 to 2023

We can also combine the lists from 1993 and 2023 to understand what has
changed.

``` r
bray <- read_csv(paste0(filepath,"Bray_Sandhills_Species.csv"))
```

    ## Rows: 255 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): Species, SciName, Bray, Notes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
bray$Species <- as.factor(bray$Species)
bray$SciName <- as.factor(bray$SciName)
bray$Bray <- as.factor(bray$Bray)

ne_nf_gbif <- read_csv(paste0(filepath,"ne_natl_forest_birds.csv")) %>%
  filter(district == "Bessey") %>%
  select(species) %>%
  rename("SciName" = species) %>%
  unique()
```

    ## Rows: 64305 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): species, individualCount, district
    ## dbl  (2): decimalLongitude, decimalLatitude
    ## date (1): eventDate
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ne_nf_gbif$GBIF <- "GBIF"

ne_nf_gbif$SciName <- as.factor(ne_nf_gbif$SciName)
ne_nf_gbif$GBIF <- as.factor(ne_nf_gbif$GBIF)

# length(unique(bray$Species))
# length(unique(ne_nf_gbif$species))
```

The length of the list derived from Bray is 255 spp., whereas the list
derived from GBIF is 0 spp.

``` r
comp_years <- bray %>%
  full_join(ne_nf_gbif,by = "SciName")
```

``` r
# species recorded by Bray not in GBIF

index <- which(is.na(comp_years$GBIF))

comp_years[index,c("Species","SciName")]
```

    ## # A tibble: 83 × 2
    ##    Species                   SciName              
    ##    <fct>                     <fct>                
    ##  1 Common Loon               Gavia immer          
    ##  2 Horned Grebe              Podiceps auritus     
    ##  3 American Bittern          Botaurus letiginosus 
    ##  4 Great Egret               Ardea alba           
    ##  5 Black-crowned Night-Heron Nycticorax nycticorax
    ##  6 Snow Goose                Anser caerulescens   
    ##  7 Brant                     Branta bernicla      
    ##  8 Green-winged Teal         Anas carolinensis    
    ##  9 Canvasback                Aythya vasilineria   
    ## 10 Red-breasted Merganser    Mergus serrator      
    ## # ℹ 73 more rows

Most of these species are water birds. Many species were missing from
the direct GBIF download, for reasons that are unknown. These include
*Astur cooperii*.

``` r
# species recorded by GBIF not in Bray

index <- which(is.na(comp_years$Bray))

comp_years[index,c("Species","SciName")]
```

    ## # A tibble: 39 × 2
    ##    Species SciName                      
    ##    <fct>   <fct>                        
    ##  1 <NA>    Branta hutchinsii            
    ##  2 <NA>    Cygnus buccinator            
    ##  3 <NA>    Cygnus buccinator/columbianus
    ##  4 <NA>    Spatula discors/cyanoptera   
    ##  5 <NA>    Anas crecca                  
    ##  6 <NA>    Aythya valisineria           
    ##  7 <NA>    Aythya americana             
    ##  8 <NA>    Aythya collaris              
    ##  9 <NA>    Aythya marila                
    ## 10 <NA>    Bucephala clangula           
    ## # ℹ 29 more rows
