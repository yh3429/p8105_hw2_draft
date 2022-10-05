p8105_hw2_draft
================
2022-10-03

``` r
library(tidyverse)
library(readxl)
```

## Problem 1

### To read and clean the NYC Transit data

``` r
NYC_transit = read_csv( "./data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv",
  col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c", Route11 = "c")) %>% 
   janitor::clean_names() %>% 
  select(
    line, station_name, station_latitude, station_longitude, 
    starts_with("route"), entry, exit_only, vending, entrance_type, 
    ada) %>% 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE))
```

### A short description for this dataset

#### What variables the dataset contains

The dataset has 20 variables including line, station name, station
latitude, station longitude, served routes (`Route1`-`Route11`), entry,
entrance type, vending, and ADA compliance.

#### Description of the data cleaning steps so far

After importing the dataset, we cleaned the data name, specified that
`Route` columns 8-11 should be character for consistency with 1-7. And
then, we converted the `Route` variable from character to a logical
variable.

#### Giving the dimension (rows x columns) of the resulting dataset. Are these data tidy?

There are 1,868 rows and 20 columns in the resulting dataset. And these
data is not tidy since the route number should be a variable. So, we
would need to convert `Route` variables from wide to long format.

### Answer following questions

``` r
NYC_transit %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 465 × 2
    ##    station_name             line    
    ##    <chr>                    <chr>   
    ##  1 25th St                  4 Avenue
    ##  2 36th St                  4 Avenue
    ##  3 45th St                  4 Avenue
    ##  4 53rd St                  4 Avenue
    ##  5 59th St                  4 Avenue
    ##  6 77th St                  4 Avenue
    ##  7 86th St                  4 Avenue
    ##  8 95th St                  4 Avenue
    ##  9 9th St                   4 Avenue
    ## 10 Atlantic Av-Barclays Ctr 4 Avenue
    ## # … with 455 more rows

``` r
NYC_transit %>% 
    filter(ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 84 × 2
    ##    station_name                   line           
    ##    <chr>                          <chr>          
    ##  1 Atlantic Av-Barclays Ctr       4 Avenue       
    ##  2 DeKalb Av                      4 Avenue       
    ##  3 Pacific St                     4 Avenue       
    ##  4 Grand Central                  42nd St Shuttle
    ##  5 34th St                        6 Avenue       
    ##  6 47-50th Sts Rockefeller Center 6 Avenue       
    ##  7 Church Av                      6 Avenue       
    ##  8 21st St                        63rd Street    
    ##  9 Lexington Av                   63rd Street    
    ## 10 Roosevelt Island               63rd Street    
    ## # … with 74 more rows

``` r
NYC_transit %>% 
  filter(vending == "NO") %>% 
  pull (entry) %>% 
  mean
```

    ## [1] 0.3770492

``` r
NYC_transit %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A") %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 60 × 2
    ##    station_name                  line           
    ##    <chr>                         <chr>          
    ##  1 Times Square                  42nd St Shuttle
    ##  2 125th St                      8 Avenue       
    ##  3 145th St                      8 Avenue       
    ##  4 14th St                       8 Avenue       
    ##  5 168th St - Washington Heights 8 Avenue       
    ##  6 175th St                      8 Avenue       
    ##  7 181st St                      8 Avenue       
    ##  8 190th St                      8 Avenue       
    ##  9 34th St                       8 Avenue       
    ## 10 42nd St                       8 Avenue       
    ## # … with 50 more rows

``` r
NYC_transit %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A", ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 17 × 2
    ##    station_name                  line            
    ##    <chr>                         <chr>           
    ##  1 14th St                       8 Avenue        
    ##  2 168th St - Washington Heights 8 Avenue        
    ##  3 175th St                      8 Avenue        
    ##  4 34th St                       8 Avenue        
    ##  5 42nd St                       8 Avenue        
    ##  6 59th St                       8 Avenue        
    ##  7 Inwood - 207th St             8 Avenue        
    ##  8 West 4th St                   8 Avenue        
    ##  9 World Trade Center            8 Avenue        
    ## 10 Times Square-42nd St          Broadway        
    ## 11 59th St-Columbus Circle       Broadway-7th Ave
    ## 12 Times Square                  Broadway-7th Ave
    ## 13 8th Av                        Canarsie        
    ## 14 Franklin Av                   Franklin        
    ## 15 Euclid Av                     Fulton          
    ## 16 Franklin Av                   Fulton          
    ## 17 Howard Beach                  Rockaway

#### Descriptions of the results from above codes

According to the results, there are 465 distinct stations are there.
Among them, there are 84 stations having ADA compliant. And there is 38%
station entrances/exits without vending allow entrance.In addition,
there are 60 stations serving the A train and of the stations that serve
the A train, there are 17 stations having ADA compliant.

## Problem 2

### To read,clean, and organize the data from Mr. Trash Wheel sheet

``` r
Mr_Trash_wheel = read_excel("./data/Trash Wheel Collection Data.xlsx",
                           sheet =1,
                           range = "A2:N550",
                           skip = 1,
                           col_names = TRUE,
                           col_types = NULL,
                           na = ""
                           ) %>% 
   drop_na() %>% 
   janitor::clean_names() %>% 
   mutate(sports_balls =  as.integer(round(sports_balls, digits = 0))) %>% 
   mutate(sheet_name="Mr")
```

### To read, clean, and organize the data from Professor Trash Wheel sheet

``` r
Pro_Trash_wheel = read_excel ("./data/Trash Wheel Collection Data.xlsx",
                           sheet =2,
                           range = "A2:M97",
                           skip = 1,
                           col_names = TRUE,
                           col_types = NULL,
                           na = ""
                           ) %>% 
   drop_na() %>% 
   janitor::clean_names() %>% 
   mutate(sheet_name="Pro")
```

### To add an additional variable and combine two datasets

``` r
Pro_Trash_wheel = mutate(Pro_Trash_wheel, 
                         year = as.character(year)
                      )
comb_Trash_wheel= 
bind_rows(Mr_Trash_wheel, Pro_Trash_wheel, .id="dumpster")
```

### A paragraph description for these datasets

1.  In the combined trash wheel dataset, there are **568** observations
    of **15** variables.The key variables includes `dumpster`, `month`,
    `year`, `date`, `weight_tons`, `volume_cubic_yards`,
    `plastic_bottles`, `polystyrene`,
    `cigarette_butts`,`glass_bottles`,`grocery_bags`, `chip_bags`,
    `sports_balls`, `homes_powered`, and `sheet_name`. In addition, the
    variable `sheet_name` is added in order to distinguish the two
    datasets with the same columns.

2.  In the Mr. trash wheel dataset, there are **486** observations of
    **15** variables. The key variables includes `dumpster`, `month`,
    `year`, `date`, `weight_tons`, `volume_cubic_yards`,
    `plastic_bottles`, `polystyrene`, `cigarette_butts`,
    `glass_bottles`, `grocery_bags`,`chip_bags`, `sports_balls`,
    `homes_powered`,`sheet_name`.

3.  In the Professor trash wheel dataset, there are **82** observations
    of **14** variables.The key variables includes `dumpster`, `month`,
    `year`, `date`, `weight_tons`, `volume_cubic_yards`,
    `plastic_bottles`, `polystyrene`, `cigarette_butts`,`glass_bottles`,
    `grocery_bags`,`chip_bags`, `homes_powered`,`sheet_name`.

#### For available data, what was the total weight of trash collected by Professor Trash Wheel?

``` r
Total_weight =  sum (Pro_Trash_wheel$weight_tons)
```

According to the sum result, the total weight of trash collected by
Professor Trash wheel is **162.54 tons**.

#### For available data, what was the total number of sports balls collected by Mr.Trash wheel in 2020?

``` r
Total_sports_balls= Mr_Trash_wheel %>%  filter(year=="2020") %>% 
  summarise(sum(sports_balls))
```

According to the sum result in 2020, the total number of sports balls
collected by Mr.Trash wheel is **856**.

## Problem 3

### First, clean the data in pols-month.csv.

``` r
pols_month = 
  read_csv ("./data/pols-month.csv") %>% 
  janitor::clean_names() %>%
  separate(mon, into = c("year", "month", "day")) %>%
  mutate(month = month.abb[as.numeric(month)]) %>% 
  mutate(president = case_when (prez_gop == 1 ~ "gop",
                                prez_dem == 1 ~ "dem")) 
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pols_month_tidy = subset(pols_month, select = -c(prez_dem, 
                                                prez_gop,
                                                day) ) %>% 
  mutate(year = as.numeric(year))

pols_month_tidy
```

    ## # A tibble: 822 × 9
    ##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##    <dbl> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ##  1  1947 Jan        23      51     253      23      45     198 dem      
    ##  2  1947 Feb        23      51     253      23      45     198 dem      
    ##  3  1947 Mar        23      51     253      23      45     198 dem      
    ##  4  1947 Apr        23      51     253      23      45     198 dem      
    ##  5  1947 May        23      51     253      23      45     198 dem      
    ##  6  1947 Jun        23      51     253      23      45     198 dem      
    ##  7  1947 Jul        23      51     253      23      45     198 dem      
    ##  8  1947 Aug        23      51     253      23      45     198 dem      
    ##  9  1947 Sep        23      51     253      23      45     198 dem      
    ## 10  1947 Oct        23      51     253      23      45     198 dem      
    ## # … with 812 more rows

### Second, clean the data in snp.csv using a similar process to the above.

``` r
snp=
  read_csv("./data/snp.csv") %>% 
  janitor::clean_names() %>%
  separate(date, into = c("month", "day", "year")) %>% 
   mutate(month = month.abb[as.numeric(month)]) 
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp_tidy = subset(snp, select = -c(day)) %>% 
  select(year, month, close) %>% 
  mutate(year = as.numeric(year))

  snp_tidy
```

    ## # A tibble: 787 × 3
    ##     year month close
    ##    <dbl> <chr> <dbl>
    ##  1    15 Jul   2080.
    ##  2    15 Jun   2063.
    ##  3    15 May   2107.
    ##  4    15 Apr   2086.
    ##  5    15 Mar   2068.
    ##  6    15 Feb   2104.
    ##  7    15 Jan   1995.
    ##  8    14 Dec   2059.
    ##  9    14 Nov   2068.
    ## 10    14 Oct   2018.
    ## # … with 777 more rows

### Third, tidy the unemployment data.

``` r
unem = 
  read_csv("./data/unemployment.csv") %>% 
   janitor::clean_names()
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unem_tidy =
  pivot_longer(
    unem, 
    jan:dec,
    names_to = "month",
    values_to = " percentage"
  ) %>% 
    mutate( year = as.numeric(year))

unem_tidy 
```

    ## # A tibble: 816 × 3
    ##     year month ` percentage`
    ##    <dbl> <chr>         <dbl>
    ##  1  1948 jan             3.4
    ##  2  1948 feb             3.8
    ##  3  1948 mar             4  
    ##  4  1948 apr             3.9
    ##  5  1948 may             3.5
    ##  6  1948 jun             3.6
    ##  7  1948 jul             3.6
    ##  8  1948 aug             3.9
    ##  9  1948 sep             3.8
    ## 10  1948 oct             3.7
    ## # … with 806 more rows

### Join the datasets by merging snp into pols, and merging unemployment into the result.

``` r
pols_snp = pols_month_tidy %>% left_join(snp_tidy)
```

    ## Joining, by = c("year", "month")

``` r
pols_snp_unem = mutate (merge(pols_snp, unem_tidy, by.x="year", by.y="year")) %>% mutate( year = as.numeric(year))
```

### range of years

``` r
# range of years - the data in pols-month.csv.
 print(max(pols_month_tidy$year, na.rm=TRUE)
                    -min(pols_month_tidy$year, na.rm=TRUE))
```

    ## [1] 68

``` r
# range of years - the data in snp.csv.
print(max(snp_tidy$year, na.rm=TRUE)
                    -min(snp_tidy$year, na.rm=TRUE))
```

    ## [1] 99

``` r
# range of years - the unemployment data
print(max(unem_tidy$year, na.rm=TRUE)
                    -min(unem_tidy$year, na.rm=TRUE))
```

    ## [1] 67

``` r
# range of years - the merged resulting dataset
print(max(pols_snp_unem$year, na.rm=TRUE)
                    -min(pols_snp_unem$year, na.rm=TRUE))
```

    ## [1] 67

### A short desciption about these datasets

#### The data in pols-month.csv.

There are **822** rows and **9** columns. The range of year is **68**.
And the key variables include `year`, `month`, `gov_gop`, `sen_gop`,
`rep_gop`, `gov_dem`, `sen_dem`, `rep_dem`, and `president`.

#### The data in snp.csv.

There are **787** rows and **3** columns. The range of year is **99**.
And the key variables include `year`, `month` and `close`.

#### The unemployment data

There are **816** rows and **3** columns. The range of year is **67**.
And the key variables include `year`, `month` and `percentage`.

#### The merged resulting dataset by Joining the datasets by merging snp into pols, and merging unemployment.

There are **9720** rows and **12** columns. The range of year is **67**.
And the key variables include `year`, `month.x`, `gov_gop`, `sen_gop`,
`rep_gop`, `gov_dem`, `sen_dem`, `rep_dem`,
`president`,`close`,`month.y` and `percentage`.
