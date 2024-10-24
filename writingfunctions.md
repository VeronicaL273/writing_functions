Writing Functions
================
Yunjia Liu
2024-10-24

``` r
x_vec = rnorm(25, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.2005150 -0.1006726 -0.4068967 -0.8617577  1.2538087  0.3044527
    ##  [7] -0.8316375  1.3257644 -0.3201016 -1.6758579 -1.0945795 -1.1349580
    ## [13]  0.1658927 -0.4463085  2.4272423  0.6863868  1.5544225  0.2886273
    ## [19]  1.4358926 -0.2029137 -1.1406786 -0.5115800 -0.8205712 -0.2683404
    ## [25]  0.1738490

``` r
z_scores = function(x) {
  
  if(!is.numeric(x)){.  #add additional
    stop("x needs to be numeric")
  }
  
  if(length(x)<5){
    stop("you need at least 5 numbers to compute z score")
  }
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1]  0.2005150 -0.1006726 -0.4068967 -0.8617577  1.2538087  0.3044527
    ##  [7] -0.8316375  1.3257644 -0.3201016 -1.6758579 -1.0945795 -1.1349580
    ## [13]  0.1658927 -0.4463085  2.4272423  0.6863868  1.5544225  0.2886273
    ## [19]  1.4358926 -0.2029137 -1.1406786 -0.5115800 -0.8205712 -0.2683404
    ## [25]  0.1738490

does it always work? No, but we add some “if”s to control the properties
of x_vec that go into the function

### a new function

``` r
mean_and_sd = function(x){
  mean_x = mean(x)
  sd_x = sd(x)
  
  out_df = tibble(
    mean = mean_x,
    sd = sd_x
  )
  
  return(out_df)
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.31  2.90

``` r
sim_df =
  tibble(
    x = rnorm(30, 10, 5)
  )
sim_df |>
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.8  4.15

Simulation function to check sample mean and sd.

``` r
sim_mean_sd = function(samp_size, true_mean = 10, true_sd = 5){  #set default value
  sim_df = 
    tibble(
      x = rnorm(samp_size, true_mean, true_sd)
    )
  
  out_df = 
    sim_df |>
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
  return(out_df)
}

sim_mean_sd(samp_size = 30, true_mean = 4,true_sd = 12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.23  10.6

``` r
sim_mean_sd(samp_size = 4, true_mean = 12,true_sd = 30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.96  23.7

``` r
sim_mean_sd(30,16,2)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  15.8  2.04

``` r
fellowship_df = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship_ring") |>
  janitor::clean_names()

two_towers_df = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers") |>
  janitor::clean_names()

return_king_df = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king") |>
  janitor::clean_names()

lotr_tidy = bind_rows(fellowship_df, two_towers_df, return_king_df) |>
  janitor::clean_names() |>
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words") |> 
  mutate(race = str_to_lower(race)) |> 
  select(movie, everything())
```

``` r
lotr_load_and_tidy = function(path, range, movie_name) {
  
  df = 
    readxl::read_excel(path, range = range) |>
    janitor::clean_names() |>
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words") |>
    mutate(
      race = str_to_lower(race),
      movie = movie_name) |> 
    select(movie, everything())
  
  df
  
}

lotr_tidy = 
  bind_rows(
    lotr_load_and_tidy("data/LotR_Words.xlsx", "B3:D6", "fellowship_ring"),
    lotr_load_and_tidy("data/LotR_Words.xlsx", "F3:H6", "two_towers"),
    lotr_load_and_tidy("data/LotR_Words.xlsx", "J3:L6", "return_king"))
```

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  mutate( drug = "marj")

data_cocaine = 
  nsduh_html |> 
  html_table() |> 
  nth(4) |>
  slice(-1) |> 
  mutate( drug = "cocaine")

data_heroin = 
  nsduh_html |> 
  html_table() |> 
  nth(5) |>
  slice(-1) |> 
  mutate( drug = "heroin")


source("./source/nsduh_table_format.R")
bind_rows(
  nsduh_table_format(html = nsduh_html, 1 ,"marj"),
  nsduh_table_format(html = nsduh_html, 1 ,"marj"),
  nsduh_table_format(html = nsduh_html, 1 ,"marj")
)
```

    ## # A tibble: 168 × 12
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 12.90a           13.36            13.28b             12.86             
    ##  2 Nort… 13.88a           14.66            13.98              13.51             
    ##  3 Midw… 12.40b           12.76            12.45              12.33             
    ##  4 South 11.24a           11.64            12.02              11.88             
    ##  5 West  15.27            15.62            15.53a             14.43             
    ##  6 Alab… 9.98             9.60             9.90               9.71              
    ##  7 Alas… 19.60a           21.92            17.30              18.44             
    ##  8 Ariz… 13.69            13.12            15.12              13.45             
    ##  9 Arka… 11.37            11.59            12.79              12.14             
    ## 10 Cali… 14.49            15.25            15.03              14.11             
    ## # ℹ 158 more rows
    ## # ℹ 7 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, drug <dbl>
