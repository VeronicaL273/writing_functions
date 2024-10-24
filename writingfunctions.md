Writing Functions
================
Yunjia Liu
2024-10-24

``` r
x_vec = rnorm(25, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -1.17527329  0.91639953 -0.56128875 -1.67855660  0.23950028 -0.97048908
    ##  [7]  0.67263839  1.06691089  0.21733213  1.40625487  1.03270466  0.08483416
    ## [13] -1.30882778 -1.23133297 -0.11650274  0.40322324 -0.66495328  0.26625249
    ## [19] -0.27503133 -0.93843337  2.31668624 -0.65648829  0.95833536  0.82472757
    ## [25] -0.82862234

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

    ##  [1] -1.17527329  0.91639953 -0.56128875 -1.67855660  0.23950028 -0.97048908
    ##  [7]  0.67263839  1.06691089  0.21733213  1.40625487  1.03270466  0.08483416
    ## [13] -1.30882778 -1.23133297 -0.11650274  0.40322324 -0.66495328  0.26625249
    ## [19] -0.27503133 -0.93843337  2.31668624 -0.65648829  0.95833536  0.82472757
    ## [25] -0.82862234

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
    ## 1  4.73  3.11

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
    ## 1  9.28  5.57

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
    ## 1  3.98  12.1

``` r
sim_mean_sd(samp_size = 4, true_mean = 12,true_sd = 30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  13.3  16.1

``` r
sim_mean_sd(30,16,2)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  16.4  2.01

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

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(4) |>
  slice(-1) |> 
  mutate( drug = "cocaine")

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(4) |>
  slice(-1) |> 
  mutate( drug = "heroin")
```
