libroScrapeR
================
kanahia
08 October, 2023

## Motivation

This package was created as a side project that started as a loose
conversation at work. The objective was to extract the data from a
[popular website](https://lubimyczytac.pl) in Poland utilized for
sharing a collection of read books. Unfortunately, website providers do
not provide a user friendly option to export the data. I decided to play
with web scrapping to extract information from the website. It may be
useful for anyone who wants to transfer own library to the third part
services, more international one like <https://www.goodreads.com/>.

At the moment the script extract the following fields:

1.  Title

2.  Author

3.  Shelf at which the book is stored

4.  Given rate

5.  User review

6.  Book ISBN number

## Info

Depending on the library size, the script may take a while. Averagely,
getting thr data from 30 pages takes around 10 minutes.

## Input

# libroScrapeR

As an input user should provide individual link to the own library that
can be generated by the user after log in as in the example below:

![](https://github.com/kanahia/libroScrapeR/assets/49271254/63b54e86-7595-4b0d-9c62-ef6572e70f02)

Script completes with final table summarizing all read books and their
associated details.

## Getting ready

The package has been written in R and minimal knowledge of R is required
to successfully complete the script.

### The enviroment

To run the script three steps must be undertaken. First of all, package
utilize `RSelenium` package. In this respect, user must verify the
google chrome version and download respective chromedriver. 1. Driver
can be downloaded from the following page:
<https://googlechromelabs.github.io/chrome-for-testing/> 2. Make sure
java is installed.

### Initialize

This step aims to initialize required driver directory skeleton. After
running this chunk, error may occur but you should wait until all
necessary files are downloaded. At the end, inspect your local directory
in terms of binman directories.

Especially pay attention if the binman_chromdriver was created:

Windows: `C:\Users\USER\AppData\Local\binman` with several other binman
directories Unix: `~/.local/share/binman_chromedriver/`

``` r
library(RSelenium)

rD <- rsDriver() # runs a chrome browser, wait for necessary files to download # must be null on windows!

remDr <- rD$client
```

### Confirm chromedriver version

1.  Check your google chrome version by typing in the url field
    <chrome://version/>

2.  Download matching chromedriver, e.g., if your version is
    `117.0.5938.132` then your chromedriver must match the major version
    `117.X.XXXX.XXX`

3.  Driver can be downloaded from
    <https://googlechromelabs.github.io/chrome-for-testing/>

4.  Navigate to
    `C:\Users\USER\AppData\Local\binman\binman_chromedriver\win32\` and
    make new directory named as the chromedriver version (e.g.,
    `117.0.5938.132`), save chromedriver and unpack in this location.

5.  Make sure you have installed java. If not install it. Confirm by
    `cmd.exe -> java -version (Windows)`

6.  Go back to R and run:

7.  for windows it should work without any arguments

8.  on archlinux works with rD \<- RSelenium::rsDriver(chromever = NULL)

``` r
library(RSelenium)

rD <- rsDriver()

remDr <- rD$client 
```

If RSelenium sucessfully initiate the browser, then close it and run the
main.R script.

### Run script

Load all the functions and run the script providing link to your
personal library:

``` r
# Example how to run the entire workflow

URL <- "https://lubimyczytac.pl/ksiegozbior/XyeMyrJrGh"
res <- libroScrapeR::run_libroScrapeR(URL = URL)
```

Once it is finished one should get a dataframe comprising all the data
describing your reading activity.

## Throubleshooting and final remarks

### License problem

If the browser has not been initialized, remove (cut out) license file
from yout local binman directory, e.g.,
`C:\UsersUSER\AppData\Local\binman\binman_chromedriver\win32\117.0.5938.92\`
and run again.

After it is done, make sure to close driver.

### Exiting

Ulitmately kill the enitre RSelenium process - otherwise you will not be
able to initialize script again.

On Windows kill chromedriver.exe and Java(TM) Platform SE binary
