
# generate results --------------------------------------------------------

URL <- "https://lubimyczytac.pl/ksiegozbior/xc5GvparjG"
#URL <- "https://lubimyczytac.pl/ksiegozbior/XyeMyrJrGh"

page_html <- rvest::read_html(x = URL)


# RSelenium

# Open firefox and extract source
rD <- RSelenium::rsDriver(chromever = NULL)
remDr <- rD[["client"]]
remDr$navigate(URL)

# Give some time to load
Sys.sleep(6)
# Increase window size to find elements
remDr$maxWindowSize()

#generate res
all_data <- list()
page <- 0
max_page <- get_last_page(html = page_html)

start <- Sys.time()

#while(cond == TRUE) {
while(page < max_page +1) {

  if(page > max_page) {

    break

  } else {

    #get books data
    books_content_row <-
      remDr$findElement(using = "id",
                        value = "booksFilteredListPaginator")

    books_html <- books_content_row$getPageSource()

    # for scrapping
    l <- get_links(html = rvest::read_html(unlist(books_html)))

    each_book <- lapply(X = l, FUN = rvest::read_html)

    #

    all_data[[page+1]] <- get_book_details(library_html = books_html,
                                           book_html = each_book)

    Sys.sleep(0.2)

    if(max_page != 1) {
      next_button <-
        remDr$findElement(using = "xpath", '//a[@aria-label="Next"]')

      next_button$clickElement()
    }

  }
  page <- page +1
  message(paste0("Proccessing page: ", page, "/", max_page))
}

df <- as.data.frame(do.call("rbind", all_data[c(2:max_page)]))

stop <- Sys.time()

stop-start

write.table(x = df,
            file = "~/Projects/books/tomson_library.txt",
            sep = "\t",
            row.names = FALSE)

xlsx::write.xlsx(x = df,
                 file = "~/Projects/books/tomson_library.xlsx",
                 row.names = FALSE)



#test

# all_css_id <-
#   page_html %>%
#     html_nodes(xpath = '//*[@id]') %>%
#     html_text2()
#
# when_read_pos <-
#   all_css_id %>%
#     grepl(pattern = "Przeczytał:\n20")
#
# all_css_id[when_read_pos] %>%
#   substring(first = 13, last = 22)
#
#
# # data ale nie na wszystko:
# page_html %>%
#   html_elements(css = 'div.small') %>%
#   html_text2()

