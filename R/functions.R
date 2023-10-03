
#' re-export magrittr pipe operator
#'
#' @name %>%
#' @rdname pipe
#'
#' @importFrom magrittr %>%
#'
#' @export
NULL


#' get_last_page
#'
#' @param html the result of rvest::read_html() of class xml_document xml_node
#'
#' @import rvest
#' @import magrittr
#'
#' @return numeric value of last page
#' @export
#'
#' @examples
#'
#' URL <- "https://lubimyczytac.pl/ksiegozbior/xc5GvparjG"
#' html <- rvest::read_html(x = URL)
#' last_page <- get_last_page(html = html)
#'
get_last_page <- function(html){

    max_page <-
      html %>%
        rvest::html_element(css = "input.jsPagerInput") %>%
        rvest::html_attr("max") %>%
        as.numeric()

    return(max_page)
}

#' get_links
#'
#' @param html the result of rvest::read_html() of class xml_document xml_node
#' @param core_link string, core link to the website
#'
#' @import rvest
#' @import magrittr
#'
#' @return vector of links
#' @export
#'
#' @examples
#'
#' URL <- "https://lubimyczytac.pl/ksiegozbior/xc5GvparjG"
#' html <- rvest::read_html(x = URL)
#' links <- get_links(html = html)
#'
get_links <- function(html,
                      core_link = "https://lubimyczytac.pl") {

  link <-
    html %>%
    rvest::html_nodes(".authorAllBooks__singleTextTitle") %>%
    rvest::html_attr("href")

  complete_link <- paste0(core_link, link)

  return(complete_link)
}

#' get_ISBN
#'
#' @param book_html the result of rvest::read_html() of class xml_document xml_node
#'
#' @import rvest
#' @import magrittr
#'
#' @return character, ISBN number
#' @export
#'
#' @examples
#' isbn <- get_ISBN(url = url)
#'
get_ISBN <- function(book_html) {

  isbn <- c()

  out <-
    vapply(X = book_html,
           FUN = function(x) {
             #check if ISBN is present
             test_con <-
               any(grepl(pattern = "ISBN",
                         x = x %>%
                           html_nodes("dt")))
             # at which position (sometimes it may differ)
             if(test_con) {
               where <-
                 which(x %>%
                         rvest::html_nodes("dt") %>%
                         rvest::html_text2() == "ISBN:")
               isbn <-
                 append(isbn,
                        x %>%
                          rvest::html_nodes("dd") %>%
                          rvest::html_text2() %>%
                          .[where])
             } else {
               isbn <- append(isbn, "Not provided")
             }
           },
           FUN.VALUE = "character"
           )

  return(out)
}


#' get_book_details on page
#'
#' @param library_html the result of rvest::read_html() of class xml_document xml_node
#' @param book_html each book html from lapply(X = get_links(page_html), FUN = rvest::read_html)
#'
#' @import rvest
#' @import magrittr
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
#' res <- get_book_details(library_html = library_html, book_html = book_html)
#'
get_book_details <- function(library_html,
                             book_html) {

  attr_list <-
    setNames(
      c("a.authorAllBooks__singleTextTitle",
        "div.authorAllBooks__singleTextAuthor",
        "div.authorAllBooks__singleTextShelfRight", #shelf
        "div.authorAllBooks__singleText",
        #"div.small",
        "div.comments-list"),
      c("Title", "Author", "Shelves", "My Rating",
        #"Date Read",
        "My Review"))

  out_list <- list()

  for(i in seq_along(attr_list)){
    out_list[[names(attr_list)[i]]] <-
      rvest::read_html(unlist(library_html)) %>%
      rvest::html_elements(css = attr_list[i]) %>%
      rvest::html_text2()
  }

  out_list$`My Rating` <-
    ifelse(test = grepl(".*Ocenił na", out_list$`My Rating`),
           yes = gsub("\\/.*", "",
                      gsub(".*Ocenił na", "",
                           gsub(":\n", "", out_list$`My Rating`))),
           no =  "Not scored yet") %>%
    stringr::str_trim()

  out_list$ISBN <- get_ISBN(book_html = book_html)

  out_list$`My Review` <-
    out_list$`My Review` %>%
    gsub(pattern = "\\\n", replacement = "")

  out_list <- do.call("cbind", out_list)

  return(out_list)
}

