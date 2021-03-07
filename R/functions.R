# Function for generating the html used to create a two-column layout
# with a thumbnail image, a title with an optional link, and some text:
#
# |       | title
# | image | text
# |       | subtext (smaller font than text)
#
# See example on my teaching page: https://jhelvy.github.io/teaching

make_title_card <- function(
  title = NULL,
  text = NULL,
  subtext = NULL,
  url = NULL,
  image_src = NULL,
  image_width = 120
) {

  if (is.null(url)) {
    html <- htmltools::HTML(paste0(
      '<div>
          <img src="', image_src, '" style="float:left; width:', image_width,
      'px; margin: 0 15px 0 0">',
          '<h3 style="margin-bottom: 5px;">', title, '</h3>',
          '<p style="margin-bottom: 5px;">',  text, '</p>',
          '<span style="font-size:0.8rem;">', subtext, '</span>',
      '</div>'
    ))
  } else {
    html <- htmltools::HTML(paste0(
      '<div>
          <img src="', image_src, '" style="float:left; width:', image_width,
      'px; margin: 0 15px 0 0">',
          '<h3 style="margin-bottom: 5px;"><a href=', url, '>', title, '</a></h3>',
          '<p style="margin-bottom: 5px;">',  text, '</p>',
          '<span style="font-size:0.8rem;">', subtext, '</span>',
      '</div>'
    ))
  }

  return(html)
}

# Creates the html to make a button to an external link
make_link_button <- function(
  text = NULL,
  url = NULL,
  icon = NULL
) {

  if (!is.null(icon)) {
    text <- paste0('<i class="', icon, '"></i> ', text)
  }
  html <- htmltools::HTML(paste0(
    '<a href="', url, '" class="link-button">', text, '</a>'
  ))
  return(html)
}

make_doi <- function(doi) {
  return(paste0('DOI: [', doi, '](https://doi.org/', doi, ')'))
}

get_cites <- function() {
  url <- "https://scholar.google.com/citations?user=DY2D56IAAAAJ"
  html <- xml2::read_html(url)
  node <- rvest::html_nodes(html, xpath='//*[@id="gsc_rsb_st"]')
  cites_df <- rvest::html_table(node)[[1]]
  names(cites_df)[1] <- "category"
  cites_df$`Since 2016` <- NULL
  cites <- tidyr::spread(cites_df, category, All)
  names(cites) <- c('citations', 'hindex', 'i10index')
  return(cites)
}
