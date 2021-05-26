library(htmltools)

aside <- function(text) {
  return(tag("aside", list(text)))
}

center <- function(text) {
  return(tag("center", list(text)))
}

aside_center <- function(text) {
  return(aside(center(list(text))))
}

aside_center_b <- function(text) {
  return(aside(center(list(tag("b", text)))))
}

haiku <- function(one, two, three) {
  return(aside_center(list(
    em(one, HTML("&#8226;"), two, HTML("&#8226;"), three)
  )))
}

markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }
  return(HTML(markdown::renderMarkdown(text = text)))
}

make_icon <- function(icon) {
  return(tag("i", list(class = icon)))
}

make_icon_text <- function(icon, text) {
  return(HTML(paste0(make_icon(icon), " ", text)))
}

# Creates the html to make a button to an external link
icon_link <- function(icon = NULL, text = NULL, url = NULL) {
  if (!is.null(icon)) {
    text <- make_icon_text(icon, text)
  }
  return(a(href = url, text, class = "icon-link"))
}

doi <- function(doi) {
  url <- paste0('https://doi.org/', doi)
  return(paste0('DOI: ', a(href = url, doi)))
}

gscholar_stats <- function(url) {
  cites <- get_cites(url)
  return(paste(
    'Citations:', cites$citations, '|',
    'h-index:',   cites$hindex, '|',
    'i10-index:', cites$i10index
  ))
}

get_cites <- function(url) {
  html <- xml2::read_html(url)
  node <- rvest::html_nodes(html, xpath='//*[@id="gsc_rsb_st"]')
  cites_df <- rvest::html_table(node)[[1]]
  names(cites_df)[1] <- "category"
  cites_df$`Since 2016` <- NULL
  cites <- tidyr::spread(cites_df, category, All)
  names(cites) <- c('citations', 'hindex', 'i10index')
  return(cites)
}

# Masonry layout for projects page:
# https://jhelvy.github.io/projects

masonry_grid <- function(...) {
  return(div(
    class = "masonry-wrapper",
    div(class = "masonry", ...)
  ))
}

masonry_item <- function(
  src = NULL,
  title = NULL,
  description = NULL,
  url = NULL
) {
  image <- img(src = src)
  if (!is.null(url)) {
    image <- a(
      href = url,
      class = "card-hover",
      img(src = src))
  }
  return(div(class = "masonry-item",
    div(
      class = "masonry-content",
      image,
      h3(class = "masonry-title", title),
      p(class = "masonry-description", description)
    )
  ))
}

create_footer <- function() {

  fill <- '#ededeb'
  height <- '14px'

  footer <- HTML(paste0(
  '© 2021 John Paul Helveston [',
  fontawesome::fa('creative-commons', fill = fill, height = height),
  fontawesome::fa('creative-commons-by', fill = fill, height = height),
  fontawesome::fa('creative-commons-sa', fill = fill, height = height),
  '](https://creativecommons.org/licenses/by-sa/4.0/)\n',
  br(),
  fontawesome::fa('wrench', fill = fill, height = height), ' Made with ',
  fontawesome::fa('heart', fill = fill, height = height), ', [',
  fontawesome::fa('code-branch', fill = fill, height = height),
  '](https://github.com/jhelvy), and the [',
  fontawesome::fa('r-project', fill = fill, height = height),
  '](https://cran.r-project.org/) ',
  '[distill](https://github.com/rstudio/distill) package\n',
  br(),
  last_updated(), "\n\n",

  '<!-- Add function to open links to external links in new tab, from: -->',
  '<!-- https://yihui.name/en/2018/09/target-blank/ -->\n\n',
  '<script src="js/external-link.js"></script>'
  ))

  save_raw(footer, "_footer.html")
}

last_updated <- function() {
  return(span(
    paste0(
      'Last updated on ',
      format(Sys.Date(), format="%B %d, %Y")
    ),
    style = "font-size:0.8rem;")
  )
}

save_raw <- function(text, path) {
    fileConn <- file(path)
    writeLines(text, fileConn)
    close(fileConn)
}
