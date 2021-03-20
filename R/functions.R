aside <- function(text) {
  return(htmltools::tag("aside", list(text)))
}

center <- function(text) {
  return(htmltools::tag("center", list(text)))
}

aside_center <- function(text) {
  return(aside(center(list(text))))
}

aside_center_b <- function(text) {
  return(aside(center(list(htmltools::tag("b", text)))))
}

haiku <- function(one, two, three) {
  return(aside_center(list(
    htmltools::em(
      one, htmltools::br(),
      two, htmltools::br(),
      three)
  )))
}

# Generates html used to create a paragraph of text with an image
# floated to the left or right. Pass markdown syntax as a string
# inside the `title` or `text` arguments.
#
# See examples on my "lab" and "teaching" pages:
# https://jhelvy.github.io/lab
# https://jhelvy.github.io/teaching

image_float_layout <- function(
  title = NULL,
  text = NULL,
  src = NULL,
  width = NULL,
  height = NULL,
  float = "left",
  margin = "0 15px 0 0"
) {
  return(htmltools::div(
    float_image(src, width, height, float, margin),
    markdown_to_html(title),
    markdown_to_html(text)
  ))
}

markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }
  return(htmltools::HTML(
    markdown::renderMarkdown(text = text))
  )
}

float_image <- function(
  src = NULL,
  width = NULL,
  height = NULL,
  float = "left",
  margin = "0 15px 0 0"
) {
  style <- get_img_style(width, height, float, margin)
  return(htmltools::img(src = src, style = style))
}

get_img_style <- function(
  width = NULL,
  height = NULL,
  float = "left",
  margin = "0 15px 0 0"
) {
  if (!is.null(width)) {
    width <- paste0("width: ", width, "px; ")
  }
  if (!is.null(height)) {
    height <- paste0("height: ", height, "px; ")
  }
  float <- paste0("float:", float, "; ")
  margin <- paste0("margin:", margin, ";")
  return(trimws(paste0(width, height, float, margin)))
}

# Creates the html to make a button to an external link
icon_link <- function(
  icon = NULL,
  text = NULL,
  url = NULL
) {
  if (!is.null(icon)) {
    text <- make_icon_text(icon, text)
  }
  return(htmltools::a(href = url, text, class = "icon-link"))
}

make_icon_text <- function(icon, text) {
  return(htmltools::HTML(paste0(
    make_icon(icon), " ", text))
  )
}

make_icon <- function(icon) {
  return(htmltools::tag("i", list(class = icon)))
}

doi <- function(doi) {
  url <- paste0('https://doi.org/', doi)
  return(paste0('DOI: ', htmltools::a(href = url, doi)))
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
  return(htmltools::div(
    class = "masonry-wrapper",
    htmltools::div(class = "masonry", ...)
  ))
}

masonry_item <- function(
  src = NULL,
  title = NULL,
  description = NULL,
  url = NULL
) {
  image <- htmltools::img(src = src)
  if (!is.null(url)) {
    image <- htmltools::a(
      href = url,
      class = "card-hover",
      htmltools::img(src = src))
  }
  return(htmltools::div(class = "masonry-item",
    htmltools::div(
      class = "masonry-content",
      image,
      htmltools::h3(class = "masonry-title", title),
      htmltools::p(class = "masonry-description", description)
    )
  ))
}

create_footer <- function() {

  fill <- '#ededeb'
  height <- '14px'

  footer <- htmltools::HTML(paste0(
  '© 2021 John Paul Helveston [',
  fontawesome::fa('creative-commons', fill = fill, height = height),
  fontawesome::fa('creative-commons-by', fill = fill, height = height),
  fontawesome::fa('creative-commons-sa', fill = fill, height = height),
  '](https://creativecommons.org/licenses/by-sa/4.0/)\n',
  htmltools::br(),
  fontawesome::fa('wrench', fill = fill, height = height), ' Made with ',
  fontawesome::fa('heart', fill = fill, height = height), ', [',
  fontawesome::fa('code-branch', fill = fill, height = height),
  '](https://github.com/jhelvy), and the [',
  fontawesome::fa('r-project', fill = fill, height = height),
  '](https://cran.r-project.org/) ',
  '[distill](https://github.com/rstudio/distill) package\n',
  htmltools::br(),
  last_updated(), "\n\n",

  '<!-- Add function to open links to external links in new tab, from: -->',
  '<!-- https://yihui.name/en/2018/09/target-blank/ -->\n\n',
  '<script src="js/external-link.js"></script>'
  ))

  save_raw(footer, "_footer.html")
}

save_raw <- function(text, path) {
    fileConn <- file(path)
    writeLines(text, fileConn)
    close(fileConn)
}

last_updated <- function() {
  return(htmltools::span(
    paste0(
      'Last updated on ',
      format(Sys.Date(), format="%B %d, %Y")
    ),
    style = "font-size:0.8rem;")
  )
}
