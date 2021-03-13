create_footer <- function() {
  footer <- htmltools::HTML(paste0(
  '© 2021 John Paul Helveston ',
  '<a href="https://github.com/jhelvy">',
  '<i class="fab fa-github" aria-hidden="true"></i></a> ',
  '<a href="https://twitter.com/JohnHelveston">',
  '<i class="fab fa-twitter" aria-hidden="true"></i></a> ',
  '<a href="https://www.linkedin.com/in/jhelvy/">',
  '<i class="fab fa-linkedin" aria-hidden="true"></i></a> ',
  '<a href="https://orcid.org/0000-0002-2657-9191">',
  '<i class="ai ai-orcid" aria-hidden="true"></i></a> ',
  '<a href="https://scholar.google.com/citations?user=DY2D56IAAAAJ">',
  '<i class="ai ai-google-scholar" aria-hidden="true"></i></a> ',
  '<a href="https://www.researchgate.net/profile/John_Helveston">',
  '<i class="ai ai-researchgate" aria-hidden="true"></i></a>\n\n',
  '<i class="fas fa-wrench"></i> Made with <i class="far fa-heart"></i>, ',
  '<a href="https://github.com/jhelvy"><i class="fas fa-code-branch"></i></a>',
  ', and the <a href="https://cran.r-project.org/">',
  '<i class="fab fa-r-project"></i></a>',
  '<a href="https://github.com/rstudio/distill"> distill</a> package.',
  '\n\n',
  '<span style="font-size:0.8rem;">Last updated ',
  'on ', format(Sys.Date(), format="%B %d, %Y"), '</span>\n\n',
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
  return(htmltools::HTML(markdown::renderMarkdown(text = text)))
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
link_button <- function(
  icon = NULL,
  text = NULL,
  url = NULL
) {
  if (!is.null(icon)) {
    text <- htmltools::HTML(paste0('<i class="', icon, '"></i> ', text))
  }
  return(htmltools::a(href = url, text, class = "link-button"))
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
