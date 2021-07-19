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

# Functions for projects page: https://jhelvy.github.io/projects

make_posts_page <- function(posts) {
  categories <- get_categories(posts)  
  posts <- unite_post_categories(posts)
  posts_html <- make_posts_html(posts)
  cats_html <- make_cats_html(categories)
  return(
    div(class = "posts-container posts-with-sidebar posts-apply-limit l-screen-inset", 
      posts_html,
      cats_html
  ))
}

make_cats_html <- function(categories) {
  
}

get_post_categories <- function(posts) {
  return(setdiff(
    names(posts), 
    c("title", "src", "url", "description")))  
}

unite_post_categories <- function(posts) {
  posts <- posts %>% 
    pivot_longer(
      names_to = "categories", 
      values_to = "val", 
      cols = categories) %>% 
    mutate(val = ifelse(val == 1, categories, NA_character_)) %>% 
    pivot_wider(
      names_from = categories, 
      values_from = val) %>% 
    unite(
      -c('title', 'src', 'url', 'description'), 
      col = "categories", sep = ";", na.rm = T)
  return(posts)
}

make_posts_html <- function(posts) {
  posts_content <- list()
  for (i in seq_len(length(posts))) {
    posts_content[[i]] <- make_post_content(posts[i,])
  }
  return(div(class = "posts-list", posts_content))
}

make_post_content <- function(post) {
  post_cats <- strsplit(post$categories, ";")[[1]]
  post_tags <- get_post_tags(post_cats)
  return(a(
    href = post$url,
    class = "post-preview",
    HTML(
      paste0(
        '<script class="post-metadata" type="text/json">{"categories":[',
        paste0(paste0('"', post_cats, '"'), collapse = ","),
        ']}</script>'
      )
    ),
    div(class = "thumbnail",
      img(src = post$src)
    ),
    div(class = "description",
      h2(post$title),
      post_tags,
      p(post$description)
    )
  ))
}

get_post_tags <- function(post_cats) {
  cats <- list()
  for (i in seq_len(length(post_cats))) {
    cats[[i]] <- div(class = "dt-tags", post_cats[i])
  }
  return(div(class = "dt-tags", cats))
}






masonry_buttons <- function(categories) {
  cat_button <- tagList(lapply(categories, function(x) {
      tags$button(
        class="btn",
        onclick = paste0("filterSelection('", x, "')"),
        stringr::str_to_title(x)
      )
  }))
  buttons_html <- div(
    id = "myBtnContainer",
    style = "text-align: center;",
    tags$button(
      class="btn active",
      onclick="filterSelection('all')",
      "Show all"
    ),
    cat_button
  )
  return(buttons_html)
}

masonry_projects_grid <- function(projects) {
  projects_html <- list()
  for (i in seq_len(nrow(projects))) {
    projects_html[[i]] <- masonry_item(projects[i,])
  }
  return(masonry_grid(projects_html))
}

masonry_grid <- function(...) {
  return(div(
    class = "masonry-wrapper",
    div(class = "masonry", ...)
  ))
}

masonry_item <- function(project) {
  image <- img(src = project$src)
  if (!is.null(project$url)) {
    image <- a(
      href = project$url,
      class = "card-hover",
      img(src = project$src))
  }
  return(
    div(class = paste0("filterDiv ", project$categories),
      div(class = "masonry-item",
        div(class = "masonry-content",
          image,
          h3(class = "masonry-title", project$title),
          p(class = "masonry-description", project$description)
        )
      )
    )
  )
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
