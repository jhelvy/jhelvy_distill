
# Function for generating the html used to create a two-column layout 
# with a thumbnail image, a title, and some text:
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
