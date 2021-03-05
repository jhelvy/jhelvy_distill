
# Function for generating the html used to create the link cards
# on my teaching page: https://jhelvy.github.io/teaching

make_link_card <- function(
  title = NULL,
  subtitle = NULL,
  description = NULL,
  url = NULL,
  image_src = NULL,
  image_width = 120
) {

  x <- htmltools::HTML(paste0(
    '<div>
        <img src="', image_src, '" style="float:left; width:', image_width, 
    'px;">',
        '<h3 style="margin-bottom: 5px;"><a href=', url, '>', title, '</a></h3>',
        '<p style="margin-bottom: 5px;">', subtitle, '</p>',
        '<span style="font-size:0.8rem;">', description, '</span>',
    '</div>'
  ))

  return(x)
}
