function init_posts_list() {

  function load_image(img) {
    var src = $(img).attr('data-src');
    if (src) {
      $(img).attr('src', src);
      $(img).load(function() {
        img.removeAttribute('data-src');
      });
    }
  }

  function set_posts_visible(posts, visible) {
    if (visible) {

      // show bottom border by default
      $(posts).removeClass('post-preview-last');

      // apply limits if need be
      var max_posts = 25;
      var apply_limits = $('.posts-container').hasClass('posts-apply-limit');
      if (apply_limits && posts.length > max_posts) {
        posts = $(posts).slice(0, max_posts);
      } else {
        $('.posts-more a').addClass('hidden');
      }

      // apply last style
      $(posts.slice(-1)[0]).addClass('post-preview-last');

      $(posts).removeClass('hidden');
      $(posts).find('img[data-src]').each(function(i, img) {
        load_image(img);
      });
    } else {
      $(posts).addClass('hidden');
    }
  }

  function apply_hash_filter() {

    // clear active state
    $('.categories .active').removeClass('active');

    // mark all posts invisible to start
    set_posts_visible($('.posts-list').children('a'), false);

    // if we have a hash filter
    if (window.location.hash && window.location.hash.startsWith("#category:")) {

      // mark posts that match the category visible
      var page_category = window.location.hash.replace(/^#category:/, "");
      var posts = $('.post-metadata').map(function(idx, script) {
        var metadata = $.parseJSON($(script).html());
        var post = null;
        $.each(metadata.categories, function(idx, category) {
          category = category.replace(/ /g,"_");
          if ((page_category || '').toLowerCase() === "articles" || category === page_category) {
            post = $(script).parent().get();
            return false;
          }
        });
        return post;
      });
      set_posts_visible(posts, true);

      // mark the hash active
      $('.categories li>a[href="' + window.location.hash + '"]').addClass('active');

      // update the list_caption
      var list_caption = $('.posts-list-caption');
      var caption = (page_category || '').toLowerCase() === "articles"
        ? list_caption.attr('data-caption')
        : ('Category: ' + page_category.replace(/_/g," "));
      list_caption.text(caption);

    } else {

      // no hash filter, make all posts visible (subject to max display)
      set_posts_visible($('.posts-list').children(), true);

      // reset list caption
      var list_caption = $('.posts-list-caption');
      list_caption.text(list_caption.attr('data-caption'));


    }
  }

  // more articles
  function apply_post_limits(apply) {
    if (apply) {
      $('.posts-container').addClass('posts-apply-limit');
      $('.posts-more a').removeClass('hidden');
    } else {
      $('.posts-container').removeClass('posts-apply-limit');
      $('.posts-more a').addClass('hidden');
    }
  }

  // click handling for tags
  $('.dt-tag').click(function(ev) {
    window.location.hash = '#category:' + $(this).text().replace(/ /g, "_");
    return false;
  })

  // hash filter handling
  apply_hash_filter();
  $(window).on('hashchange',function() {
    apply_post_limits(true);
    apply_hash_filter();
  });

  // more articles link
  $('.posts-more a').click(function(e) {
    e.preventDefault();
    apply_post_limits(false);
    apply_hash_filter();
    return false;
  });

}
