devtools::install_github("favstats/instaloadeR")

library(instaloadeR)

library(reticulate)

use_python(py_config()$python)

install_instaloadeR()

init_instaloadeR()

corona <- insta_posts(query = "coronavirus", 
                      scope = "hashtag",
                      max_posts = 10, 
                      scrape_comments = F)

#Also return comments and replies to comments for the 10 last #coronavirus posts.

corona_comments <- insta_posts(query = "coronavirus", 
                               scope = "hashtag",
                               max_posts = 10, 
                               scrape_comments = T)

corona_comments

#Return a tibble with the 10 Instagram posts by francediplo.

francediplo <- insta_posts(query = "tioantawibawa", 
                           scope = "username",
                           max_posts = 100, 
                           scrape_comments = F)


View(francediplo)


