library(shiny)
library(rtweet)
library(tweetbotornot2)
library(ggplot2)


# Define server logic required to draw a histogram
server <- function(input, output) {
  enteruser <- eventReactive(input$go, {
    gsub("\\s+|@", "", input$user)
  }, ignoreNULL = FALSE)
  output$probPlot <- renderPlot({
    ggplot2::theme_set(
      ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(
          legend.position = "none",
          plot.background = ggplot2::element_rect(colour = "#F6F8FA", fill = "#F6F8FA"),
          panel.background = ggplot2::element_rect(fill = "#F6F8FA", colour = "#F6F8FA"),
          panel.grid.major = ggplot2::element_line(colour = "#cccccc", size = ggplot2::rel(0.3)),
          panel.grid.minor = ggplot2::element_line(colour = "#cccccc", size = ggplot2::rel(0.15)),
          axis.text = ggplot2::element_text(colour = "#222222"),
          plot.title = ggplot2::element_text(size = ggplot2::rel(1.5), face = "bold"),
          plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.0)),
          plot.caption = ggplot2::element_text(colour = "#333333", size = ggplot2::rel(.9)))
    )

    is_warning <- function(x) inherits(x, c("error", "warning"))
    user <- ""
    if (is.null(user) || length(user) == 0) user <- ""
    user <- trimws(user)
    if (grepl("http", user)) {
      m <- gregexpr("(?<=twitter\\.com/)[^/]+", user, perl = TRUE)
      user <- regmatches(user, m)[[1]]
      if (length(user) == 0) {
        stop("Must enter a valid screen name or a twitter.com link to a user")
      }
    }
    user <- gsub("\\s+|@", "", user)
    if (user == "") {
      d <- data.frame(screen_name = user, prob_bot = .50, stringsAsFactors = FALSE)
    } else {
      ttoken <- readRDS(".rtweet_token.rds")
      tml <- tryCatch(rtweet::get_timelines(user, n = 200, token = ttoken),
        warning = function(w) return(w),
        error = function(e) return(e))
      if (is_warning(tml) && grepl("Not authorized", as.character(tml))) {
        stop("Oops. That user must have their visibility set to private.")
      }
      if (is_warning(tml) && grepl("that page does not exist", as.character(tml))) {
        stop("Oops. I couldn't find that user.")
      }
      if (is.null(tml) || NROW(tml) == 0L || !is.data.frame(tml)) {
        Sys.sleep(1)
        tml <- tryCatch(rtweet::get_timelines(user, n = 200, token = ttoken),
          warning = function(w) return(w),
          error = function(e) return(e))
        if (is_warning(tml) && grepl("Not authorized", as.character(tml))) {
        stop("Oops. That user must have their visibility set to private.")
        }
        if (is_warning(tml) && grepl("that page does not exist", as.character(tml))) {
          stop("Oops. I couldn't find that user.")
        }
        if (is.null(tml) || NROW(tml) == 0L || !is.data.frame(tml)) {
        stop("Oops. I'm most likely being rate limited by Twitter!",
          call. = FALSE)
        }
        #unlink(".rtweet_token.rds")
        user <- tml$screen_name[1]
        d <- tweetbotornot2::predict_bot(tml)
        d$n <- 1L
      }
    }

    if (user == "") {
      title <- "Is the Twitter account a bot or not?"
    } else {
      if (d$prob_bot > .6) {
        title <- paste0("@", user, " is probably a bot")
      } else if (d$prob_bot >= .4) {
        title <- paste0("Not sure if @", user, " is a bot or not")
      } else {
        title <- paste0("@", user, " is probably not a bot")
      }
    }
    d$q <- qnorm(d$prob_bot)
    x <- seq(-3, 3, .01)
    df <- data.frame(x = x, y = pnorm(x))
    alpha_bot <- d$prob_bot
    if (alpha_bot < .1) alpha_bot <- .1
    alpha_not <- 1 - d$prob_bot
    if (alpha_not < .1) alpha_not <- .1

    cpt <- paste0(
      "tweetbotornot2: An R package for detecting Twitter bots\n",
      "Estimate based on machine learning model with 95% accuracy"
    )

    ggplot2::ggplot(df, aes(x = x, y = y)) +
      ggplot2::geom_line(size = 1, alpha = .7) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(face = "italic",
        hjust = .95),
        plot.caption = ggplot2::element_text(
          size = ggplot2::rel(.6), color = "#808c9f")) +
      ggplot2::geom_segment(data = d, aes(x = q - (2.9 + q), xend = q, y = prob_bot,
        yend = prob_bot), colour = "#880099", alpha = .85,
        size = .9, linetype = 2) +
      ggplot2::geom_point(data = d, shape = 21, aes(x = q, y = prob_bot),
        size = 8, fill = "#ee00ff") +
      ggplot2::labs(x = NULL, y = "Probability of Bot",
        title = title,
        subtitle = "Estimated probability of user being an automated account",
        caption = cpt) +
      ggplot2::annotate("text", x = 1.75, .30, label = "NOT!", fontface = "bold",
        size = 20, colour = "#00aa00",
        alpha = alpha_not) +
      ggplot2::annotate("text", x = -1.75, .70, label = "BOT!", fontface = "bold",
        size = 20, colour = "#bb0000",
        alpha = alpha_bot) +
      ggplot2::annotate("text", x = -3.18, d$prob_bot,
        label = gsub("0\\.", ".", sprintf("%.3f", d$prob_bot)),
        fontface = "bold",
        colour = "#ee00ff", size = 5) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, .25),
        labels = c(".00", ".25", ".50", ".75", "1.00"))

    user <- enteruser()


    is_warning <- function(x) inherits(x, c("error", "warning"))

    if (is.null(user) || length(user) == 0) user <- ""
    user <- trimws(user)
    if (grepl("http", user)) {
      m <- gregexpr("(?<=twitter\\.com/)[^/]+", user, perl = TRUE)
      user <- regmatches(user, m)[[1]]
      if (length(user) == 0) {
        stop("Must enter a valid screen name or a twitter.com link to a user")
      }
    }
    user <- gsub("\\s+|@", "", user)
    if (user == "") {
      d <- data.frame(screen_name = user, prob_bot = .50, stringsAsFactors = FALSE)
    } else {
      ttoken <- readRDS(".rtweet_token.rds")
      tml <- tryCatch(rtweet::get_timelines(user, n = 200, token = ttoken),
        warning = function(w) return(w),
        error = function(e) return(e))
      if (is_warning(tml) && grepl("Not authorized", as.character(tml))) {
        stop("Oops. That user must have their visibility set to private.")
      }
      if (is_warning(tml) && grepl("that page does not exist", as.character(tml))) {
        stop("Oops. I couldn't find that user.")
      }
      if (is.null(tml) || NROW(tml) == 0L || !is.data.frame(tml)) {
        stop("Oops. I'm most likely being rate limited by Twitter!",
          call. = FALSE)
      }
      #unlink(".rtweet_token.rds")
      user <- tml$screen_name[1]
      d <- tweetbotornot2::predict_bot(tml)
      d$n <- 1L
    }

    if (user == "") {
      title <- "Is the Twitter account a bot or not?"
    } else {
      if (d$prob_bot > .6) {
        title <- paste0("@", user, " is probably a bot")
      } else if (d$prob_bot >= .4) {
        title <- paste0("Not sure if @", user, " is a bot or not")
      } else {
        title <- paste0("@", user, " is probably not a bot")
      }
    }
    d$q <- qnorm(d$prob_bot)
    x <- seq(-3, 3, .01)
    df <- data.frame(x = x, y = pnorm(x))
    alpha_bot <- d$prob_bot
    if (alpha_bot < .1) alpha_bot <- .1
    alpha_not <- 1 - d$prob_bot
    if (alpha_not < .1) alpha_not <- .1

    cpt <- paste0(
      "tweetbotornot2: An R package for detecting Twitter bots\n",
      "Estimate based on machine learning model with 95% accuracy"
    )

    ggplot2::ggplot(df, aes(x = x, y = y)) +
      ggplot2::geom_line(size = 1, alpha = .7) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(face = "italic",
        hjust = .95),
        plot.caption = ggplot2::element_text(
          size = ggplot2::rel(.6), color = "#808c9f")) +
      ggplot2::geom_segment(data = d, aes(x = q - (2.9 + q), xend = q, y = prob_bot,
        yend = prob_bot), colour = "#880099", alpha = .85,
        size = .9, linetype = 2) +
      ggplot2::geom_point(data = d, shape = 21, aes(x = q, y = prob_bot),
        size = 8, fill = "#ee00ff") +
      ggplot2::labs(x = NULL, y = "Probability of Bot",
        title = title,
        subtitle = "Estimated probability of user being an automated account",
        caption = cpt) +
      ggplot2::annotate("text", x = 1.75, .30, label = "NOT!", fontface = "bold",
        size = 20, colour = "#00aa00",
        alpha = alpha_not) +
      ggplot2::annotate("text", x = -1.75, .70, label = "BOT!", fontface = "bold",
        size = 20, colour = "#bb0000",
        alpha = alpha_bot) +
      ggplot2::annotate("text", x = -3.18, d$prob_bot,
        label = gsub("0\\.", ".", sprintf("%.3f", d$prob_bot)),
        fontface = "bold",
        colour = "#ee00ff", size = 5) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, .25),
        labels = c(".00", ".25", ".50", ".75", "1.00"))
  })
}
