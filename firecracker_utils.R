
to_seconds_recur <- function(x, val) {
  if (length(x) == 0)
    return(val)

  val <- 60 * val + x[1]
  return(to_seconds_recur(x[-1], val))
}

to_seconds <- function(x) {
  return(to_seconds_recur(as.integer(x), 0))
}

# get_data <- function(filename) {
#   data <- read.fwf(
#     filename,
#     widths = c(5, 10, 27, 3, 1, 17, 8, 5),
#     stringsAsFactors = FALSE
#   )
# 
#   data <- clean_data(data)
#   return(data)
# }
# 
read_raw <- function(text) {
  con <- textConnection(text)
  data <- readLines(con)
  close(con)
  
  return(data)
}

discard_header <- function(data) {
  index <- 1 # index of the _next_ line to be read
  for (line in data) {
    index <- index + 1
    if (grepl("^=", line)) {
      break
    }
  }

  return(data[index:length(data)])
}

discard_blanks <- function(data) {
  blanks <- grepl("^$", data)

  return(data[!blanks])
}

check_raw_data <- function(sig, data) {
  if (!any(grepl(pattern = sig, x = data, fixed=TRUE))) {
    print(sprintf("Failed to find signature string '%s'\n", sig))
    return(FALSE)
  }
  return(TRUE)
}

get_raw_data_from_url <- function(url) {
  html <- htmlParse(url)
  pres <- getNodeSet(html, "//pre")

  return(pres)
}

get_data_from_url <- function(year) {
  url <- paste("http://www.santacruzfirecracker10k.org/race-results-", year,
  sep="")
  html <- htmlParse(url)
  pres <- getNodeSet(html, "//pre")
  data5k <- xmlValue(pres[[1]])
  data10k <- xmlValue(pres[[2]])

  data5k <- read_raw(data5k)
  data10k <- read_raw(data10k)

  # Check the data
#  checked5k <- check_raw_data("5-K Results", data5k)
#  checked10k <- check_raw_data("10-K Results", data10k)
#
#  if (!checked5k)
#    print("didn't manage to read good 5k data, giving up.")
#  if (!checked10k)
#    print("didn't manage to read good 10k data, giving up.")
#
#  if (!checked5k || !checked10k)
#    return(c(NA, NA))

  # discard up to and including the header line ("====...")
  data5k <- discard_header(data5k)
  data10k <- discard_header(data10k)

  # ... and discard any remaining blank lines.
  data5k <- discard_blanks(data5k)
  data10k <- discard_blanks(data10k)

  data5k <- parseit(data5k, year)
  data10k <- parseit(data10k, year)

  data5k <- clean_data(data5k)
  data10k <- clean_data(data10k)

  return(list(data5k, data10k))
}

parseit <- function(data, year) {
  len <- length(data)
  the_names <- 
    c("Rank", "Age.Rank", "Name", "Age", "Gender", "City", "Time", "Pace")
  newdata <- data.frame(stringsAsFactors=FALSE,
    rep(1, len),
    rep("X", len),
    rep("X", len),
    rep(1, len),
    rep("X", len),
    rep("X", len),
    rep("X", len),
    rep("X", len)
  )

  names(newdata) <- the_names

  for (i in 1:len) {
    line <- data[i]
    newrow <- clean_line(line, year)
    newdata[i, ] <- newrow
  }

  # Hacky - set type on int fields.
  newdata$Age <- as.numeric(newdata$Age)
  newdata$Rank <- as.numeric(newdata$Rank)

  return(newdata)
}

# sample of 2015 data:
#0        1         2         3         4         5         6         7
#1234567890123456789012345678901234567890123456789012345678901234567890123456
#    1   1/12   JAY THOMSON         35 M SANTA CRUZ        16:01  5:10
#  325  10/10   MARIA CAVA          23 F PARK CITY       1:06:16 21:20 

# sample of 2014 data:
#0        1         2         3         4         5         6         7
#1234567890123456789012345678901234567890123456789012345678901234567890123456
#    1   1/8    NICK SHERRELL              21 M BEN LOMOND        16:48  5:25 
#  276  18/18   CINDY DUMESNY              46 F                 1:11:29 23:01 
clean_line <- function(line, year) {
  fields <- c(as.character())
  if (year == 2015) {
    starts <- c(1,  6, 16, 36, 39, 40, 57, 64)
    stops <-  c(5, 15, 35, 38, 39, 56, 63, 74)
  } else {
    starts <- c(1,  6, 16, 43, 46, 47, 64, 72)
    stops <-   c(5, 15, 42, 45, 46, 63, 71, 76)
  } 
  
  for (i in 1:length(starts)) {
    start <- starts[i]
    stop <- stops[i]

    str <- substring(line, start, stop)
    str <- str_trim(str)
    fields <- c(fields, str)
  }

  return(fields)
}

clean_data <- function(data) {
  the_names <-
    c("Rank", "Age.Rank", "Name", "Age", "Gender", "City", "Time", "Pace")
  names(data) <- the_names
  data$Age.Rank <- str_trim(data$Age.Rank)
  data$Name <- str_trim(data$Name)
  data$Pace <- str_trim(data$Pace)

  # Remove data with missing age.
  data <- subset(data, !is.na(Age))

  runtime <- 
    sapply(data$Time, FUN=function(x) unlist(strsplit(as.character(x), ':')))

  time2 <-
    sapply(runtime, FUN=function(x) to_seconds(unlist(x)))

  data$Time <- time2
  return(data)
}

