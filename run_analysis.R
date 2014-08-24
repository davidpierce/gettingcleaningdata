# run_analysis.R

# base path of the raw data
raw.data.dir <- "UCI HAR Dataset"

# raw.filename(...)
#
# Returns a path into the raw data from the arguments.
#
raw.filename <- function(...) {
    paste(raw.data.dir, ..., sep="/")
}

# raw.data.filename(DIR, FILE, SUFFIX=".txt")
#
# Returns a raw data filename.
# Example: raw.data.filename("test", "subject")
# ==> "UCI HAR Dataset/test/subject_test.txt"
#
raw.data.filename <- function(dir, file, suffix=".txt") {
    raw.filename(dir, paste(file, "_", dir, suffix, sep=""))
}

# order.by.index(X)
#
# Returns the 2nd column of X in the order determined by the 1st column.
#
order.by.index <- function(x) {
    x[order(x[,1]), 2]
}

# read.activities
#
# Returns the factor of HAR activities.
# See activity_labels.txt in the raw data.
#
read.activities <- function() {
    x <- read.table(raw.filename("activity_labels.txt"), sep=" ")
    x <- order.by.index(x)
    names(x) <- x
    x
}

# as.activity
#
# Converts argument vector into activities if possible.
# Arguments can be numeric activity codes or activity names.
#
as.activity <- function(x) {
    activities <- read.activities()
    sapply(x, function(z) {
        if (is.numeric(z)) {
            activities[z]
        } else {
            activities[toupper(as.character(z))]
        }
    })
}

# read.features
#
# Returns the factor of HAR features.
# See features.txt in the raw data.
#
read.features <- function() {
    x <- read.table(raw.filename("features.txt"))
    order.by.index(x)
}

# select.features(X)
#
# Returns the mean and std features from X.
#
select.features <- function(x) {
    grep("(mean|std)\\(\\)", x, value=T)
}

# rename.feature(X)
#
# Returns a more descriptive name for X, of the form
#     Domain.Instrument.Filter.Axis.Statistic
# If X is not one of the expected features, throws a stop.
#
rename.feature <- function(x) {
    # check to make sure input is expected form
    if (!grepl("^[tf](Gravity|Body|BodyBody)(Acc|Gyro)(Jerk)?(Mag)?-(mean|std)\\(\\)(-[XYZ])?$", x)) {
        stop("bad feature ", x)
    }
    # extract the domain, instrument, filter, axis, and statistic
    domain <- ifelse(sub("^([tf]).*", "\\1", x) == "t", "Time", "Frequency")
    instrument <- sub("^Acc", "Accelerometer",
                      sub("^Gyro", "Gyroscope",
                          sub(".*(Acc|AccJerk|Gyro|GyroJerk).*", "\\1", x)))
    filter <- sub(".*?(Gravity|Body|BodyBody).*", "\\1", x)
    axis <- sub("^Mag", "Magnitude",
                sub(".*(Mag|X|Y|Z).*", "\\1", x))
    stat <- sub(".*(mean|std).*", "\\1", x)
    # construct the descriptive name
    paste(domain, instrument, filter, axis, stat, sep=".")
}

# tidify.raw.data(DIR)
#
# Reads the HAR data from DIR and cleans it up.
# - reads subjects, activities, features, and variables
# - converts activity codes into activity factor
# - selects mean and std features from the data
# - constructs descriptive column names for the features
#
tidify.raw.data <- function(dir) {
    # read subjects, activities, features, and variables from respective files
    subjects <- read.table(raw.data.filename(dir, "subject"))[,1]
    activities <- as.activity(read.table(raw.data.filename(dir, "y"))[,1])
    features <- read.features()
    data <- read.table(raw.data.filename(dir, "X"))
    names(data) <- features
    # subset only mean and std features
    features <- select.features(features)
    data <- data[, features]
    # glue subjects, activities, variables together
    data <- cbind(subjects, activities, data)
    # insert descriptive names for columns
    names(data) <- c("Subject", "Activity", sapply(features, rename.feature))
    data
}

# summarize.subject.activity(DATA)
#
# Groups rows by Subject & Activity and computes the mean of the variables
# for each group.  DATA is a data frame of the sort produced by
# tidify.raw.data.
#
summarize.subject.activity <- function(data) {
    dataframes <- split(data, data[, c("Subject", "Activity")])
    result <- data.frame()
    for (i in seq_along(dataframes)) {
        df <- dataframes[[i]]
        result[i, "Subject"] <- df[1, "Subject"]
        result[i, "Activity"] <- df[1, "Activity"]
        for (j in 3:ncol(df)) {
            result[i, names(df)[j]] <- mean(df[, j])
        }
#         n <- ncol(df)
#         result[i, c("Subject", "Activity")] <- df[1, c("Subject", "Activity")]
#         result[i, names(df)[3:n]] <- colMeans(df[, 3:n])
    }
    result
}

# main(DIR)
#
# Produces the har_mean_std and har_subject_activity data sets
# from the raw HAR data in DIR.
#
main <- function(dir) {
    raw.data.dir <- dir
    dir.create("tidy", recursive=TRUE, showWarnings=FALSE)
    train <- tidify.raw.data("train")
    test <- tidify.raw.data("test")
    har_mean_std <- rbind(train, test)
    write.table(har_mean_std, "tidy/har_mean_std.txt", row.names=FALSE)
    har_subject_activity <- summarize.subject.activity(har_mean_std)
    write.table(har_subject_activity, "tidy/har_subject_activity.txt", row.names=FALSE)
}

main("UCI HAR Dataset")
