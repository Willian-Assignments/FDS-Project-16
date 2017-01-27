# Yun Yan (yy1533@nyu.edu)

infile <- list.files(path=file.path(getwd(), 'data'), pattern='*.csv',
                     full.names = T)

data <- bind_rows(lapply(infile, function(x) read.csv(x, stringsAsFactors=FALSE)))
