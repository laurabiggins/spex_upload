library(jsonlite)
x <- jsonlite::fromJSON(txt = "inst/extdata/arrays.txt")
ds_name <- "my new dataset"
ds_citation <- "names and journal"
ds_summary_info <- "some blurb here"
ds_data_type <- "other"

new_ds <- c(ds_name, ds_citation, ds_summary_info, ds_data_type)

x$data <- rbind(x$data, new_ds)
jsonlite::toJSON(x)
jsonlite::write_json(x, path = "inst/extdata/updated_arrays.txt")


