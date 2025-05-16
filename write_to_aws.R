
library(paws)

s3 <- s3()

bucket_name <- "hfv-sourcebook-data"
file_path <- "data/rds/pop_data.rds"
s3_key <- "rds/pop_data.rds"

s3$put_object(
  Bucket = bucket_name,
  Key = s3_key,
  Body = readBin(file_path, "raw", file.size(file_path))
)

cat("File successfully uploaded to S3:", s3_key, "in bucket:", bucket_name, "\n")

# Get the object from S3
response <- s3$get_object(
  Bucket = bucket_name,
  Key = s3_key
)

# Create a temporary file to save the S3 object content
temp_file <- tempfile(fileext = ".rds")
writeBin(response$Body, temp_file)

# Read the RDS file
pop_data_from_aws <- readRDS(temp_file)

# Clean up
file.remove(temp_file)