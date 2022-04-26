library(mmmm)

training_batch <- data.frame(cbind(DMat0,FMat0, outputValues))
input_names <- colnames(training_batch)

vertica_setup(server = "aws_prod",
              user = Sys.getenv("mmm_user"),
              password = Sys.getenv("mmm_pass"))

# Use the product_id associated with the version of your product
mmmm::new_training_batch(
  product_id = "mdl-marketingMixModel-1.0",
  training_batch_date = Sys.time(),
  training_batch = training_batch,
  #categorical_cols = c('Pclass', 'Sex'),
  continuous_cols = input_names,
  output_cols = c('outputValues'))