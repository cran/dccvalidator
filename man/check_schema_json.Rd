% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-schema-json.R
\name{check_schema_json}
\alias{check_schema_json}
\title{Check data against a JSON Schema}
\usage{
check_schema_json(
  json,
  schema,
  success_msg = "Data is valid against the schema",
  fail_msg = "Data is invalid against the schema"
)
}
\arguments{
\item{json}{Contents of a json object, or a filename containing
one.}

\item{schema}{Contents of the json schema, or a filename
containing a schema.}

\item{success_msg}{Message indicating the check succeeded.}

\item{fail_msg}{Message indicating the check failed.}
}
\value{
A condition object indicating whether the data is valid against the
schema.
}
\description{
Check a JSON blob against a JSON Schema.
}
\examples{
if (requireNamespace("jsonvalidate", quietly = TRUE)) {
schema <- '{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "properties": {
    "x": {
      "type": "integer"
    }
  },
  "required": ["x"]
}
'
json_valid <- '{
  "x": 3
}'
json_invalid <- '{
  "x": 1.5
}'
check_schema_json(json_valid, schema)
check_schema_json(json_invalid, schema)
}
}
