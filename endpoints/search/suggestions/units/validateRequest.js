const yup = require("yup");
const { error } = require("_lib");

const schema = yup.object().shape(
  {
    q: yup.string().required("Please enter a valid string"),
    latitude: yup.number().nullable(),
    longitude: yup.string().nullable(),
  },
  ["latitude", "longitude"]
);

module.exports = async ({ q, location: { latitude = "", longitude = "" } }) => {
  try {
    schema.validateSync({ q, latitude, longitude });
  } catch (err) {
    error("ValidationError", "Please provide valid payload", 500, err);
  }
};
