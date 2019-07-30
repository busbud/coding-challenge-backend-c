/**
 * 
 * @param {Error} err 
 * @returns {object}
 */
const getErrorMsg = err => {
  const response = {};
  if (process.env.NODE_ENV === "dev") {
    response.message = err.message;
  } else {
    response.message = "Oups";
  }
  return response;
};

exports.getErrorMsg = getErrorMsg;
