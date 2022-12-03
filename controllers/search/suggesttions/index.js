const {
    suggesttions
} = require("./units");

module.exports = async ({
    req,
    res
}) => {
    return await suggesttions();
};