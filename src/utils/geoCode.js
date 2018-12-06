module.exports.getCanadianCode = 
/**
 * Convert Canadian state code
 * @param {string} code Code of the state
 */
function getCanadianCode(code) {
    switch(code) {
        case "01":
            return "AB";
        case "02":
            return "BC";
        case "03":
            return "MB";
        case "04":
            return "NB";
        case "05":
            return "NL";
        case "07":
            return "NS";
        case "08":
            return "ON";
        case "09":
            return "PE";
        case "10":
            return "QC";
        case "11":
            return "SK";
        case "12":
            return "YT";
        case "13":
            return "NT";
        case "14":
            return "NU";
        default:
            return code;
    }
};