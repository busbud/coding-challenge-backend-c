"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.checkForFIPSCodes = void 0;
const fipsCodesCanada = {
    "01": "AB",
    "02": "BC",
    "03": "MB",
    "04": "NB",
    "05": "NL",
    "07": "NS",
    "08": "ON",
    "09": "PE",
    "10": "QC",
    "11": "SK",
    "12": "NT",
    "13": "NU",
    "14": "YT"
};
const checkForFIPSCodes = (stateAdminCode, country) => {
    if (country === 'Canada' && stateAdminCode) {
        if (fipsCodesCanada[stateAdminCode]) {
            console.log(fipsCodesCanada[stateAdminCode]);
            return fipsCodesCanada[stateAdminCode];
        }
    }
    return stateAdminCode;
};
exports.checkForFIPSCodes = checkForFIPSCodes;
//# sourceMappingURL=FIPSCodesCanada.js.map