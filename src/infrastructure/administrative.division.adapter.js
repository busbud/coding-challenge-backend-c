const findCodeByFipsCode = async (countryCode, fipsCode) => new Promise((resolve, reject) => {
    let code;
    switch (countryCode) {
        case 'US':
            code = fipsCode;
            break;
        case 'CA':
            code = resolveCanadaDivisions(fipsCode);
            break;
    }
    if (code === null) {
        reject(`Cannot found administrative divisions for country code ${countryCode} and fipsCode ${fipsCode}`)
    }

    resolve(code)
})

const resolveCanadaDivisions = (fipsCode) => {
    switch (fipsCode) {
        case '01':
            return 'AB';
        case '02':
            return 'BC';
        case '03':
            return 'MB';
        case '04':
            return 'NB';
        case '13':
            return 'NT';
        case '07':
            return 'NS';
        case '14':
            return 'NU';
        case '08':
            return 'ON';
        case '09':
            return 'PE';
        case '10':
            return 'QC';
        case '11':
            return 'SK';
        case '12':
            return 'YT';
        default:
            return null;
    }
}

module.exports.findCodeByFipsCode = findCodeByFipsCode
