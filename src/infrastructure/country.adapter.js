const findByCode = async (code) => new Promise((resolve, reject) => {
    switch (code) {
        case 'US':
            return resolve(buildUSCountry());
        case 'CA':
            return resolve(buildCACountry());
        default:
            return reject('Cannot found country with code ' + code)
    }
})

const buildUSCountry = () => {
    return {
        code: "US",
        name: "United States of America",
        display: "USA"
    }
}

const buildCACountry = () => {
    return {
        code: "CA",
        name: "Canada",
        display: "Canada"
    }
}

module.exports.findByCode = findByCode