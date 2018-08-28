class StringManipulationService {

    /**
     * Remove all dashes and all white spaces from a given string
     * eg. string = 'Montreal - Ouest' will return 'MontrealOuest'
     *
     * @memberof UtilityService
     */
    removeDashAndWhiteSpace(string) {
        return string
            .replace(new RegExp('-', 'g'), '') // remove '-';
            .replace(/\s/g, ""); // remove  all white space;
    }
}

module.exports = StringManipulationService;