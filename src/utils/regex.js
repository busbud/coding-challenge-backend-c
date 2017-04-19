/**
 * Regex rules used for basic sanitization of the values received as query parameters
 */
const coordinatePattern = /^-?[\d]{0,3}\.?[\d]*$/;
const nameSearchBlacklist = /[/\\^$*_+<>:;~"@#&%!=?.()|[\]{}]/g;

export { coordinatePattern, nameSearchBlacklist };
