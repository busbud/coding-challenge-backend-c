
const accentMap = {
    'a': 'á|à|ã|â|À|Á|Ã|Â',
    'e': 'é|è|ê|É|È|Ê',
    'i': 'í|ì|î|Í|Ì|Î',
    'o': 'ó|ò|ô|õ|Ó|Ò|Ô|Õ',
    'u': 'ú|ù|û|ü|Ú|Ù|Û|Ü',
    'c': 'ç|Ç',
    'n': 'ñ|Ñ',
    '': /[^a-zA-Z0-9\u00C0-\u00FF]+/g
};

export const normalizeText = (text: string): string => {
    const noAccent = Object.keys(accentMap).reduce((acc, cur) => acc.replace(new RegExp(accentMap[cur], 'g'), cur), text)
    return noAccent.toLowerCase().trim();
}

export const chunk = (word: string): string[] => {
    const normalized = `-${normalizeText(word)}-`;
    const result: string[] = [];
    for (let x = 0; x < normalized.length - 2; ++x) {
        result.push(normalized.slice(x, x + 3));
    }
    return result;
}