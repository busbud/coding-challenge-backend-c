
const accentMap = {
    'a': 'á|à|ã|â|À|Á|Ã|Â',
    'e': 'é|è|ê|É|È|Ê',
    'i': 'í|ì|î|Í|Ì|Î',
    'o': 'ó|ò|ô|õ|Ó|Ò|Ô|Õ',
    'u': 'ú|ù|û|ü|Ú|Ù|Û|Ü',
    'c': 'ç|Ç',
    'n': 'ñ|Ñ'
};

export const normalizeText = (text: string) => {
    const noAccent = Object.keys(accentMap).reduce((acc, cur) => acc.replace(new RegExp(accentMap[cur], 'g'), cur), text)
    return noAccent.toLowerCase().trim();
}