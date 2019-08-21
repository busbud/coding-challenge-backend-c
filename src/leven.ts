import levenshtein from 'js-levenshtein'

export const THRESHOLD = 0.4

export const similarity = (needle: string, haystack: string): number => {
  if (needle === haystack) {
    return 1
  }

  const longest = Math.max(needle.length, haystack.length)

  return (longest - levenshtein(needle, haystack)) / longest
}
