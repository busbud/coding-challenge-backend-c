const concatRedisResult = (i) => (results) => [].concat(...results.map((r) => r[i]))

export const concatErrors = concatRedisResult(0)
export const concatReplies = concatRedisResult(1)
