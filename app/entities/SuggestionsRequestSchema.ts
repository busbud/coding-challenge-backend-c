export const SuggestionsRequestSchema = {
    type: 'object',
    properties: {
        q: {
            type: 'string',
            allOf: [
                {
                    transform: ['trim']
                },
                {
                    minLength: 1
                }
            ]
        },
        latitude: {
            type: 'number'
        },
        longitude: {
            type: 'number'
        }
    },
    required: ['q'],
    additionalProperties: false
}
