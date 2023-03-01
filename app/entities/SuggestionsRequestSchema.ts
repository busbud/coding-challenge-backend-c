export const SuggestionsRequestSchema = {
    type: 'object',
    properties: {
        q: {
            type: 'string'
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
