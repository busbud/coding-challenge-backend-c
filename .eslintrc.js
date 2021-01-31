module.exports = {
    env: {
        browser: true,
        commonjs: true,
        es2021: true,
        jest: true,
    },
    extends: [
        'airbnb-base',
    ],
    parserOptions: {
        ecmaVersion: 12,
    },
    rules: {
        'max-len': [1, 120, 4],
        indent: ['error', 4],
        'no-underscore-dangle': ['error', { allowAfterThis: true }],
    },
};
