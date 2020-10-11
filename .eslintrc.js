module.exports = {
  extends: ["plugin:security/recommended"],
  plugins: ["security"],
  env: {
    es6: true,
    browser: true,
  },
  rules: {
    "brace-style": ["error", "stroustrup"],
    "comma-dangle": ["error", "never"],
    "no-unused-vars": ["warn"],
    "no-var": ["off"],
    "one-var": ["off"],
    indent: ["error", "space"],
  },
  prettier: {},
};
