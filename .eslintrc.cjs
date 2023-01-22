module.exports = {
  root: true,
  parser: "@typescript-eslint/parser",
  plugins: ["@typescript-eslint", "prettier"],
  env: {
    node: true,
  },
  extends: [
    "eslint:recommended",
    "plugin:@typescript-eslint/recommended",
    "plugin:prettier/recommended",
    "prettier/@typescript-eslint",
  ],
  rules: {
    "@typescript-eslint/explicit-module-boundary-types": ["off"],
    "@typescript-eslint/no-empty-interface": ["warn"],
    "@typescript-eslint/no-unused-vars": [
      "warn",
      { vars: "local", args: "none", ignoreRestSiblings: true, varsIgnorePattern: "^_" },
    ],
  },
};
