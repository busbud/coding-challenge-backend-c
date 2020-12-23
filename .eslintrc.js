module.exports = {
  root: true,
  env: {
    es2020: true,
    node: true,
    jest: true,
    es6: true,
  },
  parser: "@typescript-eslint/parser",
  parserOptions: {
    ecmaVersion: 2018,
    sourceType: "module",
  },
  plugins: ["@typescript-eslint"],
  extends: [
    "eslint:recommended",
    "plugin:@typescript-eslint/eslint-recommended",
    "plugin:@typescript-eslint/recommended",
    "prettier",
  ],
  rules: {
    "@typescript-eslint/no-explicit-any": 1,
    "@typescript-eslint/no-var-requires": 1,
    "@typescript-eslint/ban-types": 1,
    "no-underscore-dangle": "off",
    "prettier/prettier": "error",
    "class-methods-use-this": "off",
    "@typescript-eslint/camelcase": "off",
    "@typescript-eslint/no-unused-vars": [
      "error",
      {
        argsIgnorePattern: "_",
      },
    ],
    "import/extensions": [
      "error",
      "ignorePackages",
      {
        ts: "never",
      },
    ],
    "no-underscore-dangle": "off",
    "prettier/prettier": "error",
    "class-methods-use-this": "off",
    "@typescript-eslint/camelcase": "off",
    "no-var": "error",
    semi: "error",
    indent: "error",
    "no-multi-spaces": "error",
    "space-in-parens": "error",
    "no-multiple-empty-lines": "error",
    "prefer-const": "error",
    "no-use-before-define": "error",
  },
  settings: {
    "import/resolver": {
      node: {
        extensions: [".js", ".jsx", ".ts", ".tsx"],
      },
    },
  },
};
