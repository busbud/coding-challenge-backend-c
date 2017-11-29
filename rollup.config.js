import resolve from 'rollup-plugin-node-resolve';
import babel from 'rollup-plugin-babel';

export default {
  input: 'src/app.js',
  output: {
    file: 'app.js',
    format: 'cjs'
  },
  plugins: [
    resolve(),
    babel({
      exclude: 'node_modules/**',
      plugins: [
        '@babel/plugin-proposal-object-rest-spread',
        '@babel/transform-arrow-functions',
        '@babel/proposal-class-properties',
        '@babel/transform-regenerator',
        '@babel/transform-async-to-generator'
      ],
      babelrc: false
    })
  ]
};
