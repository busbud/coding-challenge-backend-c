const tsConfig = require('./tsconfig.json');
const tsConfigPaths = require('tsconfig-paths');

const baseUrl = tsConfig.compilerOptions.outDir;

tsConfigPaths.register({
    baseUrl,
    paths: tsConfig.compilerOptions.paths,
});
