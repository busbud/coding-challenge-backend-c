const instances = process.env.WEB_CONCURRENCY || 1;
const maxMemory = process.env.WEB_MEMORY || 512;

// Set --max_semi_space_size and --max_old_space_size Node values
// according to the max memory available
const maxSemiSpaceSize = 2;
const maxOldSpaceSize = 256;


const config = {
  apps: [ {
    name: 'challenge',
    script: 'app.js',

    // Options reference: https://pm2.io/doc/en/runtime/reference/ecosystem-file/
    autorestart: true,
    watch: false,
    instances: instances,
    max_memory_restart: `${ maxMemory }M`,
    exec_mode: 'cluster',
    node_args: [
      `--max_semi_space_size=${ maxSemiSpaceSize }`,
      `--max_old_space_size=${ maxOldSpaceSize }`,
    ],
    merge_logs: true,
  } ],
};

module.exports = config;
