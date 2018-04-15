/**
 * PM2 Configurations
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

// Third-part modules
var fs     = require('fs'),
    is     = require('is_js');

// Core modules
var di     = require('./di.js'),
    config = require('./config.js');

// Dependency Injection
di.set('fs', fs);
di.set('is', is);

// Define the DI as a global object
global.di = di;

// Load and cache the configurations
config.sycnLoad();

module.exports = {
  apps: [
    {
      name: 'BusBud',
      script: 'app.js',
      exec_mode: 'cluster',
      instances: config.getServer().instances,
      watch: true,
      node_args: '--no-warnings',
      max_memory_restart: '1500M',
      ignore_watch: ['node_modules', '.git', 'tmp*'],
      watch_options: {usePolling: true},
      env: {
        NODE_ENV: 'development'
      },
      env_production : {
        NODE_ENV: 'production'
      }
    }
  ]
};
