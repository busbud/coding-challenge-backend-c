'use strict';

module.exports = {
  async up (queryInterface, Sequelize) {
    return queryInterface.sequelize.query('CREATE EXTENSION unaccent');
  },

  async down (queryInterface, Sequelize) {
    return queryInterface.sequelize.query('DROP EXTENSION unaccent');
  }
};
