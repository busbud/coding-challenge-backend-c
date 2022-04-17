'use strict';

module.exports = {
  async up (queryInterface, Sequelize) {
    return queryInterface.createTable('Cities', {
      id: {
        type: Sequelize.INTEGER,
        autoIncrement: true,
        primaryKey: true,
        allowNull: false
      },
      geonameId: {
        type: Sequelize.INTEGER,
        allowNull: false
      },
      name: {
        type: Sequelize.STRING,
        allowNull: false
      },
      altNames: {
        type: Sequelize.JSON,
        allowNull: false
      },
      latitude: {
        type: Sequelize.FLOAT,
        allowNull: false
      },
      longitude: {
        type: Sequelize.FLOAT,
        allowNull: false
      },
      featClass: {
        type: Sequelize.STRING,
        allowNull: false
      },
      featCode: {
        type: Sequelize.STRING,
        allowNull: false
      },
      country: {
        type: Sequelize.STRING,
        allowNull: false
      },
      cc2: {
        type: Sequelize.JSON,
        allowNull: false
      },
      admin1: {
        type: Sequelize.STRING,
        allowNull: false
      },
      admin2: {
        type: Sequelize.STRING,
        allowNull: false
      },
      admin3: {
        type: Sequelize.STRING,
        allowNull: false
      },
      admin4: {
        type: Sequelize.STRING,
        allowNull: false
      },
      population: {
        type: Sequelize.INTEGER,
        allowNull: false
      },
      elevation: {
        type: Sequelize.INTEGER,
        allowNull: false
      },
      dem: {
        type: Sequelize.STRING,
        allowNull: false
      },
      tz: {
        type: Sequelize.STRING,
        allowNull: false
      },
      modifiedAt: {
        type: Sequelize.DATE,
        allowNull: true
      }
    });
  },

  async down (queryInterface, Sequelize) {
    return queryInterface.dropTable('Cities');
  }
};
