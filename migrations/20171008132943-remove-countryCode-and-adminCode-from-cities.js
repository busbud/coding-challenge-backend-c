module.exports = {
  up: async (queryInterface) => {
    await queryInterface.removeColumn('cities', 'countryCode');
    await queryInterface.removeColumn('cities', 'adminCode');
  },

  down: async (queryInterface, Sequelize) => {
    // Create new columns, allow NULL
    await queryInterface.addColumn('cities', 'countryCode', {
      type: Sequelize.STRING,
      allowNull: true,
    });
    await queryInterface.addColumn('cities', 'adminCode', {
      type: Sequelize.STRING,
      allowNull: true,
    });

    // Copy data
    await queryInterface.sequelize.query('UPDATE cities SET "countryCode" = cities."country_code", "adminCode" = cities."admin_code"');

    // // Remove allow NULL
    await queryInterface.changeColumn('cities', 'countryCode', {
      type: Sequelize.STRING,
      allowNull: false,
    });
    await queryInterface.changeColumn('cities', 'adminCode', {
      type: Sequelize.STRING,
      allowNull: false,
    });
  },
};
