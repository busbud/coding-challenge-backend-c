module.exports = {
  up: async (queryInterface, Sequelize) => {
    // Create new columns, allow NULL
    await queryInterface.addColumn('cities', 'country_code', {
      type: Sequelize.STRING,
      allowNull: true,
    });
    await queryInterface.addColumn('cities', 'admin_code', {
      type: Sequelize.STRING,
      allowNull: true,
    });

    // Copy data
    await queryInterface.sequelize.query('UPDATE cities SET country_code = cities."countryCode", admin_code = cities."adminCode"');

    // Remove allow NULL
    await queryInterface.changeColumn('cities', 'country_code', {
      type: Sequelize.STRING,
      allowNull: false,
    });
    await queryInterface.changeColumn('cities', 'admin_code', {
      type: Sequelize.STRING,
      allowNull: false,
    });
  },

  down: async (queryInterface) => {
    await queryInterface.removeColumn('cities', 'country_code');
    await queryInterface.removeColumn('cities', 'admin_code');
  },
};
