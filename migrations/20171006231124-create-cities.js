module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.createTable('cities', {
      id: { type: Sequelize.INTEGER, primaryKey: true },
      name: { type: Sequelize.STRING, allowNull: false },
      alternate_names: { type: Sequelize.ARRAY(Sequelize.STRING), defaultValue: [] },
      countryCode: { type: Sequelize.STRING, allowNull: false },
      adminCode: { type: Sequelize.STRING, allowNull: false },
      latitude: { type: Sequelize.FLOAT, allowNull: false },
      longitude: { type: Sequelize.FLOAT, allowNull: false },
      created_at: { type: Sequelize.DATE, allowNull: false },
      updated_at: { type: Sequelize.DATE, allowNull: false },
    });
  },
  down: async (queryInterface) => {
    await queryInterface.dropTable('cities');
  },
};
