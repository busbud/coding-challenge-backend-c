module.exports = (sequelize, DataTypes) =>
  sequelize.define(
    'City',
    {
      id: { type: DataTypes.INTEGER, primaryKey: true },
      name: { type: DataTypes.STRING, allowNull: true },
      alternateNames: {
        type: DataTypes.ARRAY(DataTypes.STRING),
        field: 'alternate_names',
      },
      countryCode: {
        type: DataTypes.STRING,
        field: 'country_code',
      },
      adminCode: {
        type: DataTypes.STRING,
        field: 'admin_code',
      },
      latitude: { type: DataTypes.FLOAT, allowNull: true },
      longitude: { type: DataTypes.FLOAT, allowNull: true },
    },
    {
      timestamps: true,
      underscored: true,
      tableName: 'cities',
    },
  );
