const Sequelize = require('sequelize');

class GeoNameModel extends Sequelize.Model {
	static init(sequelize, DataTypes) {

		return super.init(
			{
				id: {
					type: DataTypes.BIGINT, primaryKey: true,
				},
				name: {
					type: Sequelize.STRING(200), allowNull: false,
				},
				asciiName: {
					type: Sequelize.STRING(20), allowNull: false, field: 'ascii_name',
				},
				country: {
					type: Sequelize.STRING(10), allowNull: false,
				},
				slug: {
					type: Sequelize.VIRTUAL,
					get() {
						return `${this.name}, ${this.country}`
					},
				},
			},
			{
				sequelize,
				freezeTableName: true,
				createdAt: false,
				updatedAt: false,
				tableName: 'geo_names',
				scopes: {
					find: {
						attributes: ['id', 'productCategoryId', 'productClassificationId', 'productTypeId', 'name',
							'code', 'images', 'previousPrice', 'finalPrice', 'promotional', 'itemsToKilo',
							'kiloStep', 'kiloDetailsMessage', 'pricePerUnit', 'finalPricePerUnit', 'unitStep',
							'unitName', 'unitDetails', 'unitDetailsMessage', 'orderToShow', 'active'],
					}
				}
			}
		);
	}
}

module.exports = GeoNameModel;

