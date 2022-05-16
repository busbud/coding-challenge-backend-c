import { Knex } from 'knex';

export const up = async (db: Knex) => {
  await db.schema.createTable('city', (table) => {
    table.integer('id').unsigned().notNullable().unique();
    table.string('ascii', 200).notNullable().index();
    table.string('name', 200).notNullable();
    table.string('country', 2).notNullable().index();
    table.string('admin1', 2).notNullable().index();
    table.decimal('lat', 8, 6).notNullable();
    table.decimal('long', 9, 6).notNullable();
    table.integer('population').unsigned().notNullable();
  });
};

export const down = async (db: Knex) => {
  await db.schema.dropTable('city');
};
