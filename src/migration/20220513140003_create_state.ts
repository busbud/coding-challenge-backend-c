import { Knex } from 'knex';

export const up = async (db: Knex) => {
  await db.schema.createTable('state', (table) => {
    table.integer('id').unsigned().notNullable().unique();
    table.string('country', 2).notNullable();
    table.string('admin1', 100).notNullable();
    table.string('name', 200).notNullable();
  });
};

export const down = async (db: Knex) => {
  await db.schema.dropTable('state');
};
