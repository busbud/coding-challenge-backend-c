import { Knex } from 'knex';

export const up = async (db: Knex) => {
  await db.schema.createTable('country', (table) => {
    table.integer('id').unsigned().notNullable().unique();
    table.string('iso', 2).notNullable();
    table.string('country', 200).notNullable();
  });
};

export const down = async (db: Knex) => {
  await db.schema.dropTable('country');
};
