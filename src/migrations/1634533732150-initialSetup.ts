import {MigrationInterface, QueryRunner} from "typeorm";

export class initialSetup1634533732150 implements MigrationInterface {
    name = 'initialSetup1634533732150'

    public async up(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`CREATE TABLE "countries" ("isoCode" character varying(2) NOT NULL, "countryName" character varying(200) NOT NULL, CONSTRAINT "PK_dfcc02f3af5189a35e56e3363db" PRIMARY KEY ("isoCode"))`);
        await queryRunner.query(`CREATE TABLE "provinces" ("code" character varying(5) NOT NULL, "provinceCode" character varying(2) NOT NULL, "provinceName" character varying(200) NOT NULL, CONSTRAINT "PK_f4b684af62d5cb3aa174f6b9b8a" PRIMARY KEY ("code"))`);
        await queryRunner.query(`CREATE TABLE "cities" ("id" SERIAL NOT NULL, "geonameId" integer NOT NULL, "name" character varying(200) NOT NULL, "asciiName" character varying(200) NOT NULL, "alternateNames" character varying(5000) NOT NULL, "latitude" character varying NOT NULL, "longitude" character varying NOT NULL, "featureClass" character varying(1) NOT NULL, "featureCode" character varying(10) NOT NULL, "cc2" character varying(60) NOT NULL, "admin2Code" character varying(80) NOT NULL, "admin3Code" character varying(20) NOT NULL, "admin4Code" character varying(20) NOT NULL, "population" integer NOT NULL, "elevation" integer NOT NULL, "dem" character varying NOT NULL, "timezone" character varying(40) NOT NULL, "modificationDate" TIMESTAMP NOT NULL, "countryIsoCode" character varying(2), "provinceCode" character varying(5), CONSTRAINT "PK_4762ffb6e5d198cfec5606bc11e" PRIMARY KEY ("id"))`);
        await queryRunner.query(`ALTER TABLE "cities" ADD CONSTRAINT "FK_9428a0a97d881c2c1bffed784ec" FOREIGN KEY ("countryIsoCode") REFERENCES "countries"("isoCode") ON DELETE NO ACTION ON UPDATE NO ACTION`);
        await queryRunner.query(`ALTER TABLE "cities" ADD CONSTRAINT "FK_790780e390030daf0167f3f82b0" FOREIGN KEY ("provinceCode") REFERENCES "provinces"("code") ON DELETE NO ACTION ON UPDATE NO ACTION`);
    }

    public async down(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`ALTER TABLE "cities" DROP CONSTRAINT "FK_790780e390030daf0167f3f82b0"`);
        await queryRunner.query(`ALTER TABLE "cities" DROP CONSTRAINT "FK_9428a0a97d881c2c1bffed784ec"`);
        await queryRunner.query(`DROP TABLE "cities"`);
        await queryRunner.query(`DROP TABLE "provinces"`);
        await queryRunner.query(`DROP TABLE "countries"`);
    }

}
