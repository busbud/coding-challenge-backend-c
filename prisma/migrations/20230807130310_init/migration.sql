/*
  Warnings:

  - Made the column `country` on table `City` required. This step will fail if there are existing NULL values in that column.
  - Made the column `population` on table `City` required. This step will fail if there are existing NULL values in that column.
  - Made the column `latitude` on table `City` required. This step will fail if there are existing NULL values in that column.
  - Made the column `longitude` on table `City` required. This step will fail if there are existing NULL values in that column.

*/
-- AlterTable
ALTER TABLE "City" ALTER COLUMN "country" SET NOT NULL,
ALTER COLUMN "population" SET NOT NULL,
ALTER COLUMN "latitude" SET NOT NULL,
ALTER COLUMN "longitude" SET NOT NULL;
