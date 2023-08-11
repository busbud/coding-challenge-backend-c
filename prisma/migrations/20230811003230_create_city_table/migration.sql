-- CreateTable
CREATE TABLE "City" (
    "id" INTEGER NOT NULL,
    "name" VARCHAR(200) NOT NULL,
    "asciiName" VARCHAR(200) NOT NULL,
    "alternateNames" TEXT[],
    "latitude" DOUBLE PRECISION NOT NULL,
    "longitude" DOUBLE PRECISION NOT NULL,
    "countryCode" VARCHAR(2) NOT NULL,
    "stateCode" VARCHAR(2) NOT NULL,
    "population" INTEGER NOT NULL,

    CONSTRAINT "City_pkey" PRIMARY KEY ("id")
);
