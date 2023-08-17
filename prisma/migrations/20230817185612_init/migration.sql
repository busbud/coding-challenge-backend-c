-- CreateTable
CREATE TABLE "Location" (
    "id" TEXT NOT NULL,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,
    "name" VARCHAR(255) NOT NULL,
    "ascii" VARCHAR(255) NOT NULL,
    "latitude" DOUBLE PRECISION NOT NULL,
    "longitude" DOUBLE PRECISION NOT NULL,
    "country" VARCHAR(2) NOT NULL,
    "state" VARCHAR(2) NOT NULL,
    "population" INTEGER NOT NULL,

    CONSTRAINT "Location_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE INDEX "Location_name_idx" ON "Location" USING GIN ("name" gin_trgm_ops);

-- CreateIndex
CREATE INDEX "Location_ascii_idx" ON "Location" USING GIN ("ascii" gin_trgm_ops);
