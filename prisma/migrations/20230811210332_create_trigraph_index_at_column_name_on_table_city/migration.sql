-- CreateIndex
CREATE INDEX "City_name_idx" ON "City" USING GIN ("name" gin_trgm_ops);
