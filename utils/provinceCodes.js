/*
    {
      "suggestions": 
        [
          {
            "name": "London, ON, Canada",
            "latitude": "42.98339",
            "longitude": "-81.23304",
            "score": 0.9
          },
          {
            "name": "London, OH, USA",
            "latitude": "39.88645",
            "longitude": "-83.44825",
            "score": 0.5
          },
          {
            "name": "London, KY, USA",
            "latitude": "37.12898",
            "longitude": "-84.08326",
            "score": 0.5
          },
          {
            "name": "Londontowne, MD, USA",
            "latitude": "38.93345",
            "longitude": "-76.54941",
            "score": 0.3
          }
        ]
    }

    In this example there is province code in the name but there was nothing like that in London-Canada data.
    After a while I realized that admin1 is used for State information for US Cities.
    However that wasn't the case for Canada Cities. admin1 was a number field.
    So I just looked it up at https://download.geonames.org/export/dump/ and found the province code equivalents of those numbers.
*/

const getProvinceCode = (province) => {
  const provinces = [
    { id: 1, code: "AB" },
    { id: 2, code: "BC" },
    { id: 3, code: "MB" },
    { id: 4, code: "NB" },
    { id: 5, code: "NL" },
    { id: 7, code: "NS" },
    { id: 8, code: "ON" },
    { id: 9, code: "PE" },
    { id: 10, code: "QC" },
    { id: 11, code: "SK" },
    { id: 12, code: "YT" },
    { id: 13, code: "NT" },
  ];
  const filteredProvinces = provinces.filter((item) => item.id === province);

  return filteredProvinces.length > 0 ? filteredProvinces[0].code : province;
};

module.exports = getProvinceCode;
