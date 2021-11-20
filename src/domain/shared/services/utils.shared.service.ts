import { Injectable } from '@nestjs/common';

@Injectable()
export class UtilsSharedService {
  currentIpAddress() {
    let address;
    const ifaces = require('os').networkInterfaces();
    for (const dev in ifaces) {
      ifaces[dev].filter(details =>
        details.family === 'IPv4' && details.internal === false
          ? (address = details.address)
          : undefined,
      );
    }
    return address === undefined ? 'undefined' : address;
  }
  current_date() {
    const data = new Date();
    const dia = data.getDate().toString();
    const diaF = dia.length == 1 ? '0' + dia : dia;
    const mes = (data.getMonth() + 1).toString();
    const mesF = mes.length == 1 ? '0' + mes : mes;
    const anoF = data.getFullYear();
    return (
      diaF +
      '/' +
      mesF +
      '/' +
      anoF +
      ' at ' +
      data.getHours() +
      ':' +
      data.getMinutes() +
      ':' +
      data.getSeconds()
    );
  }
  
  
}
