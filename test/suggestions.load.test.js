import http from 'k6/http';

import { check, sleep } from 'k6';


// Simple load test for the suggestions endpoint
export const options = {
    stages: [
        { duration: '30s', target: 50 },
        { duration: '1m30s', target: 60 },
        { duration: '20s', target: 50 },
    ],
};


export default function () {
    const res = http.get('https://suggestionapp.herokuapp.com?q=Lon');
    check(res, { 'status was 200': (r) => r.status == 200 });
    sleep(1);
}