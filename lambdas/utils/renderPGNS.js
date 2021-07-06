"use strict";

const fs = require('fs');
const path = require('path');
const { renderPigeon } = require('../lib/pigeonRendering');
const pigeons = require(path.join(__dirname,'/pigeons.json'));

console.log('Loading pigeons...');
let ps = pigeons.map((pigeon) => {
    return renderPigeon(pigeon.genotype, 250);
});
console.log(`loaded ${pigeons.length} pigeons.`);

const imagesDir = path.join(__dirname, '/images');
if (!fs.existsSync(imagesDir)) {
    console.log(`Creating ${imagesDir}...`);
    fs.mkdirSync(imagesDir);
}

Promise.all(ps).then((bs) => {
    console.log(`Saving images to ${imagesDir}...`);
    let i = 0;
    bs.forEach((buffer) => {
        console.log(`Saving ${pigeons[i].id}.png`);
        fs.writeFileSync(`${imagesDir}/${pigeons[i].id}.png`, buffer)
        i++;
    });
    console.log('done!')
});
