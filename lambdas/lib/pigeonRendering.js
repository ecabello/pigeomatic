"use strict";

const { computePhenotype } = require('../lib/pigeonDNA');
const { createCanvas, loadImage } = require('canvas');

function renderEgg(size) {
    console.log('Rendering egg.')
    return renderLayers(['egg'], size, 1.0);
}

function renderSqueaker(genotype, size) {
    console.log('Rendering squeaker.')
    let traits = computePhenotype(genotype);
    return renderLayers(['squeaker'], size, 1.0, traits.find(t => t.inheritance == 'hemizygous'));
}

function renderPigeon(genotype, size) {
    let traits = computePhenotype(genotype);
    const traitRenderOrder = ['size', 'pigment', 'pattern', 'spread', 'rred', 'grizzle', 'dilution', 'pied', 'gazzi'];
    traits = traits.filter(t => traitRenderOrder.includes(t.id));
    traits.sort((t1, t2) => traitRenderOrder.indexOf(t1.id) - traitRenderOrder.indexOf(t2.id));

    console.log('Rendering traits: ', traits);

    let hen = false, pigment = null, scale = 1.0;
    const layers = traits.reduce((acc, trait) => {
        if (!hen)
            hen = trait.inheritance === 'hemizygous';
        let data = trait.data;
        if (data) {
            if (trait.id == 'size') {
                scale = 1/100 * data;
            }
            else {
                if (trait.id ==='pigment')
                    pigment = data;	

                var layer = layerFromTrait(trait.id, pigment, data);
                if (layer)
                    acc.push(layer);
            } 
        }
        return acc;
    }, []);

    const common = ['common', 'lineart'];
    return renderLayers(layers.concat(common), size, scale, hen);
}

function renderLayers(layers, size, scale, mirror) {
    const width = size;
    const height = size;

    const canvas = createCanvas(width, height);
    const context = canvas.getContext('2d');
    
    //context.fillStyle = '#fff';
    //context.fillRect(0, 0, width, height);
    context.clearRect(0,0, width, height);

    return Promise.all( 
        layers.map(e => loadImage(`./assets/${e}.png`))
    ).then( images => {
        if (mirror) {
            console.info('Rendering mirrored.');
            context.translate(width, 0);
            context.scale(-scale, scale);
        }
        else {
            context.scale(scale, scale);
        }
        images.forEach(img => context.drawImage(img, 0, 0, width, height));
        return canvas.toBuffer('image/png');
    });
}

function layerFromTrait(id, pigment, data) {
    switch (id) {
        case 'pattern':
            return `${id}${pigment}${data}`;
        case 'spread':
            return `${id}${pigment}`;
    }
    return `${id}${data}`;
}

module.exports = {
    renderEgg,
    renderSqueaker,
    renderPigeon
};
