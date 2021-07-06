"use strict";

const { renderPigeon } = require('../lib/pigeonRendering');

module.exports.handler = async (event) => {
    if (!event.body) {
        console.error('No request body specified');
        return {
            statusCode: 400,
            body: JSON.stringify({
                message: 'No request body specified'
            })
        };
    }

    // Size between 250px and 400px
    const size = Math.min(
        400, 
        Math.max(
            event.pathParameters ? event.pathParameters.size : 250, 
            250
        )
    );

    const pigeon = JSON.parse(event.body);
    if (pigeon && pigeon.genotype) {
        console.log('Rendering pigeon. Genotype: ', pigeon.genotype)

        const buffer = await renderPigeon(pigeon.genotype, size);
        if (buffer) {
            console.log('Succesfully rendered pigeon. Returning ', buffer.length, ' bytes...')
            return {
                statusCode: 200,
                headers: {'Content-Type': 'image/png'},
                body: buffer.toString('base64'),
                isBase64Encoded: true
            };
        }
        else {
            console.error('Error rendering pigeon');
            return {
                statusCode: 400,
                body:  JSON.stringify({
                    message: 'Error rendering pigeon'
                })
            };
        }
    }

    console.error('No valid pigeon specified');
    return {
        statusCode: 400,
        body: JSON.stringify({
            message: 'No valid pigeon specified'
        })
    };
};