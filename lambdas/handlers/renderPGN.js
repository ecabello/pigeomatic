"use strict";

const {renderEgg, renderSqueaker, renderPigeon } = require('../lib/pigeonRendering');

// 18 days incubation time
const INCUBATION_TIME = 18 * 24 * 60 * 60 * 1000;

// Squeaker until 30 days
const SQUAKER_TIME = 30 * 24 * 60 * 60 * 1000;

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
    if (pigeon && pigeon.id && pigeon.birthday && pigeon.genotype) {
        console.log('Rendering pigeon ', pigeon.id)

        let buffer;
        const now  = new Date();
        const birthday = new Date(pigeon.birthday);
        if (now > birthday) {
            // check if still a squeaker
            const squeaker = now.getTime() < (birthday.getTime() + SQUAKER_TIME);
            if (squeaker)
                buffer = await renderSqueaker(pigeon.genotype, size)
            else
                buffer = await renderPigeon(pigeon.genotype, size);
        }
        else { 
            const egg = now.getTime() > (birthday.getTime() - INCUBATION_TIME)  
            if (egg) {
                buffer = await renderEgg(size)                    
            }
            else {
                return  {
                    statusCode: 400,
                    body:  JSON.stringify({
                        message: 'Error trying to render a pigeon from the future'
                    }) 
                }
            }
        }

        if (buffer) {
            console.log('Succesfully rendered pigeon ', pigeon.id, '. Returning ', buffer.length, ' bytes...')
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
