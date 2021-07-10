"use strict";

const _ = require('lodash');

const allAlleles = [
    // Base Pigments
    { symbol: 'b', description: 'Brown', dominance: -100, blob: '-brown', trait: 'pigment' },
    { symbol: 'B(+)', description: 'Blue/Black', dominance: 0, blob: '-blueblack', trait: 'pigment' },
    { symbol: 'B(A)', description: 'Ash Red', dominance: 100, blob: '-ashred', trait: 'pigment' },
 
    // Wing patterns
    { symbol: 'c', description: 'Barless', dominance: -100, blob: null, trait: 'pattern' },
    { symbol: 'C(+)', description: 'Bar', dominance: 0, blob: '-bar', trait: 'pattern' },
    { symbol: 'C(L)', description: 'Light Checker', dominance: 100, blob: '-ltchecker', trait: 'pattern' },
    { symbol:'C(D)', description:'Dark Checker', dominance:200, blob: '-dkchecker', trait: 'pattern' },
    { symbol:'C(T)', description:'T-Pattern', dominance:300, blob: '-tpattern', trait: 'pattern' },
        
    // Spreads the color of the tail's bar to the whole body
    { symbol:'s(+)', description:'Wild Type', dominance:0, blob:null, trait: 'spread' },
    { symbol:'S', description:'Spread', dominance:100, blob: '-spread', trait: 'spread' },
        
    // Messes with the coloring of the feathers  
    { symbol:'z(wh)', description:'Recessive White', dominance:-400, blob: '-recwhite', trait: 'gazzi' },     // White
    { symbol:'z(tm)', description:'Tail Mark', dominance:-300, blob: '-tmark', trait: 'gazzi' },              // Colors Tail only
    { symbol:'z(pc)', description:'Penciled', dominance:-200, blob: '-penciled', trait: 'gazzi' },            // Colors Head and tail only
    { symbol:'z', description:'Gazzi', dominance:-100, blob: '-gazzi', trait: 'gazzi' },                      // Colors Head, wing shields and tail only
    { symbol:'Z(+)', description:'Wild Type', dominance:0, blob:null, trait: 'gazzi' },
        
    // Dilutes the base pigment
    { symbol:'d', description:'Diluted', dominance:-200, blob: '-diluted', trait: 'dilution' },               // Cuts pigment by 50%
    { symbol:'d(P)', description:'Pale', dominance:-100, blob: '-pale', trait: 'dilution' },                  // Cuts pigment by 25%
    { symbol:'D(+)', description:'Wild Type', dominance:0, blob:null, trait: 'dilution' },
    
    // Messes with the individual coloring of the feathers
    { symbol:'g(+)', description:'Wild Type', dominance:0, blob:null, trait: 'grizzle' },
    { symbol:'G', description:'Grizzle', dominance:100, blob: '-grizzle', trait: 'grizzle' },
    { symbol:'G(T)', description:'Tiger Grizzle', dominance:200, blob: '-tgrizzle', trait: 'grizzle' },
    
    // Creates white splashes in certain areas
    { symbol:'pi(Bh)', description:'Piebald', dominance:-100, blob: '-piebald', trait: 'pied' },
    { symbol:'pi(Cr)', description:'Crescent', dominance:-100, blob: '-crescent', trait: 'pied' },
    { symbol:'pi(Wf)', description:'White Flights', dominance:-100, blob: '-whiteflights', trait: 'pied' },
    { symbol:'pi(Wt)', description:'White Tail', dominance:-100, blob: '-whitetail', trait: 'pied' },
    { symbol:'pi(+)', description:'Wild Type', dominance:0, blob:null },

    // Makes the bird Brick Red colored
    { symbol: 'e', description: 'Recessive Red', dominance: -100, blob: '-rred', trait: 'rred' },
    { symbol: 'E(+)', description: 'WildType', dominance: 0, blob: null, trait: 'rred' },

    // Others
    { symbol:'SIZE', description:'Size', dominance:0, blob: 0, trait: 'size' },
    { symbol:'LNGV', description:'Longevity', dominance:0, blob: 0, trait: 'longevity' },
    { symbol:'LOYT', description:'Loyalty', dominance:0, blob: 0, trait: 'loyalty' },
    { symbol:'CHRM', description:'Charisma', dominance:0, blob: 0, trait: 'charisma' },
];


const traitHandlers = {
    pigment: codominanceHandler,
    pattern: codominanceHandler, 
    spread: codominanceHandler, 
    grizzle: codominanceHandler, 
    dilution: codominanceHandler, 
    pied: codominanceHandler, 
    gazzi: codominanceHandler,
    rred:  codominanceHandler,
    size: averagingHandler,
    longevity: averagingHandler,
    loyalty: averagingHandler,
    charisma: averagingHandler,
};

function computePhenotype(genotype) {
    const traits = genotype.reduce((acc, grp) => {
        return acc.concat(computeTraits(grp));
    }, []);

    // Take care of special Homozygous expression
    return traits.map(trait => {
        if (trait.inheritance === 'homozygous') {
            switch (trait.data) {
                case '-tgrizzle':
                case '-grizzle':
                    trait.data = '-hgrizzle'
                    break;
                case `-whiteflights`:
                    trait.data = '-hwhiteflights'
                    break;
            }
        }
        return trait;
    });
}

function computeTraits(grp) {
    const alleles = alleleGenes(grp);
    return alleles.flatMap(symbols => {
        const genes = symbols.map(findGene);
        if (genes && genes.length) {
            const handler = traitHandlers[genes[0].trait];
            if (handler)
                return handler(genes);
        }
        return [];
    });
}

function averagingHandler(genes) {
    const inheritance = determineInheritance(genes);
    const trait = genes.reduce((acc, gene) => {
        if (!acc) {
            acc = {
                id: gene.trait,
                inheritance: inheritance,
                data: 0.0
            };
        }
        acc.data += gene.blob;
        return acc;
    }, null ); 
    trait.data /= genes.length;
    return [trait];
}

function codominanceHandler(genes) {
    const inheritance = determineInheritance(genes);
    let dominance = -999999999;
    return genes.reduce((acc, gene) => {
        // dominant
        if (gene.dominance > dominance) {
            dominance = gene.dominance;
            acc = [{
                id: gene.trait,
                inheritance: inheritance, 
                data: gene.blob
            }];
        }
        // co-dominance
        else if (gene.dominance === dominance && acc.find(t => t.data !== gene.blob)) {
            acc.push({
                id: gene.trait,
                inheritance: inheritance, 
                data: gene.blob
            });
        }
        return acc;
    }, []);
}

function alleleGenes(chromosomes) {
    return chromosomes.reduce((acc, chomosome) => {
        if (acc.length) {
            chomosome.forEach((gene, index) => {
                if (index<acc.length)
                    acc[index].push(gene);
                else
                    acc.push([gene]);
            });
        }
        else
            acc = chomosome.map(gene => [gene]);
        return acc;
    }, []);
}

function determineInheritance(genes) {
    if (genes.length === 1)
        return 'hemizygous';
    if (genes.every(e => e === genes[0]))
        return 'homozygous';
    return 'heterozygous';
}

function findGene(symbol) {
    let gene = allAlleles.find(g => g.symbol === symbol);
    if (!gene && symbol.length > 3) {
        gene = allAlleles.find(g => g.symbol === symbol.substr(0,4));
        if (gene) {
            gene = _.cloneDeep(gene);
            gene.blob = parseFloat(symbol.substr(4));
        }
    }
    return gene;
}

module.exports = {
    computePhenotype
};
