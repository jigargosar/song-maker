#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

// --- Helpers ---
function toElmLiteral(value, indent = '    ') {
    if (Array.isArray(value)) {
        return `[ ${value.map(v => toElmLiteral(v, indent)).join(', ')} ]`;
    } else if (typeof value === 'object' && value !== null) {
        const entries = Object.entries(value);
        const fields = entries
            .map(([k, v], i) => {
                const prefix = i === 0 ? '' : ',';
                return `${prefix} ${k} = ${toElmLiteral(v, indent + '    ')}`;
            })
            .join('\n' + indent);
        return `{\n${indent}${fields}\n${indent.slice(4)}}`;
    } else if (typeof value === 'string') {
        return `"${value}"`;
    } else if (typeof value === 'boolean') {
        return value ? 'True' : 'False';
    } else {
        return value;
    }
}

function toElmType(value, indent = '    ') {
    if (Array.isArray(value)) {
        const first = value[0];
        return `List ${toElmType(first, indent)}`;
    } else if (typeof value === 'object' && value !== null) {
        const entries = Object.entries(value);
        const fields = entries
            .map(([k, v], i) => {
                const prefix = i === 0 ? '' : ',';
                return `${prefix} ${k} : ${toElmType(v, indent + '    ')}`;
            })
            .join('\n' + indent);
        return `{\n${indent}${fields}\n${indent.slice(4)}}`;
    } else if (typeof value === 'string') {
        return 'String';
    } else if (typeof value === 'boolean') {
        return 'Bool';
    } else if (typeof value === 'number') {
        return Number.isInteger(value) ? 'Int' : 'Float';
    } else {
        return 'Unknown';
    }
}

function generateElmModule(modulePath, aliasName, identifierName, data) {
    const moduleName = path
        .relative('src', modulePath)
        .replace(/\.elm$/, '')
        .split(path.sep)
        .map(segment => segment.charAt(0).toUpperCase() + segment.slice(1))
        .join('.');

    const typeDef = `type alias ${aliasName} =\n    ${toElmType(data, '        ')}`;
    const valueDef = `${identifierName} : ${aliasName}\n${identifierName} =\n    ${toElmLiteral(data, '        ')}`;
    return `module ${moduleName} exposing (${identifierName}, ${aliasName})\n\n${typeDef}\n\n${valueDef}\n`;
}

// --- CLI ---
const args = process.argv.slice(2);
const options = {};

for (let i = 0; i < args.length; i += 2) {
    const key = args[i];
    const val = args[i + 1];
    if (key === '--module') options.module = val;
    if (key === '--input') options.input = val;
    if (key === '--alias') options.alias = val;
    if (key === '--identifier-name') options.identifier = val;
}

if (!options.module || !options.input) {
    console.error('Usage: node elmify-config.js --module <output.elm> --input <config.js|json> [--alias <TypeAlias>] [--identifier-name <valueName>]');
    process.exit(1);
}

try {
    const inputPath = path.resolve(options.input);
    const ext = path.extname(inputPath);
    let config;

    if (ext === '.json') {
        config = JSON.parse(fs.readFileSync(inputPath, 'utf8'));
    } else if (ext === '.js') {
        config = require(inputPath);
    } else {
        throw new Error('Unsupported file type. Use .json or .js');
    }

    const aliasName = options.alias || 'Config';
    const identifierName = options.identifier || aliasName.toLowerCase();
    const elmCode = generateElmModule(options.module, aliasName, identifierName, config);

    fs.mkdirSync(path.dirname(options.module), { recursive: true });
    fs.writeFileSync(options.module, elmCode, 'utf8');
    console.log(`✅ Elm config written to ${options.module}`);
    // After writing the file
    try {
        execSync(`elm-format ${options.module} --yes`, { stdio: 'inherit' });
        console.log(`✨ elm-format applied to ${options.module}`);
    } catch (err) {
        console.warn(`⚠️ elm-format failed: ${err.message}`);
    }
} catch (err) {
    console.error('❌ Error:', err.message);
    process.exit(1);
}
