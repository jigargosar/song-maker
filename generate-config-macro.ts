import {execSync} from 'child_process';

export async function generateElm() {
    execSync(
        'node ./experiments/json-to-elm-generator/transpiler-script.js ' +
        '--module src/Configuration/PlayerConfig.elm ' +
        '--input ./experiments/json-to-elm-generator/config.json ' +
        '--alias Config ' +
        '--identifier-name cfg',
        { stdio: 'inherit' }
    );
}
