import {eventHandler} from 'runtime';

export const handler = eventHandler(capitalizeTwoWordsC1);

function capitalizeTwoWordsC1(_ctx) {
  _ctx.frame.capitalizedWord1 = _ctx.args[0];
  _ctx.call('capitalizeWord', [word2], 'capitalizeTwoWordsC2');
}