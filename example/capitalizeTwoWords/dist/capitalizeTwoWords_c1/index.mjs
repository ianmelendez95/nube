import {eventHandler} from 'runtime';

export const handler = eventHandler(capitalizeTwoWords_c1);

function capitalizeTwoWords_c1(_ctx) {
  _ctx.frame.capitalizedWord1 = _ctx.args[0];
  _ctx.call('capitalizeWord', [word2], 'capitalizeTwoWords_c2');
}