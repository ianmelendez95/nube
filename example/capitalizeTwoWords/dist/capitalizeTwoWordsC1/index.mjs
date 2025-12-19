import {eventHandler} from 'runtime';

export const handler = eventHandler(capitalizeTwoWordsC1);

function capitalizeTwoWordsC1(_ctx) {
  _ctx.frame.capitalizedWord1 = _ctx.args[0];
  return _ctx.callCC('capitalizeWord', [ _ctx.frame.word2 ], 'capitalizeTwoWords', 2);
}