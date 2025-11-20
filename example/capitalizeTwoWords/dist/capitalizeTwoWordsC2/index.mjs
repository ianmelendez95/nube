import {eventHandler} from 'runtime';

export const handler = eventHandler(capitalizeTwoWordsC2);

function capitalizeTwoWordsC2(_ctx) {
  _ctx.frame.capitalizedWord2 = _ctx.args[0];
  return capitalizedWord1 + ' ' + capitalizedWord2;
}