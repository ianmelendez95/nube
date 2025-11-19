import {eventHandler} from 'runtime';

export const handler = eventHandler(capitalizeTwoWords_c2);

function capitalizeTwoWords_c2(_ctx) {
  _ctx.frame.capitalizedWord2 = _ctx.args[0];
  return capitalizedWord1 + ' ' + capitalizedWord2;
}