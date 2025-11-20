import {eventHandler} from 'runtime';

export const handler = eventHandler(capitalizeTwoWords);

function capitalizeTwoWords(_ctx) {
  _ctx.frame.string = _ctx.args[0];
  const words = string.split(' ');
  const word1 = words[0];
  const word2 = words[1];
  _ctx.call('capitalizeWord', [word1], 'capitalizeTwoWordsC1');
}