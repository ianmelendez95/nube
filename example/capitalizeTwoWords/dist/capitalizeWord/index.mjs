import {eventHandler} from 'runtime';

export const handler = eventHandler(capitalizeWord);

function capitalizeWord(_ctx) {
  _ctx.frame.word = _ctx.args[0];
  return word[0].toUpperCase() + word.slice(1);
}