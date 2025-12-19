import {eventHandler} from 'runtime';

export const handler = eventHandler(capitalizeWord);

function capitalizeWord(_ctx) {
  _ctx.frame.word = _ctx.args[0];
  switch (_ctx.state) {
    case 0:
      return _ctx.return(_ctx.frame.word[0].toUpperCase() + _ctx.frame.word.slice(1));
  }
}