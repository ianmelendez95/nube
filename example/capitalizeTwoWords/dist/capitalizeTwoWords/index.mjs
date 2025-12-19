import {eventHandler} from 'runtime';

export const handler = eventHandler(capitalizeTwoWords);

function capitalizeTwoWords(_ctx) {
  _ctx.frame.string = _ctx.args[0];
  switch (_ctx.state) {
    case 1:
      _ctx.frame.words = _ctx.frame.string.split(' ');
      _ctx.frame.word1 = _ctx.frame.words[0];
      _ctx.frame.word2 = _ctx.frame.words[1];
      return _ctx.callCC('capitalizeWord', [ _ctx.frame.word1 ], 'capitalizeTwoWords', 1);
    case 2:
      _ctx.frame.capitalizedWord1 = _ctx.args[0];
      return _ctx.callCC('capitalizeWord', [ _ctx.frame.word2 ], 'capitalizeTwoWords', 2);
    case 3:
      _ctx.frame.capitalizedWord2 = _ctx.args[0];
      return _ctx.return(_ctx.frame.capitalizedWord1 + ' ' + _ctx.frame.capitalizedWord2);
  }
}