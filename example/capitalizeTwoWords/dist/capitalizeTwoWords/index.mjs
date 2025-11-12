import {
  eventHandler,
} from 'proxies'

export const handler = eventHandler(capitalizeTwoWords);

async function capitalizeTwoWords(_ctx)  {
  console.log("capitalizeTwoWords context:", _ctx);
  const string = _ctx.args[0];
  _ctx.frame.words = string.split(' ');
  _ctx.frame.word1 = _ctx.frame.words[0];
  _ctx.frame.word2 = _ctx.frame.words[1];
  _ctx.callCC('capitalizeWord', [_ctx.frame.word1],'capitalizeTwoWords1');
}