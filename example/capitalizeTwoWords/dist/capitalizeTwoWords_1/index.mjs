import {
  eventHandler
} from 'proxies'

export const handler = eventHandler(capitalizeTwoWords_1);

async function capitalizeTwoWords_1(_ctx)  {
  _ctx.frame.capWord1 = _ctx.args[0];
  _ctx.callCC('capitalizeWord', [_ctx.frame.word2], 'capitalizeTwoWords_2');
}