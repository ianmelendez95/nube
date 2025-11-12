import {
  eventHandler
} from 'proxies'

export const handler = eventHandler(capitalizeTwoWords1);

async function capitalizeTwoWords1(_ctx)  {
  _ctx.frame.capWord1 = _ctx.args[0];
  _ctx.callCC('capitalizeWord', [_ctx.frame.word2], 'capitalizeTwoWords2');
}