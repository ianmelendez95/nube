import {
  eventHandler
} from 'proxies'

export const handler = eventHandler(capitalizeWord);

async function capitalizeWord(_ctx)  {
    const word = _ctx.args[0];
    const result = word[0].toUpperCase() + word.slice(1);
    return _ctx.return(result);
}