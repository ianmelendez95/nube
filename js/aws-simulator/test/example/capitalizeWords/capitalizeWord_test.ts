import {handler} from './capitalizeWord/index.mjs';

Deno.test("capitalizeWord invoke directly", () => {
    handler({
        Records: [
            {
                body: JSON.stringify(["hello"])
            },
            {
                body: JSON.stringify(["world"])
            },
        ]
    });
});