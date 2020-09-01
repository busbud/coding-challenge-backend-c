import { assert } from "chai"
import { normalizeText } from "../../src/services/textUtils";

describe('Text normalized', () => {

    it('remove characters not allowed to index the name', () => {
        assert.equal(normalizeText("Jóáé' - | []]}{} , -><=23 ''\"í."), "joae23i");
    });

});