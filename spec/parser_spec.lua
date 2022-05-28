local lexer = require('luacomplete.lexer')
local tokenize = lexer.tokenize
local parse = lexer.parse2
local T = lexer.TOKEN_TYPE

local eq = assert.are.same

describe('luacomplete.parser', function()
  it('does something', function()
    local function p(s)
      local es = parse(tokenize(s))
      for _, ts in ipairs(es) do
        for _, t in ipairs(ts) do
          t.col, t.idx, t.line = nil, nil, nil
        end
      end
      return es
    end

    eq(p('foo.'), {
      { type='root',
        { type=T.IDENT, value='foo' },
      },
      { type='prop',
        { type=T.DOT },
      },
    })

    eq(p('foo.bar'), {
      { type='root',
        { type=T.IDENT, value='foo' },
      },
      { type='prop',
        { type=T.DOT },
        { type=T.IDENT, value='bar' },
      },
    })

    eq(p('foo['), {
      { type='root',
        { type=T.IDENT, value='foo' },
      },
      { type='index',
        { type=T.LSQUARE },
      },
    })

    eq(p('foo[1'), {
      { type='root',
        { type=T.IDENT, value='foo' },
      },
      { type='index',
        { type=T.LSQUARE },
        { type=T.NUMBER, value='1' },
      },
    })

    eq(p('foo[1]'), {
      { type='root',
        { type=T.IDENT, value='foo' },
      },
      { type='index',
        { type=T.LSQUARE },
        { type=T.NUMBER, value='1' },
        { type=T.RSQUARE },
      },
    })

    eq(p('foo["bar"]'), {
      { type='root',
        { type=T.IDENT, value='foo' },
      },
      { type='index',
        { type=T.LSQUARE },
        { type=T.STRING, value='"bar"' },
        { type=T.RSQUARE },
      },
    })

    eq(p('foo("bar")'), {
      { type='root',
        { type=T.IDENT, value='foo' },
      },
      { type='call2',
        { type=T.LPAREN },
        { type=T.STRING, value='"bar"' },
        { type=T.RPAREN },
      },
    })

    eq(p('foo"bar"'), {
      { type='root',
        { type=T.IDENT, value='foo' },
      },
      { type='call1',
        { type=T.STRING, value='"bar"' },
      },
    })
  end)
end)
