local luacomplete = require('luacomplete')
local tokenize = luacomplete.tokenize
local T = luacomplete.TOKEN_TYPE

local eq = assert.are.same


describe('luacomplete.parse2', function()
  it('does something', function()
    local function p(s)
      local ts = luacomplete.parse2(tokenize(s))
      -- discard line/col information
      if type(ts) == 'table' then
        for _, t in ipairs(ts) do
          if type(t) == 'table' then
            t.col, t.idx, t.line = nil, nil, nil
          end
        end
      end
      return ts
    end

    eq({
      { type=T.STRING, value='"test"' },
      { type=T.EQ },
      { type=T.NIL },
    }, p('"test" == nil'))

    eq({
      { type=T.SUB },
      { type=T.NUMBER, value='6' },
    }, p('-6'))

    eq({
      { type=T.LPAREN },
      { type=T.LPAREN },
      { type=T.NIL },
      { type=T.RPAREN },
      { type=T.GT },
      { type=T.LPAREN },
      { type=T.NIL },
      { type=T.RPAREN },
      { type=T.RPAREN },
    }, p('((nil) > (nil))'))

    eq('1:6: tried to pop from empty stack', p('(nil))'))

    eq({
      { type=T.LCURLY },
      { type=T.RCURLY },
    }, p('{}'))

    eq({
      { type=T.LCURLY },
      { type=T.LSQUARE },
      { type=T.TRUE },
      { type=T.RSQUARE },
      { type=T.ASSIGN },
      { type=T.TRUE },
      { type=T.RCURLY },
    }, p('{[true]=true}'))

    eq({
      { type=T.LCURLY },
      { type=T.IDENT, value='key' },
      { type=T.ASSIGN },
      { type=T.STRING, value='"value"' },
      { type=T.RCURLY },
    }, p('{key="value"}'))

    eq({
      { type=T.FUNCTION },
      { type=T.LPAREN },
      { type=T.RPAREN },
      { type=T.END },
    }, p('function() end'))

    eq({
      { type=T.FUNCTION },
      { type=T.LPAREN },
      { type=T.IDENT, value='a' },
      { type=T.RPAREN },
      { type=T.END },
    }, p('function(a) end'))

    eq({
      { type=T.FUNCTION },
      { type=T.LPAREN },
      { type=T.IDENT, value='a' },
      { type=T.COMMA },
      { type=T.IDENT, value='b' },
      { type=T.RPAREN },
      { type=T.END },
    }, p('function(a, b) end'))
  end)
end)


describe('luacomplete.parse', function()
  it('does something', function()
    local function p(s)
      local es = luacomplete.parse(tokenize(s))
      -- discard line/col information
      for _, ts in ipairs(es) do
        for _, t in ipairs(ts) do
          t.col, t.idx, t.line = nil, nil, nil
        end
      end
      return es
    end

    eq({
      { type='var',
        { type=T.IDENT, value='foo' },
      },
    }, p('foo'))

    eq({
      { type='var',
        { type=T.IDENT, value='foo' },
      },
      { type='prop',
        { type=T.DOT },
      },
    }, p('foo.'))

    eq({
      { type='var',
        { type=T.IDENT, value='foo' },
      },
      { type='prop',
        { type=T.DOT },
        { type=T.IDENT, value='bar' },
      },
    }, p('foo.bar'))

    eq({
      { type='var',
        { type=T.IDENT, value='foo' },
      },
      { type='prop',
        { type=T.DOT },
        { type=T.IDENT, value='bar' },
      },
      { type='prop',
        { type=T.DOT },
        { type=T.IDENT, value='baz' },
      },
    }, p('foo.bar.baz'))

    eq({
      { type='var',
        { type=T.IDENT, value='foo' },
      },
      { type='index',
        { type=T.LSQUARE },
      },
    }, p('foo['))

    eq({
      { type='var',
        { type=T.IDENT, value='foo' },
      },
      { type='index',
        { type=T.LSQUARE },
        { type=T.NUMBER, value='1' },
      },
    }, p('foo[1'))

    eq({
      { type='var',
        { type=T.IDENT, value='foo' },
      },
      { type='index',
        { type=T.LSQUARE },
        { type=T.NUMBER, value='1' },
        { type=T.RSQUARE },
      },
    }, p('foo[1]'))

    eq({
      { type='var',
        { type=T.IDENT, value='foo' },
      },
      { type='index',
        { type=T.LSQUARE },
        { type=T.STRING, value='"bar"' },
        { type=T.RSQUARE },
      },
    }, p('foo["bar"]'))

    eq({
      { type='var',
        { type=T.IDENT, value='foo' },
      },
      { type='call',
        { type=T.STRING, value='"bar"' },
      },
    }, p('foo"bar"'))

    eq({
      { type='var',
        { type=T.IDENT, value='foo' },
      },
      { type='call',
        { type=T.LPAREN },
        { type=T.RPAREN },
      },
    }, p('foo()'))

    eq({
      { type='var',
        { type=T.IDENT, value='foo' },
      },
      { type='call',
        { type=T.LPAREN },
        { type=T.STRING, value='"bar"' },
        { type=T.RPAREN },
      },
    }, p('foo("bar")'))

    eq({
      { type='var',
        { type=T.IDENT, value='foo' },
      },
      { type='call',
        { type=T.LCURLY },
        { type=T.RCURLY },
      },
    }, p('foo{}'))
  end)
end)
