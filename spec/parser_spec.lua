local luacomplete = require('luacomplete')
local tokenize = luacomplete.tokenize
local parse = luacomplete.parse
local T = luacomplete.TOKEN_TYPE

local eq = assert.are.same


describe('luacomplete.parse', function()
  local function p(s)
    local ts = parse(tokenize(s))
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

  it('parses basic expressions', function()
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
  end)

  it('parses functions', function()
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

  it('parses tables', function()
    eq({
      { type=T.LCURLY },
      { type=T.RCURLY },
    }, p('{}'))

    -- TODO: doesn't work yet
    -- eq({
    --   { type=T.LCURLY },
    --   { type=T.NUMBER, value='1' },
    --   { type=T.RCURLY },
    -- }, p('{1}'))

    -- eq({
    --   { type=T.LCURLY },
    --   { type=T.NUMBER, value='1' },
    --   { type=T.COMMA },
    --   { type=T.NUMBER, value='2' },
    --   { type=T.RCURLY },
    -- }, p('{1,2}'))

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
      { type=T.LCURLY },
      { type=T.LSQUARE },
      { type=T.LPAREN },
      { type=T.FUNCTION },
      { type=T.LPAREN },
      { type=T.RPAREN },
      { type=T.END },
      { type=T.RPAREN },
      { type=T.RSQUARE },
      { type=T.ASSIGN },
      { type=T.LPAREN },
      { type=T.FUNCTION },
      { type=T.LPAREN },
      { type=T.RPAREN },
      { type=T.END },
      { type=T.RPAREN },
      { type=T.RCURLY },
    }, p('{[(function()end)]=(function()end)}'))
  end)

  it('parses vars', function()
    eq({
      { type=T.IDENT, value='foo' },
    }, p('foo'))

    eq({
      { type=T.IDENT, value='foo' },
      { type=T.DOT },
    }, p('foo.'))

    eq({
      { type=T.IDENT, value='foo' },
      { type=T.DOT },
      { type=T.IDENT, value='bar' },
    }, p('foo.bar'))

    eq({
      { type=T.IDENT, value='foo' },
      { type=T.DOT },
      { type=T.IDENT, value='bar' },
      { type=T.DOT },
      { type=T.IDENT, value='baz' },
    }, p('foo.bar.baz'))

    eq({
      { type=T.IDENT, value='foo' },
      { type=T.LSQUARE },
    }, p('foo['))

    eq({
      { type=T.IDENT, value='foo' },
      { type=T.LSQUARE },
      { type=T.NUMBER, value='1' },
    }, p('foo[1'))

    eq({
      { type=T.IDENT, value='foo' },
      { type=T.LSQUARE },
      { type=T.NUMBER, value='1' },
      { type=T.RSQUARE },
    }, p('foo[1]'))
  end)
end)
