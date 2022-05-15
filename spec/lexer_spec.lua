local lexer = require('luacomplete.lexer')
local tokenize = lexer.tokenize

local function L(s)
  if type(s) == 'string' then
    return tokenize(s)
  elseif type(s) == 'table' then
    return tokenize(table.concat(s, s.le or '\n'))
  else
    error('expected string or table')
  end
end

local function mk_token_gen(type)
  return function(opts)
    local token = { idx=0, col=0, line=0, type=type }
    for k, v in pairs(opts) do token[k] = v end
    return token
  end
end

local token_type = lexer.token_type
local C = mk_token_gen(token_type.COMMENT)
local I = mk_token_gen(token_type.IDENT)
local N = mk_token_gen(token_type.NUMBER)
local O = mk_token_gen(token_type.OP)
local S = mk_token_gen(token_type.STRING)
local U = mk_token_gen(token_type.UNKNOWN)

local eq = assert.are.same


describe('luacomplete.lexer', function()
  describe('operator tokenizer', function()
    it('tokenizes operators', function()
      eq(L(':'), {O{ value=':' }})
      eq(L(';'), {O{ value=';' }})
      eq(L('<'), {O{ value='<' }})
      eq(L('>'), {O{ value='>' }})
      eq(L('/'), {O{ value='/' }})
      eq(L('*'), {O{ value='*' }})
      eq(L('('), {O{ value='(' }})
      eq(L(')'), {O{ value=')' }})
      eq(L('-'), {O{ value='-' }})
      eq(L('='), {O{ value='=' }})
      eq(L(','), {O{ value=',' }})
      eq(L('{'), {O{ value='{' }})
      eq(L('}'), {O{ value='}' }})
      eq(L('#'), {O{ value='#' }})
      eq(L('^'), {O{ value='^' }})
      eq(L('+'), {O{ value='+' }})
      eq(L('%'), {O{ value='%' }})
      eq(L('['), {O{ value='[' }})
      eq(L(']'), {O{ value=']' }})
      eq(L('=='), {O{ value='==' }})
      eq(L('~='), {O{ value='~=' }})
      eq(L('<='), {O{ value='<=' }})
      eq(L('>='), {O{ value='>=' }})
      eq(L('.'), {O{ value='.' }})
      eq(L('..'), {O{ value='..' }})
      eq(L('...'), {O{ value='...' }})
    end)

    it('tokenizes multiple operators', function()
      eq(L(':;<>/*()-=,{}#^+%[].'), {
        O{ idx=0,  col=0,  value=':' },
        O{ idx=1,  col=1,  value=';' },
        O{ idx=2,  col=2,  value='<' },
        O{ idx=3,  col=3,  value='>' },
        O{ idx=4,  col=4,  value='/' },
        O{ idx=5,  col=5,  value='*' },
        O{ idx=6,  col=6,  value='(' },
        O{ idx=7,  col=7,  value=')' },
        O{ idx=8,  col=8,  value='-' },
        O{ idx=9,  col=9,  value='=' },
        O{ idx=10, col=10, value=',' },
        O{ idx=11, col=11, value='{' },
        O{ idx=12, col=12, value='}' },
        O{ idx=13, col=13, value='#' },
        O{ idx=14, col=14, value='^' },
        O{ idx=15, col=15, value='+' },
        O{ idx=16, col=16, value='%' },
        O{ idx=17, col=17, value='[' },
        O{ idx=18, col=18, value=']' },
        O{ idx=19, col=19, value='.' },
      })
    end)
  end)

  describe('identifier tokenizer', function()
    it('tokenizes identifiers', function()
      eq(L('a'),    {I{ value='a' }})
      eq(L('A'),    {I{ value='A' }})
      eq(L('foo'),  {I{ value='foo' }})
      eq(L('FOO'),  {I{ value='FOO' }})
      eq(L('_foo'), {I{ value='_foo' }})
      eq(L('foo1'), {I{ value='foo1' }})
    end)

    it('tokenizes multiple identifiers', function()
      eq(L('foo bar'), {
        I{ idx=0, col=0, value='foo' },
        I{ idx=4, col=4, value='bar' },
      })
      eq(L('foo\nbar'), {
        I{ idx=0, line=0, value='foo' },
        I{ idx=4, line=1, value='bar' },
      })
    end)
  end)

  describe('number tokenizer', function()
    it('tokenizes decimal numbers', function()
      eq(L('0'), {N{ value='0' }})
      eq(L('00'), {N{ value='00' }})
      eq(L('01234'), {N{ value='01234' }})
      eq(L('56789'), {N{ value='56789' }})
    end)

    it('tokenizes floating point numbers', function()
      eq(L('0.0'),   {N{ value='0.0' }})
      eq(L('1.2'),   {N{ value='1.2' }})
      eq(L('00.00'), {N{ value='00.00' }})
      eq(L('12.34'), {N{ value='12.34' }})
    end)

    it('tokenizes hexadecimal numbers', function()
      eq(L('0x0'), {N{ value='0x0' }})
      eq(L('0X0'), {N{ value='0X0' }})
      eq(L('0x00'), {N{ value='0x00' }})
      eq(L('0X00'), {N{ value='0X00' }})
      eq(L('0x01234567'), {N{ value='0x01234567' }})
      eq(L('0X01234567'), {N{ value='0X01234567' }})
      eq(L('0x89abcdef'), {N{ value='0x89abcdef' }})
      eq(L('0X89abcdef'), {N{ value='0X89abcdef' }})
      eq(L('0x89ABCDEF'), {N{ value='0x89ABCDEF' }})
      eq(L('0X89ABCDEF'), {N{ value='0X89ABCDEF' }})
    end)

    it('tokenizes scientific notation', function()
      eq(L('314.16e-2'), {N{ value='314.16e-2' }})
      eq(L('0.31416E1'), {N{ value='0.31416E1' }})
    end)

    it('tokenizes multiple numbers', function()
      eq(L('1 23 4.5 0x6 7'), {
        N{ idx=0,  col=0,  value='1' },
        N{ idx=2,  col=2,  value='23' },
        N{ idx=5,  col=5,  value='4.5' },
        N{ idx=9,  col=9,  value='0x6' },
        N{ idx=13, col=13, value='7' },
      })
    end)
  end)

  describe('string tokenizer', function()
    it('tokenizes strings', function()
      eq(L([['']]), {S{ value=[['']] }})
      eq(L([[""]]), {S{ value=[[""]] }})
      eq(L([['foo']]), {S{ value=[['foo']] }})
      eq(L([["foo"]]), {S{ value=[["foo"]] }})
    end)

    it('handles single/double quotes', function()
      eq(L([['"']]), {S{ value=[['"']] }})
      eq(L([["'"]]), {S{ value=[["'"]] }})
    end)

    it('handles escape sequences', function()
      eq(L([['\'']]), {S{ value=[['\'']] }})
      eq(L([["\""]]), {S{ value=[["\""]] }})
    end)

    it('tokenizes multiple strings', function()
      eq(L([['''']]), {
        S{ idx=0, col=0, value=[['']] },
        S{ idx=2, col=2, value=[['']] },
      })
      eq(L([[""""]]), {
        S{ idx=0, col=0, value=[[""]] },
        S{ idx=2, col=2, value=[[""]] },
      })
    end)

    it('tokenizes incomplete strings', function()
      eq(L([[']]), {S{ value=[[']], incomplete=true }})
      eq(L([["]]), {S{ value=[["]], incomplete=true }})
      eq(L([['foo]]), {S{ value=[['foo]], incomplete=true }})
      eq(L([["foo]]), {S{ value=[["foo]], incomplete=true }})
      eq(L([['\]]), {S{ value=[['\]], incomplete=true }})
      eq(L([["\]]), {S{ value=[["\]], incomplete=true }})
    end)

    it('terminates incomplete strings at new line', function()
      eq(L{[[']], ''}, {S{ value=[[']], incomplete=true }})
      eq(L{[["]], ''}, {S{ value=[["]], incomplete=true }})
      eq(L{[['foo]], ''}, {S{ value=[['foo]], incomplete=true }})
      eq(L{[["foo]], ''}, {S{ value=[["foo]], incomplete=true }})

      eq(L{[[']], [[']]}, {
        S{ idx=0, line=0, value=[[']], incomplete=true },
        S{ idx=2, line=1, value=[[']], incomplete=true },
      })
      eq(L{[["]], [["]]}, {
        S{ idx=0, line=0, value=[["]], incomplete=true },
        S{ idx=2, line=1, value=[["]], incomplete=true },
      })

      eq(L{[['foo]], [['bar]]}, {
        S{ idx=0, line=0, value=[['foo]], incomplete=true },
        S{ idx=5, line=1, value=[['bar]], incomplete=true },
      })
      eq(L{[["foo]], [["bar]]}, {
        S{ idx=0, line=0, value=[["foo]], incomplete=true },
        S{ idx=5, line=1, value=[["bar]], incomplete=true },
      })
    end)

    it('tokenizes incomplete long strings', function()
      eq(L('[['),     {S{ value='[[',     long=true, incomplete=true }})
      eq(L('[=['),    {S{ value='[=[',    long=true, incomplete=true }})
      eq(L('[==['),   {S{ value='[==[',   long=true, incomplete=true }})
      eq(L('[===['),  {S{ value='[===[',  long=true, incomplete=true }})
      eq(L('[====['), {S{ value='[====[', long=true, incomplete=true }})

      eq(L('[[foo'),     {S{ value='[[foo',     long=true, incomplete=true }})
      eq(L('[=[foo'),    {S{ value='[=[foo',    long=true, incomplete=true }})
      eq(L('[==[foo'),   {S{ value='[==[foo',   long=true, incomplete=true }})
      eq(L('[===[foo'),  {S{ value='[===[foo',  long=true, incomplete=true }})
      eq(L('[====[foo'), {S{ value='[====[foo', long=true, incomplete=true }})
    end)

    it('tokenizes long strings', function()
      eq(L('[[]]'),         {S{ value='[[]]',         long=true }})
      eq(L('[=[]=]'),       {S{ value='[=[]=]',       long=true }})
      eq(L('[==[]==]'),     {S{ value='[==[]==]',     long=true }})
      eq(L('[===[]===]'),   {S{ value='[===[]===]',   long=true }})
      eq(L('[====[]====]'), {S{ value='[====[]====]', long=true }})

      eq(L('[[foo]]'),         {S{ value='[[foo]]',         long=true }})
      eq(L('[=[foo]=]'),       {S{ value='[=[foo]=]',       long=true }})
      eq(L('[==[foo]==]'),     {S{ value='[==[foo]==]',     long=true }})
      eq(L('[===[foo]===]'),   {S{ value='[===[foo]===]',   long=true }})
      eq(L('[====[foo]====]'), {S{ value='[====[foo]====]', long=true }})
    end)

    it('tokenizes multiple long strings', function()
      eq(L('[[]][[]]'), {
        S{ idx=0, col=0, value='[[]]', long=true },
        S{ idx=4, col=4, value='[[]]', long=true },
      })
      eq(L{'[[]]', '[[]]'}, {
        S{ idx=0, line=0, value='[[]]', long=true },
        S{ idx=5, line=1, value='[[]]', long=true },
      })
    end)

    it('tokenizes multiline long strings', function()
      eq(L('[[\nfoo\n]]'), {
        S{ value='[[\nfoo\n]]', long=true },
      })
      eq(L('[[\nfoo\n]]\n[[\nbar\n]]'), {
        S{ idx=0,  line=0, value='[[\nfoo\n]]', long=true },
        S{ idx=10, line=3, value='[[\nbar\n]]', long=true },
      })
      eq(L('[[\nfoo\n]][[\nbar\n]]'), {
        S{ idx=0, line=0, col=0, value='[[\nfoo\n]]', long=true },
        S{ idx=9, line=2, col=2, value='[[\nbar\n]]', long=true },
      })
      eq(L('[[\nfoo'), {
        S{ value='[[\nfoo', long=true, incomplete=true },
      })
    end)
  end)

  describe('comment tokenizer', function()
    it('tokenizes line comments', function()
      eq(L('--'),    {C{ value='--' }})
      eq(L('--foo'), {C{ value='--foo' }})
    end)

    it('tokenizes multiple line comments', function()
      eq(L{'--', '--'}, {
        C{ idx=0, line=0, value='--' },
        C{ idx=3, line=1, value='--' },
      })
      eq(L{'--foo', '--bar'}, {
        C{ idx=0, line=0, value='--foo' },
        C{ idx=6, line=1, value='--bar' },
      })
    end)

    it('tokenizes single brackets as line comments', function()
      eq(L('--['),  {C{ value='--[' }})
      eq(L('--[]'), {C{ value='--[]' }})
    end)

    it('tokenizes long comments not terminated with `[` as line comments', function()
      eq(L('--['),     {C{ value='--[' }})
      eq(L('--[='),    {C{ value='--[=' }})
      eq(L('--[=='),   {C{ value='--[==' }})
      eq(L('--[==='),  {C{ value='--[===' }})
      eq(L('--[===='), {C{ value='--[====' }})
    end)

    it('tokenizes incomplete long comments', function()
      eq(L('--[['),     {C{ value='--[[',     long=true, incomplete=true }})
      eq(L('--[=['),    {C{ value='--[=[',    long=true, incomplete=true }})
      eq(L('--[==['),   {C{ value='--[==[',   long=true, incomplete=true }})
      eq(L('--[===['),  {C{ value='--[===[',  long=true, incomplete=true }})
      eq(L('--[====['), {C{ value='--[====[', long=true, incomplete=true }})

      eq(L('--[[foo'),     {C{ value='--[[foo',     long=true, incomplete=true }})
      eq(L('--[=[foo'),    {C{ value='--[=[foo',    long=true, incomplete=true }})
      eq(L('--[==[foo'),   {C{ value='--[==[foo',   long=true, incomplete=true }})
      eq(L('--[===[foo'),  {C{ value='--[===[foo',  long=true, incomplete=true }})
      eq(L('--[====[foo'), {C{ value='--[====[foo', long=true, incomplete=true }})
    end)

    it('tokenizes long comments', function()
      eq(L('--[[]]'),         {C{ value='--[[]]',         long=true }})
      eq(L('--[=[]=]'),       {C{ value='--[=[]=]',       long=true }})
      eq(L('--[==[]==]'),     {C{ value='--[==[]==]',     long=true }})
      eq(L('--[===[]===]'),   {C{ value='--[===[]===]',   long=true }})
      eq(L('--[====[]====]'), {C{ value='--[====[]====]', long=true }})

      eq(L('--[[foo]]'),         {C{ value='--[[foo]]',         long=true }})
      eq(L('--[=[foo]=]'),       {C{ value='--[=[foo]=]',       long=true }})
      eq(L('--[==[foo]==]'),     {C{ value='--[==[foo]==]',     long=true }})
      eq(L('--[===[foo]===]'),   {C{ value='--[===[foo]===]',   long=true }})
      eq(L('--[====[foo]====]'), {C{ value='--[====[foo]====]', long=true }})
    end)

    it('tokenizes long comments with level above 4 as line comments', function()
      eq(L('--[=====['),        {C{ value='--[=====[' }})
      eq(L('--[=====[]=====]'), {C{ value='--[=====[]=====]' }})
    end)

    it('tokenizes multiple long comments', function()
      eq(L('--[[]]--[[]]'), {
        C{ idx=0, col=0, value='--[[]]', long=true },
        C{ idx=6, col=6, value='--[[]]', long=true },
      })
      eq(L{'--[[]]', '--[[]]'}, {
        C{ idx=0, line=0, value='--[[]]', long=true },
        C{ idx=7, line=1, value='--[[]]', long=true },
      })
    end)

    it('tokenizes multiline long comments', function()
      eq(L('--[[\nfoo\n]]'), {
        C{ value='--[[\nfoo\n]]', long=true },
      })
      eq(L('--[[\nfoo\n]]\n--[[\nbar\n]]'), {
        C{ idx=0,  line=0, value='--[[\nfoo\n]]', long=true },
        C{ idx=12, line=3, value='--[[\nbar\n]]', long=true },
      })
      eq(L('--[[\nfoo\n]]--[[\nbar\n]]'), {
        C{ idx=0, line=0, col=0, value='--[[\nfoo\n]]', long=true },
        C{ idx=11, line=2, col=2, value='--[[\nbar\n]]', long=true },
      })
      eq(L('--[[\nfoo'), {
        C{ value='--[[\nfoo', long=true, incomplete=true },
      })
    end)
  end)

  it('skips leading whitespace', function()
    eq(L(' foo'), {
      I{ idx=1, col=1, value='foo' },
    })
    eq(L{'', 'foo'}, {
      I{ idx=1, line=1, col=0, value='foo' },
    })
    eq(L{' ', ' ', 'foo'}, {
      I{ idx=4, line=2, col=0, value='foo' },
    })
    eq(L{' foo ', 'bar'}, {
      I{ idx=1, line=0, col=1, value='foo' },
      I{ idx=6, line=1, col=0, value='bar' },
    })
  end)

  it('tokenizes expressions', function()
    eq(L('2+2==4'), {
      N{ idx=0, col=0, value='2' },
      O{ idx=1, col=1, value='+' },
      N{ idx=2, col=2, value='2' },
      O{ idx=3, col=3, value='==' },
      N{ idx=5, col=5, value='4' },
    })

    eq(L('#foo'), {
      O{ idx=0, col=0, value='#' },
      I{ idx=1, col=1, value='foo' },
    })

    eq(L('foo.bar'), {
      I{ idx=0, col=0, value='foo' },
      O{ idx=3, col=3, value='.' },
      I{ idx=4, col=4, value='bar' },
    })

    eq(L('foo[1]'), {
      I{ idx=0, col=0, value='foo' },
      O{ idx=3, col=3, value='[' },
      N{ idx=4, col=4, value='1' },
      O{ idx=5, col=5, value=']' },
    })

    eq(L('foo["bar"]'), {
      I{ idx=0, col=0, value='foo' },
      O{ idx=3, col=3, value='[' },
      S{ idx=4, col=4, value='"bar"' },
      O{ idx=9, col=9, value=']' },
    })

    eq(L('foo:bar()'), {
      I{ idx=0, col=0, value='foo' },
      O{ idx=3, col=3, value=':' },
      I{ idx=4, col=4, value='bar' },
      O{ idx=7, col=7, value='(' },
      O{ idx=8, col=8, value=')' },
    })

    eq(L('foo("bar")'), {
      I{ idx=0, col=0, value='foo' },
      O{ idx=3, col=3, value='(' },
      S{ idx=4, col=4, value='"bar"' },
      O{ idx=9, col=9, value=')' },
    })

    eq(L('foo"bar"'), {
      I{ idx=0, col=0, value='foo' },
      S{ idx=3, col=3, value='"bar"' },
    })

    eq(L{'foo {','  bar = 1', '}'}, {
      I{ idx=0,  line=0, col=0, value='foo' },
      O{ idx=4,  line=0, col=4, value='{' },
      I{ idx=8,  line=1, col=2, value='bar' },
      O{ idx=12, line=1, col=6, value='=' },
      N{ idx=14, line=1, col=8, value='1' },
      O{ idx=16, line=2, col=0, value='}' },
    })

    eq(L('"foo".."bar"'), {
      S{ idx=0, col=0, value='"foo"' },
      O{ idx=5, col=5, value='..' },
      S{ idx=7, col=7, value='"bar"' },
    })
  end)

  it('tokenizes unknown characters', function()
    eq(L('|'),  {U{ value='|' }})
    eq(L('||'), {U{ value='||' }})
    eq(L('1|2'), {
      N{ idx=0, col=0, value='1' },
      U{ idx=1, col=1, value='|' },
      N{ idx=2, col=2, value='2' },
    })
    eq(L('1||2'), {
      N{ idx=0, col=0, value='1' },
      U{ idx=1, col=1, value='||' },
      N{ idx=3, col=3, value='2' },
    })
  end)
end)
