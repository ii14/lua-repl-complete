local luacomplete = require('luacomplete')
local tokenize = luacomplete.tokenize

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

local C = mk_token_gen(luacomplete.TOKEN_TYPE.COMMENT)
local I = mk_token_gen(luacomplete.TOKEN_TYPE.IDENT)
local N = mk_token_gen(luacomplete.TOKEN_TYPE.NUMBER)
local O = mk_token_gen(luacomplete.TOKEN_TYPE.OP)
local S = mk_token_gen(luacomplete.TOKEN_TYPE.STRING)
local U = mk_token_gen(luacomplete.TOKEN_TYPE.UNKNOWN)

local eq = assert.are.same


describe('luacomplete.tokenize', function()
  describe('operator tokenizer', function()
    it('tokenizes operators', function()
      eq({O{ value=':' }}, L(':'))
      eq({O{ value=';' }}, L(';'))
      eq({O{ value='<' }}, L('<'))
      eq({O{ value='>' }}, L('>'))
      eq({O{ value='/' }}, L('/'))
      eq({O{ value='*' }}, L('*'))
      eq({O{ value='(' }}, L('('))
      eq({O{ value=')' }}, L(')'))
      eq({O{ value='-' }}, L('-'))
      eq({O{ value='=' }}, L('='))
      eq({O{ value=',' }}, L(','))
      eq({O{ value='{' }}, L('{'))
      eq({O{ value='}' }}, L('}'))
      eq({O{ value='#' }}, L('#'))
      eq({O{ value='^' }}, L('^'))
      eq({O{ value='+' }}, L('+'))
      eq({O{ value='%' }}, L('%'))
      eq({O{ value='[' }}, L('['))
      eq({O{ value=']' }}, L(']'))
      eq({O{ value='==' }}, L('=='))
      eq({O{ value='~=' }}, L('~='))
      eq({O{ value='<=' }}, L('<='))
      eq({O{ value='>=' }}, L('>='))
      eq({O{ value='.' }}, L('.'))
      eq({O{ value='..' }}, L('..'))
      eq({O{ value='...' }}, L('...'))
    end)

    it('tokenizes multiple operators', function()
      eq({
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
      }, L(':;<>/*()-=,{}#^+%[].'))
    end)
  end)

  describe('identifier tokenizer', function()
    it('tokenizes identifiers', function()
      eq({I{ value='a' }}, L('a'))
      eq({I{ value='A' }}, L('A'))
      eq({I{ value='foo' }}, L('foo'))
      eq({I{ value='FOO' }}, L('FOO'))
      eq({I{ value='_foo' }}, L('_foo'))
      eq({I{ value='foo1' }}, L('foo1'))
    end)

    it('tokenizes multiple identifiers', function()
      eq({
        I{ idx=0, col=0, value='foo' },
        I{ idx=4, col=4, value='bar' },
      }, L('foo bar'))
      eq({
        I{ idx=0, line=0, value='foo' },
        I{ idx=4, line=1, value='bar' },
      }, L('foo\nbar'))
    end)
  end)

  describe('number tokenizer', function()
    it('tokenizes decimal numbers', function()
      eq({N{ value='0' }}, L('0'))
      eq({N{ value='00' }}, L('00'))
      eq({N{ value='01234' }}, L('01234'))
      eq({N{ value='56789' }}, L('56789'))
    end)

    it('tokenizes floating point numbers', function()
      eq({N{ value='0.0' }}, L('0.0'))
      eq({N{ value='1.2' }}, L('1.2'))
      eq({N{ value='00.00' }}, L('00.00'))
      eq({N{ value='12.34' }}, L('12.34'))
    end)

    it('tokenizes hexadecimal numbers', function()
      eq({N{ value='0x0' }}, L('0x0'))
      eq({N{ value='0X0' }}, L('0X0'))
      eq({N{ value='0x00' }}, L('0x00'))
      eq({N{ value='0X00' }}, L('0X00'))
      eq({N{ value='0x01234567' }}, L('0x01234567'))
      eq({N{ value='0X01234567' }}, L('0X01234567'))
      eq({N{ value='0x89abcdef' }}, L('0x89abcdef'))
      eq({N{ value='0X89abcdef' }}, L('0X89abcdef'))
      eq({N{ value='0x89ABCDEF' }}, L('0x89ABCDEF'))
      eq({N{ value='0X89ABCDEF' }}, L('0X89ABCDEF'))
    end)

    it('tokenizes scientific notation', function()
      eq({N{ value='314.16e-2' }}, L('314.16e-2'))
      eq({N{ value='0.31416E1' }}, L('0.31416E1'))
    end)

    it('tokenizes multiple numbers', function()
      eq({
        N{ idx=0,  col=0,  value='1' },
        N{ idx=2,  col=2,  value='23' },
        N{ idx=5,  col=5,  value='4.5' },
        N{ idx=9,  col=9,  value='0x6' },
        N{ idx=13, col=13, value='7' },
      }, L('1 23 4.5 0x6 7'))
    end)
  end)

  describe('string tokenizer', function()
    it('tokenizes strings', function()
      eq({S{ value=[['']] }}, L([['']]))
      eq({S{ value=[[""]] }}, L([[""]]))
      eq({S{ value=[['foo']] }}, L([['foo']]))
      eq({S{ value=[["foo"]] }}, L([["foo"]]))
    end)

    it('handles single/double quotes', function()
      eq({S{ value=[['"']] }}, L([['"']]))
      eq({S{ value=[["'"]] }}, L([["'"]]))
    end)

    it('handles escape sequences', function()
      eq({S{ value=[['\'']] }}, L([['\'']]))
      eq({S{ value=[["\""]] }}, L([["\""]]))
    end)

    it('tokenizes multiple strings', function()
      eq({
        S{ idx=0, col=0, value=[['']] },
        S{ idx=2, col=2, value=[['']] },
      }, L([['''']]))
      eq({
        S{ idx=0, col=0, value=[[""]] },
        S{ idx=2, col=2, value=[[""]] },
      }, L([[""""]]))
    end)

    it('tokenizes incomplete strings', function()
      eq({S{ value=[[']], incomplete=true }}, L([[']]))
      eq({S{ value=[["]], incomplete=true }}, L([["]]))
      eq({S{ value=[['foo]], incomplete=true }}, L([['foo]]))
      eq({S{ value=[["foo]], incomplete=true }}, L([["foo]]))
      eq({S{ value=[['\]], incomplete=true }}, L([['\]]))
      eq({S{ value=[["\]], incomplete=true }}, L([["\]]))
    end)

    it('terminates incomplete strings at new line', function()
      eq({S{ value=[[']], incomplete=true }}, L{[[']], ''})
      eq({S{ value=[["]], incomplete=true }}, L{[["]], ''})
      eq({S{ value=[['foo]], incomplete=true }}, L{[['foo]], ''})
      eq({S{ value=[["foo]], incomplete=true }}, L{[["foo]], ''})

      eq({
        S{ idx=0, line=0, value=[[']], incomplete=true },
        S{ idx=2, line=1, value=[[']], incomplete=true },
      }, L{[[']], [[']]})
      eq({
        S{ idx=0, line=0, value=[["]], incomplete=true },
        S{ idx=2, line=1, value=[["]], incomplete=true },
      }, L{[["]], [["]]})

      eq({
        S{ idx=0, line=0, value=[['foo]], incomplete=true },
        S{ idx=5, line=1, value=[['bar]], incomplete=true },
      }, L{[['foo]], [['bar]]})
      eq({
        S{ idx=0, line=0, value=[["foo]], incomplete=true },
        S{ idx=5, line=1, value=[["bar]], incomplete=true },
      }, L{[["foo]], [["bar]]})
    end)

    it('tokenizes incomplete long strings', function()
      eq({S{ value='[[',     long=true, incomplete=true }}, L('[['))
      eq({S{ value='[=[',    long=true, incomplete=true }}, L('[=['))
      eq({S{ value='[==[',   long=true, incomplete=true }}, L('[==['))
      eq({S{ value='[===[',  long=true, incomplete=true }}, L('[===['))
      eq({S{ value='[====[', long=true, incomplete=true }}, L('[====['))

      eq({S{ value='[[foo',     long=true, incomplete=true }}, L('[[foo'))
      eq({S{ value='[=[foo',    long=true, incomplete=true }}, L('[=[foo'))
      eq({S{ value='[==[foo',   long=true, incomplete=true }}, L('[==[foo'))
      eq({S{ value='[===[foo',  long=true, incomplete=true }}, L('[===[foo'))
      eq({S{ value='[====[foo', long=true, incomplete=true }}, L('[====[foo'))
    end)

    it('tokenizes long strings', function()
      eq({S{ value='[[]]',         long=true }}, L('[[]]'))
      eq({S{ value='[=[]=]',       long=true }}, L('[=[]=]'))
      eq({S{ value='[==[]==]',     long=true }}, L('[==[]==]'))
      eq({S{ value='[===[]===]',   long=true }}, L('[===[]===]'))
      eq({S{ value='[====[]====]', long=true }}, L('[====[]====]'))

      eq({S{ value='[[foo]]',         long=true }}, L('[[foo]]'))
      eq({S{ value='[=[foo]=]',       long=true }}, L('[=[foo]=]'))
      eq({S{ value='[==[foo]==]',     long=true }}, L('[==[foo]==]'))
      eq({S{ value='[===[foo]===]',   long=true }}, L('[===[foo]===]'))
      eq({S{ value='[====[foo]====]', long=true }}, L('[====[foo]====]'))
    end)

    it('tokenizes multiple long strings', function()
      eq({
        S{ idx=0, col=0, value='[[]]', long=true },
        S{ idx=4, col=4, value='[[]]', long=true },
      }, L('[[]][[]]'))
      eq({
        S{ idx=0, line=0, value='[[]]', long=true },
        S{ idx=5, line=1, value='[[]]', long=true },
      }, L{'[[]]', '[[]]'})
    end)

    it('tokenizes multiline long strings', function()
      eq({
        S{ value='[[\nfoo\n]]', long=true },
      }, L('[[\nfoo\n]]'))
      eq({
        S{ idx=0,  line=0, value='[[\nfoo\n]]', long=true },
        S{ idx=10, line=3, value='[[\nbar\n]]', long=true },
      }, L('[[\nfoo\n]]\n[[\nbar\n]]'))
      eq({
        S{ idx=0, line=0, col=0, value='[[\nfoo\n]]', long=true },
        S{ idx=9, line=2, col=2, value='[[\nbar\n]]', long=true },
      }, L('[[\nfoo\n]][[\nbar\n]]'))
      eq({
        S{ idx=0,  line=0, value='[=[\nfoo\n]=]', long=true },
        S{ idx=12, line=3, value='[=[\nbar\n]=]', long=true },
      }, L('[=[\nfoo\n]=]\n[=[\nbar\n]=]'))
      eq({
        S{ idx=0,  line=0, col=0, value='[=[\nfoo\n]=]', long=true },
        S{ idx=11, line=2, col=3, value='[=[\nbar\n]=]', long=true },
      }, L('[=[\nfoo\n]=][=[\nbar\n]=]'))
      eq({
        S{ value='[[\nfoo', long=true, incomplete=true },
      }, L('[[\nfoo'))
    end)
  end)

  describe('comment tokenizer', function()
    it('tokenizes line comments', function()
      eq({C{ value='--' }},    L('--'))
      eq({C{ value='--foo' }}, L('--foo'))
    end)

    it('tokenizes multiple line comments', function()
      eq({
        C{ idx=0, line=0, value='--' },
        C{ idx=3, line=1, value='--' },
      }, L{'--', '--'})
      eq({
        C{ idx=0, line=0, value='--foo' },
        C{ idx=6, line=1, value='--bar' },
      }, L{'--foo', '--bar'})
    end)

    it('tokenizes single brackets as line comments', function()
      eq({C{ value='--[' }},  L('--['))
      eq({C{ value='--[]' }}, L('--[]'))
    end)

    it('tokenizes long comments not terminated with `[` as line comments', function()
      eq({C{ value='--[' }},     L('--['))
      eq({C{ value='--[=' }},    L('--[='))
      eq({C{ value='--[==' }},   L('--[=='))
      eq({C{ value='--[===' }},  L('--[==='))
      eq({C{ value='--[====' }}, L('--[===='))
    end)

    it('tokenizes incomplete long comments', function()
      eq({C{ value='--[[',     long=true, incomplete=true }}, L('--[['))
      eq({C{ value='--[=[',    long=true, incomplete=true }}, L('--[=['))
      eq({C{ value='--[==[',   long=true, incomplete=true }}, L('--[==['))
      eq({C{ value='--[===[',  long=true, incomplete=true }}, L('--[===['))
      eq({C{ value='--[====[', long=true, incomplete=true }}, L('--[====['))

      eq({C{ value='--[[foo',     long=true, incomplete=true }}, L('--[[foo'))
      eq({C{ value='--[=[foo',    long=true, incomplete=true }}, L('--[=[foo'))
      eq({C{ value='--[==[foo',   long=true, incomplete=true }}, L('--[==[foo'))
      eq({C{ value='--[===[foo',  long=true, incomplete=true }}, L('--[===[foo'))
      eq({C{ value='--[====[foo', long=true, incomplete=true }}, L('--[====[foo'))
    end)

    it('tokenizes long comments', function()
      eq({C{ value='--[[]]',         long=true }}, L('--[[]]'))
      eq({C{ value='--[=[]=]',       long=true }}, L('--[=[]=]'))
      eq({C{ value='--[==[]==]',     long=true }}, L('--[==[]==]'))
      eq({C{ value='--[===[]===]',   long=true }}, L('--[===[]===]'))
      eq({C{ value='--[====[]====]', long=true }}, L('--[====[]====]'))

      eq({C{ value='--[[foo]]',         long=true }}, L('--[[foo]]'))
      eq({C{ value='--[=[foo]=]',       long=true }}, L('--[=[foo]=]'))
      eq({C{ value='--[==[foo]==]',     long=true }}, L('--[==[foo]==]'))
      eq({C{ value='--[===[foo]===]',   long=true }}, L('--[===[foo]===]'))
      eq({C{ value='--[====[foo]====]', long=true }}, L('--[====[foo]====]'))
    end)

    it('tokenizes long comments with level above 4 as line comments', function()
      eq({C{ value='--[=====[' }},        L('--[=====['))
      eq({C{ value='--[=====[]=====]' }}, L('--[=====[]=====]'))
    end)

    it('tokenizes multiple long comments', function()
      eq({
        C{ idx=0, col=0, value='--[[]]', long=true },
        C{ idx=6, col=6, value='--[[]]', long=true },
      }, L('--[[]]--[[]]'))
      eq({
        C{ idx=0, line=0, value='--[[]]', long=true },
        C{ idx=7, line=1, value='--[[]]', long=true },
      }, L{'--[[]]', '--[[]]'})
    end)

    it('tokenizes multiline long comments', function()
      eq({
        C{ value='--[[\nfoo\n]]', long=true },
      }, L('--[[\nfoo\n]]'))
      eq({
        C{ idx=0,  line=0, value='--[[\nfoo\n]]', long=true },
        C{ idx=12, line=3, value='--[[\nbar\n]]', long=true },
      }, L('--[[\nfoo\n]]\n--[[\nbar\n]]'))
      eq({
        C{ idx=0,  line=0, col=0, value='--[[\nfoo\n]]', long=true },
        C{ idx=11, line=2, col=2, value='--[[\nbar\n]]', long=true },
      }, L('--[[\nfoo\n]]--[[\nbar\n]]'))
      eq({
        C{ idx=0,  line=0, value='--[=[\nfoo\n]=]', long=true },
        C{ idx=14, line=3, value='--[=[\nbar\n]=]', long=true },
      }, L('--[=[\nfoo\n]=]\n--[=[\nbar\n]=]'))
      eq({
        C{ idx=0,  line=0, col=0, value='--[=[\nfoo\n]=]', long=true },
        C{ idx=13, line=2, col=3, value='--[=[\nbar\n]=]', long=true },
      }, L('--[=[\nfoo\n]=]--[=[\nbar\n]=]'))
      eq({
        C{ value='--[[\nfoo', long=true, incomplete=true },
      }, L('--[[\nfoo'))
    end)
  end)

  it('skips leading whitespace', function()
    eq({
      I{ idx=1, col=1, value='foo' },
    }, L(' foo'))
    eq({
      I{ idx=1, line=1, col=0, value='foo' },
    }, L{'', 'foo'})
    eq({
      I{ idx=4, line=2, col=0, value='foo' },
    }, L{' ', ' ', 'foo'})
    eq({
      I{ idx=1, line=0, col=1, value='foo' },
      I{ idx=6, line=1, col=0, value='bar' },
    }, L{' foo ', 'bar'})
  end)

  it('tokenizes expressions', function()
    eq({
      N{ idx=0, col=0, value='2' },
      O{ idx=1, col=1, value='+' },
      N{ idx=2, col=2, value='2' },
      O{ idx=3, col=3, value='==' },
      N{ idx=5, col=5, value='4' },
    }, L('2+2==4'))

    eq({
      O{ idx=0, col=0, value='#' },
      I{ idx=1, col=1, value='foo' },
    }, L('#foo'))

    eq({
      I{ idx=0, col=0, value='foo' },
      O{ idx=3, col=3, value='.' },
      I{ idx=4, col=4, value='bar' },
    }, L('foo.bar'))

    eq({
      I{ idx=0, col=0, value='foo' },
      O{ idx=3, col=3, value='[' },
      N{ idx=4, col=4, value='1' },
      O{ idx=5, col=5, value=']' },
    }, L('foo[1]'))

    eq({
      I{ idx=0, col=0, value='foo' },
      O{ idx=3, col=3, value='[' },
      S{ idx=4, col=4, value='"bar"' },
      O{ idx=9, col=9, value=']' },
    }, L('foo["bar"]'))

    eq({
      I{ idx=0, col=0, value='foo' },
      O{ idx=3, col=3, value=':' },
      I{ idx=4, col=4, value='bar' },
      O{ idx=7, col=7, value='(' },
      O{ idx=8, col=8, value=')' },
    }, L('foo:bar()'))

    eq({
      I{ idx=0, col=0, value='foo' },
      O{ idx=3, col=3, value='(' },
      S{ idx=4, col=4, value='"bar"' },
      O{ idx=9, col=9, value=')' },
    }, L('foo("bar")'))

    eq({
      I{ idx=0, col=0, value='foo' },
      S{ idx=3, col=3, value='"bar"' },
    }, L('foo"bar"'))

    eq({
      I{ idx=0,  line=0, col=0, value='foo' },
      O{ idx=4,  line=0, col=4, value='{' },
      I{ idx=8,  line=1, col=2, value='bar' },
      O{ idx=12, line=1, col=6, value='=' },
      N{ idx=14, line=1, col=8, value='1' },
      O{ idx=16, line=2, col=0, value='}' },
    }, L{'foo {','  bar = 1', '}'})

    eq({
      S{ idx=0, col=0, value='"foo"' },
      O{ idx=5, col=5, value='..' },
      S{ idx=7, col=7, value='"bar"' },
    }, L('"foo".."bar"'))
  end)

  it('tokenizes unknown characters', function()
    eq({U{ value='|' }}, L('|'))
    eq({U{ value='||' }}, L('||'))
    eq({
      N{ idx=0, col=0, value='1' },
      U{ idx=1, col=1, value='|' },
      N{ idx=2, col=2, value='2' },
    }, L('1|2'))
    eq({
      N{ idx=0, col=0, value='1' },
      U{ idx=1, col=1, value='||' },
      N{ idx=3, col=3, value='2' },
    }, L('1||2'))
  end)

  it('returns last position', function()
    -- { idx, line, col }
    local function pos(s)
      return { select(2, L(s)) }
    end

    eq({ 3, 0, 3 }, pos('foo'))
    eq({ 4, 0, 4 }, pos('foo '))
    eq({ 4, 1, 0 }, pos('foo\n'))

    eq({ 4, 0, 4 }, pos('"foo'))
    eq({ 5, 0, 5 }, pos('"foo '))
    eq({ 5, 1, 0 }, pos('"foo\n'))

    eq({ 5, 0, 5 }, pos('--foo'))
    eq({ 6, 0, 6 }, pos('--foo '))
    eq({ 6, 1, 0 }, pos('--foo\n'))

    eq({ 7, 0, 7 }, pos('[[foo]]'))
    eq({ 8, 0, 8 }, pos('[[foo]] '))
    eq({ 8, 1, 0 }, pos('[[foo]]\n'))

    eq({ 5, 0, 5 }, pos('[[foo'))
    eq({ 6, 0, 6 }, pos('[[foo '))
    eq({ 6, 1, 0 }, pos('[[foo\n'))

    eq({ 9, 2, 2 },  pos('[[\nfoo\n]]'))
    eq({ 10, 2, 3 }, pos('[[\nfoo\n]] '))
    eq({ 10, 3, 0 }, pos('[[\nfoo\n]]\n'))

    eq({ 9, 0, 9 },   pos('--[[foo]]'))
    eq({ 10, 0, 10 }, pos('--[[foo]] '))
    eq({ 10, 1, 0 },  pos('--[[foo]]\n'))

    eq({ 7, 0, 7 }, pos('--[[foo'))
    eq({ 8, 0, 8 }, pos('--[[foo '))
    eq({ 8, 1, 0 }, pos('--[[foo\n'))

    eq({ 11, 2, 2 }, pos('--[[\nfoo\n]]'))
    eq({ 12, 2, 3 }, pos('--[[\nfoo\n]] '))
    eq({ 12, 3, 0 }, pos('--[[\nfoo\n]]\n'))
  end)
end)
