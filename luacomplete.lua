local srep = string.rep

--- Token types
local TOKEN_TYPE = {
  IDENT     = 1, --- Identifier
  NUMBER    = 2, --- Number
  OP        = 3, --- Operator
  STRING    = 4, --- String
  COMMENT   = 5, --- Comment

  -- Operators
  _OP_BEGIN = 6,
  LPAREN    = 7, --- `(` operator
  RPAREN    = 8, --- `)` operator
  LCURLY    = 9, --- `{` operator
  RCURLY    = 10, --- `}` operator
  LSQUARE   = 11, --- `[` operator
  RSQUARE   = 12, --- `]` operator
  EQ        = 13, --- `==` operator
  NE        = 14, --- `~=` operator
  LT        = 15, --- `<` operator
  GT        = 16, --- `>` operator
  LE        = 17, --- `<=` operator
  GE        = 18, --- `>=` operator
  ADD       = 19, --- `+` operator
  SUB       = 20, --- `-` operator
  MUL       = 21, --- `*` operator
  DIV       = 22, --- `/` operator
  MOD       = 23, --- `%` operator
  POW       = 24, --- `^` operator
  LEN       = 25, --- `#` operator
  ASSIGN    = 26, --- `=` operator
  DOT       = 27, --- `.` operator
  CONCAT    = 28, --- `..` operator
  VARARG    = 29, --- `...` operator
  COLON     = 30, --- `:` operator
  COMMA     = 31, --- `,` operator
  SEMICOLON = 32, --- `;` operator
  _OP_END   = 33,

  -- Keywords
  _KW_BEGIN = 34,
  AND       = 35, --- `and` keyword
  BREAK     = 36, --- `break` keyword
  DO        = 37, --- `do` keyword
  ELSE      = 38, --- `else` keyword
  ELSEIF    = 39, --- `elseif` keyword
  END       = 40, --- `end` keyword
  FALSE     = 41, --- `false` keyword
  FOR       = 42, --- `for` keyword
  FUNCTION  = 43, --- `function` keyword
  GOTO      = 44, --- `goto` keyword
  IF        = 45, --- `if` keyword
  IN        = 46, --- `in` keyword
  LOCAL     = 47, --- `local` keyword
  NIL       = 48, --- `nil` keyword
  NOT       = 49, --- `not` keyword
  OR        = 50, --- `or` keyword
  REPEAT    = 51, --- `repeat` keyword
  RETURN    = 52, --- `return` keyword
  THEN      = 53, --- `then` keyword
  TRUE      = 54, --- `true` keyword
  UNTIL     = 55, --- `until` keyword
  WHILE     = 56, --- `while` keyword
  _KW_END   = 57,

  UNKNOWN   = 58, --- Unknown type
}

local OP_TO_TOKEN_TYPE = {
  ['(']   = TOKEN_TYPE.LPAREN,
  [')']   = TOKEN_TYPE.RPAREN,
  ['{']   = TOKEN_TYPE.LCURLY,
  ['}']   = TOKEN_TYPE.RCURLY,
  ['[']   = TOKEN_TYPE.LSQUARE,
  [']']   = TOKEN_TYPE.RSQUARE,
  ['==']  = TOKEN_TYPE.EQ,
  ['~=']  = TOKEN_TYPE.NE,
  ['<']   = TOKEN_TYPE.LT,
  ['>']   = TOKEN_TYPE.GT,
  ['<=']  = TOKEN_TYPE.LE,
  ['>=']  = TOKEN_TYPE.GE,
  ['+']   = TOKEN_TYPE.ADD,
  ['-']   = TOKEN_TYPE.SUB,
  ['*']   = TOKEN_TYPE.MUL,
  ['/']   = TOKEN_TYPE.DIV,
  ['%']   = TOKEN_TYPE.MOD,
  ['^']   = TOKEN_TYPE.POW,
  ['#']   = TOKEN_TYPE.LEN,
  ['=']   = TOKEN_TYPE.ASSIGN,
  ['.']   = TOKEN_TYPE.DOT,
  ['..']  = TOKEN_TYPE.CONCAT,
  ['...'] = TOKEN_TYPE.VARARG,
  [':']   = TOKEN_TYPE.COLON,
  [',']   = TOKEN_TYPE.COMMA,
  [';']   = TOKEN_TYPE.SEMICOLON,
}

local IDENT_TO_TOKEN_TYPE = {
  ['and']      = TOKEN_TYPE.AND,
  ['break']    = TOKEN_TYPE.BREAK,
  ['do']       = TOKEN_TYPE.DO,
  ['else']     = TOKEN_TYPE.ELSE,
  ['elseif']   = TOKEN_TYPE.ELSEIF,
  ['end']      = TOKEN_TYPE.END,
  ['false']    = TOKEN_TYPE.FALSE,
  ['for']      = TOKEN_TYPE.FOR,
  ['function'] = TOKEN_TYPE.FUNCTION,
  ['goto']     = TOKEN_TYPE.GOTO,
  ['if']       = TOKEN_TYPE.IF,
  ['in']       = TOKEN_TYPE.IN,
  ['local']    = TOKEN_TYPE.LOCAL,
  ['nil']      = TOKEN_TYPE.NIL,
  ['not']      = TOKEN_TYPE.NOT,
  ['or']       = TOKEN_TYPE.OR,
  ['repeat']   = TOKEN_TYPE.REPEAT,
  ['return']   = TOKEN_TYPE.RETURN,
  ['then']     = TOKEN_TYPE.THEN,
  ['true']     = TOKEN_TYPE.TRUE,
  ['until']    = TOKEN_TYPE.UNTIL,
  ['while']    = TOKEN_TYPE.WHILE,
}

local TOKEN_TYPE_LEN = {
  -- Operators
  [TOKEN_TYPE.LPAREN]    = #'(',
  [TOKEN_TYPE.RPAREN]    = #')',
  [TOKEN_TYPE.LCURLY]    = #'{',
  [TOKEN_TYPE.RCURLY]    = #'}',
  [TOKEN_TYPE.LSQUARE]   = #'[',
  [TOKEN_TYPE.RSQUARE]   = #']',
  [TOKEN_TYPE.EQ]        = #'==',
  [TOKEN_TYPE.NE]        = #'~=',
  [TOKEN_TYPE.LT]        = #'<',
  [TOKEN_TYPE.GT]        = #'>',
  [TOKEN_TYPE.LE]        = #'<=',
  [TOKEN_TYPE.GE]        = #'>=',
  [TOKEN_TYPE.ADD]       = #'+',
  [TOKEN_TYPE.SUB]       = #'-',
  [TOKEN_TYPE.MUL]       = #'*',
  [TOKEN_TYPE.DIV]       = #'/',
  [TOKEN_TYPE.MOD]       = #'%',
  [TOKEN_TYPE.POW]       = #'^',
  [TOKEN_TYPE.LEN]       = #'#',
  [TOKEN_TYPE.ASSIGN]    = #'=',
  [TOKEN_TYPE.DOT]       = #'.',
  [TOKEN_TYPE.CONCAT]    = #'..',
  [TOKEN_TYPE.VARARG]    = #'...',
  [TOKEN_TYPE.COLON]     = #':',
  [TOKEN_TYPE.COMMA]     = #',',
  [TOKEN_TYPE.SEMICOLON] = #';',

  -- Keywords
  [TOKEN_TYPE.AND]       = #'and',
  [TOKEN_TYPE.BREAK]     = #'break',
  [TOKEN_TYPE.DO]        = #'do',
  [TOKEN_TYPE.ELSE]      = #'else',
  [TOKEN_TYPE.ELSEIF]    = #'elseif',
  [TOKEN_TYPE.END]       = #'end',
  [TOKEN_TYPE.FALSE]     = #'false',
  [TOKEN_TYPE.FOR]       = #'for',
  [TOKEN_TYPE.FUNCTION]  = #'function',
  [TOKEN_TYPE.GOTO]      = #'goto',
  [TOKEN_TYPE.IF]        = #'if',
  [TOKEN_TYPE.IN]        = #'in',
  [TOKEN_TYPE.LOCAL]     = #'local',
  [TOKEN_TYPE.NIL]       = #'nil',
  [TOKEN_TYPE.NOT]       = #'not',
  [TOKEN_TYPE.OR]        = #'or',
  [TOKEN_TYPE.REPEAT]    = #'repeat',
  [TOKEN_TYPE.RETURN]    = #'return',
  [TOKEN_TYPE.THEN]      = #'then',
  [TOKEN_TYPE.TRUE]      = #'true',
  [TOKEN_TYPE.UNTIL]     = #'until',
  [TOKEN_TYPE.WHILE]     = #'while',
}

local WHITESPACE = ' \t\v\f\r'
local RE_WHITESPACE = '['..WHITESPACE..']*([^'..WHITESPACE..'])'

local M = {
  TOKEN_TYPE        = TOKEN_TYPE,
  TOKEN_TYPE_TO_LEN = TOKEN_TYPE_LEN,
}

---@class luaCompleteToken
---@field type number           Token type
---@field value string          Raw value
---@field idx number            0-indexed position
---@field line number           0-indexed line number
---@field col number            0-indexed column number
---@field long? boolean         Style of string/comment token
---@field incomplete? boolean   Is string/comment not terminated?

--- Tokenize lua source code
---
--- TODO: add support \r and \r\n line endings
---
---@param input string                Input string
---@return luaCompleteToken[] tokens  List of tokens
---@return number idx                 0-indexed end position
---@return number line                0-indexed end line number
---@return number col                 0-indexed end column number
---@return luaCompleteToken? err      First unknown token
function M.tokenize(input)
  assert(type(input) == 'string', 'expected string')

  local idx, line, col = 0, 0, 0
  local rest = input

  local function slice(n)
    local value = rest:sub(1, n)
    rest = rest:sub(n + 1)
    idx, col = idx + n, col + n
    return value
  end

  ---@return luaCompleteToken
  local function iter()
    if rest == nil or rest == '' then
      return
    end

    -- remove leading whitespace
    while true do
      -- TODO: handle \r and \r\n line endings
      local _, pos, ch = rest:find(RE_WHITESPACE)
      if pos == nil then
        idx, col = idx + #rest, col + #rest
        return
      elseif ch == '\n' then
        rest = rest:sub(pos + 1)
        idx, line, col = idx + pos, line + 1, 0
      else
        slice(pos - 1)
        break
      end
    end

    -- save starting position
    local sidx, sline, scol = idx, line, col

    -- COMMENTS
    if rest:find('^%-%-') then
      local value = slice(2)
      local long = rest:match('^%[=?=?=?=?%[')
      if long then
        -- LONG COMMENTS
        value = value..slice(#long)
        local re = '^%]'..srep('=', #long - 2)..'%]'
        while true do
          local _, pos, ch = rest:find('([%]\n])')
          if pos == nil then
            value = value..rest
            idx, col = idx + #rest, col + #rest
            rest = ''
            return {
              type = TOKEN_TYPE.COMMENT, value = value,
              idx = sidx, line = sline, col = scol,
              long = true, incomplete = true,
            }
          elseif ch == '\n' then
            -- TODO: handle \r and \r\n line endings
            value = value..rest:sub(1, pos)
            rest = rest:sub(pos + 1)
            idx, line, col = idx + pos, line + 1, 0
          else
            value = value..slice(pos - 1)
            if rest:find(re) then
              return {
                type = TOKEN_TYPE.COMMENT, value = value..slice(#long),
                idx = sidx, line = sline, col = scol,
                long = true,
              }
            else
              value = value..slice(1)
            end
          end
        end
      else
        -- LINE COMMENTS
        -- TODO: handle \r and \r\n line endings
        local pos = rest:find('\n')
        if pos == nil then
          value = rest
          idx, col = idx + #rest, col + #rest
          rest = ''
          return {
            type = TOKEN_TYPE.COMMENT, value = '--'..value,
            idx = sidx, line = sline, col = scol, }
        else
          value = value..rest:sub(1, pos - 1)
          rest = rest:sub(pos + 1)
          idx, line, col = idx + pos, line + 1, 0
          return {
            type = TOKEN_TYPE.COMMENT, value = value,
            idx = sidx, line = sline, col = scol,
          }
        end
      end
    end

    -- LONG STRINGS
    do
      local long = rest:match('^%[=?=?=?=?%[')
      if long then
        local value = slice(#long)
        local re = '^%]'..srep('=', #long - 2)..'%]'
        while true do
          local _, pos, ch = rest:find('([%]\n])')
          if pos == nil then
            value = value..rest
            idx, col = idx + #rest, col + #rest
            rest = ''
            return {
              type = TOKEN_TYPE.STRING, value = value,
              idx = sidx, line = sline, col = scol,
              long = true, incomplete = true,
            }
          elseif ch == '\n' then
            -- TODO: handle \r and \r\n line endings
            value = value..rest:sub(1, pos)
            rest = rest:sub(pos + 1)
            idx, line, col = idx + pos, line + 1, 0
          else
            value = value..slice(pos - 1)
            if rest:find(re) then
              return {
                type = TOKEN_TYPE.STRING, value = value..slice(#long),
                idx = sidx, line = sline, col = scol,
                long = true,
              }
            else
              value = value..slice(1)
            end
          end
        end
      end
    end

    -- OPERATORS
    do
      local m = rest:match('^[=~<>]=')
        or rest:match('^%.%.?%.?')
        or rest:match('^[:;<>/%*%(%)%-=,{}#%^%+%%%[%]]')
      if m then
        rest = rest:sub(#m + 1)
        idx, col = idx + #m, col + #m
        return {
          type = TOKEN_TYPE.OP, value = m,
          idx = sidx, line = sline, col = scol,
        }
      end
    end

    -- IDENTIFIERS
    do
      local m = rest:match('^[A-Za-z_][A-Za-z%d_]*')
      if m then
        rest = rest:sub(#m + 1)
        idx, col = idx + #m, col + #m
        return {
          type = TOKEN_TYPE.IDENT, value = m,
          idx = sidx, line = sline, col = scol,
        }
      end
    end

    -- NUMBERS
    do
      local m = rest:match('^0[xX][%da-fA-F]+')
        or rest:match('^%d+%.?%d*[eE][%+%-]?%d+')
        or rest:match('^%d+[%.]?[%deE]*')
      if m then
        rest = rest:sub(#m + 1)
        idx, col = idx + #m, col + #m
        return {
          type = TOKEN_TYPE.NUMBER, value = m,
          idx = sidx, line = sline, col = scol,
        }
      end
    end

    -- STRINGS
    do
      local str = rest:match('^[\'"]')
      if str then
        local value = str
        rest = rest:sub(2)
        idx, col = idx + 1, col + 1
        while true do
          local _, pos, ch = rest:find('([\n\\'..str..'])')
          if pos == nil then
            value = value..rest
            idx, col = idx + #rest, col + #rest
            rest = ''
            return {
              type = TOKEN_TYPE.STRING, value = value,
              idx = sidx, line = sline, col = scol,
              incomplete = true,
            }
          elseif ch == str then
            return {
              type = TOKEN_TYPE.STRING, value = value..slice(pos),
              idx = sidx, line = sline, col = scol,
            }
          elseif ch == '\n' then
            -- TODO: handle \r and \r\n line endings
            value = value..rest:sub(1, pos - 1)
            rest = rest:sub(pos + 1)
            idx, line, col = idx + pos, line + 1, 0
            return {
              type = TOKEN_TYPE.STRING, value = value,
              idx = sidx, line = sline, col = scol,
              incomplete = true,
            }
          else
            value = value..slice(2)
          end
        end
      end
    end

    return {
      type = TOKEN_TYPE.UNKNOWN, value = slice(1),
      idx = sidx, line = sline, col = scol,
    }
  end

  local tokens = {}
  local last
  local err
  for token in iter do
    if token.type ~= TOKEN_TYPE.UNKNOWN then
      tokens[#tokens+1] = token
      last = nil
    elseif err == nil then
      err = token
      tokens[#tokens+1] = token
      last = token
    elseif last == nil then
      tokens[#tokens+1] = token
      last = token
    else
      -- concat unknown tokens
      last.value = last.value..token.value
    end
  end
  return tokens, idx, line, col, err
end


--- Resolve operator and keyword types, in place
---@param tokens luaCompleteToken[]
---@return luaCompleteToken[] tokens
function M.resolve_tokens(tokens)
  for _, token in ipairs(tokens) do
    if token.type == TOKEN_TYPE.OP then
      token.type = assert(OP_TO_TOKEN_TYPE[token.value])
      token.value = nil
    elseif token.type == TOKEN_TYPE.IDENT then
      local ntype = IDENT_TO_TOKEN_TYPE[token.value]
      if ntype then
        token.type = ntype
        token.value = nil
      end
    end
  end
  return tokens
end

local ELSE = -1

local function new_refs()
  local refs = {}

  local ref_mt = {
    __call = function(o, t)
      for k, v in pairs(t) do
        o[k] = v
      end
      return o
    end,
  }

  local function ref(name)
    local r = refs[name]
    if not r then
      r = setmetatable({}, ref_mt)
      refs[name] = r
    end
    return r
  end

  return ref, refs
end



function M.make_trie2()
  local T = TOKEN_TYPE
  local ref = new_refs()

  ref 'exp' {
    [T.NIL] = ref 'binop_exp' {
      [T.ADD] = ref 'exp',
      [T.SUB] = ref 'exp',
      [T.MUL] = ref 'exp',
      [T.DIV] = ref 'exp',
      [T.POW] = ref 'exp',
      [T.MOD] = ref 'exp',
      [T.CONCAT] = ref 'exp',
      [T.LT] = ref 'exp',
      [T.LE] = ref 'exp',
      [T.GT] = ref 'exp',
      [T.GE] = ref 'exp',
      [T.EQ] = ref 'exp',
      [T.NE] = ref 'exp',
      [T.AND] = ref 'exp',
      [T.OR] = ref 'exp',
      [ELSE] = true,
    },

    [T.FALSE] = ref 'binop_exp',
    [T.TRUE] = ref 'binop_exp',
    [T.NUMBER] = ref 'binop_exp',
    [T.STRING] = ref 'binop_exp',
    [T.VARARG] = ref 'binop_exp',

    -- unop exp
    [T.SUB] = ref 'exp',
    [T.NOT] = ref 'exp',
    [T.LEN] = ref 'exp',

    -- tableconstructor
    [T.LCURLY] = ref 'table_ctor' {
      [T.RCURLY] = ref 'binop_exp',
      -- TODO: there could be also an exp here, if not followed by `=`.
      [T.IDENT] = {
        [T.ASSIGN] = {
          PUSH = ref 'exp',
          THEN = ref 'table_ctor_next' {
            [T.COMMA] = ref 'table_ctor',
            [T.SEMICOLON] = ref 'table_ctor',
            [T.RCURLY] = ref 'binop_exp',
            [ELSE] = 'expected `,`, `;` or `}`',
          },
        },
        [T.COMMA] = ref 'table_ctor',
        [T.SEMICOLON] = ref 'table_ctor',
        [T.RCURLY] = ref 'binop_exp',
        [ELSE] = 'expected `=`, `,`, `;`, `}`',
      },
      [T.LSQUARE] = {
        PUSH = ref 'exp',
        THEN = {
          [T.RSQUARE] = {
            [T.ASSIGN] = {
              PUSH = ref 'exp',
              THEN = ref 'table_ctor_next',
            },
            [ELSE] = 'expected `=`',
          },
          [ELSE] = 'expected `]`',
        },
      },
      [ELSE] = 'expected field or expression',
    },

    -- TODO: function
    -- TODO: prefixexp

    [T.LPAREN] = {
      PUSH = ref 'exp',
      THEN = {
        [T.RPAREN] = ref 'binop_exp',
        [ELSE] = 'expected `)`',
      },
    },

    [ELSE] = 'expected expression',
  }

  return ref 'exp'
end

--- Parse tokens
---@param ts luaCompleteToken[]
function M.parse2(ts)
  M.resolve_tokens(ts)
  local res = {}
  local trie = M.make_trie2()
  local stack = { trie }

  for _, t in ipairs(ts) do
    local pos = stack[#stack]
    local npos = assert(pos[t.type] or pos[ELSE], 'not handled')

    while npos == true do
      -- `true` => expression parsed, pop from the stack
      if #stack <= 1 then
        -- TODO: unexpected token error
        return ('%d:%d: tried to pop from empty stack'):format(t.line + 1, t.col + 1)
      end
      table.remove(stack)
      pos = stack[#stack]
      npos = assert(pos[t.type] or pos[ELSE], 'not handled')
    end

    if type(npos) == 'string' then
      -- string => parse error
      return ('%d:%d: %s'):format(t.line + 1, t.col + 1, npos)
    elseif npos.PUSH then
      -- PUSH => push onto the stack
      stack[#stack] = assert(npos.THEN)
      stack[#stack+1] = npos.PUSH
    else
      stack[#stack] = npos
    end

    table.insert(res, t)
  end

  return res
end



function M.make_trie()
  local T = TOKEN_TYPE
  local trie = {}
  local ref = new_refs()

  trie[T.IDENT] = {
    name = 'var',
    lookup = ref 'after_var' {

      [T.DOT] = {
        name = 'prop',
        lookup = {
          [T.IDENT] = ref 'after_var',
        },
      },

      [T.LSQUARE] = {
        name = 'index',
        lookup = {
          -- TODO: generic exp
          [T.NUMBER] = {
            [T.RSQUARE] = ref 'after_var',
          },
          [T.STRING] = {
            [T.RSQUARE] = ref 'after_var',
          },
          [T.TRUE] = {
            [T.RSQUARE] = ref 'after_var',
          },
          [T.FALSE] = {
            [T.RSQUARE] = ref 'after_var',
          },
        },
      },

      [T.COLON] = {
        name = 'method',
        lookup = {
          [T.IDENT] = {
            [T.LPAREN] = {
              [T.RPAREN] = ref 'after_var',
            },
          },
        },
      },

      [T.LPAREN] = {
        name = 'call',
        lookup = {
          [T.RPAREN] = ref 'after_var',
          -- TODO: generic exp list
          [T.STRING] = {
            [T.RPAREN] = ref 'after_var',
          },
        },
      },

      [T.STRING] = {
        name = 'call',
        lookup = ref 'after_var',
      },

      [T.LCURLY] = {
        name = 'call',
        lookup = {
          [T.RCURLY] = ref 'after_var',
          -- TODO: table ctor
        },
      },

    },
  }

  return trie
end

--- Parse tokens
---@param ts luaCompleteToken[]
function M.parse(ts)
  M.resolve_tokens(ts)
  local res = {}
  local trie = M.make_trie()
  local pos = trie

  for _, t in ipairs(ts) do
    local npos = pos[t.type]
    if npos then
      if npos.name then
        table.insert(res, { type=npos.name, t })
        pos = npos.lookup
      else
        table.insert(res[#res], t)
        pos = npos
      end
    end
  end

  return res
end


--- Utility function: Get token length.
---@param token luaCompleteToken
---@return number length
function M.token_len(token)
  return TOKEN_TYPE_LEN[token.type] or #token.value
end

--- Utility function: Check is token an operator. Expects resolved tokens.
---@param t number
---@return boolean
function M.is_operator(t)
  return t > TOKEN_TYPE._OP_BEGIN and t < TOKEN_TYPE._OP_END
end

--- Utility function: Check is token a keyword. Expects resolved tokens.
---@param t number
---@return boolean
function M.is_keyword(t)
  return t > TOKEN_TYPE._KW_BEGIN and t < TOKEN_TYPE._KW_END
end


return M
