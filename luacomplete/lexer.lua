local srep = string.rep

--- Token types
local token_type = {
  COMMENT = 1, --- Comment type
  IDENT   = 2, --- Identifier type
  NUMBER  = 3, --- Number type
  OP      = 4, --- Operator type
  STRING  = 5, --- String type
  UNKNOWN = 6, --- Unknown type
}

local WHITESPACE = ' \t\v\f\r'
local RE_WHITESPACE = '['..WHITESPACE..']*([^'..WHITESPACE..'])'

local lexer = {
  token_type = token_type,
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
function lexer.tokenize(input)
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
    if rest:match('^%-%-') then
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
              type = token_type.COMMENT, value = value,
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
            if rest:match(re) then
              return {
                type = token_type.COMMENT, value = value..slice(#long),
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
            type = token_type.COMMENT, value = '--'..value,
            idx = sidx, line = sline, col = scol, }
        else
          value = value..rest:sub(1, pos - 1)
          rest = rest:sub(pos + 1)
          idx, line, col = idx + pos, line + 1, 0
          return {
            type = token_type.COMMENT, value = value,
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
              type = token_type.STRING, value = value,
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
            if rest:match(re) then
              return {
                type = token_type.STRING, value = value..slice(#long),
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
          type = token_type.OP, value = m,
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
          type = token_type.IDENT, value = m,
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
          type = token_type.NUMBER, value = m,
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
              type = token_type.STRING, value = value,
              idx = sidx, line = sline, col = scol,
              incomplete = true,
            }
          elseif ch == str then
            return {
              type = token_type.STRING, value = value..slice(pos),
              idx = sidx, line = sline, col = scol,
            }
          elseif ch == '\n' then
            -- TODO: handle \r and \r\n line endings
            value = value..rest:sub(1, pos - 1)
            rest = rest:sub(pos + 1)
            idx, line, col = idx + pos, line + 1, 0
            return {
              type = token_type.STRING, value = value,
              idx = sidx, line = sline, col = scol,
              incomplete = true,
            }
          else
            value = value..slice(2)
          end
        end
      end
    end

    -- TODO: concat unknown tokens
    return {
      type = token_type.UNKNOWN, value = slice(1),
      idx = sidx, line = sline, col = scol,
    }
  end

  local tokens = {}
  local last
  for token in iter do
    if token.type ~= token_type.UNKNOWN then
      tokens[#tokens+1] = token
      last = nil
    elseif last == nil then
      tokens[#tokens+1] = token
      last = token
    else
      last.value = last.value..token.value
    end
  end
  return tokens, idx, line, col
end

return lexer
