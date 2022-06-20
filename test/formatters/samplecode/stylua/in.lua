   too_many_spaces     =        true

-- stylua: ignore
local function foo    ()
   return 1,
      2,
      3
end

local many, vars = function() return true end

local function what_indent ()
noindent  = true
  if noindent  then
     x = false
  end
      return x
end

local y = 'Double quotes are used'
