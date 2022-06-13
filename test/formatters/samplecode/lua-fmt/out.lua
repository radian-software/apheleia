too_many_spaces = true

local function foo()
    return 1, 2, 3
end

local many, vars = function()
    return true
end

local function what_indent()
    noindent = true
    if noindent then
        x = false
    end
    return x
end

local y = "Single quotes are used"
