--
-- Each entry of "syntax" describe a node of the AST tree.
-- The "properties" field gives the specification for the properties
-- of each node.
--
-- Each "properties" entry is of the form:
--
-- <name> = <ast_element_type>
--
-- where <ast_element_type> is a recursive type defined as follow:
-- it can be:
--
-- "Expression",
-- "Statement",
-- ...
-- to indicate a specific kind of "node". Alternatively a node can be
-- specified as;
--
-- { type = "node", kind = "Statement" }
--
-- In addition an <ast_element_type> can be also:
--
-- { type = "literal", value = "string" }
--
-- { type = "enum", values = {"a", "b", "c"} }
--
-- { type = "list", kind = <ast_element_type> }
--
-- { type = "choice", values = {<ast_element_type>, <ast_element_type>, ...} }
--
-- The latter two are defined recursively. A "list" is Lua table of element of a
-- given type. The "choice" allow an element to be either of one type or another.
--

local syntax = {
   Node = {
      kind = "Node",
      abstract = true
   },
   Expression = {
      kind = "Expression",
      base = "Node",
      abstract = true,
   },
   Statement = {
      kind = "Statement",
      base = "Node",
      abstract = true,
   },
   Chunk = {
      kind = "Chunk",
      base = "Node",
      properties = {
         body = {
            type = "list",
            kind = "Statement"
         },
         chunkname = { type = "literal", value = "string" },
      }
   },
   Identifier = {
      kind = "Identifier",
      base = "Expression",
      properties = {
         name = { type = "literal", value = "string" },
      }
   },
   Vararg = {
      kind = "Vararg",
      base = "Identifier",
      properties = { }
   },
   BinaryExpression = {
      kind = "BinaryExpression",
      base = "Expression",
      properties = {
         operator = {
            type   = "enum",
            values = {
               "+", "-", "*", "/", "^", "%",
               "==", "~=", ">=", ">", "<=", "<",
            }
         },
         left = "Expression",
         right = "Expression",
      }
   },
   ConcatenateExpression = {
      kind = "ConcatenateExpression",
      base = "Expression",
      properties = {
         terms = {
            type = "list",
            kind = "Expression",
         }
      }
   },
   UnaryExpression = {
      kind = "UnaryExpression",
      base = "Expression",
      properties = {
         operator = {
            type   = "enum",
            values = { "not", "-", "#", "'" },
         },
         argument = "Expression",
      }
   },
   ExpressionValue = {
      kind = "ExpressionValue",
      base = "Expression",
      properties = {
         value = "Expression",
      }
   },
   AssignmentExpression = {
      kind = "AssignmentExpression",
      base = "Statement",
      properties = {
         left = {
            type = "list",
            kind = { type = "choice", values = { "MemberExpression", "Identifier" } },
         },
         right = {
            type = "list",
            kind = "Expression",
         }
      }
   },
   LogicalExpression = {
      kind = "LogicalExpression",
      base = "Expression",
      properties = {
         operator = {
            type = "enum",
            values = { "and", "or" }
         },
         left  = "Expression",
         right = "Expression",
      }
   },
   MemberExpression = {
      kind = "MemberExpression",
      base = "Expression",
      properties = {
         object = "Expression",
         property = "Expression",
         computed = {
            type = "literal",
            value = "boolean",
            default = false
         },
      }
   },
   CallExpression = {
      kind = "CallExpression",
      base = "Expression",
      properties = {
         callee = "Expression",
         arguments = { type = "list", kind = "Expression" },
      }
   },
   SendExpression = {
      kind = "SendExpression",
      base = "Expression",
      properties = {
         receiver = "Expression",
         method = "Identifier",
         arguments = {
            type = "list",
            kind = "Expression"
         }
      }
    },
    Literal = {
      kind = "Literal",
      base = "Expression",
      properties = {
         value = {
            type = "choice",
            values = {
               { type = "literal", value = "string" },
               { type = "literal", value = "number" },
               { type = "literal", value = "nil" },
               { type = "literal", value = "boolean" },
               { type = "literal", value = "cdata" },
            }
         },
      }
   },
   Table = {
      kind = "Table",
      base = "Expression",
      properties = {
         array_entries = {
            type = "list",
            kind = "Expression",
         },
         hash_keys = {
            type = "list",
            kind = "Expression",
         },
         hash_values = {
            type = "list",
            kind = "Expression",
         },
      }
   },
   ExpressionStatement = {
      kind = "ExpressionStatement",
      base = "Statement",
      properties = {
         expression = {
            type = "choice",
            values = { "Statement", "Expression" },
         }
      }
   },
   EmptyStatement = {
      kind = "EmptyStatement",
      base = "Statement",
      properties = { },
   },
   DoStatement = {
      kind = "DoStatement",
      base = "Statement",
      properties = {
         body = {
            type = "list",
            kind = "Statement",
         }
      }
   },
   IfStatement = {
      kind = "IfStatement",
      base = "Statement",
      properties = {
         tests = {
            type = "list",
            kind = "Expression",
         },
         cons = {
            type = "list",
            kind = { type = "list", kind = "Statement" },
         },
         alternate = {
            type = "list",
            kind = "Statement",
            optional = true,
         }
      }
   },
   LabelStatement = {
      kind = "LabelStatement",
      base = "Statement",
      properties = {
         label = { type = "literal", value = "string" },
      }
   },
   GotoStatement = {
      kind = "GotoStatement",
      base = "Statement",
      properties = {
         label = { type = "literal", value = "string" }
      }
   },
   BreakStatement = {
      kind = "BreakStatement",
      base = "Statement",
      properties = { },
   },
   ReturnStatement = {
      kind = "ReturnStatement",
      base = "Statement",
      properties = {
         arguments = {
            type = "list",
            kind = "Expression"
         }
      }
   },
   WhileStatement = {
      kind = "WhileStatement",
      base = "Statement",
      properties = {
         test = "Expression",
         body = {
            type = "list",
            kind = "Statement"
         }
      }
   },
   RepeatStatement = {
      kind = "RepeatStatement",
      base = "Statement",
      properties = {
         test = "Expression",
         body = {
            type = "list",
            kind = "Statement",
         },
      }
   },
   ForInit = {
      kind = "ForInit",
      base = "Expression",
      properties = {
         id = "Identifier",
         value = "Expression",
      }
   },
   ForStatement = {
      kind = "ForStatement",
      base = "Statement",
      properties = {
         init = "ForInit",
         last = "Expression",
         step = {
            type = "node",
            kind = "Expression",
            optional = true,
         },
         body = {
            type = "list",
            kind = "Statement",
         },
      }
   },
   ForNames = {
      kind = "ForNames",
      base = "Expression",
      properties = {
         names = {
            type = "list",
            kind = "Identifier",
         }
      }
   },
   ForInStatement = {
      kind = "ForInStatement",
      base = "Statement",
      properties = {
         namelist = "ForNames",
         explist = {
            type = "list",
            kind = "Expression"
         },
         body = {
            type = "list",
            kind = "Statement",
         },
      }
   },
   LocalDeclaration = {
      kind = "LocalDeclaration",
      base = "Statement",
      properties = {
         names = {
            type = "list",
            kind = "Identifier"
         },
         expressions = {
            type = "list",
            kind = "Expression"
         }
      }
   },
   FunctionDeclaration = {
      kind = "FunctionDeclaration",
      base = "Statement",
      properties = {
         id = {
            type = "choice",
            values = { "MemberExpression", "Identifier"},
         },
         body = {
            type = "list",
            kind = "Statement",
         },
         params = {
            type = "list",
            kind = "Identifier",
         },
         vararg = {
            type = "literal",
            value = "boolean",
            default = false
         },
         locald = {
            type = "literal",
            value = "boolean",
            default = false
         }
      }
   },
   FunctionExpression = {
      kind = "FunctionExpression",
      base = "Expression",
      properties = {
         body = {
            type = "list",
            kind = "Statement",
         },
         params = {
            type = "list",
            kind = "Identifier",
         },
         vararg = {
            type = "literal",
            value = "boolean",
            default = false
         }
      }
   }
}

local check

local function iskind(prop, tag)
   if type(prop) ~= "table" then
      return false
   end
   local meta = syntax[prop.kind]
   while meta do
      if meta.kind == tag then
         return true
      end
      meta = syntax[meta.base]
   end
   return false
end

local function isnode(prop)
   return iskind(prop, "Node")
end

local function kind2str(spec)
   if type(spec) == "string" then
      return spec
   elseif spec.type == "node" then
      return spec.kind
   elseif spec.type == "list" then
      local etype = kind2str(spec.kind)
      return "list of " .. etype
   elseif spec.type == "enum" then
      local ls = {}
      for i = 1, spec.values do ls[i] = spec.values[i] end
      return table.concat(ls, ", ")
   elseif spec.type == "literal" then
      return "literal " .. spec.value
   elseif spec.type == "choice" then
      local ls = {}
      for i = 1, spec.values do ls[i] = kind2str(spec.values[i]) end
      return table.concat(ls, "|")
   else
      error("internal error: invalid spec type")
   end
end

local function check_node(tag, prop)
   if not isnode(prop) then
      return false, "expected Node"
   end
   if not iskind(prop, tag) then
      return false, "expected " .. tag
   end
   return true
end

local function check_list(spec, prop)
   if type(prop) ~= "table" then
      return false, "expected list of "..kind2str(spec.kind).." (got "..type(prop)..")"
   end
   if isnode(prop) then
      return false, "expected list of "..kind2str(spec.kind).." (got node)"
   end
   for i=1, #prop do
      check(spec.kind, prop[i])
   end
   return true
end

local function check_enum(spec, prop)
   for i=1, #spec.values do
      if prop == spec.values[i] then return true end
   end
   return false, "expected one of "..kind2str(spec).." (got '"..tostring(prop).."')"
end

local function check_literal(spec, prop)
   assert(type(spec.value) == "string")
   if type(prop) ~= spec.value then
      return false, "expected "..spec.value.." (got "..type(prop)..")"
   end
   return true
end

local function check_choice(spec, prop)
   for i = 1, #spec.values do
      if check(spec.values[i], prop) then
         return true
      end
   end
end

function check(spec, prop)
   if type(spec) == "string" then
      return check_node(spec, prop)
   elseif spec.type == "node" then
      return check_node(spec.kind, prop)
   elseif spec.type == "list" then
      return check_list(spec, prop)
   elseif spec.type == "enum" then
      return check_enum(spec, prop)
   elseif spec.type == "literal" then
      return check_literal(spec, prop)
   elseif spec.type == "choice" then
      return check_choice(spec, prop)
   else
      error("internal error: invalid spec type")
   end
end

local function validate(meta, node)
   for name, spec in pairs(meta.properties) do
      if node[name] == nil and type(spec.default) ~= 'nil' then
         node[name] = spec.default
      end
      local prop = node[name]
      if prop ~= nil or not spec.optional then
         local ok, er = check(spec, prop)
         if not ok then
            error(er.." for "..(node.kind or "?").."."..name)
         end
      end
   end
   return node
end

local function build(kind, props)
   local meta = syntax[kind]
   props.kind = kind
   return validate(meta, props)
end

return {
   syntax = syntax,
   build  = build,
}
