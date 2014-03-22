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
         chunkname = {
            type = "string"
         }
      }
   },
   Identifier = {
      kind = "Identifier",
      base = "Expression",
      properties = {
         name = {
            type = "string"
         }
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
         left = {
            type = "node",
            kind = "Expression",
         },
         right = {
            type = "node",
            kind = "Expression",
         }
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
         argument = {
            type = "node",
            kind = "Expression"
         }
      }
   },
   AssignmentExpression = {
      kind = "AssignmentExpression",
      base = "Statement",
      properties = {
         left = {
            type = "list",
            kind = { "MemberExpression", "Identifier" },
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
         left = {
            type = "node",
            kind = "Expression"
         },
         right = {
            type = "node",
            kind = "Expression"
         }
      }
   },
   MemberExpression = {
      kind = "MemberExpression",
      base = "Expression",
      properties = {
         object = {
            type = "node",
            kind = "Expression"
         },
         property = {
            type = "node",
            kind = "Expression"
         },
         computed = {
            type    = "boolean",
            default = false
         },
      }
   },
   CallExpression = {
      kind = "CallExpression",
      base = "Expression",
      properties = {
         callee = {
            type = "node",
            kind = "Expression"
         },
         arguments = {
            type = "list",
            kind = "Expression"
         }
      }
   },
   SendExpression = {
      kind = "SendExpression",
      base = "Expression",
      properties = {
         receiver = {
            type = "node",
            kind = "Expression"
         },
         method = {
            type = "node",
            kind = "Identifier",
         },
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
            type = { "string", "number", "nil", "boolean", "cdata" }
         }
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
            type = "node",
            kind = {"Statement", "Expression" }
         }
      }
   },
   EmptyStatement = {
      kind = "EmptyStatement",
      base = "Statement",
      properties = { },
   },
   BlockStatement = {
      kind = "BlockStatement",
      base = "Statement",
      properties = {
         body = {
            type = "list",
            kind = "Statement"
         }
      }
   },
   DoStatement = {
      kind = "DoStatement",
      base = "Statement",
      properties = {
         body = {
            type = "node",
            kind = "BlockStatement",
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
            kind = "BlockStatement",
         },
         alternate = {
            type = "node",
            kind = "BlockStatement",
            optional = true,
         }
      }
   },
   LabelStatement = {
      kind = "LabelStatement",
      base = "Statement",
      properties = {
         label = {
            type = "string",
         }
      }
   },
   GotoStatement = {
      kind = "GotoStatement",
      base = "Statement",
      properties = {
         label = {
            type = "string",
         }
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
         test = {
            type = "node",
            kind = "Expression"
         },
         body = {
            type = "node",
            kind = "Statement"
         }
      }
   },
   RepeatStatement = {
      kind = "RepeatStatement",
      base = "Statement",
      properties = {
         test = {
            type = "node",
            kind = "Expression",
         },
         body = {
            type = "node",
            kind = "BlockStatement"
         }
      }
   },
   ForInit = {
      kind = "ForInit",
      base = "Expression",
      properties = {
         id = {
            type = "node",
            kind = "Identifier",
         },
         value = {
            type = "node",
            kind = "Expression"
         }
      }
   },
   ForStatement = {
      kind = "ForStatement",
      base = "Statement",
      properties = {
         init = {
            type = "node",
            kind = "ForInit"
         },
         last = {
            type = "node",
            kind = "Expression"
         },
         step = {
            type = "node",
            kind = "Expression",
            optional = true,
         },
         body = {
            type = "node",
            kind = "BlockStatement"
         }
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
         init = {
            type = "node",
            kind = "ForNames"
         },
         iter = {
            type = "node",
            kind = "Expression"
         },
         body = {
            type = "node",
            kind = "BlockStatement"
         }
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
            type = "node",
            kind = "Identifier"
         },
         body = {
            type = "node",
            kind = "BlockStatement",
         },
         params = {
            type = "list",
            kind = "Identifier",
         },
         vararg = {
            type = "boolean",
            default = false
         },
         locald = {
            type = "boolean",
            default = false
         }
      }
   },
   FunctionExpression = {
      kind = "FunctionExpression",
      base = "Expression",
      properties = {
         body = {
            type = "node",
            kind = "BlockStatement",
         },
         params = {
            type = "list",
            kind = "Identifier",
         },
         vararg = {
            type = "boolean",
            default = false
         }
      }
   }
}

local function iskind(prop, kind)
   if type(prop) ~= "table" then
      return false
   end
   local meta = syntax[prop.kind]
   while meta do
      if meta.kind == kind then
         return true
      end
      meta = syntax[meta.base]
   end
   return false
end
local function isnode(prop)
   return iskind(prop, "Node")
end

local function kind2str(kind)
   if type(kind) == "table" then
      return table.concat(kind, "|")
   end
   return tostring(kind)
end

local function validate_node(spec, prop)
   if type(spec.kind) ~= "table" then
      spec.kind = { spec.kind }
   end
   if not isnode(prop) then
      return nil, "expected Node"
   end
   for i=1, #spec.kind do
      if iskind(prop, spec.kind[i]) then
         return true, prop
      end
   end
   return nil, "expected "..kind2str(spec.kind)
end

local function validate_list(spec, prop)
   if type(spec.kind) ~= "table" then
      spec.kind = { spec.kind }
   end
   if type(prop) ~= "table" then
      return nil, "expected list of "..kind2str(spec.kind).." (got "..type(prop)..")"
   end
   if isnode(prop) then
      return nil, "expected list of "..kind2str(spec.kind).." (got node)"
   end
   for i=1, #prop do
      local seen = false
      for j=1, #spec.kind do
         if iskind(prop[i], spec.kind[j]) then
            seen = true
            break
         end
      end
      if not seen then
         return nil, "expected list of "..kind2str(spec.kind)
      end
   end
   return true, prop
end

local function validate_enum(spec, prop)
   for i=1, #spec.values do
      if prop == spec.values[i] then
         return true, prop
      end
   end
   return nil, "expected one of "..kind2str(spec.values).." (got '"..tostring(prop).."')"
end

local function validate_type(spec, prop)
   if type(spec.type) == "table" then
      local seen = false
      for i=1, #spec.type do
         if type(prop) == spec.type[i] then
            return true, prop
         end
      end
      return nil, "expected any of type "..kind2str(spec.kind)
   else
      assert(type(spec.type) == "string")
      if type(prop) == spec.type then
         return true, prop
      end
      return nil, "expected "..spec.type.." (got "..type(prop)..")"
   end
end

local function validate_any(spec, prop)
   if spec.type == "node" then
      return validate_node(spec, prop)
   elseif spec.type == "list" then
      return validate_list(spec, prop)
   elseif spec.type == "enum" then
      return validate_enum(spec, prop)
   else
      return validate_type(spec, prop)
   end
end

local function validate(meta, node)
   for name, spec in pairs(meta.properties) do
      if node[name] == nil and type(spec.default) ~= 'nil' then
         node[name] = spec.default
      end
      local prop = node[name]
      if prop == nil and spec.optional then
      else
         local ok, er = validate_any(spec, prop)
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
