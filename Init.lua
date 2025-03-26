--!native
--!optimize 2

warn("init loaded")

getgenv().RBXActive = true
getgenv().invalidated = {}
getgenv().Files = {}

local cloneref_table = {}

game:FindFirstChildOfClass('UserInputService').WindowFocused:Connect(function()
	RBXActive = true
end)

game:FindFirstChildOfClass('UserInputService').WindowFocusReleased:Connect(function()
	RBXActive = false
end)

getgenv().isrbxactive = newcclosure(function()
	return RBXActive
end)

getgenv().isgameactive = newcclosure(function()
	return RBXActive
end)

getgenv().getcallingscript = newcclosure(function()
	return getgenv(0).script
end)

getgenv().get_calling_script = getcallingscript
getgenv().GetCallingScript = getcallingscript

getgenv().cloneref = newcclosure(function(object)
	if not cloneref_table[object] then cloneref_table[object] = {} end
	local clone = {}

	local mt = {
		__type = "Instance",
		__tostring = function()
			return object.Name
		end,
		__index = function(_, key)
			local value = object[key]
			if type(value) == "function" then
				return function(_, ...)
					return value(object, ...)
				end
			else
				return value
			end
		end,
		__newindex = function(_, key, value)
			object[key] = value
		end,
		__metatable = "The metatable is locked",
		__len = function()
			error("attempt to get length of a userdata value")
		end
	}

	setmetatable(clone, mt)
	table.insert(cloneref_table[object], clone)

	return clone
end)

getgenv().cache = {
	invalidate = newcclosure(function(object)
		local function clone(object)
			local old_archivable = object.Archivable
			local clone

			object.Archivable = true
			clone = object:Clone()
			object.Archivable = old_archivable

			return clone
		end

		local clone = clone(object)
		local oldParent = object.Parent

		table.insert(invalidated, object)

		object:Destroy()
		clone.Parent = oldParent
	end),
	iscached = newcclosure(function(object)
		return table.find(invalidated, object) == nil
	end),
	replace = newcclosure(function(object, new_object)
		if object:IsA("BasePart") and new_object:IsA("BasePart") then
			invalidate(object)
			table.insert(invalidated, new_object)
		end
	end)
}

getgenv().getrunningscripts = newcclosure(function()
	local scripts = {}
	for _, script in ipairs(game:GetService("Players").LocalPlayer:GetDescendants()) do
		if script:IsA("LocalScript") or script:IsA("ModuleScript") then
			scripts[#scripts + 1] = script
		end
	end
	return scripts
end)


getgenv().getscripthash = newcclosure(function(script)
	assert(typeof(scr) == 'Instance', 'Argument #1 to \'getscripthash\' must be an Instance, not ' .. typeof(scr))
	assert(scr.ClassName ~= 'LocalScript' or scr.ClassName ~= 'Script', 'Argument #1 to \'getscripthash\' must be a LocalScript or Script')

	return script:GetHash()
end)

-- File system (Disabled original to prevent spamming files)

local function startswith(a, b)
	return a:sub(1, #b) == b
end
local function endswith(hello, lo) 
	return hello:sub(#hello - #lo + 1, #hello) == lo
end

getgenv().writefile = newcclosure(function(path, content)
	local Path = path:split('/')
	local CurrentPath = {}
	for i = 1, #Path do
		local a = Path[i]
		CurrentPath[i] = a
		if not Files[a] and i ~= #Path then
			Files[table.concat(CurrentPath, '/')] = {}
			Files[table.concat(CurrentPath, '/') .. '/'] = Files[table.concat(CurrentPath, '/')]
		elseif i == #Path then
			Files[table.concat(CurrentPath, '/')] = tostring(content)
		end
	end
end)

getgenv().makefolder = newcclosure(function(path)
	Files[path] = {}
	Files[path .. '/'] = Files[path]
end)

getgenv().isfolder = newcclosure(function(path)
	return type(Files[path]) == 'table'
end)

getgenv().isfile = newcclosure(function(path)
	return type(Files[path]) == 'string'
end)

getgenv().readfile = newcclosure(function(path)
	return Files[path]
end)

getgenv().appendfile = newcclosure(function(path, text2)
	writefile(path, readfile(path) .. text2)
end)

getgenv().loadfile = newcclosure(function(path)
	local content = readfile(path)
	if not content then error('File \'' .. tostring(path) .. '\' does not exist.') return '' end
	local s, func = pcall(function()
		return loadstring(content)
	end)
	return func, not s and func or nil
end)

getgenv().delfolder = newcclosure(function(path)
	local f = Files[path]
	if type(f) == 'table' then Files[path] = nil end
end)

getgenv().delfile = newcclosure(function(path)
	local f = Files[path]
	if type(f) == 'string' then Files[path] = nil end
end)

getgenv().listfiles = newcclosure(function(path)
	if not path or path == '' then
		local Files = {}
		for i, v in pairs(Files) do
			if #i:split('/') == 1 then table.insert(Files, i) end
		end
		return Files
	end
	if type(Files[path]) ~= 'table' then return error(path .. ' is not a folder.') end
	local Files_2 = {}
	for i, v in pairs(Files) do
		if startswith(i, path .. '/') and not endswith(i, '/') and i ~= path and #i:split('/') == (#path:split('/') + 1) then table.insert(Files_2, i) end
	end
	return Files_2
end)

local VirtualInputManager = cloneref(game:GetService("VirtualInputManager"))

getgenv().mouse1click = newcclosure(function()
	VirtualInputManager:SendMouseButtonEvent(0, 0, 0, true, game, 1)
	VirtualInputManager:SendMouseButtonEvent(0, 0, 0, false, game, 1)
end)

getgenv().mouse1press = newcclosure(function()
	VirtualInputManager:SendMouseButtonEvent(0, 0, 0, true, game, 1)
end)

getgenv().mouse1release = newcclosure(function()
	VirtualInputManager:SendMouseButtonEvent(0, 0, 0, false, game, 1)
end)

getgenv().mouse2click = newcclosure(function()
	VirtualInputManager:SendMouseButtonEvent(0, 0, 1, true, game, 1)
	VirtualInputManager:SendMouseButtonEvent(0, 0, 1, false, game, 1)
end)

getgenv().mouse2press = newcclosure(function()
	VirtualInputManager:SendMouseButtonEvent(0, 0, 1, true, game, 1)
end)

getgenv().mouse2release = newcclosure(function()
	VirtualInputManager:SendMouseButtonEvent(0, 0, 1, false, game, 1)
end)

getgenv().mousemoveabs = newcclosure(function(x, y)
	VirtualInputManager:SendMouseMoveEvent(x, y, game)
end)

getgenv().mousemoverel = newcclosure(function(x, y)
	local currentPos = UserInputService:GetMouseLocation()
	VirtualInputManager:SendMouseMoveEvent(currentPos.X + x, currentPos.Y + y, game)
end)

getgenv().mousescroll = newcclosure(function(pixels)
	VirtualInputManager:SendMouseWheelEvent(0, 0, pixels > 0, game)
end)

getgenv().info = newcclosure(function(...)
	game:GetService('TestService'):Message(table.concat({...}, ' '))
end)

local lz4 = {}

type Streamer = {
	Offset: number,
	Source: string,
	Length: number,
	IsFinished: boolean,
	LastUnreadBytes: number,

	read: (Streamer, len: number?, shiftOffset: boolean?) -> string,
	seek: (Streamer, len: number) -> (),
	append: (Streamer, newData: string) -> (),
	toEnd: (Streamer) -> ()
}

type BlockData = {
	[number]: {
		Literal: string,
		LiteralLength: number,
		MatchOffset: number?,
		MatchLength: number?
	}
}

local function plainFind(str, pat)
	return string.find(str, pat, 0, true)
end

local function streamer(str): Streamer
	local Stream = {}
	Stream.Offset = 0
	Stream.Source = str
	Stream.Length = string.len(str)
	Stream.IsFinished = false	
	Stream.LastUnreadBytes = 0

	function Stream.read(self: Streamer, len: number?, shift: boolean?): string
		local len = len or 1
		local shift = if shift ~= nil then shift else true
		local dat = string.sub(self.Source, self.Offset + 1, self.Offset + len)

		local dataLength = string.len(dat)
		local unreadBytes = len - dataLength

		if shift then
			self:seek(len)
		end

		self.LastUnreadBytes = unreadBytes
		return dat
	end

	function Stream.seek(self: Streamer, len: number)
		local len = len or 1

		self.Offset = math.clamp(self.Offset + len, 0, self.Length)
		self.IsFinished = self.Offset >= self.Length
	end

	function Stream.append(self: Streamer, newData: string)
		-- adds new data to the end of a stream
		self.Source ..= newData
		self.Length = string.len(self.Source)
		self:seek(0) --hacky but forces a recalculation of the isFinished flag
	end

	function Stream.toEnd(self: Streamer)
		self:seek(self.Length)
	end

	return Stream
end

getgenv().lz4compress = newcclosure(function(str: string): string
	local blocks: BlockData = {}
	local iostream = streamer(str)

	if iostream.Length > 12 then
		local firstFour = iostream:read(4)

		local processed = firstFour
		local lit = firstFour
		local match = ""
		local LiteralPushValue = ""
		local pushToLiteral = true

		repeat
			pushToLiteral = true
			local nextByte = iostream:read()

			if plainFind(processed, nextByte) then
				local next3 = iostream:read(3, false)

				if string.len(next3) < 3 then
					--push bytes to literal block then break
					LiteralPushValue = nextByte .. next3
					iostream:seek(3)
				else
					match = nextByte .. next3

					local matchPos = plainFind(processed, match)
					if matchPos then
						iostream:seek(3)
						repeat
							local nextMatchByte = iostream:read(1, false)
							local newResult = match .. nextMatchByte

							local repos = plainFind(processed, newResult) 
							if repos then
								match = newResult
								matchPos = repos
								iostream:seek(1)
							end
						until not plainFind(processed, newResult) or iostream.IsFinished

						local matchLen = string.len(match)
						local pushMatch = true

						if iostream.Length - iostream.Offset <= 5 then
							LiteralPushValue = match
							pushMatch = false
							--better safe here, dont bother pushing to match ever
						end

						if pushMatch then
							pushToLiteral = false

							-- gets the position from the end of processed, then slaps it onto processed
							local realPosition = string.len(processed) - matchPos
							processed = processed .. match

							table.insert(blocks, {
								Literal = lit,
								LiteralLength = string.len(lit),
								MatchOffset = realPosition + 1,
								MatchLength = matchLen,
							})
							lit = ""
						end
					else
						LiteralPushValue = nextByte
					end
				end
			else
				LiteralPushValue = nextByte
			end

			if pushToLiteral then
				lit = lit .. LiteralPushValue
				processed = processed .. nextByte
			end
		until iostream.IsFinished
		table.insert(blocks, {
			Literal = lit,
			LiteralLength = string.len(lit)
		})
	else
		local str = iostream.Source
		blocks[1] = {
			Literal = str,
			LiteralLength = string.len(str)
		}
	end

	-- generate the output chunk
	-- %s is for adding header
	local output = string.rep("\x00", 4)
	local function write(char)
		output = output .. char
	end
	-- begin working through chunks
	for chunkNum, chunk in blocks do
		local litLen = chunk.LiteralLength
		local matLen = (chunk.MatchLength or 4) - 4

		-- create token
		local tokenLit = math.clamp(litLen, 0, 15)
		local tokenMat = math.clamp(matLen, 0, 15)

		local token = bit32.lshift(tokenLit, 4) + tokenMat
		write(string.pack("<I1", token))

		if litLen >= 15 then
			litLen = litLen - 15
			--begin packing extra bytes
			repeat
				local nextToken = math.clamp(litLen, 0, 0xFF)
				write(string.pack("<I1", nextToken))
				if nextToken == 0xFF then
					litLen = litLen - 255
				end
			until nextToken < 0xFF
		end

		-- push raw lit data
		write(chunk.Literal)

		if chunkNum ~= #blocks then
			-- push offset as u16
			write(string.pack("<I2", chunk.MatchOffset))

			-- pack extra match bytes
			if matLen >= 15 then
				matLen = matLen - 15

				repeat
					local nextToken = math.clamp(matLen, 0, 0xFF)
					write(string.pack("<I1", nextToken))
					if nextToken == 0xFF then
						matLen = matLen - 255
					end
				until nextToken < 0xFF
			end
		end
	end
	--append chunks
	local compLen = string.len(output) - 4
	local decompLen = iostream.Length

	return string.pack("<I4", compLen) .. string.pack("<I4", decompLen) .. output
end)

getgenv().lz4decompress = newcclosure(function(lz4data: string): string
	local inputStream = streamer(lz4data)

	local compressedLen = string.unpack("<I4", inputStream:read(4))
	local decompressedLen = string.unpack("<I4", inputStream:read(4))
	local reserved = string.unpack("<I4", inputStream:read(4))

	if compressedLen == 0 then
		return inputStream:read(decompressedLen)
	end

	local outputStream = streamer("")

	repeat
		local token = string.byte(inputStream:read())
		local litLen = bit32.rshift(token, 4)
		local matLen = bit32.band(token, 15) + 4

		if litLen >= 15 then
			repeat
				local nextByte = string.byte(inputStream:read())
				litLen += nextByte
			until nextByte ~= 0xFF
		end

		local literal = inputStream:read(litLen)
		outputStream:append(literal)
		outputStream:toEnd()
		if outputStream.Length < decompressedLen then
			--match
			local offset = string.unpack("<I2", inputStream:read(2))
			if matLen >= 19 then
				repeat
					local nextByte = string.byte(inputStream:read())
					matLen += nextByte
				until nextByte ~= 0xFF
			end

			outputStream:seek(-offset)
			local pos = outputStream.Offset
			local match = outputStream:read(matLen)
			local unreadBytes = outputStream.LastUnreadBytes
			local extra
			if unreadBytes then
				repeat
					outputStream.Offset = pos
					extra = outputStream:read(unreadBytes)
					unreadBytes = outputStream.LastUnreadBytes
					match ..= extra
				until unreadBytes <= 0
			end

			outputStream:append(match)
			outputStream:toEnd()
		end

	until outputStream.Length >= decompressedLen

	return outputStream.Source
end)

getgenv().lz4 = lz4

-- Crypt library
local b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

getgenv().getc = function(str)
	local sum = 0
	for _, code in utf8.codes(str) do
		sum = sum + code
	end
	return sum
end

getgenv().str2hexa = function(a)
	return string.gsub(
		a,
		".",
		function(b)
			return string.format("%02x", string.byte(b))
		end
	)
end

getgenv().num2s = function(c, d)
	local a = ""
	for e = 1, d do
		local f = c % 256
		a = string.char(f) .. a
		c = (c - f) / 256
	end
	return a
end

getgenv().s232num = function(a, e)
	local d = 0
	for g = e, e + 3 do
		d = d * 256 + string.byte(a, g)
	end
	return d
end

getgenv().preproc = function(h, i)
	local j = 64 - (i + 9) % 64
	i = num2s(8 * i, 8)
	h = h .. "\128" .. string.rep("\0", j) .. i
	assert(#h % 64 == 0)
	return h
end

getgenv().k = function(h, e, l)
	local m = {}
	local n = {
		0x428a2f98,
		0x71374491,
		0xb5c0fbcf,
		0xe9b5dba5,
		0x3956c25b,
		0x59f111f1,
		0x923f82a4,
		0xab1c5ed5,
		0xd807aa98,
		0x12835b01,
		0x243185be,
		0x550c7dc3,
		0x72be5d74,
		0x80deb1fe,
		0x9bdc06a7,
		0xc19bf174,
		0xe49b69c1,
		0xefbe4786,
		0x0fc19dc6,
		0x240ca1cc,
		0x2de92c6f,
		0x4a7484aa,
		0x5cb0a9dc,
		0x76f988da,
		0x983e5152,
		0xa831c66d,
		0xb00327c8,
		0xbf597fc7,
		0xc6e00bf3,
		0xd5a79147,
		0x06ca6351,
		0x14292967,
		0x27b70a85,
		0x2e1b2138,
		0x4d2c6dfc,
		0x53380d13,
		0x650a7354,
		0x766a0abb,
		0x81c2c92e,
		0x92722c85,
		0xa2bfe8a1,
		0xa81a664b,
		0xc24b8b70,
		0xc76c51a3,
		0xd192e819,
		0xd6990624,
		0xf40e3585,
		0x106aa070,
		0x19a4c116,
		0x1e376c08,
		0x2748774c,
		0x34b0bcb5,
		0x391c0cb3,
		0x4ed8aa4a,
		0x5b9cca4f,
		0x682e6ff3,
		0x748f82ee,
		0x78a5636f,
		0x84c87814,
		0x8cc70208,
		0x90befffa,
		0xa4506ceb,
		0xbef9a3f7,
		0xc67178f2
	}
	for g = 1, 16 do
		m[g] = s232num(h, e + (g - 1) * 4)
	end
	for g = 17, 64 do
		local o = m[g - 15]
		local p = bit32.bxor(bit32.rrotate(o, 7), bit32.rrotate(o, 18), bit32.rshift(o, 3))
		o = m[g - 2]
		local q = bit32.bxor(bit32.rrotate(o, 17), bit32.rrotate(o, 19), bit32.rshift(o, 10))
		m[g] = (m[g - 16] + p + m[g - 7] + q) % 2 ^ 32
	end
	local r, s, b, t, u, v, w, x = l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8]
	for e = 1, 64 do
		local p = bit32.bxor(bit32.rrotate(r, 2), bit32.rrotate(r, 13), bit32.rrotate(r, 22))
		local y = bit32.bxor(bit32.band(r, s), bit32.band(r, b), bit32.band(s, b))
		local z = (p + y) % 2 ^ 32
		local q = bit32.bxor(bit32.rrotate(u, 6), bit32.rrotate(u, 11), bit32.rrotate(u, 25))
		local A = bit32.bxor(bit32.band(u, v), bit32.band(bit32.bnot(u), w))
		local B = (x + q + A + n[e] + m[e]) % 2 ^ 32
		x = w
		w = v
		v = u
		u = (t + B) % 2 ^ 32
		t = b
		b = s
		s = r
		r = (B + z) % 2 ^ 32
	end
	l[1] = (l[1] + r) % 2 ^ 32
	l[2] = (l[2] + s) % 2 ^ 32
	l[3] = (l[3] + b) % 2 ^ 32
	l[4] = (l[4] + t) % 2 ^ 32
	l[5] = (l[5] + u) % 2 ^ 32
	l[6] = (l[6] + v) % 2 ^ 32
	l[7] = (l[7] + w) % 2 ^ 32
	l[8] = (l[8] + x) % 2 ^ 32
end

getgenv().crypt = {
	base64encode = function(data)
		return (data:gsub('.', function(x) 
			local r,b='',x:byte()
			for i=8,1,-1 do r=r..(b%2^i-b%2^(i-1)>0 and '1' or '0') end
			return r
		end)..'0000'):gsub('%d%d%d?%d?%d?%d?', function(x)
			if (#x < 6) then return '' end
			local c=0
			for i=1,6 do c=c+(x:sub(i,i)=='1' and 2^(6-i) or 0) end
			return b64:sub(c+1,c+1)
		end)..({'','==','='})[#data%3+1]
	end,
	base64decode = function(data)
		data = data:gsub('[^'..b64..'=]', '')
		return (data:gsub('.', function(x)
			if (x == '=') then return '' end
			local r,f='',b64:find(x)-1
			for i=6,1,-1 do r=r..(f%2^i-f%2^(i-1)>0 and '1' or '0') end
			return r
		end):gsub('%d%d%d?%d?%d?%d?%d?%d?', function(x)
			if (#x ~= 8) then return '' end
			local c=0
			for i=1,8 do c=c+(x:sub(i,i)=='1' and 2^(8-i) or 0) end
			return string.char(c)
		end)) 
	end,
	base64_decode = base64decode,
	base64_encode = base64encode,
	base64 = {
		encode = base64encode,
		decode = base64decode
	},
	encrypt = function(data, key, iv, mode)
		assert(type(data) == "string", "Data must be a string")
		assert(type(key) == "string", "Key must be a string")

		mode = mode or "CBC"
		iv = iv or crypt.generatebytes(16)

		local byteChange = (getc(mode) + getc(iv) + getc(key)) % 256
		local res = {}

		for i = 1, #data do
			local byte = (string.byte(data, i) + byteChange) % 256
			table.insert(res, string.char(byte))
		end

		local encrypted = table.concat(res)
		return crypt.base64encode(encrypted), iv
	end,
	decrypt = function(data, key, iv, mode)
		assert(type(data) == "string", "Data must be a string")
		assert(type(key) == "string", "Key must be a string")
		assert(type(iv) == "string", "IV must be a string")

		mode = mode or "CBC"

		local decodedData = crypt.base64decode(data)
		local byteChange = (getc(mode) + getc(iv) + getc(key)) % 256
		local res = {}

		for i = 1, #decodedData do
			local byte = (string.byte(decodedData, i) - byteChange) % 256
			table.insert(res, string.char(byte))
		end

		return table.concat(res)
	end,
	generatebytes = function(size)
		local bytes = table.create(size)

		for i = 1, size do
			bytes[i] = string.char(math.random(0, 255))
		end

		return crypt.base64encode(table.concat(bytes))
	end,
	generatekey = function()
		return crypt.generatebytes(32)
	end,
	hash = function(h)
		h = preproc(h, #h)
		local l = {0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19}
		for e = 1, #h, 64 do
			k(h, e, l)
		end
		return str2hexa(
			num2s(l[1], 4) ..
				num2s(l[2], 4) ..
				num2s(l[3], 4) .. num2s(l[4], 4) .. num2s(l[5], 4) .. num2s(l[6], 4) .. num2s(l[7], 4) .. num2s(l[8], 4)
		)
	end
}

local Drawings = {}
	
local drawings = {}

local camera = game.Workspace.CurrentCamera
local drawingUI = Instance.new("ScreenGui")

drawingUI.Name = "Drawing"
drawingUI.IgnoreGuiInset = true
drawingUI.DisplayOrder = 0x7fffffff
drawingUI.Parent = game:GetService("CoreGui")

local drawingIndex = 0
local uiStrokes = table.create(0)
local baseDrawingObj = setmetatable({
	Visible = true,
	ZIndex = 0,
	Transparency = 1,
	Color = Color3.new(),
	Remove = function(self)
		setmetatable(self, nil)
	end
}, {
	__add = function(t1, t2)
		local result = table.clone(t1)

		for index, value in t2 do
			result[index] = value
		end
		return result
	end
})
local drawingFontsEnum = {
	[0] = Font.fromEnum(Enum.Font.Roboto),
	[1] = Font.fromEnum(Enum.Font.Legacy),
	[2] = Font.fromEnum(Enum.Font.SourceSans),
	[3] = Font.fromEnum(Enum.Font.RobotoMono),
}
-- function
local function getFontFromIndex(fontIndex: number): Font
	return drawingFontsEnum[fontIndex]
end

local function convertTransparency(transparency: number): number
	return math.clamp(1 - transparency, 0, 1)
end
-- main
local DrawingLib = {}
DrawingLib.Fonts = {
	["UI"] = 0,
	["System"] = 1,
	["Flex"] = 2,
	["Monospace"] = 3
}

function DrawingLib.new(drawingType)
	drawingIndex += 1
if drawingType == "Line" then
    local lineObj = {
        From = Vector2.zero,
        To = Vector2.zero,
        Thickness = 1
    }

    for key, value in pairs(baseDrawingObj) do
        lineObj[key] = value
    end

    local lineFrame = Instance.new("Frame")
    lineFrame.Name = drawingIndex
    lineFrame.AnchorPoint = Vector2.one * 0.5
    lineFrame.BorderSizePixel = 0

    lineFrame.BackgroundColor3 = lineObj.Color
    lineFrame.Visible = lineObj.Visible
    lineFrame.ZIndex = lineObj.ZIndex
    lineFrame.BackgroundTransparency = convertTransparency(lineObj.Transparency)
    lineFrame.Size = UDim2.new()

    lineFrame.Parent = drawingUI
    local bs = table.create(0) -- Cache table
    table.insert(drawings, bs)

    local function updateLine()
        local direction = lineObj.To - lineObj.From
        local center = (lineObj.To + lineObj.From) / 2
        local distance = direction.Magnitude
        local theta = math.deg(math.atan2(direction.Y, direction.X))

        lineFrame.Position = UDim2.fromOffset(center.X, center.Y)
        lineFrame.Rotation = theta
        lineFrame.Size = UDim2.fromOffset(distance, lineObj.Thickness)
    end

    return setmetatable(bs, {
        __newindex = function(_, index, value)
            if lineObj[index] == nil then return end

            lineObj[index] = value
            if index == "From" or index == "To" or index == "Thickness" then
                updateLine()
            elseif index == "Visible" then
                lineFrame.Visible = value
            elseif index == "ZIndex" then
                lineFrame.ZIndex = value
            elseif index == "Transparency" then
                lineFrame.BackgroundTransparency = convertTransparency(value)
            elseif index == "Color" then
                lineFrame.BackgroundColor3 = value
            end
        end,
        __index = function(self, index)
            if index == "Remove" or index == "Destroy" then
                return function()
                    lineFrame:Destroy()
                    lineObj.Remove(self)
                    for k in pairs(bs) do
                        bs[k] = nil
                    end
                end
            end
            return lineObj[index]
        end
    })
	elseif drawingType == "Text" then
    local textObj = {
        Text = "",
        Font = DrawingLib.Fonts.UI,
        Size = 0,
        Position = Vector2.zero,
        Center = false,
        Outline = false,
        OutlineColor = Color3.new()
    }

    for key, value in pairs(baseDrawingObj) do
        textObj[key] = value
    end

    local textLabel = Instance.new("TextLabel")
    local uiStroke = Instance.new("UIStroke")
    
    textLabel.Name = drawingIndex
    textLabel.AnchorPoint = Vector2.one * 0.5
    textLabel.BorderSizePixel = 0
    textLabel.BackgroundTransparency = 1

    textLabel.Visible = textObj.Visible
    textLabel.TextColor3 = textObj.Color
    textLabel.TextTransparency = convertTransparency(textObj.Transparency)
    textLabel.ZIndex = textObj.ZIndex
    textLabel.FontFace = getFontFromIndex(textObj.Font)
    textLabel.TextSize = textObj.Size

    -- Function to update textLabel size and position
    local function updateTextLabel()
        local textBounds = textLabel.TextBounds
        local offset = textBounds / 2
        textLabel.Size = UDim2.fromOffset(textBounds.X, textBounds.Y)
        textLabel.Position = UDim2.fromOffset(textObj.Position.X + (textObj.Center and 0 or offset.X), textObj.Position.Y + offset.Y)
    end

    -- Connect to TextBounds property change
    textLabel:GetPropertyChangedSignal("TextBounds"):Connect(updateTextLabel)

    uiStroke.Thickness = 1
    uiStroke.Enabled = textObj.Outline
    uiStroke.Color = textObj.OutlineColor

    textLabel.Parent = drawingUI
    uiStroke.Parent = textLabel
    
    local bs = table.create(0) -- Cache table
    table.insert(drawings, bs)

    return setmetatable(bs, {
        __newindex = function(_, index, value)
            if textObj[index] == nil then return end

            textObj[index] = value
            if index == "Text" then
                textLabel.Text = value
            elseif index == "Font" then
                textLabel.FontFace = getFontFromIndex(math.clamp(value, 0, 3))
            elseif index == "Size" then
                textLabel.TextSize = value
            elseif index == "Position" then
                updateTextLabel()
            elseif index == "Center" then
                local position = value and (camera.ViewportSize / 2) or textObj.Position
                textLabel.Position = UDim2.fromOffset(position.X, position.Y)
            elseif index == "Outline" then
                uiStroke.Enabled = value
            elseif index == "OutlineColor" then
                uiStroke.Color = value
            elseif index == "Visible" then
                textLabel.Visible = value
            elseif index == "ZIndex" then
                textLabel.ZIndex = value
            elseif index == "Transparency" then
                local transparency = convertTransparency(value)
                textLabel.TextTransparency = transparency
                uiStroke.Transparency = transparency
            elseif index == "Color" then
                textLabel.TextColor3 = value
            end
        end,
        __index = function(self, index)
            if index == "Remove" or index == "Destroy" then
                return function()
                    textLabel:Destroy()
                    textObj.Remove(self)
                    for k in pairs(bs) do
                        bs[k] = nil
                    end
                end
            elseif index == "TextBounds" then
                return textLabel.TextBounds
            end
            return textObj[index]
        end
    })
elseif drawingType == "Circle" then
    local circleObj = {
        Radius = 150,
        Position = Vector2.zero,
        Thickness = 0.7,
        Filled = false
    }

    for key, value in pairs(baseDrawingObj) do
        circleObj[key] = value
    end

    local circleFrame = Instance.new("Frame")
    local uiCorner = Instance.new("UICorner")
    local uiStroke = Instance.new("UIStroke")
    
    circleFrame.Name = drawingIndex
    circleFrame.AnchorPoint = Vector2.one * 0.5
    circleFrame.BorderSizePixel = 0
    circleFrame.BackgroundTransparency = circleObj.Filled and convertTransparency(circleObj.Transparency) or 1
    circleFrame.BackgroundColor3 = circleObj.Color
    circleFrame.Visible = circleObj.Visible
    circleFrame.ZIndex = circleObj.ZIndex
    circleFrame.Size = UDim2.fromOffset(circleObj.Radius * 2, circleObj.Radius * 2)

    uiCorner.CornerRadius = UDim.new(1, 0)
    uiStroke.Thickness = circleObj.Thickness
    uiStroke.Enabled = not circleObj.Filled
    uiStroke.ApplyStrokeMode = Enum.ApplyStrokeMode.Border

    circleFrame.Parent = drawingUI
    uiCorner.Parent = circleFrame
    uiStroke.Parent = circleFrame

    local bs = table.create(0)
    table.insert(drawings, bs)

    return setmetatable(bs, {
        __newindex = function(_, index, value)
            if circleObj[index] == nil then return end

            circleObj[index] = value
            if index == "Radius" then
                circleFrame.Size = UDim2.fromOffset(value * 2, value * 2)
            elseif index == "Position" then
                circleFrame.Position = UDim2.fromOffset(value.X, value.Y)
            elseif index == "Thickness" then
                uiStroke.Thickness = math.clamp(value, 0.6, math.huge)
            elseif index == "Filled" then
                circleFrame.BackgroundTransparency = value and convertTransparency(circleObj.Transparency) or 1
                uiStroke.Enabled = not value
            elseif index == "Visible" then
                circleFrame.Visible = value
            elseif index == "ZIndex" then
                circleFrame.ZIndex = value
            elseif index == "Transparency" then
                local transparency = convertTransparency(value)
                circleFrame.BackgroundTransparency = circleObj.Filled and transparency or 1
                uiStroke.Transparency = transparency
            elseif index == "Color" then
                circleFrame.BackgroundColor3 = value
                uiStroke.Color = value
            end
        end,
        __index = function(self, index)
            if index == "Remove" or index == "Destroy" then
                return function()
                    circleFrame:Destroy()
                    circleObj.Remove(self)
                    for k in pairs(bs) do
                        bs[k] = nil
                    end
                end
            end
            return circleObj[index]
        end
    })

elseif drawingType == "Square" then
    local squareObj = {
        Size = Vector2.zero,
        Position = Vector2.zero,
        Thickness = 0.7,
        Filled = false
    }

    for key, value in pairs(baseDrawingObj) do
        squareObj[key] = value
    end

    local squareFrame = Instance.new("Frame")
    local uiStroke = Instance.new("UIStroke")

    squareFrame.Name = drawingIndex
    squareFrame.BorderSizePixel = 0
    squareFrame.BackgroundTransparency = squareObj.Filled and convertTransparency(squareObj.Transparency) or 1
    squareFrame.BackgroundColor3 = squareObj.Color
    squareFrame.Visible = squareObj.Visible
    squareFrame.ZIndex = squareObj.ZIndex
    squareFrame.Size = UDim2.fromOffset(squareObj.Size.X, squareObj.Size.Y)

    uiStroke.Thickness = squareObj.Thickness
    uiStroke.Enabled = not squareObj.Filled
    uiStroke.LineJoinMode = Enum.LineJoinMode.Miter

    squareFrame.Parent = drawingUI
    uiStroke.Parent = squareFrame

    local bs = table.create(0)
    table.insert(drawings, bs)

    return setmetatable(bs, {
        __newindex = function(_, index, value)
            if squareObj[index] == nil then return end

            squareObj[index] = value
            if index == "Size" then
                squareFrame.Size = UDim2.fromOffset(value.X, value.Y)
            elseif index == "Position" then
                squareFrame.Position = UDim2.fromOffset(value.X, value.Y)
            elseif index == "Thickness" then
                uiStroke.Thickness = math.clamp(value, 0.6, math.huge)
            elseif index == "Filled" then
                squareFrame.BackgroundTransparency = value and convertTransparency(squareObj.Transparency) or 1
                uiStroke.Enabled = not value
            elseif index == "Visible" then
                squareFrame.Visible = value
            elseif index == "ZIndex" then
                squareFrame.ZIndex = value
            elseif index == "Transparency" then
                local transparency = convertTransparency(value)
                squareFrame.BackgroundTransparency = squareObj.Filled and transparency or 1
                uiStroke.Transparency = transparency
            elseif index == "Color" then
                squareFrame.BackgroundColor3 = value
                uiStroke.Color = value
            end
        end,
        __index = function(self, index)
            if index == "Remove" or index == "Destroy" then
                return function()
                    squareFrame:Destroy()
                    squareObj.Remove(self)
                    for k in pairs(bs) do
                        bs[k] = nil
                    end
                end
            end
            return squareObj[index]
        end
    })
	elseif drawingType == "Image" then
    local imageObj = {
        Data = "",
        DataURL = "rbxassetid://0",
        Size = Vector2.zero,
        Position = Vector2.zero
    }

    for key, value in pairs(baseDrawingObj) do
        imageObj[key] = value
    end

    local imageFrame = Instance.new("ImageLabel")
    imageFrame.Name = drawingIndex
    imageFrame.BorderSizePixel = 0
    imageFrame.ScaleType = Enum.ScaleType.Stretch
    imageFrame.BackgroundTransparency = 1
    imageFrame.Visible = imageObj.Visible
    imageFrame.ZIndex = imageObj.ZIndex
    imageFrame.ImageTransparency = convertTransparency(imageObj.Transparency)
    imageFrame.ImageColor3 = imageObj.Color
    imageFrame.Image = imageObj.DataURL
    imageFrame.Size = UDim2.fromOffset(imageObj.Size.X, imageObj.Size.Y)
    imageFrame.Position = UDim2.fromOffset(imageObj.Position.X, imageObj.Position.Y)

    imageFrame.Parent = drawingUI

    local bs = table.create(0)
    table.insert(drawings, bs)

    return setmetatable(bs, {
        __newindex = function(_, index, value)
            if imageObj[index] == nil then return end

            imageObj[index] = value
            if index == "Data" then
            --We can use it with getcustommasset
            elseif index == "DataURL" then
                imageFrame.Image = value
            elseif index == "Size" then
                imageFrame.Size = UDim2.fromOffset(value.X, value.Y)
            elseif index == "Position" then
                imageFrame.Position = UDim2.fromOffset(value.X, value.Y)
            elseif index == "Visible" then
                imageFrame.Visible = value
            elseif index == "ZIndex" then
                imageFrame.ZIndex = value
            elseif index == "Transparency" then
                imageFrame.ImageTransparency = convertTransparency(value)
            elseif index == "Color" then
                imageFrame.ImageColor3 = value
            end
        end,
        __index = function(self, index)
            if index == "Remove" or index == "Destroy" then
                return function()
                    imageFrame:Destroy()
                    imageObj.Remove(self)
                    for k in pairs(bs) do
                        bs[k] = nil
                    end
                end
            elseif index == "Data" then
                return nil --We can use it with getcustommasset
            end
            return imageObj[index]
        end
    })
	elseif drawingType == "Quad" then
    local quadObj = {
        PointA = Vector2.zero,
        PointB = Vector2.zero,
        PointC = Vector2.zero,
        PointD = Vector2.zero,
        Thickness = 1,
        Filled = false
    }

    for key, value in pairs(baseDrawingObj) do
        quadObj[key] = value
    end

    local _linePoints = {
        A = DrawingLib.new("Line"),
        B = DrawingLib.new("Line"),
        C = DrawingLib.new("Line"),
        D = DrawingLib.new("Line")
    }

    local bs = {}
    table.insert(drawings, bs)

    local function updateLines()
        _linePoints.A.From = quadObj.PointA
        _linePoints.A.To = quadObj.PointB
        _linePoints.B.From = quadObj.PointB
        _linePoints.B.To = quadObj.PointC
        _linePoints.C.From = quadObj.PointC
        _linePoints.C.To = quadObj.PointD
        _linePoints.D.From = quadObj.PointD
        _linePoints.D.To = quadObj.PointA
    end

    updateLines()

    return setmetatable(bs, {
        __newindex = function(_, index, value)
            if quadObj[index] == nil then return end

            quadObj[index] = value
            if index == "PointA" or index == "PointB" or index == "PointC" or index == "PointD" then
                updateLines()
            elseif index == "Thickness" or index == "Visible" or index == "Color" or index == "ZIndex" then
                for _, linePoint in pairs(_linePoints) do
                    linePoint[index] = value
                end
            elseif index == "Filled" then
			--I didnt make that
            end
        end,
        __index = function(self, index)
            if index == "Remove" or index == "Destroy" then
                return function()
                    for _, linePoint in pairs(_linePoints) do
                        linePoint:Destroy()
                    end
                    quadObj.Remove(self)
                    for k in pairs(bs) do
                        bs[k] = nil
                    end
                end
            end
            return quadObj[index]
        end
    })

elseif drawingType == "Triangle" then
    local triangleObj = {
        PointA = Vector2.zero,
        PointB = Vector2.zero,
        PointC = Vector2.zero,
        Thickness = 1,
        Filled = false
    }

    for key, value in pairs(baseDrawingObj) do
        triangleObj[key] = value
    end

    local _linePoints = {
        A = DrawingLib.new("Line"),
        B = DrawingLib.new("Line"),
        C = DrawingLib.new("Line")
    }

    local bs = {}
    table.insert(drawings, bs)

    local function updateLines()
        _linePoints.A.From = triangleObj.PointA
        _linePoints.A.To = triangleObj.PointB
        _linePoints.B.From = triangleObj.PointB
        _linePoints.B.To = triangleObj.PointC
        _linePoints.C.From = triangleObj.PointC
        _linePoints.C.To = triangleObj.PointA
    end

    updateLines()

    return setmetatable(bs, {
        __newindex = function(_, index, value)
            if triangleObj[index] == nil then return end

            triangleObj[index] = value
            if index == "PointA" or index == "PointB" or index == "PointC" then
                updateLines()
            elseif index == "Thickness" or index == "Visible" or index == "Color" or index == "ZIndex" then
                for _, linePoint in pairs(_linePoints) do
                    linePoint[index] = value
                end
            elseif index == "Filled" then
                -- Placeholder for future functionality
            end
        end,
        __index = function(self, index)
            if index == "Remove" or index == "Destroy" then
                return function()
                    for _, linePoint in pairs(_linePoints) do
                        linePoint:Destroy()
                    end
                    triangleObj.Remove(self)
                    for k in pairs(bs) do
                        bs[k] = nil
                    end
                end
            end
            return triangleObj[index]
        end
    })
end
end

getgenv()["Drawing"] = DrawingLib
getgenv()["Drawing"]["Fonts"] = {
    ['UI'] = 0,
    ['System'] = 1,
    ['Plex'] = 2,
    ['Monospace'] = 3
}

getgenv()["cleardrawcache"] = newcclosure(function()
    for _, v in pairs(Drawings) do
        v:Remove()
    end
    table.clear(drawings)
end)

getgenv()["clear_draw_cache"] = cleardrawcache
getgenv()["ClearDrawCache"] = cleardrawcache

getgenv()["isrenderobj"] = newcclosure(function(Inst)
    for _, v in pairs(drawings) do
        if v == Inst and type(v) == "table" then
            return true
        end
    end
    return false
end)

getgenv()["is_render_obj"] = isrenderobj
getgenv()["IsRenderObj"] = isrenderobj

getgenv()["getrenderproperty"] = newcclosure(function(a, b)
    return a[b]
end)

getgenv()["get_render_property"] = getrenderproperty
getgenv()["GetRenderProperty"] = getrenderproperty

getgenv()["setrenderproperty"] = newcclosure(function(a, b, c)
    local success, err = pcall(function()
        a[b] = c
    end)
    if not success and err then warn(err) end
end)

getgenv()["set_render_property"] = getrenderproperty
getgenv()["SetRenderProperty"] = setrenderproperty
