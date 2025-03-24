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

getgenv().getscripts = newcclosure(function()
	local returntable = {}
	for i, v in pairs(game:GetDescendants()) do
		if v:IsA("LocalScript") or v:IsA("ModuleScript") then
			table.insert(returntable, v)
		end
	end
	return returntable
end)

getgenv().getnilinstances = nil

getgenv().getsenv = newcclosure(function(script_instance)
	for i, v in pairs(getreg()) do
		if type(v) == "function" then
			if getfenv(v).script == script_instance then
				return getfenv(v)
			end
		end
	end
end)

getgenv().getmodules = newcclosure(function()
	local t = {}
	for i,v in pairs(getinstances()) do
		if v:IsA('ModuleScript') then
			table.insert(t, v)
		end
	end
	return t
end)

getgenv().getloadedmodules = newcclosure(function()
	local t = {}
	for i,v in pairs(getinstances()) do
		if v:IsA('ModuleScript') then
			table.insert(t, v)
		end
	end
	return t
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

local _isscriptable = clonefunction(isscriptable)
local _setscriptable = clonefunction(setscriptable)
local ScriptableCache = {}

--[[
	Returns the value of a hidden property of `object`, which cannot be indexed normally.
	If the property is hidden, the second return value will be `true`. Otherwise, it will be `false`.
]]
getgenv().gethiddenproperty = newcclosure(function(Inst, Idx) 
	assert(typeof(Inst) == "Instance", "invalid argument #1 to 'gethiddenproperty' [Instance expected]", 2);
	local Was = _isscriptable(Inst, Idx);
	_setscriptable(Inst, Idx, true)

	local Value = Inst[Idx]
	_setscriptable(Inst, Idx, Was)

	return Value, not Was
end)

getgenv().get_hidden_property = gethiddenproperty
getgenv().GetHiddenProperty = gethiddenproperty

-- Sets the value of a hidden property of `object`, which cannot be set normally. Returns whether the property was hidden.
getgenv().sethiddenproperty = (function(Inst, Idx, Value) 
	assert(typeof(Inst) == "Instance", "invalid argument #1 to 'sethiddenproperty' [Instance expected]", 2);
	local Was = _isscriptable(Inst, Idx);
	_setscriptable(Inst, Idx, true)

	Inst[Idx] = Value

	_setscriptable(Inst, Idx, Was)

	return not Was
end)

getgenv().set_hidden_property = sethiddenproperty
getgenv().SetHiddenProperty = sethiddenproperty

getgenv().isscriptable = newcclosure(function(Inst: Instance, Property: string)
	local InstanceCache = ScriptableCache[Inst]
	if InstanceCache then
		local Value = InstanceCache[Property]
		if Value ~= nil then
			return Value
		end
	end
	return _isscriptable(Inst, Property)
end)

getgenv().is_scriptable = isscriptable
getgenv().IsScriptable = isscriptable

getgenv().setscriptable = newcclosure(function(Inst: Instance, Property: string, Scriptable: boolean)
	local WasScriptable = _isscriptable(Inst, Property)
	if ScriptableCache[Inst] == nil then
		ScriptableCache[Inst] = {}
	end
	ScriptableCache[Inst][Property] = Scriptable
	return WasScriptable
end)

getgenv().set_scriptable = setscriptable
getgenv().SetScriptable = setscriptable

getgenv().fireclickdetector = newcclosure(function(Target)
	assert(typeof(Target) == "Instance", "invalid argument #1 to 'fireclickdetector' (Instance expected, got " .. type(Target) .. ") ", 2)
	local ClickDetector = Target:FindFirstChild("ClickDetector") or Target
	local PreviousParent = ClickDetector["Parent"]

	local NewPart = Instance.new("Part", getrenv()["workspace"])
	do
		NewPart["Transparency"] = 1
		NewPart["Size"] = Vector3.new(30, 30, 30)
		NewPart["Anchored"] = true
		NewPart["CanCollide"] = false
		getrenv()["task"].delay(15, function()
			if NewPart:IsDescendantOf(getrenv()["game"]) then
				NewPart:Destroy()
			end
		end)
		ClickDetector["Parent"] = NewPart
		ClickDetector["MaxActivationDistance"] = math.huge
	end

	local VirtualUser = getrenv()["game"]:FindService("VirtualUser") or getrenv()["game"]:GetService("VirtualUser")

	local HeartbeatConnection = RunService["Heartbeat"]:Connect(function()
		local CurrentCamera = getrenv()["workspace"]["CurrentCamera"] or getrenv()["workspace"]["Camera"]
		NewPart["CFrame"] = CurrentCamera["CFrame"] * CFrame.new(0, 0, -20) * CFrame.new(CurrentCamera["CFrame"]["LookVector"]['X'], CurrentCamera["CFrame"]["LookVector"]['Y'], CurrentCamera["CFrame"]["LookVector"]['Z'])
		VirtualUser:ClickButton1(Vector2.new(20, 20), CurrentCamera["CFrame"])
	end)

	ClickDetector["MouseClick"]:Once(function()
		HeartbeatConnection:Disconnect()
		ClickDetector["Parent"] = PreviousParent
		NewPart:Destroy()
	end)
end)

getgenv().fire_click_detector = fireclickdetector
getgenv().FireClickDetector = fireclickdetector

-- Dispatches a ProximityPrompt.
getgenv().fireproximityprompt = (function(ProximityPrompt, Amount, Skip)
	assert(typeof(ProximityPrompt) == "Instance", "invalid argument #1 to 'fireproximityprompt' (Instance expected, got " .. typeof(ProximityPrompt) .. ") ", 2)
	assert(ProximityPrompt:IsA("ProximityPrompt"), "invalid argument #1 to 'fireproximityprompt' (ProximityPrompt expected, got " .. ProximityPrompt["ClassName"] .. ") ", 2)

	Amount = Amount or 1
	Skip = Skip or false

	assert(type(Amount) == "number", "invalid argument #2 to 'fireproximityprompt' (number expected, got " .. type(Amount) .. ") ", 2)
	assert(type(Skip) == "boolean", "invalid argument #2 to 'fireproximityprompt' (boolean expected, got " .. type(Amount) .. ") ", 2)

	local OldHoldDuration = ProximityPrompt.HoldDuration
	local OldMaxDistance = ProximityPrompt.MaxActivationDistance

	ProximityPrompt["MaxActivationDistance"] = 9e9
	ProximityPrompt:InputHoldBegin()

	for i = 1, Amount or 1 do
		if Skip then
			ProximityPrompt["HoldDuration"] = 0
		else
			getrenv()["task"].wait(ProximityPrompt["HoldDuration"] + 0.01)
		end
	end

	ProximityPrompt:InputHoldEnd()
	ProximityPrompt["MaxActivationDistance"] = OldMaxDistance
	ProximityPrompt["HoldDuration"] = OldHoldDuration
end)

getgenv().fire_proximity_prompt = fireproximityprompt
getgenv().FireProximityPrompt = fireproximityprompt

getgenv().info = newcclosure(function(...)
	game:GetService('TestService'):Message(table.concat({...}, ' '))
end)

function GetObjects(asset)
	return {
		game:GetService("InsertService"):LoadLocalAsset(asset)
	}
end

local BlacklistedFunctions = {
    "OpenVideosFolder",
    "OpenScreenshotsFolder",
    "GetRobuxBalance",
    "PerformPurchase",
    "PromptBundlePurchase",
    "PromptNativePurchase",
    "PromptProductPurchase",
    "PromptPurchase",
    "PromptGamePassPurchase",
    "PromptRobloxPurchase",
    "PromptThirdPartyPurchase",
    "Publish",
    "GetMessageId",
    "OpenBrowserWindow",
    "OpenNativeOverlay",
    "RequestInternal",
    "ExecuteJavaScript",
    "EmitHybridEvent",
    "AddCoreScriptLocal",
    "HttpRequestAsync",
    "ReportAbuse",
    "SaveScriptProfilingData",
    "OpenUrl",
    "DeleteCapture",
    "DeleteCapturesAsync"
}

local Metatable = getrawmetatable(game)
local OldMetatable = Metatable.__namecall

setreadonly(Metatable, false)
Metatable.__namecall = function(Self, ...)
    local Method = getnamecallmethod()
   
    if table.find(BlacklistedFunctions, Method) then
        warn("Attempt to call dangerous function.")
        return nil
    end

    if Method == "HttpGet" or Method == "HttpGetAsync" then
        return rqst({Url = Url, Method = "GET"}).Body --httpget(...)
    elseif Method == "GetObjects" then 
        return GetObjects(...)
    end

    return OldMetatable(Self, ...)
end

local OldIndex = Metatable.__index

setreadonly(Metatable, false)
Metatable.__index = function(Self, i)
    if table.find(BlacklistedFunctions, i) then
        warn("Attempt to call dangerous function.")
        return nil
    end

    if Self == game then
        if i == "HttpGet" or i == "HttpGetAsync" then 
            return rqst({Url = Url, Method = "GET"}).Body --httpget
        elseif i == "GetObjects" then 
            return GetObjects
        end
    end
    return OldIndex(Self, i)
end

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

local DrawingLib = {}

local Camera = game:GetService("Workspace"):FindFirstChild("Camera")
local RunService = game:GetService("RunService")
local CoreGui = cloneref(game:GetService("CoreGui"))

local BaseDrawingProperties = setmetatable({
	Visible = true,
	Color = Color3.new(),
	Transparency = 0,
	Position, Vector2.new(),
	Remove = newcclosure(function()
	end)
}, {
	__add = newcclosure(function(tbl1, tbl2)
		local new = {}
		for i, v in next, tbl1 do
			new[i] = v
		end
		for i, v in next, tbl2 do
			new[i] = v
		end
		return new
	end)
})

local DrawingUI = nil;

DrawingLib.new = newcclosure(function(Type)
	if DrawingUI == nil then
		DrawingUI = Instance.new("ScreenGui");
		DrawingUI.Parent = CoreGui;
		DrawingUI.Name = "DrawingLib"
		DrawingUI.DisplayOrder = 1999999999
		DrawingUI.IgnoreGuiInset = true
	end

	if (Type == "Line") then
		local LineProperties = ({
			To = Vector2.new(),
			From = Vector2.new(),
			Thickness = 1,
		} + BaseDrawingProperties)

		local LineFrame = Instance.new("Frame");
		LineFrame.AnchorPoint = Vector2.new(0.5, 0.5);
		LineFrame.BorderSizePixel = 0

		LineFrame.BackgroundColor3 = LineProperties.Color
		LineFrame.Visible = LineProperties.Visible
		LineFrame.BackgroundTransparency =  LineProperties.Transparency
		LineFrame.ZIndex=3000

		LineFrame.Parent = DrawingUI

		return setmetatable({}, {
			__newindex = newcclosure(function(self, Property, Value)
				if (Property == "To") then
					local To = Value
					local Direction = (To - LineProperties.From);
					local Center = (To + LineProperties.From) / 2
					local Distance = Direction.Magnitude
					local Theta = math.atan2(Direction.Y, Direction.X);

					LineFrame.Position = UDim2.fromOffset(Center.X, Center.Y);
					LineFrame.Rotation = math.deg(Theta);
					LineFrame.Size = UDim2.fromOffset(Distance, LineProperties.Thickness);

					LineProperties.To = To
				end
				if (Property == "From") then
					local From = Value
					local Direction = (LineProperties.To - From);
					local Center = (LineProperties.To + From) / 2
					local Distance = Direction.Magnitude
					local Theta = math.atan2(Direction.Y, Direction.X);

					LineFrame.Position = UDim2.fromOffset(Center.X, Center.Y);
					LineFrame.Rotation = math.deg(Theta);
					LineFrame.Size = UDim2.fromOffset(Distance, LineProperties.Thickness);


					LineProperties.From = From
				end
				if (Property == "Visible") then
					LineFrame.Visible = Value
					LineProperties.Visible = Value
				end
				if (Property == "Thickness") then
					Value = Value < 1 and 1 or Value

					local Direction = (LineProperties.To - LineProperties.From);
					local Distance = Direction.Magnitude

					LineFrame.Size = UDim2.fromOffset(Distance, Value);

					LineProperties.Thickness = Value
				end
				if (Property == "Transparency") then
					LineFrame.BackgroundTransparency = 1 - Value
					LineProperties.Transparency = 1 - Value
				end
				if (Property == "Color") then
					LineFrame.BackgroundColor3 = Value
					LineProperties.Color = Value 
				end
				if (Property == "ZIndex") then
					LineFrame.ZIndex = Value
				end
			end),
			__index = newcclosure(function(self, Property)
				if (string.lower(tostring(Property)) == "remove") then
					return (function()
						LineFrame:Destroy();
					end)
				end
				if Property == "Destroy" then
					return (function()
						LineFrame:Destroy();
					end)
				end
				return LineProperties[Property]
			end)
		})
	end

	if (Type == "Circle") then
		local CircleProperties = ({
			Radius = 150,
			Filled = false,
			Thickness = 0,
			Position = Vector2.new()
		} + BaseDrawingProperties)

		local CircleFrame = Instance.new("Frame");

		CircleFrame.AnchorPoint = Vector2.new(0.5, 0.5);
		CircleFrame.BorderSizePixel = 0

		CircleFrame.BackgroundColor3 = CircleProperties.Color
		CircleFrame.Visible = CircleProperties.Visible
		CircleFrame.BackgroundTransparency = CircleProperties.Transparency

		local Corner = Instance.new("UICorner", CircleFrame);
		Corner.CornerRadius = UDim.new(1, 0);
		CircleFrame.Size = UDim2.new(0, CircleProperties.Radius, 0, CircleProperties.Radius);

		CircleFrame.Parent = DrawingUI

		local Stroke = Instance.new("UIStroke", CircleFrame)
		Stroke.Thickness = CircleProperties.Thickness
		Stroke.Enabled = true
		Stroke.Transparency = 0

		return setmetatable({}, {
			__newindex = newcclosure(function(self, Property, Value)
				if (Property == "Radius") then
					CircleFrame.Size = UDim2.new(0,Value*2,0,Value*2)
					CircleProperties.Radius = Value
				end
				if (Property == "Position") then
					CircleFrame.Position = UDim2.new(0, Value.X, 0, Value.Y);
					CircleProperties.Position = Value
				end
				if (Property == "Filled") then
					if Value == true then	
						CircleFrame.BackgroundTransparency = CircleProperties.Transparency
						Stroke.Enabled = not Value
						CircleProperties.Filled = Value
					else
						CircleFrame.BackgroundTransparency = (Value == true and 0 or 1)
						Stroke.Enabled = not Value
						CircleProperties.Filled = Value
					end
				end
				if (Property == "Color") then
					CircleFrame.BackgroundColor3 = Value
					Stroke.Color = Value
					CircleProperties.Color = Value
				end
				if (Property == "Thickness") then
					Stroke.Thickness = Value
					CircleProperties.Thickness = Value
				end
				if (Property == "Transparency") then
					CircleFrame.BackgroundTransparency = Value
					CircleProperties.Transparency = Value
				end
				if (Property == "Visible") then
					CircleFrame.Visible = Value
					CircleProperties.Visible = Value
				end
				if (Property == "ZIndex") then
					CircleFrame.ZIndex = Value
				end
			end),
			__index = newcclosure(function(self, Property)
				if (string.lower(tostring(Property)) == "remove") then
					return (function()
						CircleFrame:Destroy();
					end)
				end
				if Property ==  "Destroy" then
					return (function()
						CircleFrame:Destroy();
					end)
				end
				return CircleProperties[Property]
			end)
		})
	end

	if (Type == "Text") then
		local TextProperties = ({
			Text = "",
			Center = false,
			Outline = false,
			OutlineColor = Color3.new(),
			Position = Vector2.new(),
			TextBounds = Vector2.new(),
		} + BaseDrawingProperties)

		local TextLabel = Instance.new("TextLabel");
		TextLabel.AnchorPoint = Vector2.new(0.5,0.5)
		TextLabel.BorderSizePixel = 0
		TextLabel.Font = Enum.Font.SourceSans
		TextLabel.TextSize = 14
		TextLabel.TextXAlignment = Enum.TextXAlignment.Left or Enum.TextXAlignment.Right
		TextLabel.TextYAlignment = Enum.TextYAlignment.Top

		TextLabel.TextColor3 = TextProperties.Color
		TextLabel.Visible = true
		TextLabel.BackgroundTransparency = 1
		TextLabel.TextTransparency = 1 - TextProperties.Transparency

		local Stroke = Instance.new("UIStroke", TextLabel)
		Stroke.Thickness = 0
		Stroke.Enabled = false
		Stroke.Color = TextProperties.OutlineColor
		TextLabel.Parent = DrawingUI

		return setmetatable({}, {
			__newindex = newcclosure(function(self, Property, Value)
				if (Property == "Text") then
					TextLabel.Text = Value
					TextProperties.Text = Value
				end
				if (Property == "Position") then
					TextLabel.Position = UDim2.fromOffset(Value.X, Value.Y);
					TextProperties.Position = Vector2.new(Value.X, Value.Y);
				end
				if (Property == "Size") then
					TextLabel.TextSize = Value
					TextProperties.TextSize = Value
				end
				if (Property == "Color") then
					TextLabel.TextColor3 = Value
					--	Stroke.Color = Value
					TextProperties.Color = Value
				end
				if (Property == "Transparency") then
					TextLabel.TextTransparency = 1 - Value
					Stroke.Transparency = 1 - Value
					TextProperties.Transparency = 1 - Value
				end
				if (Property == "OutlineOpacity") then
					TextLabel.TextStrokeTransparency = Value
					--Stroke.Transparency = Value
				end
				if (Property == "OutlineColor") then
					Stroke.Color = Value
					TextProperties.OutlineColor = Value
				end
				if (Property == "Visible") then
					TextLabel.Visible = Value
					TextProperties.Visible = Value
				end
				if (Property == "Outline") then
					if Value == true then
						Stroke.Thickness = 1
						Stroke.Enabled = Value
					else
						Stroke.Thickness = 0
						Stroke.Enabled = Value
					end
				end
				if (Property == "TextBounds") then
					TextLabel.TextBounds = Vector2.new(TextProperties.Position , Value);
				end
				if (Property == "Center") then
					if Value == true then
						TextLabel.TextXAlignment = Enum.TextXAlignment.Center;
						TextLabel.TextYAlignment = Enum.TextYAlignment.Center;
						TextProperties.Center = Enum.TextYAlignment.Center;
					else
						TextProperties.Center = Value
					end
				end
				if (Property == "ZIndex") then
					TextLabel.ZIndex = Value
				end
			end),
			__index = newcclosure(function(self, Property)
				if (string.lower(tostring(Property)) == "remove") then
					return (function()
						TextLabel:Destroy();
					end)
				end
				if Property == "Destroy" then
					return (function()
						TextLabel:Destroy();
					end)
				end
				return TextProperties[Property]
			end)
		})
	end

	if (Type == "Square") then
		local SquareProperties = ({
			Thickness = 1,
			Size = Vector2.new(),
			Position = Vector2.new(),
			Filled = false,
		} + BaseDrawingProperties);
		local SquareFrame = Instance.new("Frame");

		--SquareFrame.AnchorPoint = Vector2.new(0.5, 0.5);
		SquareFrame.BorderSizePixel = 0

		SquareFrame.Visible = SquareProperties.Visible
		SquareFrame.Parent = DrawingUI

		local Stroke = Instance.new("UIStroke", SquareFrame)
		Stroke.Thickness = 2
		Stroke.Enabled = true
		SquareFrame.BackgroundTransparency = 0
		Stroke.Transparency = 0

		return setmetatable({}, {
			__newindex = newcclosure(function(self, Property, Value)
				if (Property == "Position") then
					SquareFrame.Position = UDim2.fromOffset(Value.X, Value.Y);
					SquareProperties.Position = Value
				end
				if (Property == "Size") then
					SquareFrame.Size = UDim2.new(0, Value.X, 0, Value.Y);
					SquareProperties.Size = Value
				end
				if (Property == "Thickness") then
					Stroke.Thickness = Value
					SquareProperties.Thickness = Value
				end
				if (Property == "Color") then
					SquareFrame.BackgroundColor3 = Value
					Stroke.Color = Value
					SquareProperties.Color = Value
				end
				if (Property == "Transparency") then
					--SquareFrame.BackgroundTransparency = Value
					--	Stroke.Transparency = Value
					SquareProperties.Transparency = Value
				end
				if (Property == "Visible") then
					SquareFrame.Visible = Value
					SquareProperties.Visible = Value
				end
				if (Property == "Filled") then -- requires beta
					if Value == true then	
						SquareFrame.BackgroundTransparency = SquareProperties.Transparency
						Stroke.Transparency = 1
						Stroke.Enabled = not Value
						SquareProperties.Filled = Value
					else
						SquareFrame.BackgroundTransparency = (Value == true and 0 or 1)
						Stroke.Enabled = not Value
						SquareProperties.Filled = Value
					end
				end
			end),
			__index = newcclosure(function(self, Property)
				if (string.lower(tostring(Property)) == "remove") then
					return (function()
						SquareFrame:Destroy();
					end)
				end
				if Property == "Destroy" then				
					return (function()
						SquareFrame:Destroy();
					end)
				end
				return SquareProperties[Property]
			end)
		})
	end

	if (Type == "Image") then
		local ImageProperties = ({
			Data = "rbxassetid://848623155", -- roblox assets only rn
			Size = Vector2.new(),
			Position = Vector2.new(),
			Rounding = 0,
			Color = Color3.new(),
		});

		local ImageLabel = Instance.new("ImageLabel");

		ImageLabel.BorderSizePixel = 0
		ImageLabel.ScaleType = Enum.ScaleType.Stretch
		ImageLabel.Transparency = 1

		ImageLabel.ImageColor3 = ImageProperties.Color
		ImageLabel.Visible = false
		ImageLabel.Parent = DrawingUI

		return setmetatable({}, {
			__newindex = newcclosure(function(self, Property, Value)
				if (Property == "Size") then
					ImageLabel.Size = UDim2.new(0, Value.X, 0, Value.Y);
					ImageProperties.Text = Value
				end
				if (Property == "Position") then
					ImageLabel.Position = UDim2.new(0, Value.X, 0, Value.Y);
					ImageProperties.Position = Value
				end
				if (Property == "Size") then
					ImageLabel.Size = UDim2.new(0, Value.X, 0, Value.Y);
					ImageProperties.Size = Value
				end
				if (Property == "Transparency") then
					ImageLabel.ImageTransparency = math.clamp(1-Value,0,1)
					ImageProperties.Transparency = math.clamp(1-Value,0,1)
				end
				if (Property == "Visible") then
					ImageLabel.Visible = Value
					ImageProperties.Visible = Value
				end
				if (Property == "Color") then
					ImageLabel.ImageColor3 = Value
					ImageProperties.Color = Value
				end
				if (Property == "Data") then
					ImageLabel.Image = Value
					ImageProperties.Data = Value
				end
				if (Property == "ZIndex") then
					ImageLabel.ZIndex = Value
				end
			end),
			__index = newcclosure(function(self, Property)
				if (string.lower(tostring(Property)) == "remove") then
					return (function()
						ImageLabel:Destroy();
					end)
				end
				if Property ==  "Destroy" then
					return (function()
						ImageLabel:Destroy();
					end)
				end
				return ImageProperties[Property]
			end)
		})
	end

	if (Type == "Quad") then -- idk if this will work lmao
		local QuadProperties = ({
			Thickness = 1,
			Transparency = 1,	
			Color = Color3.new(),
			PointA = Vector2.new();
			PointB = Vector2.new();
			PointC = Vector2.new();
			PointD = Vector2.new();
			Filled = false;
		}  + BaseDrawingProperties);

		local PointA = DrawingLib.new("Line")
		local PointB = DrawingLib.new("Line")
		local PointC = DrawingLib.new("Line")
		local PointD = DrawingLib.new("Line")

		return setmetatable({}, {
			__newindex = newcclosure(function(self, Property, Value)
				if Property == "Thickness" then
					PointA.Thickness = Value
					PointB.Thickness = Value
					PointC.Thickness = Value
					PointD.Thickness = Value
					QuadProperties.Thickness = Value
				end
				if Property == "PointA" then
					PointA.From = Value
					PointB.To = Value
				end
				if Property == "PointB" then
					PointB.From = Value
					PointC.To = Value
				end
				if Property == "PointC" then
					PointC.From = Value
					PointD.To = Value
				end
				if Property == "PointD" then
					PointD.From = Value
					PointA.To = Value
				end
				if Property == "Filled" then
					-- i'll do this later
				end
				if Property == "Color" then
					PointA.Color = Value
					PointB.Color = Value
					PointC.Color = Value
					PointD.Color = Value
					QuadProperties.Color = Value
				end
				if Property == "Transparency" then
					PointA.Transparency = Value
					PointB.Transparency = Value
					PointC.Transparency = Value
					PointD.Transparency = Value
					QuadProperties.Transparency = Value
				end
				if Property == "Visible" then
					PointA.Visible = Value
					PointB.Visible = Value
					PointC.Visible = Value
					PointD.Visible = Value
					QuadProperties.Visible = Value
				end
				if (Property == "ZIndex") then
					PointA.ZIndex = Value
					PointB.ZIndex = Value
					PointC.ZIndex = Value
					PointD.ZIndex = Value
					QuadProperties.ZIndex = Value
				end
			end),
			__index = newcclosure(function(self, Property)
				if (string.lower(tostring(Property)) == "remove") then
					return (function()
						PointA:Remove();
						PointB:Remove();
						PointC:Remove();
						PointD:Remove();
					end)
				end
				if Property ==  "Destroy" then
					return (function()
						PointA:Remove();
						PointB:Remove();
						PointC:Remove();
						PointD:Remove();
					end)
				end
				return QuadProperties[Property]
			end)
		});
	end

	if (Type == "Triangle") then  -- idk if this will work lmao
		local TriangleProperties = ({
			Thickness = 1,
			Transparency = 1,	
			Color = Color3.new(),
			PointA = Vector2.new();
			PointB = Vector2.new();
			PointC = Vector2.new();
			PointD = Vector2.new();
			Filled = false;
		}  + BaseDrawingProperties);

		local PointA = DrawingLib.new("Line")
		local PointB = DrawingLib.new("Line")
		local PointC = DrawingLib.new("Line")

		return setmetatable({}, {
			__newindex = newcclosure(function(self, Property, Value)
				if Property == "Thickness" then
					PointA.Thickness = Value
					PointB.Thickness = Value
					PointC.Thickness = Value
					PointD.Thickness = Value
					TriangleProperties.Thickness = Value
				end
				if Property == "PointA" then
					PointA.From = Value
					PointB.To = Value
				end
				if Property == "PointB" then
					PointB.From = Value
					PointC.To = Value
				end
				if Property == "PointC" then
					PointC.From = Value
					PointD.To = Value
				end
				if Property == "PointD" then
					PointD.From = Value
					PointA.To = Value
				end
				if Property == "Filled" then
					-- i'll do this later
				end
				if Property == "Color" then
					PointA.Color = Value
					PointB.Color = Value
					PointC.Color = Value
					PointD.Color = Value
					TriangleProperties.Color = Value
				end
				if Property == "Transparency" then
					PointA.Transparency = Value
					PointB.Transparency = Value
					PointC.Transparency = Value
					PointD.Transparency = Value
					TriangleProperties.Transparency = Value
				end
				if Property == "Visible" then
					PointA.Visible = Value
					PointB.Visible = Value
					PointC.Visible = Value
					PointD.Visible = Value
					TriangleProperties.Visible = Value
				end
				if (Property == "ZIndex") then
					PointA.ZIndex = Value
					PointB.ZIndex = Value
					PointC.ZIndex = Value
					PointD.ZIndex = Value
					TriangleProperties.ZIndex = Value
				end
			end),
			__index = newcclosure(function(self, Property)
				if (string.lower(tostring(Property)) == "remove") then
					return (function()
						PointA:Remove();
						PointB:Remove();
						PointC:Remove();
					end)
				end
				if Property ==  "Destroy" then
					return (function()
						PointA:Remove();
						PointB:Remove();
						PointC:Remove();
					end)
				end
				return TriangleProperties[Property]
			end)
		});
	end
end)


DrawingLib.clear = newcclosure(function() 
	DrawingUI:ClearAllChildren();
end)

if RunService:IsStudio() then
	return DrawingLib
else
	if getgenv then
		getgenv()["Drawing"] = DrawingLib
		getgenv()["clear_drawing_lib"] = DrawingLib.clear
		Drawing = drawing
	else
		Drawing = DrawingLib
	end
end
getgenv()["Drawing"] = DrawingLib
getgenv()["clear_drawing_lib"] = DrawingLib.clear
getgenv().Drawing = DrawingLib
getgenv().clear_drawing_lib = DrawingLib.clear

Drawing.Fonts = {}

Drawing.Fonts.UI = 0
Drawing.Fonts.System = 1
Drawing.Fonts.Plex = 2
Drawing.Fonts.Monospace = 3

getgenv().cleardrawcache = DrawingLib.clear
Drawing.cleardrawcache = DrawingLib.clear
local rendered = false
getgenv().isrenderobj = newcclosure(function(a, drawing2)
	if rendered == true then
		return false
	end

	rendered = true
	return true
end)

getgenv().setrenderproperty = newcclosure(function(drawing, object, value)
	drawing[object] = value
	return object
end)

getgenv().getrenderproperty = function(drawing, object)
	local value = drawing[object]
	return value
end
