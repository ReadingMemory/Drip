local ScreenGui = Instance.new("ScreenGui", game:GetService("CoreGui"))
ScreenGui.ResetOnSpawn = false
ScreenGui.ZIndexBehavior = Enum.ZIndexBehavior.Sibling

local Frame = Instance.new("Frame")
local TextBox = Instance.new("TextBox")
local ExecuteButton = Instance.new("TextButton")

Frame.Size = UDim2.new(0, 300, 0, 200)
Frame.Position = UDim2.new(0.5, -150, 0.5, -100)
Frame.BackgroundColor3 = Color3.fromRGB(50, 50, 50)
Frame.Parent = ScreenGui
Frame.Active = true
Frame.Draggable = true

TextBox.Size = UDim2.new(1, -10, 0.7, -10)
TextBox.Position = UDim2.new(0, 5, 0, 5)
TextBox.Text = "Enter script here"
TextBox.Parent = Frame
TextBox.BackgroundColor3 = Color3.fromRGB(255, 255, 255)
TextBox.TextColor3 = Color3.fromRGB(0, 0, 0)

ExecuteButton.Size = UDim2.new(1, -10, 0.2, -10)
ExecuteButton.Position = UDim2.new(0, 5, 0.8, 0)
ExecuteButton.Text = "Execute"
ExecuteButton.Parent = Frame
ExecuteButton.BackgroundColor3 = Color3.fromRGB(70, 70, 70)
ExecuteButton.TextColor3 = Color3.fromRGB(255, 255, 255)

ExecuteButton.MouseButton1Click:Connect(function()
	local scriptText = TextBox.Text
	local func, err = loadstring(scriptText)
	if func then
		func()
	else
		warn("Error executing script: " .. err)
	end
end)
