print("Script started") -- Check if script is running at all

local success, err = pcall(function()
    local ScreenGui = Instance.new("ScreenGui")
    ScreenGui.ResetOnSpawn = false
    ScreenGui.ZIndexBehavior = Enum.ZIndexBehavior.Sibling
    ScreenGui.Parent = game:GetService("CoreGui") -- Assuming you have access

    print("ScreenGui created and parented to CoreGui")

    local Frame = Instance.new("Frame")
    Frame.Size = UDim2.new(0, 300, 0, 200)
    Frame.Position = UDim2.new(0.5, -150, 0.5, -100)
    Frame.BackgroundColor3 = Color3.fromRGB(50, 50, 50)
    Frame.Active = true
    Frame.Draggable = true
    Frame.Parent = ScreenGui

    print("Frame created and parented")

    local TextBox = Instance.new("TextBox")
    TextBox.Size = UDim2.new(1, -10, 0.7, -10)
    TextBox.Position = UDim2.new(0, 5, 0, 5)
    TextBox.Text = "Enter script here"
    TextBox.BackgroundColor3 = Color3.fromRGB(255, 255, 255)
    TextBox.TextColor3 = Color3.fromRGB(0, 0, 0)
    TextBox.Parent = Frame

    print("TextBox created and parented")

    local ExecuteButton = Instance.new("TextButton")
    ExecuteButton.Size = UDim2.new(1, -10, 0.2, -10)
    ExecuteButton.Position = UDim2.new(0, 5, 0.8, 0)
    ExecuteButton.Text = "Execute"
    ExecuteButton.BackgroundColor3 = Color3.fromRGB(70, 70, 70)
    ExecuteButton.TextColor3 = Color3.fromRGB(255, 255, 255)
    ExecuteButton.Parent = Frame

    print("ExecuteButton created and parented")

    ExecuteButton.MouseButton1Click:Connect(function()
        print("Execute button clicked")
        local scriptText = TextBox.Text
        print("Script entered:", scriptText)
        
        local func, err = loadstring(scriptText)
        if func then
            print("Executing script...")
            func() -- Run the script
        else
            warn("Error in script: " .. err)
        end
    end)

    print("Script finished successfully")
end)

if not success then
    warn("Error in GUI creation: " .. err)
end
