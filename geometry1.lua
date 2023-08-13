--- Fur test, as geometry.

--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--
-- [ MIT license: http://www.opensource.org/licenses/mit-license.php ]
--

-- Standard library imports --
local abs = math.abs
local insert = table.insert
local pairs = pairs
local pi = math.pi
local random = math.random
local remove = table.remove

-- Plugins --
local memoryBitmap = require("plugin.memoryBitmap")

-- Modules --
local controls = require("controls")
local math_utils = require("math_utils")

-- Solar2D globals --
local display = display

-- Solar2D modules --
local widget = require("widget")

--
--
--

display.setDefault("background", .275)

--
--
--

local NCols, NRows = 100, 100

--
--
--

local CellWidth, CellHeight = 4, 4

--
--
--

local Raycast = controls.MakeRaycaster(NCols, NRows, CellWidth, CellHeight)

--
--
--

local Tex = memoryBitmap.newTexture{ width = NCols, height = NRows }

--
--
--

local RowInfo = {}

local function Paint (col, row)
  Tex:setPixel(col, row, 1, 0, 0, 1)

  local info = RowInfo[row] or {}

  for i = 1, #info do
    if col <= info[i] then
      if col < info[i] then
        insert(info, i, col)
      end
      
      return
    end
  end
  
  info[#info + 1], RowInfo[row] = col, info
end

--
--
--

local Image = controls.PaintObject(Tex, CellWidth, CellHeight, Raycast, Paint)

Image:setStrokeColor(0, 0, 1)

Image.anchorX, Image.x = 0, 50
Image.anchorY, Image.y = 0, 100
Image.strokeWidth = 2

--
--
--

local function RandomAxis (c, l, r)
  local ax, ay, az = random(-40, 40) / 50, random(-40, 40) / 50, random(30, 50) / 50
  
  if c then
    ax = (c + random(-3, 3) - (l + r) / 2) / (r - l)
  end
  
  local alen = math_utils.Length(ax, ay, az)

  return ax / alen, ay / alen, az / alen
end

--
--
--

local function GetAxes (c, l, r) -- can add angle limit and constrain w 
  local vx, vy, vz = RandomAxis(c, l, r)
  local wx, wy, wz = RandomAxis()
  local angle = math_utils.GetAngle(vx, vy, vz, wx, wy, wz)

  if abs(angle) < pi / 8 then
    return GetAxes(c, l, r)
  end

  local sina = math_utils.SinXOverX(angle)

  vx, vy = vx / sina, vy / sina
  wx, wy = wx / sina, wy / sina

  return angle, vx, vy, wx, wy
end

--
--
--

local function GetIntervals ()
  for _, edges in pairs(RowInfo) do
    local left, prev = true, edges[1]

    for i = 2, #edges do
      local cur = edges[i]
      
      if cur ~= prev + 1 then
        left = not left
      elseif left then
        edges[i] = false -- only keep the leftmost one
      else
        edges[i - 1] = false -- only keep the rightmost one
      end
      
      prev = cur
    end
    
    for i = #edges, 1, -1 do
      if not edges[i] then
        remove(edges, i)
      end
    end
    
    if #edges % 2 ~= 0 then
      remove(edges)
    end
  end
end

--
--
--

local Densities = { 80, 75, 90 }

--
--
--

local function DoRows (func)
  for row, edges in pairs(RowInfo) do
    for i = 1, #edges, 2 do
      local left, right = edges[i], edges[i + 1]

      for col = left, right do
        for j = 1, #Densities do
          if random(100) < Densities[j] then
            func(row, col, left, right)
          end
        end
      end
    end
  end
end

--
--
--

local R, G, B = .7, .3, .2

local WW = 5

local Hairs = {}

--
--
--

local function AddHair (row, col, left, right)
  local angle, vx, vy, wx, wy = GetAxes(col, left, right)
  local hair = {
    angle = angle,
    dx = random(-2, 2),
    dy = random(-2, 2),
    vx = vx, vy = vy,
    wx = wx, wy = wy,
    row = row, col = col,
    speed = random(5, 8) / 5,
    t = 0
  }

  local n = random(3, 5)
  
  for i = 1, n do
    hair[i] = display.newRect(0, 0, WW, WW)
    
    local tb = (i - 1) / n
    local tt = i / n

    hair[i].blendMode = "screen"

    local sb = 1 - tb * .9
    local st = 1 - tt * .9
  
    hair[i]:setFillVertexColor(1, R, G, B, st)
    hair[i]:setFillVertexColor(4, R, G, B, st)
    hair[i]:setFillVertexColor(2, R, G, B, sb)
    hair[i]:setFillVertexColor(3, R, G, B, sb)
    -- 1, 4: top color
    -- 2, 3: bottom
  end
  
  Hairs[#Hairs + 1] = hair
end

--
--
--

widget.newButton{
  left = Image.contentBounds.xMax + 20,
  top = Image.contentBounds.yMin,
  label = "Build",

  onEvent = function(event)
    if event.phase == "ended" then
      GetIntervals()
      DoRows(AddHair)
    end
  end
}

local function GetDisp (ex, ey, cx, cy)
  return cx - ex, cy - ey
end

local function PingPongUnit (t)
  t = t % 2
      
  if t > 1 then
    return 2 - t
  else
    return t
  end
end

local I, N = 0, 7

local MaxWidth, MinWidth = 2, .5
local HairPartLength = 3.5

timer.performWithDelay(75, function()
  local ibounds = Image.contentBounds
  
  for i = I + 1, #Hairs, N do
    local hair = Hairs[i]

    display.remove(hair.line)

    local x1, y1 = ibounds.xMin + hair.col * CellWidth + hair.dx, ibounds.yMin + hair.row * CellHeight + hair.dy

    local n = #hair

    local lx1, ly1 = x1 - MaxWidth / 2, y1
    local rx1, ry1 = x1 + MaxWidth / 2, y1

    local t = hair.t

    for j = 1, n do
      local dx, dy = math_utils.Slerp(hair.vx, hair.vy, hair.wx, hair.wy, hair.angle, PingPongUnit(t))
      local x2, y2 = x1 + dx * HairPartLength, y1 + dy * HairPartLength
      
      local mx, my = (x1 + x2) / 2, (y1 + y2) / 2
      
      local w = MaxWidth + j * (MinWidth - MaxWidth) / n
      local nx, ny = -dy * w / 2, dx * w / 2
      
      local r = hair[j]
      
      local lx2, ly2 = x2 - nx, y2 - ny
      local rx2, ry2 = x2 + nx, y2 + ny
      
      local hpath = r.path
      
      r.x, r.y = mx, my
      
      hpath.x1, hpath.y1 = GetDisp(mx - WW / 2, my - WW / 2, lx2, ly2)
      hpath.x2, hpath.y2 = GetDisp(mx - WW / 2, my + WW / 2, lx1, ly1)
      hpath.x3, hpath.y3 = GetDisp(mx + WW / 2, my + WW / 2, rx1, ry1)
      hpath.x4, hpath.y4 = GetDisp(mx + WW / 2, my - WW / 2, rx2, ry2)
      
      lx1, ly1 = lx2, ly2
      rx1, ry1 = rx2, ry2

      x1, y1 = x2, y2
      t = t + 2.725 * hair.speed
    end
    
    hair.t = t
  end

  I = (I + 1) % N
end, 0)