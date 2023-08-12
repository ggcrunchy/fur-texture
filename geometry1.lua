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
local asin = math.asin
local floor = math.floor
local ipairs = ipairs
local max = math.max
local min = math.min
local pairs = pairs
local pi = math.pi
local random = math.random
local sin = math.sin
local sqrt = math.sqrt

-- Modules --
local widget = require("widget")

-- Plugins --
local memoryBitmap = require("plugin.memoryBitmap")

-- Solar2D globals --
local display = display

--
--
--

display.setDefault("background", .275)

--
--
--

local function Quantize (pos, dim)
  return floor(pos / dim)
end

local NCols, NRows = 100, 100

local function Visit (col, row, func, arg1, arg2)
  if col >= 1 and col <= NCols and row >= 1 and row <= NRows then
    func(col, row, arg1, arg2)
  end
end

local CellWidth, CellHeight = 4, 4

local function Raycast (x1, y1, x2, y2, func, arg1, arg2)
  local coff, roff = Quantize(x1, CellWidth), Quantize(y1, CellHeight)
  local x0, c1 = coff * CellWidth, coff + 1 
  local y0, r1 = roff * CellHeight, roff + 1
  local c2, r2 = Quantize(x2, CellWidth) + 1, Quantize(y2, CellHeight) + 1
  local cstep, rstep = c1 < c2 and 1 or -1, r1 < r2 and 1 or -1

  if c1 == c2 then -- vertical
    for row = r1, r2, rstep do
      Visit(c1, row, func, arg1, arg2)
    end
  elseif r1 == r2 then -- horizontal
    for col = c1, c2, cstep do
      Visit(col, r1, func, arg1, arg2)
    end
  else
    local dc, dr = abs(c1 - c2), abs(r1 - r2)

    if dc == dr then -- diagonal
      for _ = 1, dc do
        Visit(c1, r1, func, arg1, arg2)

        c1, r1 = c1 + cstep, r1 + rstep
      end
    elseif dc < dr then -- more tall than wide
      local xoff, dx, dy = x1 - x0, abs(x2 - x1), y2 - y1
      local t, m = c1 < c2 and CellWidth - xoff or xoff, dy / dx

      -- Columns, first through penultimate:
      for col = c1, c2 - cstep, cstep do
        local rto = Quantize(y1 + t * m, CellHeight) + 1

        for row = r1, rto, rstep do
          Visit(col, row, func, arg1, arg2)
        end

        r1, t = rto, t + CellWidth
      end

      -- Last column:
      for row = r1, r2, rstep do
        Visit(c2, row, func, arg1, arg2)
      end
    else -- more wide than tall
      local yoff, dx, dy = y1 - y0, x2 - x1, abs(y2 - y1)
      local t, m = r1 < r2 and CellHeight - yoff or yoff, dx / dy

      -- Rows, first through penultimate:
      for row = r1, r2 - rstep, rstep do
        local cto = Quantize(x1 + t * m, CellWidth) + 1

        for col = c1, cto, cstep do
          Visit(col, row, func, arg1, arg2)
        end

        c1, t = cto, t + CellHeight
      end

      -- Last row:
      for col = c1, c2, cstep do
        Visit(col, r2, func, arg1, arg2)
      end
    end
  end
end

--
--
--

local RowInfo = {}

local Tex = memoryBitmap.newTexture{ width = NCols, height = NRows }

local function Paint (col, row)
  Tex:setPixel(col, row, 1, 0, 0, 1)

  local info = RowInfo[row]

  if info then
  --  info.left, info.right = min(col, info.left), max(col, info.right)
    for i = 1, #info do
      if col <= info[i] then
        if col < info[i] then
          table.insert(info, i, col)
        end
        
        return
      end
    end
    
    info[#info + 1] = col

  else
    RowInfo[row] = { col }--left = col, right = col }
  end
end

--
--
--

local Left, Top = 50, 100

local Image = display.newImageRect(Tex.filename, Tex.baseDir, Tex.width * CellWidth, Tex.height * CellHeight)

Image:setStrokeColor(0, 0, 1)

Image.anchorX, Image.x = 0, Left
Image.anchorY, Image.y = 0, Top
Image.strokeWidth = 2

Image:addEventListener("touch", function(event)
  local phase, target = event.phase, event.target

  if phase == "began" then
    display.getCurrentStage():setFocus(target)

    local x, y = event.x - Left, event.y - Top

    Raycast(x, y, x, y, Paint) -- trivial cast
    Tex:invalidate()

    target.last_x, target.last_y = event.x, event.y
    target.is_touched = true
  elseif target.is_touched then
    if phase == "moved" then
      Raycast(target.last_x - Left, target.last_y - Top, event.x - Left, event.y - Top, Paint)
      Tex:invalidate()

      target.last_x, target.last_y = event.x, event.y
    elseif phase == "ended" or phase == "cancelled" then
      display.getCurrentStage():setFocus(nil)

      target.is_touched = false
    end
  else -- swipe?
    return
  end

  return true
end)

--
--
--

-- For the following, see http://www.plunk.org/~hatch/rightway.html

local function AngleBetween (dot, vmw, vpw)
  if dot < 0 then
    return pi - 2 * asin(vpw / 2)
  else
    return 2 * asin(vmw / 2)
  end
end

local function SinXOverX (x)
  if 1 + x * x == 1 then
    return 1
  else
    return sin(x) / x
  end
end

local function Slerp (vx, vy, wx, wy, angle, t)
  local s = 1 - t
  local u = s * SinXOverX(s * angle)
  local v = t * SinXOverX(t * angle)

  return u * vx + v * wx, u * vy + v * wy
end

--
--
--

local function Length (vx, vy, vz)
  return sqrt(vx * vx + vy * vy + vz * vz)
end

local function GetAngle (vx, vy, vz, wx, wy, wz)
  local dot = vx * wx + vy * wy + vz * wz
  local vmw = Length(vx - wx, vy - wy, vz - wz)
  local vpw = Length(vx + wx, vy + wy, vz + wz)

  return AngleBetween(dot, vmw, vpw)
end

local function RandomAxis (c, l, r)
  local ax, ay, az = random(-40, 40) / 50, random(-40, 40) / 50, random(30, 50) / 50
  
  if c then
    ax = (c + random(-3, 3) - (l + r) / 2) / (r - l)
  end
  
  local alen = Length(ax, ay, az)

  return ax / alen, ay / alen, az / alen
end

--
--
--

local Densities = { 80, 75, 90 }

local function GetAxes (c, l, r) -- can add angle limit and constrain w 
  local vx, vy, vz = RandomAxis(c, l, r)
  local wx, wy, wz = RandomAxis()
  local angle = GetAngle(vx, vy, vz, wx, wy, wz)

  if abs(angle) < pi / 8 then
    return GetAxes(c, l, r)
  end

  local sina = SinXOverX(angle)

  vx, vy = vx / sina, vy / sina
  wx, wy = wx / sina, wy / sina

  return angle, vx, vy, wx, wy
end

local WW = 5

local Hairs = {}

widget.newButton{
  left = Image.contentBounds.xMax + 20,
  top = Image.contentBounds.yMin,
  label = "Build",

  onEvent = function(event)
    if event.phase == "ended" then
      for row, edges in pairs(RowInfo) do
        local left, prev = true, edges[1]

        for i = 2, #edges do
          local cur = edges[i]
          
          if cur ~= prev + 1 then
            left = not left
          elseif left then
            edges[i] = false
          else
            edges[i - 1] = false
          end
          
          prev = cur
        end
        
        for i = #edges, 1, -1 do
          if not edges[i] then
            table.remove(edges, i)
          end
        end
        
        if #edges % 2 ~= 0 then
          table.remove(edges)
        end
      end
      
      for _, density in ipairs(Densities) do
        for row, edges in pairs(RowInfo) do
          for ii = 1, #edges, 2 do
          for col = edges[ii], edges[ii + 1] do--edges.left, edges.right do
            if random(100) < density then
              local angle, vx, vy, wx, wy = GetAxes(col, edges[ii], edges[ii + 1])--edges.left, edges.right)
              local hair = {
                angle = angle,
                dx = random(-2, 2),
                dy = random(-2, 2),
                vx = vx, vy = vy,
                wx = wx, wy = wy,
                row = row, col = col,
                r = random(), g = random(), b = random(),
                speed = random(5, 8) / 5,
                t = 0
              }
 
              local n = random(3, 5)
              local R, G, B = .7, .3, .2
              
              for i = 1, n do
                hair[i] = display.newRect(0, 0, WW, WW)
              --  hair[i].alpha=.6
                
                local tb = (i - 1) / n
                local tt = i / n
                hair[i].blendMode="screen"
              --  hair[i]:setFillColor(.7, .3, .2)
                local sb = 1 - tb * .9
                local st = 1 - tt * .9
              
                hair[i]:setFillVertexColor(1, R, G, B, st)--R * st, G * st, B * st)
                hair[i]:setFillVertexColor(4, R, G, B, st)--, R * st, G * st, B * st)
                hair[i]:setFillVertexColor(2, R, G, B, sb)--, R * sb, G * sb, B * sb)
                hair[i]:setFillVertexColor(3, R, G, B, sb)--, R * sb, G * sb, B * sb)
                -- 1, 4: top color
                -- 2, 3: bottom
              end
              
              Hairs[#Hairs + 1] = hair
            end
          end
          end
        end
      end
    end
  end
}

local function GetDisp (ex, ey, cx, cy)
  return cx - ex, cy - ey
end

local I, N = 0, 7

timer.performWithDelay(75, function(event)
  local ibounds = Image.contentBounds
  
  for i = I + 1, #Hairs, N do
    local hair = Hairs[i]

    display.remove(hair.line)

    local x1, y1 = ibounds.xMin + hair.col * CellWidth + hair.dx, ibounds.yMin + hair.row * CellHeight + hair.dy

    local n = #hair
    local w1 = 2
    local w2 = .5

    local lx1, ly1 = x1 - w1 / 2, y1
    local rx1, ry1 = x1 + w1 / 2, y1

    local t = hair.t

    for j = 1, n do
      local tt = t % 2
      
      if tt > 1 then
        tt = 2 - tt
      end
      
      local dx, dy = Slerp(hair.vx, hair.vy, hair.wx, hair.wy, hair.angle, tt)
      local x2, y2 = x1 + dx * 3.5, y1 + dy * 3.5
      
      local mx, my = (x1 + x2) / 2, (y1 + y2) / 2
      
      local w = w1 + j * (w2 - w1) / n
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