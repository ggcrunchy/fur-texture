--- Fur test, with separate low and range texures.

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
    info.left, info.right = min(col, info.left), max(col, info.right)
  else
    RowInfo[row] = { left = col, right = col }
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

local function UpdateTimeInfo (col, row, layer, time)
  local index = (row - 1) * NCols + col
  local value = layer[index]

  if value then
    value[1], value[2] = min(value[1], time), max(value[2], time)
  else
    value = { time, time }
  end

  layer[index] = value
end

local function GetTimePart (value, index)
  if not value then
    return 0
  elseif index == 1 then
    return value[1] / 256
  else
    return (value[2] - value[1] + 1) / 256
  end
end

--
--
--

-- In early development, this looked atrocious with filtering. Now it
-- actually seems okay, if still off. The "low" value might often survive,
-- or at least be reasonably close, whereas the "range" will be all over
-- the place.

display.setDefault("magTextureFilter", "nearest")
display.setDefault("minTextureFilter", "nearest")

local Tex2 = memoryBitmap.newTexture{ width = NCols, height = NRows }
local Tex3 = memoryBitmap.newTexture{ width = NCols, height = NRows }

display.setDefault("magTextureFilter", "linear")
display.setDefault("minTextureFilter", "linear")

--
--
--

local function BakeLayers (layers)
  local r, g, b, a, index = layers[1], layers[2], layers[3], layers[4], 1

  for row = 1, NRows do
    for col = 1, NCols do
      local r, g, b = r[index], g[index], b[index]
      local rlow, glow, blow, alpha = GetTimePart(r, 1), GetTimePart(g, 1), GetTimePart(b, 1), a[index] or 0
      local rrange, grange, brange = GetTimePart(r, 2), GetTimePart(g, 2), GetTimePart(b, 2)

      -- If a pixel is shown at all times, just bake it into the skin instead; this will
      -- be the case at all the follicles, for instance. This gives us some slack in the
      -- encoding and seems to avoid some artifacts, presumably related to rounding.
      if rrange == 1 then
        alpha, rlow, rrange = alpha + .25, 0, 0
      end

      if grange == 1 then
        alpha, glow, grange = alpha + .25, 0, 0
      end

      if brange == 1 then
        alpha, blow, brange = alpha + .25, 0, 0
      end

      Tex2:setPixel(col, row, rlow, glow, blow, alpha)
      Tex3:setPixel(col, row, rrange, grange, brange, 1)
      
      index = index + 1
    end
  end

  Tex2:invalidate()
  Tex3:invalidate()
end

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
  if 1 + x^2 == 1 then
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
  return sqrt(vx^2 + vy^2 + vz^2)
end

local function GetAngle (vx, vy, vz, wx, wy, wz)
  local dot = vx * wx + vy * wy + vz * wz
  local vmw = Length(vx - wx, vy - wy, vz - wz)
  local vpw = Length(vx + wx, vy + wy, vz + wz)

  return AngleBetween(dot, vmw, vpw)
end

local function RandomAxis ()
  local ax, ay, az = random(-20, 20) / 50, random(-20, 20) / 50, random(30, 50) / 50
  local alen = Length(ax, ay, az)

  return ax / alen, ay / alen, az / alen
end

local SegmentLength = 11.5

local function RenderSegment (layer, time, x1, y1, vx, vy)
  local x2, y2 = x1 + vx * SegmentLength, y1 + vy * SegmentLength

  Raycast(x1, y1, x2, y2, UpdateTimeInfo, layer, time)

  return x2, y2
end

--
--
--

local Densities = { 20, 15, 20 }

widget.newButton{
  left = Image.contentBounds.xMax + 20,
  top = Image.contentBounds.yMin,
  label = "Build",

  onEvent = function(event)
    if event.phase == "ended" then
      local alayer = {}

      for row, edges in pairs(RowInfo) do
        local r0 = (row - 1) * NCols

        for col = edges.left, edges.right do
          alayer[r0 + col] = .25
        end
      end

      local layers = {}

      for _, density in ipairs(Densities) do
        local layer = {}

        for row, edges in pairs(RowInfo) do
          for col = edges.left, edges.right do
            if random(100) < density then
              local n = random(2, 6)
              local vx, vy, vz = RandomAxis()
              local wx, wy, wz = RandomAxis()
              local angle = GetAngle(vx, vy, vz, wx, wy, wz)
              local sina = SinXOverX(angle)

              vx, vy = vx / sina, vy / sina
              wx, wy = wx / sina, wy / sina

              for j = 0, 255 do
                local t, x, y = j / 256, col * CellWidth, row * CellHeight

                for _ = 1, n do
                  x, y = RenderSegment(layer, j, x, y, Slerp(vx, vy, wx, wy, angle, t))
                  t = random(j * 2, j * 2 + 5) / 550 -- slightly shifted
                end
              end
            end
          end
        end
    
        layers[#layers + 1] = layer
      end

      layers[#layers + 1] = alayer

      BakeLayers(layers)
    end
  end
}

--
--
--

local Image2 = display.newImageRect(Tex2.filename, Tex2.baseDir, 200, 200)

Image2:setStrokeColor(1, 0, 0)

Image2.anchorX, Image2.x = 0, Left
Image2.anchorY, Image2.y = 0, Top + 450
Image2.strokeWidth = 2

local Image3 = display.newImageRect(Tex3.filename, Tex3.baseDir, 200, 200)

Image3:setStrokeColor(1, 0, 0)

Image3.anchorX, Image3.x = 0, Left + 250
Image3.anchorY, Image3.y = 0, Top + 450
Image3.strokeWidth = 2

--
--
--

graphics.defineEffect{
  category = "composite", name = "fur",

  vertexData = {
    { index = 0, name = "t1", min = 0, max = 1, default = 0 },
    { index = 1, name = "t2", min = 0, max = 1, default = 0 },
    { index = 2, name = "t3", min = 0, max = 1, default = 0 },
  },

  fragment = [[
    P_COLOR vec4 FragmentKernel (P_UV vec2 uv)
    {
      P_COLOR vec4 rgba = texture2D(CoronaSampler0, uv);
      P_COLOR vec3 range = texture2D(CoronaSampler1, uv).rgb;
      P_COLOR float blank = step(dot(rgba, rgba), 0.) * step(dot(range, range), 0.);

      // See if the time falls within the bounds for each component.
      P_UV vec3 low = rgba.rgb, when = CoronaVertexUserData.xyz - low;
      P_COLOR vec3 ge_lower = step(vec3(0.), when);
      P_COLOR vec3 le_upper = step(when, range);
      P_COLOR vec3 bounded = ge_lower * le_upper;

      // Accumulate the tint for any valid component. This is not a very sophisticated
      // blending policy, but always-visible pixels are easy, cf. BakeLayers() above.
      P_COLOR float fur_tint = dot(bounded, vec3(.25));
      P_COLOR vec4 gray = vec4(rgba.a + fur_tint);

      // Tint the final grayscale value. If the pixel was hidden, clear anything done.
      return CoronaColorScale(gray) * (1. - blank);
    }
  ]]
}

--
--
--

local Image4 = display.newRect(0, 0, 300, 300)

Image4.fill = {
  type = "composite",
  paint1 = { type = "image", filename = Tex2.filename, baseDir = Tex2.baseDir },
  paint2 = { type = "image", filename = Tex3.filename, baseDir = Tex3.baseDir }
}

Image4.fill.effect = "composite.custom.fur"

Image4:setStrokeColor(0, 1, 0)

Image4.anchorX, Image4.x = 0, Left
Image4.anchorY, Image4.y = 0, Top + 700
Image4.strokeWidth = 2

Image4:setFillColor(.7, .3, .2) -- vair

transition.to(Image4.fill.effect, { t1 = 1, time = 1300, transition = easing.continuousLoop, iterations = 0 })
transition.to(Image4.fill.effect, { t2 = 1, time = 2300, transition = easing.continuousLoop, iterations = 0 })
transition.to(Image4.fill.effect, { t3 = 1, time = 1300, transition = easing.continuousLoop, iterations = 0 })