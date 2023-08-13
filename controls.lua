--- Some user interface controls for grids.

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
local floor = math.floor

-- Solar2D globals --
local display = display

-- Exports --
local M = {}

--
--
--

local function Quantize (pos, dim)
  return floor(pos / dim)
end

--
--
--

---
function M.MakeRaycaster (NCols, NRows, CellWidth, CellHeight)

  local function Visit (col, row, func, arg1, arg2)
    if col >= 1 and col <= NCols and row >= 1 and row <= NRows then
      func(col, row, arg1, arg2)
    end
  end

  --
  --
  --

  return function (x1, y1, x2, y2, func, arg1, arg2)
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

end

--
--
--

function M.PaintObject (tex, cell_width, cell_height, raycast, paint)
  local object = display.newImageRect(tex.filename, tex.baseDir, tex.width * cell_width, tex.height * cell_height)

  --
  --
  --

  object:addEventListener("touch", function(event)
    local phase, target = event.phase, event.target
    local bounds = target.contentBounds

    if phase == "began" then
      display.getCurrentStage():setFocus(target)

      local x, y = event.x - bounds.xMin, event.y - bounds.yMin

      raycast(x, y, x, y, paint) -- trivial cast
      tex:invalidate()

      target.last_x, target.last_y = event.x, event.y
      target.is_touched = true
    elseif target.is_touched then
      if phase == "moved" then
        raycast(target.last_x - bounds.xMin, target.last_y - bounds.yMin, event.x - bounds.xMin, event.y - bounds.yMin, paint)
        tex:invalidate()

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

  return object
end

--
--
--

return M