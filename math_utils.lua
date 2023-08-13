--- A few math utilities.
--
--
-- For several of them, see http://www.plunk.org/~hatch/rightway.html

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
local asin = math.asin
local pi = math.pi
local sin = math.sin
local sqrt = math.sqrt

-- Cached module references --
local _AngleBetween_
local _Length_
local _SinXOverX_

-- Exports --
local M = {}

--
--
--

---
function M.AngleBetween (dot, vmw, vpw)
  if dot < 0 then
    return pi - 2 * asin(vpw / 2)
  else
    return 2 * asin(vmw / 2)
  end
end

--
--
--

function M.GetAngle (vx, vy, vz, wx, wy, wz)
  local dot = vx * wx + vy * wy + vz * wz
  local vmw = _Length_(vx - wx, vy - wy, vz - wz)
  local vpw = _Length_(vx + wx, vy + wy, vz + wz)

  return _AngleBetween_(dot, vmw, vpw)
end


--
--
--

function M.Length (vx, vy, vz)
  return sqrt(vx * vx + vy * vy + vz * vz)
end
--
--
--

---
function M.SinXOverX (x)
  if 1 + x * x == 1 then
    return 1
  else
    return sin(x) / x
  end
end

--
--
--

---
function M.Slerp (vx, vy, wx, wy, angle, t)
  local s = 1 - t
  local u = s * _SinXOverX_(s * angle)
  local v = t * _SinXOverX_(t * angle)

  return u * vx + v * wx, u * vy + v * wy
end

--
--
--

_AngleBetween_ = M.AngleBetween
_Length_ = M.Length
_SinXOverX_ = M.SinXOverX

return M