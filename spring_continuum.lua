--- An approximation (over a square)--with lots of guesswork--based on
-- the idea described in https://www.cs.otago.ac.nz/staffpriv/mccane/publications/fur_simulation.pdf.

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
local cos = math.cos
local exp = math.exp
local floor = math.floor
local ipairs = ipairs
local max = math.max
local random = math.random
local sin = math.sin
local sqrt = math.sqrt

--
--
--

-- An implementation of Ken Perlin's simplex noise.
--
-- Based on code and comments in [Simplex noise demystified][1],
-- by Stefan Gustavson.
--
-- Thanks to Mike Pall for some cleanup and improvements (and for [LuaJIT][2]!).
--
-- [1]: http://www.itn.liu.se/~stegu/simplexnoise/simplexnoise.pdf
-- [2]: http://www.luajit.org

-- Permutation of 0-255, replicated to allow easy indexing with sums of two bytes --
local Perms = {
	151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225,
	140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148,
	247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32,
	57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68,	175,
	74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111,	229, 122,
	60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54,
	65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169,
	200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64,
	52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212,
	207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213,
	119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9,
	129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104,
	218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241,
	81,	51, 145, 235, 249, 14, 239,	107, 49, 192, 214, 31, 181, 199, 106, 157,
	184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93,
	222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180
}

for i = 1, 256 do
  Perms[256 + i] = Perms[i]
end

-- The above, mod 12 for each element --
local Perms12 = {}

for i = 1, 512 do
	Perms12[i] = Perms[i] % 12
end

-- Gradients for 2D, 3D case --
local Grads3 = {
	{ 1, 1, 0 }, { -1, 1, 0 }, { 1, -1, 0 }, { -1, -1, 0 },
	{ 1, 0, 1 }, { -1, 0, 1 }, { 1, 0, -1 }, { -1, 0, -1 },
	{ 0, 1, 1 }, { 0, -1, 1 }, { 0, 1, -1 }, { 0, -1, -1 }
}

-- 2D weight contribution
local function GetN (ix, iy, x, y)
    local t = .5 - x * x - y * y
    local index = Perms12[ix + Perms[iy + 1] + 1]
    local grad = Grads3[index + 1]

    return max(0, t^4) * (grad[1] * x + grad[2] * y)
end

-- 2D skew factor:
local F = (math.sqrt(3) - 1) / 2
local G = (3 - math.sqrt(3)) / 6
local G2 = 2 * G - 1

--- 2-dimensional simplex noise.
-- @number x Value #1.
-- @number y Value #2.
-- @treturn number Noise value &isin; [-1, +1].
local function SampleNoise (x, y)
  -- Skew the input space to determine which simplex cell we are in.
  local s = (x + y) * F
  local ix, iy = floor(x + s), floor(y + s)

  -- Unskew the cell origin back to (x, y) space.
  local t = (ix + iy) * G
  local x0 = x + t - ix
  local y0 = y + t - iy

  -- Calculate the contribution from the two fixed corners.
  -- A step of (1,0) in (i,j) means a step of (1-G,-G) in (x,y), and
  -- A step of (0,1) in (i,j) means a step of (-G,1-G) in (x,y).
  ix, iy = ix % 256, iy % 256

  local n0 = GetN(ix, iy, x0, y0)
  local n2 = GetN(ix + 1, iy + 1, x0 + G2, y0 + G2)

  --[[
      Determine other corner based on simplex (equilateral triangle) we are in:
      if x0 > y0 then
          ix, x1 = ix + 1, x1 - 1
      else
          iy, y1 = iy + 1, y1 - 1
      end
  ]]
  local xi = x0 > y0 and 1 or 0
  local n1 = GetN(ix + xi, iy + (1 - xi), x0 + G - xi, y0 + G - (1 - xi))

  -- Add contributions from each corner to get the final noise value.
  -- The result is scaled to return values in the interval [-1,1].
  return 70.1480580019 * (n0 + n1 + n2)
end

--
--
--

local RHS = {}

function RHS.catmull_rom (coeffs, a, b, c, d)
	coeffs.a = .5 * (-b + 2 * c - d)
	coeffs.b = .5 * (2 * a - 5 * c + 3 * d)
	coeffs.c = .5 * (b + 4 * c - 3 * d)
	coeffs.d = .5 * (-c + d)
end

local function ComputeCoefficients (stype, pos, tan, t)
	local eval, t2 = RHS[stype], t * t

	if pos then
		eval(pos, 1, t, t2, t2 * t)
	end

	if tan then
		eval(tan, 0, 1, 2 * t, 3 * t2)
	end
end

local function ResolveCoefficients (coeffs, a, b, c, d)
	local x = coeffs.a * a.x + coeffs.b * b.x + coeffs.c * c.x + coeffs.d * d.x
	local y = coeffs.a * a.y + coeffs.b * b.y + coeffs.c * c.y + coeffs.d * d.y

	return x, y
end

local Coeffs = {}

local function GetPosition (stype, a, b, c, d, t)
	ComputeCoefficients(stype, Coeffs, nil, t)

	return ResolveCoefficients(Coeffs, a, b, c, d)
end

--
--
--

local AirResistance = .18
local Stiffness = 0.014
local Rho = 1.72

local NeutralFric = .02
local MaxFric = .07

local ThetaMax = math.pi / 13

local Epsilon = .07

--
--
--

local Pos = {}

--
--
--

local Spline = { {}, {}, {}, {} }

for i = 2, 4 do -- remaining point filled in by NextPoint()
  Spline[i].x, Spline[i].y = random(), random()
end

--
--
--

local Until, Start = -1

local function NextPoint (now)
  Start = now
  Until = now + random(5700, 8100)

  local last = Spline[1]

  last.x, last.y = random(), random()

  for i = 1, 3 do
    Spline[i] = Spline[i + 1]
  end

  Spline[4] = last
end

--
--
--

local function Set (out, x, y, z)
  out.x, out.y, out.z = x, y, z

  return out
end

--
--
--

local function Cross (v, w, out)
  -- use temps to allow `v` or `w` as `out`
  local x = v.y * w.z - v.z * w.y
  local y = v.z * w.x - v.x * w.z
  local z = v.x * w.y - v.y * w.x

  return Set(out, x, y, z)
end

--
--
--

local function Dot (v, w)
  return v.x * w.x + v.y * w.y + v.z * w.z
end

--
--
--

local function Scale (v, k, out)
  return Set(out, v.x * k, v.y * k, v.z * k) -- n.b. `v` may be `out`
end

--
--
--

local function Add (v, w, out)
  return Set(out, v.x + w.x, v.y + w.y, v.z + w.z) -- n.b. `v` may be `out`
end

--
--
--

local function AddScaled (v, w, k, out)
  return Add(v, Scale(w, k, out), out) -- n.b. `v` or `w` may be `out`
end

--
--
--

local function Sub (v, w, out)
  return Set(out, v.x - w.x, v.y - w.y, v.z - w.z) -- n.b. `v` may be `out`
end

--
--
--

local A, B, C = {}, {}, {}

--
--
--

local function Rodrigues (v, n, out) -- n = theta * unit axis
  local theta_squared = Dot(n, n)

  if 1 + theta_squared ~= 1 then
    local theta = sqrt(theta_squared)
  
    --
    --
    --

    Scale(v, cos(theta), A)

    --
    --
    --

    Scale(Cross(n, v, out), sin(theta) / theta, B)
    
    --
    --
    --
    
    local sin_half_theta = sin(theta / 2)
    local one_minus_cos_theta = 2 * sin_half_theta * sin_half_theta -- cf. https://plunk.org/~hatch/rightway.html
    
    Scale(n, Dot(n, v) * one_minus_cos_theta / theta_squared, C)
    
    --
    --
    --
    
    return Add(Add(A, B, out), C, out)
  else
    return Set(out, v.x, v.y, v.z)
  end
end

--
--
--

local function ZVec (z)
  return { x = 0, y = 0, z = z }
end

--
--
--

local N = ZVec(1)

--
--
--

local function Zero ()
  return { x = 0, y = 0, z = 0 }
end

--
--
--

local Gravity = ZVec(-5)

--
--
--

local Temp1, Temp2 = {}, {}

--
--
--

local function Length (v)
  return sqrt(Dot(v, v))
end

--
--
--

local function Lerp (a, b, t)
  return a + (b - a) * t
end

--
--
--

local P = {
  { x = 200, y = 200 },
  { x = 600, y = 200 },
  { x = 200, y = 600 },
  { x = 600, y = 600 }
}

for _, v in ipairs(P) do
  v.wind = {}
  v.ang_pos = Zero()
  v.ang_vel = Zero()
  v.n_prime = {}
  v.f = Zero()
  v.z = 0
end

local Update

local LastTime

Runtime:addEventListener("enterFrame", function(event)
  local now, dt = event.time, 0

  if LastTime then
    dt = (now - LastTime) / 1000
  end

  LastTime = now

  --
  --
  --
  
  if Until <= now then
    NextPoint(now)
  end

  local delta = (now - Start) / (Until - Start)

  Pos.x, Pos.y = GetPosition("catmull_rom", Spline[1], Spline[2], Spline[3], Spline[4], delta)

  --
  --
  --

  for _, v in ipairs(P) do
    v.wind.x = 55 * SampleNoise(v.x + Pos.x, v.y)
    v.wind.y = 55 * SampleNoise(v.x, v.y + Pos.y)
    v.wind.z = 27 * SampleNoise(Pos.x, Pos.y) * .7

    Rodrigues(N, v.ang_pos, v.n_prime)
    Cross(v.ang_pos, N--[[v.n_prime]], v.f) -- TODO? is "n" in paper...
  end

  Update()

  --
  --
  --

  for i, v in ipairs(P) do
    local nabla_dot_f = 0

--[[
-- TODO?: is this actually what it wants?
    for j, w in ipairs(P) do
      Cross(v.ang_pos, w.n_prime, w.f)
    end
--]]
    for j, w in ipairs(P) do
      if i ~= j then
        Sub(v.f, w.f, Temp1)
        Sub(w, v, Temp2)

        nabla_dot_f = nabla_dot_f + Dot(Temp1, Temp2) / Dot(Temp2, Temp2)
      end
    end
    
    local det_J_t = exp(nabla_dot_f / (#P - 1))
    local h = 1 - max(Lerp(NeutralFric, MaxFric, det_J_t), 0) -- n.b. assume no change in surface area, i.e. det_J_b = 1 and k_fb unknown

    Cross(v.n_prime, Gravity, A) -- mass-independent acceleration; n.b. assumes surface acceleration of 0
    Cross(v.wind, v.n_prime, B) -- air-resistance acceleration; n.b. assumes surface wind velocity of 0
    AddScaled(A, B, AirResistance / Rho, B)
    AddScaled(B, v.ang_pos, -Stiffness, C) -- spring acceleration
    -- AddScaled(v.ang_pos, v.ang_vel, dt, v.ang_pos) -- TODO? better here?

    local theta = Length(v.ang_pos)
--local a,b,c=C.x,C.y,C.z
    AddScaled(v.ang_vel, C, h * dt, C)

    if theta >= ThetaMax / 2 then
      local len = Length(v.ang_vel)

      if theta < ThetaMax then
        Scale(C, (1 - (Epsilon + 1) * theta / ThetaMax) / len, v.ang_vel)
      else
        Scale(C, -Epsilon / len, v.ang_vel)
      end
    else
      Set(v.ang_vel, C.x, C.y, C.z)
    end

    AddScaled(v.ang_pos, v.ang_vel, dt, v.ang_pos)
if i == 1 then--[[
  print("a", a, b, c)
  print("v", v.ang_vel.x, v.ang_vel.y, v.ang_vel.z)
  print("p", v.ang_pos.x, v.ang_pos.y, v.ang_pos.z)
  print("")]]
end
  end 
end)








local function Quad (indices, offset, w)
  local br = offset
  local tr = br - w

  -- above: 1       2 / 5
  -- below: 3 / 4   6

  indices[#indices + 1] = tr - 1
  indices[#indices + 1] = tr
  indices[#indices + 1] = br - 1
  indices[#indices + 1] = br - 1
  indices[#indices + 1] = tr
  indices[#indices + 1] = br
end

local indices = {}
local vertices = {}
local uvs = {}

local U, V = {}, {}
local w, h = 5, 5

for row = 1, h do
  U[row] = 1 - (row - 1) / (h - 1)
end

for col = 1, w do
  V[col] = (col - 1) / (w - 1) -- used as v = -pi / 2 + pi * theta
end

local vi = 0

local K = 15

for i = 0, K do
  for j = 0, K do
    for row = 1, h do
      for col = 1, w do
        uvs[#uvs + 1] = i / K -- horizontal position of hair...
        uvs[#uvs + 1] = j / K -- ...and vertical
        vertices[#vertices + 1] = col * 125 -- some dummy position so mesh at least exists
        vertices[#vertices + 1] = row * 125

        if col > 1 and row > 1 then
          Quad(indices, vi + col, w)
        end
      end
      
      vi = vi + w
    end
  end
end

assert(#indices < 65536, "32-bit indices are unsupported")

local m = display.newMesh{
  x = display.contentCenterX, y = display.contentCenterY,
  mode = "indexed",
  
  vertices = vertices,
  uvs = uvs,
  indices = indices
}

vi = 1

for _ = 1, (K + 1) * (K + 1) do
  for row = 1, h do
    for col = 1, w do
      m:setFillVertexColor(vi, U[row], V[col], 0, 0) -- smuggle u, v in as red and green

      vi = vi + 1
    end
  end
end

graphics.defineEffect{
  category = "generator", name = "fluffy",
  
  uniformData  = {
    {
      name = "ang_pos",
      type = "mat4",
      index = 0
    }, {
      name = "n_prime",
      type = "mat4",
      index = 1
    }, {
      name = "light",
      type = "vec3",
      index = 2
    }, {
      name = "z_scale",
      type = "float",
      index = 3
    }
  },
  
  vertex = ([[
    uniform P_POSITION mat4 u_UserData0; // columns: n1 = (x, y, z, _), n2, n3, n4...
    uniform P_POSITION mat4 u_UserData1; // ...w1, w2, w3, w4
    uniform P_POSITION float u_UserData3; // z scale
  
    varying P_POSITION vec3 v_Pos;
    varying P_POSITION vec3 v_Tan;
  
  // varyings: pos
  // X, Y, Z (same for all)
  
    P_POSITION vec2 VertexKernel (P_POSITION vec2 _)
    {
      // texcoord: normalized position of root, within rect
      // color: r = fraction of height, g = fraction of longitude (-1, +1)
      
      P_POSITION vec2 root = vec2(%f, %f) + (CoronaTexCoord - vec2(.5)) * vec2(200., 300.);
      
      P_POSITION vec3 n = mix(
        mix(u_UserData0[0], u_UserData0[2], CoronaTexCoord.x),
        mix(u_UserData0[1], u_UserData0[3], CoronaTexCoord.x),
      CoronaTexCoord.y).xyz;
      P_POSITION vec3 w = mix(
        mix(u_UserData1[0], u_UserData1[2], CoronaTexCoord.x),
        mix(u_UserData1[1], u_UserData1[3], CoronaTexCoord.x),
      CoronaTexCoord.y).xyz;
      
      n = normalize(n);

      P_UV float w2 = dot(w, w), l_xy = 0.;

      P_POSITION vec3 xi, yi, zi, p;
      
      //if (w2 > .0025) // we have some rotation
      {
        P_UV float theta = max(sqrt(w2), .0025);
        
        zi = w / theta;
        
        P_UV float comp_z = dot(n, zi);
        
        l_xy = 1. - comp_z;
        yi = normalize(n * l_xy);
        xi = cross(zi, yi);
// ^^^ todo x and z in wrong order
        P_COLOR vec2 u_long = v_ColorScale.rg;
        P_UV float u_theta = u_long.x * theta;
        P_UV float sin_ut = sin(u_theta), sin_hut = sin(u_theta / 2.);
        P_UV float one_minus_cos_ut = 2. * sin_hut * sin_hut; // cf. Hatch
        
        const P_UV float Length = 35.; // TODO!
        
        P_UV float Width = mix(5., .75, u_long.x); // TODO!
        
        
        
        P_POSITION vec3 pi = (zi * u_long.x + (l_xy / theta) * (xi * one_minus_cos_ut + yi * sin_ut)) * Length;
        P_POSITION vec3 ti = zi + l_xy * (xi * sin_ut + yi * cos(u_theta));
        P_POSITION vec3 side = normalize(vec3(0., -ti.z, +ti.y)) * Width;//-ti.z, -ti.y, +ti.x, 0.));
        P_POSITION vec3 ni = normalize(vec3(dot(ti.yz, ti.yz), -ti.x * ti.y, -ti.x * ti.z));//-ti.x * ti.z, -ti.y * ti.z, dot(ti.xy, ti.xy)));
        // TODO: scale radius according to whatever is depth (x?)
        // endpoints = pi -+ vec(-ti.y, +ti.x, 0) * radius
          // probably will have z-fighting
          // still need almost-screen-facing direction to sculpt cylindrical surface....
            // if we have (x, y, z) as ti
            // then have (-y, x, 0) as left
            // then cross(ti, ti_perp) is (-x * z, -y * z, x^2 + y^2)
        P_POSITION vec3 off = pi + mix(-side, +side, u_long.y) + ni * (1. - 4. * u_long.y * u_long.y) * Width;

        v_Pos = off;
        v_Tan = ti;
       
        return root - off.yz;
      }
      /*
      else
      {
        xi = vec3(1., 0., 0.);
        yi = vec3(0., 1., 0.);
        zi = vec3(0., 0., 1.);
      }
      */
      return root + v_ColorScale.xy * vec2(5., 35.);
    }
  ]]):format(display.contentCenterX, display.contentCenterY),

  fragment = ([[
    uniform P_POSITION vec3 u_UserData2; // light

    varying P_POSITION vec3 v_Pos;
    varying P_POSITION vec3 v_Tan;
      
    const P_UV float PI = %f;
    const P_UV float cos_alpha = %f;
    const P_UV float sin_alpha = %f;
    const P_UV float tan_alpha = %f;
  
    P_POSITION vec3 GetNormalAt (P_POSITION vec3 u, P_POSITION vec3 v, P_POSITION vec3 w, P_UV float phi, bool flip)
    {
      P_POSITION vec3 bent = (v * sin(phi) - w * cos(phi)) * cos_alpha;

      return bent - u * (sin_alpha * phi) * (flip ? -1. : +1.);
    }
  
    P_UV float Integrate (P_POSITION vec3 u, P_POSITION vec3 v, P_POSITION vec3 w, P_POSITION vec3 vec, P_UV float phi1, P_UV float phi2, bool flip)
    {
      return dot(GetNormalAt(u, v, w, phi2, flip) - GetNormalAt(u, v, w, phi1, flip), vec);
    }
  
    P_COLOR vec4 FragmentKernel (P_UV vec2 uv)
    {
      P_POSITION vec3 ldir = normalize(u_UserData2 - v_Pos);
      
      P_POSITION vec3 u = normalize(v_Tan);
      
      P_POSITION vec3 v = cross(ldir, u);
      P_POSITION vec3 w = cross(u, v);
      
      const P_POSITION vec3 e = vec3(1., 0., 0.);
      
      P_POSITION vec3 h = normalize(ldir + e);
      
      P_UV float ep_length = 1. - dot(e, u);
      
      P_UV float phi_e = atan(dot(e, w), dot(e, v));
      
      P_POSITION vec3 tu_e = tan_alpha * u / ep_length;
      
      P_UV float phi_d = acos(dot(tu_e, e)); // TODO: Hatch
      P_UV float phi_d0 = max(phi_d + phi_e, 0.);
      P_UV float phi_d1 = min(phi_d - phi_e, PI);
      
      const P_UV float k_d = .2;
      
      P_UV float psi_d0 = (k_d / PI) * Integrate(u, v, w, ldir, phi_d0, phi_d1, false);

      P_UV float phi_s = acos(dot(tu_e, h)); // TODO: ditto...
      P_UV float phi_s0 = max(phi_d0, max(phi_s + phi_e, 0.));
      P_UV float phi_s1 = min(phi_d1, min(phi_s - phi_e, PI));
      
      const P_UV float k_s = .3, p_s = 13.;
      
      P_UV float psi_s0 = pow((k_s / PI) * Integrate(u, v, w, h, phi_d0, phi_d1, false), p_s);
      
      P_UV float one_minus_l_u = (1. - dot(ldir, u)) / PI;
      
      const P_UV float k_d1 = .6;
     
      P_UV float psi_d1 = (k_d1 * one_minus_l_u) * Integrate(u, v, w, ldir, phi_d1, phi_d0 + 2. * PI, false);
      
      const P_UV float k_s1 = .4;
     
      P_UV float psi_s1 = pow((k_s1 * one_minus_l_u) * Integrate(u, v, w, h, phi_s1, phi_s0 + 2. * PI, true), p_s);
      
      P_UV float psi = psi_d0 + psi_s0 + psi_d1 + psi_s1;
      
      return vec4(vec3(1.) * psi, 1.) * vec4(.1, .9, .1, 1.);
    }
  ]]):format(
    math.pi,
    math.cos(math.pi / 60), math.sin(math.pi / 60), math.tan(math.pi / 60)
  )
}



m.fill.effect = "generator.custom.fluffy"


m.fill.effect.light = { 1, .2, .2 }

local aps, nps = {}, {}

for i = 1, 16 do
  aps[i], nps[i] = 0, 0
end

--Runtime:addEventListener("enterFrame", function()
function Update ()
  local offset = 0

  for i, v in ipairs(P) do
    local ap, np = v.ang_pos, v.n_prime
--[[
    aps[i], aps[i + 4], aps[i + 8] = ap.x, ap.y, ap.z
    nps[i], nps[i + 4], nps[i + 8] = np.x, np.y, np.z
--]]
---[[
    aps[offset + 1], aps[offset + 2], aps[offset + 3], aps[offset + 4] = ap.x, ap.y, ap.z, 0
    nps[offset + 1], nps[offset + 2], nps[offset + 3], nps[offset + 4] = np.x, np.y, np.z, 0
--]]
    offset = offset + 4
  end
  
  m.fill.effect.ang_pos = aps
  m.fill.effect.n_prime = nps
  
  -- light, eye
--end)
end







local function R ()
  return -1 + math.random() * 2
end

local function NN ()
  local a = { x = R(), y = R(), z = R() }

  return Scale(a, 1 / Length(a), a)
end

local Temp3 = {}

local function LengthWithFunc (func, u, v)
  return Length(func(u, v, Temp3))
end

local function AsinOfLength (func, u, v)
  return 2 * math.asin(LengthWithFunc(func, u, v) / 2)
end

local function LL (u, v)
  if Dot(u, v) < 0 then
    return "ASIN, L:", math.pi - AsinOfLength(Add, u, v)
  else
    return "ASIN, G:", AsinOfLength(Sub, u, v)
  end
end

for i = 1, 50 do
  local v, w = NN(), NN()
  local uav = LengthWithFunc(Add, v, w)
  local umv = LengthWithFunc(Sub, v, w)

  local aa = math.acos(Dot(v, w))
  local bb = 2 * math.atan2(umv, uav)

  print("ACOS:", aa)
  print("ATAN2:", bb)
  print(LL(v, w))
  print("")
end