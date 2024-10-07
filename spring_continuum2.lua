--- An approximation (over a square)--with lots of guesswork--based on
-- the idea described in https://www.cs.otago.ac.nz/staffpriv/mccane/publications/fur_simulation.pdf.
--
-- This largely dispenses with the actual spring basis, instead just being
-- the lighting model. Rather than do simplex noise-based updates to the
-- springs proper, it is used per-hair from within vertex shaders to update
-- each hair. Each hair is given some parameters that look into the noise
-- which is itself steered around, akin to the previous wind.

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
local random = math.random

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

  Update()
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
  local x, y = random(), random()

  for row = 1, h do
    for col = 1, w do
      m:setFillVertexColor(vi, U[row], V[col], x, y) -- smuggle u, v in as red and green; offsets as blue and alpha

      vi = vi + 1
    end
  end
end

graphics.defineEffect{
  category = "generator", name = "fluffy",
  
  uniformData  = {
    {
      name = "light",
      type = "vec3",
      index = 0
    }, {
      name = "offset",
      type = "vec2",
      index = 1
    }
  },
  
  vertex = ([[
    uniform P_POSITION vec2 u_UserData1; // offset

    varying P_POSITION vec3 v_Pos;
    varying P_POSITION vec3 v_Tan;

    //
    // psrdnoise2.glsl
    //
    // Authors: Stefan Gustavson (stefan.gustavson@gmail.com)
    // and Ian McEwan (ijm567@gmail.com)
    // Version 2021-12-02, published under the MIT license (see below)
    //
    // Copyright (c) 2021 Stefan Gustavson and Ian McEwan.
    //
    // Permission is hereby granted, free of charge, to any person obtaining a
    // copy of this software and associated documentation files (the "Software"),
    // to deal in the Software without restriction, including without limitation
    // the rights to use, copy, modify, merge, publish, distribute, sublicense,
    // and/or sell copies of the Software, and to permit persons to whom the
    // Software is furnished to do so, subject to the following conditions:
    //
    // The above copyright notice and this permission notice shall be included
    // in all copies or substantial portions of the Software.
    //
    // THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    // IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    // FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
    // THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    // LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    // FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    // DEALINGS IN THE SOFTWARE.
    //

    //
    // Periodic (tiling) 2-D simplex noise (hexagonal lattice gradient noise)
    // with rotating gradients and analytic derivatives.
    //
    // This is (yet) another variation on simplex noise. Unlike previous
    // implementations, the grid is axis-aligned and slightly stretched in
    // the y direction to permit rectangular tiling.
    // The noise pattern can be made to tile seamlessly to any integer period
    // in x and any even integer period in y. Odd periods may be specified
    // for y, but then the actual tiling period will be twice that number.
    //
    // The rotating gradients give the appearance of a swirling motion, and
    // can serve a similar purpose for animation as motion along z in 3-D
    // noise. The rotating gradients in conjunction with the analytic
    // derivatives allow for "flow noise" effects as presented by Ken
    // Perlin and Fabrice Neyret.
    //

    //
    // 2-D tiling simplex noise with rotating gradients and analytical derivative.
    // "vec2 x" is the point (x,y) to evaluate, and
    // "float alpha" is the rotation (in radians) for the swirling gradients.
    // The "float" return value is the noise value.
    //
    // The rotation by alpha uses one single addition. Unlike the 3-D version
    // of psrdnoise(), setting alpha == 0.0 gives no speedup.
    //
    P_UV float psrdnoise (P_UV vec2 x, P_UV float alpha)
    {
      // Transform to simplex space (axis-aligned hexagonal grid)
      P_UV vec2 uv = vec2(x.x + x.y*0.5, x.y);

      // Determine which simplex we're in, with i0 being the "base"
      P_UV vec2 i0 = floor(uv);
      P_UV vec2 f0 = fract(uv);
      // o1 is the offset in simplex space to the second corner
      P_UV float cmp = step(f0.y, f0.x);
      P_UV vec2 o1 = vec2(cmp, 1.0-cmp);

      // Enumerate the remaining simplex corners
      P_UV vec2 i1 = i0 + o1;
      P_UV vec2 i2 = i0 + vec2(1.0, 1.0);

      // Transform corners back to texture space
      P_UV vec2 v0 = vec2(i0.x - i0.y * 0.5, i0.y);
      P_UV vec2 v1 = vec2(v0.x + o1.x - o1.y * 0.5, v0.y + o1.y);
      P_UV vec2 v2 = vec2(v0.x + 0.5, v0.y + 1.0);

      // Compute vectors from v to each of the simplex corners
      P_UV vec2 x0 = x - v0;
      P_UV vec2 x1 = x - v1;
      P_UV vec2 x2 = x - v2;

      P_UV vec3 iu = vec3(i0.x, i1.x, i2.x);
      P_UV vec3 iv = vec3(i0.y, i1.y, i2.y);

      // Compute one pseudo-random hash value for each corner
      P_UV vec3 hash = mod(iu, 289.0);
      hash = mod((hash*51.0 + 2.0)*hash + iv, 289.0);
      hash = mod((hash*34.0 + 10.0)*hash, 289.0);

      // Pick a pseudo-random angle and add the desired rotation
      P_UV vec3 psi = hash * 0.07482 + alpha;
      P_UV vec3 gx = cos(psi);
      P_UV vec3 gy = sin(psi);

      // Reorganize for dot products below
      P_UV vec2 g0 = vec2(gx.x,gy.x);
      P_UV vec2 g1 = vec2(gx.y,gy.y);
      P_UV vec2 g2 = vec2(gx.z,gy.z);

      // Radial decay with distance from each simplex corner
      P_UV vec3 w = 0.8 - vec3(dot(x0, x0), dot(x1, x1), dot(x2, x2));
      w = max(w, 0.0);
      P_UV vec3 w2 = w * w;
      P_UV vec3 w4 = w2 * w2;

      // The value of the linear ramp from each of the corners
      P_UV vec3 gdotx = vec3(dot(g0, x0), dot(g1, x1), dot(g2, x2));

      // Multiply by the radial decay and sum up the noise value
      P_UV float n = dot(w4, gdotx);

      // Scale the return value to fit nicely into the range [-1,1]
      return 10.9 * n;
    }
  
    P_POSITION vec3 Rodrigues (P_POSITION vec3 v, P_POSITION vec3 n, P_UV float theta)
    {
      P_POSITION vec3 sv = v * cos(theta);
      P_POSITION vec3 cs = cross(n, v) * sin(theta);        
      P_UV float sin_half_theta = sin(theta / 2.);
      P_UV float one_minus_cos_theta = 2. * sin_half_theta * sin_half_theta; // cf. https://plunk.org/~hatch/rightway.html
      P_POSITION vec3 sn = n * dot(n, v) * one_minus_cos_theta;
      
      return sv + cs + sn;
    }
  
    P_POSITION vec2 VertexKernel (P_POSITION vec2 _)
    {
      // texcoord: normalized position of root, within rect
      // color: r = fraction of height, g = fraction of longitude (-1, +1), ga = per-instance offset
      
      P_POSITION vec2 root = vec2(%f, %f) + (CoronaTexCoord - vec2(.5)) * vec2(200., 300.);

      const P_UV float PI = %f;
      
      P_UV float w_angle = 1.975 * PI * psrdnoise(u_UserData1 + vec2(5. * v_ColorScale.b, 0.), v_ColorScale.r);
      P_UV float theta = .975 * PI * mix(-.37, .37, (u_UserData1.g + v_ColorScale.a) * .75);
      P_POSITION vec3 w = normalize(vec3(
        cos(w_angle + v_ColorScale.b), mix(-.125, .125, v_ColorScale.a), sin(w_angle + v_ColorScale.b)
      ));
      P_POSITION vec3 n = Rodrigues(vec3(0., 0., 1.), w, theta);

// w as plane circle + minor tilt?
// what about length?
// n as Rodrigues() of that from (0, 0, 1)?

      P_POSITION vec3 zi = w;
      P_UV float comp_z = dot(n, zi);
      P_UV float l_xy = 1. - comp_z;
      P_POSITION vec3 yi = normalize(n * l_xy);
      P_POSITION vec3 xi = cross(zi, yi);
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
  ]]):format(
    display.contentCenterX, display.contentCenterY,
    math.pi
  ),

  fragment = ([[
    uniform P_POSITION vec3 u_UserData0; // light

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
      P_POSITION vec3 ldir = normalize(u_UserData0 - v_Pos);
      
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

local Offset = {}

function Update ()
  Offset[1], Offset[2] = Pos.x, Pos.y
  m.fill.effect.offset = Offset
end






--[[
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
]]