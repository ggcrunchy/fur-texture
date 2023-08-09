Fur texture generator
=====================

This is an attempt to take the sort of fur effect one might see [in 3D](https://hhoppe.com/fur.pdf) and capture some of that
information in a data texture. The preliminary steps are the same, but then the waviness of the fur is compressed into the
texels: one layer per component (RGB), along with opaque and fixed parts (alpha). In particular, the layers capture several
moments of time in individual bits, indicating whether any fur was visible then at that texel; whereas a tint is recorded
in the alpha according to any hairs always being there, along with any "skin".

Needless to say this is rather lossy, but with suitable constraints on the hairs it actually yields passable results.

A description of the current state (and a brief video) may be found [here](https://forums.solar2d.com/t/fur-take-two/354724)
and a previous effort is [here](https://forums.solar2d.com/t/fur-foliage-wip/343798).

---

The "separated" module was an experiment but so far looks to be a dud.
