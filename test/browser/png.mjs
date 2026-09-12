// A PNG AS A GRID THE SUITE CAN READ, shared by the drivers that screenshot.
import { inflateSync } from "node:zlib";

/** A PNG as a grid this file can read: node's own inflate, then the five
 * filters.  Chromium screenshots are 8-bit RGB(A), uninterlaced. */
export function pixels(png) {
  if (png.readUInt32BE(0) !== 0x89504e47) throw new Error("not a PNG");
  let at = 8, w = 0, h = 0, depth = 0, colour = 0, interlace = 0;
  const idat = [];
  while (at < png.length) {
    const len = png.readUInt32BE(at);
    const kind = png.toString("ascii", at + 4, at + 8);
    const body = png.subarray(at + 8, at + 8 + len);
    if (kind === "IHDR") {
      w = body.readUInt32BE(0); h = body.readUInt32BE(4);
      depth = body[8]; colour = body[9]; interlace = body[12];
    } else if (kind === "IDAT") idat.push(body);
    else if (kind === "IEND") break;
    at += len + 12;
  }
  if (depth !== 8 || interlace !== 0 || (colour !== 2 && colour !== 6))
    throw new Error(`unsupported PNG: depth ${depth} colour ${colour}`);
  const n = colour === 6 ? 4 : 3;
  const raw = inflateSync(Buffer.concat(idat));
  const out = Buffer.alloc(w * h * n);
  const stride = w * n;
  for (let y = 0; y < h; y += 1) {
    const filter = raw[y * (stride + 1)];
    const line = raw.subarray(y * (stride + 1) + 1, y * (stride + 1) + 1 + stride);
    const to = y * stride, up = to - stride;
    for (let x = 0; x < stride; x += 1) {
      const a = x >= n ? out[to + x - n] : 0;
      const b = y > 0 ? out[up + x] : 0;
      const c = x >= n && y > 0 ? out[up + x - n] : 0;
      let v = line[x];
      if (filter === 1) v += a;
      else if (filter === 2) v += b;
      else if (filter === 3) v += (a + b) >> 1;
      else if (filter === 4) {
        const q = a + b - c;
        const pa = Math.abs(q - a), pb = Math.abs(q - b), pc = Math.abs(q - c);
        v += (pa <= pb && pa <= pc) ? a : (pb <= pc ? b : c);
      }
      out[to + x] = v & 0xff;
    }
  }
  const hex = (v) => v.toString(16).padStart(2, "0");
  const box = (b, f) => {
    let k = 0;
    for (let y = Math.max(0, b.y); y < Math.min(h, b.y + b.h); y += 1)
      for (let x = Math.max(0, b.x); x < Math.min(w, b.x + b.w); x += 1) k += f(x, y);
    return k;
  };
  return {
    w, h,
    at(x, y) {
      if (x < 0 || y < 0 || x >= w || y >= h) return null;
      const i = y * stride + x * n;
      return "#" + hex(out[i]) + hex(out[i + 1]) + hex(out[i + 2]);
    },
    /** How many pixels of BOX carry HEX — the reading a wash is judged by. */
    count(b, want) { return box(b, (x, y) => (this.at(x, y) === want ? 1 : 0)); },
    /** How many pixels of BOX this frame and OTHER disagree on. */
    differs(b, other) {
      return box(b, (x, y) => (this.at(x, y) === other.at(x, y) ? 0 : 1));
    },
  };
}
