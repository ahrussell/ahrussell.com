// ═══════════════════════════════════════════════════════════════════════
// Costanza — "How It Works" Visualization Options
// Four approaches to animating the epoch lifecycle
// ═══════════════════════════════════════════════════════════════════════

import React, { useState, useEffect } from 'react';
import { createRoot } from 'react-dom/client';
import { motion, AnimatePresence } from 'framer-motion';

const h = React.createElement;

// ── Design tokens (match blog post CSS variables) ────────────────────
const C = {
  bg:          '#f9fafb',
  elevated:    '#ffffff',
  elevatedAlt: '#e3e8ee',
  text:        '#111827',
  muted:       '#6b7280',
  accent:      '#4b9ea0',
  accentSoft:  '#c9e3e3',
  accentDim:   '#4b9ea033',
  accentStrong:'#F31940',
  border:      '#d1d5db',
};
const F = {
  body:    '"IBM Plex Sans", system-ui, sans-serif',
  heading: '"DM Serif Display", Georgia, serif',
  mono:    '"IBM Plex Mono", monospace',
};
const ease = [0.4, 0, 0.2, 1];

// ── Shared hooks ─────────────────────────────────────────────────────
function useStepCycle(n, ms = 2500) {
  const [step, setStep] = useState(0);
  useEffect(() => {
    const id = setInterval(() => setStep(s => (s + 1) % n), ms);
    return () => clearInterval(id);
  }, [n, ms]);
  return step;
}

// ── Geometry helpers ─────────────────────────────────────────────────
function polar(cx, cy, r, deg) {
  const rad = (deg - 90) * Math.PI / 180;
  return [cx + r * Math.cos(rad), cy + r * Math.sin(rad)];
}
function arcPath(cx, cy, r, s, e) {
  const [sx, sy] = polar(cx, cy, r, s);
  const [ex, ey] = polar(cx, cy, r, e);
  return `M${sx},${sy} A${r},${r} 0 ${e - s > 180 ? 1 : 0} 1 ${ex},${ey}`;
}

// ── Shared small components ──────────────────────────────────────────
function Dots({ n, active, cx: dotCx = 350, cy }) {
  const sp = 12, x0 = dotCx - (n - 1) * sp / 2;
  return h('g', null, Array.from({ length: n }, (_, i) =>
    h(motion.circle, {
      key: i, cx: x0 + i * sp, cy, r: 2.5, strokeWidth: 1.5,
      animate: {
        fill:   i === active ? C.accent : 'transparent',
        stroke: i === active ? C.accent : C.border,
      },
      transition: { duration: 0.3 },
    })
  ));
}


// ═══════════════════════════════════════════════════════════════════════
//  OPTION A — THE EPOCH RING
//  Radial process diagram with orbiting progress indicator.
//  Six phases arranged as arcs around a ring; a dot orbits to mark
//  progress while the center shows a description of the active phase.
// ═══════════════════════════════════════════════════════════════════════
function EpochRing() {
  const step = useStepCycle(6, 2800);
  const cx = 350, cy = 178, r = 118;
  const SPAN = 60, GAP = 5;

  const phases = [
    { label: 'Post Bounty',     short: 'BOUNTY',   desc: ['Treasury offers a bounty for', 'someone to run Costanza\u2019s brain'] },
    { label: 'Reverse Auction', short: 'AUCTION',  desc: ['Bidders compete \u2014 the', 'lowest bid wins the right to run'] },
    { label: 'Execute in TEE',  short: 'EXECUTE',  desc: ['Winner runs the brain inside', 'a hardware-secured enclave'] },
    { label: 'Generate Proof',  short: 'PROOF',    desc: ['TEE produces cryptographic', 'proof of unmodified execution'] },
    { label: 'Verify on Chain', short: 'VERIFY',   desc: ['Smart contract checks the', 'proof\u2019s validity on-chain'] },
    { label: 'Act & Pay',       short: 'ACT+PAY',  desc: ['Action is executed and the', 'bounty is paid to the winner'] },
  ];

  // ── Small icons drawn at each phase node on the ring ──
  const phaseIcons = [
    // Coin (bounty)
    (clr) => h('g', null,
      h('circle', { r: 5, fill: 'none', stroke: clr, strokeWidth: 1.3 }),
      h('text', { textAnchor: 'middle', dominantBaseline: 'central', fontFamily: F.mono, fontSize: 7, fontWeight: 700, fill: clr }, '$'),
    ),
    // Down arrow (auction)
    (clr) => h('path', { d: 'M0,-4.5 L0,3.5 M-3,1 L0,4.5 L3,1', stroke: clr, fill: 'none', strokeWidth: 1.3, strokeLinecap: 'round', strokeLinejoin: 'round' }),
    // CPU (execute)
    (clr) => h('g', null,
      h('rect', { x: -4, y: -4, width: 8, height: 8, rx: 1, fill: 'none', stroke: clr, strokeWidth: 1.2 }),
      h('circle', { r: 1.5, fill: clr }),
    ),
    // Lock (proof)
    (clr) => h('g', null,
      h('rect', { x: -4, y: -1, width: 8, height: 6, rx: 1, fill: 'none', stroke: clr, strokeWidth: 1.2 }),
      h('path', { d: 'M-2,-1 V-3.5 A2,2,0,0,1,2,-3.5 V-1', fill: 'none', stroke: clr, strokeWidth: 1.2 }),
    ),
    // Checkmark (verify)
    (clr) => h('path', { d: 'M-3.5,0 L-1,2.5 L4,-3.5', fill: 'none', stroke: clr, strokeWidth: 1.6, strokeLinecap: 'round', strokeLinejoin: 'round' }),
    // Lightning (act)
    (clr) => h('path', { d: 'M1,-5 L-2,0 L1,0 L-1,5', fill: 'none', stroke: clr, strokeWidth: 1.3, strokeLinecap: 'round', strokeLinejoin: 'round' }),
  ];

  return h('svg', {
    viewBox: '0 0 700 385',
    width: '100%',
    style: { display: 'block' },
    'aria-label': 'Epoch lifecycle ring diagram',
  },
    // Glow filter
    h('defs', null,
      h('filter', { id: 'a-glow', x: '-50%', y: '-50%', width: '200%', height: '200%' },
        h('feGaussianBlur', { stdDeviation: '3', result: 'b' }),
        h('feMerge', null, h('feMergeNode', { in: 'b' }), h('feMergeNode', { in: 'SourceGraphic' }))
      ),
    ),

    // Faint background ring
    h('circle', { cx, cy, r, fill: 'none', stroke: C.border, strokeWidth: 0.75, strokeDasharray: '2 5', opacity: 0.5 }),

    // Phase arcs
    ...phases.map((_, i) => {
      const s = i * SPAN + GAP / 2, e = (i + 1) * SPAN - GAP / 2;
      const active = step === i;
      return h(motion.path, {
        key: `arc-${i}`, d: arcPath(cx, cy, r, s, e),
        fill: 'none', strokeLinecap: 'round',
        animate: {
          stroke:      active ? C.accent : C.border,
          strokeWidth: active ? 3.5     : 1.5,
          opacity:     active ? 1       : 0.45,
        },
        transition: { duration: 0.5, ease },
      });
    }),

    // Phase icon nodes on the ring
    ...phases.map((_, i) => {
      const midDeg = i * SPAN + SPAN / 2;
      const [nx, ny] = polar(cx, cy, r, midDeg);
      const active = step === i;
      return h('g', { key: `icon-${i}`, transform: `translate(${nx},${ny})` },
        // background disc
        h(motion.circle, {
          r: 11, strokeWidth: 1.5,
          animate: {
            fill:   active ? C.accent   : C.elevated,
            stroke: active ? C.accent   : C.border,
          },
          transition: { duration: 0.4, ease },
        }),
        // icon
        h(motion.g, {
          animate: { opacity: active ? 1 : 0.5 },
          transition: { duration: 0.3 },
        }, phaseIcons[i](active ? '#fff' : C.muted)),
      );
    }),

    // Outer labels
    ...phases.map((phase, i) => {
      const midDeg = i * SPAN + SPAN / 2;
      const [lx, ly] = polar(cx, cy, r + 28, midDeg);
      const active = step === i;
      const anchor = Math.abs(lx - cx) < 25 ? 'middle' : lx < cx ? 'end' : 'start';
      return h(motion.text, {
        key: `lbl-${i}`, x: lx, y: ly,
        textAnchor: anchor, dominantBaseline: 'central',
        fontFamily: F.body, fontSize: 10.5,
        fontWeight: active ? 600 : 400,
        animate: { fill: active ? C.text : C.muted, opacity: active ? 1 : 0.55 },
        transition: { duration: 0.35 },
      }, phase.label);
    }),

    // Orbiting glow
    h('g', { transform: `translate(${cx},${cy})` },
      h(motion.g, {
        animate: { rotate: step * SPAN },
        transition: { duration: 0.9, ease },
      },
        h('circle', { cx: 0, cy: -r, r: 7, fill: C.accent, opacity: 0.18 }),
        h('circle', { cx: 0, cy: -r, r: 3, fill: C.accent }),
      ),
    ),

    // Center description (crossfade)
    h(AnimatePresence, { mode: 'wait' },
      h(motion.g, {
        key: step,
        initial: { opacity: 0 }, animate: { opacity: 1 }, exit: { opacity: 0 },
        transition: { duration: 0.3 },
      },
        h('text', { x: cx, y: cy - 20, textAnchor: 'middle', fontFamily: F.heading, fontSize: 14, fill: C.text }, phases[step].label),
        ...phases[step].desc.map((line, j) =>
          h('text', { key: j, x: cx, y: cy + 1 + j * 15, textAnchor: 'middle', fontFamily: F.body, fontSize: 10.5, fill: C.muted }, line),
        ),
      ),
    ),

    h(Dots, { n: 6, active: step, cy: 370 }),
  );
}


// ═══════════════════════════════════════════════════════════════════════
//  OPTION B — THE PIPELINE
//  Horizontal left-to-right flow. Five stage nodes connected by paths;
//  an animated token traverses the pipeline, triggering each stage.
// ═══════════════════════════════════════════════════════════════════════
function Pipeline() {
  const step = useStepCycle(5, 2600);
  const y = 115, R = 22;

  const stages = [
    { label: 'Treasury',       x: 75,  desc: 'Posts bounty to blockchain' },
    { label: 'Auction',        x: 222, desc: 'Lowest bid wins the job' },
    { label: 'TEE Hardware',   x: 370, desc: 'Brain runs in sealed enclave' },
    { label: 'Smart Contract', x: 518, desc: 'Verifies cryptographic proof' },
    { label: 'Action',         x: 645, desc: 'Donate, invest, adjust, or wait' },
  ];

  // Per-stage icons (rendered at 0,0, sized ±6 px)
  const icons = [
    // $ coin
    (a) => h(motion.text, { key: 'i0', textAnchor: 'middle', dominantBaseline: 'central', fontFamily: F.mono, fontSize: 15, fontWeight: 700, animate: { fill: a ? '#fff' : C.muted }, transition: { duration: 0.3 } }, '$'),
    // gavel / down arrow
    (a) => h(motion.path, { key: 'i1', d: 'M0,-5.5 L0,4 M-3.5,0.5 L0,4.5 L3.5,0.5', fill: 'none', strokeWidth: 1.8, strokeLinecap: 'round', strokeLinejoin: 'round', animate: { stroke: a ? '#fff' : C.muted }, transition: { duration: 0.3 } }),
    // lock (TEE)
    (a) => h('g', { key: 'i2' },
      h(motion.rect, { x: -5, y: -1, width: 10, height: 7.5, rx: 1.5, fill: 'none', strokeWidth: 1.4, animate: { stroke: a ? '#fff' : C.muted }, transition: { duration: 0.3 } }),
      h(motion.path, { d: 'M-2.5,-1 V-4 A2.5,2.5,0,0,1,2.5,-4 V-1', fill: 'none', strokeWidth: 1.4, animate: { stroke: a ? '#fff' : C.muted }, transition: { duration: 0.3 } }),
    ),
    // checkmark
    (a) => h(motion.path, { key: 'i3', d: 'M-4,0.5 L-1,3.5 L5,-3.5', fill: 'none', strokeWidth: 2, strokeLinecap: 'round', strokeLinejoin: 'round', animate: { stroke: a ? '#fff' : C.muted }, transition: { duration: 0.3 } }),
    // fan-out
    (a) => h('g', { key: 'i4' },
      h(motion.circle, { cx: -2, cy: 0, r: 1.5, animate: { fill: a ? '#fff' : C.muted }, transition: { duration: 0.3 } }),
      h(motion.path, { d: 'M0,-3.5 L5,-5.5 M0,-1 L5,-1 M0,1.5 L5,1.5 M0,4 L5,6', fill: 'none', strokeWidth: 1.2, animate: { stroke: a ? '#fff' : C.muted }, transition: { duration: 0.3 } }),
    ),
  ];

  return h('svg', {
    viewBox: '0 0 720 265',
    width: '100%',
    style: { display: 'block' },
    'aria-label': 'Epoch pipeline flow diagram',
  },

    // ── Background connector lines (dashed, always visible) ──
    ...stages.slice(0, -1).map((s, i) =>
      h('line', { key: `bg-${i}`, x1: s.x + R + 2, y1: y, x2: stages[i + 1].x - R - 2, y2: y, stroke: C.border, strokeWidth: 1, strokeDasharray: '3 4', opacity: 0.5 }),
    ),

    // ── Animated connector lines (draw-on effect) ──
    ...stages.slice(0, -1).map((s, i) => {
      const done = step > i;
      return h(motion.path, {
        key: `ln-${i}`,
        d: `M${s.x + R + 2},${y} L${stages[i + 1].x - R - 2},${y}`,
        stroke: C.accent, strokeWidth: 1.5, fill: 'none',
        initial: { pathLength: 0, opacity: 0 },
        animate: { pathLength: done ? 1 : 0, opacity: done ? 0.8 : 0 },
        transition: { duration: 0.55, ease },
      });
    }),

    // ── Directional chevrons on connectors ──
    ...stages.slice(0, -1).map((s, i) => {
      const mx = (s.x + stages[i + 1].x) / 2;
      return h(motion.path, {
        key: `ch-${i}`,
        d: `M${mx - 3},${y - 3.5} L${mx + 3},${y} L${mx - 3},${y + 3.5}`,
        stroke: C.accent, fill: 'none', strokeWidth: 1.2, strokeLinecap: 'round', strokeLinejoin: 'round',
        animate: { opacity: step > i ? 0.6 : 0 },
        transition: { duration: 0.3, delay: step > i ? 0.35 : 0 },
      });
    }),

    // ── Stage nodes ──
    ...stages.map((s, i) => {
      const active = step === i;
      return h('g', { key: `nd-${i}` },
        // outer ring (subtle pulse when active)
        h(motion.circle, {
          cx: s.x, cy: y, r: R + 4, fill: 'none', strokeWidth: 1,
          animate: { stroke: active ? C.accentSoft : 'transparent', opacity: active ? 1 : 0 },
          transition: { duration: 0.5 },
        }),
        // main disc
        h(motion.circle, {
          cx: s.x, cy: y, r: R, strokeWidth: 1.5,
          animate: { fill: active ? C.accent : C.elevated, stroke: active ? C.accent : C.border },
          transition: { duration: 0.4, ease },
        }),
        // icon
        h('g', { transform: `translate(${s.x},${y})` }, icons[i](active)),
        // label
        h('text', { x: s.x, y: y + R + 18, textAnchor: 'middle', fontFamily: F.body, fontSize: 10.5, fontWeight: 500, fill: C.text }, s.label),
        // active description
        h(motion.text, {
          x: s.x, y: y + R + 32, textAnchor: 'middle', fontFamily: F.body, fontSize: 9.5,
          animate: { opacity: active ? 1 : 0, fill: C.muted },
          transition: { duration: 0.3 },
        }, s.desc),
      );
    }),

    // ── Traveling token ──
    h(motion.circle, {
      cy: y, r: 4.5, fill: C.accentStrong,
      animate: { cx: stages[step].x },
      transition: { duration: 0.8, ease },
    }),

    h(Dots, { n: 5, active: step, cy: 248 }),
  );
}


// ═══════════════════════════════════════════════════════════════════════
//  OPTION C — THE SEQUENCE
//  Vertical sequence diagram in the style of UML, but cleaner.
//  Three actor lifelines; animated arrows reveal the message flow
//  step-by-step, top to bottom.
// ═══════════════════════════════════════════════════════════════════════
function SequenceDiagram() {
  const step = useStepCycle(8, 1700);

  const actors = [
    { label: 'Smart Contract', x: 120 },
    { label: 'Bidders / Winner', x: 350 },
    { label: 'TEE Hardware', x: 580 },
  ];
  const hdrY = 22, hdrH = 30;
  const startY = 80, gap = 42;

  const msgs = [
    { from: 0, to: 1, label: 'Post bounty' },
    { from: 1, to: 0, label: 'Submit bids + bonds' },
    { from: 0, to: 1, label: 'Award lowest bidder' },
    { from: 1, to: 2, label: 'Run brain program' },
    { from: 2, to: 1, label: 'Cryptographic proof' },
    { from: 1, to: 0, label: 'Submit proof + action' },
    { from: 0, to: 0, label: 'Verify proof' },          // self-loop
    { from: 0, to: 1, label: 'Execute action & pay bounty' },
  ];

  const bottomY = startY + msgs.length * gap;

  return h('svg', {
    viewBox: '0 0 700 440',
    width: '100%',
    style: { display: 'block' },
    'aria-label': 'Epoch sequence diagram',
  },

    // ── Actor header boxes ──
    ...actors.map((a, i) => h('g', { key: `hdr-${i}` },
      h('rect', { x: a.x - 60, y: hdrY, width: 120, height: hdrH, rx: 4, fill: C.elevated, stroke: C.border, strokeWidth: 1 }),
      h('text', { x: a.x, y: hdrY + hdrH / 2, textAnchor: 'middle', dominantBaseline: 'central', fontFamily: F.body, fontSize: 10.5, fontWeight: 600, fill: C.text }, a.label),
    )),

    // ── Lifelines (dashed) ──
    ...actors.map((a, i) => h('line', {
      key: `ll-${i}`, x1: a.x, y1: hdrY + hdrH, x2: a.x, y2: bottomY + 8,
      stroke: C.border, strokeWidth: 1, strokeDasharray: '4 4',
    })),

    // ── Active lifeline highlight bars ──
    ...actors.map((a, ai) => {
      // find first & last message involving this actor up to current step
      let first = -1, last = -1;
      msgs.forEach((m, mi) => {
        if (mi > step) return;
        if (m.from === ai || m.to === ai) {
          if (first === -1) first = mi;
          last = mi;
        }
      });
      if (first === -1) return null;
      return h(motion.rect, {
        key: `hl-${ai}`,
        x: a.x - 3, width: 6, rx: 3,
        fill: C.accentSoft,
        animate: {
          y: startY + first * gap - 5,
          height: (last - first) * gap + 10,
          opacity: 0.5,
        },
        transition: { duration: 0.4, ease },
      });
    }),

    // ── Message arrows ──
    ...msgs.map((msg, i) => {
      const my = startY + i * gap;
      const vis = i <= step;
      const act = i === step;
      const isSelf = msg.from === msg.to;

      if (isSelf) {
        // Self-loop (goes to the left)
        const lx = actors[msg.from].x;
        const loopW = 30, loopH = 16;
        const d = `M${lx},${my} C${lx - loopW},${my} ${lx - loopW},${my + loopH} ${lx},${my + loopH}`;
        return h('g', { key: `msg-${i}` },
          h(motion.path, {
            d, fill: 'none', strokeLinecap: 'round',
            animate: { pathLength: vis ? 1 : 0, opacity: vis ? (act ? 1 : 0.35) : 0, stroke: act ? C.accent : C.muted, strokeWidth: act ? 1.5 : 1 },
            transition: { duration: 0.45, ease },
          }),
          // arrowhead
          h(motion.path, {
            d: `M${lx - 5},${my + loopH - 4} L${lx},${my + loopH} L${lx - 5},${my + loopH + 3}`,
            fill: 'none', strokeLinecap: 'round', strokeLinejoin: 'round', strokeWidth: 1.4,
            animate: { opacity: vis ? (act ? 1 : 0.35) : 0, stroke: act ? C.accent : C.muted },
            transition: { duration: 0.25, delay: vis ? 0.25 : 0 },
          }),
          // label
          h(motion.text, {
            x: lx - loopW - 6, y: my + loopH / 2, textAnchor: 'end', dominantBaseline: 'central',
            fontFamily: F.body, fontSize: 9,
            animate: { opacity: vis ? (act ? 1 : 0.4) : 0, fill: act ? C.text : C.muted },
            transition: { duration: 0.3, delay: vis ? 0.15 : 0 },
          }, msg.label),
        );
      }

      const fx = actors[msg.from].x, tx = actors[msg.to].x;
      const right = tx > fx;
      const x1 = fx + (right ? 6 : -6), x2 = tx + (right ? -6 : 6);
      const ahDir = right ? 1 : -1;

      return h('g', { key: `msg-${i}` },
        // line
        h(motion.path, {
          d: `M${x1},${my} L${x2},${my}`, fill: 'none', strokeLinecap: 'round',
          animate: { pathLength: vis ? 1 : 0, opacity: vis ? (act ? 1 : 0.35) : 0, stroke: act ? C.accent : C.muted, strokeWidth: act ? 1.5 : 1 },
          transition: { duration: 0.45, ease },
        }),
        // arrowhead
        h(motion.path, {
          d: `M${x2 - 5 * ahDir},${my - 3.5} L${x2},${my} L${x2 - 5 * ahDir},${my + 3.5}`,
          fill: 'none', strokeLinecap: 'round', strokeLinejoin: 'round', strokeWidth: 1.4,
          animate: { opacity: vis ? (act ? 1 : 0.35) : 0, stroke: act ? C.accent : C.muted },
          transition: { duration: 0.25, delay: vis ? 0.3 : 0 },
        }),
        // label
        h(motion.text, {
          x: (fx + tx) / 2, y: my - 9, textAnchor: 'middle',
          fontFamily: F.body, fontSize: 9,
          animate: { opacity: vis ? (act ? 1 : 0.4) : 0, fill: act ? C.text : C.muted },
          transition: { duration: 0.3, delay: vis ? 0.15 : 0 },
        }, msg.label),
      );
    }),

    // ── Step number badge ──
    h(AnimatePresence, { mode: 'wait' },
      h(motion.g, {
        key: step,
        initial: { opacity: 0 }, animate: { opacity: 1 }, exit: { opacity: 0 },
        transition: { duration: 0.2 },
      },
        h('circle', { cx: 30, cy: startY + step * gap, r: 10, fill: C.elevated, stroke: C.accent, strokeWidth: 1.5 }),
        h('text', { x: 30, y: startY + step * gap, textAnchor: 'middle', dominantBaseline: 'central', fontFamily: F.mono, fontSize: 9, fontWeight: 600, fill: C.accent }, `${step + 1}`),
      ),
    ),

    h(Dots, { n: 8, active: step, cy: 425 }),
  );
}


// ═══════════════════════════════════════════════════════════════════════
//  OPTION D — THE STATE MACHINE
//  Finite-state diagram showing both the happy path (pentagon) and
//  the failure/recovery path through a SLEEPING state.  Demonstrates
//  the self-healing economics: bond forfeiture + auto-escalating bids.
// ═══════════════════════════════════════════════════════════════════════
function StateMachine() {
  const step = useStepCycle(5, 2600);

  // Pentagon layout — happy-path states
  const states = [
    { id: 'idle',    label: 'Idle',      x: 350, y: 52,  desc: 'Waiting for next epoch' },
    { id: 'auction', label: 'Auction',   x: 558, y: 148, desc: 'Collecting bids' },
    { id: 'execute', label: 'Executing', x: 490, y: 300, desc: 'Brain running in TEE' },
    { id: 'verify',  label: 'Verifying', x: 210, y: 300, desc: 'Checking proof on-chain' },
    { id: 'done',    label: 'Complete',  x: 142, y: 148, desc: 'Action executed, bounty paid' },
  ];

  // Failure state (center)
  const sleeping = { label: 'Sleeping', x: 350, y: 195, desc: 'Epoch skipped \u2014 max bid +10%' };

  // Transition curves (quadratic Bezier, clockwise)
  const transitions = [
    { d: 'M395,52  Q490,40  540,128',  label: 'epoch begins' },
    { d: 'M570,170 Q580,245 510,285',  label: 'lowest bid wins' },
    { d: 'M465,312 Q350,340 235,312',  label: 'proof submitted' },
    { d: 'M195,280 Q140,230 145,170',  label: 'proof valid' },
    { d: 'M158,128 Q200,45  305,50',   label: 'next epoch' },
  ];

  // Failure paths (dashed)
  const failPaths = [
    { d: 'M535,165 Q460,175 395,190', label: 'no proof \u2192 bond lost' },
    { d: 'M465,290 Q420,250 390,205', label: 'timeout' },
    { d: 'M350,175 L350,75',          label: 'resume (+10% max bid)' },
  ];

  const W = 88, H = 28, rx = 6;

  return h('svg', {
    viewBox: '0 0 700 390',
    width: '100%',
    style: { display: 'block' },
    'aria-label': 'Epoch state machine diagram',
  },

    // ── Failure-path arrows (always visible, muted, dashed) ──
    ...failPaths.map((fp, i) => h('g', { key: `fp-${i}` },
      h('path', { d: fp.d, fill: 'none', stroke: C.border, strokeWidth: 1, strokeDasharray: '4 3', opacity: 0.6 }),
      // small label
      (() => {
        // approximate label position: midpoint of the Bezier
        const parts = fp.d.match(/[-\d.]+/g).map(Number);
        const mx = parts.length >= 6 ? (parts[0] + parts[parts.length - 2]) / 2 : (parts[0] + parts[2]) / 2;
        const my = parts.length >= 6 ? (parts[1] + parts[parts.length - 1]) / 2 : (parts[1] + parts[3]) / 2;
        return h('text', { x: mx + (i === 2 ? 8 : 0), y: my + (i === 2 ? 0 : -7), textAnchor: 'middle', fontFamily: F.mono, fontSize: 7.5, fill: C.border, letterSpacing: '0.02em' }, fp.label);
      })(),
    )),

    // ── Happy-path transition curves ──
    ...transitions.map((t, i) => {
      const active = step === i;
      const past = step > i || (step === 0 && i === transitions.length - 1 && false); // don't highlight wrap
      return h('g', { key: `tr-${i}` },
        // background (always visible but faint)
        h('path', { d: t.d, fill: 'none', stroke: C.border, strokeWidth: 0.75, opacity: 0.4 }),
        // animated path
        h(motion.path, {
          d: t.d, fill: 'none', strokeLinecap: 'round',
          animate: {
            pathLength:  active ? 1 : 0,
            stroke:      C.accent,
            strokeWidth: 1.8,
            opacity:     active ? 1 : 0,
          },
          transition: { duration: 0.7, ease },
        }),
        // label
        (() => {
          const parts = t.d.match(/[-\d.]+/g).map(Number);
          const mx = parts.length >= 6 ? (parts[0] + 2 * parts[2] + parts[4]) / 4 : (parts[0] + parts[2]) / 2;
          const my = parts.length >= 6 ? (parts[1] + 2 * parts[3] + parts[5]) / 4 : (parts[1] + parts[3]) / 2;
          // offset label away from pentagon center
          const dx = mx - 350, dy = my - 195;
          const mag = Math.sqrt(dx * dx + dy * dy) || 1;
          const ox = dx / mag * 14, oy = dy / mag * 14;
          return h(motion.text, {
            x: mx + ox, y: my + oy - 2,
            textAnchor: 'middle', fontFamily: F.body, fontSize: 9, fontWeight: 500,
            animate: { opacity: active ? 1 : 0.35, fill: active ? C.accent : C.muted },
            transition: { duration: 0.3 },
          }, t.label);
        })(),
      );
    }),

    // ── SLEEPING state (always visible, muted) ──
    h('rect', { x: sleeping.x - W / 2, y: sleeping.y - H / 2, width: W, height: H, rx, fill: C.elevated, stroke: C.border, strokeWidth: 1, strokeDasharray: '4 3' }),
    h('text', { x: sleeping.x, y: sleeping.y - 1, textAnchor: 'middle', dominantBaseline: 'central', fontFamily: F.body, fontSize: 10, fill: C.muted }, sleeping.label),
    h('text', { x: sleeping.x, y: sleeping.y + 24, textAnchor: 'middle', fontFamily: F.body, fontSize: 8.5, fill: C.border }, sleeping.desc),

    // ── Happy-path state nodes ──
    ...states.map((s, i) => {
      // active = current destination state for this step
      // step i means transition i just completed, so destination is states[(i+1) % 5]
      // but let's define: step 0 → states[0] is active (IDLE), step 1 → states[1] (AUCTION), etc.
      const active = step === i;

      return h('g', { key: `st-${i}` },
        // glow ring
        h(motion.rect, {
          x: s.x - W / 2 - 4, y: s.y - H / 2 - 4, width: W + 8, height: H + 8, rx: rx + 2,
          fill: 'none', strokeWidth: 1.5,
          animate: { stroke: active ? C.accentSoft : 'transparent', opacity: active ? 1 : 0 },
          transition: { duration: 0.4 },
        }),
        // box
        h(motion.rect, {
          x: s.x - W / 2, y: s.y - H / 2, width: W, height: H, rx,
          strokeWidth: 1.5,
          animate: { fill: active ? C.accent : C.elevated, stroke: active ? C.accent : C.border },
          transition: { duration: 0.4, ease },
        }),
        // label
        h(motion.text, {
          x: s.x, y: s.y - 1, textAnchor: 'middle', dominantBaseline: 'central',
          fontFamily: F.body, fontSize: 11, fontWeight: 600,
          animate: { fill: active ? '#fff' : C.text },
          transition: { duration: 0.3 },
        }, s.label),
        // sub-description
        h(motion.text, {
          x: s.x, y: s.y + H / 2 + 14, textAnchor: 'middle',
          fontFamily: F.body, fontSize: 8.5,
          animate: { opacity: active ? 1 : 0, fill: C.muted },
          transition: { duration: 0.3 },
        }, s.desc),
      );
    }),

    // ── Active state step indicator ──
    h(AnimatePresence, { mode: 'wait' },
      h(motion.text, {
        key: step, x: 350, y: 365, textAnchor: 'middle',
        fontFamily: F.mono, fontSize: 9, letterSpacing: '0.06em',
        fill: C.muted,
        initial: { opacity: 0 }, animate: { opacity: 0.6 }, exit: { opacity: 0 },
        transition: { duration: 0.2 },
      }, `STATE: ${states[step].id.toUpperCase()}`),
    ),

    h(Dots, { n: 5, active: step, cy: 382 }),
  );
}


// ═══════════════════════════════════════════════════════════════════════
//  COMPARISON WRAPPER
// ═══════════════════════════════════════════════════════════════════════

const options = [
  { id: 'A', title: 'The Epoch Ring',     sub: 'Radial process with orbiting indicator',        Comp: EpochRing },
  { id: 'B', title: 'The Pipeline',       sub: 'Horizontal flow with traversing token',         Comp: Pipeline },
  { id: 'C', title: 'The Sequence',       sub: 'Vertical actor-lane message diagram',           Comp: SequenceDiagram },
  { id: 'D', title: 'The State Machine',  sub: 'Finite-state diagram with failure recovery',    Comp: StateMachine },
];

function VizComparison() {
  return h('div', { style: { display: 'flex', flexDirection: 'column', gap: '2.2rem' } },
    ...options.map(({ id, title, sub, Comp }) =>
      h('div', {
        key: id,
        style: {
          background: C.elevated,
          border: `1px solid ${C.border}`,
          borderRadius: '6px',
          overflow: 'hidden',
        },
      },
        // Header
        h('div', { style: { padding: '0.85rem 1.1rem 0' } },
          h('div', {
            style: {
              fontFamily: F.mono, fontSize: '0.68rem', color: C.muted,
              textTransform: 'uppercase', letterSpacing: '0.1em', marginBottom: '0.15rem',
            },
          }, `Option ${id}`),
          h('div', { style: { fontFamily: F.heading, fontSize: '1.05rem', color: C.text } }, title),
          h('div', { style: { fontFamily: F.body, fontSize: '0.8rem', color: C.muted, marginTop: '0.1rem' } }, sub),
        ),
        // Viz
        h('div', { style: { padding: '0.5rem 0.35rem 0.6rem' } }, h(Comp)),
      ),
    ),
  );
}

// ── Mount ────────────────────────────────────────────────────────────
const el = document.getElementById('viz-comparison');
if (el) createRoot(el).render(h(VizComparison));
