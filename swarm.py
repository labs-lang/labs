import json
import numpy as np

import gizeh
import moviepy.editor as mpy

filename = "out/swarm.json"

with open(filename, encoding='utf-8') as json_file:
    text = json_file.read()
    trace = json.loads(text)

radius = 5
x_max = 50
y_max = 50
W, H = x_max * radius, y_max * radius  # width, height, in pixels


ids = [c["id"] for c in trace[0]]
colors = {i: tuple(map(tuple, np.random.rand(1, 3)))[0] for i in ids}

fps = 24
duration = (len(trace) + 1) // fps
total_frames = len(trace)


def make_frame(t):
    step = int(t * fps / (total_frames - fps) * len(trace))
    step = min(step, len(trace) - 1)
    surface = gizeh.Surface(W, H, bg_color=(1, 1, 1))
    for comp in trace[step]:
        loc = tuple(x * radius for x in comp["I"]["loc"])
        c = gizeh.circle(radius, xy=loc, fill=colors[comp["id"]])
        txt = gizeh.text(str.format("{}/{}", step, len(trace)),
                         fontfamily="sans",
                         fontsize=16,
                         fill=(0, 0, 0), xy=(50, 10))
        c.draw(surface)
        txt.draw(surface)
        if "dir" in comp["L"]:
            d = tuple(x * radius * 2 for x in comp["L"]["dir"])
            delta = (loc[0] + d[0], loc[1] + d[1])
            line = gizeh.polyline(
                points=[loc, delta], stroke_width=2,
                stroke=colors[comp["id"]], fill=colors[comp["id"]])
            line.draw(surface)
    return surface.get_npimage(y_origin="top")


clip = mpy.VideoClip(make_frame, duration=duration)
clip.write_gif("circle.gif", fps=fps, opt="OptimizePlus", fuzz=10)
