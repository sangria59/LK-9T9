#!/usr/bin/env python3
"""
Sway Which-Key - Popup keybind hints for Sway modes
Shows a popup for the current mode and closes it immediately when any keybind is used or the mode changes.
"""

import json
import subprocess
import signal
import sys

MODE_HINTS = {
    "mode?": [
        ["w: window control", "m: move focus"],
        ["e: execute", "P: power"],
        ["s: screen", "ESC: cancel"]
    ],
    "window control": [
        ["q: kill", "Shift+f: floating toggle"],
        ["f: fullscreen", "Shift+t: focus mode"],
        ["b: split horizontal", "Shift+s: layout stacking"],
        ["v: split vertical", "r: resize"],
        ["t: layout tabbed", "ESC: cancel"],
        ["s: toggle split", ""]
    ],
    "execute": [
        ["w: librewolf", "t: terminal"],
        ["e: emacs", "ESC: cancel"]
    ],
    "screen": [
        ["s: screenshot full", ""],
        ["Shift+s: screenshot selection", ""],
        ["ESC: cancel", ""]
    ],
    "move focus": [
        ["h/←: left", "j/↓: down"],
        ["k/↑: up", "l/→: right"],
        ["m: move window", "ESC/RET: cancel"]
    ],
    "move window": [
        ["h/←: left", "j/↓: down"],
        ["k/↑: up", "l/→: right"],
        ["m: move focus", "w: to workspace"],
        ["ESC/RET: cancel", ""]
    ],
    "move to workspace": [
        ["1-0: workspace 1-10", ""],
        ["ESC: cancel", ""]
    ],
    "resize": [
        ["h/←: shrink width", "k/↑: shrink height"],
        ["l/→: grow width", "j/↓: grow height"],
        ["RET/ESC: done", ""]
    ],
    "power": [
        ["o: poweroff", ""],
        ["ESC: cancel", ""]
    ]
}

current_hint_window = None

def format_hints(mode_name):
    """Format keybind hints with manual columns"""
    if mode_name not in MODE_HINTS:
        return ""
    lines = []
    for row in MODE_HINTS[mode_name]:
        formatted_row = "  ".join(f"{col:30}" for col in row if col)
        lines.append(f"  {formatted_row}")
    return "\n".join(lines)

def show_hint_window(mode_name):
    """Spawn a foot terminal popup for the current mode"""
    global current_hint_window
    kill_hint_window()

    if mode_name == "default" or mode_name not in MODE_HINTS:
        return

    hints_text = format_hints(mode_name)
    title = f"{mode_name}"

    cmd = [
        "foot",
        "--app-id=sway-whichkey",
        "--title=Sway Which-Key",
        "--font=monospace:size=12",
        "-e", "bash", "-c",
        f'printf "\\033[1;36m{title}\\033[0m\\n\\n{hints_text}"; exec sleep 9999'
    ]

    current_hint_window = subprocess.Popen(cmd)


def kill_hint_window():
    """Kill the current popup window if it exists"""
    global current_hint_window
    if current_hint_window:
        try:
            current_hint_window.terminate()
            current_hint_window.wait(timeout=1)
        except Exception:
            pass
        current_hint_window = None

    # Kill any stray foot windows from previous popups
    subprocess.run(["pkill", "-9", "-f", "sway-whichkey"], stderr=subprocess.DEVNULL)


def monitor_modes():
    """Listen to Sway mode changes and input events to manage the popup"""
    proc = subprocess.Popen(
        ["swaymsg", "-t", "subscribe", "-m", '["mode","input"]'],
        stdout=subprocess.PIPE,
        text=True
    )

    try:
        for line in proc.stdout:
            try:
                event = json.loads(line)

                # Mode changes
                if "change" in event:
                    mode_name = event.get("change", "default")
                    show_hint_window(mode_name)

                # Any key pressed in the current mode
                elif event.get("type") == "input" and event.get("change") == "key":
                    kill_hint_window()

            except json.JSONDecodeError:
                continue
    except KeyboardInterrupt:
        kill_hint_window()
        proc.terminate()

def signal_handler(sig, frame):
    kill_hint_window()
    sys.exit(0)

if __name__ == "__main__":
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    monitor_modes()
