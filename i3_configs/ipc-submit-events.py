#!/usr/bin/env python3

import i3ipc
i3 = i3ipc.Connection()

def on_workspace_focus(self, e):
    # The first parameter is the connection to the ipc and the second is an object
    # with the data of the event sent from i3.
    if e.current:
        print('Windows on this workspace:')
        for w in e.current.leaves():
            print(w.name)


def on_window_focus(i3, e):
    focused = i3.get_tree().find_focused()
    ws_name = "%s:%s - %s" % (focused.workspace().num, focused.window_class if focused.window_class else 'Empty', focused.workspace().num)
    i3.command('rename workspace to "%s"' % ws_name)

# Subscribe to events
i3.on('workspace::focus', on_window_focus)
i3.on("window::focus", on_window_focus)

i3.main()
